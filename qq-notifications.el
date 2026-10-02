;;; qq-notifications.el --- Desktop notifications for emacs-qq -*- lexical-binding: t; -*-

;;; Commentary:

;; Telega-style delayed desktop notifications with QQ-native mention rules.

;;; Code:

(require 'cl-lib)
(require 'notifications)
(require 'ring)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'appkit-ui)
(require 'qq-customize)
(require 'qq-state)
(require 'qq-chat)
(require 'qq-media)

(defvar qq-notifications-mode nil)

(defvar qq-notifications--last-id nil
  "Currently displayed desktop notification id.")

(defvar qq-notifications--seen-anchors (make-hash-table :test #'equal)
  "Message anchors already scheduled by the notification event hook.")

(defvar qq-notifications--seen-anchor-order nil
  "Newest-first bounded order for `qq-notifications--seen-anchors'.")

(defconst qq-notifications--seen-anchor-limit 512
  "Maximum number of notification deduplication anchors retained.")

(defvar qq-notifications--history nil
  "Ring of normalized messages shown as desktop notifications.")

(defconst qq-notifications--history-buffer-name "*QQ Notifications*"
  "Fallback name of the account-scoped notification history buffer.")

(defvar qq-notifications--history-buffer nil
  "Live notification history buffer, including after a user rename.")

(defconst qq-notifications--history-buffer-owner-token
  'qq-notifications-history
  "Marker identifying buffers explicitly owned by notification history.")

(defvar-local qq-notifications--history-buffer-owner nil
  "Ownership marker for a notification history buffer.")

(defvar qq-notifications--generation 0
  "Account generation owning notification callbacks and timers.")

(defvar qq-notifications--resetting-p nil
  "Non-nil while account notification state is crossing a reset boundary.")

(defvar qq-notifications--display-owner nil
  "Exact operation token for the newest notification display attempt.")

(defvar qq-notifications--last-id-owner nil
  "Display operation that owns `qq-notifications--last-id'.")

(defvar qq-notifications--delay-owners nil
  "Owned delayed-delivery timers for the current account generation.")

(defvar qq-notifications--timeout-owners nil
  "Owned desktop-notification timeout timers for the current generation.")

(defun qq-notifications--display-owner-current-p (owner)
  "Return non-nil when OWNER is the exact current display operation."
  (and qq-notifications-mode
       (not qq-notifications--resetting-p)
       (eq owner qq-notifications--display-owner)
       (= (or (plist-get owner :generation) -1)
          qq-notifications--generation)))

(defun qq-notifications--display-id-current-p (owner id)
  "Return non-nil when OWNER still owns displayed notification ID."
  (and (qq-notifications--display-owner-current-p owner)
       (eq owner qq-notifications--last-id-owner)
       (equal id qq-notifications--last-id)))

(defun qq-notifications--backend-id-authoritatively-owned-p (id)
  "Return non-nil when the exact current display operation owns backend ID."
  (and qq-notifications--last-id-owner
       (qq-notifications--display-id-current-p
        qq-notifications--last-id-owner id)))

(defun qq-notifications--owner-current-p (owner owners)
  "Return non-nil when OWNER is current and present in OWNERS."
  (and qq-notifications-mode
       (not qq-notifications--resetting-p)
       (memq owner owners)
       (= (or (plist-get owner :generation) -1)
          qq-notifications--generation)))

(defun qq-notifications--cancel-timer-value (timer)
  "Cancel TIMER without allowing a backend failure to escape."
  (when (timerp timer)
    (condition-case err
        (cancel-timer timer)
      (error
       (message "qq: notification timer cancellation failed: %s"
                (error-message-string err)))
      (quit
       (message "qq: notification timer cancellation was interrupted")))))

(defun qq-notifications--cancel-owner-timer (owner)
  "Cancel the timer retained by OWNER without allowing failure to escape."
  (when-let* ((timer (plist-get owner :timer)))
    (qq-notifications--cancel-timer-value timer)))

(defun qq-notifications--forget-avatar-owner (owner)
  "Erase retained notification content and avatar state from retired OWNER."
  (when (plist-member owner :arguments)
    (setf (plist-get owner :arguments) nil
          (plist-get owner :avatar-keys) nil
          (plist-get owner :icon-file) nil)))

(defun qq-notifications--close-backend-id (id)
  "Close backend notification ID without mutating client ownership state."
  (when id
    (ignore-errors (notifications-close-notification id))))

(defun qq-notifications--revoke-async-work ()
  "Revoke and cancel all notification work owned by the current account."
  ;; Revoke owners before cancellation: a cancellation implementation may
  ;; synchronously run callbacks, which must already observe themselves stale.
  (cl-incf qq-notifications--generation)
  (let ((owners (append qq-notifications--delay-owners
                        qq-notifications--timeout-owners))
        (last-id qq-notifications--last-id))
    (qq-notifications--forget-avatar-owner qq-notifications--display-owner)
    (unless (eq qq-notifications--last-id-owner qq-notifications--display-owner)
      (qq-notifications--forget-avatar-owner qq-notifications--last-id-owner))
    (setq qq-notifications--delay-owners nil
          qq-notifications--timeout-owners nil
          qq-notifications--display-owner nil
          qq-notifications--last-id nil
          qq-notifications--last-id-owner nil)
    (dolist (owner owners)
      (qq-notifications--cancel-owner-timer owner))
    (when last-id
      (qq-notifications--close-backend-id last-id))))

(defun qq-notifications--history-buffer-owned-p (buffer)
  "Return non-nil when live BUFFER has an explicit history owner marker."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (eq qq-notifications--history-buffer-owner
             qq-notifications--history-buffer-owner-token))))

(defun qq-notifications--history-buffer-killed ()
  "Forget the tracked history buffer when its owning buffer is killed."
  (when (eq (current-buffer) qq-notifications--history-buffer)
    (setq qq-notifications--history-buffer nil)))

(defun qq-notifications--claim-history-buffer (buffer)
  "Mark BUFFER as notification history owned and return it."
  (with-current-buffer buffer
    (setq-local qq-notifications--history-buffer-owner
                qq-notifications--history-buffer-owner-token)
    (add-hook 'kill-buffer-hook
              #'qq-notifications--history-buffer-killed nil t))
  (setq qq-notifications--history-buffer buffer)
  buffer)

(defun qq-notifications--erase-history-buffer (buffer)
  "Erase account data from live notification history BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (widen)
        (erase-buffer)
        (set-buffer-modified-p nil)))))

(defun qq-notifications--dispose-history-buffer (buffer)
  "Close account-scoped notification history BUFFER, erasing on failure."
  (when (buffer-live-p buffer)
    (condition-case err
        (with-current-buffer buffer
          (let ((kill-buffer-query-functions nil)
                (buffer-offer-save nil))
            (set-buffer-modified-p nil)
            (unless (kill-buffer buffer)
              (error "Notification history buffer refused closure"))))
      (error
       (qq-notifications--erase-history-buffer buffer)
       (message "qq: notification history cleanup failed: %s"
                (error-message-string err)))
      (quit
       (qq-notifications--erase-history-buffer buffer)
       (message "qq: notification history cleanup was interrupted")))))

(defun qq-notifications--clear-history-ring ()
  "Remove every retained history object, then forget the history ring."
  (when (ring-p qq-notifications--history)
    (while (not (ring-empty-p qq-notifications--history))
      (ring-remove qq-notifications--history 0)))
  (setq qq-notifications--history nil))

(defun qq-notifications--reset-pass ()
  "Perform one notification cleanup pass under the resetting barrier."
  (qq-notifications--revoke-async-work)
  (clrhash qq-notifications--seen-anchors)
  (setq qq-notifications--seen-anchor-order nil)
  (qq-notifications--clear-history-ring)
  (let ((buffers
         (seq-filter
          #'qq-notifications--history-buffer-owned-p
          (delq nil
                (delete-dups
                 (list qq-notifications--history-buffer
                       (get-buffer
                        qq-notifications--history-buffer-name)))))))
    ;; Revoke the global owner before kill hooks can run reentrantly.
    (setq qq-notifications--history-buffer nil)
    (dolist (buffer buffers)
      (qq-notifications--dispose-history-buffer buffer)))
  ;; A kill hook may have retained or directly recreated account data.  The
  ;; outer reset's final pass repeats this boundary before lifting the barrier.
  (clrhash qq-notifications--seen-anchors)
  (setq qq-notifications--seen-anchor-order nil)
  (qq-notifications--clear-history-ring)
  (qq-notifications--revoke-async-work))

(defun qq-notifications-reset-session-state ()
  "Clear timers, history, and presentation belonging to the old QQ account."
  (let ((outermost-p (not qq-notifications--resetting-p)))
    (when outermost-p
      (setq qq-notifications--resetting-p t))
    (unwind-protect
        (qq-notifications--reset-pass)
      (when outermost-p
        (unwind-protect
            ;; Catch owners or buffers installed reentrantly by cancellation
            ;; and kill hooks while the barrier was already active.
            (qq-notifications--reset-pass)
          (setq qq-notifications--resetting-p nil))))))

(defun qq-notifications--history-ring ()
  "Return the notification history ring at its configured size."
  (unless (and (ring-p qq-notifications--history)
               (= (ring-size qq-notifications--history)
                  qq-notifications-history-ring-size))
    (let ((old (and (ring-p qq-notifications--history)
                    (ring-elements qq-notifications--history))))
      (setq qq-notifications--history
            (make-ring (max 1 qq-notifications-history-ring-size)))
      (dolist (entry (reverse (seq-take old qq-notifications-history-ring-size)))
        (ring-insert qq-notifications--history entry))))
  qq-notifications--history)

(defun qq-notifications--remember-anchor (anchor)
  "Remember notification ANCHOR while keeping deduplication state bounded."
  (puthash anchor t qq-notifications--seen-anchors)
  (push anchor qq-notifications--seen-anchor-order)
  (when (> (length qq-notifications--seen-anchor-order)
           qq-notifications--seen-anchor-limit)
    (let ((expired (car (last qq-notifications--seen-anchor-order))))
      (setq qq-notifications--seen-anchor-order
            (butlast qq-notifications--seen-anchor-order))
      (remhash expired qq-notifications--seen-anchors))))

(defun qq-notifications--session-muted-p (session)
  "Return non-nil when SESSION is in QQ's quiet/muted mode."
  (eq (alist-get 'muted-p session) t))

(defun qq-notifications--mention-breaks-mute-p (message)
  "Return non-nil when MESSAGE has a high-priority native QQ mention."
  (or (qq-state-message-mentions-self-p message)
      (and qq-notifications-at-all-breaks-mute
           (qq-state-message-mentions-all-p message))))

(defun qq-notifications--chat-observable-p (session-key)
  "Return non-nil when SESSION-KEY is selected on a focused frame."
  (seq-some
   (lambda (frame)
     (and (frame-live-p frame)
          (eq (frame-focus-state frame) t)
          (let* ((window (frame-selected-window frame))
                 (buffer (and (window-live-p window) (window-buffer window))))
            (and (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (and (derived-mode-p 'qq-chat-mode)
                        (equal qq-chat--session-key session-key)))))))
   (frame-list)))

(defun qq-notifications-message-notify-p (message)
  "Return non-nil when normalized MESSAGE should produce a notification."
  (let* ((session-key (alist-get 'session-key message))
         (session (and session-key (qq-state-session session-key)))
         (time (or (alist-get 'time message) 0)))
    (and session-key
         session
         (not (alist-get 'self-p message))
         (not (qq-state-message-recalled-p message))
         (or (not (numberp time))
             (<= (- (float-time) time) qq-notifications-max-message-age))
         (or (not (qq-notifications--session-muted-p session))
             (qq-notifications--mention-breaks-mute-p message))
         (not (qq-notifications--chat-observable-p session-key)))))

(defun qq-notifications--title (message session)
  "Return desktop notification title for MESSAGE in SESSION."
  (let ((sender (or (alist-get 'sender-name message)
                    (alist-get 'sender-id message)
                    "QQ"))
        (chat-title (or (alist-get 'title session)
                        (alist-get 'key session)
                        "QQ")))
    (if (eq (alist-get 'type session) 'group)
        (format "%s — %s" sender chat-title)
      sender)))

(defun qq-notifications--body (message)
  "Return compact desktop notification body for MESSAGE."
  (if (not qq-notifications-show-preview)
      "有新的未读消息"
    (let* ((preview (string-trim
                     (or (qq-state-message-preview message)
                         (alist-get 'raw-message message)
                         "[message]")))
           (prefix (cond
                    ((qq-state-message-mentions-self-p message) "@你  ")
                    ((qq-state-message-mentions-all-p message) "@全体成员  ")
                    (t ""))))
      (truncate-string-to-width
       (concat prefix preview)
       (max 1 qq-notifications-body-limit)
       nil nil "…"))))

(defun qq-notifications-open-message (session-key center)
  "Open SESSION-KEY and jump to exact typed history CENTER."
  (when (fboundp 'x-focus-frame)
    (ignore-errors (x-focus-frame (selected-frame))))
  (ignore-errors (raise-frame (selected-frame)))
  (qq-chat-open session-key)
  (when (and center (derived-mode-p 'qq-chat-mode))
    (qq-chat-goto-message center t)))

(defun qq-notifications--close (id &optional owner)
  "Close current desktop notification ID when optionally owned by OWNER."
  (when (and (equal id qq-notifications--last-id)
             (or (null owner) (eq owner qq-notifications--last-id-owner)))
    ;; Revoke client state before the backend close can run arbitrary hooks.
    (qq-notifications--forget-avatar-owner qq-notifications--last-id-owner)
    (setq qq-notifications--last-id nil
          qq-notifications--last-id-owner nil)
    (qq-notifications--close-backend-id id)
    t))

(defun qq-notifications--retire-timeouts-for-display (display-owner id)
  "Cancel timeouts belonging to DISPLAY-OWNER and notification ID."
  (dolist (owner (copy-sequence qq-notifications--timeout-owners))
    (when (and (eq display-owner (plist-get owner :display-owner))
               (equal id (plist-get owner :id)))
      (setq qq-notifications--timeout-owners
            (delq owner qq-notifications--timeout-owners))
      (qq-notifications--cancel-owner-timer owner))))

(defun qq-notifications--timeout-fired (owner)
  "Close the desktop notification owned by current timeout OWNER."
  (let ((display-owner (plist-get owner :display-owner))
        (id (plist-get owner :id)))
    (when (and (qq-notifications--owner-current-p
                owner qq-notifications--timeout-owners)
               (qq-notifications--display-id-current-p display-owner id))
      (setq qq-notifications--timeout-owners
            (delq owner qq-notifications--timeout-owners))
      (qq-notifications--close id display-owner))))

(defun qq-notifications--schedule-timeout (display-owner id)
  "Schedule timeout for ID owned by exact DISPLAY-OWNER."
  (when (qq-notifications--display-id-current-p display-owner id)
    (let ((owner (list :generation qq-notifications--generation
                       :display-owner display-owner
                       :id id :timer nil))
          timer)
      (push owner qq-notifications--timeout-owners)
      (condition-case err
          (setq timer
                (run-with-timer qq-notifications-timeout nil
                                #'qq-notifications--timeout-fired owner))
        (error
         (message "qq: failed to schedule notification timeout: %s"
                  (error-message-string err)))
        (quit
         (message "qq: notification timeout scheduling was interrupted")))
      (if (and timer
               (memq owner qq-notifications--timeout-owners)
               (qq-notifications--display-id-current-p display-owner id))
          (progn
            (setf (plist-get owner :timer) timer)
            owner)
        ;; RUN-WITH-TIMER itself can reenter reset/show before returning.
        (setq qq-notifications--timeout-owners
              (delq owner qq-notifications--timeout-owners))
        (when timer
          (qq-notifications--cancel-timer-value timer))
        nil))))

(defun qq-notifications--remove-history-record (record)
  "Remove exact history RECORD returned by the display transaction."
  (when-let* ((ring (car-safe record))
              (entry (cdr-safe record)))
    (when (ring-p ring)
      (when-let* ((index (cl-position entry (ring-elements ring) :test #'eq)))
        (ring-remove ring index)))))

(defun qq-notifications--record-history (owner message)
  "Record MESSAGE for exact display OWNER and return a rollback record."
  (when (qq-notifications--display-owner-current-p owner)
    (let ((ring (qq-notifications--history-ring))
          (entry (copy-tree message)))
      (when (qq-notifications--display-owner-current-p owner)
        (ring-insert ring entry)
        (let ((record (cons ring entry)))
          (if (qq-notifications--display-owner-current-p owner)
              record
            ;; RING-INSERT may itself run instrumented/reentrant Lisp.
            (qq-notifications--remove-history-record record)
            nil))))))

(defun qq-notifications--avatar-file (key)
  "Return the authoritative local avatar file for logical KEY, or nil."
  (when-let* ((resource (and key (qq-media--cached-resource key))))
    (qq-media--resource-image-file key resource)))

(defun qq-notifications--avatar-icon (keys)
  "Return a cached circular notification icon for avatar KEYS.

The first key supplies the main avatar; an optional second key supplies
the lower-right sender badge.  A missing badge never replaces the main
avatar with the sender."
  (when-let* ((main (qq-notifications--avatar-file (car keys))))
    (let* ((badge (qq-notifications--avatar-file (cadr keys)))
           (main-mime (appkit-media-image-mime-type main))
           (badge-mime (and badge (appkit-media-image-mime-type badge))))
      (if (not main-mime)
          main
        (unless badge-mime (setq badge nil))
        (let* ((identity
                (mapcar
                 (lambda (file)
                   (when file
                     (let ((attributes (file-attributes file)))
                       (list file
                             (file-attribute-modification-time attributes)
                             (file-attribute-size attributes)
                             (file-attribute-inode-number attributes)))))
                 (list main badge)))
               (directory
                (expand-file-name "notification-icons" qq-media-cache-directory))
               (file
                (expand-file-name
                 (concat "avatar-1-" (secure-hash 'sha256
                                                (prin1-to-string identity))
                         ".svg")
                 directory)))
          (unless (file-exists-p file)
            (make-directory directory t)
            (let* ((svg (svg-create 128 128))
                   (size (if badge 116 128))
                   (radius (/ size 2))
                   (temporary (make-temp-file
                               (expand-file-name ".avatar-" directory))))
              (unwind-protect
                  (progn
                    (svg-circle
                     (svg-clip-path svg :id "main") radius radius radius)
                    (svg-embed
                     svg main main-mime nil
                     :x 0 :y 0 :width size :height size
                     :preserveAspectRatio "xMidYMid slice"
                     :clip-path "url(#main)")
                    (when badge
                      (svg-circle svg 101 101 30 :fill "#f8fafc")
                      (svg-circle
                       (svg-clip-path svg :id "sender") 101 101 27)
                      (svg-embed
                       svg badge badge-mime nil
                       :x 74 :y 74 :width 54 :height 54
                       :preserveAspectRatio "xMidYMid slice"
                       :clip-path "url(#sender)"))
                    (with-temp-file temporary
                      (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
                      (svg-print svg))
                    (rename-file temporary file t))
                (when (file-exists-p temporary)
                  (delete-file temporary)))))
          file)))))

(defun qq-notifications--prepare-avatar (owner message session)
  "Prepare OWNER's notification avatar for MESSAGE in SESSION.

Reuse QQ's existing asynchronous avatar acquisition.  Explicit desktop
image arguments retain priority and do not start unused avatar requests."
  (unless (or (plist-member qq-notifications-extra-args :app-icon)
              (plist-member qq-notifications-extra-args :image-path)
              (plist-member qq-notifications-extra-args :image-data))
    (condition-case err
        (let* ((group-p (eq (alist-get 'type session) 'group))
               (sender-key (qq-media-message-avatar-cache-key message))
               (main-key (if group-p
                             (qq-media-session-avatar-cache-key session)
                           sender-key))
               (keys (and main-key
                          (if group-p (list main-key sender-key)
                            (list main-key)))))
          (setf (plist-get owner :avatar-keys) keys)
          (when (and keys (qq-account-get (qq-runtime-current-account-id)))
            (when (and group-p
                       (not (qq-notifications--avatar-file main-key)))
              (qq-media-group-avatar-image (alist-get 'target-id session)))
            (when (and sender-key
                       (qq-notifications--display-owner-current-p owner)
                       (not (qq-notifications--avatar-file sender-key)))
              (qq-media-message-avatar-image message)))
          (when (qq-notifications--display-owner-current-p owner)
            (qq-notifications--avatar-icon keys)))
      (error
       (message "qq: notification avatar preparation failed: %s"
                (error-message-string err))
       nil))))

(defun qq-notifications--refresh-avatar (&optional key)
  "Update the current notification after avatar cache KEY becomes ready.

Replace its backend ID without adding history or restarting its timeout.
Exact display ownership prevents late cache updates from reviving a closed
notification or crossing an account reset."
  (let* ((owner qq-notifications--last-id-owner)
         (id qq-notifications--last-id)
         (keys (plist-get owner :avatar-keys)))
    (when (and keys
               (qq-notifications--display-id-current-p owner id)
               (or (null key) (member key keys)))
      (condition-case err
          (when-let* ((icon (qq-notifications--avatar-icon keys)))
            (when (and (qq-notifications--display-id-current-p owner id)
                       (not (equal icon (plist-get owner :icon-file))))
              (let ((previous (plist-get owner :icon-file))
                    replacement-id
                    committed-p)
                ;; Publish before the backend can reenter.  A nested refresh
                ;; may advance the same display to an even newer avatar.
                (setf (plist-get owner :icon-file) icon)
                (unwind-protect
                    (progn
                      (setq replacement-id
                            (apply #'notifications-notify
                                   :replaces-id id :app-icon icon
                                   (plist-get owner :arguments)))
                      (setq committed-p
                            (and (equal replacement-id id)
                                 (qq-notifications--display-id-current-p
                                  owner id))))
                  (unless committed-p
                    (when (and replacement-id
                               (not
                                (qq-notifications--backend-id-authoritatively-owned-p
                                 replacement-id)))
                      (qq-notifications--close-backend-id replacement-id))
                    (when (and (qq-notifications--display-id-current-p owner id)
                               (equal icon (plist-get owner :icon-file)))
                      (setf (plist-get owner :icon-file) previous)))))))
        (error
         (message "qq: notification avatar update failed: %s"
                  (error-message-string err)))
        (quit
         (message "qq: notification avatar update was interrupted"))))))

(defun qq-notifications--show (message)
  "Show one desktop notification for normalized MESSAGE."
  (when (and qq-notifications-mode
             (not qq-notifications--resetting-p))
    (let* ((previous-owner qq-notifications--display-owner)
           ;; Preallocate mutable fields so plist updates retain token identity.
           (owner (list :generation qq-notifications--generation
                        :avatar-keys nil :arguments nil :icon-file nil))
           backend-id
           history-record
           committed-p
           previous-retired-p
           result)
      ;; Install the token before even the policy predicate: instrumented
      ;; predicates can recursively display another notification in the same
      ;; account generation, and that nested operation must win.
      (setq qq-notifications--display-owner owner)
      (unwind-protect
          (condition-case err
              (catch 'abort
                (unless (qq-notifications-message-notify-p message)
                  (when (qq-notifications--display-owner-current-p owner)
                    (setq qq-notifications--display-owner previous-owner))
                  (throw 'abort nil))
                (unless (qq-notifications--display-owner-current-p owner)
                  (throw 'abort nil))
                (let* ((session-key (alist-get 'session-key message))
                       (session (qq-state-session session-key))
                       (center (qq-message-history-locator message))
                       (icon (qq-notifications--prepare-avatar
                              owner message session))
                       (custom-close
                        (plist-get qq-notifications-extra-args :on-close))
                       (args
                        (append
                         (when icon (list :app-icon icon))
                         (list
                          :app-name "emacs-qq"
                          :title (qq-notifications--title message session)
                          :body (qq-notifications--body message)
                          :urgency
                          (if (qq-state-message-mentions-self-p message)
                              "critical"
                            "normal")
                          :timeout -1
                          :actions '("default" "Open")
                          :on-action
                          (lambda (&rest _)
                            (when (qq-notifications--display-owner-current-p
                                   owner)
                              (qq-notifications-open-message
                               session-key center)))
                          :on-close
                          (lambda (id reason)
                            (when (qq-notifications--display-id-current-p owner id)
                              (setq qq-notifications--last-id nil
                                    qq-notifications--last-id-owner nil)
                              (qq-notifications--forget-avatar-owner owner)
                              (qq-notifications--retire-timeouts-for-display
                               owner id))
                            (when custom-close
                              (funcall custom-close id reason))))
                         qq-notifications-extra-args)))
                  (unless (qq-notifications--display-owner-current-p owner)
                    (throw 'abort nil))
                  ;; Replacements reuse the original callback registrations;
                  ;; notifications.el otherwise retains duplicate ID entries.
                  (setf (plist-get owner :arguments)
                        (cl-loop for (name value) on args by #'cddr
                                 unless (memq name '(:on-action :on-close))
                                 append (list name value))
                        (plist-get owner :icon-file) icon)
                  (when qq-notifications--last-id
                    (let ((old-id qq-notifications--last-id)
                          (old-owner qq-notifications--last-id-owner))
                      (qq-notifications--retire-timeouts-for-display
                       old-owner old-id)
                      (unless (qq-notifications--display-owner-current-p owner)
                        (throw 'abort nil))
                      (qq-notifications--close old-id old-owner)
                      (setq previous-retired-p t)
                      (unless (qq-notifications--display-owner-current-p owner)
                        (throw 'abort nil))))
                  (setq backend-id (apply #'notifications-notify args))
                  ;; A nested display/reset during the backend call owns all
                  ;; client state.  The outer backend id is now an orphan.
                  (unless (qq-notifications--display-owner-current-p owner)
                    (throw 'abort nil))
                  (unless backend-id
                    (throw 'abort nil))
                  (setq history-record
                        (qq-notifications--record-history owner message))
                  (unless (and history-record
                               (qq-notifications--display-owner-current-p owner))
                    (throw 'abort nil))
                  (setq qq-notifications--last-id backend-id
                        qq-notifications--last-id-owner owner
                        committed-p t)
                  (when qq-notifications-timeout
                    (qq-notifications--schedule-timeout owner backend-id))
                  ;; Timer setup and avatar replacement can both reenter.
                  ;; Also catch a cache completion during the initial D-Bus call.
                  (when (qq-notifications--display-id-current-p
                         owner backend-id)
                    (qq-notifications--refresh-avatar)
                    (when (qq-notifications--display-id-current-p
                           owner backend-id)
                      (setq result backend-id)))))
            (error
             (message "qq: desktop notification failed: %s"
                      (error-message-string err)))
            (quit
             (message "qq: desktop notification was interrupted")))
        (unless committed-p
          (qq-notifications--forget-avatar-owner owner)
          (when history-record
            (qq-notifications--remove-history-record history-record))
          (when backend-id
            ;; A backend may reuse an id while synchronously displaying a
            ;; nested notification.  Only close an actual orphan: the stale
            ;; outer operation must not close the nested operation's id.
            (unless (qq-notifications--backend-id-authoritatively-owned-p
                     backend-id)
              (qq-notifications--close-backend-id backend-id)))
          (when (qq-notifications--display-owner-current-p owner)
            (setq qq-notifications--display-owner
                  (unless previous-retired-p previous-owner))))
        ;; Committed outer state can become stale while scheduling its timeout;
        ;; a nested operation has already retired/closed it in that case.
        (unless (and committed-p
                     (qq-notifications--display-id-current-p owner backend-id))
          (setq result nil)))
      result)))

(defun qq-notifications--deliver-delayed (owner message)
  "Deliver MESSAGE only while delayed OWNER belongs to this account."
  (when (qq-notifications--owner-current-p
         owner qq-notifications--delay-owners)
    (setq qq-notifications--delay-owners
          (delq owner qq-notifications--delay-owners))
    (if-let* ((account-id (plist-get owner :account-id)))
        (when (qq-account-get account-id)
          (qq-runtime-with-account account-id
            (qq-notifications--show message)))
      (qq-notifications--show message))))

(defun qq-notifications--schedule-delayed (message)
  "Schedule one account-owned delayed notification for MESSAGE."
  (when (and qq-notifications-mode
             (not qq-notifications--resetting-p))
    (let ((owner (list :generation qq-notifications--generation
                       :account-id (qq-runtime-current-account-id)
                       :timer nil))
          timer)
      (push owner qq-notifications--delay-owners)
      (condition-case err
          (setq timer
                (run-with-timer qq-notifications-delay nil
                                #'qq-notifications--deliver-delayed
                                owner (copy-tree message)))
        (error
         (message "qq: failed to schedule delayed notification: %s"
                  (error-message-string err)))
        (quit
         (message "qq: delayed notification scheduling was interrupted")))
      (if (and timer
               (qq-notifications--owner-current-p
                owner qq-notifications--delay-owners))
          (progn
            (setf (plist-get owner :timer) timer)
            owner)
        (setq qq-notifications--delay-owners
              (delq owner qq-notifications--delay-owners))
        (when timer
          (qq-notifications--cancel-timer-value timer))
        nil))))

(defun qq-notifications--handle-state-change (event)
  "Schedule a desktop notification for one incoming state EVENT."
  (unless qq-notifications--resetting-p
    (when (eq (plist-get event :type) 'reset)
      (qq-notifications-reset-session-state))
    (when (and (eq (plist-get event :type) 'message)
               (eq (plist-get event :mutation) 'create)
               (eq (plist-get event :source) 'event))
      (when-let* ((message (plist-get event :message))
                  (anchor (qq-state-message-anchor message)))
        (unless (gethash anchor qq-notifications--seen-anchors)
          (qq-notifications--remember-anchor anchor)
          (if (> qq-notifications-delay 0)
              (qq-notifications--schedule-delayed message)
            (qq-notifications--show message)))))))

;;;###autoload
(define-minor-mode qq-notifications-mode
  "Toggle telega-style desktop notifications for emacs-qq."
  :global t
  :group 'qq-notifications
  (if qq-notifications-mode
      (progn
        (add-hook 'qq-state-change-hook #'qq-notifications--handle-state-change)
        (add-hook 'qq-media-cache-update-hook
                  #'qq-notifications--refresh-avatar))
    (remove-hook 'qq-state-change-hook #'qq-notifications--handle-state-change)
    (remove-hook 'qq-media-cache-update-hook #'qq-notifications--refresh-avatar)
    (qq-notifications--revoke-async-work)))

;;;###autoload
(defun qq-notifications-history ()
  "Show recent emacs-qq desktop notifications."
  (interactive)
  (unless qq-notifications--resetting-p
    (let* ((tracked
            (and (qq-notifications--history-buffer-owned-p
                  qq-notifications--history-buffer)
                 qq-notifications--history-buffer))
           (fixed (get-buffer qq-notifications--history-buffer-name))
           (buffer
            (or tracked
                (and (qq-notifications--history-buffer-owned-p fixed) fixed)
                ;; Never claim or erase an ordinary user buffer that happens
                ;; to use our fallback name.
                (if fixed
                    (generate-new-buffer qq-notifications--history-buffer-name)
                  (get-buffer-create qq-notifications--history-buffer-name)))))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (special-mode)
          ;; `special-mode' kills local variables and hooks, so install the
          ;; explicit ownership marker only after changing major modes.
          (qq-notifications--claim-history-buffer buffer)
          (dolist (message (ring-elements (qq-notifications--history-ring)))
            (let* ((generation qq-notifications--generation)
                   (session-key (alist-get 'session-key message))
                   (session (qq-state-session session-key))
                   (center (qq-message-history-locator message))
                   (title (qq-notifications--title message session))
                   (body (qq-notifications--body message)))
              (insert-text-button
               title
               'follow-link t
               'keymap appkit-ui-button-map
               'action (lambda (_button)
                         (when (and (not qq-notifications--resetting-p)
                                    (= generation
                                       qq-notifications--generation))
                           (qq-notifications-open-message
                            session-key center))))
              (insert (format "  %s\n" body))))))
      (pop-to-buffer buffer))))

(provide 'qq-notifications)

;;; qq-notifications.el ends here
