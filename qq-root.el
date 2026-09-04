;;; qq-root.el --- Root buffer for emacs-qq -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; Persistent keyed-EWOC root view.  Session rows use appkit's shared list
;; reconciliation and one-line presentation infrastructure.

;;; Code:

(require 'cl-lib)
(require 'ewoc)
(require 'seq)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-projection)
(require 'appkit-transaction)
(require 'appkit-presentation)
(require 'appkit-position)
(require 'appkit-ewoc)
(require 'qq-core)
(require 'qq-chat)
(require 'qq-account)
(require 'qq-media)
(require 'qq-login)
(require 'qq-runtime)
(require 'qq-state)
(require 'qq-protocol)

(autoload 'qq-user-open "qq-user" nil t)
(autoload 'qq-group-open "qq-group" nil t)
(autoload 'qq-contacts-open "qq-contacts" nil t)
(autoload 'qq-group-requests-open "qq-group-requests" nil t)
(declare-function qq-user-open "qq-user" (user-id))
(declare-function qq-group-open "qq-group" (group-id))
(declare-function qq-contacts-open "qq-contacts" ())
(declare-function qq-group-requests-open "qq-group-requests" ())
(declare-function qq-root-transient "qq-transient" ())

(defconst qq-root-buffer-name "*qq-root*"
  "Name of the emacs-qq root buffer.")

(defconst qq-root--activity-icon-slot-width 4
  "Reserved icon slot width for one-line session rows.")

(defvar-local qq-root--ewoc nil
  "Persistent EWOC containing root metadata and session rows.")

(defvar-local qq-root--node-table nil
  "Stable root entry key to EWOC node table.")

(defvar-local qq-root--fill-column nil
  "Last root width measured from a window that actually displayed it.")

(defvar-local qq-root--scope nil
  "Stable account ID shown by this root, or `gateway' for account management.")

(cl-defstruct (qq-root--entry
               (:constructor qq-root--entry-create))
  key
  type
  text
  face
  account
  session
  width)

(defun qq-root--selected-window ()
  "Return the selected window when it displays the current root buffer."
  (let ((win (selected-window)))
    (and (window-live-p win)
         (eq (window-buffer win) (current-buffer))
         win)))

(defun qq-root--display-window ()
  "Return the widest live window displaying the current root buffer."
  (let ((best nil)
        (best-width -1))
    (dolist (win (get-buffer-window-list (current-buffer) nil t) best)
      (let ((width (if (window-live-p win)
                       (window-width win 'remap)
                     -1)))
        (when (> width best-width)
          (setq best win
                best-width width))))))

(defun qq-root--compute-fill-column (&optional window)
  "Compute root row width from live WINDOW, or return nil."
  (when-let* ((win (or window (qq-root--display-window)))
              (width (appkit-geometry-window-width
                      win qq-root-auto-fill-margin-columns)))
    (max 60 width)))

(defun qq-root--stable-fill-column ()
  "Return a stable width for the next root reconciliation.

Measure the selected root window when possible.  A passive background update
reuses the last measured width instead
of accidentally borrowing the selected chat window."
  (or (when-let* ((win (qq-root--selected-window)))
        (qq-root--compute-fill-column win))
      (and (integerp qq-root--fill-column)
           (> qq-root--fill-column 0)
           qq-root--fill-column)
      (qq-root--compute-fill-column (qq-root--display-window))
      80))

(defun qq-root--buffer-width ()
  "Return current root row width in columns."
  (max 60 (or qq-root--fill-column
              (setq-local qq-root--fill-column
                          (qq-root--stable-fill-column)))))

(defun qq-root--format-time (timestamp)
  "Return display string for TIMESTAMP."
  (if (and timestamp (> timestamp 0))
      (format-time-string "%m-%d %H:%M" (seconds-to-time timestamp))
    ""))

(defun qq-root--account-title (account self-info)
  "Return the user-facing title for selected ACCOUNT and SELF-INFO.

SELF-INFO may lag behind a Gateway account switch.  Its nickname is therefore
used only when its QQ number agrees with ACCOUNT."
  (let* ((account-uin (alist-get 'uin account))
         (self-uin (alist-get 'user_id self-info))
         (self-current-p
          (and self-info
               account-uin
               (equal account-uin self-uin)))
         (nickname (and self-current-p
                        (alist-get 'nickname self-info)))
         (label (alist-get 'label account))
         (uin (or account-uin
                  (and self-current-p self-uin)))
         (title (or nickname label uin "Unbound account")))
    (concat title
            (if (and uin (not (equal title uin)))
                (format " (%s)" uin)
              ""))))

(defun qq-root--header-line ()
  "Return dynamic header line for the root buffer."
  (let* ((gateway-p (eq qq-root--scope 'gateway))
         (self-info (and (not gateway-p) (qq-state-self-info)))
         (status
          (if gateway-p
              (qq-server-state)
            (qq-state-connection-status)))
         (owner (and (not gateway-p) qq-root--scope))
         (account (qq-account-get owner))
         (accounts (qq-account-list))
         (online-count
          (cl-count-if
           (lambda (snapshot)
             (equal (alist-get 'phase snapshot) "online"))
           accounts))
         (account-count (length accounts))
         (account-summary
          (cond
           (gateway-p "account manager")
           (account
            (format "%s — %s"
                    (qq-root--account-title account self-info)
                    (alist-get 'phase account)))
           (t "account no longer managed")))
         (registry-summary
          (and (> account-count 1)
               (format " · %d/%d online" online-count account-count))))
    (format " emacs-qq  [%s]  %s%s"
            status account-summary (or registry-summary ""))))

(defun qq-root--recent-sessions ()
  "Return the ordered sessions in the authoritative recent projection."
  (mapcar
   (lambda (session-key)
     (or (qq-state-session session-key)
         (error "qq: recent projection references missing session %s"
                session-key)))
   (qq-state-recent-session-keys)))

(defun qq-root--activity-metrics (&optional sessions)
  "Return root activity metrics for SESSIONS or current recent projection."
  (let* ((sessions (or sessions (qq-root--recent-sessions)))
         (badges-complete-p
          (cl-every (lambda (session)
                      (let ((badge (alist-get 'unread-badge-count session)))
                        (and (integerp badge) (>= badge 0))))
                    sessions))
         (unread (and badges-complete-p
                      (cl-count-if
                       (lambda (session)
                         (> (alist-get 'unread-badge-count session) 0))
                       sessions)))
         (important (and badges-complete-p
                         (cl-count-if #'qq-root--session-important-unread-p
                                      sessions)))
         (muted (and badges-complete-p
                     (cl-count-if
                      (lambda (session)
                        (and (qq-root--session-muted-p session)
                             (> (alist-get 'unread-badge-count session) 0)))
                      sessions)))
         (dms (cl-count-if (lambda (session)
                             (not (eq (alist-get 'type session) 'group)))
                           sessions)))
    (list :all (length sessions)
          :unread unread
          :important important
          :muted muted
          :dms dms)))

(defun qq-root--filter-chip (label count &optional active)
  "Return one root filter chip for LABEL and exact COUNT.

Render `?' instead of a partial number when COUNT is nil."
  (format "[%s%s:%s]"
          (if active "*" "")
          label
          (if (integerp count) (number-to-string count) "?")))

(defun qq-root--filters-line (&optional sessions)
  "Return filter-chip line for SESSIONS or current state."
  (let ((metrics (qq-root--activity-metrics sessions)))
    (string-join
     (list (qq-root--filter-chip "Main" (plist-get metrics :all) t)
           (qq-root--filter-chip "Important" (plist-get metrics :important))
           (qq-root--filter-chip "Muted" (plist-get metrics :muted))
           (qq-root--filter-chip "DMs" (plist-get metrics :dms))
           "[activity sort:recent]")
     "  ")))

(defun qq-root--mode-divider-line ()
  "Return a divider line carrying the active root mode marker."
  (let* ((label "(activity/all)")
         (width (max (qq-root--buffer-width) (+ 8 (string-width label))))
         (filler (max 0 (- width (string-width label) 2)))
         (left (/ filler 2))
         (right (- filler left)))
    (concat "_/"
            (make-string left ?-)
            label
            (make-string right ?-))))

(defun qq-root--session-context-label (session)
  "Return one-line context label for SESSION."
  (let ((label (or (alist-get 'title session)
                   (alist-get 'key session)
                   "session")))
    (if (eq (alist-get 'pinned session) t)
        (concat "\N{PUSHPIN} " label)
      label)))

(defun qq-root--session-muted-p (session)
  "Return non-nil when SESSION has QQ message notifications muted."
  (eq (alist-get 'muted-p session) t))

(defun qq-root--session-mention-kinds (session)
  "Return unread native mention kinds represented by SESSION."
  (delq nil
        (list (and (or (alist-get 'unread-at-me-message-id session)
                       (alist-get 'unread-at-me-message-seq session))
                   'at-me)
              (and (or (alist-get 'unread-at-all-message-id session)
                       (alist-get 'unread-at-all-message-seq session))
                   'at-all))))

(defun qq-root--session-important-unread-p (session)
  "Return non-nil when SESSION has unmuted unread or an unread mention."
  (or (and (> (or (alist-get 'unread-badge-count session) 0) 0)
           (not (qq-root--session-muted-p session)))
      (qq-root--session-mention-kinds session)))

(defun qq-root--format-unread-count (count)
  "Format positive badge COUNT using QQ's visible 99+ cap."
  (if (> count 99) "99+" (number-to-string count)))

(defun qq-root--session-unread-trail (session)
  "Return SESSION's propertized unread trail for the title brackets.

Like telega's chat unread trail, the active adapter's complete named Badge
Count follows the title and uses a muted or unmuted face.  Mention kinds
remain independently prominent even when the badge is unavailable or the
session muted."
  (let* ((badge (alist-get 'unread-badge-count session))
         (unread (and (integerp badge) (> badge 0) badge))
         (mentions (qq-root--session-mention-kinds session))
         (count-face (if (qq-root--session-muted-p session)
                         'qq-root-muted-count
                       'qq-root-unmuted-count)))
    (string-join
     (delq nil
           (list (and unread
                      (propertize (qq-root--format-unread-count unread)
                                  'face count-face))
                 (and (memq 'at-me mentions)
                      (propertize "@" 'face 'qq-root-mention-count))
                 (and (memq 'at-all mentions)
                      (propertize "@all" 'face 'qq-root-mention-count))))
     " ")))

(defun qq-root--session-icon-face (session)
  "Return the icon face for SESSION."
  (cond
   ((qq-root--session-muted-p session) 'shadow)
   ((eq (alist-get 'type session) 'group) 'font-lock-keyword-face)
   (t 'font-lock-variable-name-face)))

(defun qq-root--insert-session-icon (session)
  "Insert inline avatar/icon for SESSION."
  (let ((start (point)))
    (insert (qq-media-session-avatar-display-string session))
    (add-text-properties
     start
     (point)
     (list 'face (qq-root--session-icon-face session)
           'help-echo (format "Open avatar for %s"
                              (or (alist-get 'title session)
                                  (alist-get 'key session)
                                  "session"))))))

(defun qq-root--session-last-message (session)
  "Return SESSION's cached latest message object, or nil."
  (when-let* ((session-key (alist-get 'key session)))
    (let ((identities
           (delete-dups
            (delq nil
                  (list (alist-get 'last-message-id session)
                        (alist-get 'last-message-local-id session))))))
      (seq-find
       (lambda (message)
         (seq-some
          (lambda (identity)
            (member identity
                    (delq nil
                          (list (qq-state-message-anchor message)
                                (alist-get 'server-id message)
                                (alist-get 'id message)
                                (alist-get 'local-id message)))))
          identities))
       (qq-state-session-messages session-key)))))

(defun qq-root--session-preview-label-face (session identity message)
  "Return the sender-label face for SESSION's latest MESSAGE.

Prefer MESSAGE's stable sender identity so the root and timeline agree.  A
session-only preview uses summary IDENTITY; its visible SENDER is a last-resort
fallback when the protocol supplied no identity."
  (cond
   ((qq-chat--present-string (alist-get 'sender-name message))
    (qq-chat--message-title-face message))
   ((eq (alist-get 'last-message-self-p session) t)
    (if-let* ((color-face (appkit-name-color-face identity)))
        (list color-face 'qq-msg-self-title)
      'qq-msg-self-title))
   ((when-let* ((color-face (appkit-name-color-face identity)))
      (list color-face 'qq-msg-user-title)))
   (t 'qq-msg-user-title)))

(defun qq-root--session-preview-model (session)
  "Return the Appkit one-line preview for SESSION.

Peer chat-actions take priority over the last message.  Group previews identify
an ordinary message's sender; private previews do so only for outgoing
messages, since the session title already identifies an incoming peer."
  (let* ((session-key (alist-get 'key session))
         (action-text
          (and qq-chat-show-peer-actions
               session-key
               (qq-state-preview-one-line
                (qq-state-action-text session-key))))
         (preview
          (qq-state-preview-one-line
           (alist-get 'last-message-preview session)))
         (sender
          (qq-state-preview-one-line
           (alist-get 'last-message-sender-name session)))
         (identity
          (or (qq-chat--present-string
               (alist-get 'last-message-sender-id session))
              sender))
         (message (qq-root--session-last-message session))
         (show-sender-p
          (and (not (string-empty-p sender))
               (pcase (alist-get 'type session)
                 ('group t)
                 ('private (eq (alist-get 'last-message-self-p session) t))
                 (_ nil)))))
    (if (and (stringp action-text) (not (string-empty-p action-text)))
        (appkit-ui-one-line-preview-create
         :text (concat (or qq-chat-action-prefix ".. ") action-text))
      (let ((label
             (and show-sender-p
                  (not (string-empty-p preview))
                  sender)))
        (qq-media-message-one-line-preview
         message preview
         :label label
         :separator (and label ":")
         :label-face
         (and label
              (qq-root--session-preview-label-face
               session identity message)))))))

(defun qq-root--session-one-line-row (session)
  "Return one-line row model for SESSION."
  (let* ((session-key (alist-get 'key session))
         (unread (or (alist-get 'unread-badge-count session) 0))
         (muted (qq-root--session-muted-p session))
         (important (qq-root--session-important-unread-p session))
         (preview (qq-root--session-preview-model session)))
    (appkit-presentation-one-line-row-create
     :icon-inserter (lambda ()
                      (qq-root--insert-session-icon session))
     :context (qq-root--session-context-label session)
     :context-trail (qq-root--session-unread-trail session)
     :preview preview
     ;; Unread activity lives in the title trail.  Keep a dedicated time-tail
     ;; face (meant for a trailing status glyph) off the plain timestamp.
     :time (qq-root--format-time (alist-get 'last-message-time session))
     :time-face 'shadow
     :time-tail-face nil
     :line-properties
     (list 'qq-root-row-type 'session
           'qq-root-session-key session-key
           'qq-root-badge-count unread
           'qq-root-has-unread (and (> unread 0) t)
           'qq-root-muted-p muted
           'qq-root-has-important-unread (and important t))
     :help-echo (format "Open %s%s"
                        session-key
                        (if muted " (message notifications muted)" "")))))

(defun qq-root--insert-session-line (session)
  "Insert one session row for SESSION."
  (appkit-presentation-insert-one-line-row
   (qq-root--session-one-line-row session)
   :indent 2
   :width (qq-root--buffer-width)
   :icon-slot-width qq-root--activity-icon-slot-width
   :context-width-spec '(0.32 16 30)))

(defun qq-root--session-entry-key (session-key)
  "Return the stable root entry key for SESSION-KEY."
  (cons 'session session-key))

(defun qq-root--entry-printer (entry)
  "Insert one persistent root ENTRY."
  (pcase (qq-root--entry-type entry)
    ('note
     (appkit-presentation-insert-note-line (qq-root--entry-text entry)
                                           :face (qq-root--entry-face entry)))
    ('blank (insert "\n"))
    ('login (qq-login-insert-view (qq-root--entry-text entry)))
    ('account
     (let* ((account (qq-root--entry-account entry))
            (account-id (alist-get 'account_id account))
            (label (or (alist-get 'label account)
                       (alist-get 'uin account)
                       account-id))
            (uin (alist-get 'uin account))
            (phase (alist-get 'phase account))
            (start (point)))
       (insert (format "  %-16s  %s%s\n"
                       phase label
                       (if (and uin (not (equal label uin)))
                           (format " (%s)" uin)
                         "")))
       (add-text-properties
        start (point)
        (list 'qq-root-row-type 'account
              'qq-root-account-id account-id
              'mouse-face 'highlight
              'help-echo "Open this account's persistent QQ view"))))
    ('session (qq-root--insert-session-line (qq-root--entry-session entry)))
    (type (error "qq: unknown root entry type %S" type))))

(defun qq-root--project-gateway-entries ()
  "Project every managed account into the Gateway manager root."
  (let ((accounts (qq-account-list)))
    (append
     (list
      (qq-root--entry-create
       :key 'accounts-heading :type 'note
       :text (format "Managed accounts: %d" (length accounts))
       :face 'font-lock-doc-face)
      (qq-root--entry-create :key 'accounts-gap :type 'blank))
     (mapcar
      (lambda (account)
        (qq-root--entry-create
         :key (cons 'account (alist-get 'account_id account))
         :type 'account
         :account account))
      accounts)
     (unless accounts
       (list
        (qq-root--entry-create
         :key 'no-accounts :type 'note
         :text "No managed QQ accounts.  Start login to create one.")))
     (when-let* ((login (qq-login-view-model)))
       (list
        (qq-root--entry-create :key 'login-gap :type 'blank)
        (qq-root--entry-create
         :key 'login :type 'login :text login))))))

(defun qq-root--project-account-entries ()
  "Project the current account state into stable-keyed root entries."
  (let* ((sessions (qq-root--recent-sessions))
         (width (qq-root--buffer-width))
         (metadata
          (list
           (qq-root--entry-create
            :key 'filters :type 'note
            :text (qq-root--filters-line sessions)
            :face 'font-lock-doc-face)
           (qq-root--entry-create
            :key 'divider :type 'note
            :text (qq-root--mode-divider-line) :face 'shadow)
           (qq-root--entry-create :key 'metadata-gap :type 'blank))))
    (append
     metadata
     (mapcar
      (lambda (session)
        (qq-root--entry-create
         :key (qq-root--session-entry-key (alist-get 'key session))
         :type 'session
         :session session
         :width width))
      sessions)
     (unless sessions
       (list
        (qq-root--entry-create
         :key 'empty :type 'note :text "No recent conversations available yet."))))))

(defun qq-root--project-entries ()
  "Project this root buffer's explicit account or Gateway scope."
  (if (eq qq-root--scope 'gateway)
      (qq-root--project-gateway-entries)
    (unless (and (stringp qq-root--scope)
                 (equal qq-root--scope qq-runtime--account-id))
      (error "qq: account root has no stable account scope"))
    (qq-root--project-account-entries)))

(defun qq-root--session-entry-keys ()
  "Return stable entry keys for all current sessions."
  (unless (eq qq-root--scope 'gateway)
    (mapcar (lambda (session)
              (qq-root--session-entry-key (alist-get 'key session)))
            (qq-root--recent-sessions))))

(defun qq-root--render (surface _model change)
  "Render CHANGE in SURFACE, preserving stable rows and semantic position."
  (unless (ewoc-p qq-root--ewoc)
    (error "qq: root surface is not initialized"))
  (let* ((entries (appkit-projection-change-keys change))
         (geometry-p (appkit-projection-change-geometry-p change))
         (position (appkit-projection-change-position change))
         (reconcile-p (or (appkit-projection-change-full-p change) geometry-p)))
    (when (or reconcile-p entries position)
      (appkit-with-content-update surface
        (let ((snapshot
               (appkit-position-capture
                :anchor-property 'qq-root-session-key
                :preserve-window-start t)))
          (when geometry-p
            (setq-local qq-root--fill-column (qq-root--stable-fill-column)))
          (with-silent-modifications
            (if reconcile-p
                (setq qq-root--node-table
                      (appkit-ewoc-reconcile
                       qq-root--ewoc
                       (qq-root--project-entries)
                       #'qq-root--entry-key
                       :force-keys (qq-root--session-entry-keys)))
              (dolist (key entries)
                (appkit-ewoc-invalidate-key
                 qq-root--ewoc qq-root--node-table key))))
          (when snapshot
            (appkit-position-restore snapshot)))))
    (when (appkit-projection-change-frame-p change)
      (force-mode-line-update)))
  nil)

(defun qq-root--line-property (property &optional pos)
  "Return text PROPERTY at POS or the beginning of POS's line.

When POS is nil, use point."
  (let ((probe (or pos (point))))
    (or (get-text-property probe property)
        (get-text-property
         (save-excursion
           (goto-char probe)
           (line-beginning-position))
         property))))

(defun qq-root--session-key-at-point (&optional pos)
  "Return root session key at POS, or current point when POS is nil."
  (qq-root--line-property 'qq-root-session-key pos))

(defun qq-root--account-id-at-point (&optional pos)
  "Return managed account ID at POS, or nil."
  (qq-root--line-property 'qq-root-account-id pos))

(defun qq-root--session-at-point ()
  "Return root session object at point, or nil."
  (when-let* ((session-key (qq-root--session-key-at-point)))
    (qq-state-session session-key)))

(defun qq-root-open-at-point ()
  "Open the session or managed account at point."
  (interactive)
  (cond
   ((qq-root--session-key-at-point)
    (qq-chat-open (qq-root--session-key-at-point)))
   ((qq-root--account-id-at-point)
    (qq-root-open-account (qq-root--account-id-at-point)))
   (t (user-error "qq: no session or account at point"))))

(defun qq-root-open-avatar-at-point ()
  "Open avatar/icon for the session at point."
  (interactive)
  (qq-media-open-session-avatar
   (or (qq-root--session-at-point)
       (user-error "qq: no session at point"))))

(defun qq-root-open-user-at-point ()
  "Open the private session user's profile at point."
  (interactive)
  (let* ((session (or (qq-root--session-at-point)
                      (user-error "qq: no session at point")))
         (user-id (or (alist-get 'peer-uin session)
                      (alist-get 'target-id session))))
    (unless (and (eq (alist-get 'type session) 'private)
                 (qq-protocol-user-uin-p user-id))
      (user-error "qq: session has no user profile"))
    (qq-user-open user-id)))

(defun qq-root-open-info-at-point ()
  "Open the user or group profile for the session at point."
  (interactive)
  (let* ((session (or (qq-root--session-at-point)
                      (user-error "qq: no session at point")))
         (type (alist-get 'type session))
         (target-id (or (and (eq type 'private) (alist-get 'peer-uin session))
                        (alist-get 'target-id session))))
    (pcase type
      ('private
       (unless (qq-protocol-user-uin-p target-id)
         (user-error "qq: session has no user profile"))
       (qq-user-open target-id))
      ('group
       (unless (qq-protocol-group-uin-p target-id)
         (user-error "qq: session has no group profile"))
       (qq-group-open target-id))
      (_ (user-error "qq: session has no profile page")))))

(defun qq-root-open-self-user ()
  "Open the logged-in user's profile."
  (interactive)
  (let ((user-id (alist-get 'user_id (qq-state-self-info))))
    (unless (qq-protocol-user-uin-p user-id)
      (user-error "qq: self user profile is unavailable"))
    (qq-user-open user-id)))

(defun qq-root-mouse-open-at-point (event)
  "Open the session clicked by mouse EVENT."
  (interactive "e")
  (mouse-set-point event)
  (qq-root-open-at-point))

(defun qq-root--move-linewise (direction predicate &optional wrap)
  "Move point linewise in DIRECTION until PREDICATE succeeds.

DIRECTION should be 1 or -1.  PREDICATE is called with no arguments at each
candidate line.  When WRAP is non-nil, wrap to buffer edge once."
  (let ((origin (point))
        (wrapped nil)
        (found nil))
    (while (not found)
      (forward-line direction)
      (cond
       ((and (> direction 0) (eobp))
        (if (and wrap (not wrapped))
            (progn
              (setq wrapped t)
              (goto-char (point-min)))
          (setq found 'stop)))
       ((and (< direction 0) (bobp))
        (if (and wrap (not wrapped))
            (progn
              (setq wrapped t)
              (goto-char (point-max))
              (forward-line -1))
          (setq found 'stop)))
       ((funcall predicate)
        (setq found t))))
    (unless (eq found t)
      (goto-char origin)
      nil)))

(defun qq-root-button-forward ()
  "Move point to the next session row."
  (interactive)
  (qq-root--move-linewise
   1
   (lambda ()
     (qq-root--session-key-at-point))
   t))

(defun qq-root-button-backward ()
  "Move point to the previous session row."
  (interactive)
  (qq-root--move-linewise
   -1
   (lambda ()
     (qq-root--session-key-at-point))
   t))

(defun qq-root-tab-dwim ()
  "Move to the next session row."
  (interactive)
  (qq-root-button-forward))

(defun qq-root-next-unread ()
  "Move point to the next unread session row."
  (interactive)
  (unless (qq-root--move-linewise
           1
           (lambda ()
             (> (or (get-text-property (point) 'qq-root-badge-count) 0) 0))
           t)
    (message "qq: no unread sessions")))

(defun qq-root--read-session-key (prompt &optional predicate)
  "Read and return a session key using PROMPT.

When PREDICATE is non-nil, only sessions for which it returns non-nil are
offered."
  (let* ((sessions (if predicate
                       (seq-filter predicate (qq-state-sessions))
                     (qq-state-sessions)))
         (choices
          (mapcar (lambda (session)
                    (cons (format "%s  [%s]"
                                  (or (alist-get 'title session)
                                      (alist-get 'key session))
                                  (alist-get 'key session))
                          (alist-get 'key session)))
                  sessions)))
    (unless choices
      (user-error "qq: no sessions available"))
    (cdr (assoc (completing-read prompt choices nil t) choices))))

(defun qq-root-open-session ()
  "Prompt for a session and open its chat buffer."
  (interactive)
  (qq-chat-open (qq-root--read-session-key "Open session: ")))

(defun qq-root-switch-account ()
  "Select a managed account and open its persistent root view.

Views belonging to other accounts remain live and visible."
  (interactive)
  (call-interactively #'qq-account-select)
  (qq-root-open))

(defun qq-root-open-account (account-id)
  "Open stable ACCOUNT-ID's persistent root without closing other accounts."
  (interactive (list (qq-account--read-account-id "Open QQ account: ")))
  (unless (qq-account-get account-id)
    (user-error "qq: QQ account does not exist: %s" account-id))
  (qq-root-open account-id))

(defun qq-root-refresh ()
  "Request fresh native recent and directory snapshots."
  (interactive)
  (qq-core-refresh))

(defvar qq-root-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'qq-root-refresh)
    (define-key map (kbd "c") #'qq-contacts-open)
    (define-key map (kbd "N") #'qq-group-requests-open)
    (define-key map (kbd "/") #'qq-root-open-session)
    (define-key map (kbd "RET") #'qq-root-open-at-point)
    (define-key map (kbd "a") #'qq-root-open-avatar-at-point)
    (define-key map (kbd "i") #'qq-root-open-info-at-point)
    (define-key map (kbd "I") #'qq-root-open-self-user)
    (define-key map [mouse-1] #'qq-root-mouse-open-at-point)
    (define-key map (kbd "n") #'qq-root-button-forward)
    (define-key map (kbd "p") #'qq-root-button-backward)
    (define-key map (kbd "TAB") #'qq-root-tab-dwim)
    (define-key map (kbd "<backtab>") #'qq-root-button-backward)
    (define-key map (kbd "u") #'qq-root-next-unread)
    (define-key map (kbd "?") #'qq-root-transient)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `qq-root-mode'.")

(define-derived-mode qq-root-mode special-mode "QQ-Root"
  "Major mode for the emacs-qq root buffer.

`?' opens `qq-root-transient' (discoverable command menu)."
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (buffer-disable-undo)
  (setq-local buffer-undo-list t)
  (setq-local switch-to-buffer-preserve-window-point nil)
  (setq-local qq-root--fill-column nil)
  (setq-local qq-root--node-table (make-hash-table :test #'equal))
  (setq-local header-line-format '(:eval (qq-root--header-line)))
  (let ((inhibit-read-only t)
        (buffer-undo-list t))
    (erase-buffer)
    (setq-local qq-root--ewoc
                (ewoc-create #'qq-root--entry-printer nil nil t)))
  (add-hook 'window-size-change-functions
            #'qq-root--on-window-size-change nil t)
  (add-hook 'display-line-numbers-mode-hook
            #'qq-root--on-window-size-change nil t)
  (add-hook 'text-scale-mode-hook #'qq-root--on-text-scale-change nil t))

(defun qq-root--live-view (&optional account-id)
  "Return ACCOUNT-ID's existing live Appkit root view, or nil.

This lookup reads the current runtime registry directly.  It deliberately
does not call `qq-runtime-app', because state and media hooks must not create
an application session merely to discover that no root is open."
  (let* ((gateway-p (eq account-id 'gateway))
         (owner (and (not gateway-p)
                     (or account-id (qq-runtime-current-account-id))))
         (runtime (and owner (qq-runtime-account owner)))
         (app (if gateway-p
                  (and (appkit-app-live-p qq-runtime--app) qq-runtime--app)
                (and runtime (qq-runtime-account-app runtime)))))
    (when app
      (when-let* ((view (appkit-app-surface app 'root)))
        (and (with-current-buffer (appkit-surface-buffer view)
               (derived-mode-p 'qq-root-mode))
             view)))))

(defun qq-root--setup-scope (scope _view)
  "Bind the current root buffer to explicit SCOPE."
  (setq-local qq-root--scope scope))

(defun qq-root-open (&optional account-id)
  "Open ACCOUNT-ID's root, or the current UI account's root.

ACCOUNT-ID may be `gateway' to open the multi-account manager."
  (interactive)
  (let* ((owner (and (not (eq account-id 'gateway))
                     (or account-id (qq-runtime-current-account-id))))
         (host (seq-find
                (lambda (buffer)
                  (with-current-buffer buffer
                    (and (derived-mode-p 'qq-root-mode)
                         (equal qq-root--scope (or owner 'gateway))
                         (not (appkit-current-surface)))))
                (buffer-list))))
    (if owner
        (qq-runtime-with-account owner
          (let* ((app (qq-runtime-app owner))
                 (existing (appkit-app-surface app 'root))
                 (name (qq-runtime-account-display-name owner))
                 (view (qq-runtime-open-account-surface
                        :account-id owner :id 'root :mode 'qq-root-mode
                        :buffer (and (not existing) host)
                        :buffer-name (format "*qq-root:%s*" name)
                        :render-function #'qq-root--render
                        :setup (apply-partially #'qq-root--setup-scope owner)
                        :select t))
                 (buffer (appkit-surface-buffer view)))
            (with-current-buffer buffer
              (unless existing
                (appkit-surface-send view (list 'qq-render (appkit-projection-change-create :full-p t :frame-p t))))
              (qq-root--reflow-visible nil)
              (unless (qq-root--session-key-at-point)
                (goto-char (point-min))
                (qq-root-button-forward)))
            buffer))
      (let* ((app (qq-runtime-gateway-app))
             (existing (appkit-app-surface app 'root))
             (view (qq-runtime-open-surface
                    :app app :id 'root :mode 'qq-root-mode
                    :buffer (and (not existing) host)
                    :buffer-name qq-root-buffer-name
                    :render-function #'qq-root--render
                    :setup (apply-partially #'qq-root--setup-scope 'gateway)
                    :select t))
             (buffer (appkit-surface-buffer view)))
        (with-current-buffer buffer
          (unless existing
            (appkit-surface-send view (list 'qq-render (appkit-projection-change-create :full-p t :frame-p t))))
          (qq-root--reflow-visible nil)
          (unless (qq-root--session-key-at-point)
            (goto-char (point-min))
            (qq-root-button-forward)))
        buffer))))

(defun qq-root-open-gateway ()
  "Open the Gateway-wide account manager root."
  (interactive)
  (qq-root-open 'gateway))

(defun qq-root--reflow-visible (&optional force)
  "Render root geometry when its visible width changes or FORCE is non-nil."
  (when (derived-mode-p 'qq-root-mode)
    (when-let* ((win (or (qq-root--selected-window)
                         (qq-root--display-window)))
                (next (qq-root--compute-fill-column win))
                (surface (qq-root--live-view qq-root--scope)))
      (when (or force (not (equal next qq-root--fill-column)))
        (appkit-surface-send
         surface (list 'qq-render
                       (appkit-projection-change-create :geometry-p t)))
        t))))

(defun qq-root--on-window-size-change (&optional _frame)
  "Reflow a visible root buffer after its window geometry changes."
  (qq-root--reflow-visible nil))

(defun qq-root--on-text-scale-change ()
  "Reflow a visible root buffer after text scaling changes."
  (qq-root--reflow-visible t))

(defun qq-root--session-avatar-media-key (session)
  "Return the exact avatar cache key used by SESSION, or nil."
  (qq-media-session-avatar-cache-key session))

(defun qq-root--handle-media-cache-update (media-key)
  "Invalidate every account root row identified by MEDIA-KEY."
  (when (stringp media-key)
    (dolist (runtime (qq-runtime-accounts))
      (let ((owner (qq-runtime-account-id runtime)))
        (when (qq-root--live-view owner)
          (qq-runtime-with-account owner
            (let (keys)
              (dolist (session (qq-state-sessions))
                (when
                    (or
                     (equal media-key
                            (qq-root--session-avatar-media-key session))
                     (member
                      media-key
                      (qq-media-message-one-line-preview-keys
                       (qq-root--session-last-message session))))
                  (push
                   (qq-root--session-entry-key (alist-get 'key session))
                   keys)))
              (when keys
                (when-let* ((surface (qq-root--live-view owner)))
                  (appkit-surface-send surface (list 'qq-render (appkit-projection-change-create :keys keys :resources (list media-key)))))))))))))

(defun qq-root--handle-state-change (event)
  "Render the account root affected by state EVENT."
  (when-let* ((owner (plist-get event :account-id))
              (surface (qq-root--live-view owner)))
    (let* ((type (plist-get event :type))
           (session-key (plist-get event :session-key))
           (keys (and session-key
                      (list (qq-root--session-entry-key session-key))))
           (change
            (pcase type
              ((or 'connection 'self-info)
               (appkit-projection-change-create :frame-p t))
              ('action
               (and keys (appkit-projection-change-create :keys keys)))
              ((or 'session 'message 'history 'reset 'sessions-refreshed
                   'friends-refreshed 'groups-refreshed 'recent-order)
               (appkit-projection-change-create :full-p t :keys keys)))))
      (when change
        (appkit-surface-send surface (list 'qq-render change))))))

(defun qq-root--handle-login-change ()
  "Reconcile the root after the foreground login presentation changes."
  (when-let* ((surface (qq-root--live-view 'gateway)))
    (appkit-surface-send
     surface (list 'qq-render (appkit-projection-change-create :full-p t)))))

(defun qq-root--handle-gateway-account-change (&rest _arguments)
  "Refresh account manager rows and every account-root header."
  (when-let* ((surface (qq-root--live-view 'gateway)))
    (appkit-surface-send
     surface (list 'qq-render
                   (appkit-projection-change-create :full-p t :frame-p t))))
  (dolist (runtime (qq-runtime-accounts))
    (when-let* ((surface (qq-root--live-view (qq-runtime-account-id runtime))))
      (appkit-surface-send
       surface (list 'qq-render (appkit-projection-change-create :frame-p t))))))

(add-hook 'qq-media-cache-update-hook #'qq-root--handle-media-cache-update)
(add-hook 'qq-state-change-hook #'qq-root--handle-state-change)
(add-hook 'qq-login-change-hook #'qq-root--handle-login-change)
(add-hook 'qq-account-registry-changed-hook
          #'qq-root--handle-gateway-account-change)
(add-hook 'qq-account-selection-changed-hook
          #'qq-root--handle-gateway-account-change)

(provide 'qq-root)

;;; qq-root.el ends here
