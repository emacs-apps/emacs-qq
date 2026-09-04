;;; qq-media.el --- QQ-specific media helpers for emacs-qq -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; Resource helpers for QQ avatars, emoji, and structured message media.

;;; Code:

(require 'appkit-media-effect)
(require 'appkit-surface)

(defun qq-media--initiating-surface (owner)
  "Return the exact initiating QQ Surface, never an account replacement."
  (let* ((surface (or owner (appkit-current-surface)))
         (app (and (appkit-surface-live-p surface)
                   (appkit-surface-app surface))))
    (unless (and app (memq (appkit-app-type-name (appkit-app-type app))
                           '(qq qq-account)))
      (user-error "qq: media requires a live QQ Surface"))
    surface))

(defun qq-media--resolve-open-start (_context input _observe resolve reject)
  "Resolve QQ's opaque INPUT; callbacks only settle the Effect gate."
  (let ((account (plist-get input :account-id)) operation)
    (cl-labels ((start ()
                  (let ((failed (lambda (_body reason) (funcall reject reason))))
                    (cond
                     ((plist-get input :segment)
                      (qq-media-resolve-segment-resource
                       (plist-get input :segment) resolve failed))
                     ((plist-get input :user-avatar)
                      (qq-media--fetch-native-user-avatar
                       (plist-get input :user-avatar) resolve failed))
                     ((plist-get input :group-avatar)
                      (qq-media--fetch-native-group-avatar
                       (plist-get input :group-avatar) resolve failed))
                     (t (funcall resolve (plist-get input :resource)))))))
      (setq operation
            (if account (qq-runtime-with-account account (start)) (start))))
    (cond
     ((qq-remote-media-operation-p operation)
      (appkit-cancellation-create
       :kind 'transport
       :cancel (lambda () (qq-remote-media-cancel-operation operation))))
     ((qq-request-p operation)
      (appkit-cancellation-create
       :kind 'transport :cancel (lambda () (qq-request-cancel operation))))
     ((stringp operation)
      (appkit-cancellation-create
       :kind 'transport :cancel (lambda () (qq-server-cancel operation))))
     (t (appkit-cancellation-create :kind 'logical)))))

(defun qq-media--effect-failed (_input reason)
  "Map terminal media failure REASON into a Surface message."
  (list 'qq-media 'failed (format "%s" reason)))

(defun qq-media--presentation-effect (input resource)
  "Create presentation of acquired RESOURCE described by INPUT."
  (let ((video-p (eq (plist-get input :kind) 'video)))
    (appkit-effect-create
     :key 'qq-media-open
     :input (if video-p
                (appkit-media-video-presentation-create
                 (qq-media--appkit-resource resource) :label "qq"
                 :cache-key (plist-get input :cache-key)
                 :cache-directory qq-media-cache-directory)
              (alist-get 'file resource))
     :start (if video-p #'appkit-media-video-presentation-start
              #'appkit-media-file-presentation-start)
     :success (lambda (_input _result) '(qq-media closed))
     :failure #'qq-media--effect-failed
     :cancellation-requirement 'logical)))

(defun qq-media--open-file-target (resource cache-key)
  "Return the established QQ file-open cache path for RESOURCE and CACHE-KEY."
  (expand-file-name
   (format "%s-%s"
           (substring (md5 (format "%s" (or cache-key (format "open-file-url:%s" (alist-get 'url resource))))) 0 10)
           (appkit-media-sanitize-filename (or (appkit-media-resource-name resource) "media.bin")))
   (expand-file-name "open" qq-media-cache-directory)))

(defun qq-media--acquire-effect (input resource)
  "Acquire remote RESOURCE into QQ's established image/file cache."
  (let* ((key (plist-get input :cache-key))
         (image-p (eq (plist-get input :kind) 'image))
         (canonical (qq-media--appkit-resource resource))
         (cache-key (and key (qq-media--remote-image-cache-key key resource)))
         (base (and image-p
                    (expand-file-name
                     (md5 (or cache-key
                              (format "open-image-url:%s" (alist-get 'url resource))))
                     qq-media-cache-directory)))
         (target (unless image-p
                   (qq-media--open-file-target canonical cache-key))))
    (appkit-effect-create
     :key 'qq-media-open
     :input (if image-p
                (appkit-media-image-acquisition-create canonical base)
              (appkit-media-acquisition-create canonical target))
     :start (if image-p #'appkit-media-image-acquisition-start
              #'appkit-media-acquisition-start)
     :success (lambda (_acquisition file)
                (list 'qq-media 'acquired input
                      (cons (cons 'file file)
                            (assq-delete-all 'file (copy-tree resource)))))
     :failure #'qq-media--effect-failed
     :cancellation-requirement 'transport)))

(defun qq-media-update (_context model message)
  "Commit media MESSAGE and emit acquisition or presentation Effects."
  (let ((next (copy-sequence model)) effect)
    (pcase message
      (`(qq-media open ,input)
       (setq next (plist-put next :media (list :status 'loading :input input))
             effect
             (appkit-effect-create
              :key 'qq-media-open :input input
              :start #'qq-media--resolve-open-start
              :success (lambda (request resource)
                         (list 'qq-media 'resolved request resource))
              :failure #'qq-media--effect-failed
              :cancellation-requirement 'logical)))
      (`(qq-media resolved ,input ,resource)
       (let ((resource (copy-tree resource)))
         (when (qq-media--prefer-remote-image-resource-p
                (plist-get input :cache-key) resource)
           (setf (alist-get 'file resource) nil))
         (when (and (eq (plist-get input :kind) 'file)
                    (not (appkit-media-file-present-p (alist-get 'file resource))))
           (let ((cached (qq-media--open-file-target
                          (qq-media--appkit-resource resource)
                          (plist-get input :cache-key))))
             (when (appkit-media-file-present-p cached)
               (setf (alist-get 'file resource) cached))))
         (if (or (eq (plist-get input :kind) 'video)
                 (appkit-media-file-present-p (alist-get 'file resource)))
             (setq next (plist-put next :media
                                   (list :status 'presenting :resource resource))
                   effect (qq-media--presentation-effect input resource))
           (setq next (plist-put next :media
                                 (list :status 'acquiring :resource resource))
                 effect (qq-media--acquire-effect input resource)))))
      (`(qq-media acquired ,input ,resource)
       (when-let* ((key (plist-get input :cache-key)))
         (qq-media--cache-resource key resource))
       (setq next (plist-put next :media (list :status 'presenting :resource resource))
             effect (qq-media--presentation-effect input resource)))
      (`(qq-media failed ,reason)
       (setq next (plist-put next :media (list :status 'failed :error reason))))
      (`(qq-media closed)
       (setq next (plist-put next :media '(:status closed)))))
    (appkit-next :model next :render appkit-render-none
                 :commands (and effect (list (appkit-command-start-effect effect))))))

(defun qq-media--send-open (owner input)
  "Send owned media INPUT to the exact initiating Surface OWNER."
  (let* ((surface (qq-media--initiating-surface owner))
         (app (appkit-surface-app surface))
         (account (and (eq (appkit-app-type-name (appkit-app-type app)) 'qq-account)
                       (appkit-app-identity app))))
    (appkit-surface-send surface
                         (list 'qq-media 'open (plist-put input :account-id account)))))

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'appkit-media)
(require 'appkit-media-image)
(require 'appkit-ui)
(require 'qq-account)
(require 'qq-customize)
(require 'qq-favorite-emoji)
(require 'qq-remote-media)
(require 'qq-rpc)
(require 'qq-server)
(require 'qq-protocol)
(require 'qq-runtime)
(require 'qq-state)

(defvar qq-media-animated-face-image-height)

(defvar qq-media-cache-update-hook nil
  "Hook run after media resource/image cache updates.")

(defvar qq-media--resource-cache (make-hash-table :test #'equal)
  "Simple in-memory resource cache keyed by logical resource identity.")

(defvar qq-media--image-cache (make-hash-table :test #'equal)
  "In-memory image object cache keyed by logical resource identity.")

(defvar qq-media--preview-missing-cache (make-hash-table :test #'equal)
  "Preview keys whose current media source could not produce an image.")

(defvar qq-media--native-preview-attempts (make-hash-table :test #'equal)
  "Renderer-owned native preview attempts keyed by (MEDIA-ID . PART).

A failed automatic attempt remains here until an explicit operation succeeds
and populates the stable cache, or account/Gateway lifecycle clears it.
Redisplay therefore observes operation state but never schedules a retry.")

(defvar qq-media--fetching-cache (make-hash-table :test #'equal)
  "Set of logical resource identities currently being fetched.")

(defvar qq-media--download-state-table (make-hash-table :test #'equal)
  "Download state plist table keyed by QQ media logical identity.")

(defvar qq-media--native-record-playbacks (make-hash-table :test #'equal)
  "Telega-style playback state keyed by native record media ID.")

(defvar qq-media--native-record-current-id nil
  "Media ID of the one preparing, playing, or paused native record.")

(defvar qq-media--system-emoji-tables (make-hash-table :test #'equal)
  "Managed account id to validated system-emoji catalog table.")

(defvar qq-media--system-emoji-requests (make-hash-table :test #'equal)
  "Managed account ids with an in-flight system-emoji catalog request.")

(defvar qq-media--lottie-players (make-hash-table :test #'equal)
  "System face id to exact live Lottie playback owner.")

(defvar qq-media--lottie-current-frames (make-hash-table :test #'equal)
  "System face id to the current unpublished PNG playback frame.")

(defun qq-media--default-error (_response reason)
  "Display a media operation failure REASON."
  (message "qq: %s" (or reason "media operation failed")))

(defun qq-media--json-truthy-p (value)
  "Return non-nil when JSON VALUE is a true-ish flag."
  (and value
       (not (memq value '(:false :null)))
       (not (member value '(0 "0" "false")))))

(defun qq-media-clear-cache ()
  "Clear all account-owned media caches and asynchronous work."
  (interactive)
  (qq-media--stop-all-native-record-playback)
  (condition-case nil
      (appkit-media-clear-video-decoration-cache 'qq)
    (error nil)
    (quit nil))
  (let (keys transfers remote-operations)
    (maphash
     (lambda (key fetching)
       (push key keys)
       (cond
        ((appkit-media-transfer-p fetching)
         (push fetching transfers))
        ((and (qq-remote-media-operation-p fetching)
              (qq-remote-media-operation-active-p fetching))
         (push fetching remote-operations))))
     qq-media--fetching-cache)
    (maphash
     (lambda (_key entry)
       (when-let* ((transfer (plist-get entry :transfer)))
         (when (appkit-media-transfer-p transfer)
           (push transfer transfers))))
     qq-media--download-state-table)
    (dolist (key keys)
      (condition-case nil
          (appkit-media-cancel-video-preview (concat "qq:" key))
        (error nil)
        (quit nil)))
    (dolist (transfer transfers)
      (condition-case nil
          (appkit-media-cancel-transfer transfer)
        (error nil)
        (quit nil)))
    (dolist (operation remote-operations)
      (condition-case nil
          (qq-remote-media-cancel-operation operation)
        (error nil)
        (quit nil))))
  (clrhash qq-media--resource-cache)
  (clrhash qq-media--image-cache)
  (clrhash qq-media--preview-missing-cache)
  (clrhash qq-media--native-preview-attempts)
  (clrhash qq-media--fetching-cache)
  (clrhash qq-media--download-state-table)
  (clrhash qq-media--system-emoji-tables)
  (clrhash qq-media--system-emoji-requests)
  (maphash
   (lambda (_id owner)
     (when-let* ((process (plist-get owner :process)))
       (when (process-live-p process)
         (delete-process process))))
   qq-media--lottie-players)
  (clrhash qq-media--lottie-players)
  (clrhash qq-media--lottie-current-frames)
  (when (file-directory-p qq-media-cache-directory)
    (ignore-errors (delete-directory qq-media-cache-directory t)))
  (message "qq: media cache cleared"))

(defun qq-media--appkit-resource (resource)
  "Adapt QQ RESOURCE to the strict appkit media resource contract."
  (appkit-media-resource-create
   :file (alist-get 'file resource)
   :url (alist-get 'url resource)
   :name (alist-get 'name resource)
   :mime-type (alist-get 'mime-type resource)))

(defun qq-media--cached-resource (key)
  "Return cached resource for KEY when still usable."
  (let ((resource (copy-tree (gethash key qq-media--resource-cache))))
    (when resource
      (if (or (appkit-media-file-present-p (alist-get 'file resource))
              (appkit-media-url-present-p (alist-get 'url resource)))
          resource
        (remhash key qq-media--resource-cache)
        nil))))

(defun qq-media--cache-resource (key resource)
  "Store RESOURCE under KEY and return RESOURCE."
  (puthash key (copy-tree resource) qq-media--resource-cache)
  resource)

(defun qq-media--cached-image (key)
  "Return cached image object for KEY when valid."
  (let ((image (gethash key qq-media--image-cache)))
    (when image
      (condition-case _
          (progn
            (image-size image t)
            image)
        (error
         (remhash key qq-media--image-cache)
         nil)))))

(defun qq-media--cache-image (key image)
  "Store IMAGE object under KEY and return IMAGE."
  (when image
    (puthash key image qq-media--image-cache))
  image)

(defun qq-media--note-cache-updated (&optional media-key)
  "Notify UI that media cache content changed.

When MEDIA-KEY is non-nil, it identifies the logical cache entry that changed.

Defer the hook to the next command loop via `run-at-time'.  Asynchronous
transfer callbacks can run outside a safe redisplay context; immediate
`erase-buffer' in special-mode forward viewers is unreliable from filters."
  (run-at-time
   0 nil
   (lambda ()
     (run-hook-with-args 'qq-media-cache-update-hook media-key))))

(defun qq-media--native-media-id (segment expected-type)
  "Return SEGMENT's native media ID when its type is EXPECTED-TYPE."
  (when (equal (alist-get 'type segment) expected-type)
    (let ((media-id (alist-get 'media_id (alist-get 'data segment))))
      (and (qq-remote-media--id-p media-id) media-id))))

(defun qq-media--native-record-media-id (segment)
  "Return validated native record media ID from SEGMENT, or nil."
  (qq-media--native-media-id segment "record"))

(defun qq-media--native-image-media-id (segment)
  "Return validated native image media ID from SEGMENT, or nil."
  (qq-media--native-media-id segment "image"))

(defun qq-media--native-video-media-id (segment)
  "Return validated native video media ID from SEGMENT, or nil."
  (qq-media--native-media-id segment "video"))

(defun qq-media--native-file-media-id (segment)
  "Return validated native private/group file media ID from SEGMENT, or nil."
  (when (member (alist-get 'type segment) '("file" "group_file"))
    (let ((media-id (alist-get 'media_id (alist-get 'data segment))))
      (and (qq-remote-media--id-p media-id) media-id))))

(defun qq-media--native-record-key (media-id)
  "Return logical media cache key for native record MEDIA-ID."
  (format "record:%s" media-id))

(defun qq-media--native-image-key (media-id)
  "Return logical media cache key for native image MEDIA-ID."
  (format "image:%s" media-id))

(defun qq-media--native-video-key (media-id)
  "Return logical content cache key for native video MEDIA-ID."
  (format "video:%s" media-id))

(defun qq-media--native-file-key (media-id)
  "Return logical content cache key for native file MEDIA-ID."
  (format "file:%s" media-id))

(defun qq-media--native-video-thumbnail-key (media-id)
  "Return logical thumbnail cache key for native video MEDIA-ID."
  (format "video-thumbnail:%s" media-id))

(defun qq-media--native-record-played-seconds (entry)
  "Return Appkit playback position represented by native record ENTRY."
  (let ((session (plist-get entry :session)))
    (if (appkit-media-player-session-p session)
        (appkit-media-player-played-seconds session)
      (max 0.0 (float (or (plist-get entry :played-seconds) 0.0))))))

(defun qq-media-native-record-playback-state (segment-or-media-id)
  "Return public Appkit playback state for SEGMENT-OR-MEDIA-ID, or nil.

The result excludes the session, preparation operation, local-access ID, and
ephemeral filesystem path retained by the private QQ adapter."
  (let ((media-id (if (stringp segment-or-media-id)
                      segment-or-media-id
                    (qq-media--native-record-media-id segment-or-media-id))))
    (when-let* ((entry
                 (and media-id
                      (gethash media-id qq-media--native-record-playbacks))))
      (let* ((session (plist-get entry :session))
             (session-status
              (and (appkit-media-player-session-p session)
                   (appkit-media-player-status session)))
             (status
              (pcase session-status
                ('starting 'preparing)
                ((or 'playing 'paused 'finished 'failed) session-status)
                (_ (plist-get entry :status)))))
        (list :media-id media-id
              :status status
              :duration-seconds
              (or (and (appkit-media-player-session-p session)
                       (appkit-media-player-session-duration-seconds session))
                  (plist-get entry :duration-seconds))
              :played-seconds
              (qq-media--native-record-played-seconds entry)
              :error (plist-get entry :error))))))

(defun qq-media-native-record-playback-available-p ()
  "Return non-nil when Appkit can play a local native record."
  (appkit-media-player-available-p nil 'audio))

(defun qq-media--notify-native-record-state (media-id)
  "Notify open chats that native record MEDIA-ID changed playback state."
  (qq-media--note-cache-updated (qq-media--native-record-key media-id)))

(defun qq-media--close-local-access (access-id)
  "Best-effort revoke local resource ACCESS-ID."
  (when (qq-resource--local-access-id-p access-id)
    (condition-case error-data
        (when (and (qq-server-ready-p)
                   (qq-rpc-method-available-p "resource.close_local"))
          (qq-resource-close-local
           access-id nil
           (lambda (_body reason)
             (message "qq: failed to close local resource lease: %s" reason))))
      (error
       ;; The service TTL remains the disconnected-client fallback.
       (message "qq: could not request resource lease closure: %s"
                (error-message-string error-data))))))

(defun qq-media--dispose-native-record-entry
    (media-id &optional status error-text)
  "Stop and clean native record MEDIA-ID, recording STATUS and ERROR-TEXT."
  (when-let* ((entry (gethash media-id qq-media--native-record-playbacks)))
    (let ((operation (plist-get entry :operation))
          (session (plist-get entry :session))
          (access-id (plist-get entry :access-id))
          (owner-handle (plist-get entry :owner-handle)))
      ;; Revoke exact callback ownership before any cancellation can reenter.
      (setq entry (plist-put entry :operation nil))
      (setq entry (plist-put entry :session nil))
      (setq entry (plist-put entry :access-id nil))
      (setq entry (plist-put entry :owner-handle nil))
      (setq entry (plist-put entry :status (or status 'stopped)))
      (setq entry (plist-put entry :error error-text))
      (puthash media-id entry qq-media--native-record-playbacks)
      (when (and (qq-remote-media-operation-p operation)
                 (qq-remote-media-operation-active-p operation))
        (qq-remote-media-cancel-operation operation))
      (when (appkit-media-player-session-p session)
        (appkit-media-player-stop session))
      (when (and (appkit-handle-p owner-handle)
                 (appkit-handle-alive-p owner-handle))
        (appkit-retire-handle owner-handle))
      (qq-media--close-local-access access-id)
      (when (equal qq-media--native-record-current-id media-id)
        (setq qq-media--native-record-current-id nil))
      (qq-media--notify-native-record-state media-id)
      entry)))

(defun qq-media--stop-all-native-record-playback ()
  "Cancel every native-record preparation/session and revoke its lease."
  (let (media-ids)
    (maphash (lambda (media-id _entry) (push media-id media-ids))
             qq-media--native-record-playbacks)
    (dolist (media-id media-ids)
      (qq-media--dispose-native-record-entry media-id 'stopped))
    (clrhash qq-media--native-record-playbacks)
    (setq qq-media--native-record-current-id nil)))

(defun qq-media--native-record-session-current-p (media-id session)
  "Return non-nil when SESSION exactly owns native record MEDIA-ID."
  (eq session
      (plist-get
       (gethash media-id qq-media--native-record-playbacks)
       :session)))

(defun qq-media--native-record-session-changed (media-id session)
  "Project exact Appkit SESSION state for native record MEDIA-ID."
  (when (qq-media--native-record-session-current-p media-id session)
    (let* ((entry (gethash media-id qq-media--native-record-playbacks))
           (status (appkit-media-player-status session))
           (terminal-p (memq status '(finished failed stopped)))
           (access-id (and terminal-p (plist-get entry :access-id))))
      (setq entry (plist-put entry :status status))
      (setq entry
            (plist-put entry :played-seconds
                       (appkit-media-player-played-seconds session)))
      (setq entry
            (plist-put entry :error
                       (and (eq status 'failed)
                            "record player exited abnormally")))
      (when terminal-p
        (setq entry (plist-put entry :access-id nil))
        (when (equal qq-media--native-record-current-id media-id)
          (setq qq-media--native-record-current-id nil)))
      (puthash media-id entry qq-media--native-record-playbacks)
      (when access-id
        (qq-media--close-local-access access-id))
      (qq-media--notify-native-record-state media-id))))

(defun qq-media--start-native-record-player (media-id result)
  "Start Appkit playback for prepared native record MEDIA-ID using RESULT."
  (let* ((access (alist-get 'access result))
         (access-id (alist-get 'access_id access))
         (path (alist-get 'path access))
         (entry (gethash media-id qq-media--native-record-playbacks)))
    (if (not (and entry
                  (qq-media-native-record-playback-available-p)
                  (stringp path)
                  (file-regular-p path)))
        (progn
          (qq-media--close-local-access access-id)
          (qq-media--dispose-native-record-entry
           media-id 'failed "record playback input or player is unavailable")
          (message "qq: Native record playback input or player is unavailable")
          nil)
      (setq entry (plist-put entry :access-id access-id))
      (puthash media-id entry qq-media--native-record-playbacks)
      (condition-case error-data
          (let* ((owner (plist-get entry :owner))
                 (duration (plist-get entry :duration-seconds))
                 (session
                  (appkit-media-player-start-file
                   path
                   :kind 'audio
                   :owner owner
                   :duration-seconds duration
                   :on-change
                   (apply-partially
                    #'qq-media--native-record-session-changed media-id)))
                 (current
                  (gethash media-id qq-media--native-record-playbacks)))
            (if (not (and current
                          (eq (plist-get current :status) 'preparing)
                          (qq-account-get (plist-get current :account-id))))
                (progn
                  (appkit-media-player-stop session)
                  (qq-media--close-local-access access-id)
                  nil)
              (when-let* ((handle (plist-get current :owner-handle)))
                (when (appkit-handle-alive-p handle)
                  (appkit-retire-handle handle)))
              (setq current (plist-put current :operation nil))
              (setq current (plist-put current :owner-handle nil))
              (setq current (plist-put current :session session))
              (setq current
                    (plist-put
                     current :status
                     (appkit-media-player-status session)))
              (setq current (plist-put current :error nil))
              (puthash media-id current qq-media--native-record-playbacks)
              (setq qq-media--native-record-current-id media-id)
              (qq-media--notify-native-record-state media-id)
              (when (appkit-media-player-session-finalized-p session)
                (qq-media--native-record-session-changed media-id session))
              session))
        (error
         (qq-media--dispose-native-record-entry
          media-id 'failed (error-message-string error-data))
         nil)))))

(defun qq-media--native-record-prepared (media-id result)
  "Consume playback preparation RESULT for exact MEDIA-ID."
  (let ((entry (gethash media-id qq-media--native-record-playbacks)))
    (if (not (and entry
                  (equal qq-media--native-record-current-id media-id)
                  (eq (plist-get entry :status) 'preparing)
                  (qq-account-get (plist-get entry :account-id))))
        (qq-media--close-local-access
         (alist-get 'access_id (alist-get 'access result)))
      (qq-media--start-native-record-player media-id result))))

(defun qq-media--native-record-prepare-failed (media-id _body reason)
  "Record playback preparation failure REASON for MEDIA-ID."
  (let ((entry (gethash media-id qq-media--native-record-playbacks)))
    (when (and entry
               (equal qq-media--native-record-current-id media-id)
               (eq (plist-get entry :status) 'preparing))
      (qq-media--dispose-native-record-entry media-id 'failed reason)
      (message "qq: Record playback preparation failed: %s" reason))))

(cl-defun qq-media-play-native-record (segment &key owner)
  "Prepare, play, pause, or resume native record SEGMENT.

OWNER owns preparation until the local file is handed to an Appkit playback
session.  Playing records pause by stopping ffplay; paused records resume with
a fresh process at Appkit's retained Telega-style progress."
  (let* ((media-id (qq-media--native-record-media-id segment))
         (entry (and media-id
                     (gethash media-id qq-media--native-record-playbacks)))
         (session (and entry (plist-get entry :session)))
         (status
          (or (and (appkit-media-player-session-p session)
                   (appkit-media-player-status session))
              (and entry (plist-get entry :status)))))
    (unless media-id
      (user-error "qq: Native record has no materializable media ID"))
    (pcase status
      ('preparing
       (qq-media--dispose-native-record-entry media-id 'stopped)
       (message "qq: canceled record playback preparation"))
      ((or 'playing 'paused)
       (condition-case error-data
           (appkit-media-player-toggle session)
         (error
          (qq-media--dispose-native-record-entry
           media-id 'failed (error-message-string error-data)))))
      (_
       (unless (qq-media-native-record-playback-available-p)
         (user-error
          "qq: Record player unavailable; customize `appkit-media-audio-player-command'"))
       (when (and qq-media--native-record-current-id
                  (not (equal qq-media--native-record-current-id media-id)))
         (qq-media--dispose-native-record-entry
          qq-media--native-record-current-id 'stopped))
       (let* ((account-id (qq-runtime-current-account-id))
              (owner-handle
               (and account-id owner
                    (appkit-register-handle
                     owner 'function
                     (apply-partially
                      #'qq-media--dispose-native-record-entry
                      media-id 'stopped))))
              (next
               (list :status 'preparing
                     :account-id account-id
                     :owner owner
                     :duration-seconds
                     (alist-get 'duration_seconds
                                (alist-get 'data segment))
                     :played-seconds 0.0
                     :owner-handle owner-handle
                     :operation nil
                     :session nil
                     :access-id nil
                     :error nil)))
         (unless account-id
           (user-error "qq: Select an online account before playing a record"))
         (puthash media-id next qq-media--native-record-playbacks)
         (setq qq-media--native-record-current-id media-id)
         (qq-media--notify-native-record-state media-id)
         (condition-case error-data
             (let ((operation
                    (qq-remote-media-prepare-record-playback
                     media-id
                     (apply-partially
                      #'qq-media--native-record-prepared media-id)
                     (apply-partially
                      #'qq-media--native-record-prepare-failed media-id))))
               ;; Synchronous transports may already have handed off to Appkit.
               (when-let* ((current
                            (gethash media-id
                                     qq-media--native-record-playbacks)))
                 (when (eq (plist-get current :status) 'preparing)
                   (setq current (plist-put current :operation operation))
                   (puthash media-id current
                            qq-media--native-record-playbacks))))
           (error
            (qq-media--dispose-native-record-entry
             media-id 'failed (error-message-string error-data))
            (signal (car error-data) (cdr error-data)))))))
    (qq-media-native-record-playback-state media-id)))

(defun qq-media--native-remote-media-changed (_reason media-id)
  "Redisplay cards affected by remote MEDIA-ID state changes."
  (if media-id
      (unless (qq-remote-media media-id)
        (remhash (cons media-id 'content) qq-media--native-preview-attempts)
        (remhash (cons media-id 'thumbnail) qq-media--native-preview-attempts))
    (clrhash qq-media--native-preview-attempts))
  (if-let* ((media (and media-id (qq-remote-media media-id))))
      (pcase (alist-get 'kind media)
        ("record" (qq-media--notify-native-record-state media-id))
        ("image"
         (qq-media--note-cache-updated
          (qq-media--native-image-key media-id)))
        ("video"
         (qq-media--note-cache-updated
          (qq-media--native-video-key media-id))
         (qq-media--note-cache-updated
          (qq-media--native-video-thumbnail-key media-id)))
        ;; Native file segments key their cache by the opaque media handle.
        ("file" (qq-media--note-cache-updated (qq-media--native-file-key media-id)))
        (_ (qq-media--note-cache-updated nil)))
    (qq-media--note-cache-updated nil)))

(add-hook 'qq-remote-media-changed-hook
          #'qq-media--native-remote-media-changed)

(defun qq-media--image-from-file (file height)
  "Create an Emacs image object from FILE at pixel HEIGHT, or nil."
  (when (appkit-media-file-present-p file)
    (condition-case _
        (let ((image (create-image file nil nil
                                   :height (max 1 height) :ascent 'center)))
          (if (fboundp 'appkit-media--mark-inline-animation-image)
              (appkit-media--mark-inline-animation-image image file)
            image))
      (error nil))))

(defun qq-media--avatar-image-from-file (file pixel-size)
  "Create a circular avatar from FILE at PIXEL-SIZE.

Retain the ordinary image decoder as a capability fallback when the current
Emacs display cannot render SVG clipping."
  (or (appkit-media-circular-image-from-file file pixel-size)
      (qq-media--image-from-file file pixel-size)))

(defun qq-media--preview-image-from-file (file spec)
  "Create a preview image object from FILE using appkit media helpers.

SPEC may be a numeric maximum height for compact decorative images."
  (when (appkit-media-file-present-p file)
    (appkit-media-preview-image-from-file
     file
     qq-media-preview-image-max-width
     (if (numberp spec) spec qq-media-preview-image-height))))

(defun qq-media--one-line-preview-image-from-file (file _spec)
  "Create a single-row thumbnail from FILE for compact preview surfaces."
  (when (appkit-media-file-present-p file)
    (appkit-media-one-line-preview-image-from-file
     file
     (* (max 1 qq-media-one-line-preview-columns)
        (max 1 (frame-char-width))))))

(defun qq-media--image-display-string (image fallback)
  "Return display string for IMAGE, or FALLBACK when IMAGE is nil."
  (appkit-media-image-display-string image fallback))

(defun qq-media-composer-image-preview (file)
  "Return a Telega-style one-line composer preview for local image FILE."
  (qq-media--one-line-preview-image-from-file file nil))

(defun qq-media--prefer-remote-image-resource-p (key resource)
  "Return non-nil when RESOURCE at KEY should prefer its remote image.

User-avatar local files may be stale cache entries.  A remote avatar copy is
reusable only when its disk identity includes the URL that supplied it."
  (and (stringp key)
       (string-prefix-p "avatar:" key)
       resource
       (appkit-media-url-present-p (alist-get 'url resource))))

(defun qq-media--remote-image-cache-key (key &optional resource)
  "Return the disk identity for logical image KEY and optional RESOURCE."
  (if (qq-media--prefer-remote-image-resource-p key resource)
      (format "%s:url:%s" key (alist-get 'url resource))
    key))

(defun qq-media--remote-image-cache-file-base (key &optional resource)
  "Return disk cache file base for logical image KEY and optional RESOURCE."
  (expand-file-name
   (md5 (qq-media--remote-image-cache-key key resource))
   qq-media-cache-directory))

(defun qq-media--remote-image-cache-existing-file (key &optional resource)
  "Return an existing disk cache file for KEY and RESOURCE, or nil."
  (appkit-media-image-cache-existing-file
   (qq-media--remote-image-cache-file-base key resource)))

(defun qq-media--resource-image-file (key resource)
  "Return local image file for RESOURCE at KEY, consulting disk cache when needed."
  (let* ((cached-file
          (and key (qq-media--remote-image-cache-existing-file key resource)))
         (prefer-remote (qq-media--prefer-remote-image-resource-p key resource))
         (file (alist-get 'file resource)))
    (cond
     ((appkit-media-file-present-p cached-file)
      (setf (alist-get 'file resource nil nil #'eq) cached-file)
      (qq-media--cache-resource key resource)
      cached-file)
     ((and prefer-remote (appkit-media-url-present-p (alist-get 'url resource)))
      nil)
     ((appkit-media-file-present-p file)
      file)
     (t nil))))

(defun qq-media--finish-resource-image-fetch (key &optional file image resource)
  "Finalize image fetch for KEY using FILE, IMAGE, and RESOURCE.

Always clear the fetching flag and notify UI.  Previously we only called
`qq-media--note-cache-updated' when IMAGE was non-nil, so a successful file
download that failed `create-image' (or a failed URL fetch) left forward
viewers stuck on \"[loading preview]\" while RET open still worked via the
non-preview resource path."
  (when (and resource file)
    (setf (alist-get 'file resource nil nil #'eq) file)
    (qq-media--cache-resource key resource))
  (when image
    (qq-media--cache-image key image))
  (remhash key qq-media--fetching-cache)
  (qq-media--note-cache-updated key))

(defun qq-media--start-resource-image-download (key resource spec builder)
  "Download remote image RESOURCE for KEY, then build image with BUILDER."
  (let* ((url (alist-get 'url resource))
         (cache-base (qq-media--remote-image-cache-file-base key resource))
         (disk-cache-file
          (qq-media--remote-image-cache-existing-file key resource))
         (cache-file (qq-media--resource-image-file key resource))
         (cached-image (and cache-file (funcall builder cache-file spec))))
    (cond
     (cached-image
      (qq-media--finish-resource-image-fetch key cache-file cached-image resource))
     ((not (appkit-media-url-present-p url))
      (qq-media--finish-resource-image-fetch key nil nil resource))
     (t
      ;; This branch has explicitly selected the remote URL.  Never pass an
      ;; older local avatar (or a local file whose preview failed) to Appkit,
      ;; whose canonical resource policy correctly prefers local files.
      (when (appkit-media-file-present-p disk-cache-file)
        (ignore-errors (delete-file disk-cache-file)))
      (condition-case err
          (let ((remote-resource (copy-tree resource)))
            (setf (alist-get 'file remote-resource nil nil #'eq) nil)
            (let ((transfer
                   (appkit-media-cache-image-resource-async
                    (qq-media--appkit-resource remote-resource)
                    cache-base
                    (lambda (target-file)
                      (qq-media--finish-resource-image-fetch
                       key target-file
                       (funcall builder target-file spec)
                       resource))
                    (lambda (_reason)
                      (qq-media--finish-resource-image-fetch
                       key nil nil resource)))))
              ;; Setup failures report synchronously and clear KEY.  Preserve
              ;; that terminal state instead of restoring a stale handle.
              (when (gethash key qq-media--fetching-cache)
                (puthash key transfer qq-media--fetching-cache))))
        (error
         (message "qq: failed to start remote image fetch for %s: %s"
                  key
                  (error-message-string err))
         (qq-media--finish-resource-image-fetch key nil nil resource)))))))

(defun qq-media--accept-resource-image-fetch
    (key fetched-resource spec builder)
  "Consume FETCHED-RESOURCE for active image KEY using SPEC and BUILDER."
  (when (gethash key qq-media--fetching-cache)
    (let* ((resource (qq-media--cache-resource key fetched-resource))
           (original-file (and resource (alist-get 'file resource)))
           (file (and resource
                      (qq-media--resource-image-file key resource))))
      (cond
       ((appkit-media-file-present-p file)
        (qq-media--finish-resource-image-fetch
         key file (funcall builder file spec) resource))
       ((and (qq-media--prefer-remote-image-resource-p key resource)
             (appkit-media-file-present-p original-file))
        (let ((image (funcall builder original-file spec)))
          (when image
            (qq-media--cache-image key image)
            (qq-media--note-cache-updated key)))
        (qq-media--start-resource-image-download
         key resource spec builder))
       ((appkit-media-url-present-p (alist-get 'url resource))
        (qq-media--start-resource-image-download
         key resource spec builder))
       (t
        (qq-media--finish-resource-image-fetch key nil nil resource))))))

(defun qq-media--reject-resource-image-fetch (key)
  "Settle an active image fetch for KEY without a usable resource."
  (when (gethash key qq-media--fetching-cache)
    (qq-media--finish-resource-image-fetch key nil nil nil)))

(defun qq-media--start-resource-image-fetch (key fetcher spec builder)
  "Start FETCHER for image KEY and contain synchronous setup failures.

Renderers must always be able to return their textual fallback.  Account
lifecycle or transport setup can reject a fetch before returning a request
handle, so synchronous failure follows the same settlement path as an
asynchronous error."
  (condition-case error-data
      (funcall
       fetcher
       (lambda (resource)
         (qq-media--accept-resource-image-fetch key resource spec builder))
       (lambda (_response _reason)
         (qq-media--reject-resource-image-fetch key)))
    (error
     (qq-media--reject-resource-image-fetch key)
     (message "qq: failed to start image fetch for %s: %s"
              key (error-message-string error-data)))))

(defun qq-media--ensure-resource-image (key fetcher spec &optional image-builder)
  "Return cached image for KEY, triggering FETCHER when needed.

FETCHER must accept SUCCESS and ERROR callbacks.  SUCCESS receives one
resource alist.  SPEC is forwarded to IMAGE-BUILDER, which defaults to
`qq-media--image-from-file'."
  (let ((builder (or image-builder #'qq-media--image-from-file)))
    (or (qq-media--cached-image key)
        (let* ((resource (qq-media--cached-resource key))
               (original-file (and resource (alist-get 'file resource)))
               (file (and resource (qq-media--resource-image-file key resource))))
          (cond
           ((appkit-media-file-present-p file)
            (qq-media--cache-image key (funcall builder file spec)))
           ((gethash key qq-media--fetching-cache)
            nil)
           ((and resource
                 (qq-media--prefer-remote-image-resource-p key resource)
                 (appkit-media-file-present-p original-file))
            (puthash key t qq-media--fetching-cache)
            (qq-media--start-resource-image-download key resource spec builder)
            (qq-media--cache-image key (funcall builder original-file spec)))
           ((and resource (appkit-media-url-present-p (alist-get 'url resource)))
            (puthash key t qq-media--fetching-cache)
            (qq-media--start-resource-image-download key resource spec builder)
            nil)
           (t
            (puthash key t qq-media--fetching-cache)
            (qq-media--start-resource-image-fetch key fetcher spec builder)
            nil))))))

(defun qq-media--native-image-animated-p (segment)
  "Return non-nil when native image SEGMENT is known to be animated."
  (let* ((data (alist-get 'data segment))
         (summary (alist-get 'summary data)))
    (or (qq-media--json-truthy-p (alist-get 'animated data))
        (and (stringp summary)
             (string-match-p "动画" summary)))))

(defun qq-media--native-preview-part-failed-p (media-id part)
  "Return non-nil when MEDIA-ID PART has terminal materialization failure."
  (when-let* ((media (qq-remote-media media-id))
              (snapshot (qq-remote-media-part media part)))
    (equal (alist-get 'phase snapshot) "failed")))

(defun qq-media--ensure-native-preview-image
    (key media-id part fetcher &optional image-builder)
  "Return KEY's image or begin one renderer-owned MEDIA-ID PART attempt.

FETCHER accepts success and error callbacks.  IMAGE-BUILDER converts the
persistent file into an Emacs image.  A terminal or locally failed automatic
attempt is not reissued by later redisplay.  User actions continue to call the
ordinary segment operations directly and can therefore retry."
  (when-let* ((file (qq-media--remote-image-cache-existing-file key)))
    (qq-media--cache-resource key `((file . ,file))))
  (let ((attempt-key (cons media-id part)))
    (cond
     ((or (qq-media--cached-image key)
          (qq-media--cached-resource key))
      (qq-media--ensure-resource-image key fetcher nil image-builder))
     ((qq-media--native-preview-part-failed-p media-id part) nil)
     ((gethash attempt-key qq-media--native-preview-attempts) nil)
     (t
      (puthash attempt-key t qq-media--native-preview-attempts)
      (qq-media--ensure-resource-image
       key
       (lambda (done error)
         (funcall
          fetcher
          (lambda (resource)
            (remhash attempt-key qq-media--native-preview-attempts)
            (funcall done resource))
          (lambda (response reason)
            ;; Keep ATTEMPT-KEY terminal.  Only an explicit operation or a
            ;; lifecycle replacement may create another native request.
            (funcall error response reason))))
       nil image-builder)))))

(defun qq-media--fetch-native-image-part-resource
    (segment media-id part key callback errback)
  "Materialize image-like PART of native MEDIA-ID from SEGMENT into KEY.

The Gateway grants only a short-lived local access path.  Copy it through
Appkit's atomic image cache before closing the lease; CALLBACK therefore sees
only a client-owned stable file.  ERRBACK receives failures."
  (let ((error-fn (or errback #'qq-media--default-error))
        (animated-p
         (and (eq part 'content)
              (qq-media--native-image-animated-p segment))))
    (if (not media-id)
        (funcall error-fn nil "segment has no native media handle")
      (condition-case error-data
          (let ((operation
                 (qq-remote-media-prepare-part-local-access
                  media-id part
                  (lambda (result)
                    (let* ((access (alist-get 'access result))
                           (access-id (alist-get 'access_id access))
                           (path (alist-get 'path access))
                           (closed nil))
                      (cl-labels
                          ((close-access
                             ()
                             (unless closed
                               (setq closed t)
                               (qq-media--close-local-access access-id)))
                           (finish
                             (file)
                             (close-access)
                             (let ((resource
                                    `((file . ,file)
                                      (name . ,(file-name-nondirectory file))
                                      ,@(when animated-p
                                          '((animated . t))))))
                               (if animated-p
                                   (qq-media--prepare-animated-face-resource
                                    resource callback)
                                 (funcall callback resource))))
                           (fail-copy
                             (reason)
                             (close-access)
                             (funcall error-fn nil reason)))
                        (condition-case copy-error
                            (appkit-media-cache-image-resource-async
                             `((file . ,path))
                             (qq-media--remote-image-cache-file-base key)
                             #'finish #'fail-copy)
                          ((error quit)
                           (close-access)
                           (funcall
                            error-fn nil
                            (error-message-string copy-error)))))))
                  error-fn)))
            ;; Preview ownership places a marker in this table before invoking
            ;; the fetcher.  Do not overwrite a synchronous completion.
            (when (and (qq-remote-media-operation-active-p operation)
                       (gethash key qq-media--fetching-cache))
              (puthash key operation qq-media--fetching-cache))
            operation)
        ((error quit)
         (funcall error-fn nil (error-message-string error-data))
         nil)))))

(defun qq-media--fetch-native-image-resource
    (segment key callback errback)
  "Materialize native image SEGMENT into KEY.

Call CALLBACK with the persistent resource; call ERRBACK on failure."
  (qq-media--fetch-native-image-part-resource
   segment (qq-media--native-image-media-id segment) 'content
   key callback errback))

(defun qq-media--fetch-native-video-thumbnail-resource
    (segment key callback errback)
  "Materialize native video SEGMENT thumbnail into KEY.

Call CALLBACK with the persistent resource; call ERRBACK on failure."
  (qq-media--fetch-native-image-part-resource
   segment (qq-media--native-video-media-id segment) 'thumbnail
   key callback errback))

(defun qq-media--materialize-native-content-to-cache
    (media-id key target callback errback &optional mime-type)
  "Materialize native MEDIA-ID content into client-owned TARGET under KEY."
  (let ((error-fn (or errback #'qq-media--default-error)))
    (condition-case error-data
        (let ((operation
               (qq-remote-media-prepare-part-local-access
                media-id 'content
                (lambda (result)
                  (let* ((access (alist-get 'access result))
                         (access-id (alist-get 'access_id access))
                         (path (alist-get 'path access))
                         (closed nil))
                    (cl-labels
                        ((close-access
                           ()
                           (unless closed
                             (setq closed t)
                             (qq-media--close-local-access access-id)))
                         (finish
                           (file)
                           (close-access)
                           (funcall
                            callback
                            `((file . ,file)
                              (name . ,(file-name-nondirectory file))
                              ,@(when mime-type
                                  `((mime-type . ,mime-type))))))
                         (fail-copy
                           (reason)
                           (close-access)
                           (funcall error-fn nil reason)))
                      (condition-case copy-error
                          (appkit-media-copy-or-download-resource-async
                           `((file . ,path)) target #'finish #'fail-copy)
                        ((error quit)
                         (close-access)
                         (funcall error-fn nil
                                  (error-message-string copy-error)))))))
                error-fn)))
          (when (and (qq-remote-media-operation-active-p operation)
                     (gethash key qq-media--fetching-cache))
            (puthash key operation qq-media--fetching-cache))
          operation)
      ((error quit)
       (funcall error-fn nil (error-message-string error-data))
       nil))))

(defun qq-media--fetch-native-video-resource
    (segment key callback errback)
  "Materialize native video SEGMENT content into KEY."
  (if-let* ((media-id (qq-media--native-video-media-id segment)))
      (qq-media--materialize-native-content-to-cache
       media-id key
       (expand-file-name
        (format "native-video-%s.mp4" (secure-hash 'sha256 key))
        qq-media-cache-directory)
       callback errback "video/mp4")
    (funcall (or errback #'qq-media--default-error)
             nil "video segment has no native media handle")))

(defun qq-media--fetch-native-file-resource
    (segment key callback errback)
  "Materialize SEGMENT's opaque native media handle into the client cache."
  (if-let* ((media-id (qq-media--native-file-media-id segment)))
      (let ((safe-name (qq-media-segment-default-save-name segment)))
        (qq-media--materialize-native-content-to-cache
         media-id key
         (expand-file-name
          (format "native-file-%s-%s"
                  (substring (secure-hash 'sha256 key) 0 16)
                  safe-name)
          qq-media-cache-directory)
         callback errback))
    (funcall (or errback #'qq-media--default-error)
             nil "file segment has no native media handle")))

(defun qq-media--resource-fetching-p (key)
  "Return non-nil when KEY is currently being fetched."
  (gethash key qq-media--fetching-cache))

(defun qq-media--resolve-resource (key fetcher callback)
  "Resolve resource by KEY using FETCHER, then run CALLBACK.

FETCHER is called with a one-argument callback that receives the resource
alist returned by the resource fetcher."
  (if-let* ((cached (qq-media--cached-resource key)))
      (funcall callback cached)
    (funcall fetcher
             (lambda (resource)
               (let ((cached-resource (qq-media--cache-resource key resource)))
                 (qq-media--note-cache-updated key)
                 (funcall callback cached-resource))))))

(defun qq-media--segment-file-keys (segment)
  "Return candidate file keys from SEGMENT, in preference order."
  (let* ((data (alist-get 'data segment))
         (keys (list (alist-get 'file_id data)
                     (alist-get 'file data)
                     (alist-get 'path data))))
    (delete-dups
     (seq-filter
      (lambda (key)
        (and key
             (or (not (stringp key))
                 (not (string-empty-p (string-trim key))))))
      keys))))

(defun qq-media--segment-file-key (segment)
  "Return best file key from SEGMENT."
  (car (qq-media--segment-file-keys segment)))

(defun qq-media--absolute-local-file-present-p (file)
  "Return non-nil when FILE is an existing absolute local path.

Protocol `file' values are often opaque names or handles.  Even when such a
relative string happens to exist below `default-directory', it is not proof
that the server designated that local file."
  (and (stringp file)
       (file-name-absolute-p file)
       (appkit-media-file-present-p file)))

(defun qq-media--video-local-file-present-p (file &optional resource)
  "Return non-nil when FILE is a plausible playable video local file.

RESOURCE may provide MIME metadata.  Image artifacts are video posters or
bounded GIF previews and must never be promoted to the playable video source."
  (and (qq-media--absolute-local-file-present-p file)
       (not (appkit-media-image-file-name-p file))
       (let ((mime-type (and resource (alist-get 'mime-type resource))))
         (not (and (stringp mime-type)
                   (string-prefix-p "image/" (downcase mime-type)))))))

(defun qq-media--segment-existing-path (segment)
  "Return an existing local filesystem path from SEGMENT, or nil.

Outbound attach/pending segments carry absolute paths in `file' or `path'.
Those must never be treated as remote media handles (Telega-style local-first
rendering)."
  (let* ((data (alist-get 'data segment))
         (candidates (list (alist-get 'path data)
                           (alist-get 'file data))))
    (catch 'found
      (dolist (candidate candidates)
        (when (qq-media--absolute-local-file-present-p candidate)
          (throw 'found candidate)))
      nil)))

(defun qq-media--resource-from-local+url (local url)
  "Build a resource alist from LOCAL path and optional URL."
  (append
   (when (appkit-media-file-present-p local)
     `((file . ,local)))
   (when (appkit-media-url-present-p url)
     `((url . ,url)))))

(defun qq-media--resolve-fileish-segment
    (segment callback errback &optional final-error)
  "Resolve file-like SEGMENT from an existing local path or direct URL."
  (let* ((capabilities (qq-media-segment-capabilities segment))
         (url (plist-get capabilities :remote-url))
         (local (qq-media--segment-existing-path segment))
         (error-fn (or errback #'qq-media--default-error))
         (fail-msg
          (or final-error "media segment has neither local file nor URL")))
    (cond
     (local
      (funcall callback (qq-media--resource-from-local+url local url)))
     ((appkit-media-url-present-p url)
      (funcall callback `((url . ,url))))
     (t
      (funcall error-fn nil fail-msg)))))

(defun qq-media-segment-display-name (segment)
  "Return SEGMENT's display filename, or nil when none was projected."
  (let ((data (alist-get 'data segment)))
    (or (alist-get 'file_name data)
        (alist-get 'name data)
        (alist-get 'file data)
        (appkit-media-url-filename (alist-get 'url data)))))

(defun qq-media--file-segment-kind (segment)
  "Return semantic kind of file SEGMENT, or nil for another segment type.

An authoritative `file_kind' presentation fact wins.  Filename inference is
only the fallback for older/general file producers that do not provide one."
  (when (member (alist-get 'type segment) '("file" "group_file"))
    (let* ((data (alist-get 'data segment))
           (file-kind (alist-get 'file_kind data))
           (name (qq-media-segment-display-name segment)))
      (pcase file-kind
        ("image" 'image)
        ("video" 'video)
        ("other" 'file)
        (_ (cond
            ((appkit-media-image-file-name-p name) 'image)
            ((appkit-media-video-file-name-p name) 'video)
            (t 'file)))))))

(defun qq-media-imageish-file-segment-p (segment)
  "Return non-nil when SEGMENT is a file presented as an image."
  (eq (qq-media--file-segment-kind segment) 'image))

(defun qq-media-videoish-segment-p (segment)
  "Return non-nil when SEGMENT carries a video preview source."
  (or (equal (alist-get 'type segment) "video")
      (eq (qq-media--file-segment-kind segment) 'video)))

(defun qq-media-segment-preview-capable-p (segment)
  "Return non-nil when SEGMENT supports inline preview rendering."
  (or (member (alist-get 'type segment) '("image" "mface"))
      (qq-media-imageish-file-segment-p segment)
      (qq-media-videoish-segment-p segment)))

(defun qq-media--segment-url (segment)
  "Return best direct URL from SEGMENT, or nil."
  (alist-get 'url (alist-get 'data segment)))

(defun qq-media--video-remote-status (segment)
  "Return normalized remote status symbol for video SEGMENT.

The wire values are `available', `resolvable', `expired', `unavailable', and
`unresolved'.  A missing or otherwise unknown value is an invalid protocol
state and must not enable a remote operation."
  (let ((value (alist-get 'remote_status (alist-get 'data segment))))
    (cond
     ((equal value "available") 'available)
     ((equal value "resolvable") 'resolvable)
     ((equal value "expired") 'expired)
     ((equal value "unavailable") 'unavailable)
     ((equal value "unresolved") 'unresolved)
     (t 'invalid))))

(defun qq-media--transfer-status-text (state)
  "Return compact user-visible transfer status for download STATE."
  (let ((status (plist-get state :status))
        (path (plist-get state :path))
        (error-text (plist-get state :error)))
    (pcase status
      ('downloading "downloading…")
      ('downloaded
       (if (and (stringp path) (not (string-empty-p path)))
           (format "local: %s" (file-name-nondirectory path))
         "downloaded"))
      ('error
       (if (and (stringp error-text) (not (string-empty-p error-text)))
           (format "download failed: %s"
                   (truncate-string-to-width error-text 68 nil nil t))
         "download failed"))
      (_ nil))))

(defun qq-media--legacy-segment-capabilities (segment)
  "Return actions available from local files and direct native URLs."
  (let* ((type (alist-get 'type segment))
         (supported (member type
                            '("image" "file" "record" "video" "face" "mface")))
         (video-p (equal type "video"))
         (remote-status
          (if video-p (qq-media--video-remote-status segment) 'not-applicable))
         (local-file (qq-media-segment-local-file segment))
         (url (qq-media--segment-url segment))
         (usable-url-p
          (and (appkit-media-url-present-p url)
               (or (not video-p) (eq remote-status 'available))))
         (resolve-remote (and supported usable-url-p))
         (download-state (qq-media-segment-download-state segment))
         (download-status (plist-get download-state :status))
         (open (and supported (or local-file resolve-remote)))
         (download (and supported resolve-remote (not local-file)
                        (not (memq download-status
                                   '(downloading downloaded)))))
         (save (and supported (or local-file resolve-remote)))
         (remote-error
          (and video-p
               (pcase remote-status
                 ('expired "video resource has expired")
                 ('unavailable "video resource is unavailable")
                 ('invalid "video resource has invalid remote_status")
                 ('unresolved "video resource is unresolved")
                 ('resolvable
                  "video resource requires an unavailable legacy resolver")
                 ('available
                  (unless usable-url-p
                    "available video resource has no URL")))))
         (status
          (if video-p
              (pcase remote-status
                ('expired "Expired")
                ('unavailable "Unavailable")
                ('unresolved "Unresolved")
                ('resolvable "Unavailable")
                ('invalid "Invalid remote status")
                (_ (qq-media--transfer-status-text download-state)))
            (qq-media--transfer-status-text download-state))))
    (list :open open
          :download download
          :save save
          :copy-url (and usable-url-p t)
          :status status
          :local-file local-file
          :remote-status remote-status
          :resolve-remote resolve-remote
          :remote-url (and usable-url-p url)
          :remote-error remote-error
          :download-state download-state)))

(defconst qq-media--native-record-required-methods
  '("media.materialize"
    "media.cancel"
    "resource.derive_playable_record"
    "resource.open_local"
    "resource.close_local")
  "Native protocol methods required for record playback preparation.")

(defun qq-media--native-record-methods-ready-p ()
  "Return non-nil when native record playback can start for this account."
  (and (qq-runtime-current-account-id)
       (qq-server-ready-p)
       (cl-every #'qq-rpc-method-available-p
                 qq-media--native-record-required-methods)))

(defun qq-media--native-record-capabilities (media-id)
  "Return the action/status model for native record MEDIA-ID."
  (let* ((playback
          (qq-media-native-record-playback-state media-id))
         (playback-status (plist-get playback :status))
         (playback-error (plist-get playback :error))
         (remote (qq-remote-media media-id))
         (content (qq-remote-media-part remote 'content))
         (phase (alist-get 'phase content))
         (problem (alist-get 'error content))
         (problem-message (alist-get 'message problem))
         (active (memq playback-status '(preparing playing paused)))
         (player-ready (qq-media-native-record-playback-available-p))
         (methods-ready (qq-media--native-record-methods-ready-p))
         (open (or active (and player-ready methods-ready)))
         (remote-error
          (cond
           ((and (eq playback-status 'failed) playback-error)
            playback-error)
           ((equal phase "failed")
            (or problem-message "remote record materialization failed"))
           ((not player-ready)
            "configured record player is unavailable")
           ((not methods-ready)
            "native record playback methods are unavailable")))
         (status
          (pcase playback-status
            ('preparing "Preparing…")
            ('playing "Playing")
            ('paused "Paused")
            ('finished "Finished")
            ('failed
             (format "Playback failed: %s"
                     (truncate-string-to-width
                      (or playback-error "unknown player error") 68 nil nil t)))
            (_
             (cond
              ((equal phase "materializing")
               (let ((done (alist-get 'bytes_done content))
                     (total (or (alist-get 'bytes_total content)
                                (alist-get 'expected_size content))))
                 (if total
                     (format "Preparing %s/%s bytes" done total)
                   (format "Preparing %s bytes" done))))
              ((equal phase "materialized") "Ready")
              ((equal phase "failed")
               (format "Retry: %s"
                       (truncate-string-to-width
                        (or problem-message "materialization failed")
                        68 nil nil t)))
              ((not player-ready) "Player unavailable")
              ((not (qq-runtime-current-account-id)) "Select an account")
              ((not methods-ready) "Playback unavailable")
              (t "Remote voice"))))))
    (list :open (and open t)
          :download nil
          :save nil
          :copy-url nil
          :status status
          :local-file nil
          :remote-status (or phase 'unprojected)
          :resolve-remote nil
          :remote-url nil
          :remote-error remote-error
          :download-state nil)))

(defconst qq-media--native-content-required-methods
  '("media.materialize"
    "media.cancel"
    "resource.status"
    "resource.open_local"
    "resource.close_local")
  "Native protocol methods required for content materialization.")

(defun qq-media--native-content-methods-ready-p ()
  "Return non-nil when native content materialization can start."
  (and (qq-runtime-current-account-id)
       (qq-server-ready-p)
       (cl-every #'qq-rpc-method-available-p
                 qq-media--native-content-required-methods)))

(defun qq-media--native-content-capabilities (segment media-id noun)
  "Return native content actions for SEGMENT and MEDIA-ID named NOUN."
  (let* ((local-file (qq-media-segment-local-file segment))
         (remote (qq-remote-media media-id))
         (content (qq-remote-media-part remote 'content))
         (phase (alist-get 'phase content))
         (problem (alist-get 'error content))
         (problem-message (alist-get 'message problem))
         (methods-ready (qq-media--native-content-methods-ready-p))
         (download-state (qq-media-segment-download-state segment))
         (download-status (plist-get download-state :status))
         (resolve-remote (and methods-ready (not local-file)))
         (remote-error
          (cond
           ((not (qq-runtime-current-account-id))
            (format "select an account before loading this %s" noun))
           ((not methods-ready)
            (format "native %s materialization methods are unavailable" noun))
           ((equal phase "failed")
            (or problem-message
                (format "remote %s materialization failed" noun)))))
         (status
          (or (qq-media--transfer-status-text download-state)
              (pcase phase
                ("materializing"
                 (let ((done (alist-get 'bytes_done content))
                       (total (or (alist-get 'bytes_total content)
                                  (alist-get 'expected_size content))))
                   (if total
                       (format "Loading %s/%s bytes" done total)
                     (format "Loading %s bytes" done))))
                ("materialized" "Ready")
                ("failed"
                 (format "Retry: %s"
                         (truncate-string-to-width
                          (or problem-message "materialization failed")
                          68 nil nil t)))
                (_ (unless local-file (format "Remote %s" noun)))))))
    (list :open (and (or local-file methods-ready) t)
          :download (and resolve-remote
                         (not (memq download-status
                                    '(downloading downloaded))))
          :save (and (or local-file methods-ready) t)
          :copy-url nil
          :status status
          :local-file local-file
          :remote-status (or phase 'unprojected)
          :resolve-remote resolve-remote
          :remote-url nil
          :remote-error remote-error
          :download-state download-state)))

(defun qq-media--native-image-capabilities (segment media-id)
  "Return the action/status model for native image SEGMENT and MEDIA-ID."
  (qq-media--native-content-capabilities segment media-id "image"))

(defun qq-media--native-video-capabilities (segment media-id)
  "Return the action/status model for native video SEGMENT and MEDIA-ID."
  (qq-media--native-content-capabilities segment media-id "video"))

(defun qq-media-segment-capabilities (segment)
  "Return the centralized action/status model for media SEGMENT.

Native records and images use only their opaque remote-media handles.  Other
segments remain on the dormant v1 resource model while that client is
retired."
  (cond
   ((qq-media--native-record-media-id segment)
    (qq-media--native-record-capabilities
     (qq-media--native-record-media-id segment)))
   ((qq-media--native-image-media-id segment)
    (qq-media--native-image-capabilities
     segment (qq-media--native-image-media-id segment)))
   ((qq-media--native-video-media-id segment)
    (qq-media--native-video-capabilities
     segment (qq-media--native-video-media-id segment)))
   ((qq-media--native-file-media-id segment)
    (qq-media--native-content-capabilities
     segment (qq-media--native-file-media-id segment)
     (symbol-name (or (qq-media--file-segment-kind segment) 'file))))
   (t
    (qq-media--legacy-segment-capabilities segment))))

(defun qq-media--segment-resource-key (segment)
  "Return logical resource cache key for SEGMENT, or nil."
  (let* ((type (alist-get 'type segment))
         (file-key (qq-media--segment-file-key segment))
         (url (qq-media--segment-url segment))
         (data (alist-get 'data segment))
         (record-media-id (qq-media--native-record-media-id segment))
         (image-media-id (qq-media--native-image-media-id segment))
         (video-media-id (qq-media--native-video-media-id segment))
         (file-media-id (qq-media--native-file-media-id segment))
         (emoji-id (alist-get 'id data)))
    (pcase type
      ("image" (or (and image-media-id
                        (qq-media--native-image-key image-media-id))
                   (and file-key (format "image:%s" file-key))
                   (and (appkit-media-url-present-p url)
                        (format "image-url:%s" url))))
      ("video"
       (or (and video-media-id
                (qq-media--native-video-key video-media-id))
           (and file-key (format "video:%s" file-key))
           (and (appkit-media-url-present-p url)
                (format "video-url:%s" url))))
      ((or "file" "group_file")
       (or (and file-media-id (qq-media--native-file-key file-media-id))
           (and file-key (format "%s:%s" type file-key))
           (and (appkit-media-url-present-p url)
                (format "%s-url:%s" type url))))
      ("record" (or (and record-media-id
                         (qq-media--native-record-key record-media-id))
                    (and file-key (format "record:%s" file-key))))
      ("face" (and emoji-id (format "face:%s" emoji-id)))
      ("mface" (or (and file-key (format "mface:%s" file-key))
                   (and (appkit-media-url-present-p url)
                        (format "mface-url:%s" url))))
      (_ nil))))

(defun qq-media--fetch-segment-resource (segment callback &optional errback)
  "Fetch media resource for SEGMENT and pass it to CALLBACK.

Native segments materialize their opaque `media_id'.  Other segments may use
only an existing local path or a direct URL."
  (let* ((type (alist-get 'type segment))
         (data (alist-get 'data segment))
         (emoji-id (alist-get 'id data))
         (error-fn (or errback #'qq-media--default-error)))
    (pcase type
      ("image"
       (if-let* ((media-id (qq-media--native-image-media-id segment)))
           (qq-media--fetch-native-image-resource
            segment (qq-media--native-image-key media-id)
            callback error-fn)
         (qq-media--resolve-fileish-segment
          segment callback error-fn
          "image segment has neither local file nor URL")))
      ("video"
       (if-let* ((media-id (qq-media--native-video-media-id segment)))
           (qq-media--fetch-native-video-resource
            segment (qq-media--native-video-key media-id)
            callback error-fn)
         (qq-media--resolve-fileish-segment
          segment callback error-fn
          "video segment has neither local file nor URL")))
      ((or "file" "group_file")
       (if-let* ((media-id (qq-media--native-file-media-id segment)))
           (qq-media--fetch-native-file-resource
            segment (qq-media--native-file-key media-id)
            callback error-fn)
         (qq-media--resolve-fileish-segment
          segment callback error-fn
          (format "%s segment has neither local file nor URL" type))))
      ("record"
       (if-let* ((local (qq-media--segment-existing-path segment)))
           (funcall callback `((file . ,local)))
         (funcall error-fn nil "record segment has no local file")))
      ("face"
       (cond
        ((not emoji-id)
         (funcall error-fn nil "face segment has no id"))
        ((qq-media--face-resource-from-local emoji-id)
         (funcall callback (qq-media--face-resource-from-local emoji-id)))
        (t
         (funcall error-fn nil
                  "base face has no local resource in the native backend"))))
      ("mface"
       (qq-media--resolve-fileish-segment
        segment callback error-fn
        "mface segment has neither local file nor URL"))
      (_
       (funcall error-fn nil (format "unsupported segment type for resource fetch: %s" type))))))

(defun qq-media-resolve-segment-resource (segment callback &optional errback)
  "Resolve media resource for SEGMENT and pass it to CALLBACK."
  (let ((cache-key (qq-media--segment-resource-key segment))
        (capabilities (qq-media-segment-capabilities segment)))
    (let ((url (plist-get capabilities :remote-url))
          (local (plist-get capabilities :local-file))
          (remote-error (plist-get capabilities :remote-error)))
      (cond
       (local
        (let ((resource (qq-media--resource-from-local+url local url)))
          (when cache-key
            (qq-media--cache-resource cache-key resource)
            (qq-media--note-cache-updated cache-key))
          (funcall callback resource)))
       ((not (plist-get capabilities :resolve-remote))
        (if errback
            (funcall errback nil (or remote-error "media resource is unavailable"))
          (user-error "qq: %s" (or remote-error "media resource is unavailable"))))
       ;; The video wire model has already resolved the one official URL.  Never
       ;; send its file token through the generic get_file resolver.
       ((and (equal (alist-get 'type segment) "video") url)
        (let ((resource `((url . ,url))))
          (when cache-key
            (qq-media--cache-resource cache-key resource)
            (qq-media--note-cache-updated cache-key))
          (funcall callback resource)))
       ((and cache-key (qq-media--cached-resource cache-key))
        (funcall callback (qq-media--cached-resource cache-key)))
       (cache-key
        (qq-media--fetch-segment-resource
         segment
         (lambda (resource)
           (let ((cached-resource (qq-media--cache-resource cache-key resource)))
             (qq-media--note-cache-updated cache-key)
             (funcall callback cached-resource)))
         errback))
       ((appkit-media-url-present-p url)
        (funcall callback `((url . ,url))))
       (errback
        (funcall errback nil "resource has neither local file, cache key, nor URL"))
       (t
        (user-error "qq: segment has neither local file nor URL"))))))

(cl-defun qq-media-segment-open (segment &key owner)
  "Resolve SEGMENT in an Effect owned by the exact initiating Surface."
  (if (qq-media--native-record-media-id segment)
      (qq-media-play-native-record
       segment :owner (qq-media--initiating-surface owner))
    (qq-media--send-open
     owner (list :segment (copy-tree segment)
                 :kind (qq-media-segment-kind segment)
                 :cache-key (qq-media--segment-resource-key segment)))))

(defun qq-media-segment-openable-p (segment)
  "Return non-nil when SEGMENT can be opened via `qq-media'."
  (plist-get (qq-media-segment-capabilities segment) :open))

(defun qq-media-segment-playable-p (segment)
  "Return non-nil when SEGMENT supports `qq-media-segment-play'."
  (and (eq (qq-media-segment-kind segment) 'video)
       (plist-get (qq-media-segment-capabilities segment) :open)))

(defun qq-media-segment-kind (segment)
  "Return semantic open kind for timeline SEGMENT."
  (let ((type (alist-get 'type segment)))
    (cond
     ((equal type "video") 'video)
     ((member type '("image" "face" "mface")) 'image)
     ((member type '("file" "group_file"))
      (qq-media--file-segment-kind segment))
     (t 'file))))

(cl-defun qq-media-open-resource (resource &optional kind cache-key &key owner)
  "Ask the exact initiating Surface to acquire and present QQ RESOURCE."
  (qq-media--send-open
   owner (list :resource (copy-tree resource)
               :kind (appkit-media-resource-kind
                      (qq-media--appkit-resource resource) kind)
               :cache-key cache-key)))

(defun qq-media-segment-default-save-name (segment)
  "Return default filename for saving SEGMENT locally."
  (let* ((data (alist-get 'data segment))
         (type (or (alist-get 'type segment) "media"))
         (cached-resource (qq-media--cached-resource (qq-media--segment-resource-key segment)))
         (seed (or (qq-media--segment-file-key segment)
                   (alist-get 'media_id data)
                   (alist-get 'id data)
                   (alist-get 'emoji_id data)
                   (substring (md5 (prin1-to-string segment)) 0 8)))
         (name (or (qq-media-segment-display-name segment)
                   (and cached-resource
                        (let ((file (alist-get 'file cached-resource)))
                          (and (stringp file) (file-name-nondirectory file))))
                   (appkit-media-url-filename (qq-media--segment-url segment))
                   (format "%s-%s.bin" type seed))))
    (appkit-media-sanitize-filename name)))

(defun qq-media-segment-download-key (segment)
  "Return stable download-state key for SEGMENT."
  (or (qq-media--segment-resource-key segment)
      (format "download:%s" (md5 (prin1-to-string segment)))))

(defun qq-media-segment-download-path (segment)
  "Return default local download path for SEGMENT."
  (let* ((key (qq-media-segment-download-key segment))
         (safe-name (qq-media-segment-default-save-name segment)))
    (expand-file-name
     (format "%s-%s" (substring (md5 key) 0 10) safe-name)
     qq-media-download-directory)))

(defun qq-media-segment-download-state (segment)
  "Return normalized download state plist for SEGMENT."
  (let* ((key (qq-media-segment-download-key segment))
         (entry (copy-tree (or (gethash key qq-media--download-state-table) '())))
         (path (or (plist-get entry :path)
                   (qq-media-segment-download-path segment)))
         (status (plist-get entry :status)))
    (setq entry (plist-put entry :path path))
    (when (and (eq status 'downloaded)
               (not (appkit-media-file-present-p path)))
      (setq entry (plist-put entry :status 'not-downloaded))
      (setq entry (plist-put entry :error nil))
      (setq status 'not-downloaded))
    (unless (plist-get entry :status)
      (setq entry (plist-put entry :status (if (appkit-media-file-present-p path)
                                               'downloaded
                                             'not-downloaded))))
    (puthash key entry qq-media--download-state-table)
    entry))

(defun qq-media--put-segment-download-state (segment entry)
  "Store download-state ENTRY for SEGMENT and notify UI."
  (let ((key (qq-media-segment-download-key segment)))
    (puthash key entry qq-media--download-state-table)
    (qq-media--note-cache-updated key)
    ;; A previous preview attempt may have had only a poster or no source at
    ;; all.  Once the actual media exists, allow preview extraction to retry.
    (when (and (eq (plist-get entry :status) 'downloaded)
               (qq-media--absolute-local-file-present-p
                (plist-get entry :path)))
      (when-let* ((preview-key (qq-media-segment-preview-key segment)))
        (when (gethash preview-key qq-media--preview-missing-cache)
          (remhash preview-key qq-media--preview-missing-cache)
          (qq-media--note-cache-updated preview-key)))))
  entry)

(defun qq-media-segment-local-file (segment)
  "Return best local file path for SEGMENT, or nil."
  (let* ((video-p (qq-media-videoish-segment-p segment))
         (segment-file (qq-media--segment-existing-path segment))
         (download-state (qq-media-segment-download-state segment))
         (download-path (plist-get download-state :path))
         (cached-resource (qq-media--cached-resource (qq-media--segment-resource-key segment)))
         (cached-file (and cached-resource (alist-get 'file cached-resource)))
         (preview-key (qq-media-segment-preview-key segment))
         ;; An image preview is itself a usable local image.  A video preview
         ;; is only a poster or a bounded GIF and must never become the source
         ;; handed to the video player.
         (preview-resource (and (not video-p)
                                preview-key
                                (qq-media--cached-resource preview-key)))
         (preview-file (and (not video-p)
                            (or (and preview-resource
                                     (alist-get 'file preview-resource))
                                (and preview-key
                                     (qq-media--remote-image-cache-existing-file
                                      preview-key))))))
    (cond
     ((if video-p
          (qq-media--video-local-file-present-p segment-file)
        (qq-media--absolute-local-file-present-p segment-file))
      segment-file)
     ((if video-p
          (qq-media--video-local-file-present-p download-path)
        (qq-media--absolute-local-file-present-p download-path))
      download-path)
     ((if video-p
          (qq-media--video-local-file-present-p cached-file cached-resource)
        (qq-media--absolute-local-file-present-p cached-file))
      cached-file)
     ((qq-media--absolute-local-file-present-p preview-file) preview-file)
     (t nil))))

(defun qq-media--segment-media-id (segment)
  "Return SEGMENT's opaque native media ID, or nil."
  (or (qq-media--native-record-media-id segment)
      (qq-media--native-image-media-id segment)
      (qq-media--native-video-media-id segment)
      (qq-media--native-file-media-id segment)))

(defun qq-media-segment-cancel-transfer (segment)
  "Cancel SEGMENT's in-flight download or native materialization."
  (let* ((state (qq-media-segment-download-state segment))
         (transfer (plist-get state :transfer))
         (media-id (qq-media--segment-media-id segment)))
    (cond
     ((appkit-media-transfer-p transfer)
      (appkit-media-cancel-transfer transfer))
     (media-id
      (qq-remote-media-cancel media-id))
     (t
      (user-error "qq: no media transfer is in progress")))))

(defun qq-media-segment-transfer (segment)
  "Return Appkit transfer control spec for SEGMENT, or nil.

Native materialization exposes `bytes_done' and `bytes_total'.  HTTP
downloads are shown without a byte count until the transport reports one."
  (let* ((download (qq-media-segment-download-state segment))
         (download-status (plist-get download :status))
         (media-id (qq-media--segment-media-id segment))
         (content (and media-id
                       (qq-remote-media-part
                        (qq-remote-media media-id) 'content)))
         (phase (alist-get 'phase content))
         (cancel (lambda ()
                   (qq-media-segment-cancel-transfer segment))))
    (cond
     ((eq download-status 'downloading)
      (list :direction 'download
            :state 'active
            :action cancel
            :action-label "Cancel"
            :help-echo "Cancel this download"))
     ((equal phase "materializing")
      (list :direction 'download
            :state 'active
            :bytes-done (alist-get 'bytes_done content)
            :bytes-total (or (alist-get 'bytes_total content)
                             (alist-get 'expected_size content))
            :action cancel
            :action-label "Cancel"
            :help-echo "Cancel this transfer"))
     (t nil))))

(cl-defun qq-media-segment-start-download
    (segment &optional open-after &key owner)
  "Download SEGMENT into `qq-media-download-directory'.

When OPEN-AFTER is non-nil, capture the exact initiating Surface before download."
  (when open-after
    (setq owner (qq-media--initiating-surface owner)))
  (let* ((capabilities (qq-media-segment-capabilities segment))
         (entry (plist-get capabilities :download-state))
         (path (plist-get entry :path))
         (status (plist-get entry :status)))
    (cond
     ((eq status 'downloading)
      (user-error "qq: media download already in progress"))
     ((and (eq status 'downloaded)
           (appkit-media-file-present-p path))
      (when open-after
        (qq-media-segment-open-local segment :owner owner))
      path)
     ((not (plist-get capabilities :download))
      (user-error "qq: %s"
                  (or (plist-get capabilities :remote-error)
                      "media resource cannot be downloaded")))
     (t
      (let ((token (make-symbol "qq-media-download")))
        (cl-labels
            ((owned-entry ()
               (let ((current (qq-media-segment-download-state segment)))
                 (and (eq token (plist-get current :token)) current)))
             (finish (status reason)
               (when-let* ((current (owned-entry)))
                 (setq current (plist-put current :status status))
                 (setq current (plist-put current :error reason))
                 (setq current (plist-put current :path path))
                 (setq current (plist-put current :transfer nil))
                 (setq current (plist-put current :token nil))
                 (qq-media--put-segment-download-state segment current)
                 t)))
          (setq entry (plist-put entry :status 'downloading))
          (setq entry (plist-put entry :error nil))
          (setq entry (plist-put entry :path path))
          (setq entry (plist-put entry :transfer nil))
          (setq entry (plist-put entry :token token))
          (qq-media--put-segment-download-state segment entry)
          (message "qq: downloading media -> %s" path)
          (qq-media-resolve-segment-resource
           segment
           (lambda (resource)
             (when (owned-entry)
               (condition-case err
                   (let ((transfer
                          (appkit-media-copy-or-download-resource-async
                           (qq-media--appkit-resource resource) path
                           (lambda (_file)
                             (when (finish 'downloaded nil)
                               (message "qq: downloaded media -> %s" path)
                               (when (and open-after (appkit-surface-live-p owner))
                                 (qq-media-segment-open-local
                                  segment :owner owner))))
                           (lambda (reason)
                             (when (finish 'error reason)
                               (message "qq: media download failed: %s"
                                        reason))))))
                     ;; Local copies and setup errors finish synchronously.
                     (when-let* ((current (owned-entry)))
                       (setq current (plist-put current :transfer transfer))
                       (qq-media--put-segment-download-state segment current)))
                 ((error quit)
                  (let ((reason (error-message-string err)))
                    (when (finish 'error reason)
                      (message "qq: media download setup failed: %s"
                               reason)))))))
           (lambda (_response reason)
             (when (finish 'error reason)
               (message "qq: media download failed: %s" reason))))
          nil))))))

(cl-defun qq-media-segment-open-local (segment &key owner)
  "Open the best local file available for SEGMENT.

OWNER lifecycle-owns an external player when SEGMENT is a video."
  (if-let* ((file (qq-media-segment-local-file segment)))
      (qq-media-open-resource
       `((file . ,file))
       (qq-media-segment-kind segment)
       (qq-media--segment-resource-key segment)
       :owner owner)
    (user-error "qq: media segment has no local file yet")))

(defun qq-media-segment-save-as (segment &optional target-path)
  "Save SEGMENT to TARGET-PATH, prompting when nil."
  (interactive)
  (let* ((capabilities (qq-media-segment-capabilities segment))
         (_ (unless (plist-get capabilities :save)
              (user-error "qq: %s"
                          (or (plist-get capabilities :remote-error)
                              "media resource cannot be saved"))))
         (default-name (qq-media-segment-default-save-name segment))
         (target (or target-path
                     (read-file-name "Save media as: "
                                     nil
                                     default-name
                                     nil
                                     default-name))))
    (cl-labels
        ((save-resource (resource)
           (appkit-media-copy-or-download-resource-async
            (qq-media--appkit-resource resource) target
            (lambda (_file) (message "qq: saved media -> %s" target))
            (lambda (reason)
              (message "qq: failed to save media: %s" reason)))))
      (condition-case err
          (if-let* ((local-file (qq-media-segment-local-file segment)))
              (save-resource
               `((file . ,local-file)
                 (name . ,(qq-media-segment-default-save-name segment))))
            (qq-media-resolve-segment-resource
             segment #'save-resource
             (lambda (_response reason)
               (message "qq: failed to save media: %s" reason))))
        (error
         (user-error "qq: failed to save media: %s"
                     (error-message-string err)))))))

(cl-defun qq-media-segment-play (segment &key owner)
  "Play video SEGMENT through the initiating Surface's media Effects."
  (unless (qq-media-segment-playable-p segment)
    (user-error "qq: segment is not playable"))
  (qq-media--send-open
   owner (list :segment (copy-tree segment) :kind 'video
               :cache-key (qq-media--segment-resource-key segment))))

(defun qq-media-message-primary-segment (message)
  "Return the most useful openable segment from MESSAGE, or nil."
  (let ((segments (alist-get 'segments message))
        found)
    (while (and segments (not found))
      (let ((segment (car segments)))
        (when (qq-media-segment-openable-p segment)
          (setq found segment))
        (setq segments (cdr segments))))
    found))

(defun qq-media-message-has-openable-resource-p (message)
  "Return non-nil when MESSAGE has at least one openable resource segment."
  (not (null (qq-media-message-primary-segment message))))

(cl-defun qq-media-open-message-resource (message &key owner)
  "Open the most relevant resource from MESSAGE.

OWNER lifecycle-owns an external player when the primary segment is a video."
  (if-let* ((segment (qq-media-message-primary-segment message)))
      (qq-media-segment-open segment :owner owner)
    (user-error "qq: message has no openable media segment")))

(defun qq-media--avatar-owner ()
  "Return the managed account owning an avatar request."
  (let ((owner (qq-runtime-current-account-id)))
    (unless (and owner (qq-account-get owner))
      (user-error "qq: avatar resolution requires a managed QQ account"))
    owner))

(defun qq-media--project-avatar-locator
    (result owner identity-key identity)
  "Project RESULT into an HTTPS avatar resource.

OWNER is the requesting account.  IDENTITY-KEY and IDENTITY identify the
exact user or group whose avatar was requested."
  (unless
      (and
       (qq-server-wire-exact-object-keys-p
        result (list 'account_id identity-key 'url))
       (equal (alist-get 'account_id result) owner)
       (equal (alist-get identity-key result) identity)
       (qq-protocol-non-empty-string-p (alist-get 'url result))
       (string-prefix-p "https://" (alist-get 'url result)))
    (error "qq: Gateway returned an invalid avatar locator"))
  `((url . ,(alist-get 'url result))))

(defun qq-media--fetch-avatar-locator
    (method identity-key identity callback errback)
  "Resolve one native avatar through METHOD.

IDENTITY-KEY names the exact decimal IDENTITY parameter.  CALLBACK receives
an owned media resource alist; ERRBACK follows the native RPC convention."
  (unless (qq-protocol-uint64-decimal-p identity)
    (user-error "qq: avatar resolution requires an exact decimal identity"))
  (let ((owner (qq-media--avatar-owner)))
    (qq-rpc-call
     method
     `((account_id . ,owner) (,identity-key . ,identity))
     :current-p (lambda () (and (qq-account-get owner) t))
     :stale-code "account_removed"
     :stale-message "QQ account was removed during avatar resolution"
     :projector
     (lambda (result)
       (qq-media--project-avatar-locator
        result owner identity-key identity))
     :callback
     (lambda (resource)
       (qq-runtime-with-account owner
         (qq-rpc-invoke callback resource)))
     :errback
     (lambda (body reason)
       (qq-runtime-with-account owner
         (qq-rpc-invoke errback body reason))))))

(defun qq-media--fetch-native-user-avatar-locator
    (user-id callback &optional errback)
  "Resolve USER-ID's native HTTPS avatar and call CALLBACK."
  (qq-media--fetch-avatar-locator
   "contact.get_user_avatar" 'user_uin user-id callback errback))

(defun qq-media--fetch-native-group-avatar
    (group-id callback &optional errback)
  "Resolve GROUP-ID's native HTTPS avatar and call CALLBACK."
  (qq-media--fetch-avatar-locator
   "contact.get_group_avatar" 'group_uin group-id callback errback))

(defun qq-media--native-user-avatar-resource (user-id)
  "Return an authoritative cached avatar resource for USER-ID, or nil.

The native friend directory is preferred.  Message snapshots remain a useful
fallback for identities observed outside that directory."
  (or
   (when-let* ((friend (qq-state-friend user-id))
               (url (alist-get 'avatar_url friend))
               ((appkit-media-url-present-p url)))
     `((url . ,url)))
   (catch 'resource
     (dolist (session (qq-state-sessions))
       (dolist (message (qq-state-session-messages (alist-get 'key session)))
         (when (and (equal (format "%s" (or (alist-get 'sender-id message) ""))
                           (format "%s" user-id))
                    (appkit-media-url-present-p
                     (alist-get 'sender-avatar-url message)))
           (throw 'resource
                  `((url . ,(alist-get 'sender-avatar-url message)))))))
     nil)))

(defun qq-media--fetch-native-user-avatar (user-id done error)
  "Resolve USER-ID avatar and call DONE or ERROR."
  (if-let* ((resource (qq-media--native-user-avatar-resource user-id)))
      (funcall done resource)
    (if (and (qq-runtime-current-account-id)
             (member "contact.get_user_avatar"
                     (qq-server-capabilities)))
        (qq-media--fetch-native-user-avatar-locator user-id done error)
      (funcall error nil "native avatar locator is unavailable"))))

(defun qq-media-open-user-avatar (user-id)
  "Acquire and present USER-ID's avatar in the initiating Surface."
  (unless user-id (user-error "qq: missing user id for avatar"))
  (qq-media--send-open
   nil (list :user-avatar user-id :kind 'image
             :cache-key (format "avatar:%s" user-id))))

(defun qq-media-open-group-avatar (group-id)
  "Acquire and present GROUP-ID's avatar in the initiating Surface."
  (unless group-id (user-error "qq: missing group id for avatar"))
  (qq-media--send-open
   nil (list :group-avatar group-id :kind 'image
             :cache-key (format "group-avatar:%s" group-id))))

(defun qq-media-open-session-avatar (session)
  "Open session avatar for SESSION."
  (let ((target-id (alist-get 'target-id session)))
    (pcase (alist-get 'type session)
      ('private (qq-media-open-user-avatar target-id))
      ('group (qq-media-open-group-avatar target-id))
      ('dataline (user-error "qq: dataline sessions have no QQ avatar"))
      (type (user-error "qq: %s sessions have no QQ avatar" type)))))

(defun qq-media--message-avatar-identity (message)
  "Return MESSAGE sender's canonical QQ user identity, or nil."
  (when-let* ((sender-id (or (alist-get 'sender-id message)
                             (alist-get 'user-id message)))
              ((qq-protocol-user-uin-p sender-id)))
    (list :user sender-id)))

(defun qq-media--message-snapshot-avatar-url (message)
  "Return MESSAGE's authoritative snapshot avatar URL, or nil."
  (let ((url (alist-get 'sender-avatar-url message)))
    (and (appkit-media-url-present-p url) url)))

(defun qq-media--message-snapshot-avatar-key (message)
  "Return the URL-scoped avatar cache key for MESSAGE, or nil."
  (when-let* ((url (qq-media--message-snapshot-avatar-url message)))
    (format "message-avatar-url:%s" url)))

(defun qq-media-message-avatar-cache-key (message)
  "Return the logical avatar cache key affecting MESSAGE, or nil."
  (or (qq-media--message-snapshot-avatar-key message)
      (pcase (qq-media--message-avatar-identity message)
        (`(:user ,user-id) (format "avatar:%s" user-id)))))

(defun qq-media-open-message-avatar (message)
  "Open sender avatar for MESSAGE."
  (if-let* ((url (qq-media--message-snapshot-avatar-url message))
            (key (qq-media--message-snapshot-avatar-key message)))
      (qq-media-open-image-url key url)
    (pcase (qq-media--message-avatar-identity message)
      (`(:user ,user-id) (qq-media-open-user-avatar user-id))
      (_ (user-error "qq: message sender has no native avatar identity")))))

(defun qq-media-message-avatar-image (message)
  "Return the identity-correct inline sender avatar for MESSAGE."
  (let ((snapshot-url (qq-media--message-snapshot-avatar-url message)))
    (if snapshot-url
        (qq-media--ensure-resource-image
         (qq-media--message-snapshot-avatar-key message)
         (lambda (done _error)
           (funcall done `((url . ,snapshot-url))))
         qq-media-avatar-image-height
         #'qq-media--avatar-image-from-file)
      (pcase (qq-media--message-avatar-identity message)
        (`(:user ,user-id) (qq-media-avatar-image user-id))))))

(defun qq-media-avatar-image (user-id)
  "Return inline avatar image for USER-ID, triggering fetch when needed."
  (let ((key (format "avatar:%s" user-id)))
    (qq-media--ensure-resource-image
     key
     (lambda (done error)
       (qq-media--fetch-native-user-avatar user-id done error))
     qq-media-avatar-image-height
     #'qq-media--avatar-image-from-file)))

(defun qq-media-avatar-display-string (user-id)
  "Return inline display string for USER-ID avatar.

When image data is not ready yet, return a textual fallback."
  (qq-media--image-display-string
   (qq-media-avatar-image user-id)
   "@"))

(defun qq-media-avatar-cached-display-string (user-id)
  "Return cached inline avatar for USER-ID without starting network work."
  (qq-media--image-display-string
   (qq-media--cached-image (format "avatar:%s" user-id))
   "@"))

(defun qq-media-url-preview-image (key url &optional max-height)
  "Return preview image for remote URL under cache KEY.

Trigger an asynchronous download when the image is not cached yet."
  (when (appkit-media-url-present-p url)
    (qq-media--ensure-resource-image
     key
     (lambda (done _error)
       (funcall done `((url . ,url))))
     max-height
     #'qq-media--preview-image-from-file)))

(defun qq-media-url-preview-display-string (key url fallback &optional max-height)
  "Return display string for remote image URL cached under KEY.

Use FALLBACK until the preview is available."
  (qq-media--image-display-string
   (qq-media-url-preview-image key url max-height)
   fallback))

(defun qq-media-url-one-line-preview-image (key url)
  "Return KEY's one-line URL thumbnail, scheduling its shared resource load.

KEY must name a resource used exclusively in compact one-line presentation."
  (when (appkit-media-url-present-p url)
    (qq-media--ensure-resource-image
     key
     (lambda (done _error)
       (funcall done `((url . ,url))))
     nil
     #'qq-media--one-line-preview-image-from-file)))

(defun qq-media-url-one-line-preview-display-string (key url fallback)
  "Return URL as KEY's one-line thumbnail, or FALLBACK while it loads."
  (appkit-media-one-line-image-display-string
   (qq-media-url-one-line-preview-image key url)
   fallback))

(defun qq-media-poke-image-cache-key (url)
  "Return the cache key used for a decorative POKE image URL."
  (and (appkit-media-url-present-p url)
       (format "poke-image-url:%s" url)))

(defun qq-media-open-image-url (key url)
  "Open remote image URL using media cache KEY."
  (unless (appkit-media-url-present-p url)
    (user-error "qq: image has no remote URL"))
  (qq-media-open-resource `((url . ,url)) 'image key))

(defun qq-media-group-avatar-image (group-id)
  "Return inline group avatar image for GROUP-ID, triggering fetch when needed."
  (qq-media--ensure-resource-image
   (format "group-avatar:%s" group-id)
   (lambda (done error)
     (qq-media--fetch-native-group-avatar group-id done error))
   qq-media-avatar-image-height
   #'qq-media--avatar-image-from-file))

(defun qq-media-group-avatar-display-string (group-id)
  "Return inline display string for GROUP-ID avatar.

When image data is not ready yet, return a textual fallback."
  (qq-media--image-display-string
   (qq-media-group-avatar-image group-id)
   "#"))

(defun qq-media-group-avatar-cached-display-string (group-id)
  "Return cached inline group avatar for GROUP-ID without fetching it."
  (qq-media--image-display-string
   (qq-media--cached-image (format "group-avatar:%s" group-id))
   "#"))

(defun qq-media-session-avatar-cache-key (session)
  "Return the exact avatar cache key used to present SESSION, or nil."
  (let ((target-id (alist-get 'target-id session)))
    (when target-id
      (pcase (alist-get 'type session)
        ('private (format "avatar:%s" target-id))
        ('group (format "group-avatar:%s" target-id))
        (_ nil)))))

(defun qq-media-session-avatar-display-string (session)
  "Return one-line destination avatar or icon for SESSION.

When the current UI account is not represented by a managed Gateway account,
use only cached presentation and do not start a backend request."
  (let ((target-id (alist-get 'target-id session))
        (resolve-p
         (qq-account-get (qq-runtime-current-account-id))))
    (pcase (alist-get 'type session)
      ('private
       (if resolve-p
           (qq-media-avatar-display-string target-id)
         (qq-media-avatar-cached-display-string target-id)))
      ('group
       (if resolve-p
           (qq-media-group-avatar-display-string target-id)
         (qq-media-group-avatar-cached-display-string target-id)))
      ('dataline "📱")
      ('guild-channel "#")
      ('service "◇")
      (_ "?"))))

(defun qq-media-session-avatar-cached-display-string (session)
  "Return cached one-line destination avatar or icon for SESSION.

Unlike `qq-media-session-avatar-display-string', this never starts resource
resolution and is safe while the account has no managed backend runtime."
  (let ((target-id (alist-get 'target-id session)))
    (pcase (alist-get 'type session)
      ('private (qq-media-avatar-cached-display-string target-id))
      ('group (qq-media-group-avatar-cached-display-string target-id))
      ('dataline "📱")
      ('guild-channel "#")
      ('service "◇")
      (_ "?"))))

(defun qq-media--system-emoji-download-info-p (value)
  "Return non-nil when VALUE is one closed system-emoji download locator."
  (and (qq-server-wire-exact-object-keys-p
        value '(base_resource_url advanced_resource_url))
       (cl-every
        (lambda (key)
          (let ((url (alist-get key value)))
            (or (null url)
                (and (qq-protocol-non-empty-string-p url)
                     (string-prefix-p "https://" url)))))
        '(base_resource_url advanced_resource_url))))

(defun qq-media--system-emoji-entry-p (entry)
  "Return non-nil when ENTRY is one exact Gateway system emoji."
  (and
   (qq-server-wire-exact-object-keys-p
    entry
    '(id description qzone_code qcid emoji_type animated_pack_id
      animated_sticker_id download associate_words hidden start_time
      end_time animation_width animation_height interact_pack_id
      interact_sticker_id))
   (qq-protocol-non-empty-string-p (alist-get 'id entry))
   (stringp (alist-get 'description entry))
   (stringp (alist-get 'qzone_code entry))
   (cl-every
    (lambda (key)
      (let ((value (alist-get key entry)))
        (and (integerp value) (<= 0 value #xffffffff))))
    '(qcid emoji_type animation_width animation_height))
   (< (alist-get 'emoji_type entry) 6)
   (cl-every
    (lambda (key)
      (let ((value (alist-get key entry)))
        (or (null value)
            (and (integerp value) (< 0 value #xffffffff)))))
    '(animated_pack_id animated_sticker_id interact_pack_id interact_sticker_id))
   (qq-media--system-emoji-download-info-p (alist-get 'download entry))
   (let ((words (alist-get 'associate_words entry)))
     (and (proper-list-p words)
          (<= (length words) 64)
          (cl-every #'stringp words)))
   (memq (alist-get 'hidden entry) '(t nil :false))
   (cl-every
    (lambda (key)
      (let ((value (alist-get key entry)))
        (or (null value) (and (integerp value) (< 0 value)))))
    '(start_time end_time))))

(defun qq-media--project-system-emoji-catalog (result)
  "Validate and return Gateway system-emoji catalog RESULT."
  (unless (qq-server-wire-exact-object-keys-p result '(panels))
    (error "qq: Gateway returned malformed system-emoji catalog"))
  (let ((panels (alist-get 'panels result))
        (count 0))
    (unless
        (and
         (proper-list-p panels)
         (<= (length panels) 3)
         (cl-every
          (lambda (panel)
            (and
             (qq-server-wire-exact-object-keys-p panel '(kind groups download))
             (member (alist-get 'kind panel) '("normal" "super" "red_heart"))
             (qq-media--system-emoji-download-info-p
              (alist-get 'download panel))
             (let ((groups (alist-get 'groups panel)))
               (and
                (proper-list-p groups)
                (<= (length groups) 256)
                (cl-every
                 (lambda (group)
                   (let ((emojis (alist-get 'emojis group)))
                     (and
                      (qq-server-wire-exact-object-keys-p
                       group '(name emojis start_time end_time group_type))
                      (stringp (alist-get 'name group))
                      (integerp (alist-get 'group_type group))
                      (proper-list-p emojis)
                      (<= (+ count (length emojis)) 4096)
                      (progn (setq count (+ count (length emojis))) t)
                      (cl-every #'qq-media--system-emoji-entry-p emojis))))
                 groups)))))
          panels))
      (error "qq: Gateway returned invalid system-emoji catalog panels"))
    (copy-tree result)))

(defun qq-media--system-emoji-table-from-catalog (catalog)
  "Build a numeric face-id table from validated CATALOG."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (panel (alist-get 'panels catalog))
      (dolist (group (alist-get 'groups panel))
        (dolist (entry (alist-get 'emojis group))
          (let ((id (alist-get 'id entry)))
            (when (and (stringp id)
                       (string-match-p "\\`[0-9]+\\'" id))
              (puthash id entry table))))))
    table))

(defun qq-media--ensure-system-emoji-catalog ()
  "Start at most one system-emoji catalog request for the current account."
  (when-let* ((owner (qq-runtime-current-account-id))
              ((qq-account-get owner))
              ((member "system_emoji.list" (qq-server-capabilities)))
              ((not (gethash owner qq-media--system-emoji-tables)))
              ((not (gethash owner qq-media--system-emoji-requests))))
    (puthash owner t qq-media--system-emoji-requests)
    (qq-rpc-call
     "system_emoji.list" `((account_id . ,owner))
     :current-p (lambda () (and (qq-account-get owner) t))
     :stale-code "account_removed"
     :stale-message "QQ account was removed during system-emoji catalog fetch"
     :projector #'qq-media--project-system-emoji-catalog
     :callback
     (lambda (catalog)
       (remhash owner qq-media--system-emoji-requests)
       (puthash owner
                (qq-media--system-emoji-table-from-catalog catalog)
                qq-media--system-emoji-tables)
       (qq-media--note-cache-updated "system-emoji-catalog"))
     :errback
     (lambda (_body _reason)
       (remhash owner qq-media--system-emoji-requests)))))

(defun qq-media-system-emoji-entry (emoji-id)
  "Return current account catalog entry for numeric EMOJI-ID, or nil."
  (qq-media--ensure-system-emoji-catalog)
  (when-let* ((owner (qq-runtime-current-account-id))
              (table (gethash owner qq-media--system-emoji-tables)))
    (gethash (format "%s" emoji-id) table)))

(defvar qq-media--face-names-table nil
  "Lazy hash table: face id string → QDes name (e.g. \"/斜眼笑\").")

(defun qq-media--face-names-file ()
  "Return resolved path to the face-names JSON, preferring a readable file."
  (let* ((configured qq-media-face-names-file)
         (lib (or (locate-library "qq-media.el")
                  (locate-library "qq-customize.el")))
         (beside (and lib
                      (expand-file-name
                       "qq-face-names.json"
                       (file-name-directory (file-truename lib))))))
    (cond
     ((and (stringp configured) (file-readable-p configured)) configured)
     ((and (stringp beside) (file-readable-p beside)) beside)
     (t configured))))

(defun qq-media--load-face-names-table ()
  "Load face-names JSON into `qq-media--face-names-table'."
  (or qq-media--face-names-table
      (let ((table (make-hash-table :test #'equal))
            (file (qq-media--face-names-file)))
        (when (and (stringp file) (file-readable-p file))
          (condition-case err
              (let* ((json-object-type 'alist)
                     (json-array-type 'list)
                     (json-key-type 'string)
                     (json-false nil)
                     (data (json-read-file file)))
                (dolist (pair data)
                  (when (and (consp pair)
                             (stringp (car pair))
                             (stringp (cdr pair)))
                    (puthash (car pair) (cdr pair) table))))
            (error
             (message "qq: failed to load face names from %s: %s"
                      file (error-message-string err)))))
        (setq qq-media--face-names-table table)
        table)))

(defun qq-media-face-name (emoji-id)
  "Return human-readable QQ face name for EMOJI-ID, or nil."
  (let* ((id (format "%s" emoji-id))
         (entry (qq-media-system-emoji-entry id))
         (catalog-name (and entry (alist-get 'description entry)))
         (table (qq-media--load-face-names-table)))
    (or (and (qq-protocol-non-empty-string-p catalog-name) catalog-name)
        (gethash id table))))

(defun qq-media--face-id-number (id)
  "Return numeric value of face ID string, or nil."
  (and (stringp id)
       (string-match-p "\\`[0-9]+\\'" id)
       (string-to-number id)))

(defun qq-media-face-completion-candidates ()
  "Return base face candidates sorted by numeric id.

Each candidate looks like \"/斜眼笑  (178)\" so users can search by name
or numeric id.  Use `qq-media-face-id-from-completion'.

Order is by face id (0, 1, 2, …) — the same order as QQ's default
emoji panel.  Pair with `qq-media-face-completion-table' so Vertico
does not re-sort by string length/history."
  (let ((table (copy-hash-table (qq-media--load-face-names-table)))
        (candidates nil))
    (when-let* ((owner (qq-runtime-current-account-id))
                (catalog (gethash owner qq-media--system-emoji-tables)))
      (maphash
       (lambda (id entry)
         (let ((name (alist-get 'description entry)))
           (when (qq-protocol-non-empty-string-p name)
             (puthash id name table))))
       catalog))
    (maphash
     (lambda (id name)
       (push (cons id (format "%s  (%s)"
                              (or name (format "[face:%s]" id))
                              id))
             candidates))
     table)
    (mapcar
     #'cdr
     (sort candidates
           (lambda (a b)
             (let ((ida (qq-media--face-id-number (car a)))
                   (idb (qq-media--face-id-number (car b))))
               (cond
                ((and ida idb) (< ida idb))
                (ida t)
                (idb nil)
                (t (string-lessp (car a) (car b))))))))))

(defun qq-media-face-id-from-completion (candidate)
  "Extract face id string from a `qq-media-face-completion-candidates' CANDIDATE."
  (let ((text (and (stringp candidate)
                   (substring-no-properties candidate))))
    (cond
     ((and text
           (string-match "(\\([0-9]+\\))\\'" text))
      (match-string 1 text))
     ((and text
           (string-match-p "\\`[0-9]+\\'" text))
      text)
     (t nil))))

(defun qq-media--face-completion-prefix (id)
  "Return minibuffer prefix string with the face image for ID, or spaces."
  (let* ((file (qq-media--local-base-emoji-file id))
         (image (and file
                     (qq-media--image-from-file
                      file
                      (max 1 qq-media-face-image-height)))))
    (if image
        (concat (propertize " " 'display image) " ")
      "  ")))

(defun qq-media-face-affixation-function (candidates)
  "Affixation function: show local face PNG before each CANDIDATE.

Uses LinuxQQ `default-emojis/<id>.png' only (sync).  Missing files get
a blank spacer so columns stay aligned."
  (mapcar
   (lambda (cand)
     (let ((id (qq-media-face-id-from-completion cand)))
       (list cand
             (if id (qq-media--face-completion-prefix id) "  ")
             "")))
   candidates))

(defun qq-media-face-completion-table ()
  "Completion table for base QQ faces with images and stable id order.

Metadata:
- `display-sort-function'/`cycle-sort-function' = identity (keep id order)
- `affixation-function' = face PNG prefix for Vertico/Icomplete"
  (let ((candidates (qq-media-face-completion-candidates)))
    (lambda (string pred action)
      (if (eq action 'metadata)
          '(metadata
            (category . qq-face)
            (display-sort-function . identity)
            (cycle-sort-function . identity)
            (affixation-function . qq-media-face-affixation-function))
        (complete-with-action action candidates string pred)))))


;;; Favorite / custom faces (收藏表情)

(defun qq-media-refresh-custom-faces
    (&optional callback errback force-refresh)
  "Fetch the selected account's ordered native favorite entries.

Gateway remains the sole catalog cache.  CALLBACK receives the entry list;
ERRBACK receives the Gateway error body and reason.  With FORCE-REFRESH
non-nil, bypass Gateway's replaceable cache."
  (qq-favorite-emoji-list
   force-refresh
   (lambda (catalog)
     (when callback
       (funcall callback (copy-tree (alist-get 'entries catalog)))))
   errback))

(defun qq-media-ensure-custom-faces (&optional callback errback force)
  "Call CALLBACK with the selected account's native favorite entries.

FORCE bypasses Gateway's catalog cache.  Emacs deliberately keeps no second
catalog cache, so account switching cannot expose another account's entries."
  (qq-media-refresh-custom-faces callback errback force))

(defun qq-media-custom-face-id (face)
  "Return FACE's durable native favorite identity, or nil."
  (let ((favorite-id (and (listp face)
                          (alist-get 'favorite_emoji_id face))))
    (and (qq-favorite-emoji-id-p favorite-id) favorite-id)))

(defun qq-media-custom-face-label (face &optional index)
  "Return a stable human completion label for favorite FACE.

INDEX keeps candidates distinct even if their short MD5 prefixes collide."
  (let* ((md5 (alist-get 'md5 face))
         (short (if (and (stringp md5) (>= (length md5) 8))
                    (concat (substring md5 0 8) "…")
                  "favorite"))
         (base (format "[收藏] %s" short)))
    (if index
        (format "%s  (%d)" base (1+ index))
      base)))

(defun qq-media-custom-face-image (face)
  "Return FACE's cached preview, asynchronously fetching its catalog URL."
  (when-let* ((favorite-id (qq-media-custom-face-id face))
              (url (alist-get 'url face))
              ((appkit-media-url-present-p url)))
    (qq-media-url-preview-image
     (format "favorite-emoji:%s" favorite-id)
     url
     (max qq-media-face-image-height 32))))

(defun qq-media--custom-face-completion-prefix (face)
  "Return a visual completion prefix for favorite FACE."
  (if-let* ((image (qq-media-custom-face-image face)))
      (concat (propertize " " 'display image) " ")
    "  "))

(defun qq-media-custom-face-display-string (face)
  "Return composer display text for native favorite FACE."
  (qq-media--image-display-string
   (qq-media-custom-face-image face)
   (qq-media-custom-face-label face)))

(defun qq-media-custom-face-sendable-p (face)
  "Return non-nil when FACE carries one durable native identity."
  (and (qq-media-custom-face-id face) t))

(defun qq-media-custom-face-to-segment (face)
  "Convert native favorite FACE into a durable composer segment.

No URL, local path, Resource ID, or Prepared Attachment ID enters the draft.
Those shorter-lived capabilities are acquired only when the draft is sent."
  (let ((favorite-id (qq-media-custom-face-id face)))
    (unless favorite-id
      (user-error "qq: favorite face has no durable identity"))
    `((type . "favorite_emoji")
      (data . ((favorite_emoji_id . ,favorite-id))))))

(defun qq-media-face-text-fallback (emoji-id)
  "Return plain-text fallback for face EMOJI-ID (never a CQ blob)."
  (or (qq-media-face-name emoji-id)
      (format "[face:%s]" emoji-id)))

(defun qq-media--system-emoji-cache-roots ()
  "Return configured and discovered LinuxQQ dynamic emoji cache roots."
  (delete-dups
   (seq-filter
    #'file-directory-p
    (append
     (mapcar #'expand-file-name qq-media-system-emoji-cache-directories)
     (file-expand-wildcards
      (expand-file-name
       ".config/QQ/nt_qq_*/nt_data/Emoji/BaseEmojiSyastems/EmojiSystermResource"
       "~")
      t)))))

(defun qq-media--local-base-emoji-lottie-file (emoji-id)
  "Return LinuxQQ's cached Lottie JSON for EMOJI-ID, or nil."
  (let ((id (format "%s" emoji-id)))
    (seq-some
     (lambda (root)
       (let ((file (expand-file-name
                    (format "%s/lottie/%s.json" id id) root)))
         (and (file-readable-p file) file)))
     (qq-media--system-emoji-cache-roots))))

(defun qq-media-face-to-segment (emoji-id)
  "Build one closed composer segment for system EMOJI-ID."
  (let* ((id (format "%s" emoji-id))
         (number (qq-media--face-id-number id))
         (entry (qq-media-system-emoji-entry id))
         (emoji-type (and entry (alist-get 'emoji_type entry)))
         (pack-id (and entry (alist-get 'animated_pack_id entry)))
         (sticker-id (and entry (alist-get 'animated_sticker_id entry)))
         (description (and entry (alist-get 'description entry))))
    (unless (and number (<= 0 number #xffffffff))
      (user-error "qq: system face id is not a uint32"))
    (cond
     ((< number 260)
      `((type . "face")
        (data . ((id . ,id) (face_type . "basic")
                 ,@(when description `((description . ,description)))))))
     ((and (memq emoji-type '(1 2)) pack-id sticker-id)
      `((type . "face")
        (data . ((id . ,id) (face_type . "animated")
                 (pack_id . ,pack-id) (sticker_id . ,sticker-id)
                 ,@(when description `((description . ,description)))))))
     ((and entry (= (or emoji-type 0) 0))
      `((type . "face")
        (data . ((id . ,id) (face_type . "small")
                 ,@(when description `((description . ,description)))))))
     (t
      (qq-media--ensure-system-emoji-catalog)
      (user-error "qq: system face %s needs its native catalog metadata" id)))))

(defun qq-media--local-base-emoji-file (emoji-id)
  "Return the best LinuxQQ image resource for EMOJI-ID, or nil."
  (let* ((id (format "%s" emoji-id))
         (dynamic
          (seq-some
           (lambda (root)
             (seq-find
              #'file-readable-p
              (list
               (expand-file-name (format "%s/apng/%s.png" id id) root)
               (expand-file-name (format "%s/png/%s.png" id id) root)
               (expand-file-name (format "%s/png/%s_0.png" id id) root))))
           (qq-media--system-emoji-cache-roots)))
         (dir qq-media-default-emoji-directory))
    (or dynamic
        (when (and (stringp dir)
                   (not (string-empty-p id))
                   (file-directory-p dir))
          (seq-find
           #'file-readable-p
           (mapcar (lambda (ext)
                     (expand-file-name (concat id "." ext) dir))
                   '("png" "gif" "webp" "jpg" "jpeg")))))))

(defun qq-media--face-resource-from-local (emoji-id)
  "Return resource alist for local face EMOJI-ID, or nil."
  (when-let* ((file (qq-media--local-base-emoji-file emoji-id)))
    `((file . ,file)
      (emoji_id . ,(format "%s" emoji-id))
      (description . ,(qq-media-face-name emoji-id))
      (animated . ,(and (string-match-p "/apng/" file) t)))))

(defun qq-media--prepare-animated-face-resource (resource callback)
  "Pass RESOURCE to CALLBACK, converting native APNG to animated GIF.

Emacs' PNG loader displays APNG as a single frame.  QQ's native base emoji
service returns APNG resources, so convert those once into the existing media
cache; GIF is then handled by appkit's bounded inline-animation machinery."
  (let* ((resource (copy-tree resource))
         (file (alist-get 'file resource))
         (animated (qq-media--json-truthy-p (alist-get 'animated resource)))
         (ffmpeg (and animated (executable-find "ffmpeg"))))
    (if (not (and ffmpeg
                  (appkit-media-file-present-p file)
                  (string-match-p "\\.png\\'" (downcase file))))
        (funcall callback resource)
      (let* ((target (expand-file-name
                      (format "face-animation-%s.gif" (md5 file))
                      qq-media-cache-directory)))
        (if (appkit-media-file-present-p target)
            (progn
              (setf (alist-get 'file resource) target)
              (funcall callback resource))
          (make-directory qq-media-cache-directory t)
          (let ((buffer (generate-new-buffer " *qq-face-apng*")))
            (make-process
             :name (format "qq-face-apng-%s" (substring (md5 file) 0 8))
             :buffer buffer
             :noquery t
             :command
             (list ffmpeg "-nostdin" "-y" "-loglevel" "error"
                   "-i" file "-filter_complex"
                   (concat "[0:v]fps=20,scale=128:-1:flags=lanczos,split[a][b];"
                           "[a]palettegen=max_colors=128[p];"
                           "[b][p]paletteuse=dither=bayer:bayer_scale=3")
                   "-loop" "0" target)
             :sentinel
             (lambda (process _event)
               (when (memq (process-status process) '(exit signal))
                 (unwind-protect
                     (progn
                       (when (and (= (process-exit-status process) 0)
                                  (appkit-media-file-present-p target))
                         (setf (alist-get 'file resource) target))
                       (funcall callback resource))
                   (when (buffer-live-p (process-buffer process))
                     (kill-buffer (process-buffer process)))))))))))))

(defun qq-media--face-image-from-file (file height)
  "Create a base-face image from FILE, enlarging animated resources."
  (let ((image (qq-media--image-from-file file height)))
    (if (and image (appkit-media-inline-animation-image-p image))
        (qq-media--image-from-file file qq-media-animated-face-image-height)
      image)))

(defun qq-media-face-image (emoji-id)
  "Return inline QQ system-face image for EMOJI-ID.

Dynamic LinuxQQ account caches take precedence over the packaged static
fallback.  APNG resources are converted once through the bounded GIF path."
  (let* ((id (format "%s" emoji-id))
         (frame (gethash id qq-media--lottie-current-frames))
         (key (format "face:%s" id))
         (local (qq-media--face-resource-from-local id)))
    (cond
     ((and frame (file-readable-p frame))
      (qq-media--image-from-file frame qq-media-animated-face-image-height))
     (local
      (qq-media--cache-resource key local)
      (qq-media--ensure-resource-image
       key
       (lambda (done _error)
         (if (alist-get 'animated local)
             (qq-media--prepare-animated-face-resource local done)
           (funcall done local)))
       qq-media-face-image-height
       #'qq-media--face-image-from-file)))))

(defun qq-media-face-display-string (emoji-id &optional description)
  "Return inline display string for QQ face EMOJI-ID.

Prefer a dynamic/static image, then DESCRIPTION or the catalog/static name.
When native Lottie JSON exists, attach playback identity to the projection."
  (let* ((id (format "%s" emoji-id))
         (lottie (qq-media--local-base-emoji-lottie-file id))
         (text
          (qq-media--image-display-string
           (qq-media-face-image id)
           (or (and (stringp description)
                    (not (string-empty-p description))
                    description)
               (qq-media-face-text-fallback id)))))
    (when (and lottie (> (length text) 0))
      (add-text-properties
       0 (length text)
       `(qq-system-face-id ,id
         help-echo "RET: play native Lottie system face")
       text))
    text))

(defun qq-media--lottie-player-current-p (id owner)
  "Return non-nil when OWNER still owns system face ID playback."
  (eq (gethash id qq-media--lottie-players) owner))

(defun qq-media--publish-lottie-frame (id owner bytes)
  "Publish complete PNG BYTES for system face ID still owned by OWNER."
  (when (qq-media--lottie-player-current-p id owner)
    (let* ((directory (expand-file-name "face-lottie-playing/"
                                        qq-media-cache-directory))
           (file (progn
                   (make-directory directory t)
                   (make-temp-file
                    (expand-file-name (format "%s-" id) directory)
                    nil ".png"))))
      (condition-case err
          (progn
            (let ((coding-system-for-write 'no-conversion))
              (write-region bytes nil file nil 'silent))
            (push file (plist-get owner :frame-files))
            (puthash id file qq-media--lottie-current-frames)
            (remhash (format "face:%s" id) qq-media--image-cache)
            (qq-media--note-cache-updated (format "face:%s" id)))
        (error
         (message "qq: could not project Lottie frame: %s"
                  (error-message-string err))
         (ignore-errors (delete-file file))
         (when-let* ((process (plist-get owner :process)))
           (when (process-live-p process)
             (delete-process process))))))))

(defun qq-media--finish-lottie-player (id owner output)
  "Retire system face ID playback OWNER and process OUTPUT buffer."
  (when (qq-media--lottie-player-current-p id owner)
    (remhash id qq-media--lottie-players)
    (remhash id qq-media--lottie-current-frames)
    (remhash (format "face:%s" id) qq-media--image-cache)
    (qq-media--note-cache-updated (format "face:%s" id)))
  (when-let* ((files (plist-get owner :frame-files)))
    (run-at-time
     0.5 nil
     (lambda (retired-files)
       (dolist (file retired-files)
         (ignore-errors (delete-file file))))
     files))
  (when (buffer-live-p output)
    (kill-buffer output)))

(defun qq-media-play-system-face (emoji-id)
  "Stream native Lottie frames for system EMOJI-ID."
  (interactive "sSystem face id: ")
  (let* ((id (format "%s" emoji-id))
         (source (qq-media--local-base-emoji-lottie-file id))
         (renderer qq-media-lottie-renderer-command))
    (unless source
      (user-error "qq: system face %s has no local Lottie resource" id))
    (unless (and renderer (file-executable-p renderer))
      (user-error "qq: Lottie playback requires tgs2png"))
    (when-let* ((previous (gethash id qq-media--lottie-players))
                (process (plist-get previous :process)))
      (when (process-live-p process)
        (delete-process process)))
    (let* ((owner (list :process nil :frame-files nil))
           (output (generate-new-buffer " *qq-system-face-lottie*"))
           process)
      (puthash id owner qq-media--lottie-players)
      (with-current-buffer output
        (set-buffer-multibyte nil))
      (condition-case err
          (progn
            (setq process
                  (make-process
                   :name (format "qq-system-face-%s" id)
                   :command
                   (list renderer
                         "-s" (format "0x%d" qq-media-animated-face-image-height)
                         source)
                   :buffer output
                   :stderr nil
                   :coding 'no-conversion
                   :noquery t
                   :connection-type 'pipe
                   :filter
                   (lambda (proc bytes)
                     (when (and (process-live-p proc)
                                (qq-media--lottie-player-current-p id owner)
                                (buffer-live-p output))
                       (condition-case filter-error
                           (with-current-buffer output
                             (goto-char (point-max))
                             (insert bytes)
                             (when-let*
                                 ((frame
                                   (appkit-media-png-stream-pop-latest output)))
                               (qq-media--publish-lottie-frame
                                id owner frame)))
                         (error
                          (message "qq: invalid Lottie frame stream: %s"
                                   (error-message-string filter-error))
                          (delete-process proc)))))
                   :sentinel
                   (lambda (proc _event)
                     (when (memq (process-status proc) '(exit signal))
                       (qq-media--finish-lottie-player id owner output)))))
            (setf (plist-get owner :process) process)
            t)
        (error
         (remhash id qq-media--lottie-players)
         (when (buffer-live-p output)
           (kill-buffer output))
         (user-error "qq: could not start Lottie playback: %s"
                     (error-message-string err)))))))

(defun qq-media-play-system-face-at-point ()
  "Play the native Lottie system face projected at point."
  (interactive)
  (let ((id (or (get-text-property (point) 'qq-system-face-id)
                (and (> (point) (point-min))
                     (get-text-property (1- (point)) 'qq-system-face-id)))))
    (if id
        (qq-media-play-system-face id)
      (user-error "qq: no Lottie system face at point"))))

(defun qq-media-segment-preview-key (segment)
  "Return preview cache key for SEGMENT, or nil when unsupported."
  (when (qq-media-segment-preview-capable-p segment)
    (let* ((native-image-id (qq-media--native-image-media-id segment))
           (native-video-id (qq-media--native-video-media-id segment))
           (native-file-id (qq-media--native-file-media-id segment))
           (type (alist-get 'type segment))
           (preview-type
            (cond
             ((qq-media-imageish-file-segment-p segment) "file-image")
             ((qq-media-videoish-segment-p segment)
              (appkit-media-video-preview-policy-key))
             (t type)))
           (file-key (qq-media--segment-file-key segment))
           (url (qq-media--segment-url segment)))
      (cond
       (native-image-id (qq-media--native-image-key native-image-id))
       (native-video-id
        (qq-media--native-video-thumbnail-key native-video-id))
       (native-file-id (qq-media--native-file-key native-file-id))
       (file-key (format "preview:%s:%s" preview-type file-key))
       ((appkit-media-url-present-p url)
        (format "preview:%s-url:%s" preview-type url))
       (t nil)))))

(defun qq-media-segment-cache-keys (segment)
  "Return logical media cache keys that can affect SEGMENT rendering."
  (let* ((type (alist-get 'type segment))
         (data (alist-get 'data segment))
         (face-id (and (equal type "face") (alist-get 'id data)))
         keys)
    (when face-id
      (push (format "face:%s" face-id) keys))
    (when-let* ((resource-key (qq-media--segment-resource-key segment)))
      (push resource-key keys))
    (when-let* ((download-key (qq-media-segment-download-key segment)))
      (push download-key keys))
    (when-let* ((preview-key (qq-media-segment-preview-key segment)))
      (push preview-key keys))
    (when (and (equal type "gray-tip")
               (equal (alist-get 'kind data) "poke")
               (appkit-media-url-present-p (alist-get 'image-url data)))
      (push (qq-media-poke-image-cache-key (alist-get 'image-url data)) keys))
    (delete-dups (delq nil keys))))

(defun qq-media--video-segment-preview-image (segment key)
  "Return cached preview for video SEGMENT, starting extraction if needed."
  (or (qq-media--cached-image key)
      (unless (or (gethash key qq-media--fetching-cache)
                  (gethash key qq-media--preview-missing-cache))
        (let* ((data (alist-get 'data segment))
               (capabilities (qq-media-segment-capabilities segment))
               (local-file (plist-get capabilities :local-file))
               (source (or (and (appkit-media-file-present-p local-file)
                                local-file)
                           (plist-get capabilities :remote-url)))
               (preview-source
                (seq-find
                 (lambda (candidate)
                   (or (appkit-media-file-present-p candidate)
                       (appkit-media-url-present-p candidate)))
                 (list (alist-get 'thumb data)
                       (alist-get 'thumbnail data)
                       (alist-get 'thumbnail_url data))))
               (source-size (alist-get 'file_size data))
               (duration (alist-get 'duration_secs data))
               (cache-base (qq-media--remote-image-cache-file-base key))
               (cache-file (qq-media--remote-image-cache-existing-file key)))
          (cond
           ((and cache-file (appkit-media-file-present-p cache-file))
            (qq-media--cache-image
             key
             (qq-media--preview-image-from-file cache-file nil)))
           ((not (or (appkit-media-file-present-p source)
                     (appkit-media-url-present-p source)
                     (appkit-media-file-present-p preview-source)
                     (appkit-media-url-present-p preview-source)))
            (puthash key t qq-media--preview-missing-cache)
            nil)
           (t
            (puthash key t qq-media--fetching-cache)
            (make-directory (file-name-directory cache-base) t)
            (condition-case err
                (appkit-media-start-video-preview
                 :key (concat "qq:" key)
                 :source source
                 :preview-source preview-source
                 :source-size source-size
                 :duration duration
                 :cache-base cache-base
                 :callback
                 (lambda (image _target-file)
                   (remhash key qq-media--fetching-cache)
                   (if image
                       (qq-media--cache-image key image)
                     (puthash key t qq-media--preview-missing-cache))
                   (qq-media--note-cache-updated key)))
              (error
               (remhash key qq-media--fetching-cache)
               (puthash key t qq-media--preview-missing-cache)
               (message "qq: video preview failed for %s: %s"
                        key (error-message-string err))))
            nil))))))

(defun qq-media-segment-preview-image (segment)
  "Return inline preview image for SEGMENT, triggering fetch when needed.

Live evidence (emacsclient, forward image
`25BA8E226776B3099D323947E8FE87BE.png`):
- `get_image` with the bare NT file name never invokes success/error
  (left `fetching' stuck for 12s+ with no resource cache).
- The segment `url' downloads successfully through appkit in a few seconds and
  builds a preview image.

So when the wire segment already carries a URL (or local path), seed that
into the preview resource cache *before* `ensure', so we take the URL
download branch instead of blocking forever.  Only fall back to the direct
resource fetcher when there is no usable URL/local path.

Preview failures are soft and do not surface an operation error."
  (let ((key (qq-media-segment-preview-key segment))
        (native-image-id (qq-media--native-image-media-id segment))
        (native-video-id (qq-media--native-video-media-id segment))
        (native-file-id (qq-media--native-file-media-id segment))
        (local (qq-media--segment-existing-path segment))
        (url (qq-media--segment-url segment)))
    (when key
      (cond
       (native-video-id
        (when-let* ((image
                     (qq-media--ensure-native-preview-image
                      key native-video-id 'thumbnail
                      (lambda (done error)
                        (qq-media--fetch-native-video-thumbnail-resource
                         segment key done error))
                      #'qq-media--preview-image-from-file)))
          (or (appkit-media-video-preview-display-image image 'qq)
              image)))
       ((qq-media-videoish-segment-p segment)
        (when-let* ((image (qq-media--video-segment-preview-image segment key)))
          (or (appkit-media-video-preview-display-image image 'qq)
              image)))
       (t
        (when (or local (appkit-media-url-present-p url))
          (qq-media--cache-resource
           key
           (qq-media--resource-from-local+url local url)))
        (if-let* ((media-id (or native-image-id native-file-id)))
            (qq-media--ensure-native-preview-image
             key media-id 'content
             (lambda (done error)
               (if native-image-id
                   (qq-media--fetch-native-image-resource
                    segment key done error)
                 (qq-media--fetch-native-file-resource
                  segment key done error)))
             #'qq-media--preview-image-from-file)
          (qq-media--ensure-resource-image
           key
           (lambda (done error)
             (if (qq-media-segment-preview-capable-p segment)
                 (qq-media--resolve-fileish-segment
                  segment
                  done
                  (lambda (_response _reason)
                    (funcall error nil "preview image not found"))
                  "preview image not found")
               (funcall done nil)))
           nil
           #'qq-media--preview-image-from-file)))))))

(defun qq-media-segment-preview-fetching-p (segment)
  "Return non-nil when preview fetch for SEGMENT is currently active."
  (when-let* ((key (qq-media-segment-preview-key segment)))
    (qq-media--resource-fetching-p key)))

(defun qq-media-message-primary-preview-segment (message)
  "Return MESSAGE's first segment capable of a visual preview."
  (let ((segments (and (listp message) (alist-get 'segments message))))
    (seq-find
     #'qq-media-segment-preview-capable-p
     (if (vectorp segments) (append segments nil) segments))))

(defun qq-media--preview-image-source-file (image)
  "Return local source file carried by IMAGE, or nil."
  (when (and (consp image) (eq (car image) 'image))
    (let ((file (plist-get (cdr image) :file)))
      (and (appkit-media-file-present-p file) file))))

(defun qq-media-segment-one-line-preview-image (segment)
  "Return a one-line image for SEGMENT, scheduling acquisition when cold."
  (when-let* ((key (qq-media-segment-preview-key segment)))
    (let* ((preview-image (qq-media-segment-preview-image segment))
           (resource (qq-media--cached-resource key))
           (file
            (or (qq-media--segment-existing-path segment)
                (and resource
                     (qq-media--resource-image-file key resource))
                (qq-media--remote-image-cache-existing-file key resource)
                (qq-media--preview-image-source-file preview-image))))
      (when file
        (qq-media--one-line-preview-image-from-file file nil)))))

(cl-defun qq-media-message-one-line-preview
    (message text &key label separator label-face)
  "Return an Appkit one-line preview for MESSAGE and visible TEXT."
  (let* ((segment (qq-media-message-primary-preview-segment message))
         (image
          (and segment
               (qq-media-segment-one-line-preview-image segment)))
         (visual
          (and image
               (appkit-media-one-line-image-display-string image "▧"))))
    (appkit-ui-one-line-preview-create
     :text text
     :label label
     :separator separator
     :visual visual
     :visual-columns
     (and visual (max 1 qq-media-one-line-preview-columns))
     :label-face label-face)))

(defun qq-media-message-one-line-preview-keys (message)
  "Return media keys that can change MESSAGE's one-line preview."
  (when-let* ((segment (qq-media-message-primary-preview-segment message))
              (key (qq-media-segment-preview-key segment)))
    (list key)))

(provide 'qq-media)

;;; qq-media.el ends here
