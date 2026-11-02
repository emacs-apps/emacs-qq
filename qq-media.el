;;; qq-media.el --- QQ-specific media helpers for emacs-qq -*- lexical-binding: t; -*-

;; Author: 0WD0 <me@0wd0.com>

;;; Commentary:

;; Resource helpers for QQ avatars, emoji, and structured message media.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url-parse)
(require 'appkit-media)
(require 'appkit-media-effect)
(require 'appkit-media-image)
(require 'appkit-chat-avatar)
(require 'appkit-surface)
(require 'appkit-resource)
(require 'appkit-scroll)
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

;; Effect functions need runtime macros and dynamic bindings at compile time.
(defvar qq-media--consumer nil
  "Exact presentation owner, dynamically bound by non-Surface consumers.")

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
  (let ((account (plist-get input :account-id))
        (qq-media--consumer (plist-get input :consumer))
        operation)
    (cl-labels ((start ()
                  (let ((failed (lambda (_body reason) (funcall reject reason))))
                    (cond
                     ((plist-get input :segment)
                      (qq-media-resolve-segment-resource
                       (plist-get input :segment) resolve failed
                       (eq (plist-get input :kind) 'video)))
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
       :kind 'transport
       :cancel (lambda () (qq-request-cancel operation))))
     ((stringp operation)
      (appkit-cancellation-create
       :kind 'transport
       :cancel (lambda () (qq-server-cancel operation))))
     (t (appkit-cancellation-create
         :kind 'logical)))))

(defun qq-media--effect-failed (_input reason)
  "Map terminal media failure REASON into a Surface message."
  (list 'qq-media 'failed (format "%s" reason)))

(defun qq-media--presentation-effect (input resource)
  "Present granted RESOURCE with independent retained viewer ownership."
  (let* ((canonical (qq-media--appkit-resource resource))
         (kind (appkit-media-resource-kind canonical (plist-get input :kind)))
         (owner-account (plist-get (qq-media--owner-state (plist-get input :consumer)) :account)))
    (appkit-effect-create
     :key 'qq-media-open :input resource
     :start
     (lambda (context source observe resolve reject)
       (qq-runtime-with-account owner-account
         (if (memq kind '(image video))
             (appkit-media-video-presentation-start
              context
              (appkit-media-playback-presentation-create
               (appkit-media-playback-create
                :resource canonical
                :kind kind
                :label "qq"
                :cache-policy 'none)
               :setup-function
               (lambda (session viewer)
                 (let ((state (qq-media--owner-state viewer)))
                   (plist-put state :account (plist-get input :account-id))
                   (plist-put state :invalidate
                              (lambda ()
                                (when (appkit-media-video-session-live-p session)
                                  (appkit-media-video-session-close session))
                                (when (buffer-live-p viewer) (kill-buffer viewer)))))
                 (qq-media--acquire-source source #'ignore #'ignore viewer)
                 (when-let* ((stream-id (alist-get 'stream-id source)))
                   (appkit-cancellation-create
                    :kind 'logical
                    :cancel (lambda () (qq-remote-media-close-stream stream-id))))))
              observe resolve reject)
           (condition-case condition
               (let ((viewer (find-file-noselect (alist-get 'file source))))
                 (with-current-buffer viewer
                   (when (alist-get 'resource-id source)
                     (setq buffer-read-only t
                           buffer-auto-save-file-name nil
                           backup-inhibited t)
                     (let ((state (qq-media--owner-state viewer)))
                       (plist-put state :account (plist-get input :account-id))
                       (plist-put state :viewer-source (copy-tree source))))
                   (qq-media--acquire-source source #'ignore #'ignore viewer))
                 (pop-to-buffer viewer)
                 (funcall resolve viewer)
                 nil)
             ((error quit) (funcall reject (error-message-string condition)) nil)))))
     :success (lambda (_input _result) '(qq-media closed))
     :failure #'qq-media--effect-failed
     :cancellation-requirement 'logical)))


(defun qq-media--acquire-effect (input resource)
  "Acquire RESOURCE through Gateway under INPUT's exact initiating owner."
  (let ((owner-account (plist-get (qq-media--owner-state (plist-get input :consumer)) :account)))
    (appkit-effect-create
     :key 'qq-media-open :input resource
     :start
     (lambda (_context source _observe resolve reject)
       (qq-runtime-with-account owner-account
         (qq-media--acquire-source
          source resolve (lambda (_body reason) (funcall reject reason))
          (plist-get input :consumer)))
       (appkit-cancellation-create :kind 'logical))
     :success (lambda (_source ready) (list 'qq-media 'acquired input ready))
     :failure #'qq-media--effect-failed
     :cancellation-requirement 'logical)))

(defun qq-media-update (_context model message)
  "Commit media MESSAGE and emit acquisition or presentation Effects."
  (let ((next (copy-sequence model)) effect)
    (pcase message
      (`(qq-media open ,input)
       (setq next (plist-put next :media (list :status 'loading :input input))
             effect
             (appkit-effect-create
              :key 'qq-media-open
              :input input
              :start #'qq-media--resolve-open-start
              :success (lambda (request resource)
                         (list 'qq-media 'resolved request resource))
              :failure #'qq-media--effect-failed
              :cancellation-requirement 'logical)))
      (`(qq-media resolved ,input ,resource)
       (setq next (plist-put next :media
                             (list :status 'acquiring :resource resource))
             effect (qq-media--acquire-effect input resource)))
      (`(qq-media acquired ,input ,resource)
       (when-let* ((key (plist-get input :cache-key))
                   ((not (alist-get 'stream-id resource))))
         (qq-media--cache-resource key resource))
       (setq next (plist-put next :media (list :status 'presenting :resource resource))
             effect (qq-media--presentation-effect input resource)))
      (`(qq-media failed ,reason)
       (setq next (plist-put next :media (list :status 'failed :error reason))))
      (`(qq-media closed)
       (setq next (plist-put next :media '(:status closed)))))
    (appkit-next
     :model next
     :render appkit-render-none
     :commands (and effect (list (appkit-command-start-effect effect))))))

(defun qq-media--send-open (owner input)
  "Send owned media INPUT to the exact initiating Surface OWNER."
  (let* ((surface (qq-media--initiating-surface owner))
         (app (appkit-surface-app surface))
         (account (and (eq (appkit-app-type-name (appkit-app-type app)) 'qq-account)
                       (appkit-app-identity app))))
    (qq-runtime-with-account
        (if account (qq-state-partition-store (appkit-app-model app)) (qq-runtime-state))
      (qq-media--owner-state surface))
    (appkit-surface-send
     surface
     (list 'qq-media 'open
           (plist-put (plist-put input :account-id account) :consumer surface)))))

(defvar qq-media-animated-face-image-height)

(defvar qq-media-cache-update-hook nil
  "Hook run after media resource/image cache updates.")

(defvar qq-media--cache-update-timer nil
  "Timer delivering the current batch of cache notifications.")

(defvar qq-media--pending-cache-updates (make-hash-table :test #'equal)
  "Logical media keys awaiting notification; nil denotes a global update.")

(defvar qq-media--resource-cache (make-hash-table :test #'equal)
  "Simple in-memory resource cache keyed by logical resource identity.")

(defvar qq-media--image-cache (make-hash-table :test #'equal)
  "In-memory image object cache keyed by logical resource identity.")

(defvar qq-media--one-line-image-cache (make-hash-table :test #'equal)
  "Compact preview descriptors and their source/geometry identity by file.")

(defvar qq-media--preview-missing-cache (make-hash-table :test #'equal)
  "Failed preview keys: t for acquisition failure, or the undecodable local file.")

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

(defvar qq-media-system-emoji-catalog-hook nil
  "Hook called with ACCOUNT and ERROR when its system catalog settles.
ERROR is nil on success, otherwise a human-readable failure reason.")

(defvar qq-media--consumers (make-hash-table :test #'eq)
  "Live presentation owners and their independent Gateway access references.")
(defvar qq-media--generation 0)
(defvar-local qq-media--buffer-consumer nil)
(defvar qq-media--invalidated-keys (make-hash-table :test #'equal))

(defun qq-media--current-consumer ()
  "Return the exact retained presentation owner of this call."
  (or qq-media--consumer
      (and (appkit-surface-live-p (appkit-current-surface))
           (appkit-current-surface))
      (and (boundp 'qq-runtime--surface-owner)
           (appkit-surface-live-p qq-runtime--surface-owner)
           qq-runtime--surface-owner)
      (current-buffer)))

(defun qq-media--owner-state (&optional owner)
  "Return live bookkeeping for OWNER, installing its lifetime cleanup once."
  (let* ((owner (or owner (qq-media--current-consumer)))
         (state (gethash owner qq-media--consumers)))
    (or state
        (let* ((account (qq-runtime-current-account-id))
               (state (list :account account
                            :refs (make-hash-table :test #'equal)
                            :aliases (make-hash-table :test #'equal)
                            :files (make-hash-table :test #'equal)
                            :operations (make-hash-table :test #'equal))))
          (puthash owner state qq-media--consumers)
          (cond
           ((appkit-surface-p owner)
            (appkit-register-handle
             owner 'function (lambda () (qq-media-release-consumer owner))))
           ((bufferp owner)
            (with-current-buffer owner
              (setq-local qq-media--buffer-consumer owner)
              (add-hook 'kill-buffer-hook #'qq-media--release-buffer-consumer nil t)
              (add-hook 'change-major-mode-hook #'qq-media--release-buffer-consumer nil t))))
          (let ((buffer (if (appkit-surface-p owner)
                            (appkit-surface-buffer owner)
                          (and (bufferp owner) owner))))
            (when (buffer-live-p buffer)
              (let ((validate
                     (lambda ()
                       (when (eq state (gethash owner qq-media--consumers))
                         (maphash (lambda (_id ref) (qq-resource-access-live-p ref))
                                  (plist-get state :refs))))))
                (plist-put state :validation (cons buffer validate))
                (with-current-buffer buffer
                  (add-hook 'pre-command-hook validate nil t)))))
          state))))

(defun qq-media--release-buffer-consumer ()
  "Release retained media when its ordinary buffer closes or changes mode."
  (when qq-media--buffer-consumer
    (qq-media-release-consumer qq-media--buffer-consumer)
    (setq qq-media--buffer-consumer nil)))

(defun qq-media-release-consumer (owner)
  "Release OWNER without cancelling another consumer's acquisition."
  (when-let* ((state (gethash owner qq-media--consumers)))
    (remhash owner qq-media--consumers)
    (when-let* ((validation (plist-get state :validation))
                ((buffer-live-p (car validation))))
      (with-current-buffer (car validation)
        (remove-hook 'pre-command-hook (cdr validation) t)))
    (maphash
     (lambda (_key token)
       (when (qq-remote-media-operation-p (car token))
         (qq-remote-media-cancel-operation (car token))))
     (plist-get state :operations))
    (maphash (lambda (_id ref) (qq-resource-access-release ref))
             (plist-get state :refs))
    (dolist (table (list qq-media--fetching-cache
                         qq-media--native-preview-attempts))
      (let (keys)
        (maphash (lambda (key _value)
                   (when (eq (nth 1 key) owner) (push key keys)))
                 table)
        (dolist (key keys) (remhash key table))))
    (qq-media--prune-presentation-cache)))

(defun qq-media--scoped-key (key)
  "Keep logical KEY aliases separate across account contexts."
  (cons (qq-runtime-current-account-id) key))

(defun qq-media--retain-cache-key (key)
  "Keep logical KEY's presentation while this consumer is alive.
Register failures too, so closing a surface retires its negative cache."
  (let* ((aliases (plist-get (qq-media--owner-state) :aliases))
         (scoped (qq-media--scoped-key key)))
    (unless (gethash scoped aliases)
      (puthash scoped nil aliases))))

(defun qq-media--prune-presentation-cache ()
  "Forget presentation descriptors with no remaining live consumer.
Gateway files and resources have separate lifetimes; this only releases
Emacs descriptors and decoded images after their final presentation closes."
  (let ((keys (make-hash-table :test #'equal))
        (files (make-hash-table :test #'equal))
        (images (make-hash-table :test #'eq)))
    (maphash
     (lambda (_owner state)
       (maphash (lambda (key _id) (puthash key t keys))
                (plist-get state :aliases))
       (when-let* ((owned-files (plist-get state :files)))
         (maphash (lambda (file _present) (puthash file t files)) owned-files)))
     qq-media--consumers)
    (maphash
     (lambda (key resource)
       (when (gethash key keys)
         (when-let* ((file (alist-get 'file resource)))
           (puthash file t files))))
     qq-media--resource-cache)
    (maphash (lambda (key image)
               (when (gethash key keys) (puthash image t images)))
             qq-media--image-cache)
    (maphash (lambda (file entry)
               (when (gethash file files) (puthash (cdr entry) t images)))
             qq-media--one-line-image-cache)
    (cl-labels ((flush (image)
                  (when (and (consp image) (eq (car image) 'image)
                             (not (gethash image images))
                             (not (gethash (plist-get (cdr image) :file) files)))
                    (ignore-errors (image-flush image t)))))
      (maphash
       (lambda (key image)
         (unless (gethash key keys)
           (flush image)
           (remhash key qq-media--image-cache)))
       qq-media--image-cache)
      (maphash
       (lambda (file entry)
         (unless (gethash file files)
           (flush (cdr entry))
           (remhash file qq-media--one-line-image-cache)))
       qq-media--one-line-image-cache))
    (dolist (table (list qq-media--resource-cache
                         qq-media--preview-missing-cache
                         qq-media--invalidated-keys))
      (maphash (lambda (key _value)
                 (unless (gethash key keys) (remhash key table)))
               table))))


(defun qq-media--fetch-key (key)
  "Keep acquisition attempts local to each independent retained consumer."
  (list (qq-runtime-current-account-id) (qq-media--current-consumer) key))

(defun qq-media--owned-callback (owner generation function)
  "Capture OWNER's account and reject callbacks from another GENERATION."
  (let* ((state (qq-media--owner-state owner))
         (owner-account (plist-get state :account)))
    (lambda (&rest args)
      (when (and (= generation qq-media--generation)
                 (eq state (gethash owner qq-media--consumers)))
        (let ((qq-media--consumer owner))
          (qq-runtime-with-account owner-account
            (apply (or function #'ignore) args)))))))

(defun qq-media--adopt-access (owner resource-id access ref callback)
  "Attach REF to exact OWNER and pass its granted RESOURCE-ID to CALLBACK."
  (if-let* ((state (gethash owner qq-media--consumers)))
      (let* ((refs (plist-get state :refs))
             (old (gethash resource-id refs))
             (snapshot (qq-resource resource-id)))
        (when (and old (not (eq old ref)))
          (qq-resource-access-release old))
        (puthash resource-id ref refs)
        (funcall callback
                 `((resource-id . ,resource-id)
                   (file . ,(alist-get 'path access))
                   (name . ,(alist-get 'suggested_name snapshot))
                   (mime-type . ,(alist-get 'media_type snapshot)))))
    (qq-resource-access-release ref)))

(defun qq-media--acquire-resource (resource-id callback errback &optional owner)
  "Retain RESOURCE-ID for OWNER; callbacks keep exact account and generation."
  (let* ((owner (or owner (qq-media--current-consumer)))
         (state (qq-media--owner-state owner))
         (refs (plist-get state :refs))
         (generation qq-media--generation)
         (ref (gethash resource-id refs))
         (access (and ref (qq-resource-access-grant ref)))
         (done (qq-media--owned-callback owner generation callback))
         (fail (qq-media--owned-callback owner generation errback)))
    (if access
        (qq-media--adopt-access owner resource-id access ref done)
      (let ((pending
             (qq-resource-acquire-access
              resource-id
              (lambda (access new-ref)
                (if (and (= generation qq-media--generation)
                         (eq state (gethash owner qq-media--consumers)))
                    (let ((old (gethash resource-id refs)))
                      (if-let* ((old-grant (and old (qq-resource-access-grant old))))
                          (progn
                            (unless (eq old new-ref)
                              (qq-resource-access-release new-ref))
                            (qq-media--adopt-access
                             owner resource-id old-grant old done))
                        (qq-media--adopt-access
                         owner resource-id access new-ref done)))
                  (qq-resource-access-release new-ref)))
              fail)))
        (when (and (eq state (gethash owner qq-media--consumers))
                   (qq-resource-access-live-p pending)
                   (not (qq-resource-access-live-p (gethash resource-id refs))))
          (puthash resource-id pending refs))
        pending))))

(defun qq-media--merge-source (source metadata)
  "Merge SOURCE over METADATA, keeping one owned entry per field."
  (let (merged)
    (dolist (resource (list source metadata))
      (dolist (entry resource)
        (unless (assq (car entry) merged)
          (push (copy-tree entry) merged))))
    (nreverse merged)))

(defun qq-media--acquire-source (resource callback errback &optional owner)
  "Resolve RESOURCE through Gateway, never Appkit's automatic disk cache."
  (let* ((owner (or owner (qq-media--current-consumer)))
         (_state (qq-media--owner-state owner))
         (generation qq-media--generation)
         (done (qq-media--owned-callback
                owner generation
                (lambda (ready)
                  (funcall callback (qq-media--merge-source ready resource)))))
         (fail (qq-media--owned-callback owner generation errback))
         (id (alist-get 'resource-id resource))
         (file (alist-get 'file resource))
         (url (alist-get 'url resource)))
    (cl-labels ((acquire (snapshot)
                 (qq-resource-await-ready
                  (alist-get 'resource_id snapshot)
                  (qq-media--owned-callback
                   owner generation
                   (lambda (ready)
                     (qq-media--acquire-resource
                      (alist-get 'resource_id ready) done fail owner)))
                  fail)))
      (cond
       (id (qq-media--acquire-resource id done fail owner))
       ((alist-get 'stream-id resource) (funcall done resource))
       ((appkit-media-url-present-p url)
        (qq-resource-acquire-url
         (if (string-prefix-p "http://" url t)
             (concat "https://" (substring url 7)) url)
         (alist-get 'name resource)
         (qq-media--owned-callback owner generation #'acquire) fail))
       ((appkit-media-file-present-p file) (funcall done resource))
       (t (funcall fail nil "media source is unavailable"))))))

(defun qq-media--forget-file-displays (buffer paths)
  "Remove stale file image specs from BUFFER before its next owned repaint."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (boundp 'appkit-media--inline-hosts)
        (dolist (host (copy-sequence appkit-media--inline-hosts))
          (when (member (alist-get 'file (appkit-media-inline-host-resource host)) paths)
            (appkit-media--inline-close host))))
      (let ((inhibit-read-only t) (inhibit-modification-hooks t))
        (with-silent-modifications
          (save-restriction
            (widen)
            (let ((position (point-min)))
              (while (< position (point-max))
                (let* ((end (next-single-property-change position 'display nil (point-max)))
                       (display (get-text-property position 'display)))
                  (when (and (consp display) (eq (car display) 'image)
                             (member (plist-get (cdr display) :file) paths))
                    (remove-text-properties position end '(display nil)))
                  (setq position end))))))))))

(defun qq-media--refresh-file-viewer (owner state)
  "Reacquire the original for independent file viewer OWNER after reconnect."
  (when-let* ((source (plist-get state :viewer-source))
              ((buffer-live-p owner))
              (id (alist-get 'resource-id source))
              ((not (qq-resource-access-live-p (gethash id (plist-get state :refs)))))
              ((not (plist-get state :viewer-pending)))
              ((qq-server-ready-p)))
    (plist-put state :viewer-pending t)
    (qq-runtime-with-account (plist-get state :account)
      (qq-media--acquire-resource
       id
       (lambda (ready)
         (plist-put state :viewer-pending nil)
         (with-current-buffer owner
           (let ((inhibit-read-only t))
             (set-visited-file-name (alist-get 'file ready) t)
             (revert-buffer t t t)
             (setq buffer-read-only t
                   buffer-auto-save-file-name nil
                   backup-inhibited t))))
       (lambda (_body _reason) (plist-put state :viewer-pending nil))
       owner))))

(defun qq-media--resources-changed (_reason resource-id)
  "Refresh retained independent viewers when original RESOURCE-ID is ready."
  (when (and resource-id
             (equal (alist-get 'phase (qq-resource resource-id)) "ready"))
    (maphash
     (lambda (owner state)
       (when (equal resource-id
                    (alist-get 'resource-id (plist-get state :viewer-source)))
         (qq-media--refresh-file-viewer owner state)))
     qq-media--consumers)))

(defun qq-media--invalidate-access (resource-id reason)
  "Invalidate aliases, file specs and playback for an expired capability."
  (let (paths affected)
    (maphash
     (lambda (scoped resource)
       (when (and (alist-get 'resource-id resource)
                  (or (null resource-id)
                      (equal resource-id (alist-get 'resource-id resource))))
         (when-let* ((path (alist-get 'file resource))) (push path paths))
         (setf (alist-get 'file resource) nil)
         (remhash scoped qq-media--image-cache)
         (unless (eq reason 'open-failed)
           (remhash scoped qq-media--preview-missing-cache))
         (puthash scoped t qq-media--invalidated-keys)
         (qq-media--note-cache-updated (cdr scoped))))
     qq-media--resource-cache)
    (maphash
     (lambda (owner state)
       (when (or (null resource-id)
                 (gethash resource-id (plist-get state :refs)))
         (push (cons owner state) affected)))
     qq-media--consumers)
    (dolist (entry affected)
      (let* ((owner (car entry))
             (state (cdr entry))
             (buffer (if (appkit-surface-p owner)
                         (appkit-surface-buffer owner)
                       (and (bufferp owner) owner))))
        (qq-media--forget-file-displays buffer paths)
        (when-let* ((invalidate (plist-get state :invalidate))) (funcall invalidate))
        (when (plist-get state :viewer-source)
          (plist-put state :viewer-pending nil)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert "QQ media access expired or disconnected; waiting for Gateway.\n")
                (set-buffer-modified-p nil))))
          (unless (eq reason 'open-failed)
            (qq-media--refresh-file-viewer owner state)))))
    (let (records)
      (maphash
       (lambda (id entry)
         (when (and (plist-get entry :lease)
                    (not (qq-resource-access-live-p (plist-get entry :lease))))
           (push id records)))
       qq-media--native-record-playbacks)
      (dolist (id records) (qq-media--dispose-native-record-entry id 'stopped))))
  (clrhash qq-media--one-line-image-cache)
  (clrhash qq-media--native-preview-attempts))

(defun qq-media--transport-state-changed (&rest _args)
  "Invalidate capabilities at every transport boundary, including reconnect."
  (unless (qq-server-ready-p)
    (dolist (table (list qq-media--resource-cache qq-media--image-cache
                         qq-media--preview-missing-cache))
      (maphash (lambda (key _value) (puthash key t qq-media--invalidated-keys)) table))
    (maphash
     (lambda (key _value)
       (puthash (cons (car key) (nth 2 key)) t qq-media--invalidated-keys))
     qq-media--fetching-cache)
    (cl-incf qq-media--generation)
    (qq-media--stop-all-native-record-playback)
    (let (cancels)
      (maphash (lambda (_owner state)
                 (when-let* ((cancel (plist-get state :cancel))) (push cancel cancels)))
               qq-media--consumers)
      (mapc #'funcall cancels))
    (qq-media--invalidate-access nil 'transport)
    (clrhash qq-media--resource-cache)
    (clrhash qq-media--image-cache)
    (clrhash qq-media--preview-missing-cache)
    (clrhash qq-media--fetching-cache)
    (maphash (lambda (_owner state) (clrhash (plist-get state :refs)))
             qq-media--consumers)))

(defun qq-media--gateway-ready (_instance-id)
  "Repaint retained consumers after either same-instance or new-instance ready."
  (clrhash qq-media--preview-missing-cache)
  (clrhash qq-media--native-preview-attempts)
  (maphash (lambda (scoped _value)
             (qq-media--note-cache-updated (cdr scoped)))
           qq-media--invalidated-keys)
  (clrhash qq-media--invalidated-keys)
  (maphash #'qq-media--refresh-file-viewer qq-media--consumers))

(add-hook 'qq-resource-access-invalidated-hook #'qq-media--invalidate-access)
(add-hook 'qq-server-state-hook #'qq-media--transport-state-changed)
(add-hook 'qq-account-registry-ready-hook #'qq-media--gateway-ready t)
(add-hook 'qq-resource-changed-hook #'qq-media--resources-changed)

(defun qq-media--default-error (_response reason)
  "Display a media operation failure REASON."
  (message "qq: %s" (or reason "media operation failed")))

(defun qq-media--json-truthy-p (value)
  "Return non-nil when JSON VALUE is a true-ish flag."
  (and value
       (not (memq value '(:false :null)))
       (not (member value '(0 "0" "false")))))

(defun qq-media-clear-cache ()
  "Drop in-memory presentation ownership; never delete Gateway or user files."
  (interactive)
  (cl-incf qq-media--generation)
  (qq-media--stop-all-native-record-playback)
  (maphash
   (lambda (_key entry)
     (when-let* ((cancel (plist-get entry :cancel))) (funcall cancel)))
   qq-media--download-state-table)
  (let (owners)
    (maphash (lambda (owner _state) (push owner owners)) qq-media--consumers)
    (dolist (owner owners)
      (when-let* ((cancel (plist-get (gethash owner qq-media--consumers) :cancel)))
        (funcall cancel))
      (qq-media-release-consumer owner)))
  (dolist (table (list qq-media--resource-cache qq-media--image-cache
                       qq-media--one-line-image-cache qq-media--preview-missing-cache
                       qq-media--native-preview-attempts qq-media--fetching-cache
                       qq-media--download-state-table qq-media--system-emoji-tables
                       qq-media--system-emoji-requests
                       qq-media--invalidated-keys))
    (clrhash table))
  (appkit-media-clear-video-decoration-cache 'qq)
  (when (timerp qq-media--cache-update-timer)
    (cancel-timer qq-media--cache-update-timer))
  (setq qq-media--cache-update-timer nil)
  (clrhash qq-media--pending-cache-updates))

(defun qq-media--appkit-resource (resource)
  "Adapt QQ RESOURCE to the strict appkit media resource contract.
Upgrade legacy HTTP locators before acquisition, without changing their
source identity or allowing a plaintext fallback.  Gateway loopback streams
keep their served URL."
  (let ((url (alist-get 'url resource)))
    (appkit-media-resource-create
     :file (alist-get 'file resource)
     :url (if (and (stringp url) (string-prefix-p "http://" url t)
                   (not (alist-get 'stream-id resource)))
              (concat "https://" (substring url 7))
            url)
     :name (alist-get 'name resource)
     :mime-type (alist-get 'mime-type resource))))

(defun qq-media--cached-resource (key)
  "Return KEY's account-scoped source only with a live access reference."
  (let* ((resource (copy-tree (gethash (qq-media--scoped-key key) qq-media--resource-cache)))
         (id (alist-get 'resource-id resource)))
    (when resource
      (qq-media--retain-cache-key key)
      (if (not id)
          resource
        (let* ((state (qq-media--owner-state))
               (ref (gethash id (plist-get state :refs)))
               (access (and ref (qq-resource-access-grant ref))))
          (if access
              (progn
                (setf (alist-get 'file resource) (alist-get 'path access))
                (qq-media--claim-alias key resource)
                resource)
            (setf (alist-get 'file resource) nil)
            (unless (or (qq-resource-access-live-p ref)
                        (gethash (qq-media--scoped-key key) qq-media--preview-missing-cache)
                        (not (qq-server-ready-p)))
              (qq-media--acquire-resource
               id
               (lambda (ready)
                 (qq-media--cache-resource
                  key (qq-media--merge-source ready resource))
                 (qq-media--note-cache-updated key))
               (lambda (_body _reason)
                 (puthash (qq-media--scoped-key key) t qq-media--preview-missing-cache)
                 (qq-media--note-cache-updated key))))
            nil))))))

(defun qq-media--cache-resource (key resource)
  "Store account-scoped RESOURCE, invalidating specs when its grant path changes."
  (let* ((scoped (qq-media--scoped-key key))
         (old (gethash scoped qq-media--resource-cache)))
    (unless (equal (alist-get 'file old) (alist-get 'file resource))
      (remhash scoped qq-media--image-cache))
    (puthash scoped (copy-tree resource) qq-media--resource-cache))
  (qq-media--claim-alias key resource)
  resource)

(defun qq-media--claim-alias (key resource)
  "Replace this owner's logical KEY reference without pinning obsolete sources."
  (let* ((state (qq-media--owner-state))
         (aliases (plist-get state :aliases))
         (id (alist-get 'resource-id resource))
         (scoped (qq-media--scoped-key key))
         (old (gethash scoped aliases)))
    (puthash scoped id aliases)
    (when (and old (not (equal old id)))
      (let ((used nil))
        (maphash (lambda (_key value) (when (equal value old) (setq used t))) aliases)
        (unless used
          (qq-resource-access-release (gethash old (plist-get state :refs)))
          (remhash old (plist-get state :refs)))))))

(defun qq-media--cached-image (key)
  "Return KEY's image only while its source authority remains live."
  (when (qq-media--cached-resource key)
    (gethash (qq-media--scoped-key key) qq-media--image-cache)))

(defun qq-media--cache-image (key image)
  "Store IMAGE object under KEY and return IMAGE."
  (when image
    (qq-media--retain-cache-key key)
    (puthash (qq-media--scoped-key key) image qq-media--image-cache)
    (remhash (qq-media--scoped-key key) qq-media--preview-missing-cache))
  image)

(defun qq-media--flush-cache-updates ()
  "Deliver pending cache updates outside transfer callbacks.
Detach the batch first so notifications raised by a listener are delivered
on the next turn rather than recursively mutating the batch being visited."
  (let ((pending qq-media--pending-cache-updates))
    (setq qq-media--cache-update-timer nil
          qq-media--pending-cache-updates (make-hash-table :test #'equal))
    (if (gethash nil pending)
        (run-hook-with-args 'qq-media-cache-update-hook nil)
      (maphash (lambda (key _)
                 (run-hook-with-args 'qq-media-cache-update-hook key))
               pending))))

(defun qq-media--note-cache-updated (&optional media-key)
  "Queue one UI notification for logical MEDIA-KEY.
Duplicate keys share a command-loop timer.  Nil requests an explicit global
refresh and supersedes individual keys in the pending batch."
  (if media-key
      (unless (gethash nil qq-media--pending-cache-updates)
        (puthash media-key t qq-media--pending-cache-updates))
    (clrhash qq-media--pending-cache-updates)
    (puthash nil t qq-media--pending-cache-updates))
  (unless qq-media--cache-update-timer
    (setq qq-media--cache-update-timer
          (run-at-time 0 nil #'qq-media--flush-cache-updates))))

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


(defun qq-media--dispose-native-record-entry
    (media-id &optional status error-text)
  "Stop and clean native record MEDIA-ID, recording STATUS and ERROR-TEXT."
  (when-let* ((entry (gethash media-id qq-media--native-record-playbacks)))
    (let ((operation (plist-get entry :operation))
          (session (plist-get entry :session))
          (lease (plist-get entry :lease))
          (owner-handle (plist-get entry :owner-handle)))
      ;; Revoke exact callback ownership before any cancellation can reenter.
      (setq entry (plist-put entry :operation nil))
      (setq entry (plist-put entry :session nil))
      (setq entry (plist-put entry :lease nil))
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
      (qq-resource-access-release lease)
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
           (lease (and terminal-p (plist-get entry :lease))))
      (setq entry (plist-put entry :status status))
      (setq entry
            (plist-put entry :played-seconds
                       (appkit-media-player-played-seconds session)))
      (setq entry
            (plist-put entry :error
                       (and (eq status 'failed)
                            "record player exited abnormally")))
      (when terminal-p
        (setq entry (plist-put entry :lease nil))
        (when (equal qq-media--native-record-current-id media-id)
          (setq qq-media--native-record-current-id nil)))
      (puthash media-id entry qq-media--native-record-playbacks)
      (when lease
        (qq-resource-access-release lease))
      (qq-media--notify-native-record-state media-id))))

(defun qq-media--start-native-record-player (media-id result)
  "Start Appkit playback for prepared native record MEDIA-ID using RESULT."
  (let* ((access (alist-get 'access result))
         (lease (alist-get 'lease result))
         (path (alist-get 'path access))
         (entry (gethash media-id qq-media--native-record-playbacks)))
    (if (not (and entry
                  (qq-media-native-record-playback-available-p)
                  (stringp path)
                  (file-regular-p path)))
        (progn
          (qq-resource-access-release lease)
          (qq-media--dispose-native-record-entry
           media-id 'failed "record playback input or player is unavailable")
          (message "qq: Native record playback input or player is unavailable")
          nil)
      (setq entry (plist-put entry :lease lease))
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
                  (qq-resource-access-release lease)
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
        (qq-resource-access-release (alist-get 'lease result))
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
       (if (qq-resource-access-grant (plist-get entry :lease))
           (condition-case error-data
               (appkit-media-player-toggle session)
             (error
              (qq-media--dispose-native-record-entry
               media-id 'failed (error-message-string error-data))))
         (qq-media--dispose-native-record-entry media-id 'stopped)
         (qq-media-play-native-record segment :owner owner)))
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
                     :lease nil
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
                      #'qq-media--native-record-prepare-failed media-id)
                     (alist-get 'media-source segment))))
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
  "Notify both original and distinct presentation aliases of native MEDIA-ID."
  (if (not media-id)
      (progn
        (clrhash qq-media--native-preview-attempts)
        (qq-media--note-cache-updated nil))
    (dolist (media-id (qq-remote-media-presentation-ids media-id))
    (unless (qq-remote-media media-id)
      (let (keys)
        (maphash (lambda (key _value)
                   (when (equal (car (nth 2 key)) media-id) (push key keys)))
                 qq-media--native-preview-attempts)
        (dolist (key keys) (remhash key qq-media--native-preview-attempts))))
    (dolist (key (list (qq-media--native-image-key media-id)
                       (qq-media--native-video-key media-id)
                       (qq-media--native-video-thumbnail-key media-id)
                       (qq-media--native-file-key media-id)
                       (qq-media--native-record-key media-id)))
      (qq-media--note-cache-updated key)
      (qq-media--note-cache-updated (concat "preview:" key))))))

(add-hook 'qq-remote-media-changed-hook
          #'qq-media--native-remote-media-changed)

(defun qq-media--image-from-file (file height)
  "Create an Emacs image object from FILE at pixel HEIGHT, or nil."
  (when (appkit-media-file-present-p file)
    (appkit-media-image-with-resource
     (if (eq (video-source-format file) 'lottie)
         (appkit-media-lottie-poster file height)
       (condition-case _
           (create-image file nil nil :height (max 1 height) :ascent 'center)
         (error nil)))
     (appkit-media-resource-create :file file)
     :interaction nil)))

(defun qq-media--avatar-image-from-file (file pixel-size)
  "Create a circular avatar from FILE at PIXEL-SIZE.

Retain the ordinary image decoder as a capability fallback when the current
Emacs display cannot render SVG clipping."
  (or (appkit-media-circular-image-from-file file pixel-size)
      (appkit-media-image-with-resource
       (qq-media--image-from-file file pixel-size) nil
       :animated nil :autoplay nil :interaction nil)))

(defun qq-media--preview-image-from-file (file spec)
  "Create a preview image object from FILE using appkit media helpers.

SPEC may be a numeric maximum height for compact decorative images."
  (when (appkit-media-file-present-p file)
    (appkit-media-preview-image-from-file
     file
     qq-media-preview-image-max-width
     (if (numberp spec) spec qq-media-preview-image-height))))

(defun qq-media--one-line-preview-image-from-file (file)
  "Return a cached single-row thumbnail from FILE for compact surfaces."
  (when (appkit-media-file-present-p file)
    (let* ((state (qq-media--owner-state))
           (files (or (plist-get state :files)
                      (let ((table (make-hash-table :test #'equal)))
                        (plist-put state :files table)
                        table))))
      (puthash file t files))
    (let* ((width (* (max 1 qq-media-one-line-preview-columns)
                     (max 1 (frame-char-width))))
           (attributes (file-attributes file))
           (identity
            (list (file-attribute-modification-time attributes)
                  (file-attribute-size attributes)
                  width (appkit-media--base-char-pixel-height)
                  (image-type-available-p 'canvas)))
           (cached (gethash file qq-media--one-line-image-cache)))
      (if (equal identity (car cached))
          (cdr cached)
        (let ((image (appkit-media-one-line-preview-image-from-file file width)))
          (puthash file (cons identity image) qq-media--one-line-image-cache)
          image)))))

(defun qq-media--resource-image-file (_key resource)
  "Return RESOURCE's explicitly granted or user-owned local source."
  (let ((file (alist-get 'file resource)))
    (and (appkit-media-file-present-p file) file)))

(defun qq-media--finish-resource-image-fetch (key &optional file image resource)
  "Finalize image fetch for KEY using FILE, IMAGE, and RESOURCE.

Retain a failed attempt before notifying UI, so a fallback redraw cannot
immediately start the same failed request again.  Still notify on failure
to replace any loading presentation with its textual fallback."
  (when (and resource file)
    (setf (alist-get 'file resource nil nil #'eq) file)
    (qq-media--cache-resource key resource))
  (if image
      (qq-media--cache-image key image)
    (puthash (qq-media--scoped-key key) (or file t) qq-media--preview-missing-cache))
  (remhash (qq-media--fetch-key key) qq-media--fetching-cache)
  (qq-media--note-cache-updated key))

(defun qq-media--start-resource-image-download (key resource spec builder)
  "Acquire RESOURCE through Gateway before selecting its presentation."
  (qq-media--acquire-source
   resource
   (lambda (ready)
     (qq-media--accept-resource-image-fetch
      key (qq-media--merge-source ready resource) spec builder t))
   (lambda (_body _reason)
     (qq-media--finish-resource-image-fetch key nil nil resource))))

(defun qq-media--accept-resource-image-fetch (key resource spec builder &optional acquired)
  "Render RESOURCE once acquired, or acquire it before rendering.
An unusable granted path settles as failure, never another acquisition."
  (when (gethash (qq-media--fetch-key key) qq-media--fetching-cache)
    (if-let* ((file (qq-media--resource-image-file key resource)))
        (qq-media--finish-resource-image-fetch
         key file (funcall builder file spec) resource)
      (if acquired
          (qq-media--finish-resource-image-fetch key nil nil resource)
        (qq-media--start-resource-image-download key resource spec builder)))))

(defun qq-media--reject-resource-image-fetch (key)
  "Settle an active image fetch for KEY without a usable resource."
  (when (gethash (qq-media--fetch-key key) qq-media--fetching-cache)
    (qq-media--finish-resource-image-fetch key nil nil nil)))

(defun qq-media--start-resource-image-fetch (key fetcher spec builder)
  "Start FETCHER with exact captured owner/account and transport generation."
  (let* ((owner (qq-media--current-consumer))
         (generation qq-media--generation)
         (done (qq-media--owned-callback
                owner generation
                (lambda (resource)
                  (qq-media--accept-resource-image-fetch key resource spec builder))))
         (fail (qq-media--owned-callback
                owner generation
                (lambda (_body _reason)
                  (qq-media--reject-resource-image-fetch key)))))
    (condition-case error-data
        (funcall fetcher done fail)
      (error (funcall fail nil (error-message-string error-data))))))

(defun qq-media--ensure-resource-image (key fetcher spec &optional image-builder)
  "Return KEY's image or acquire a Gateway source for its retained owner."
  (qq-media--retain-cache-key key)
  (let ((builder (or image-builder #'qq-media--image-from-file)))
    (or (qq-media--cached-image key)
        (let* ((resource (qq-media--cached-resource key))
               (file (qq-media--resource-image-file key resource)))
          (cond
           ((gethash (qq-media--fetch-key key) qq-media--fetching-cache) nil)
           (file
            (unless (equal file (gethash (qq-media--scoped-key key) qq-media--preview-missing-cache))
              (let ((image (funcall builder file spec)))
                (if image (qq-media--cache-image key image)
                  (puthash (qq-media--scoped-key key) file qq-media--preview-missing-cache)
                  nil))))
           ((gethash (qq-media--scoped-key key) qq-media--preview-missing-cache) nil)
           ((not (qq-server-ready-p)) nil)
           (t
            (puthash (qq-media--fetch-key key) t qq-media--fetching-cache)
            (if resource
                (qq-media--start-resource-image-download key resource spec builder)
              (qq-media--start-resource-image-fetch key fetcher spec builder))
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
granted file into an Emacs image.  A terminal or locally failed automatic
attempt is not reissued by later redisplay.  User actions continue to call the
ordinary segment operations directly and can therefore retry."
  (or (qq-media--cached-image key)
      (progn
        (let ((attempt-key (cons media-id part)))
          (cond
           ((qq-media--cached-resource key)
            (qq-media--ensure-resource-image key fetcher nil image-builder))
           ((not (qq-server-ready-p)) nil)
           ((qq-media--native-preview-part-failed-p media-id part) nil)
           ((gethash (qq-media--fetch-key attempt-key) qq-media--native-preview-attempts) nil)
           (t
            (puthash (qq-media--fetch-key attempt-key) t qq-media--native-preview-attempts)
            (qq-media--ensure-resource-image
             key
             (lambda (done error)
               (funcall
                fetcher
                (lambda (resource)
                  (remhash (qq-media--fetch-key attempt-key) qq-media--native-preview-attempts)
                  (funcall done resource))
                (lambda (response reason)
                  ;; Only explicit operations or lifecycle replacement retry.
                  (funcall error response reason))))
             nil image-builder)))))))

(defun qq-media--fetch-native-image-part-resource
    (segment media-id part key callback errback)
  "Retain native MEDIA-ID PART's source; cancellation detaches only this owner."
  (let* ((owner (qq-media--current-consumer))
         (state (qq-media--owner-state owner))
         (owner-account (plist-get state :account))
         (operations (plist-get state :operations))
         (token (list nil))
         (generation qq-media--generation)
         (done (qq-media--owned-callback owner generation callback))
         (fail (qq-media--owned-callback
                owner generation (or errback #'qq-media--default-error)))
         operation)
    (puthash key token operations)
    (setq operation
          (qq-remote-media-prepare-part-local-access
           media-id part
           (lambda (result)
             (let ((ref (alist-get 'lease result)))
               (if (not (and (= generation qq-media--generation)
                             (eq state (gethash owner qq-media--consumers))
                             (eq token (gethash key operations))))
                   (qq-resource-access-release ref)
                 (remhash key operations)
                 (let ((qq-media--consumer owner))
                   (qq-runtime-with-account owner-account
                     (qq-media--adopt-access
                      owner (alist-get 'resource_id result) (alist-get 'access result) ref
                      (lambda (resource)
                        (when (and (eq part 'content)
                                   (qq-media--native-image-animated-p segment))
                          (setf (alist-get 'animated resource) t))
                        (qq-media--cache-resource
                         (if (eq part 'content) (qq-media--segment-resource-key segment) key)
                         resource)
                        (funcall done resource))))))))
           (lambda (body reason)
             (when (eq token (gethash key operations))
               (remhash key operations)
               (funcall fail body reason)))
           (alist-get 'media-source segment)))
    (setcar token operation)
    operation))

(defun qq-media--fetch-native-image-resource
    (segment key callback errback)
  "Materialize native image SEGMENT into KEY.

Call CALLBACK with the retained original; call ERRBACK on failure."
  (qq-media--fetch-native-image-part-resource
   segment (qq-media--native-image-media-id segment) 'content
   key callback errback))

(defun qq-media--fetch-native-video-thumbnail-resource
    (segment key callback errback)
  "Materialize native video SEGMENT thumbnail into KEY.

Call CALLBACK with the retained thumbnail; call ERRBACK on failure."
  (qq-media--fetch-native-image-part-resource
   segment (qq-media--native-video-media-id segment) 'thumbnail
   key callback errback))

(defun qq-media--fetch-native-content (segment media-id key callback errback)
  "Retain native original MEDIA-ID for SEGMENT without copying its bytes."
  (qq-media--fetch-native-image-part-resource
   segment media-id 'content key callback errback))

(defun qq-media--fetch-native-video-resource (segment key callback errback)
  "Retain native video SEGMENT's original source under KEY."
  (if-let* ((media-id (qq-media--native-video-media-id segment)))
      (qq-media--fetch-native-content segment media-id key callback errback)
    (funcall (or errback #'qq-media--default-error)
             nil "video segment has no native media handle")))

(defun qq-media--fetch-native-file-resource (segment key callback errback)
  "Retain native file SEGMENT's original source under KEY."
  (if-let* ((media-id (qq-media--native-file-media-id segment)))
      (qq-media--fetch-native-content segment media-id key callback errback)
    (funcall (or errback #'qq-media--default-error)
             nil "file segment has no native media handle")))

(defun qq-media--resource-fetching-p (key)
  "Return non-nil when KEY is currently being fetched."
  (gethash (qq-media--fetch-key key) qq-media--fetching-cache))

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
       (if emoji-id
           (qq-media--fetch-face-resource emoji-id callback error-fn)
         (funcall error-fn nil "face segment has no id")))
      ("mface"
       (qq-media--resolve-fileish-segment
        segment callback error-fn
        "mface segment has neither local file nor URL"))
      (_
       (funcall error-fn nil (format "unsupported segment type for resource fetch: %s" type))))))

(defun qq-media-resolve-segment-resource
    (segment callback &optional errback streamable)
  "Resolve SEGMENT's original through Gateway with captured presentation owner.

With STREAMABLE, a native video that is not yet local resolves to a Gateway
loopback stream instead of materializing; callers needing a file omit it."
  (let* ((key (qq-media--segment-resource-key segment))
         (owner (qq-media--current-consumer))
         (generation qq-media--generation)
         (fail (qq-media--owned-callback
                owner generation (or errback #'qq-media--default-error)))
         (done (qq-media--owned-callback
                owner generation
                (lambda (resource)
                  (qq-media--acquire-source
                   resource
                   (lambda (ready)
                     (when (and key (not (alist-get 'stream-id ready)))
                       (qq-media--cache-resource key ready)
                       (qq-media--note-cache-updated key))
                     (funcall callback ready))
                   fail owner))))
         (cached (and key (qq-media--cached-resource key)))
         (download (plist-get (qq-media-segment-download-state segment) :path))
         (local (or (qq-media--segment-existing-path segment)
                    (and (appkit-media-file-present-p download) download)))
         (stream-media-id (and streamable (not local) (not cached)
                               (qq-media--native-video-stream-media-id segment))))
    (cond
     (local (funcall done `((file . ,local))))
     (cached (funcall done cached))
     (stream-media-id
      (qq-remote-media-open-stream
       stream-media-id
       (qq-media--owned-callback
        owner generation
        (lambda (stream)
          (funcall done (qq-media--stream-resource segment stream))))
       ;; An older Gateway or an unstreamable rendition still materializes.
       (qq-media--owned-callback
        owner generation
        (lambda (&rest _)
          (qq-media--fetch-segment-resource segment done fail)))
       (alist-get 'media-source segment)))
     (t (qq-media--fetch-segment-resource segment done fail)))
       (alist-get 'media-source segment)))

(defun qq-media--native-video-stream-media-id (segment)
  "Return native video SEGMENT's media ID when it should stream, or nil.

Content that is already materialized opens from its staged resource."
  (when-let* ((media-id (qq-media--native-video-media-id segment))
              ((qq-remote-media-streaming-available-p)))
    (unless (equal (alist-get 'phase (qq-remote-media-part
                                      (qq-remote-media media-id) 'content))
                   "materialized")
      media-id)))

(defun qq-media--stream-resource (segment stream)
  "Return a playable resource for SEGMENT's Gateway loopback STREAM."
  `((url . ,(alist-get 'url stream))
    (stream-id . ,(alist-get 'stream_id stream))
    (name . ,(or (alist-get 'name (alist-get 'data segment)) "video.mp4"))
    (mime-type . "video/mp4")))

(cl-defun qq-media-segment-open (segment &key owner)
  "Resolve SEGMENT in an Effect owned by the exact initiating Surface."
  (if (qq-media--native-record-media-id segment)
      (qq-media-play-native-record
       segment
       :owner (qq-media--initiating-surface owner))
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
         (entry (copy-tree (or (gethash (qq-media--scoped-key key) qq-media--download-state-table) '())))
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
    (puthash (qq-media--scoped-key key) entry qq-media--download-state-table)
    entry))

(defun qq-media--put-segment-download-state (segment entry)
  "Store download-state ENTRY for SEGMENT and notify UI."
  (let ((key (qq-media-segment-download-key segment)))
    (puthash (qq-media--scoped-key key) entry qq-media--download-state-table)
    (qq-media--note-cache-updated key)
    ;; A previous preview attempt may have had only a poster or no source at
    ;; all.  Once the actual media exists, allow preview extraction to retry.
    (when (and (eq (plist-get entry :status) 'downloaded)
               (qq-media--absolute-local-file-present-p
                (plist-get entry :path)))
      (when-let* ((preview-key (qq-media-segment-preview-key segment)))
        (when (gethash (qq-media--scoped-key preview-key) qq-media--preview-missing-cache)
          (remhash (qq-media--scoped-key preview-key) qq-media--preview-missing-cache)
          (qq-media--note-cache-updated preview-key)))))
  entry)

(defun qq-media-segment-local-file (segment)
  "Return SEGMENT's original file, never a preview derivative."
  (let* ((segment-file (qq-media--segment-existing-path segment))
         (download (plist-get (qq-media-segment-download-state segment) :path))
         (original (qq-media--cached-resource (qq-media--segment-resource-key segment)))
         (file (alist-get 'file original)))
    (cond
     ((qq-media--absolute-local-file-present-p segment-file) segment-file)
     ((qq-media--absolute-local-file-present-p download) download)
     ((qq-media--absolute-local-file-present-p file) file))))

(defun qq-media-segment-inline-resource (segment)
  "Return only an owned local presentation source; never stream a remote URL."
  (let* ((preview (and (not (qq-media-videoish-segment-p segment))
                       (qq-media--cached-resource
                        (qq-media-segment-preview-key segment))))
         (file (or (alist-get 'file preview)
                   (qq-media-segment-local-file segment))))
    (when (appkit-media-file-present-p file)
      (appkit-media-resource-create :file file))))

(defun qq-media--segment-media-id (segment)
  "Return SEGMENT's opaque native media ID, or nil."
  (or (qq-media--native-record-media-id segment)
      (qq-media--native-image-media-id segment)
      (qq-media--native-video-media-id segment)
      (qq-media--native-file-media-id segment)))

(defun qq-media-segment-cancel-transfer (segment)
  "Cancel this consumer's work without cancelling shared Gateway materialization."
  (if-let* ((cancel (plist-get (qq-media-segment-download-state segment) :cancel)))
      (funcall cancel)
    (when-let* ((state (gethash (qq-media--current-consumer) qq-media--consumers)))
      (dolist (key (delete-dups
                    (list (qq-media--segment-resource-key segment)
                          (qq-media-segment-preview-key segment))))
        (let ((operation (car (gethash key (plist-get state :operations)))))
          (remhash key (plist-get state :operations))
          (when (qq-remote-media-operation-p operation)
            (qq-remote-media-cancel-operation operation)))
        (remhash (qq-media--fetch-key key) qq-media--fetching-cache)
        (puthash (qq-media--scoped-key key) t qq-media--preview-missing-cache)
        (qq-media--note-cache-updated key)))))

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
  "Export SEGMENT's original with an operation-held source reference."
  (when open-after (setq owner (qq-media--initiating-surface owner)))
  (let* ((entry (qq-media-segment-download-state segment))
         (path (plist-get entry :path))
         (owner-account (plist-get (qq-media--owner-state) :account)))
    (when (eq (plist-get entry :status) 'downloading)
      (user-error "qq: media download already in progress"))
    (if (and (eq (plist-get entry :status) 'downloaded)
             (appkit-media-file-present-p path))
        (progn
          (when open-after (qq-media-segment-open-local segment :owner owner))
          path)
      (setq entry (plist-put entry :status 'downloading))
      (qq-media--put-segment-download-state segment entry)
      (let ((cancel
             (qq-media--export-segment
              segment path
              (lambda (_file)
                (qq-runtime-with-account owner-account
                  (qq-media--put-segment-download-state
                   segment (list :path path :status 'downloaded))
                  (when (and open-after (appkit-surface-live-p owner))
                    (qq-media-segment-open-local segment :owner owner))))
              (lambda (reason)
                (qq-runtime-with-account owner-account
                  (qq-media--put-segment-download-state
                   segment (list :path path :status 'error :error reason)))))))
        (when (eq (plist-get (qq-media-segment-download-state segment) :status)
                  'downloading)
          (qq-media--put-segment-download-state
           segment (list :path path :status 'downloading :cancel cancel))))
      nil)))

(cl-defun qq-media-segment-open-local (segment &key owner)
  "Open SEGMENT's original, preserving granted identity for viewer ownership."
  (if-let* ((file (qq-media-segment-local-file segment)))
      (let* ((cached (qq-media--cached-resource (qq-media--segment-resource-key segment)))
             (resource (if (equal file (alist-get 'file cached))
                           cached `((file . ,file)))))
        (qq-media-open-resource
         resource (qq-media-segment-kind segment)
         (qq-media--segment-resource-key segment) :owner owner))
    (user-error "qq: media segment has no local file yet")))

(defun qq-media--export-segment (segment target done fail)
  "Copy original SEGMENT to user TARGET while holding an independent lease."
  (let* ((owner (list 'qq-media-export))
         (qq-media--consumer owner)
         (owner-account (plist-get (qq-media--owner-state owner) :account))
         (finished nil)
         transfer)
    (cl-labels
        ((finish (success value)
           (unless finished
             (setq finished t)
             (unwind-protect
                 (qq-runtime-with-account owner-account
                   (funcall (if success done fail) value))
               (qq-media-release-consumer owner))))
         (copy-source (resource)
           (condition-case condition
               (setq transfer
                     (appkit-media-copy-or-download-resource-async
                      (appkit-media-resource-create :file (alist-get 'file resource))
                      target
                      (lambda (file) (finish t file))
                      (lambda (reason) (finish nil reason))))
             ((error quit) (finish nil (error-message-string condition))))))
      (condition-case condition
          (qq-media-resolve-segment-resource
           segment #'copy-source
           (lambda (_body reason) (finish nil reason)))
        ((error quit) (finish nil (error-message-string condition))))
      (let ((cancel
             (lambda ()
               (unless finished
                 (when (appkit-media-transfer-p transfer)
                   (appkit-media-cancel-transfer transfer))
                 (finish nil "cancelled")))))
        (when-let* ((state (gethash owner qq-media--consumers)))
          (plist-put state :cancel cancel))
        cancel))))

(defun qq-media-segment-save-as (segment &optional target-path)
  "Save SEGMENT's original bytes to the explicit user-owned TARGET-PATH."
  (interactive)
  (let* ((name (qq-media-segment-default-save-name segment))
         (target (or target-path (read-file-name "Save media as: " nil name nil name))))
    (qq-media--export-segment
     segment target
     (lambda (_file) (message "qq: saved media -> %s" target))
     (lambda (reason) (message "qq: failed to save media: %s" reason)))))

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
      (qq-media-segment-open segment
                             :owner owner)
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
   (when-let* ((friend (qq-state-friend (qq-runtime-state) user-id))
               (url (alist-get 'avatar_url friend))
               ((appkit-media-url-present-p url)))
     `((url . ,url)))
   (catch 'resource
     (dolist (session (qq-state-sessions (qq-runtime-state)))
       (dolist (message (qq-state-session-messages (qq-runtime-state) (alist-get 'key session)))
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
  (appkit-chat-avatar-one-line-string
   (qq-media-avatar-image user-id)
   "@"))

(defun qq-media-avatar-cached-display-string (user-id)
  "Return cached inline avatar for USER-ID without starting network work."
  (appkit-chat-avatar-one-line-string
   (qq-media--cached-image (format "avatar:%s" user-id))
   "@"))

(defun qq-media-http-url-p (value)
  "Return non-nil for an absolute HTTP(S) VALUE with a host.
Reject whitespace and credentials rather than turning malformed metadata
into a remote image request or browser action."
  (and (stringp value)
       (not (string-match-p "[[:space:][:cntrl:]]" value))
       (string-match-p "\\`https?://" value)
       (condition-case nil
           (let ((url (url-generic-parse-url value)))
             (and (member (url-type url) '("http" "https"))
                  (qq-protocol-non-empty-string-p (url-host url))
                  (not (url-user url))
                  (not (url-password url))))
         (error nil))))

(defun qq-media-link-preview-icon-url (segment)
  "Return SEGMENT's usable native URL-preview icon, or nil.
QQ's default-picture flag denotes a generic placeholder, not a site icon.
Only a preview of an actual HTTP(S) link may load a remote icon."
  (let* ((data (alist-get 'data segment))
         (preview (alist-get 'link_preview data))
         (text (alist-get 'text data))
         (icon (alist-get 'icon_url preview)))
    (and (equal (alist-get 'type segment) "text")
         preview
         (eq (alist-get 'is_default_picture preview) :false)
         (stringp text)
         (qq-media-http-url-p (string-trim text))
         (qq-media-http-url-p icon)
         icon)))

(defun qq-media-card-image-cache-key (segment)
  "Return the account-owned identity of SEGMENT's card cover."
  (when (equal (alist-get 'type segment) "card")
    (let* ((data (alist-get 'data segment))
           (url (alist-get 'image data))
           (post (alist-get 'post (alist-get 'guild-share data))))
      (when (qq-media-http-url-p url)
        (list 'qq-card-cover (qq-runtime-current-account-id)
              (qq-server-gateway-instance-id) (or post url))))))

(defun qq-media-card-image-demand (segment)
  "Describe SEGMENT's shared preview acquisition without starting work."
  (when-let* ((identity (qq-media-card-image-cache-key segment)))
    (let* ((url (alist-get 'image (alist-get 'data segment)))
           (base (expand-file-name
                  (secure-hash 'sha256 (prin1-to-string
                                       (list (nth 1 identity) (nth 3 identity))))
                  (expand-file-name "qq/card-images/" user-emacs-directory))))
      (appkit-resource-demand-create
       :key (list :media identity)
       :input (appkit-media-image-acquisition-create
               (appkit-media-resource-create :url url :name "卡片图片") base)
       :loader #'appkit-media-image-resource-load
       :acquisition-identity (list identity url)
       :sharing-policy 'app-private :cache-policy 'while-interested))))

(defun qq-media-card-image-resource (surface segment)
  "Return SEGMENT's cover URL and any file already acquired by SURFACE."
  (let* ((identity (qq-media-card-image-cache-key segment))
         (state (appkit-resource-state surface (list :media identity))))
    (appkit-media-resource-create
     :url (alist-get 'image (alist-get 'data segment))
     :file (when (and state (eq (appkit-resource-state-status state) 'ready))
             (appkit-resource-state-value state)))))

(defun qq-media-link-preview-cache-key (segment)
  "Return SEGMENT's safe native link icon resource identity, or nil."
  (when-let* ((url (qq-media-link-preview-icon-url segment)))
    (format "link-preview-icon:%s" url)))

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
  (appkit-media-image-display-string
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
     (lambda (file _spec)
       (qq-media--one-line-preview-image-from-file file)))))

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
  (appkit-chat-avatar-one-line-string
   (qq-media-group-avatar-image group-id)
   "#"))

(defun qq-media-group-avatar-cached-display-string (group-id)
  "Return cached inline group avatar for GROUP-ID without fetching it."
  (appkit-chat-avatar-one-line-string
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
       (qq-media--note-cache-updated "system-emoji-catalog")
       (run-hook-with-args 'qq-media-system-emoji-catalog-hook owner nil))
     :errback
     (lambda (_body reason)
       (remhash owner qq-media--system-emoji-requests)
       (run-hook-with-args 'qq-media-system-emoji-catalog-hook owner
                           (or reason "System emoji catalog unavailable"))))))

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

(defun qq-media--large-face-entry-p (entry)
  "Return non-nil when catalog ENTRY has a supported large-face identity."
  (and (memq (alist-get 'emoji_type entry) '(1 2 3))
       (alist-get 'animated_pack_id entry)
       (alist-get 'animated_sticker_id entry)))

(defun qq-media-face-completion-candidates (&optional large-p)
  "Return system face candidates sorted by numeric id.
With LARGE-P, include only visible catalog entries with large-face metadata.

Each candidate looks like \"/斜眼笑  (178)\" so users can search by name
or numeric id.  Use `qq-media-face-id-from-completion'.

Order is by face id (0, 1, 2, …) — the same order as QQ's default
emoji panel.  Pair with `qq-media-face-completion-table' so Vertico
does not re-sort by string length/history."
  (let ((table (if large-p (make-hash-table :test #'equal)
                 (copy-hash-table (qq-media--load-face-names-table))))
        (candidates nil))
    (when-let* ((owner (qq-runtime-current-account-id))
                (catalog (gethash owner qq-media--system-emoji-tables)))
      (maphash
       (lambda (id entry)
         (let ((name (alist-get 'description entry)))
           (when (and (qq-protocol-non-empty-string-p name)
                      (or (not large-p)
                          (and (qq-media--large-face-entry-p entry)
                               (not (eq t (alist-get 'hidden entry))))))
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
  (let ((image (qq-media-face-image id)))
    (if image
        (concat (appkit-media-image-display-string image " ") " ")
      "  ")))

(defun qq-media-face-affixation-function (candidates)
  "Affixation function: show the cached face preview before each CANDIDATE.

Missing resources load asynchronously through Gateway; pending faces get
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

(defun qq-media-favorite-image-p (segment)
  "Return non-nil when SEGMENT has original native image content to collect."
  (and (qq-media--native-image-media-id segment) t))

(defun qq-media-save-image-as-favorite (segment callback &optional errback)
  "Collect original image SEGMENT in the initiating account's favorites.

Materialize the native content, not a thumbnail or an Emacs preview conversion.
The resulting Resource belongs to remote media and is borrowed, never released
by this operation.  CALLBACK and ERRBACK follow the native RPC convention."
  (let ((media-id (qq-media--native-image-media-id segment))
        (owner (qq-favorite-emoji--current-owner)))
    (unless media-id
      (user-error "qq: this image has no native original to collect"))
    (qq-remote-media-materialize
     media-id
     (lambda (_media)
       (qq-remote-media-await-materialized
        media-id
        (lambda (media)
          (let ((resource-id
                 (alist-get 'resource_id
                            (qq-remote-media-part media 'content))))
            (cond
             ((not (qq-account-get owner))
              (qq-rpc-client-error
               errback "account_removed"
               "QQ account was removed while collecting the image"))
             ((not (equal (alist-get 'account_id media) owner))
              (qq-rpc-client-error
               errback "media_account_mismatch"
               "The image belongs to another QQ account"))
             (t
              (qq-runtime-with-account owner
                (qq-favorite-emoji-add resource-id callback errback))))))
        errback))
     errback)))

(defun qq-media-custom-face-id (face)
  "Return FACE's durable native favorite identity, or nil."
  (let ((favorite-id (and (listp face)
                          (alist-get 'favorite_emoji_id face))))
    (and (qq-favorite-emoji-id-p favorite-id) favorite-id)))

(defun qq-media-custom-face-label (face &optional index)
  "Return a stable human completion label for favorite FACE.

INDEX keeps candidates distinct even if their short MD5 prefixes collide."
  (let* ((description (alist-get 'description face))
         (md5 (alist-get 'md5 face))
         (short (if (and (stringp md5) (>= (length md5) 8))
                    (concat (substring md5 0 8) "…")
                  "favorite"))
         (base (format "[收藏] %s"
                       (if (and (stringp description)
                                (not (string-empty-p description)))
                           (replace-regexp-in-string
                            "[\n\r\t]+" " " description)
                         short))))
    (if index
        (format "%s  (%d)" base (1+ index))
      base)))

(defun qq-media-custom-face-image (face)
  "Return favorite FACE's preview using its authoritative Gateway identity."
  (when-let* ((id (qq-media-custom-face-id face)))
    (qq-media--ensure-resource-image
     (format "favorite-emoji:%s" id)
     (lambda (done fail)
       (let ((owner (qq-media--current-consumer))
             (generation qq-media--generation))
         (qq-favorite-emoji-materialize
          id
          (qq-media--owned-callback
           owner generation
           (lambda (result)
             (qq-media--acquire-source
              `((resource-id . ,(alist-get 'resource_id (alist-get 'resource result))))
              done fail owner)))
          fail)))
     (max qq-media-face-image-height 32))))

(defun qq-media--custom-face-completion-prefix (face)
  "Return a visual completion prefix for favorite FACE."
  (if-let* ((image (qq-media-custom-face-image face)))
      (concat (appkit-media-image-display-string image " ") " ")
    "  "))

(defun qq-media-custom-face-display-string (face)
  "Return composer display text for native favorite FACE."
  (appkit-media-image-display-string
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


(defun qq-media--fetch-face-resource (emoji-id callback errback)
  "Retain the original system EMOJI-ID resource granted by Gateway."
  (let* ((id (format "%s" emoji-id))
         (owner (qq-media--current-consumer))
         (generation qq-media--generation)
         (fail (qq-media--owned-callback owner generation errback)))
    (qq-rpc-call
     "system_emoji.resource" `((emoji_id . ,id))
     :projector
     (lambda (result)
       (unless (and (qq-server-wire-exact-object-keys-p
                     result '(emoji_id mime_type animated resource))
                    (equal (alist-get 'emoji_id result) id)
                    (member (alist-get 'mime_type result)
                            '("application/lottie+json" "image/apng"
                              "image/png" "image/gif" "image/webp" "image/jpeg"))
                    (memq (alist-get 'animated result) '(t nil :false)))
         (error "qq: Gateway returned an invalid system-emoji resource"))
       (qq-resource--upsert (alist-get 'resource result) 'system-emoji-resource)
       result)
     :callback
     (qq-media--owned-callback
      owner generation
      (lambda (result)
        (let ((resource-id (alist-get 'resource_id (alist-get 'resource result))))
          (qq-resource-await-ready
           resource-id
           (qq-media--owned-callback
            owner generation
            (lambda (_snapshot)
              (qq-media--acquire-resource
               resource-id
               (lambda (resource)
                 (setf (alist-get 'mime-type resource) (alist-get 'mime_type result)
                       (alist-get 'emoji_id resource) id
                       (alist-get 'animated resource) (eq t (alist-get 'animated result)))
                 (funcall callback resource))
               fail owner)))
           fail))))
     :errback fail)))

(defun qq-media-face-resource (emoji-id)
  "Return EMOJI-ID's cached original resource, starting one Gateway request.

This is a presentation cache, never a discovery path into LinuxQQ data."
  (let* ((key (format "face:%s" emoji-id))
         (cached (qq-media--cached-resource key)))
    (qq-media--retain-cache-key key)
    (or cached
        (progn
          (when (and (qq-server-ready-p)
                     (qq-rpc-method-available-p "system_emoji.resource")
                     (not (gethash (qq-media--fetch-key key) qq-media--fetching-cache))
                     (not (gethash (qq-media--scoped-key key) qq-media--preview-missing-cache)))
            (let ((token (list 'face-resource)))
              (puthash (qq-media--fetch-key key) token qq-media--fetching-cache)
              (cl-labels
                  ((fail (_body _reason)
                     (when (eq token (gethash (qq-media--fetch-key key) qq-media--fetching-cache))
                       (remhash (qq-media--fetch-key key) qq-media--fetching-cache)
                       (puthash (qq-media--scoped-key key) t qq-media--preview-missing-cache)
                       (qq-media--note-cache-updated key))))
                (condition-case error-data
                    (qq-media--fetch-face-resource
                     emoji-id
                     (lambda (resource)
                       (when (eq token (gethash (qq-media--fetch-key key) qq-media--fetching-cache))
                         (remhash (qq-media--fetch-key key) qq-media--fetching-cache)
                         (qq-media--cache-resource key resource)
                         (qq-media--note-cache-updated key)))
                     #'fail)
                  (error (fail nil (error-message-string error-data)))))))
          (qq-media--cached-resource key)))))

(defun qq-media-face-to-segment (emoji-id)
  "Build an explicitly small system EMOJI-ID segment.
Large-face catalog metadata never changes this selected send form."
  (let* ((id (format "%s" emoji-id))
         (number (qq-media--face-id-number id))
         (description (qq-media-face-name id)))
    (unless (and number (<= 0 number #xffffffff))
      (user-error "qq: system face id is not a uint32"))
    `((type . "face")
      (data . ((id . ,id)
               (face_type . ,(if (< number 260) "basic" "small"))
               ,@(when description `((description . ,description))))))))

(defun qq-media-large-face-to-segment (emoji-id)
  "Build an explicitly large system EMOJI-ID segment from native metadata."
  (let* ((id (format "%s" emoji-id))
         (number (qq-media--face-id-number id))
         (entry (qq-media-system-emoji-entry id))
         (description (alist-get 'description entry)))
    (unless (and number (< 0 number) (<= number #xffffffff))
      (user-error "qq: large system face id is not a non-zero uint32"))
    (unless (qq-media--large-face-entry-p entry)
      (user-error "qq: large system face %s needs its native catalog metadata" id))
    `((type . "face")
      (data . ((id . ,id) (face_type . "animated")
               (pack_id . ,(alist-get 'animated_pack_id entry))
               (sticker_id . ,(alist-get 'animated_sticker_id entry))
               (sticker_type . ,(alist-get 'emoji_type entry))
               ,@(when description `((description . ,description))))))))

(defun qq-media--derive-resource (resource kind callback errback)
  "Acquire a distinct Gateway KIND derivative without relabelling RESOURCE."
  (let* ((owner (qq-media--current-consumer))
         (generation qq-media--generation)
         (done (qq-media--owned-callback owner generation callback))
         (fail (qq-media--owned-callback owner generation errback)))
    (cl-labels
        ((derive (id)
           (qq-resource-derive-preview
            id kind
            (qq-media--owned-callback
             owner generation
             (lambda (snapshot)
               (qq-resource-await-ready
                (alist-get 'resource_id snapshot)
                (qq-media--owned-callback
                 owner generation
                 (lambda (ready)
                   (qq-media--acquire-resource
                    (alist-get 'resource_id ready) done fail owner)))
                fail)))
            fail)))
      (if-let* ((id (alist-get 'resource-id resource)))
          (derive id)
        (qq-resource-stage-local
         (alist-get 'file resource) (alist-get 'name resource) nil
         (qq-media--owned-callback
          owner generation
          (lambda (snapshot)
            (qq-resource-await-ready
             (alist-get 'resource_id snapshot)
             (qq-media--owned-callback
              owner generation
              (lambda (ready) (derive (alist-get 'resource_id ready))))
             fail)))
         fail)))))

(defun qq-media-face-image (emoji-id &optional animated-p)
  "Return the immutable first-frame image for QQ face EMOJI-ID.
ANIMATED-P selects the larger chat animation geometry.  Lottie is drawn by
the native renderer from the original JSON, never an intermediate GIF."
  (let* ((id (format "%s" emoji-id))
         (key (format "face:%s%s" id (if animated-p ":poster" "")))
         (height (if animated-p qq-media-animated-face-image-height
                   qq-media-face-image-height))
         (local (qq-media-face-resource id)))
    (when local
      (qq-media--cache-resource key local)
      (appkit-media-image-with-resource
       (qq-media--ensure-resource-image
        key (lambda (done _error) (funcall done local))
        height
        (if (equal (alist-get 'mime-type local) "application/lottie+json")
            #'appkit-media-lottie-poster
          #'qq-media--image-from-file))
       (appkit-media-resource-create
        :file (alist-get 'file local)
        :mime-type (alist-get 'mime-type local))
       :interaction (and animated-p 'media) :label "qq"
       :animation-loop-policy 'forever))))

(defun qq-media-face-display-string (emoji-id &optional description)
  "Return an inline fallback display string for QQ face EMOJI-ID."
  (appkit-media-image-display-string
   (qq-media-face-image emoji-id)
   (or (and (stringp description)
            (not (string-empty-p description))
            description)
       (qq-media-face-text-fallback emoji-id))))

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
             ((qq-media-videoish-segment-p segment) "video")
             (t type)))
           (file-key (qq-media--segment-file-key segment))
           (url (qq-media--segment-url segment)))
      (cond
       (native-image-id (concat "preview:" (qq-media--native-image-key native-image-id)))
       (native-video-id
        (qq-media--native-video-thumbnail-key native-video-id))
       (native-file-id (concat "preview:" (qq-media--native-file-key native-file-id)))
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
    (when-let* ((icon-key (qq-media-link-preview-cache-key segment)))
      (push icon-key keys))
    (when-let* ((card-key (qq-media-card-image-cache-key segment)))
      (push card-key keys))
    (when (and (equal type "gray-tip")
               (equal (alist-get 'kind data) "poke")
               (appkit-media-url-present-p (alist-get 'image-url data)))
      (push (qq-media-poke-image-cache-key (alist-get 'image-url data)) keys))
    (delete-dups (delq nil keys))))

(defun qq-media--video-segment-preview-image (segment key)
  "Render Gateway's representative video preview, never run client ffmpeg."
  (qq-media--ensure-resource-image
   key
   (lambda (done fail)
     (qq-media-resolve-segment-resource
      segment
      (lambda (original)
        (qq-media--derive-resource original "video" done fail))
      fail))
   nil #'qq-media--preview-image-from-file))

(defun qq-media-segment-preview-image (segment)
  "Return a Gateway-owned preview for SEGMENT; failures remain soft."
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
        (when (and (not (qq-media--cached-resource key))
                   (or local (appkit-media-url-present-p url)))
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
            (or (and (not (qq-media-videoish-segment-p segment))
                     (qq-media--segment-existing-path segment))
                (and resource
                     (qq-media--resource-image-file key resource))
                (qq-media--preview-image-source-file preview-image))))
      (when file
        (let ((image (qq-media--one-line-preview-image-from-file file)))
          (if (qq-media-videoish-segment-p segment)
              (appkit-media-image-with-resource
               image (qq-media-segment-inline-resource segment)
               :kind 'video :interaction nil :fit 'cover)
            image))))))

(defun qq-media--message-one-line-preview-text
    (message text &optional mention-function)
  "Render MESSAGE's faces and mentions inline, retaining TEXT otherwise.

MENTION-FUNCTION, when non-nil, receives an `at' segment's data and returns
its styled one-line text, so a mention keeps its face as in the timeline."
  (let ((segments (alist-get 'segments message)))
    (if (or (qq-state-message-recalled-p message)
            (not (seq-some
                  (lambda (segment)
                    (let ((type (alist-get 'type segment)))
                      (or (equal type "face")
                          (and mention-function (equal type "at")))))
                  segments)))
        text
      (qq-state-preview-one-line
       (mapconcat
        (lambda (segment)
          (let* ((type (alist-get 'type segment))
                 (data (alist-get 'data segment))
                 (fallback
                  (qq-state-message-preview-from-segments (qq-runtime-state) (list segment)))
                 (id (and (equal type "face") (alist-get 'id data)))
                 (image (and id (qq-media-face-image id))))
            (cond
             (image
              (concat " " (appkit-media-one-line-image-display-string
                           image fallback)
                      " "))
             (id (concat " " fallback " "))
             ((and mention-function (equal type "at"))
              (funcall mention-function data))
             ;; Keep the text's own edge whitespace: it separates segments.
             ((equal type "text") (or (alist-get 'text data) ""))
             (t fallback))))
        segments "")))))

(cl-defun qq-media-message-one-line-preview
    (message text &key label separator label-face mention-function)
  "Return an Appkit one-line preview with inline faces for MESSAGE and TEXT.

MENTION-FUNCTION styles mention segments; see
`qq-media--message-one-line-preview-text'."
  (let* ((segment (qq-media-message-primary-preview-segment message))
         (image
          (and segment
               (qq-media-segment-one-line-preview-image segment)))
         (visual
          (and image
               (appkit-media-one-line-image-display-string image "▧"))))
    (appkit-ui-one-line-preview-create
     :text (qq-media--message-one-line-preview-text
            message text mention-function)
     :label label
     :separator separator
     :visual visual
     :visual-columns
     (and visual (max 1 qq-media-one-line-preview-columns))
     :label-face label-face)))

(defun qq-media-message-one-line-preview-keys (message)
  "Return media keys that can change MESSAGE's one-line preview."
  (let* ((segment (qq-media-message-primary-preview-segment message))
         (key (and segment (qq-media-segment-preview-key segment)))
         (keys (and key (list key))))
    (seq-doseq (segment (alist-get 'segments message))
      (when-let* (((equal (alist-get 'type segment) "face"))
                  (id (alist-get 'id (alist-get 'data segment))))
        (cl-pushnew (format "face:%s" id) keys :test #'equal)))
    keys))


(defvar-local qq-media--viewport nil)
(defvar-local qq-media--viewport-property nil)
(defvar-local qq-media--viewport-observer nil)

(defun qq-media--visible-row-keys (property)
  "Return rows intersecting visible windows according to PROPERTY."
  (let (keys)
    (dolist (window (get-buffer-window-list (current-buffer) nil t))
      (when-let* ((range (appkit-scroll-window-visible-range window)))
        (let ((position (car range)) (end (cdr range)))
          (while (< position end)
            (when-let* ((key (get-text-property position property)))
              (cl-pushnew key keys :test #'equal))
            (setq position (next-single-property-change position property nil end))))))
    (nreverse keys)))

(defun qq-media--check-viewport (&rest _ignored)
  "Update Appkit's resource interests when the visible media rows change."
  (when-let* ((surface (appkit-current-surface))
              ((appkit-surface-live-p surface))
              (property qq-media--viewport-property))
    (let ((keys (qq-media--visible-row-keys property)))
      (unless (equal keys qq-media--viewport)
        (setq qq-media--viewport keys)
        (qq-runtime-invalidate surface :frame-p t)))))

(defun qq-media--load-index (_context input success failure)
  "Retain INPUT's media index through Appkit's shared interest lifetime."
  (pcase-let ((`(,account ,media-id ,source) input))
    (let (capability pending settled)
      (cl-labels
          ((release () (when capability
                         (funcall (appkit-cancellation-cancel capability))))
           (accept (media)
             (if (not capability)
                 (setq pending media)
               (setq settled t)
               (funcall success
                        (appkit-resource-value-create
                         :value media :release #'release)))))
        (setq capability
              (qq-runtime-with-account account
                (qq-remote-media-acquire-lease
                 media-id source #'accept
                 (lambda (_body reason) (funcall failure reason)))))
        (when pending (accept pending))
        (appkit-cancellation-create
         :kind 'logical :cancel (lambda () (unless settled (release))))))))

(defun qq-media--lease-demand (segment)
  "Describe SEGMENT's native media lease without acquiring it."
  (when-let* ((media-id (alist-get 'media_id (alist-get 'data segment))))
    (let* ((account (qq-runtime-current-account-id))
           (source (alist-get 'media-source segment))
           (key (list :media-lease account (qq-server-gateway-instance-id)
                      qq-resource--access-generation (or source media-id))))
      (appkit-resource-demand-create
       :key key :input (list account media-id source)
       :loader #'qq-media--load-index :acquisition-identity key
       :sharing-policy 'app-private :cache-policy 'while-interested))))

(defun qq-media-render-result (surface messages &optional result property row-key)
  "Combine visible media interests with RESULT using Appkit's resource broker.
The broker coalesces across chat, search and forward Surfaces and closes each
lease only after its last interested row disappears.  Open viewers separately
retain their resource access or stream capability."
  (setq property (or property 'qq-chat-message-anchor)
        row-key (or row-key #'qq-state-message-anchor)
        result (or result (appkit-render-result-create)))
  (setq qq-media--viewport-property property
        qq-media--viewport (qq-media--visible-row-keys property))
  (when (and (appkit-surface-live-p surface)
             (not (and (appkit-scroll-observer-p qq-media--viewport-observer)
                       (appkit-scroll-observer-active-p qq-media--viewport-observer))))
    (setq qq-media--viewport-observer
          (appkit-scroll-observer-install
           surface :start-function #'qq-media--check-viewport
           :end-function #'qq-media--check-viewport
           :visibility-function #'qq-media--check-viewport)))
  (let ((demands (make-hash-table :test #'equal))
        (interests (make-hash-table :test #'equal))
        (builders (append (when (qq-rpc-method-available-p "media.acquire")
                            (list #'qq-media--lease-demand))
                          (list #'qq-media-card-image-demand)))
        (update (appkit-render-result-resource-interest-update result)))
    ;; Native leases retain priority within the shared viewport budget.
    (dolist (build-demand builders)
      (dolist (message messages)
        (let ((row (funcall row-key message)))
          (when (member row qq-media--viewport)
            (dolist (segment (alist-get 'segments message))
              (when-let* ((demand (funcall build-demand segment))
                          (key (appkit-resource-demand-key demand)))
                (when (or (gethash key demands) (< (hash-table-count demands) 64))
                  (puthash key demand demands)
                  (puthash key (cons row (gethash key interests)) interests))))))))
    (appkit-render-result-create
     :resource-demands
     (append (appkit-render-result-resource-demands result)
             (hash-table-values demands))
     :resource-interest-update
     (appkit-resource-interest-update-create
      :mode 'replace
      :entries
      (append (and update (appkit-resource-interest-update-entries update))
              (mapcar
               (lambda (key)
                 (appkit-resource-interest-create
                  :key key :row-keys (delete-dups (gethash key interests))))
               (hash-table-keys interests)))))))


(provide 'qq-media)

;;; qq-media.el ends here
