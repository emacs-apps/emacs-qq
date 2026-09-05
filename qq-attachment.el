;;; qq-attachment.el --- QQ prepared attachments -*- lexical-binding: t; -*-

;; Author: 0WD0 <me@0wd0.com>

;;; Commentary:

;; Projection and upload-ahead helpers for Gateway Prepared Attachments.
;; Staged resources remain account-neutral; attachments bind
;; those immutable bytes to one stable account slot, one conversation, and one
;; media use.  Native MsgInfo, upload keys, Highway tickets, and local paths
;; never enter this projection.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'qq-rpc)
(require 'qq-account)
(require 'qq-favorite-emoji)
(require 'qq-resource)
(require 'qq-request)
(require 'qq-server)
(require 'qq-runtime)
(require 'qq-state)

(defvar qq-attachment-changed-hook nil
  "Hook called with REASON and ATTACHMENT-ID after projection changes.")

(defvar qq-attachment-desync-hook nil
  "Hook called with an unsolicited attachment stream error body.")

(defvar qq-attachment--attachments (make-hash-table :test #'equal))
(defvar qq-attachment--order nil)
(defvar qq-attachment--gateway-instance-id nil)
(defvar qq-attachment--refresh-owner nil
  "Identity of the newest authoritative attachment registry refresh.")
(defvar qq-attachment--resync-request-id nil
  "Identity of the in-flight automatic attachment registry resync.")

(cl-defstruct (qq-attachment-operation
               (:constructor qq-attachment-operation-create))
  "One client-side stage-and-prepare operation.

The service owns byte copying and QQ upload work.  This object owns only the
request callbacks and temporary resource/attachment identities created on
behalf of one caller."
  active-p
  request-id
  source-resource-id
  resource-id
  thumbnail-resource-id
  attachment-id
  resource-watch
  attachment-watch
  thumbnail-process
  thumbnail-buffer
  thumbnail-file)

(defun qq-attachment--id-p (value)
  "Return non-nil when VALUE is a canonical opaque attachment identity."
  (and (stringp value)
       (string-match-p
        (concat
         "\\`att-[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-"
         "[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}\\'")
        value)))

(defun qq-attachment (attachment-id)
  "Return a copy of Prepared ATTACHMENT-ID, or nil."
  (qq-server-value-copy
   (and attachment-id
        (gethash attachment-id qq-attachment--attachments))))

(defun qq-attachments ()
  "Return copied Prepared Attachment snapshots in authoritative order."
  (delq nil
        (mapcar #'qq-attachment qq-attachment--order)))

(defun qq-attachment--clear (reason)
  "Clear the projected attachment registry for REASON."
  (let ((changed (or qq-attachment--order
                     (> (hash-table-count
                         qq-attachment--attachments) 0)))
        (resync-marker qq-attachment--resync-request-id))
    (qq-rpc-cancel-latest
     'qq-attachment--refresh-owner "superseded_request"
     "Gateway attachment refresh context was cleared")
    (when (eq resync-marker qq-attachment--resync-request-id)
      (setq qq-attachment--resync-request-id nil))
    (setq qq-attachment--attachments (make-hash-table :test #'equal)
          qq-attachment--order nil)
    (when changed
      (qq-rpc-run-hook
       'qq-attachment-changed-hook reason nil))))

(defun qq-attachment-reset ()
  "Forget the client attachment projection without mutating service state."
  (setq qq-attachment--gateway-instance-id nil)
  (qq-attachment--clear 'reset))

(defun qq-attachment--replace (snapshots reason)
  "Atomically replace attachments with SNAPSHOTS for REASON."
  (let ((next (make-hash-table :test #'equal))
        order)
    (dolist (snapshot snapshots)
      (let ((attachment-id (alist-get 'attachment_id snapshot)))
        (when (gethash attachment-id next)
          (error "qq: Gateway attachment list duplicates %s" attachment-id))
        (puthash attachment-id snapshot next)
        (push attachment-id order)))
    (setq qq-attachment--attachments next
          qq-attachment--order (nreverse order))
    (qq-rpc-run-hook 'qq-attachment-changed-hook reason nil)
    (qq-attachments)))

(defun qq-attachment--phase-rank (phase)
  "Return lifecycle rank for attachment PHASE."
  (pcase phase
    ("queued" 0)
    ("negotiating" 1)
    ("uploading" 2)
    ("ready" 3)
    ("sending" 4)
    ((or "failed" "consumed" "canceled") 5)
    (_ -1)))

(defun qq-attachment--transition-p (from to)
  "Return non-nil when attachment phase FROM may transition to TO.

Gateway delivery coalesces snapshots by attachment identity, so a forward
update may omit intermediate phases such as `sending'.  Terminal phases remain
immutable."
  (let ((from-rank (qq-attachment--phase-rank from))
        (to-rank (qq-attachment--phase-rank to)))
    (or (equal from to)
        (and (>= from-rank 0)
             (< from-rank 5)
             (> to-rank from-rank)))))

(defun qq-attachment--same-identity-p (left right)
  "Return non-nil when LEFT and RIGHT describe the same attachment."
  (and (cl-every (lambda (key)
                   (equal (alist-get key left) (alist-get key right)))
                 '(attachment_id resource_id account_id use
                   bytes_total created_at))
       (qq-attachment--conversation-equal-p
        (alist-get 'conversation left)
        (alist-get 'conversation right))))

(defun qq-attachment--upsert (raw-snapshot reason)
  "Merge RAW-SNAPSHOT for REASON without regressing its lifecycle."
  (let* ((snapshot raw-snapshot)
         (attachment-id (alist-get 'attachment_id snapshot))
         (existing (gethash attachment-id
                            qq-attachment--attachments)))
    (when (and existing
               (not (qq-attachment--same-identity-p existing snapshot)))
      (error "qq: Gateway attachment identity changed for %s" attachment-id))
    (cond
     ((null existing)
      (setq qq-attachment--order
            (append qq-attachment--order (list attachment-id))))
     ((< (qq-attachment--phase-rank (alist-get 'phase snapshot))
         (qq-attachment--phase-rank (alist-get 'phase existing)))
      (setq snapshot existing))
     ((not (qq-attachment--transition-p
            (alist-get 'phase existing) (alist-get 'phase snapshot)))
      (error "qq: Gateway attachment lifecycle transition is invalid for %s: %s -> %s"
             attachment-id
             (alist-get 'phase existing)
             (alist-get 'phase snapshot)))
     ((< (alist-get 'updated_at snapshot) (alist-get 'updated_at existing))
      (setq snapshot existing))
     ((and (equal (alist-get 'phase existing) "uploading")
           (equal (alist-get 'phase snapshot) "uploading")
           (qq-protocol-decimal-less-p
            (alist-get 'bytes_done snapshot) (alist-get 'bytes_done existing)))
      (setq snapshot existing))
     ((and (member (alist-get 'phase existing)
                   '("failed" "consumed" "canceled"))
           (not (equal snapshot existing)))
      (error "qq: Gateway attachment terminal snapshot changed")))
    (unless (equal snapshot existing)
      (puthash attachment-id snapshot qq-attachment--attachments)
      (qq-rpc-run-hook
       'qq-attachment-changed-hook reason attachment-id))
    (qq-server-value-copy snapshot)))

(defun qq-attachment-refresh (&optional callback errback reason)
  "Fetch the authoritative Prepared Attachment registry."
  (qq-rpc-latest-call
   'qq-attachment--refresh-owner "attachment.list" nil
   :projector
   (lambda (result)
     (qq-attachment--replace
      (alist-get 'attachments result) (or reason 'resync)))
   :callback callback
   :errback errback))

(defun qq-attachment--conversation-params (session-key)
  "Return target-scoped attachment conversation for SESSION-KEY."
  (let* ((identity (qq-state-session-key-identity session-key))
         (kind (alist-get 'type identity))
         (target (alist-get 'target-id identity)))
    (pcase kind
      ('private `((kind . "private") (peer_uin . ,target)))
      ('group `((kind . "group") (group_uin . ,target)))
      (_ (user-error
          "qq: Prepared attachments support only private or group chats")))))

(defun qq-attachment--prepare
    (session-key resource-id use media-name callback errback)
  "Prepare RESOURCE-ID for SESSION-KEY with USE.

MEDIA-NAME is used only in local errors.  CALLBACK receives the queued
snapshot; later progress is projected through
`qq-attachment-changed-hook'."
  (let* ((account-id (or (qq-runtime-current-account-id)
                         (user-error "qq: select a QQ account first")))
         (resource (qq-resource resource-id))
         (conversation
          (qq-attachment--conversation-params session-key)))
    (unless (and resource (equal (alist-get 'phase resource) "ready"))
      (user-error "qq: %s preparation requires a ready staged resource"
                  media-name))
    (qq-rpc-call
     "attachment.prepare"
     `((account_id . ,account-id)
       (resource_id . ,resource-id)
       (conversation . ,conversation)
       (use . ,use))
     :current-p
     (lambda () (qq-account-get account-id))
     :stale-code "invalid_gateway_result"
     :stale-message "QQ account was removed during attachment preparation"
     :projector
     (lambda (result)
       (qq-runtime-with-account account-id
         (qq-attachment--upsert
          (alist-get 'attachment result) 'prepare-response)))
     :callback
     (and callback
          (lambda (value)
            (qq-runtime-with-account account-id
              (funcall callback value))))
     :errback
     (and errback
          (lambda (body reason)
            (qq-runtime-with-account account-id
              (funcall errback body reason)))))))

(defun qq-attachment-prepare-image
    (session-key resource-id &optional summary sub-type callback errback)
  "Prepare staged RESOURCE-ID as an image for SESSION-KEY.

CALLBACK receives the queued snapshot.  Progress and completion are
projected through `qq-attachment-changed-hook'."
  (qq-attachment--prepare
   session-key resource-id
   `((kind . "image")
     (summary . ,(or summary "[图片]"))
     (sub_type . ,(or sub-type 0)))
   "Image" callback errback))

(defun qq-attachment-prepare-record
    (session-key resource-id &optional callback errback)
  "Prepare staged RESOURCE-ID as native Silk for SESSION-KEY.

The staged resource must already contain message-ready Tencent Silk.  Audio
conversion creates a separate derived resource and is not implicit here.
CALLBACK receives the queued snapshot."
  (qq-attachment--prepare
   session-key resource-id '((kind . "record")) "Record" callback errback))

(defun qq-attachment-prepare-video
    (session-key resource-id thumbnail-resource-id
                 &optional callback errback)
  "Prepare staged RESOURCE-ID as video for SESSION-KEY.

THUMBNAIL-RESOURCE-ID must name a distinct ready staged image.  CALLBACK
receives the queued snapshot; progress and completion are projected through
`qq-attachment-changed-hook'."
  (when (equal resource-id thumbnail-resource-id)
    (user-error "qq: Video body and thumbnail must be distinct resources"))
  (let ((thumbnail (qq-resource thumbnail-resource-id)))
    (unless (and thumbnail
                 (equal (alist-get 'phase thumbnail) "ready"))
      (user-error
       "qq: Video preparation requires a ready staged thumbnail")))
  (qq-attachment--prepare
   session-key resource-id
   `((kind . "video")
     (thumbnail_resource_id . ,thumbnail-resource-id))
   "Video" callback errback))

(defun qq-attachment-status
    (attachment-id &optional callback errback)
  "Fetch ATTACHMENT-ID and merge it into the local projection."
  (unless (qq-attachment--id-p attachment-id)
    (user-error "qq: Attachment ID must be an opaque att- UUID"))
  (qq-rpc-call
   "attachment.status" `((attachment_id . ,attachment-id))
   :projector
   (lambda (result)
     (qq-attachment--upsert
      (alist-get 'attachment result) 'status))
   :callback callback
   :errback errback))

(defun qq-attachment-release
    (attachment-id &optional callback errback)
  "Idempotently release or cancel Prepared ATTACHMENT-ID."
  (unless (qq-attachment--id-p attachment-id)
    (user-error "qq: Attachment ID must be an opaque att- UUID"))
  (qq-rpc-call
   "attachment.release" `((attachment_id . ,attachment-id))
   :callback callback
   :errback errback))

(defun qq-attachment--await
    (attachment-id callback errback)
  "Wait until projected ATTACHMENT-ID becomes ready or terminal.

Return a `qq-account-watch' that removes this local observer without changing
service state."
  (let (observer watch)
    (setq watch
          (qq-request-watch-create
           :active-p t
           :cancel-function
           (lambda ()
             (remove-hook 'qq-attachment-changed-hook observer))))
    (setq observer
          (lambda (_reason changed-id)
            (when (and (qq-request-watch-active-p watch)
                       (or (null changed-id)
                           (equal changed-id attachment-id)))
              (let ((snapshot (qq-attachment attachment-id)))
                (cond
                 ((null snapshot)
                  (qq-request-watch-cancel watch)
                  (qq-rpc-client-error
                   errback "attachment_disappeared"
                   "Prepared attachment disappeared during upload"))
                 ((equal (alist-get 'phase snapshot) "ready")
                  (qq-request-watch-cancel watch)
                  (qq-rpc-invoke callback snapshot))
                 ((member (alist-get 'phase snapshot)
                          '("failed" "consumed" "canceled"))
                  (qq-request-watch-cancel watch)
                  (let ((problem (alist-get 'error snapshot)))
                    (qq-rpc-client-error
                     errback
                     (or (alist-get 'code problem) "attachment_canceled")
                     "%s"
                     (or (alist-get 'message problem)
                         (format "Prepared attachment became %s before use"
                                 (alist-get 'phase snapshot)))))))))))
    (add-hook 'qq-attachment-changed-hook observer)
    (funcall observer 'initial attachment-id)
    watch))

(defun qq-attachment--cancel-local-work (operation)
  "Cancel callback ownership and observers held by OPERATION."
  (when-let* ((request-id
               (qq-attachment-operation-request-id operation)))
    (qq-server-cancel request-id)
    (setf (qq-attachment-operation-request-id operation) nil))
  (when-let* ((watch
               (qq-attachment-operation-resource-watch operation)))
    (qq-request-watch-cancel watch)
    (setf (qq-attachment-operation-resource-watch operation) nil))
  (when-let* ((watch
               (qq-attachment-operation-attachment-watch operation)))
    (qq-request-watch-cancel watch)
    (setf (qq-attachment-operation-attachment-watch operation) nil))
  (when-let* ((process
               (qq-attachment-operation-thumbnail-process operation)))
    (setf (qq-attachment-operation-thumbnail-process operation) nil)
    (when (processp process)
      (set-process-sentinel process #'ignore)
      (when (process-live-p process)
        (delete-process process))))
  (when-let* ((buffer
               (qq-attachment-operation-thumbnail-buffer operation)))
    (setf (qq-attachment-operation-thumbnail-buffer operation) nil)
    (when (buffer-live-p buffer)
      (kill-buffer buffer)))
  (when-let* ((file
               (qq-attachment-operation-thumbnail-file operation)))
    (setf (qq-attachment-operation-thumbnail-file operation) nil)
    (when (file-exists-p file)
      (delete-file file))))

(defun qq-attachment--release-created (operation)
  "Best-effort release service objects created for OPERATION."
  (when-let* ((attachment-id
               (qq-attachment-operation-attachment-id operation)))
    (setf (qq-attachment-operation-attachment-id operation) nil)
    (condition-case nil
        (qq-attachment-release attachment-id)
      (error nil)))
  (let ((resource-ids
         (delete-dups
          (delq nil
                (list
                 (qq-attachment-operation-resource-id operation)
                 (qq-attachment-operation-source-resource-id
                  operation)
                 (qq-attachment-operation-thumbnail-resource-id
                  operation))))))
    (setf (qq-attachment-operation-resource-id operation) nil
          (qq-attachment-operation-source-resource-id operation) nil
          (qq-attachment-operation-thumbnail-resource-id operation) nil)
    (dolist (resource-id resource-ids)
      (condition-case nil
          (qq-resource-release resource-id)
        (error nil)))))

(defun qq-attachment-cancel-operation (operation)
  "Cancel unfinished stage-and-prepare OPERATION idempotently.

This only revokes client callbacks and objects created by this operation.  It
never stops an account or the long-lived service."
  (when (and (qq-attachment-operation-p operation)
             (qq-attachment-operation-active-p operation))
    (setf (qq-attachment-operation-active-p operation) nil)
    (qq-attachment--cancel-local-work operation)
    (qq-attachment--release-created operation)
    t))

(defun qq-attachment--fail-operation
    (operation errback body reason)
  "Settle OPERATION as failed and invoke ERRBACK with BODY and REASON."
  (when (qq-attachment-operation-active-p operation)
    (setf (qq-attachment-operation-active-p operation) nil)
    (qq-attachment--cancel-local-work operation)
    (qq-attachment--release-created operation)
    (qq-rpc-invoke errback body reason)))

(defun qq-attachment--acquire-transform-and-prepare
    (media-name transform-phase acquire transform prepare callback errback)
  "Run ACQUIRE and TRANSFORM, then PREPARE media for one target.

ACQUIRE receives success and failure callbacks and must return a request token.
MEDIA-NAME and TRANSFORM-PHASE describe lifecycle errors.  TRANSFORM receives
the ready source resource plus success and failure callbacks.  PREPARE receives
the final ready resource ID plus success and failure callbacks.  CALLBACK runs
only after the Prepared Attachment reaches `ready'.  Return a cancellable
local operation that owns every service object created before that handoff."
  (let* ((account-id (or (qq-runtime-current-account-id)
                         (user-error "qq: select a QQ account first")))
         (operation
          (qq-attachment-operation-create :active-p t)))
    (cl-labels
        ((fail
          (body reason)
          (qq-attachment--fail-operation
           operation errback body reason))
         (release-source
          ()
          (when-let* ((resource-id
                       (qq-attachment-operation-source-resource-id
                        operation)))
            (setf
             (qq-attachment-operation-source-resource-id operation)
             nil)
            (condition-case nil
                (qq-resource-release resource-id)
              (error nil))))
         (start-request
          (thunk)
          (condition-case error-data
              (let ((request-id (funcall thunk)))
                (when (and request-id
                           (qq-attachment-operation-active-p
                            operation))
                  (setf
                   (qq-attachment-operation-request-id operation)
                   request-id)))
            (error
             (fail nil (error-message-string error-data)))))
         (await-resource
          (resource-id ready-callback)
          (let ((watch
                 (qq-resource-await-ready
                  resource-id ready-callback #'fail)))
            (when (qq-request-watch-active-p watch)
              (setf
               (qq-attachment-operation-resource-watch operation)
               watch))))
         (await-attachment
          (attachment)
          (when (qq-attachment-operation-active-p operation)
            (let ((attachment-id (alist-get 'attachment_id attachment)))
              (setf (qq-attachment-operation-request-id operation) nil
                    (qq-attachment-operation-attachment-id operation)
                    attachment-id)
              (let ((watch
                     (qq-attachment--await
                      attachment-id
                      (lambda (ready)
                        (when
                            (qq-attachment-operation-active-p operation)
                          (if (not (qq-account-get account-id))
                              (fail
                               nil
                               (format
                                "QQ account was removed during %s preparation"
                                media-name))
                            (setf
                             (qq-attachment-operation-active-p operation)
                             nil
                             (qq-attachment-operation-attachment-watch
                              operation)
                             nil)
                            (qq-rpc-invoke callback ready))))
                      #'fail)))
                (when (qq-request-watch-active-p watch)
                  (setf
                   (qq-attachment-operation-attachment-watch operation)
                   watch))))))
         (prepare-final
          (_resource)
          (when (qq-attachment-operation-active-p operation)
            (setf (qq-attachment-operation-resource-watch
                   operation)
                  nil)
            (if (not (qq-account-get account-id))
                (fail
                 nil
                 (format "QQ account was removed during %s"
                         transform-phase))
              (start-request
               (lambda ()
                 (funcall
                  prepare
                  (qq-attachment-operation-resource-id operation)
                  #'await-attachment #'fail))))))
         (transform-complete
          (resource)
          (when (qq-attachment-operation-active-p operation)
            (let* ((resource-id (alist-get 'resource_id resource))
                   (source-id
                    (qq-attachment-operation-source-resource-id
                     operation)))
              (setf (qq-attachment-operation-request-id operation) nil
                    (qq-attachment-operation-resource-id operation)
                    resource-id)
              (when (equal source-id resource-id)
                (setf
                 (qq-attachment-operation-source-resource-id operation)
                 nil))
              (if (not (qq-account-get account-id))
                  (fail
                   nil
                   (format "QQ account was removed during %s"
                           transform-phase))
                ;; A distinct derived resource no longer depends on the staged
                ;; source once the transform request has returned.
                (release-source)
                (await-resource resource-id #'prepare-final)))))
         (source-ready
          (resource)
          (when (qq-attachment-operation-active-p operation)
            (setf (qq-attachment-operation-resource-watch
                   operation)
                  nil)
            (if (not (qq-account-get account-id))
                (fail
                 nil
                 (format "QQ account was removed during %s staging"
                         media-name))
              (start-request
               (lambda ()
                 (funcall transform resource #'transform-complete #'fail))))))
         (stage-complete
          (resource)
          (when (qq-attachment-operation-active-p operation)
            (let* ((resource-id (alist-get 'resource_id resource))
                   (source-id
                    (qq-attachment-operation-source-resource-id
                     operation)))
              (setf (qq-attachment-operation-request-id operation) nil
                    (qq-attachment-operation-source-resource-id
                     operation)
                    resource-id)
              (when (and source-id (not (equal source-id resource-id)))
                (error "qq: Resource stage changed identity within one operation"))
              (await-resource resource-id #'source-ready)))))
      (condition-case error-data
          (let ((request-id (funcall acquire #'stage-complete #'fail)))
            (when (and request-id
                       (qq-attachment-operation-active-p operation))
              (setf (qq-attachment-operation-request-id operation)
                    request-id)))
        ((error quit)
         (setf (qq-attachment-operation-active-p operation) nil)
         (qq-attachment--release-created operation)
         (signal (car error-data) (cdr error-data))))
      operation)))

(defun qq-attachment-stage-and-prepare-image
    (session-key path &optional summary sub-type callback errback)
  "Stage local PATH and prepare one ready image for SESSION-KEY.

CALLBACK runs only after the Prepared Attachment reaches `ready'.  Return a
cancellable local operation; canceling it also best-effort releases every
resource or attachment already created for it."
  (let ((path (expand-file-name path)))
    (qq-attachment--acquire-transform-and-prepare
     "image" "image staging"
     (lambda (success failure)
       (qq-resource-stage-local
        path (file-name-nondirectory path) nil success failure))
     (lambda (resource success _failure)
     (funcall success resource))
     (lambda (resource-id success failure)
       (qq-attachment-prepare-image
        session-key resource-id summary sub-type success failure))
     callback errback)))

(defun qq-attachment-materialize-and-prepare-favorite
    (session-key favorite-emoji-id &optional callback errback)
  "Materialize FAVORITE-EMOJI-ID and prepare it for SESSION-KEY.

The native favorite service publishes verified bytes into the existing
Resource Store.  They then follow the ordinary image preparation lifecycle
with subtype 1.  Return a cancellable `qq-attachment-operation'."
  (qq-attachment--acquire-transform-and-prepare
   "favorite emoji" "favorite-emoji materialization"
   (lambda (success failure)
     (qq-favorite-emoji-materialize
      favorite-emoji-id
      (lambda (materialized)
        (funcall success (alist-get 'resource materialized)))
      failure))
   (lambda (resource success _failure)
     (funcall success resource))
   (lambda (resource-id success failure)
     (qq-attachment-prepare-image
      session-key resource-id "[收藏表情]" 1 success failure))
   callback errback))

(defun qq-attachment-stage-and-prepare-record
    (session-key path &optional callback errback)
  "Stage PCM WAV at PATH and prepare one native record for SESSION-KEY.

The account-neutral source is derived into a distinct Tencent Silk resource.
Once derivation succeeds, the temporary WAV resource is released while the
Silk resource proceeds through target-scoped attachment preparation.  Return a
cancellable operation with the same ownership semantics as the image helper."
  (let ((path (expand-file-name path)))
    (qq-attachment--acquire-transform-and-prepare
     "record" "record derivation"
     (lambda (success failure)
       (qq-resource-stage-local
        path (file-name-nondirectory path) nil success failure))
     (lambda (resource success failure)
     (qq-resource-derive-record
      (alist-get 'resource_id resource) nil success failure))
     (lambda (resource-id success failure)
       (qq-attachment-prepare-record
        session-key resource-id success failure))
     callback errback)))

(defun qq-attachment--video-thumbnail-command (video-path thumbnail-path)
  "Return the ffmpeg command extracting VIDEO-PATH to THUMBNAIL-PATH."
  (let ((ffmpeg (executable-find "ffmpeg")))
    (unless ffmpeg
      (user-error
       "qq: Sending video requires ffmpeg to generate a thumbnail"))
    (list ffmpeg "-nostdin" "-y" "-loglevel" "error"
          "-ss" "0" "-i" video-path "-frames:v" "1"
          "-vf" "scale=640:640:force_original_aspect_ratio=decrease"
          "-q:v" "3" thumbnail-path)))

(defun qq-attachment--extract-video-thumbnail
    (operation video-path thumbnail-path callback errback)
  "Asynchronously extract VIDEO-PATH into THUMBNAIL-PATH for OPERATION.

CALLBACK runs with no arguments after a nonempty image is written.  ERRBACK
receives one human-readable reason.  OPERATION owns the process, diagnostic
buffer, and temporary output, so cancellation remains leak-free."
  (let ((buffer (generate-new-buffer " *qq-video-thumbnail*"))
        process)
    (setf (qq-attachment-operation-thumbnail-buffer operation) buffer)
    (cl-labels
        ((finished
          (candidate _event)
          (unless (process-live-p candidate)
            (when (eq candidate
                      (qq-attachment-operation-thumbnail-process operation))
              (let* ((status (process-exit-status candidate))
                     (diagnostic
                      (and (buffer-live-p buffer)
                           (with-current-buffer buffer
                             (string-trim (buffer-string))))))
                (setf (qq-attachment-operation-thumbnail-process operation) nil
                      (qq-attachment-operation-thumbnail-buffer operation) nil)
                (when (buffer-live-p buffer)
                  (kill-buffer buffer))
                (when (qq-attachment-operation-active-p operation)
                  (if (and (= status 0)
                           (file-regular-p thumbnail-path)
                           (> (file-attribute-size
                               (file-attributes thumbnail-path))
                              0))
                      (funcall callback)
                    (funcall
                     errback
                     (if (string-empty-p (or diagnostic ""))
                         "ffmpeg could not generate a video thumbnail"
                       (format "ffmpeg thumbnail failed: %s"
                               diagnostic))))))))))
      (condition-case error-data
          (progn
            (setq process
                  (make-process
                   :name "qq-video-thumbnail"
                   :buffer buffer
                   :command
                   (qq-attachment--video-thumbnail-command
                    video-path thumbnail-path)
                   :noquery t
                   :sentinel #'ignore))
            (setf (qq-attachment-operation-thumbnail-process operation)
                  process)
            (set-process-sentinel process #'finished)
            ;; A short video may finish before the real sentinel is installed.
            (unless (process-live-p process)
              (finished process "finished\n"))
            process)
        ((error quit)
         (setf (qq-attachment-operation-thumbnail-buffer operation) nil)
         (when (buffer-live-p buffer)
           (kill-buffer buffer))
         (funcall errback (error-message-string error-data))
         nil)))))

(defun qq-attachment-stage-and-prepare-video
    (session-key path &optional callback errback)
  "Stage local video PATH and a generated thumbnail for SESSION-KEY.

Thumbnail extraction uses ffmpeg without a shell.  The video and JPEG are
published as distinct immutable resources and retained until the target-bound
Prepared Attachment reaches `ready'.  CALLBACK receives that ready snapshot.
Return a cancellable `qq-attachment-operation'."
  (let* ((path (expand-file-name path))
         (account-id (or (qq-runtime-current-account-id)
                         (user-error "qq: select a QQ account first")))
         (attributes (file-attributes path))
         (_mp4
          (unless (equal (downcase (or (file-name-extension path) ""))
                         "mp4")
            (user-error
             "qq: Native video sending currently requires an MP4 file")))
         (thumbnail-file (make-temp-file "qq-video-thumbnail-" nil ".jpg"))
         (operation
          (qq-attachment-operation-create
           :active-p t :thumbnail-file thumbnail-file)))
    (unless (and attributes (file-regular-p path) (file-readable-p path))
      (qq-attachment--cancel-local-work operation)
      (user-error "qq: Video source is not a readable regular file: %s" path))
    (cl-labels
        ((account-current-p
          ()
          (qq-account-get account-id))
         (fail
          (body reason)
          (qq-attachment--fail-operation operation errback body reason))
         (start-request
          (thunk)
          (when (and (qq-attachment-operation-active-p operation)
                     (account-current-p))
            (let ((marker (list 'video-request)) token)
              (setf (qq-attachment-operation-request-id operation) marker)
              (condition-case error-data
                  (setq token (funcall thunk))
                ((error quit)
                 (when (eq marker
                           (qq-attachment-operation-request-id operation))
                   (setf (qq-attachment-operation-request-id operation) nil))
                 (fail nil (error-message-string error-data))))
              (if (eq marker
                      (qq-attachment-operation-request-id operation))
                  (setf (qq-attachment-operation-request-id operation) token)
                ;; A synchronous completion advanced or settled the chain.
                (when token
                  (qq-server-cancel token)))
              token)))
         (await-resource
          (resource-id ready-callback)
          (let ((watch
                 (qq-resource-await-ready
                  resource-id ready-callback #'fail)))
            (when (and (qq-request-watch-active-p watch)
                       (qq-attachment-operation-active-p operation))
              (setf (qq-attachment-operation-resource-watch operation)
                    watch))))
         (attachment-ready
          (attachment)
          (when (qq-attachment-operation-active-p operation)
            (let ((attachment-id (alist-get 'attachment_id attachment)))
              (setf (qq-attachment-operation-request-id operation) nil
                    (qq-attachment-operation-attachment-id operation)
                    attachment-id)
              (let ((watch
                     (qq-attachment--await
                      attachment-id
                      (lambda (ready)
                        (when (qq-attachment-operation-active-p operation)
                          (setf
                           (qq-attachment-operation-active-p operation) nil
                           (qq-attachment-operation-attachment-watch operation)
                           nil)
                          (qq-rpc-invoke callback ready)))
                      #'fail)))
                (when (qq-request-watch-active-p watch)
                  (setf
                   (qq-attachment-operation-attachment-watch operation)
                   watch))))))
         (thumbnail-ready
          (resource)
          (when (qq-attachment-operation-active-p operation)
            (setf (qq-attachment-operation-resource-watch operation) nil
                  (qq-attachment-operation-thumbnail-resource-id operation)
                  (alist-get 'resource_id resource))
            ;; Resource Store is now authoritative for the generated bytes.
            (when-let* ((file
                         (qq-attachment-operation-thumbnail-file operation)))
              (setf (qq-attachment-operation-thumbnail-file operation) nil)
              (when (file-exists-p file)
                (delete-file file)))
            (if (not (account-current-p))
                (fail nil "QQ account was removed during video staging")
              (start-request
               (lambda ()
                 (qq-attachment-prepare-video
                  session-key
                  (qq-attachment-operation-resource-id operation)
                  (qq-attachment-operation-thumbnail-resource-id operation)
                  #'attachment-ready #'fail))))))
         (thumbnail-staged
          (resource)
          (when (qq-attachment-operation-active-p operation)
            (setf (qq-attachment-operation-request-id operation) nil
                  (qq-attachment-operation-thumbnail-resource-id operation)
                  (alist-get 'resource_id resource))
            (await-resource
             (alist-get 'resource_id resource) #'thumbnail-ready)))
         (video-ready
          (resource)
          (when (qq-attachment-operation-active-p operation)
            (setf (qq-attachment-operation-resource-watch operation) nil
                  (qq-attachment-operation-resource-id operation)
                  (alist-get 'resource_id resource))
            (if (not (account-current-p))
                (fail nil "QQ account was removed during video staging")
              (start-request
               (lambda ()
                 (qq-resource-stage-local
                  thumbnail-file
                  (file-name-nondirectory thumbnail-file) nil
                  #'thumbnail-staged #'fail))))))
         (video-staged
          (resource)
          (when (qq-attachment-operation-active-p operation)
            (setf (qq-attachment-operation-request-id operation) nil
                  (qq-attachment-operation-resource-id operation)
                  (alist-get 'resource_id resource))
            (await-resource (alist-get 'resource_id resource) #'video-ready))))
      (qq-attachment--extract-video-thumbnail
       operation path thumbnail-file
       (lambda ()
         (if (not (account-current-p))
             (fail nil "QQ account was removed during thumbnail extraction")
           (start-request
            (lambda ()
              (qq-resource-stage-local
               path (file-name-nondirectory path) nil
               #'video-staged #'fail)))))
       (lambda (reason) (fail nil reason)))
      operation)))

(defun qq-attachment--conversation-equal-p (left right)
  "Return non-nil when LEFT and RIGHT name the same private/group target.

Compare by kind + target id rather than raw alist `equal', so key order from
JSON projection cannot false-fail a sendable check."
  (let ((left-kind (alist-get 'kind left))
        (right-kind (alist-get 'kind right)))
    (and (equal left-kind right-kind)
         (pcase left-kind
           ("private"
            (equal (format "%s" (or (alist-get 'peer_uin left) ""))
                   (format "%s" (or (alist-get 'peer_uin right) ""))))
           ("group"
            (equal (format "%s" (or (alist-get 'group_uin left) ""))
                   (format "%s" (or (alist-get 'group_uin right) ""))))
           (_ nil)))))

(defun qq-attachment-assert-sendable
    (attachment-id session-key account-id &optional expected-use)
  "Return ATTACHMENT-ID after strict SESSION-KEY and ACCOUNT-ID checks.

ACCOUNT-ID is a stable account-slot identity.  EXPECTED-USE defaults to image
and may be \"record\" or \"video\"."
  (unless (qq-attachment--id-p attachment-id)
    (user-error "qq: Media segment lacks an opaque attachment ID"))
  (unless (qq-protocol-non-empty-string-p account-id)
    (user-error "qq: Prepared attachment requires a stable account ID"))
  (let ((expected-use (or expected-use "image"))
        (snapshot (qq-attachment attachment-id))
        (conversation
         (qq-attachment--conversation-params session-key)))
    (unless (member expected-use '("image" "record" "video"))
      (error "qq: Unknown prepared attachment use %S" expected-use))
    (unless snapshot
      (user-error "qq: Prepared attachment %s is not projected" attachment-id))
    (unless (equal (alist-get 'phase snapshot) "ready")
      (user-error "qq: Prepared attachment %s is not ready" attachment-id))
    (unless (equal (alist-get 'account_id snapshot) account-id)
      (user-error "qq: Prepared attachment belongs to another account"))
    (unless (qq-attachment--conversation-equal-p
             (alist-get 'conversation snapshot) conversation)
      (user-error "qq: Prepared attachment belongs to another conversation"))
    (unless (equal (alist-get 'kind (alist-get 'use snapshot)) expected-use)
      (user-error "qq: Prepared attachment is not %s" expected-use))
    attachment-id))

(defun qq-attachment--request-resync (reason)
  "Request one authoritative attachment refresh for REASON."
  (qq-rpc-request-single-flight
   'qq-attachment--resync-request-id 'attachment-resync
   (lambda (success failure)
     (qq-attachment-refresh success failure reason))
   "attachment"))

(defun qq-attachment--handle-ready (instance-id)
  "Synchronize attachments after Gateway ready INSTANCE-ID."
  (unless (qq-protocol-non-empty-string-p instance-id)
    (error "qq: Gateway ready instance identity is malformed"))
  (unless (equal instance-id qq-attachment--gateway-instance-id)
    (qq-attachment--clear 'gateway-changed))
  (setq qq-attachment--gateway-instance-id
        (qq-server-value-copy instance-id))
  (if (qq-rpc-method-available-p "attachment.list")
      (qq-attachment--request-resync 'ready)
    (qq-attachment--clear 'capability-unavailable)))

(defun qq-attachment--handle-event (event data)
  "Project native service attachment EVENT with DATA."
  (pcase event
    ("attachment.changed"
     (qq-attachment--upsert
      (alist-get 'attachment data) 'changed))
    (_ (error "qq: Unowned Gateway attachment event %s" event))))

(defun qq-attachment--handle-projection-resync (projection body)
  "Resynchronize after runtime PROJECTION events were lost with BODY."
  (when (equal projection "attachments")
    (qq-rpc-run-hook
     'qq-attachment-desync-hook
     body)
    (qq-attachment--request-resync 'resync)))

(add-hook 'qq-account-registry-ready-hook #'qq-attachment--handle-ready)
(qq-rpc-register-event
 "attachment.changed" #'qq-attachment--handle-event)
(add-hook 'qq-account-projection-resync-hook
          #'qq-attachment--handle-projection-resync)

(provide 'qq-attachment)

;;; qq-attachment.el ends here
