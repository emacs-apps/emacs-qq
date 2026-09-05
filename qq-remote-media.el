;;; qq-remote-media.el --- QQ remote-media projection -*- lexical-binding: t; -*-

;; Author: 0WD0 <me@0wd0.com>

;;; Commentary:

;; Remote QQ media is neither a staged resource nor a Prepared Attachment.
;; This module projects opaque `media-*' handles and owns the inbound record
;; materialize -> derive PCM WAV -> local access pipeline.  Signed URLs,
;; native file UUIDs, and canonical blob paths never enter Emacs.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'qq-rpc)
(require 'qq-account)
(require 'qq-resource)
(require 'qq-request)
(require 'qq-runtime)

(defvar qq-remote-media-changed-hook nil
  "Hook called with REASON and MEDIA-ID after remote-media state changes.")

(defvar qq-remote-media-desync-hook nil
  "Hook called with an unsolicited remote-media stream error body.")

(defvar qq-remote-media--media (make-hash-table :test #'equal))
(defvar qq-remote-media--order nil)
(defvar qq-remote-media--gateway-instance-id nil)
(defvar qq-remote-media--refresh-owner nil
  "Identity of the newest authoritative remote-media registry refresh.")
(defvar qq-remote-media--resync-request-id nil
  "Identity of the in-flight automatic remote-media registry resync.")
(defvar qq-remote-media--playable-resources (make-hash-table :test #'equal)
  "Ready or staging PCM WAV resource ID cached for each remote media ID.")

(cl-defstruct (qq-remote-media-operation
               (:constructor qq-remote-media-operation-create))
  "One cancellable remote-media materialization pipeline."
  active-p
  media-id
  part
  account-id
  request-id
  watch
  source-resource-id
  playback-resource-id)

(defun qq-remote-media--id-p (value)
  "Return non-nil when VALUE is an opaque remote-media identity."
  (and (stringp value)
       (string-match-p
        "\\`media-[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}\\'"
        value)))

(defun qq-remote-media (media-id)
  "Return a copy of remote MEDIA-ID, or nil."
  (qq-server-value-copy
   (and media-id (gethash media-id qq-remote-media--media))))

(defun qq-remote-media--part-name (part)
  "Return PART's Gateway wire name.

PART is one of the symbols `content' and `thumbnail'."
  (unless (memq part '(content thumbnail))
    (error "qq: Unknown remote-media part %S" part))
  (symbol-name part))

(defun qq-remote-media-part (media part)
  "Return PART snapshot from remote MEDIA, or nil when unavailable."
  (alist-get part media))

(defun qq-remote-media-list ()
  "Return copied remote-media snapshots in authoritative order."
  (delq nil (mapcar #'qq-remote-media qq-remote-media--order)))

(defun qq-remote-media--clear (reason)
  "Clear projected remote media for REASON."
  (let ((changed (or qq-remote-media--order
                     (> (hash-table-count qq-remote-media--media) 0)))
        (resync-marker qq-remote-media--resync-request-id))
    (qq-rpc-cancel-latest
     'qq-remote-media--refresh-owner "superseded_request"
     "Gateway media refresh context was cleared")
    (when (eq resync-marker qq-remote-media--resync-request-id)
      (setq qq-remote-media--resync-request-id nil))
    (setq qq-remote-media--media (make-hash-table :test #'equal)
          qq-remote-media--order nil
          qq-remote-media--playable-resources (make-hash-table :test #'equal))
    (when changed
      (qq-rpc-run-hook 'qq-remote-media-changed-hook reason nil))))

(defun qq-remote-media-reset ()
  "Forget client remote-media projection without mutating service state."
  (setq qq-remote-media--gateway-instance-id nil)
  (qq-remote-media--clear 'reset))

(defun qq-remote-media--same-identity-p (left right)
  "Return non-nil when LEFT and RIGHT identify the same remote media."
  (cl-every (lambda (key)
              (equal (alist-get key left) (alist-get key right)))
            '(media_id account_id sequence segment_index kind created_at)))

(defun qq-remote-media--merge-message-id (existing snapshot)
  "Merge monotone exact-message evidence from EXISTING into SNAPSHOT."
  (let ((existing-id (alist-get 'message_id existing))
        (incoming-id (alist-get 'message_id snapshot)))
    (when (and existing-id incoming-id (not (equal existing-id incoming-id)))
      (error "qq: Gateway remote-media exact message identity changed"))
    (if (and existing-id (null incoming-id))
        (let ((merged (copy-tree snapshot)))
          (setf (alist-get 'message_id merged) existing-id)
          merged)
      snapshot)))

(defun qq-remote-media--upsert (snapshot reason)
  "Merge SNAPSHOT into the remote-media projection for REASON."
  (let* ((media-id (alist-get 'media_id snapshot))
         (existing (gethash media-id qq-remote-media--media)))
    (when existing
      (unless (qq-remote-media--same-identity-p existing snapshot)
        (error "qq: Gateway remote-media identity changed for %s" media-id))
      (setq snapshot (qq-remote-media--merge-message-id existing snapshot)))
    (when (and existing
               (< (alist-get 'updated_at snapshot)
                  (alist-get 'updated_at existing)))
      (setq snapshot existing))
    (unless existing
      (setq qq-remote-media--order
            (append qq-remote-media--order (list media-id))))
    (unless (equal snapshot existing)
      (puthash media-id snapshot qq-remote-media--media)
      (qq-rpc-run-hook 'qq-remote-media-changed-hook reason media-id))
    (qq-server-value-copy snapshot)))

(defun qq-remote-media--replace (snapshots reason)
  "Atomically replace projected media with SNAPSHOTS for REASON."
  (let ((next (make-hash-table :test #'equal))
        order)
    (dolist (snapshot snapshots)
      (let ((media-id (alist-get 'media_id snapshot)))
        (when (gethash media-id next)
          (error "qq: Gateway media list duplicates %s" media-id))
        (puthash media-id snapshot next)
        (push media-id order)))
    (setq qq-remote-media--media next
          qq-remote-media--order (nreverse order))
    (qq-rpc-run-hook 'qq-remote-media-changed-hook reason nil)
    (qq-remote-media-list)))

(defun qq-remote-media--remove (media-id reason)
  "Remove opaque MEDIA-ID from projection for REASON."
  (when (gethash media-id qq-remote-media--media)
    (remhash media-id qq-remote-media--media)
    (remhash media-id qq-remote-media--playable-resources)
    (setq qq-remote-media--order (delete media-id qq-remote-media--order))
    (qq-rpc-run-hook 'qq-remote-media-changed-hook reason media-id)
    t))

(defun qq-remote-media-refresh (&optional callback errback reason)
  "Fetch authoritative remote-media registry.

CALLBACK receives copied snapshots; ERRBACK receives a response and reason.
REASON defaults to `resync'."
  (qq-rpc-latest-call
   'qq-remote-media--refresh-owner "media.list" nil
   :projector
   (lambda (result)
     (qq-remote-media--replace
      (alist-get 'media result) (or reason 'resync)))
   :callback callback
   :errback errback))

(defun qq-remote-media-status (media-id &optional callback errback)
  "Fetch and project remote MEDIA-ID.

CALLBACK receives the projected snapshot; ERRBACK receives failure details."
  (unless (qq-remote-media--id-p media-id)
    (user-error "qq: Media ID must be an opaque media- UUID"))
  (qq-rpc-call
   "media.status" `((media_id . ,media-id))
   :projector
   (lambda (result)
     (qq-remote-media--upsert (alist-get 'media result) 'status))
   :callback callback
   :errback errback))

(defun qq-remote-media-materialize-part
    (media-id part &optional callback errback)
  "Start materializing PART of remote MEDIA-ID without waiting.

CALLBACK receives the current media snapshot, normally in `materializing'
phase.  Progress and completion arrive through `media.changed'.  ERRBACK
receives failure details."
  (unless (qq-remote-media--id-p media-id)
    (user-error "qq: Media ID must be an opaque media- UUID"))
  (qq-rpc-call
   "media.materialize"
   `((media_id . ,media-id)
     (part . ,(qq-remote-media--part-name part)))
   :projector
   (lambda (result)
     (qq-remote-media--upsert
      (alist-get 'media result) 'materialize-response))
   :callback callback
   :errback errback))

(defun qq-remote-media-materialize (media-id &optional callback errback)
  "Start materializing remote MEDIA-ID content.

Call CALLBACK with its current snapshot; call ERRBACK on failure."
  (qq-remote-media-materialize-part
   media-id 'content callback errback))

(defun qq-remote-media-cancel-part (media-id part &optional callback errback)
  "Idempotently cancel active PART materialization of remote MEDIA-ID.

Completed media stays materialized.  An active operation returns to
`available', preserving the reusable remote handle."
  (unless (qq-remote-media--id-p media-id)
    (user-error "qq: Media ID must be an opaque media- UUID"))
  (qq-rpc-call
   "media.cancel"
   `((media_id . ,media-id)
     (part . ,(qq-remote-media--part-name part)))
   :projector
   (lambda (result)
     (qq-remote-media--upsert
      (alist-get 'media result) 'cancel-response))
   :callback callback
   :errback errback))

(defun qq-remote-media-cancel (media-id &optional callback errback)
  "Idempotently cancel remote MEDIA-ID content materialization.

Call CALLBACK with its current snapshot; call ERRBACK on failure."
  (qq-remote-media-cancel-part media-id 'content callback errback))

(defun qq-remote-media-await-part-materialized
    (media-id part callback errback)
  "Observe MEDIA-ID until PART materializes or reaches a terminal state.

Return a `qq-account-watch' that removes only this local observer.  CALLBACK
receives the materialized media snapshot; ERRBACK receives terminal failure."
  (unless (qq-remote-media--id-p media-id)
    (user-error "qq: Media ID must be an opaque media- UUID"))
  (let (observer watch)
    (setq watch
          (qq-request-watch-create
           :active-p t
           :cancel-function
           (lambda ()
             (remove-hook 'qq-remote-media-changed-hook observer))))
    (setq observer
          (lambda (_reason changed-id)
            (when (and (qq-request-watch-active-p watch)
                       (or (null changed-id) (equal changed-id media-id)))
              (let ((media (qq-remote-media media-id)))
                (cond
                 ((null media)
                  (qq-request-watch-cancel watch)
                  (qq-rpc-client-error
                   errback "media_disappeared" "Remote media disappeared"))
                 ((null (qq-remote-media-part media part))
                  (qq-request-watch-cancel watch)
                  (qq-rpc-client-error
                   errback "media_part_unavailable"
                   "Remote media part is unavailable"))
                 ((equal
                   (alist-get 'phase (qq-remote-media-part media part))
                   "materialized")
                  (qq-request-watch-cancel watch)
                  (qq-rpc-invoke callback media))
                 ((equal
                   (alist-get 'phase (qq-remote-media-part media part))
                   "failed")
                  (qq-request-watch-cancel watch)
                  (let ((problem
                         (alist-get
                          'error (qq-remote-media-part media part))))
                    (qq-rpc-client-error
                     errback
                     (or (alist-get 'code problem) "media_materialize_failed")
                     "%s"
                     (or (alist-get 'message problem)
                         "Remote media materialization failed"))))
                 ((equal
                   (alist-get 'phase (qq-remote-media-part media part))
                   "available")
                  (qq-request-watch-cancel watch)
                  (qq-rpc-client-error
                   errback "media_materialize_canceled"
                   "Remote media materialization was canceled")))))))
    (add-hook 'qq-remote-media-changed-hook observer)
    (funcall observer 'initial media-id)
    watch))

(defun qq-remote-media-await-materialized (media-id callback errback)
  "Observe MEDIA-ID content until it reaches a terminal state.

Call CALLBACK on materialization or ERRBACK on another terminal outcome."
  (qq-remote-media-await-part-materialized
   media-id 'content callback errback))

(defun qq-remote-media-release (media-id &optional callback errback)
  "Idempotently release remote MEDIA-ID without releasing its resources.

CALLBACK receives the release receipt; ERRBACK receives failure details."
  (unless (qq-remote-media--id-p media-id)
    (user-error "qq: Media ID must be an opaque media- UUID"))
  (qq-rpc-call
   "media.release" `((media_id . ,media-id))
   :callback callback
   :errback errback))

(defun qq-remote-media--cancel-operation-local (operation)
  "Cancel local request and observer ownership held by OPERATION."
  (when-let* ((request-id (qq-remote-media-operation-request-id operation)))
    (qq-server-cancel request-id)
    (setf (qq-remote-media-operation-request-id operation) nil))
  (when-let* ((watch (qq-remote-media-operation-watch operation)))
    (qq-request-watch-cancel watch)
    (setf (qq-remote-media-operation-watch operation) nil)))

(defun qq-remote-media-cancel-operation (operation)
  "Cancel playback-preparation OPERATION locally and in the service."
  (when (and (qq-remote-media-operation-p operation)
             (qq-remote-media-operation-active-p operation))
    (setf (qq-remote-media-operation-active-p operation) nil)
    (qq-remote-media--cancel-operation-local operation)
    (when (qq-rpc-method-available-p "media.cancel")
      (condition-case error-data
          (qq-remote-media-cancel-part
           (qq-remote-media-operation-media-id operation)
           (or (qq-remote-media-operation-part operation) 'content)
           nil
           (lambda (_body failure)
             (message "qq: Media cancellation failed: %s" failure)))
        (error
         (message "qq: Media cancellation failed: %s"
                  (error-message-string error-data)))))
    t))

(defun qq-remote-media--account-current-p (operation)
  "Return non-nil when OPERATION's stable account still exists."
  (and (qq-account-get
        (qq-remote-media-operation-account-id operation))
       t))

(defun qq-remote-media-prepare-part-local-access
    (media-id part callback &optional errback)
  "Materialize PART of remote MEDIA-ID and open a local access lease.

CALLBACK receives an alist containing `media_id', `resource_id', and `access'.
The callback owns the access lease and must close its `access_id' after
copying or consuming the isolated file.  ERRBACK follows the Gateway failure
convention.  Return a cancellable `qq-remote-media-operation'."
  (unless (qq-remote-media--id-p media-id)
    (user-error "qq: Media ID must be an opaque media- UUID"))
  (let* ((account-id (qq-runtime-current-account-id))
         (operation
          (qq-remote-media-operation-create
           :active-p t :media-id media-id :part part
           :account-id account-id)))
    (unless account-id
      (user-error "qq: Select an account before materializing remote media"))
    (cl-labels
        ((fail
          (body reason)
          (when (qq-remote-media-operation-active-p operation)
            (setf (qq-remote-media-operation-active-p operation) nil)
            (qq-remote-media--cancel-operation-local operation)
            (qq-rpc-invoke errback body reason)))
         (ensure-account
          ()
          (if (qq-remote-media--account-current-p operation)
              t
            (fail
             '((code . "account_removed")
               (message . "QQ account was removed during media materialization"))
             "QQ account was removed during media materialization")
            nil))
         (start-request
          (thunk)
          (when (and (qq-remote-media-operation-active-p operation)
                     (ensure-account))
            (condition-case error-data
                (let ((request-id (funcall thunk)))
                  (when (and request-id
                             (qq-remote-media-operation-active-p operation))
                    (setf
                     (qq-remote-media-operation-request-id operation)
                     request-id)))
              (error (fail nil (error-message-string error-data))))))
         (handoff-access
          (access)
          (if (not (and (qq-remote-media-operation-active-p operation)
                        (ensure-account)))
              (qq-resource-close-local (alist-get 'access_id access))
            (setf (qq-remote-media-operation-active-p operation) nil
                  (qq-remote-media-operation-request-id operation) nil)
            (let ((result
                   `((media_id . ,media-id)
                     (resource_id
                      . ,(qq-remote-media-operation-source-resource-id
                          operation))
                     (access . ,access))))
              (if (not callback)
                  (qq-resource-close-local (alist-get 'access_id access))
                (condition-case error-data
                    (funcall callback result)
                  (error
                   (qq-resource-close-local
                    (alist-get 'access_id access))
                   (message "qq: media access callback failed: %s"
                            (error-message-string error-data))))))))
         (resource-ready
          (resource)
          (when (and (qq-remote-media-operation-active-p operation)
                     (ensure-account))
            (setf (qq-remote-media-operation-watch operation) nil)
            (start-request
             (lambda ()
               (qq-resource-open-local
                (alist-get 'resource_id resource)
                #'handoff-access #'fail)))))
         (resource-status
          (resource)
          (when (qq-remote-media-operation-active-p operation)
            (setf (qq-remote-media-operation-request-id operation) nil
                  (qq-remote-media-operation-source-resource-id operation)
                  (alist-get 'resource_id resource))
            (let ((watch
                   (qq-resource-await-ready
                    (alist-get 'resource_id resource)
                    #'resource-ready #'fail)))
              (when (qq-request-watch-active-p watch)
                (setf (qq-remote-media-operation-watch operation) watch)))))
         (materialized
          (media)
          (when (qq-remote-media-operation-active-p operation)
            (setf (qq-remote-media-operation-watch operation) nil)
            (let* ((owner (alist-get 'account_id media))
                   (part-snapshot (qq-remote-media-part media part))
                   (resource-id
                    (and part-snapshot
                         (alist-get 'resource_id part-snapshot))))
              (if (not (and (ensure-account)
                            (equal owner account-id)
                            resource-id))
                  (when (qq-remote-media-operation-active-p operation)
                    (fail nil
                          "Remote media part is unavailable or belongs to another managed account"))
                (start-request
                 (lambda ()
                   (qq-resource-status
                    resource-id #'resource-status #'fail)))))))
         (await-media
          ()
          (let ((watch
                 (qq-remote-media-await-part-materialized
                  media-id part #'materialized #'fail)))
            (when (qq-request-watch-active-p watch)
              (setf (qq-remote-media-operation-watch operation) watch))))
         (materialize-started
          (_media)
          (when (qq-remote-media-operation-active-p operation)
            (setf (qq-remote-media-operation-request-id operation) nil)
            (await-media))))
      (start-request
       (lambda ()
         (qq-remote-media-materialize-part
          media-id part #'materialize-started #'fail)))
      operation)))

(defun qq-remote-media-prepare-local-access
    (media-id callback &optional errback)
  "Materialize remote MEDIA-ID content and open a local access lease.

Call CALLBACK with the access result; call ERRBACK on failure."
  (qq-remote-media-prepare-part-local-access
   media-id 'content callback errback))

(defun qq-remote-media-prepare-record-playback
    (media-id callback &optional errback)
  "Prepare local PCM WAV access for remote record MEDIA-ID.

The service materializes native Silk, derives a separate playback WAV, then
issues a revocable local access grant.  CALLBACK receives an alist containing
`media_id', `source_resource_id', `playback_resource_id', and `access'.
ERRBACK receives a response body and human-readable failure reason.
Return a cancellable `qq-remote-media-operation'."
  (unless (qq-remote-media--id-p media-id)
    (user-error "qq: Record media ID must be an opaque media- UUID"))
  (let* ((account-id (qq-runtime-current-account-id))
         (operation
          (qq-remote-media-operation-create
           :active-p t :media-id media-id :part 'content
           :account-id account-id)))
    (unless account-id
      (user-error "qq: Select an account before playing a record"))
    (cl-labels
        ((fail
          (body reason)
          (when (qq-remote-media-operation-active-p operation)
            (setf (qq-remote-media-operation-active-p operation) nil)
            (qq-remote-media--cancel-operation-local operation)
            (qq-rpc-invoke errback body reason)))
         (ensure-account
          ()
          (if (qq-remote-media--account-current-p operation)
              t
            (fail '((code . "account_removed")
                    (message . "QQ account was removed during record playback preparation"))
                  "QQ account was removed during record playback preparation")
            nil))
         (start-request
          (thunk)
          (when (and (qq-remote-media-operation-active-p operation)
                     (ensure-account))
            (condition-case error-data
                (let ((request-id (funcall thunk)))
                  (when (and request-id
                             (qq-remote-media-operation-active-p operation))
                    (setf
                     (qq-remote-media-operation-request-id operation)
                     request-id)))
              (error (fail nil (error-message-string error-data))))))
         (await-resource
          (resource-id ready-callback)
          (let ((watch
                 (qq-resource-await-ready
                  resource-id ready-callback #'fail)))
            (when (qq-request-watch-active-p watch)
              (setf (qq-remote-media-operation-watch operation) watch))))
         (handoff-access
          (access)
          (if (not (and (qq-remote-media-operation-active-p operation)
                        (ensure-account)))
              (qq-resource-close-local (alist-get 'access_id access))
            (setf (qq-remote-media-operation-active-p operation) nil
                  (qq-remote-media-operation-request-id operation) nil)
            (let ((result
                   `((media_id . ,media-id)
                     (source_resource_id
                      . ,(qq-remote-media-operation-source-resource-id operation))
                     (playback_resource_id
                      . ,(qq-remote-media-operation-playback-resource-id operation))
                     (access . ,access))))
              (if (not callback)
                  (qq-resource-close-local
                   (alist-get 'access_id access))
                (condition-case error-data
                    (funcall callback result)
                  (error
                   (qq-resource-close-local
                    (alist-get 'access_id access))
                   (message "qq: record playback callback failed: %s"
                            (error-message-string error-data))))))))
         (open-playable
          (resource)
          (when (and (qq-remote-media-operation-active-p operation)
                     (ensure-account))
            (setf (qq-remote-media-operation-watch operation) nil
                  (qq-remote-media-operation-playback-resource-id operation)
                  (alist-get 'resource_id resource))
            (puthash media-id (alist-get 'resource_id resource)
                     qq-remote-media--playable-resources)
            (start-request
             (lambda ()
               (qq-resource-open-local
                (alist-get 'resource_id resource) #'handoff-access #'fail)))))
         (await-playable
          (resource)
          (let ((resource-id (alist-get 'resource_id resource)))
            (setf (qq-remote-media-operation-request-id operation) nil
                  (qq-remote-media-operation-playback-resource-id operation)
                  resource-id)
            (puthash media-id resource-id qq-remote-media--playable-resources)
            (await-resource resource-id #'open-playable)))
         (derive-playable
         (source)
          (start-request
           (lambda ()
             (qq-resource-derive-playable-record
              (alist-get 'resource_id source) nil #'await-playable #'fail))))
         (source-ready
          (source)
          (when (and (qq-remote-media-operation-active-p operation)
                     (ensure-account))
            (setf (qq-remote-media-operation-watch operation) nil)
            (let* ((cached-id (gethash media-id
                                       qq-remote-media--playable-resources))
                   (cached (and cached-id (qq-resource cached-id))))
              (pcase (and cached (alist-get 'phase cached))
                ("ready" (open-playable cached))
                ("staging" (await-resource cached-id #'open-playable))
                (_
                 (remhash media-id qq-remote-media--playable-resources)
                 (derive-playable source))))))
         (source-status
          (resource)
          (when (qq-remote-media-operation-active-p operation)
            (setf (qq-remote-media-operation-request-id operation) nil
                  (qq-remote-media-operation-source-resource-id operation)
                  (alist-get 'resource_id resource))
            (await-resource (alist-get 'resource_id resource) #'source-ready)))
         (materialized
         (media)
          (when (qq-remote-media-operation-active-p operation)
            (setf (qq-remote-media-operation-watch operation) nil)
            (let* ((account-id (alist-get 'account_id media))
                   (content (qq-remote-media-part media 'content))
                   (resource-id
                    (and content (alist-get 'resource_id content))))
              (if (not (and (ensure-account)
                            (equal account-id
                                   (qq-remote-media-operation-account-id
                                    operation))
                            resource-id))
                  (when (qq-remote-media-operation-active-p operation)
                    (fail nil "Remote record belongs to another managed account"))
                (start-request
                 (lambda ()
                   (qq-resource-status
                    resource-id #'source-status #'fail)))))))
         (await-media
          ()
          (let ((watch
                 (qq-remote-media-await-materialized
                  media-id #'materialized #'fail)))
            (when (qq-request-watch-active-p watch)
              (setf (qq-remote-media-operation-watch operation) watch))))
         (materialize-started
          (_media)
          (when (qq-remote-media-operation-active-p operation)
            (setf (qq-remote-media-operation-request-id operation) nil)
            (await-media))))
      (start-request
       (lambda ()
         (qq-remote-media-materialize
          media-id #'materialize-started #'fail)))
      operation)))

(defun qq-remote-media--drop-stale-playable-resources (_reason resource-id)
  "Forget cached playable mappings affected by RESOURCE-ID."
  (let (stale-media-ids)
    (maphash
     (lambda (media-id cached-id)
       (when (and (or (null resource-id) (equal resource-id cached-id))
                  (let ((resource (qq-resource cached-id)))
                    (or (null resource)
                        (member (alist-get 'phase resource)
                                '("failed" "released")))))
         (push media-id stale-media-ids)))
     qq-remote-media--playable-resources)
    (dolist (media-id stale-media-ids)
      (remhash media-id qq-remote-media--playable-resources))))

(defun qq-remote-media--request-resync (reason)
  "Request one authoritative remote-media refresh for REASON."
  (qq-rpc-request-single-flight
   'qq-remote-media--resync-request-id 'media-resync
   (lambda (success failure)
     (qq-remote-media-refresh success failure reason))
   "media"))

(defun qq-remote-media--handle-ready (instance-id)
  "Synchronize remote media after Gateway ready INSTANCE-ID."
  (unless (equal instance-id qq-remote-media--gateway-instance-id)
    (qq-remote-media--clear 'gateway-changed))
  (setq qq-remote-media--gateway-instance-id
        (qq-server-value-copy instance-id))
  (if (qq-rpc-method-available-p "media.list")
      (qq-remote-media--request-resync 'ready)
    (qq-remote-media--clear 'capability-unavailable)))

(defun qq-remote-media--handle-event (event data)
  "Project native remote-media EVENT with DATA."
  (pcase event
    ("media.changed"
     (qq-remote-media--upsert (alist-get 'media data) 'changed))
    ("media.removed"
     (qq-remote-media--remove (alist-get 'media_id data) 'removed))
    (_ (error "qq: Unowned Gateway media event %s" event))))

(defun qq-remote-media--handle-projection-resync (projection body)
  "Resynchronize after runtime PROJECTION events were lost with BODY."
  (when (equal projection "remote_media")
    (qq-rpc-run-hook 'qq-remote-media-desync-hook body)
    (qq-remote-media--request-resync 'resync)))

(add-hook 'qq-resource-changed-hook
          #'qq-remote-media--drop-stale-playable-resources)
(add-hook 'qq-account-registry-ready-hook #'qq-remote-media--handle-ready)
(dolist (event '("media.changed" "media.removed"))
  (qq-rpc-register-event event #'qq-remote-media--handle-event))
(add-hook 'qq-account-projection-resync-hook
          #'qq-remote-media--handle-projection-resync)

(provide 'qq-remote-media)

;;; qq-remote-media.el ends here
