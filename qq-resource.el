;;; qq-resource.el --- QQ service resource projection -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; Client-side projection for Gateway-owned immutable staged resources.
;; Local paths are accepted only by the explicit staging command and are never
;; retained in resource snapshots.  Resource identities remain account-neutral
;; and survive websocket reconnects; `resource.list' is the resync boundary.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'qq-rpc)
(require 'qq-account)
(require 'qq-request)
(require 'qq-server)

(defvar qq-resource-changed-hook nil
  "Hook called with REASON and RESOURCE-ID after resource projection changes.")

(defvar qq-resource-desync-hook nil
  "Hook called with an unsolicited resource stream error body.")

(defvar qq-resource--resources (make-hash-table :test #'equal))
(defvar qq-resource--order nil)
(defvar qq-resource--gateway-instance-id nil)
(defvar qq-resource--refresh-owner nil
  "Identity of the newest authoritative resource registry refresh.")
(defvar qq-resource--resync-request-id nil
  "Identity of the in-flight automatic resource registry resync.")

(defun qq-resource-id-p (value)
  "Return non-nil when VALUE is an opaque staged-resource identity."
  (and (stringp value)
       (string-prefix-p "res-" value)
       (> (length value) 4)
       (not (string-match-p "[[:space:][:cntrl:]]" value))))

(defun qq-resource--local-access-id-p (value)
  "Return non-nil when VALUE is an opaque local-access identity."
  (and (stringp value)
       (string-match-p
        "\\`access-[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}\\'"
        value)))

(defun qq-resource--prefixed-digest-id-p (value prefix)
  "Return non-nil when VALUE is PREFIX followed by one SHA-256-shaped token."
  (and (stringp value)
       (string-prefix-p prefix value)
       (qq-resource--hex-digest-p
        (substring value (length prefix)) 64)))

(defun qq-resource--import-source-id-p (value)
  "Return non-nil when VALUE is an opaque LinuxQQ import-source identity."
  (qq-resource--prefixed-digest-id-p value "src-linuxqq-"))

(defun qq-resource--import-candidate-id-p (value)
  "Return non-nil when VALUE is an opaque LinuxQQ import-candidate identity."
  (qq-resource--prefixed-digest-id-p value "imp-linuxqq-"))

(defun qq-resource--hex-digest-p (value length)
  "Return non-nil when VALUE is a lowercase hexadecimal digest of LENGTH."
  (and (stringp value)
       (= (length value) length)
       (string-match-p (format "\\`[0-9a-f]\\{%d\\}\\'" length) value)))

(defun qq-resource--safe-name-p (value)
  "Return non-nil when VALUE is one safe staged-resource display basename."
  (and (qq-protocol-non-empty-string-p value)
       (<= (string-bytes value) 255)
       (not (member value '("." "..")))
       (not (string-match-p "[/\\[:cntrl:]]" value))))

(defun qq-resource (resource-id)
  "Return a copy of staged RESOURCE-ID, or nil."
  (qq-server-value-copy
   (and resource-id
        (gethash resource-id qq-resource--resources))))

(defun qq-resources ()
  "Return copied staged-resource snapshots in authoritative order."
  (delq nil (mapcar #'qq-resource qq-resource--order)))

(defun qq-resource--clear (reason)
  "Clear the projected registry for REASON."
  (let ((changed (or qq-resource--order
                     (> (hash-table-count qq-resource--resources) 0)))
        (resync-marker qq-resource--resync-request-id))
    (qq-rpc-cancel-latest
     'qq-resource--refresh-owner "superseded_request"
     "Gateway resource refresh context was cleared")
    (when (eq resync-marker qq-resource--resync-request-id)
      (setq qq-resource--resync-request-id nil))
    (setq qq-resource--resources (make-hash-table :test #'equal)
          qq-resource--order nil)
    (when changed
      (qq-rpc-run-hook 'qq-resource-changed-hook reason nil))))

(defun qq-resource-reset ()
  "Forget the client resource projection without mutating service state."
  (setq qq-resource--gateway-instance-id nil)
  (qq-resource--clear 'reset))

(defun qq-resource--replace (snapshots reason)
  "Atomically replace resources with SNAPSHOTS for REASON."
  (let ((next (make-hash-table :test #'equal))
        order)
    (dolist (snapshot snapshots)
      (let ((resource-id (alist-get 'resource_id snapshot)))
        (when (gethash resource-id next)
          (error "qq: Gateway resource list duplicates %s" resource-id))
        (puthash resource-id snapshot next)
        (push resource-id order)))
    (setq qq-resource--resources next
          qq-resource--order (nreverse order))
    (qq-rpc-run-hook 'qq-resource-changed-hook reason nil)
    (qq-resources)))

(defun qq-resource--phase-rank (phase)
  "Return monotonic lifecycle rank for resource PHASE."
  (pcase phase
    ("staging" 0)
    ((or "ready" "failed") 1)
    ("released" 2)
    (_ -1)))

(defun qq-resource--same-identity-p (left right)
  "Return non-nil when LEFT and RIGHT describe the same immutable resource."
  (cl-every (lambda (key)
              (equal (alist-get key left) (alist-get key right)))
            '(resource_id suggested_name size created_at)))

(defun qq-resource--upsert (raw-snapshot reason)
  "Merge RAW-SNAPSHOT for REASON without regressing its lifecycle."
  (let* ((snapshot raw-snapshot)
         (resource-id (alist-get 'resource_id snapshot))
         (existing (gethash resource-id qq-resource--resources)))
    (when (and existing
               (not (qq-resource--same-identity-p existing snapshot)))
      (error "qq: Gateway resource identity changed for %s" resource-id))
    (let ((incoming-rank (qq-resource--phase-rank
                          (alist-get 'phase snapshot)))
          (existing-rank (and existing
                              (qq-resource--phase-rank
                               (alist-get 'phase existing)))))
      (cond
       ((null existing)
        (setq qq-resource--order
              (append qq-resource--order (list resource-id))))
       ((< incoming-rank existing-rank)
        (setq snapshot existing))
       ((and (= incoming-rank existing-rank)
             (not (equal (alist-get 'phase snapshot)
                         (alist-get 'phase existing))))
        (error "qq: Gateway resource changed between terminal phases"))
       ((< (alist-get 'updated_at snapshot)
           (alist-get 'updated_at existing))
        (setq snapshot existing))
       ((and (= incoming-rank existing-rank)
             (not (equal snapshot existing)))
        (error "qq: Gateway resource terminal snapshot changed"))
       ((and (> incoming-rank existing-rank)
             (not (or (equal (alist-get 'phase snapshot) "released")
                      (and (equal (alist-get 'phase existing) "staging")
                           (member (alist-get 'phase snapshot)
                                   '("ready" "failed"))))))
        (error "qq: Gateway resource lifecycle transition is invalid")))
      (unless (equal snapshot existing)
        (puthash resource-id snapshot qq-resource--resources)
        (qq-rpc-run-hook
         'qq-resource-changed-hook reason resource-id))
      (qq-server-value-copy snapshot))))

(defun qq-resource--remove (resource-id reason)
  "Remove opaque RESOURCE-ID from the projection for REASON."
  (when (gethash resource-id qq-resource--resources)
    (remhash resource-id qq-resource--resources)
    (setq qq-resource--order
          (delete resource-id qq-resource--order))
    (qq-rpc-run-hook
     'qq-resource-changed-hook reason resource-id)
    t))

(defun qq-resource-refresh (&optional callback errback reason)
  "Fetch the authoritative staged-resource registry.

CALLBACK receives copied snapshots.  ERRBACK follows the Gateway transport
error convention.  REASON defaults to `resync'."
  (qq-rpc-latest-call
   'qq-resource--refresh-owner "resource.list" nil
   :projector
   (lambda (result)
     (qq-resource--replace
      (alist-get 'resources result) (or reason 'resync)))
   :callback callback
   :errback errback))

(defun qq-resource-stage-local
    (path &optional suggested-name expected-sha256 callback errback)
  "Copy local PATH into Gateway-owned immutable resource storage.

SUGGESTED-NAME is an optional safe display basename.  EXPECTED-SHA256, when
non-nil, must be a lowercase hexadecimal digest.  CALLBACK receives the
staging snapshot; ERRBACK follows the Gateway convention."
  (let* ((path (expand-file-name path))
         ;; Numeric uid/gid avoids potentially blocking NSS name lookups; this
         ;; operation needs only the regular-file bit and byte size.
         (attributes (file-attributes path)))
    (unless (and attributes (file-regular-p path))
      (user-error "qq: Resource source is not a regular file: %s" path))
    (when (and suggested-name
               (not (qq-resource--safe-name-p suggested-name)))
      (user-error "qq: Resource suggested name must be a safe basename"))
    (when (and expected-sha256
               (not (qq-resource--hex-digest-p expected-sha256 64)))
      (user-error "qq: Expected SHA-256 must be lowercase hexadecimal"))
    (qq-rpc-call
     "resource.stage_local"
     `((path . ,path)
       (suggested_name . ,suggested-name)
       (expected
        . ((size . ,(number-to-string (file-attribute-size attributes)))
           (sha256 . ,expected-sha256))))
     :projector
     (lambda (result)
       (qq-resource--upsert
        (alist-get 'resource result) 'stage-response))
     :callback callback
     :errback errback)))

(defun qq-resource-derive-record
    (source-resource-id &optional suggested-name callback errback)
  "Derive message-ready Tencent Silk from SOURCE-RESOURCE-ID.

The source must be a ready mono PCM WAV accepted by the native service.
SUGGESTED-NAME, when non-nil, names the distinct derived resource.  CALLBACK
receives its staging snapshot; source and result retain independent
lifecycle and release operations."
  (unless (qq-resource-id-p source-resource-id)
    (user-error "qq: Record source ID must be an opaque res- identity"))
  (when (and suggested-name
             (not (qq-resource--safe-name-p suggested-name)))
    (user-error "qq: Derived record name must be a safe basename"))
  (qq-rpc-call
   "resource.derive_record"
   `((source_resource_id . ,source-resource-id)
     (suggested_name . ,suggested-name))
   :projector
   (lambda (result)
     (let ((snapshot (alist-get 'resource result)))
       (when (equal (alist-get 'resource_id snapshot) source-resource-id)
         (error "qq: Gateway record derivation reused its source identity"))
       (qq-resource--upsert snapshot 'derive-record-response)))
   :callback callback
   :errback errback))

(defun qq-resource-derive-playable-record
    (source-resource-id &optional suggested-name callback errback)
  "Derive portable PCM WAV playback from native SOURCE-RESOURCE-ID.

The source must be a ready QQ/Tencent Silk resource accepted by the native
service.  SUGGESTED-NAME, when non-nil, names the distinct derived resource.
CALLBACK receives its staging snapshot; source and result retain
independent lifecycle and release operations."
  (unless (qq-resource-id-p source-resource-id)
    (user-error "qq: Native record source ID must be an opaque res- identity"))
  (when (and suggested-name
             (not (qq-resource--safe-name-p suggested-name)))
    (user-error "qq: Playable record name must be a safe basename"))
  (qq-rpc-call
   "resource.derive_playable_record"
   `((source_resource_id . ,source-resource-id)
     (suggested_name . ,suggested-name))
   :projector
   (lambda (result)
     (let ((snapshot (alist-get 'resource result)))
       (when (equal (alist-get 'resource_id snapshot) source-resource-id)
         (error "qq: Gateway playable record derivation reused its source identity"))
       (qq-resource--upsert
        snapshot 'derive-playable-record-response)))
   :callback callback
   :errback errback))

(defun qq-resource--check-local-access
    (access expected-resource-id)
  "Check filesystem authority in local ACCESS for EXPECTED-RESOURCE-ID."
  (unless (equal (alist-get 'resource_id access) expected-resource-id)
    (error "qq: Gateway local resource access contradicts its resource"))
  (let ((path (alist-get 'path access)))
    (unless (and (stringp path)
                 (file-name-absolute-p path)
                 (file-regular-p path))
      (error "qq: Gateway local resource access path is not a regular absolute file")))
  access)

(defun qq-resource-open-local
    (resource-id &optional callback errback)
  "Lease a short-lived isolated local copy of ready RESOURCE-ID.

CALLBACK receives an access grant containing `access_id', `resource_id',
`path', and `expires_at'.  The path is deliberately absent from ordinary
resource snapshots and must later be revoked with
`qq-resource-close-local'."
  (unless (qq-resource-id-p resource-id)
    (user-error "qq: Resource ID must be an opaque res- identity"))
  (qq-rpc-call
   "resource.open_local" `((resource_id . ,resource-id))
   :projector
   (lambda (result)
     (qq-resource--check-local-access
      (alist-get 'access result) resource-id))
   :callback callback
   :errback errback))

(defun qq-resource-close-local
    (access-id &optional callback errback)
  "Idempotently revoke local resource ACCESS-ID."
  (unless (qq-resource--local-access-id-p access-id)
    (user-error "qq: Local resource access ID must be an access- UUID"))
  (qq-rpc-call
   "resource.close_local" `((access_id . ,access-id))
   :callback callback
   :errback errback))

(defun qq-resource-await-ready (resource-id callback errback)
  "Wait until projected RESOURCE-ID becomes ready or terminal.

Return a `qq-account-watch' that detaches this local observer without changing
service state.  CALLBACK receives the ready snapshot."
  (unless (qq-resource-id-p resource-id)
    (user-error "qq: Resource ID must be an opaque res- identity"))
  (let (observer watch)
    (setq watch
          (qq-request-watch-create
           :active-p t
           :cancel-function
           (lambda ()
             (remove-hook 'qq-resource-changed-hook observer))))
    (setq observer
          (lambda (_reason changed-id)
            (when (and (qq-request-watch-active-p watch)
                       (or (null changed-id) (equal changed-id resource-id)))
              (let ((resource (qq-resource resource-id)))
                (cond
                 ((null resource)
                  (qq-request-watch-cancel watch)
                  (qq-rpc-client-error
                   errback "resource_disappeared"
                   "Staged resource disappeared"))
                 ((equal (alist-get 'phase resource) "ready")
                  (qq-request-watch-cancel watch)
                  (qq-rpc-invoke callback resource))
                 ((member (alist-get 'phase resource) '("failed" "released"))
                  (qq-request-watch-cancel watch)
                  (let ((problem (alist-get 'error resource)))
                    (qq-rpc-client-error
                     errback
                     (or (alist-get 'code problem) "resource_released")
                     "%s"
                     (or (alist-get 'message problem)
                         "Staged resource was released")))))))))
    (add-hook 'qq-resource-changed-hook observer)
    (funcall observer 'initial resource-id)
    watch))

(defun qq-resource--check-unique (items key context)
  "Return ITEMS after asserting unique KEY values for CONTEXT."
  (let (seen)
    (dolist (item items)
      (let ((value (alist-get key item)))
        (when (member value seen)
          (error "qq: Gateway %s contains duplicate %s" context key))
        (push value seen))))
  items)

(defun qq-resource-import-sources (&optional callback errback)
  "List configured read-only native-cache sources.

CALLBACK receives pathless source snapshots."
  (qq-rpc-call
   "resource.import.list_sources" nil
   :projector
   (lambda (result)
     (qq-resource--check-unique
      (alist-get 'sources result) 'source_id "native-cache sources"))
   :callback callback
   :errback errback))

(defun qq-resource-import-images
    (source-id &optional after limit callback errback)
  "List one page of image candidates from native SOURCE-ID.

AFTER is an opaque cursor returned by the previous page.  LIMIT defaults to
100 and must be between 1 and 1000.  CALLBACK receives the domain page."
  (unless (qq-resource--import-source-id-p source-id)
    (user-error "qq: Native-cache source ID is malformed"))
  (when (and after
             (not (qq-resource--import-candidate-id-p after)))
    (user-error "qq: Native-cache image cursor is malformed"))
  (setq limit (or limit 100))
  (unless (and (integerp limit) (<= 1 limit 1000))
    (user-error "qq: Native-cache image page size must be between 1 and 1000"))
  (qq-rpc-call
   "resource.import.list_images"
   `((source_id . ,source-id) (after . ,after) (limit . ,limit))
   :projector
   (lambda (result)
     (qq-resource--check-unique
      (alist-get 'candidates result) 'candidate_id "native image page")
     result)
   :callback callback
   :errback errback))

(defun qq-resource-import-image
    (source-id candidate-id &optional callback errback)
  "Stage native image CANDIDATE-ID from SOURCE-ID into Resource Store."
  (unless (qq-resource--import-source-id-p source-id)
    (user-error "qq: Native-cache source ID is malformed"))
  (unless (qq-resource--import-candidate-id-p candidate-id)
    (user-error "qq: Native-cache image candidate ID is malformed"))
  (qq-rpc-call
   "resource.import.stage_image"
   `((source_id . ,source-id) (candidate_id . ,candidate-id))
   :projector
   (lambda (result)
     (qq-resource--upsert
      (alist-get 'resource result) 'native-import-response))
   :callback callback
   :errback errback))

(defun qq-resource-status
    (resource-id &optional callback errback)
  "Fetch RESOURCE-ID and merge it into the local projection."
  (unless (qq-resource-id-p resource-id)
    (user-error "qq: Resource ID must be an opaque res- identity"))
  (qq-rpc-call
   "resource.status" `((resource_id . ,resource-id))
   :projector
   (lambda (result)
     (qq-resource--upsert
      (alist-get 'resource result) 'status))
   :callback callback
   :errback errback))

(defun qq-resource-release
    (resource-id &optional callback errback)
  "Idempotently release staged RESOURCE-ID."
  (unless (qq-resource-id-p resource-id)
    (user-error "qq: Resource ID must be an opaque res- identity"))
  (qq-rpc-call
   "resource.release" `((resource_id . ,resource-id))
   :callback callback
   :errback errback))

(defun qq-resource--request-resync (reason)
  "Request one authoritative resource refresh for REASON."
  (qq-rpc-request-single-flight
   'qq-resource--resync-request-id 'resource-resync
   (lambda (success failure)
     (qq-resource-refresh success failure reason))
   "resource"))

(defun qq-resource--handle-ready (instance-id)
  "Synchronize resources after Gateway ready INSTANCE-ID."
  (unless (qq-protocol-non-empty-string-p instance-id)
    (error "qq: Gateway ready instance identity is malformed"))
  (unless (equal instance-id qq-resource--gateway-instance-id)
    (qq-resource--clear 'gateway-changed))
  (setq qq-resource--gateway-instance-id
        (qq-server-value-copy instance-id))
  (if (qq-rpc-method-available-p "resource.list")
      (qq-resource--request-resync 'ready)
    (qq-resource--clear 'capability-unavailable)))

(defun qq-resource--handle-event (event data)
  "Project native service resource EVENT with DATA."
  (pcase event
    ("resource.changed"
     (qq-resource--upsert
      (alist-get 'resource data) 'changed))
    ("resource.removed"
     (qq-resource--remove
      (alist-get 'resource_id data) 'removed))
    (_ (error "qq: Unowned Gateway resource event %s" event))))

(defun qq-resource--handle-projection-resync (projection body)
  "Resynchronize after runtime PROJECTION events were lost with BODY."
  (when (equal projection "resources")
    (qq-rpc-run-hook
     'qq-resource-desync-hook
     body)
    (qq-resource--request-resync 'resync)))

(add-hook 'qq-account-registry-ready-hook #'qq-resource--handle-ready)
(dolist (event '("resource.changed" "resource.removed"))
  (qq-rpc-register-event
   event #'qq-resource--handle-event))
(add-hook 'qq-account-projection-resync-hook
          #'qq-resource--handle-projection-resync)

(provide 'qq-resource)

;;; qq-resource.el ends here
