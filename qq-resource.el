;;; qq-resource.el --- QQ service resource projection -*- lexical-binding: t; -*-

;; Author: 0WD0 <me@0wd0.com>

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

(defvar qq-resource-access-invalidated-hook nil
  "Hook called with RESOURCE-ID and REASON when its local consumers expire.")

(defvar qq-resource--access-pool (make-hash-table :test #'equal))
(defvar qq-resource--access-generation 0)

(cl-defstruct (qq-resource--access-entry
               (:constructor qq-resource--access-entry-create))
  resource-id generation grant refs pending timer)

(cl-defstruct (qq-resource--access-ref
               (:constructor qq-resource--access-ref-create))
  entry active-p callback errback)

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
  (qq-resource-access-reset)
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

(defun qq-resource-acquire-url (url &optional suggested-name callback errback)
  "Acquire public HTTP(S) URL as a Gateway-owned resource.
CALLBACK receives a snapshot; use `qq-resource-await-ready' before access."
  (unless (and (stringp url) (string-match-p "\\`https?://" url))
    (user-error "qq: Resource URL must use HTTP or HTTPS"))
  (when (and suggested-name (not (qq-resource--safe-name-p suggested-name)))
    (user-error "qq: URL resource name must be a safe basename"))
  (let ((generation qq-resource--access-generation))
    (qq-rpc-call
     "resource.acquire_url"
     `((url . ,url) (suggested_name . ,suggested-name))
     :current-p (lambda () (= generation qq-resource--access-generation))
     :projector (lambda (result)
                  (qq-resource--upsert
                   (alist-get 'resource result) 'acquire-url-response))
     :callback callback :errback errback)))

(defun qq-resource-derive-preview
    (source-resource-id kind &optional callback errback)
  "Derive Gateway preview KIND from SOURCE-RESOURCE-ID.
CALLBACK receives a distinct resource snapshot, possibly still staging."
  (unless (qq-resource-id-p source-resource-id)
    (user-error "qq: Preview source ID must be an opaque res- identity"))
  (unless (member kind '("animation" "video"))
    (user-error "qq: Unknown resource preview kind %S" kind))
  (let ((generation qq-resource--access-generation))
    (qq-rpc-call
     "resource.derive_preview"
     `((source_resource_id . ,source-resource-id) (kind . ,kind))
     :current-p (lambda () (= generation qq-resource--access-generation))
     :projector
     (lambda (result)
       (let ((snapshot (alist-get 'resource result)))
         (when (equal (alist-get 'resource_id snapshot) source-resource-id)
           (error "qq: Gateway preview reused its source identity"))
         (qq-resource--upsert snapshot 'derive-preview-response)))
     :callback callback :errback errback)))

(defun qq-resource--check-local-access
    (access expected-resource-id)
  "Check filesystem authority in local ACCESS for EXPECTED-RESOURCE-ID."
  (unless (and (qq-server-wire-exact-object-keys-p
                access '(access_id resource_id path expires_at))
               (qq-resource--local-access-id-p (alist-get 'access_id access))
               (qq-resource-id-p (alist-get 'resource_id access))
               (integerp (alist-get 'expires_at access))
               (> (alist-get 'expires_at access) 0))
    (error "qq: Gateway local resource access is malformed"))
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
  "Open an isolated local grant for ready RESOURCE-ID.

CALLBACK receives `access_id', `resource_id', `path', and epoch-second
`expires_at'.  Paths never enter resource snapshots.  Automatic media
consumers should use `qq-resource-acquire-access' instead of this raw API."
  (unless (qq-resource-id-p resource-id)
    (user-error "qq: Resource ID must be an opaque res- identity"))
  (qq-rpc-call
   "resource.open_local" `((resource_id . ,resource-id))
   :projector
   (lambda (result)
     (let ((access (alist-get 'access result)))
       (condition-case error-data
           (qq-resource--check-local-access access resource-id)
         (error
          (qq-resource--access-close-grant access)
          (signal (car error-data) (cdr error-data))))))
   :callback callback
   :errback errback))

(defun qq-resource-renew-local (access-id &optional callback errback)
  "Renew ACCESS-ID and return its grant with epoch-second expiry to CALLBACK."
  (unless (qq-resource--local-access-id-p access-id)
    (user-error "qq: Local resource access ID must be an access- UUID"))
  (qq-rpc-call
   "resource.renew_local" `((access_id . ,access-id))
   :projector
   (lambda (result)
     (let ((access (alist-get 'access result)))
       (condition-case error-data
           (progn
             (qq-resource--check-local-access
              access (alist-get 'resource_id access))
             (unless (equal (alist-get 'access_id access) access-id)
               (error "qq: Gateway renewed a different access identity"))
             access)
         (error
          (qq-resource--access-close-grant access)
          (signal (car error-data) (cdr error-data))))))
   :callback callback :errback errback))

(defun qq-resource-close-local
    (access-id &optional callback errback)
  "Idempotently revoke local resource ACCESS-ID."
  (unless (qq-resource--local-access-id-p access-id)
    (user-error "qq: Local resource access ID must be an access- UUID"))
  (qq-rpc-call
   "resource.close_local" `((access_id . ,access-id))
   :callback callback
   :errback errback))

(defun qq-resource--access-close-grant (grant)
  "Close GRANT when the transport still permits best-effort cleanup."
  (let ((access-id (alist-get 'access_id grant)))
    (when (and (qq-resource--local-access-id-p access-id)
               (qq-server-ready-p))
      (condition-case nil
          (qq-resource-close-local access-id nil #'ignore)
        (error nil)))))

(defun qq-resource--access-current-p (entry)
  "Return non-nil while ENTRY owns this generation's referenced resource."
  (and (= (qq-resource--access-entry-generation entry)
          qq-resource--access-generation)
       (eq entry (gethash (qq-resource--access-entry-resource-id entry)
                          qq-resource--access-pool))
       (qq-resource--access-entry-refs entry)))

(defun qq-resource--access-cancel-timer (entry)
  "Cancel ENTRY's renewal or expiry timer."
  (when-let* ((timer (qq-resource--access-entry-timer entry)))
    (cancel-timer timer)
    (setf (qq-resource--access-entry-timer entry) nil)))

(defun qq-resource--access-retire (entry &optional reason body failure)
  "Retire ENTRY, invalidating its references for REASON when non-nil."
  (let ((resource-id (qq-resource--access-entry-resource-id entry))
        (grant (qq-resource--access-entry-grant entry))
        (refs (qq-resource--access-entry-refs entry)))
    (when (eq entry (gethash resource-id qq-resource--access-pool))
      (remhash resource-id qq-resource--access-pool))
    (qq-resource--access-cancel-timer entry)
    (setf (qq-resource--access-entry-refs entry) nil
          (qq-resource--access-entry-grant entry) nil
          (qq-resource--access-entry-pending entry) nil)
    ;; Leave in-flight RPC observations alive so a late grant can be closed.
    (dolist (ref refs)
      (let ((errback (qq-resource--access-ref-errback ref)))
        (setf (qq-resource--access-ref-active-p ref) nil
              (qq-resource--access-ref-entry ref) nil
              (qq-resource--access-ref-callback ref) nil
              (qq-resource--access-ref-errback ref) nil)
        (when reason
          (qq-rpc-invoke
           errback
           (or body `((code . "resource_access_invalidated")
                      (message . ,(format "Local access invalidated: %s" reason))))
           (or failure (format "Local access invalidated: %s" reason))))))
    (qq-resource--access-close-grant grant)
    (when reason
      (qq-rpc-run-hook
       'qq-resource-access-invalidated-hook resource-id reason))))

(defun qq-resource-access-reset (&optional reason)
  "Invalidate all references and advance the local transport generation.
REASON defaults to `reset'.  No canonical resource is released."
  (let ((entries qq-resource--access-pool))
    (setq qq-resource--access-pool (make-hash-table :test #'equal))
    (cl-incf qq-resource--access-generation)
    (maphash (lambda (_resource-id entry)
               (qq-resource--access-retire entry (or reason 'reset)))
             entries)))

(defun qq-resource-access-release (ref)
  "Idempotently detach this independent consumer REF, never another consumer."
  (when (and (qq-resource--access-ref-p ref)
             (qq-resource--access-ref-active-p ref))
    (let ((entry (qq-resource--access-ref-entry ref)))
      (setf (qq-resource--access-ref-active-p ref) nil
            (qq-resource--access-ref-entry ref) nil
            (qq-resource--access-ref-callback ref) nil
            (qq-resource--access-ref-errback ref) nil
            (qq-resource--access-entry-refs entry)
            (delq ref (qq-resource--access-entry-refs entry)))
      (unless (qq-resource--access-entry-refs entry)
        (qq-resource--access-retire entry))
      t)))

(defun qq-resource-access-live-p (ref)
  "Return non-nil for REF's current generation, including a pending open.
An expired grant invalidates all its references even after Emacs suspension."
  (when (and (qq-resource--access-ref-p ref)
             (qq-resource--access-ref-active-p ref))
    (let* ((entry (qq-resource--access-ref-entry ref))
           (grant (qq-resource--access-entry-grant entry)))
      (when (qq-resource--access-current-p entry)
        (if (and grant (<= (alist-get 'expires_at grant) (float-time)))
            (progn (qq-resource--access-retire entry 'expired) nil)
          t)))))

(defun qq-resource-access-grant (ref)
  "Return REF's valid grant or nil, checking epoch-second expiry on every use."
  (when (qq-resource-access-live-p ref)
    (qq-server-value-copy
     (qq-resource--access-entry-grant (qq-resource--access-ref-entry ref)))))

(defun qq-resource--access-deliver (ref)
  "Deliver the current grant to REF once without copying its opaque identity."
  (when-let* ((grant (qq-resource-access-grant ref)))
    (let ((callback (qq-resource--access-ref-callback ref)))
      (setf (qq-resource--access-ref-callback ref) nil
            (qq-resource--access-ref-errback ref) nil)
      (when callback
        (condition-case error-data
            (funcall callback grant ref)
          ((error quit)
           (qq-resource-access-release ref)
           (message "qq: Local access callback failed: %s"
                    (error-message-string error-data))))))))

(defun qq-resource--access-schedule (entry &optional expiry-only)
  "Schedule ENTRY before expiry, or at expiry while a renewal is outstanding."
  (qq-resource--access-cancel-timer entry)
  (let* ((remaining (- (alist-get 'expires_at
                                 (qq-resource--access-entry-grant entry))
                       (float-time)))
         (delay (if expiry-only remaining
                  (- remaining (min 60.0 (/ remaining 2.0))))))
    (setf (qq-resource--access-entry-timer entry)
          (run-at-time
           (max 0 delay) nil
           (lambda ()
             (setf (qq-resource--access-entry-timer entry) nil)
             (when (qq-resource--access-current-p entry)
               (if (<= (alist-get 'expires_at
                                 (qq-resource--access-entry-grant entry))
                       (float-time))
                   (qq-resource--access-retire entry 'expired)
                 (qq-resource--access-request entry t))))))))

(defun qq-resource--access-request (entry &optional renew)
  "Start ENTRY's single shared open or RENEW request."
  (unless (qq-resource--access-entry-pending entry)
    (let ((marker (list 'access-request))
          (previous (qq-resource--access-entry-grant entry)))
      (setf (qq-resource--access-entry-pending entry) marker)
      (when renew (qq-resource--access-schedule entry t))
      (cl-labels
          ((current-p ()
             (and (qq-resource--access-current-p entry)
                  (eq marker (qq-resource--access-entry-pending entry))))
           (fail (body reason)
             (when (current-p)
               (qq-resource--access-retire
                entry (if renew 'renew-failed 'open-failed) body reason)))
           (accept (grant)
             (if (not (current-p))
                 (qq-resource--access-close-grant grant)
               (condition-case error-data
                   (progn
                     (qq-resource--check-local-access
                      grant (qq-resource--access-entry-resource-id entry))
                     (unless (> (alist-get 'expires_at grant) (float-time))
                       (error "qq: Gateway returned an expired local grant"))
                     (when renew
                       (unless (and
                                (> (alist-get 'expires_at previous) (float-time))
                                (>= (alist-get 'expires_at grant)
                                   (alist-get 'expires_at previous))
                                (cl-every
                                 (lambda (key)
                                   (equal (alist-get key grant)
                                          (alist-get key previous)))
                                 '(access_id resource_id path)))
                         (error "qq: Gateway renewal changed or expired its grant")))
                     (setf (qq-resource--access-entry-grant entry)
                           (qq-server-value-copy grant)
                           (qq-resource--access-entry-pending entry) nil)
                     (qq-resource--access-schedule entry)
                     (dolist (ref (copy-sequence
                                  (qq-resource--access-entry-refs entry)))
                       (qq-resource--access-deliver ref)))
                 (error
                  (qq-resource--access-close-grant grant)
                  (fail nil (error-message-string error-data)))))))
        (condition-case error-data
            (if renew
                (qq-resource-renew-local
                 (alist-get 'access_id previous) #'accept #'fail)
              (qq-resource-open-local
               (qq-resource--access-entry-resource-id entry) #'accept #'fail))
          (quit
           (let ((inhibit-quit t))
             (fail nil "Local resource access canceled"))
           (signal (car error-data) (cdr error-data)))
          (error (fail nil (error-message-string error-data))))))))

(defun qq-resource-acquire-access (resource-id callback errback)
  "Return an independent opaque reference to ready RESOURCE-ID immediately.
CALLBACK receives (ACCESS REF), possibly synchronously.  ERRBACK receives
(BODY REASON).  Release REF with `qq-resource-access-release'; never close
the shared ACCESS directly.  Expiry invalidates all outstanding references."
  (unless (qq-resource-id-p resource-id)
    (user-error "qq: Resource ID must be an opaque res- identity"))
  (let ((entry (gethash resource-id qq-resource--access-pool)))
    (when (and entry (qq-resource--access-entry-grant entry)
               (<= (alist-get 'expires_at (qq-resource--access-entry-grant entry))
                   (float-time)))
      (qq-resource--access-retire entry 'expired)
      ;; An invalidation observer may already have acquired its replacement.
      (setq entry (gethash resource-id qq-resource--access-pool)))
    (unless entry
      (setq entry (qq-resource--access-entry-create
                   :resource-id resource-id
                   :generation qq-resource--access-generation))
      (puthash resource-id entry qq-resource--access-pool))
    (let ((ref (qq-resource--access-ref-create
                :entry entry :active-p t :callback callback :errback errback)))
      (push ref (qq-resource--access-entry-refs entry))
      (if (qq-resource--access-entry-grant entry)
          (qq-resource--access-deliver ref)
        (qq-resource--access-request entry))
      ref)))

(defun qq-resource-access-retain (ref callback errback)
  "Return an independent reference to REF's resource, or report invalidation."
  (if (qq-resource-access-live-p ref)
      (qq-resource-acquire-access
       (qq-resource--access-entry-resource-id (qq-resource--access-ref-entry ref))
       callback errback)
    (qq-rpc-client-error errback "resource_access_invalidated"
                         "Local access reference is no longer valid")))

(defun qq-resource--access-handle-state (state)
  "Revoke local authority whenever transport leaves readiness for STATE."
  (unless (eq state 'ready)
    (qq-resource-access-reset 'transport-boundary)))

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
  (qq-resource-access-reset 'transport-boundary)
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
(add-hook 'qq-server-state-hook #'qq-resource--access-handle-state)
(dolist (event '("resource.changed" "resource.removed"))
  (qq-rpc-register-event
   event #'qq-resource--handle-event))
(add-hook 'qq-account-projection-resync-hook
          #'qq-resource--handle-projection-resync)

(provide 'qq-resource)

;;; qq-resource.el ends here
