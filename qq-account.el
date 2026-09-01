;;; qq-account.el --- QQ account lifecycle client -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; Client-side projection of nt-gateway's long-lived multi-account registry.
;; The selected account is local Emacs UI state; all managed accounts continue
;; running independently when the websocket or Emacs process disconnects.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'qq-rpc)
(require 'qq-server)
(require 'qq-protocol)

(declare-function qq-runtime-call-with-account
                  "qq-runtime" (account-id function))
(declare-function qq-state-session-key
                  "qq-state" (type target-id &optional variant))
(declare-function qq-state-upsert-session
                  "qq-state" (session-key fields &optional message))
(declare-function qq-chat-open "qq-chat" (session-key))

(defvar qq-account-registry-changed-hook nil
  "Hook called with REASON and ACCOUNT-ID after the local registry changes.

REASON is one of `ready', `resync', `changed', `response', or `removed'.
ACCOUNT-ID is nil for an authoritative full-registry replacement.")

(defvar qq-account-selection-changed-hook nil
  "Hook called with OLD-ACCOUNT-ID and NEW-ACCOUNT-ID after selection changes.")

(defvar qq-account-registry-ready-hook nil
  "Hook called with GATEWAY-INSTANCE-ID after `gateway.ready' is projected.

The account registry has already been authoritatively replaced when handlers
run.  Consumers may use this typed synchronization boundary to refresh other
Gateway-owned registries without inspecting raw transport events.")

(defvar qq-account-desync-hook nil
  "Hook called with a protocol error body after transient events were lost.")

(defvar qq-account-projection-resync-hook nil
  "Hook called with PROJECTION and BODY after projection events were lost.

PROJECTION is one of \"conversation_read_states\", \"resources\",
\"attachments\", or \"remote_media\".
The account projection is resynchronized by this registry itself and is not
dispatched through this hook.  BODY is the owned event or error body that
reported the loss.  Consumers filter on PROJECTION and refresh their own
Gateway-owned registry from an authoritative snapshot.")

(defconst qq-account--foreign-projections
  '("conversation_read_states" "resources" "attachments" "remote_media")
  "Runtime projections owned by registries other than the account registry.")

(defvar qq-account--accounts (make-hash-table :test #'equal))
(defvar qq-account--account-order nil)
(defvar qq-account--current-account-id nil)
(defvar qq-account--gateway-instance-id nil)
(defvar qq-account--refresh-owner nil
  "Identity of the newest authoritative account registry refresh.")
(defvar qq-account--resync-request-id nil
  "Identity of the in-flight automatic account registry resync.")


(defun qq-account-get (account-id)
  "Return a copy of managed ACCOUNT-ID's snapshot, or nil."
  (qq-server-value-copy
   (and account-id (gethash account-id qq-account--accounts))))

(defun qq-account-list ()
  "Return managed account snapshots in authoritative Gateway order."
  (delq nil
        (mapcar (lambda (account-id)
                  (qq-account-get account-id))
                qq-account--account-order)))

(defun qq-account-current-id ()
  "Return the account selected by this Emacs client, or nil."
  (and qq-account--current-account-id
       (copy-sequence qq-account--current-account-id)))

(defun qq-account-current ()
  "Return a copy of this Emacs client's selected account snapshot."
  (qq-account-get qq-account--current-account-id))

(defun qq-account--set-current-account (account-id)
  "Select ACCOUNT-ID locally and publish an exact selection change."
  (when account-id
    (unless (gethash account-id qq-account--accounts)
      (user-error "qq: QQ account does not exist: %s" account-id)))
  (unless (equal account-id qq-account--current-account-id)
    (let ((old qq-account--current-account-id))
      (setq qq-account--current-account-id
            (and account-id (copy-sequence account-id)))
      (qq-rpc-run-hook
       'qq-account-selection-changed-hook old account-id)))
  account-id)

(defun qq-account--account-display-name (snapshot)
  "Return a completion label for account SNAPSHOT."
  (let ((label (alist-get 'label snapshot))
        (uin (alist-get 'uin snapshot))
        (account-id (alist-get 'account_id snapshot))
        (phase (alist-get 'phase snapshot)))
    (format "%s%s — %s [%s]"
            (or label uin "Unbound account")
            (if (and label uin) (format " (%s)" uin) "")
            phase account-id)))

(defun qq-account--read-account-id (&optional prompt)
  "Read a managed account ID using PROMPT."
  (let ((accounts (qq-account-list)))
    (unless accounts
      (user-error "qq: Gateway has no managed accounts"))
    (let* ((choices
            (mapcar (lambda (snapshot)
                      (cons (qq-account--account-display-name snapshot)
                            (alist-get 'account_id snapshot)))
                    accounts))
           (default
            (when-let* ((current (qq-account-current)))
              (qq-account--account-display-name current))))
      (cdr (assoc
            (completing-read (or prompt "QQ account: ")
                             choices nil t nil nil default)
            choices)))))

;;;###autoload
(defun qq-account-select (account-id)
  "Select managed ACCOUNT-ID for this Emacs client's chat projection."
  (interactive (list (qq-account--read-account-id "Select QQ account: ")))
  (qq-account--set-current-account account-id)
  (when (called-interactively-p 'interactive)
    (message "qq: selected QQ account %s" account-id))
  account-id)

(defun qq-account--maybe-select-single-account ()
  "Select the sole managed account when no explicit selection remains."
  (unless (and qq-account--current-account-id
               (gethash qq-account--current-account-id qq-account--accounts))
    (qq-account--set-current-account
     (and (= (hash-table-count qq-account--accounts) 1)
          (car qq-account--account-order)))))

(defun qq-account--replace-accounts (snapshots reason instance-id)
  "Replace the registry with SNAPSHOTS for REASON and INSTANCE-ID."
  (let ((next (make-hash-table :test #'equal))
        order)
    (dolist (snapshot snapshots)
      (let ((account-id (alist-get 'account_id snapshot)))
        (when (gethash account-id next)
          (error "qq: Duplicate QQ account ID %s" account-id))
        (puthash account-id snapshot next)
        (push account-id order)))
    (setq qq-account--accounts next
          qq-account--account-order (nreverse order)
          qq-account--gateway-instance-id
          (and instance-id (copy-sequence instance-id)))
    (qq-account--maybe-select-single-account)
    (qq-rpc-run-hook 'qq-account-registry-changed-hook reason nil)
    (qq-account-list)))

(defun qq-account--upsert-account (raw-snapshot reason)
  "Merge domain account RAW-SNAPSHOT for REASON."
  (let* ((snapshot raw-snapshot)
         (account-id (alist-get 'account_id snapshot))
         (existing (gethash account-id qq-account--accounts)))
    (unless existing
      (setq qq-account--account-order
            (append qq-account--account-order (list account-id))))
    (unless (equal existing snapshot)
      (puthash account-id snapshot qq-account--accounts)
      (qq-account--maybe-select-single-account)
      (qq-rpc-run-hook
       'qq-account-registry-changed-hook reason account-id))
    snapshot))

(defun qq-account--remove-account (account-id reason)
  "Remove ACCOUNT-ID locally for REASON and return its old snapshot."
  (let ((old (gethash account-id qq-account--accounts)))
    (when old
      (remhash account-id qq-account--accounts)
      (setq qq-account--account-order
            (delete account-id qq-account--account-order))
      (qq-account--maybe-select-single-account)
      (qq-rpc-run-hook
       'qq-account-registry-changed-hook reason account-id))
    old))

(defun qq-account-refresh-accounts (&optional callback errback reason)
  "Fetch the authoritative managed-account registry.

CALLBACK receives the copied account list.  ERRBACK follows the transport
error convention.  REASON defaults to `resync'."
  (qq-rpc-latest-call
   'qq-account--refresh-owner "account.list" nil
   :projector
   (lambda (result)
     (qq-account--replace-accounts
      (alist-get 'accounts result) (or reason 'resync)
      (qq-server-gateway-instance-id)))
   :callback callback
   :errback errback))

(defun qq-account--account-command
    (method account-id callback errback &optional remove-p)
  "Send account METHOD for ACCOUNT-ID and update the local registry.

CALLBACK receives the domain account snapshot and ERRBACK receives a protocol
error body plus reason.

When REMOVE-P is non-nil, remove the returned snapshot instead of merging it."
  (unless (qq-protocol-non-empty-string-p account-id)
    (user-error "qq: Account ID must be a non-empty opaque string"))
  (qq-rpc-call
   method `((account_id . ,account-id))
   :projector
   (lambda (snapshot)
     (if remove-p
         (qq-account--remove-account account-id 'removed)
       (qq-account--upsert-account snapshot 'response))
     snapshot)
   :callback callback
   :errback errback))

(defun qq-account--interactive-success (snapshot)
  "Report interactive success represented by account SNAPSHOT."
  (message "qq: account %s is %s"
           (alist-get 'account_id snapshot)
           (alist-get 'phase snapshot)))

(defun qq-account--interactive-error (_body reason)
  "Report interactive Gateway failure REASON."
  (message "qq: Gateway request failed: %s" reason))

(defun qq-device--interactive-reset-success (_receipt)
  "Report successful explicit Device Profile reset."
  (message "qq: default QQ Device Profile was reset; saved EasyLogin records are invalid"))

;;;###autoload
(defun qq-device-reset (&optional callback errback)
  "Explicitly reset the shared default QQ Device Profile.

Every Managed Account must first have no Native Session.  Success rotates the
MachineGuid and invalidates every Login Record bound to the previous device.
CALLBACK receives t; ERRBACK receives a failure body and reason."
  (interactive
   (progn
     (unless
         (yes-or-no-p
          "Reset the shared QQ device and invalidate all quick logins? ")
       (user-error "qq: Device Profile reset cancelled"))
     (list #'qq-device--interactive-reset-success
           #'qq-account--interactive-error)))
  (when-let* ((active
               (seq-find
                (lambda (account)
                  (not (member (alist-get 'phase account)
                               '("stopped" "logged_out" "failed"))))
                (qq-account-list))))
    (user-error "qq: Stop account %s before resetting the shared device"
                (alist-get 'account_id active)))
  (qq-rpc-call
   "device.reset" nil
   :projector
   (lambda (result)
     (unless (qq-server-wire-exact-object-keys-p result nil)
       (error "qq: Gateway returned an invalid Device Reset receipt"))
     t)
   :callback callback
   :errback errback))

;;;###autoload
(defun qq-account-create (label &optional callback errback)
  "Create a persistent QQ account slot with optional LABEL.

CALLBACK receives its snapshot; ERRBACK receives a failure body and reason."
  (interactive
   (list (let ((value (string-trim (read-string "Account label (optional): "))))
           (and (not (string-empty-p value)) value))
         #'qq-account--interactive-success
         #'qq-account--interactive-error))
  (when label
    (unless (and (qq-protocol-non-empty-string-p label)
                 (equal label (string-trim label))
                 (<= (length label) 128))
      (user-error "qq: Account label must be trimmed and at most 128 characters")))
  (qq-rpc-call
   "account.create" (if label `((label . ,label)) '((label)))
   :projector
   (lambda (snapshot)
     (setq snapshot (qq-account--upsert-account snapshot 'response))
     ;; Creating a slot is an explicit local choice, unlike discovering
     ;; several pre-existing slots in gateway.ready.
     (qq-account--set-current-account (alist-get 'account_id snapshot))
     snapshot)
   :callback callback
   :errback errback))

;;;###autoload
(defun qq-account-status (account-id &optional callback errback)
  "Fetch managed ACCOUNT-ID's current snapshot.

CALLBACK receives the snapshot; ERRBACK receives a failure body and reason."
  (interactive
   (list (qq-account--read-account-id)
         #'qq-account--interactive-success
         #'qq-account--interactive-error))
  (qq-account--account-command
   "account.status" account-id callback errback))

;;;###autoload
(defun qq-account-set-presence
    (account-id presence &optional callback errback)
  "Set PRESENCE for managed ACCOUNT-ID.

CALLBACK receives an acknowledgement carrying the account ID and
requested presence.  This command does not change the account lifecycle phase
or store presence in the local account snapshot."
  (unless (qq-protocol-non-empty-string-p account-id)
    (user-error "qq: Account ID must be a non-empty opaque string"))
  (setq presence
        (qq-protocol-validate-account-presence
         presence "account presence" 'user-error))
  (unless (qq-account-get account-id)
    (user-error "qq: QQ account does not exist: %s" account-id))
  (qq-rpc-call
   "account.set_presence"
   `((account_id . ,account-id) (presence . ,presence))
   :callback callback
   :errback errback))

(defun qq-account--device-common-fields-p (device)
  "Return non-nil when DEVICE has valid common roster fields."
  (and (qq-protocol-uint32-p (alist-get 'instance_id device))
       (> (alist-get 'instance_id device) 0)
       (let ((client-type (alist-get 'client_type device)))
         (or (null client-type) (qq-protocol-uint32-p client-type)))
       (member (alist-get 'kind device)
               '("computer" "phone" "pad" "unknown"))
       (let ((platform-id (alist-get 'platform_id device)))
         (or (null platform-id) (qq-protocol-uint32-p platform-id)))
       (let ((name (alist-get 'device_name device)))
         (or (null name) (stringp name)))))

(defun qq-account--online-client-p (device)
  "Return non-nil when DEVICE is a closed PushParams client row."
  (and
   (qq-server-wire-exact-object-keys-p
    device '(instance_id client_type kind state platform_id platform_type
             new_client_type device_name))
   (qq-account--device-common-fields-p device)
   (let ((state (alist-get 'state device)))
     (or (null state) (qq-protocol-uint32-p state)))
   (let ((platform-type (alist-get 'platform_type device)))
     (or (null platform-type) (stringp platform-type)))
   (let ((client-type (alist-get 'new_client_type device)))
     (or (null client-type) (qq-protocol-uint32-p client-type)))))

(defun qq-account--dataline-candidate-p (device)
  "Return non-nil when DEVICE is a closed 528/349 candidate row."
  (and
   (qq-server-wire-exact-object-keys-p
    device '(app_id instance_id client_type kind platform_id device_name
             field_11))
   (qq-account--device-common-fields-p device)
   (let ((app-id (alist-get 'app_id device)))
     (or (null app-id) (qq-protocol-uint32-p app-id)))
   (let ((field-11 (alist-get 'field_11 device)))
     (or (null field-11) (qq-protocol-uint32-p field-11)))))

(defun qq-account--dataline-peer-p (peer)
  "Return non-nil when PEER is a pinned DataLine class route."
  (and
   (qq-server-wire-exact-object-keys-p
    peer '(class peer_uid route_profile candidate_instance_ids))
   (let ((class (alist-get 'class peer))
         (uid (alist-get 'peer_uid peer)))
     (or (and (equal class "phone")
              (equal uid "u_Wcc5rknRRqRO8y5gxMD6sA"))
         (and (equal class "pad")
              (equal uid "u_l7jpPIZxQo0mzJwoEt-SKw"))))
   (equal (alist-get 'route_profile peer) "linuxqq_3_2_31_51102")
   (let ((instances (alist-get 'candidate_instance_ids peer))
         (seen (make-hash-table :test #'eql))
         valid)
     (setq valid (and (listp instances) instances))
     (dolist (instance instances)
       (unless (and (qq-protocol-uint32-p instance)
                    (> instance 0)
                    (not (gethash instance seen)))
         (setq valid nil))
       (puthash instance t seen))
     valid)))

(defun qq-account--project-device-roster (roster predicate context)
  "Validate source ROSTER rows with PREDICATE for CONTEXT."
  (let ((state (alist-get 'state roster)))
    (cond
     ((equal state "unknown")
      (unless (qq-server-wire-exact-object-keys-p roster '(state))
        (error "qq: Gateway returned an open %s roster" context))
      '((state . "unknown")))
     ((equal state "observed")
      (unless (and (qq-server-wire-exact-object-keys-p roster '(state devices))
                   (listp (alist-get 'devices roster)))
        (error "qq: Gateway returned an invalid %s roster" context))
      (let ((seen-instances (make-hash-table :test #'eql))
            devices)
        (dolist (device (alist-get 'devices roster))
          (unless (funcall predicate device)
            (error "qq: Gateway returned invalid %s metadata" context))
          (let ((instance-id (alist-get 'instance_id device)))
            (when (gethash instance-id seen-instances)
              (error "qq: Gateway returned duplicate %s instance %s"
                     context instance-id))
            (puthash instance-id t seen-instances))
          (push (qq-server-value-copy device) devices))
        `((state . "observed") (devices . ,(nreverse devices)))))
     (t (error "qq: Gateway returned an invalid %s roster state" context)))))

(defun qq-account--project-dataline-peer-roster (roster)
  "Validate build-profile DataLine class ROSTER."
  (let ((state (alist-get 'state roster)))
    (cond
     ((equal state "unknown")
      (unless (qq-server-wire-exact-object-keys-p roster '(state))
        (error "qq: Gateway returned an open DataLine-peer roster"))
      '((state . "unknown")))
     ((equal state "observed")
      (unless (and (qq-server-wire-exact-object-keys-p roster '(state devices))
                   (listp (alist-get 'devices roster)))
        (error "qq: Gateway returned an invalid DataLine-peer roster"))
      (let ((seen-classes (make-hash-table :test #'equal))
            peers)
        (dolist (peer (alist-get 'devices roster))
          (unless (qq-account--dataline-peer-p peer)
            (error
             "qq: Gateway returned invalid DataLine class-route metadata"))
          (let ((class (alist-get 'class peer)))
            (when (gethash class seen-classes)
              (error "qq: Gateway returned duplicate DataLine class route %s"
                     class))
            (puthash class t seen-classes))
          (push (qq-server-value-copy peer) peers))
        `((state . "observed") (devices . ,(nreverse peers)))))
     (t (error "qq: Gateway returned an invalid DataLine-peer roster state")))))

(defun qq-account--project-online-devices (result expected-account-id)
  "Validate independent device RESULT rosters for EXPECTED-ACCOUNT-ID."
  (unless (and
           (qq-server-wire-exact-object-keys-p
            result '(account_id online_clients data_line_candidates
                     data_line_peers))
           (equal (alist-get 'account_id result) expected-account-id))
    (error "qq: Gateway returned an invalid account-device snapshot"))
  `((account_id . ,(copy-sequence expected-account-id))
    (online_clients
     . ,(qq-account--project-device-roster
         (alist-get 'online_clients result) #'qq-account--online-client-p
         "online-client"))
    (data_line_candidates
     . ,(qq-account--project-device-roster
         (alist-get 'data_line_candidates result)
         #'qq-account--dataline-candidate-p "DataLine-candidate"))
    (data_line_peers
     . ,(qq-account--project-dataline-peer-roster
         (alist-get 'data_line_peers result)))))

(defun qq-account--open-dataline-peer (account-id peer)
  "Open ACCOUNT-ID's build-profile DataLine class PEER chat."
  (qq-account-select account-id)
  (require 'qq-state)
  (require 'qq-chat)
  (let* ((uid (alist-get 'peer_uid peer))
         (class (alist-get 'class peer))
         (title (if (equal class "pad") "My pad" "My phone"))
         (session-key (qq-state-session-key 'dataline uid 'desktop)))
    (require 'qq-runtime)
    (qq-runtime-call-with-account
     account-id
     (lambda ()
       (qq-state-upsert-session
        session-key
        `((type . dataline)
          (title . ,title)
          (target-id . ,uid)
          (peer-uid . ,uid)
          (variant . "desktop")
          (chat-type . "8"))
        nil)))
    (qq-chat-open session-key)))

(defun qq-account--display-online-devices (snapshot)
  "Display the online-device SNAPSHOT in a read-only buffer."
  (let* ((account-id (alist-get 'account_id snapshot))
         (online (alist-get 'online_clients snapshot))
         (dataline (alist-get 'data_line_candidates snapshot))
         (peers (alist-get 'data_line_peers snapshot))
         (buffer (get-buffer-create
                  (format "*QQ My Devices: %s*" account-id))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "My Devices — account %s\n\n" account-id))
        (insert "Online clients (PushParams)\n")
        (cond
         ((equal (alist-get 'state online) "unknown")
          (insert "  Snapshot not observed yet.\n"))
         ((null (alist-get 'devices online))
          (insert "  QQ reports no online client rows.\n"))
         (t
          (dolist (device (alist-get 'devices online))
            (insert
             (format "  %s — kind: %s; client type: %s; instance: %s\n"
                     (let ((name (alist-get 'device_name device)))
                       (if (qq-protocol-non-empty-string-p name)
                           name
                         (capitalize (alist-get 'kind device))))
                     (alist-get 'kind device)
                     (or (alist-get 'client_type device) "unknown")
                     (alist-get 'instance_id device))))))
        (insert "\nDataLine candidates (528/349)\n")
        (cond
         ((equal (alist-get 'state dataline) "unknown")
          (insert "  Snapshot not observed yet.\n"))
         ((null (alist-get 'devices dataline))
          (insert "  QQ reports no DataLine candidate rows.\n"))
         (t
          (dolist (device (alist-get 'devices dataline))
            (insert
             (format "  %s — kind: %s; client type: %s; instance: %s\n"
                     (let ((name (alist-get 'device_name device)))
                       (if (qq-protocol-non-empty-string-p name)
                           name
                         (capitalize (alist-get 'kind device))))
                     (alist-get 'kind device)
                     (or (alist-get 'client_type device) "unknown")
                     (alist-get 'instance_id device))))))
        (insert "\nDataLine class routes (LinuxQQ 3.2.31 profile)\n")
        (cond
         ((equal (alist-get 'state peers) "unknown")
          (insert "  Candidate snapshot not observed yet.\n"))
         ((null (alist-get 'devices peers))
          (insert "  No currently reachable phone/pad class route.\n"))
         (t
          (dolist (peer (alist-get 'devices peers))
            (let ((route (copy-tree peer))
                  (owner (copy-sequence account-id)))
              (insert "  ")
              (insert-text-button
               (format "Open %s class chat" (alist-get 'class route))
               'follow-link t
               'help-echo
               (concat "Open the shared DataLine class conversation "
                       "(not one physical device)")
               'action
               (lambda (_button)
                 (qq-account--open-dataline-peer owner route)))
              (insert
               (format " — UID: %s; candidate instances: %s\n"
                       (alist-get 'peer_uid route)
                       (mapconcat #'number-to-string
                                  (alist-get 'candidate_instance_ids route)
                                  ", ")))))))
        (insert
         "\nA class route does not select one physical same-class device.\n")
        (goto-char (point-min))
        (special-mode)))
    (display-buffer buffer)
    snapshot))

;;;###autoload
(defun qq-account-list-online-devices
    (account-id &optional callback errback)
  "List QQ clients currently online for managed ACCOUNT-ID.

The result keeps the PushParams online-client roster and the 528/349 DataLine
candidate roster independent.  Each source is explicitly `unknown' or
`observed', so an observed empty list does not collapse into not-yet-observed.
It also carries a third, explicitly derived computer/phone/pad class-route
projection for the pinned LinuxQQ profile.  This projection never claims that
a class UID selects one physical same-class Device Instance.

CALLBACK receives the closed source and class-route rosters.  ERRBACK follows
the Gateway transport convention."
  (interactive
   (list (qq-account--read-account-id "List devices for account: ")
         #'qq-account--display-online-devices
         #'qq-account--interactive-error))
  (unless (qq-protocol-non-empty-string-p account-id)
    (user-error "qq: Account ID must be a non-empty opaque string"))
  (unless (qq-account-get account-id)
    (user-error "qq: QQ account does not exist: %s" account-id))
  (qq-rpc-call
   "account.list_online_devices" `((account_id . ,account-id))
   :projector
   (lambda (result)
     (qq-account--project-online-devices result account-id))
   :callback callback
   :errback errback))

;;;###autoload
(defun qq-account-start (account-id &optional callback errback)
  "Start ACCOUNT-ID's Native Session.

CALLBACK receives the snapshot; ERRBACK receives a failure body and reason."
  (interactive
   (list (qq-account--read-account-id "Start account: ")
         #'qq-account--interactive-success
         #'qq-account--interactive-error))
  (qq-account--account-command
   "account.start" account-id callback errback))

(defun qq-account--login-command
    (method account-id params secret callback errback)
  "Send login METHOD for ACCOUNT-ID with PARAMS and copied SECRET.

PARAMS is called with the copied secret.  CALLBACK receives the account
snapshot; ERRBACK receives a failure body and reason."
  (unless (qq-protocol-non-empty-string-p account-id)
    (user-error "qq: Account ID must be a non-empty opaque string"))
  (let ((secret-copy (copy-sequence secret)))
    (unwind-protect
        (qq-rpc-call
         method (append `((account_id . ,account-id))
                        (funcall params secret-copy))
         :projector
         (lambda (snapshot)
           (qq-account--upsert-account snapshot 'response))
         :callback callback
         :errback errback)
      (clear-string secret-copy))))

(defun qq-account--project-quick-login-accounts (result)
  "Validate RESULT and return its ordered EasyLogin account metadata."
  (unless (and (qq-server-wire-exact-object-keys-p result '(accounts))
               (listp (alist-get 'accounts result)))
    (error "qq: Gateway returned an invalid quick-login account list"))
  (let ((seen-uins (make-hash-table :test #'equal))
        accounts)
    (dolist (account (alist-get 'accounts result))
      (unless
          (and
           (qq-server-wire-exact-object-keys-p
            account '(uin uid generated_at_unix))
           (qq-protocol-uint64-decimal-p (alist-get 'uin account))
           (qq-protocol-non-empty-string-p (alist-get 'uid account))
           (integerp (alist-get 'generated_at_unix account))
           (>= (alist-get 'generated_at_unix account) 0))
        (error "qq: Gateway returned invalid quick-login account metadata"))
      (let ((uin (alist-get 'uin account)))
        (when (gethash uin seen-uins)
          (error "qq: Gateway returned duplicate quick-login UIN %s" uin))
        (puthash uin t seen-uins))
      (push (qq-server-value-copy account) accounts))
    (nreverse accounts)))

;;;###autoload
(defun qq-account-login-list (&optional callback errback)
  "List identities with reusable native EasyLogin credentials.

CALLBACK receives ordered non-secret account metadata.  ERRBACK receives a
failure body and reason.  The Gateway advertises this operation only when its
encrypted credential store is available."
  (interactive
   (list (lambda (accounts)
           (message "qq: %d quick-login account%s available"
                    (length accounts)
                    (if (= (length accounts) 1) "" "s")))
         #'qq-account--interactive-error))
  (qq-rpc-call
   "account.login.list" nil
   :projector #'qq-account--project-quick-login-accounts
   :callback callback
   :errback errback))

;;;###autoload
(defun qq-account-login-password
    (account-id uin password &optional qimei callback errback)
  "Begin password login for ACCOUNT-ID using exact UIN and PASSWORD.

QIMEI is optional.  The caller's PASSWORD string is not retained or mutated.
CALLBACK receives the account snapshot; ERRBACK receives a failure body and
reason."
  (interactive
   (let* ((account-id (qq-account--read-account-id "Login account: "))
          (snapshot (qq-account-get account-id)))
     (list account-id
           (read-string "QQ UIN: " (alist-get 'uin snapshot))
           (read-passwd "QQ password: ") nil
           #'qq-account--interactive-success
           #'qq-account--interactive-error)))
  (unless (qq-protocol-uint64-decimal-p uin)
    (user-error "qq: UIN must be a canonical nonzero uint64 string"))
  (unless (qq-protocol-non-empty-string-p password)
    (user-error "qq: Password must not be empty"))
  (unless (or (null qimei) (qq-protocol-non-empty-string-p qimei))
    (user-error "qq: QIMEI must be a non-empty string or nil"))
  (qq-account--login-command
   "account.login.password" account-id
   (lambda (secret)
     `((uin . ,uin) (password . ,secret)
       ,@(when qimei `((qimei . ,qimei)))))
   password callback errback))

;;;###autoload
(defun qq-account-login-quick
    (account-id uin &optional qimei callback errback)
  "Begin native EasyLogin for ACCOUNT-ID using the stored identity for UIN.

QIMEI is optional.  CALLBACK receives the account snapshot; ERRBACK receives
a failure body and reason.  No reusable credential material crosses the
Gateway protocol."
  (unless (qq-protocol-uint64-decimal-p uin)
    (user-error "qq: UIN must be a canonical nonzero uint64 string"))
  (unless (or (null qimei) (qq-protocol-non-empty-string-p qimei))
    (user-error "qq: QIMEI must be a non-empty string or nil"))
  (unless (qq-protocol-non-empty-string-p account-id)
    (user-error "qq: Account ID must be a non-empty opaque string"))
  (qq-rpc-call
   "account.login.quick"
   `((account_id . ,account-id)
     (uin . ,uin)
     ,@(when qimei `((qimei . ,qimei))))
   :projector
   (lambda (snapshot)
     (qq-account--upsert-account snapshot 'response))
   :callback callback
   :errback errback))

;;;###autoload
(defun qq-account-login-captcha
    (account-id challenge-id ticket rand-str sid &optional callback errback)
  "Continue ACCOUNT-ID's captcha CHALLENGE-ID.

TICKET, RAND-STR, and SID carry proof captured by the foreground browser
adapter.  CALLBACK receives the account snapshot; ERRBACK receives a failure
body and reason."
  (dolist (value (list challenge-id ticket rand-str sid))
    (unless (qq-protocol-non-empty-string-p value)
      (user-error "qq: Captcha proof fields must be non-empty strings")))
  (qq-account--login-command
   "account.login.captcha" account-id
   (lambda (secret)
     `((challenge_id . ,challenge-id) (ticket . ,secret)
       (rand_str . ,rand-str) (sid . ,sid)))
   ticket callback errback))

;;;###autoload
(defun qq-account-stop (account-id &optional callback errback)
  "Take ACCOUNT-ID's Native Session offline while retaining quick login.

CALLBACK receives the snapshot; ERRBACK receives a failure body and reason."
  (interactive
   (let ((account-id (qq-account--read-account-id "Stop Native Session: ")))
     (unless (yes-or-no-p
              (format "Stop Native Session %s and retain quick login? " account-id))
       (user-error "qq: Account stop cancelled"))
     (list account-id #'qq-account--interactive-success
           #'qq-account--interactive-error)))
  (qq-account--account-command
   "account.stop" account-id callback errback))

;;;###autoload
(defun qq-account-logout (account-id &optional callback errback)
  "Take ACCOUNT-ID offline and delete quick login, retaining its managed slot.

CALLBACK receives the snapshot; ERRBACK receives a failure body and reason."
  (interactive
   (let ((account-id (qq-account--read-account-id "Log out account: ")))
     (unless (yes-or-no-p
              (format "Log QQ account %s out and delete quick login? " account-id))
       (user-error "qq: Account logout cancelled"))
     (list account-id #'qq-account--interactive-success
           #'qq-account--interactive-error)))
  (qq-account--account-command
   "account.logout" account-id callback errback))

;;;###autoload
(defun qq-account-remove (account-id &optional callback errback)
  "Delete stopped ACCOUNT-ID and its Gateway-managed local material.

CALLBACK receives the removed snapshot; ERRBACK receives a failure body and
reason."
  (interactive
   (let ((account-id (qq-account--read-account-id "Remove account slot: ")))
     (unless (yes-or-no-p
              (format "Permanently remove QQ account slot %s? " account-id))
       (user-error "qq: Account removal cancelled"))
     (list account-id #'qq-account--interactive-success
           #'qq-account--interactive-error)))
  (qq-account--account-command
   "account.remove" account-id callback errback t))

(defun qq-account--handle-event (event data)
  "Project native service EVENT with DATA into the account registry."
  (pcase event
    ("gateway.ready"
     (let ((instance-id (alist-get 'gateway_instance_id data))
           (accounts (alist-get 'accounts data)))
       (let ((resync-marker qq-account--resync-request-id))
         (qq-rpc-cancel-latest
          'qq-account--refresh-owner "superseded_request"
          "Gateway account refresh was superseded by gateway.ready")
         (when (eq resync-marker qq-account--resync-request-id)
           (setq qq-account--resync-request-id nil)))
       (let ((projected
              (qq-account--replace-accounts accounts 'ready instance-id)))
         (qq-rpc-run-hook 'qq-account-registry-ready-hook instance-id)
         projected)))
    ("account.changed"
     (qq-account--upsert-account data 'changed))
    ("account.removed"
     (qq-account--remove-account
      (alist-get 'account_id data) 'removed))
    (_ (error "qq: Unowned Gateway account event %s" event))))

(defun qq-account--resync-accounts (body)
  "Resynchronize the account registry after transient event loss BODY."
  (qq-rpc-run-hook 'qq-account-desync-hook body)
  (qq-rpc-request-single-flight
   'qq-account--resync-request-id 'account-resync
   (lambda (success failure)
     (qq-account-refresh-accounts success failure 'resync))
   "QQ account"))

(defun qq-account--handle-protocol-error (body)
  "Handle unsolicited Gateway protocol error BODY.

A websocket-level event stream lag may have dropped events for every runtime
projection, so all projection owners resynchronize."
  (when (equal (alist-get 'code body) "event_stream_lagged")
    (qq-account--resync-accounts body)
    (dolist (projection qq-account--foreign-projections)
      (qq-rpc-run-hook
       'qq-account-projection-resync-hook projection body))))

(defun qq-account--handle-resync-required (_event data)
  "Resynchronize one runtime projection after lossy DATA."
  (let ((projection (alist-get 'projection data)))
    (unless (and (qq-server-wire-exact-object-keys-p data '(projection skipped))
                 (qq-protocol-non-empty-string-p projection)
                 (qq-protocol-non-empty-string-p (alist-get 'skipped data)))
      (error "qq: Malformed runtime.resync_required event"))
    (cond
     ((equal projection "accounts")
      (qq-account--resync-accounts data))
     ((member projection qq-account--foreign-projections)
      (qq-rpc-run-hook
       'qq-account-projection-resync-hook projection data))
     (t
      (error "qq: Unowned runtime projection %s requires resync"
             projection)))))

(dolist (event '("gateway.ready" "account.changed" "account.removed"))
  (qq-rpc-register-event event #'qq-account--handle-event))
(qq-rpc-register-event
 "runtime.resync_required" #'qq-account--handle-resync-required)
(qq-rpc-register-error
 "event_stream_lagged" #'qq-account--handle-protocol-error)

(provide 'qq-account)

;;; qq-account.el ends here
