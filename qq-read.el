;;; qq-read.el --- Authoritative QQ conversation read state -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; Owns the nt-gateway read-state snapshot and event contract. Native read
;; cursors remain transport data; the four unread completeness domains are
;; projected independently into `qq-state'. In particular,
;; latest_sequence-read_sequence is never treated as an unread count.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'qq-account)
(require 'qq-rpc)
(require 'qq-runtime)
(require 'qq-state)

(declare-function qq-state-apply-session-read-projection
                  "qq-state" (session-key projection))

(defvar qq-read-projection-error-hook nil
  "Hook called with EVENT, DATA, and REASON after read projection fails.")

(defvar qq-read--observation-clock 0
  "Monotonic client clock for read-state requests and events.")

(defvar qq-read--conversation-observations (make-hash-table :test #'equal)
  "Newest accepted observation token keyed by account and conversation.")

(defvar qq-read--observed-account-phases (make-hash-table :test #'equal)
  "Last observed Native Session phase keyed by managed account ID.")

(defun qq-read--closed-object-p (value required &optional optional)
  "Return non-nil when alist VALUE has unique REQUIRED and OPTIONAL keys."
  (and (proper-list-p value)
       (cl-every (lambda (entry)
                   (and (consp entry) (symbolp (car entry))))
                 value)
       (let ((keys (mapcar #'car value)))
         (and (= (length keys) (length (delete-dups (copy-sequence keys))))
              (cl-every (lambda (key) (memq key keys)) required)
              (cl-every (lambda (key) (memq key (append required optional)))
                        keys)))))

(defun qq-read--cursor-p (value)
  "Return non-nil when VALUE is a canonical uint64 cursor string."
  (qq-protocol-uint64-decimal-p value t))

(defun qq-read--message-id-p (value)
  "Return non-nil when VALUE is an exact nonzero uint64 message ID."
  (qq-protocol-uint64-decimal-p value))

(defun qq-read--position-p (value)
  "Return non-nil when VALUE is a closed unread message position."
  (and (qq-read--closed-object-p value '(sequence message_id))
       (qq-protocol-uint64-decimal-p (alist-get 'sequence value))
       (let ((message-id (alist-get 'message_id value)))
         (or (null message-id) (qq-read--message-id-p message-id)))))

(defun qq-read--conversation-p (value)
  "Return non-nil when VALUE is a closed read-state conversation identity."
  (and (listp value)
       (pcase (alist-get 'kind value)
         ("group"
          (and (qq-read--closed-object-p value '(kind group_uin))
               (qq-protocol-uint64-decimal-p (alist-get 'group_uin value))))
         ("private"
          (and (qq-read--closed-object-p
                value '(kind peer_uid) '(peer_uin))
               (qq-protocol-non-empty-string-p (alist-get 'peer_uid value))
               (let ((uin (alist-get 'peer_uin value)))
                 (or (null uin) (qq-protocol-uint64-decimal-p uin)))))
         (_ nil))))

(defun qq-read--message-count-p (value)
  "Return non-nil when VALUE is a closed ordinary-message projection."
  (pcase (and (listp value) (alist-get 'status value))
    ("unknown"
     (qq-read--closed-object-p value '(status)))
    ("exact"
     (and (qq-read--closed-object-p value '(status count))
          (let ((count (alist-get 'count value)))
            (and (integerp count) (>= count 0)))))
    (_ nil)))

(defun qq-read--badge-count-p (value)
  "Return non-nil when VALUE is a closed Managed Account badge projection."
  (pcase (and (listp value) (alist-get 'status value))
    ("unknown"
     (qq-read--closed-object-p value '(status)))
    ("exact"
     (and (qq-read--closed-object-p value '(status count))
          (let ((count (alist-get 'count value)))
            (and (integerp count) (>= count 0)))))
    (_ nil)))

(defun qq-read--first-unread-p (value)
  "Return non-nil when VALUE is a closed first-unread projection."
  (pcase (and (listp value) (alist-get 'status value))
    ("unknown"
     (qq-read--closed-object-p value '(status)))
    ("exact"
     (and (qq-read--closed-object-p value '(status position))
          (let ((position (alist-get 'position value)))
            (or (null position) (qq-read--position-p position)))))
    (_ nil)))

(defun qq-read--mentions-p (value)
  "Return non-nil when VALUE is a closed unread-mention projection."
  (pcase (and (listp value) (alist-get 'status value))
    ("unknown"
     (qq-read--closed-object-p value '(status)))
    ("exact"
     (and (qq-read--closed-object-p value '(status at_me at_all))
          (let ((at-me (alist-get 'at_me value))
                (at-all (alist-get 'at_all value)))
            (and (or (null at-me) (qq-read--position-p at-me))
                 (or (null at-all) (qq-read--position-p at-all))))))
    (_ nil)))

(defun qq-read--unread-p (value)
  "Return non-nil when VALUE has four independent unread projections."
  (and (qq-read--closed-object-p
        value '(message_count badge_count first_unread mentions))
       (let ((message-count (alist-get 'message_count value))
             (badge-count (alist-get 'badge_count value)))
         (and (qq-read--message-count-p message-count)
              (qq-read--badge-count-p badge-count)
              (qq-read--first-unread-p (alist-get 'first_unread value))
              (qq-read--mentions-p (alist-get 'mentions value))
              (or (not (and (equal (alist-get 'status message-count) "exact")
                            (equal (alist-get 'status badge-count) "exact")))
                  (>= (alist-get 'count badge-count)
                      (alist-get 'count message-count)))))))

(defun qq-read--state-p (value)
  "Return non-nil when VALUE is a closed conversation read-state snapshot."
  (and
   (qq-read--closed-object-p
    value '(conversation read_sequence latest_sequence unread))
   (qq-read--conversation-p (alist-get 'conversation value))
   (qq-read--cursor-p (alist-get 'read_sequence value))
   (qq-read--cursor-p (alist-get 'latest_sequence value))
   (not (qq-protocol-decimal-less-p
         (alist-get 'latest_sequence value)
         (alist-get 'read_sequence value)))
   (qq-read--unread-p (alist-get 'unread value))
   (let* ((read (alist-get 'read_sequence value))
          (latest (alist-get 'latest_sequence value))
          (unread (alist-get 'unread value))
          (message-count (alist-get 'message_count unread))
          (first-state (alist-get 'first_unread unread))
          (mention-state (alist-get 'mentions unread))
          (message-exact
           (equal (alist-get 'status message-count) "exact"))
          (first-exact (equal (alist-get 'status first-state) "exact"))
          (mentions-exact
           (equal (alist-get 'status mention-state) "exact"))
          (count (and message-exact (alist-get 'count message-count)))
          (first (and first-exact (alist-get 'position first-state)))
          (at-me (and mentions-exact (alist-get 'at_me mention-state)))
          (at-all (and mentions-exact (alist-get 'at_all mention-state)))
          (positions (delq nil (list first at-me at-all)))
          (first-sequence (and first (alist-get 'sequence first))))
     (and (or (not first-exact) first (null positions))
          (cond
           ((and message-exact (zerop count)) (null positions))
           ((and first-exact count (> count 0)) first)
           (t t))
          (cl-every
           (lambda (position)
             (let ((sequence (alist-get 'sequence position)))
               (and (qq-protocol-decimal-less-p read sequence)
                    (not (qq-protocol-decimal-less-p latest sequence))
                    (or (null first-sequence)
                        (equal sequence first-sequence)
                        (qq-protocol-decimal-less-p first-sequence sequence)))))
           positions)))))

(defun qq-read--state-key (state)
  "Return a stable deduplication key for checked STATE."
  (let ((conversation (alist-get 'conversation state)))
    (pcase (alist-get 'kind conversation)
      ("group" (list 'group (alist-get 'group_uin conversation)))
      ("private" (list 'private (alist-get 'peer_uid conversation))))))

(defun qq-read--check-page (value &optional expected-account-id)
  "Validate and isolate read-state page VALUE.

When EXPECTED-ACCOUNT-ID is non-nil, reject a contradictory owner."
  (unless (and (qq-read--closed-object-p value '(account_id states))
               (qq-protocol-non-empty-string-p (alist-get 'account_id value))
               (or (null expected-account-id)
                   (equal expected-account-id (alist-get 'account_id value)))
               (proper-list-p (alist-get 'states value)))
    (error "qq: malformed conversation read-state page"))
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (state (alist-get 'states value))
      (unless (qq-read--state-p state)
        (error "qq: malformed conversation read-state snapshot"))
      (let ((key (qq-read--state-key state)))
        (when (gethash key seen)
          (error "qq: duplicate conversation in read-state page"))
        (puthash key t seen))))
  (copy-tree value))

(defun qq-read--session-key (state)
  "Return the local session key represented by checked STATE, or nil."
  (let ((conversation (alist-get 'conversation state)))
    (pcase (alist-get 'kind conversation)
      ("group"
       (qq-state-session-key 'group (alist-get 'group_uin conversation)))
      ("private"
       (when-let* ((peer-uin (alist-get 'peer_uin conversation)))
         (qq-state-session-key 'private peer-uin))))))

(defun qq-read--position-field (position field)
  "Return FIELD from nullable unread POSITION."
  (and (listp position) (alist-get field position)))

(defun qq-read--state-projection (state)
  "Convert checked Gateway STATE to an internal session read projection."
  (let* ((unread (alist-get 'unread state))
         (message-state (alist-get 'message_count unread))
         (badge-state (alist-get 'badge_count unread))
         (first-state (alist-get 'first_unread unread))
         (mention-state (alist-get 'mentions unread))
         (message-exact (equal (alist-get 'status message-state) "exact"))
         (badge-exact (equal (alist-get 'status badge-state) "exact"))
         (first-exact (equal (alist-get 'status first-state) "exact"))
         (mentions-exact
          (equal (alist-get 'status mention-state) "exact"))
         (count (and message-exact (alist-get 'count message-state)))
         (badge-count (and badge-exact (alist-get 'count badge-state)))
         (first (and first-exact (alist-get 'position first-state)))
         (at-me (and mentions-exact (alist-get 'at_me mention-state)))
         (at-all (and mentions-exact (alist-get 'at_all mention-state)))
         (first-id (qq-read--position-field first 'message_id)))
    `((unread-message-count . ,count)
      (unread-badge-count . ,badge-count)
      (first-unread-message-id . ,first-id)
      (first-unread-message-seq
       . ,(qq-read--position-field first 'sequence))
      (unread-at-me-message-id
       . ,(qq-read--position-field at-me 'message_id))
      (unread-at-me-message-seq
       . ,(qq-read--position-field at-me 'sequence))
      (unread-at-all-message-id
       . ,(qq-read--position-field at-all 'message_id))
      (unread-at-all-message-seq
       . ,(qq-read--position-field at-all 'sequence))
      (read-position-available . ,(and first-id t))
      (read-latest-message-id . nil))))

(defun qq-read--next-observation-token ()
  "Allocate a freshness token for one read-state request or event."
  (cl-incf qq-read--observation-clock))

(defun qq-read--accept-observation-p (account-id conversation-key token)
  "Accept TOKEN for ACCOUNT-ID and CONVERSATION-KEY unless a newer one won."
  (let* ((key (list account-id conversation-key))
         (known (gethash key qq-read--conversation-observations 0)))
    (when (< known token)
      (puthash key token qq-read--conversation-observations)
      t)))

(defun qq-read--apply-state (account-id state token)
  "Apply checked STATE for ACCOUNT-ID when TOKEN is current."
  ;; Freshness is owned by the Gateway conversation identity, not by the
  ;; optional local projection.  A private event without a public UIN must
  ;; still supersede an older in-flight snapshot for the same peer UID.
  (when (qq-read--accept-observation-p
         account-id (qq-read--state-key state) token)
    (when-let* ((session-key (qq-read--session-key state)))
      (qq-runtime-with-account account-id
        (qq-state-upsert-session session-key nil nil)
        (qq-state-apply-session-read-projection
         session-key (qq-read--state-projection state))))))

(defun qq-read--request-states (method account-id callback errback)
  "Request ACCOUNT-ID's read states through METHOD."
  (unless (qq-protocol-non-empty-string-p account-id)
    (user-error "qq: conversation read states require an account slot"))
  (qq-rpc-call
   method `((account_id . ,account-id))
   :projector (lambda (value) (qq-read--check-page value account-id))
   :callback callback
   :errback errback))

(defun qq-read-list-states (account-id &optional callback errback)
  "List ACCOUNT-ID's retained authoritative conversation read states."
  (qq-read--request-states
   "conversation.list_read_states" account-id callback errback))

(defun qq-read-refresh-states (account-id &optional callback errback)
  "Acquire missing evidence and return ACCOUNT-ID's read states."
  (qq-read--request-states
   "conversation.refresh_read_states" account-id callback errback))

(defun qq-read-refresh-account (account-id &optional callback errback)
  "Refresh and project ACCOUNT-ID's authoritative read-state snapshot."
  (let ((token (qq-read--next-observation-token)))
    (qq-read-refresh-states
     account-id
     (lambda (page)
       (dolist (state (alist-get 'states page))
         (qq-read--apply-state account-id state token))
       (when callback (funcall callback page)))
     errback)))

(defun qq-read--default-error (_body reason)
  "Report automatic read-state refresh failure REASON."
  (message "qq: conversation read-state refresh failed: %s" reason))

(defun qq-read--refresh-online-accounts ()
  "Refresh read-state snapshots for every online managed account."
  (when (qq-rpc-method-available-p "conversation.refresh_read_states")
    (dolist (account (qq-account-list))
      (when (equal (alist-get 'phase account) "online")
        (qq-read-refresh-account
         (alist-get 'account_id account) nil #'qq-read--default-error)))))

(defun qq-read--handle-ready (_instance-id)
  "Start a fresh read-state delivery epoch after Gateway ready."
  (setq qq-read--conversation-observations (make-hash-table :test #'equal)
        qq-read--observed-account-phases (make-hash-table :test #'equal))
  (dolist (account (qq-account-list))
    (puthash (alist-get 'account_id account)
             (alist-get 'phase account)
             qq-read--observed-account-phases))
  (qq-read--refresh-online-accounts))

(defun qq-read--handle-account-change (reason account-id)
  "Refresh a newly online ACCOUNT-ID after registry REASON."
  (unless (eq reason 'ready)
    (when account-id
      (let* ((account (qq-account-get account-id))
             (old-phase (gethash account-id qq-read--observed-account-phases))
             (new-phase (and account (alist-get 'phase account))))
        (if account
            (puthash account-id new-phase qq-read--observed-account-phases)
          (remhash account-id qq-read--observed-account-phases))
        (when (and account
                   (equal new-phase "online")
                   (not (equal old-phase "online"))
                   (qq-rpc-method-available-p
                    "conversation.refresh_read_states"))
          (qq-read-refresh-account account-id nil #'qq-read--default-error))))))

(defun qq-read--handle-projection-resync (projection _body)
  "Refresh after runtime read-state PROJECTION events were lost."
  (when (equal projection "conversation_read_states")
    (qq-read--refresh-online-accounts)))

(defun qq-read--handle-event (event data)
  "Project authoritative Gateway read-state EVENT DATA."
  (condition-case error-data
      (progn
        (unless (and (equal event "conversation.read_state_changed")
                     (qq-read--closed-object-p data '(account_id state))
                     (qq-protocol-non-empty-string-p
                      (alist-get 'account_id data))
                     (qq-read--state-p (alist-get 'state data)))
          (error "qq: malformed conversation read-state event"))
        (let ((account-id (alist-get 'account_id data)))
          (unless (qq-account-get account-id)
            (error "qq: read-state event belongs to an unknown account"))
          (qq-read--apply-state
           account-id (alist-get 'state data)
           (qq-read--next-observation-token))))
    (error
     (let ((reason (error-message-string error-data)))
       (qq-rpc-run-hook
        'qq-read-projection-error-hook event (copy-tree data) reason)
       (message "qq: Gateway %s projection skipped: %s" event reason)))))

(qq-rpc-register-event
 "conversation.read_state_changed" #'qq-read--handle-event)
(add-hook 'qq-account-registry-ready-hook #'qq-read--handle-ready)
(add-hook 'qq-account-registry-changed-hook #'qq-read--handle-account-change)
(add-hook 'qq-account-projection-resync-hook
          #'qq-read--handle-projection-resync)

(provide 'qq-read)

;;; qq-read.el ends here
