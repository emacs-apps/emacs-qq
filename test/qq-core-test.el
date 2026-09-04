;;; qq-core-test.el --- Tests for the product operation boundary -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq)

(defun qq-core-test-message-event ()
  "Return one valid private native service message event."
  '((account_id . "slot-a")
    (message
     (message_id . "7348923749823749823")
     (sent_at . 1784700000)
     (sender . ((uin . "10001") (uid . "u_peer")))
     (recipient . ((uin . "10002") (uid . "u_self")))
     (conversation . ((kind . "private")))
     (sender_presentation
      . ((nickname . "Alice Nick") (remark . "Alice Remark")))
     (sequence . "9007199254740999")
     (client_sequence . "9007199254741001")
     (random . 7)
     (message_type . 166)
     (sub_type . 0)
     (segments
      ((kind . "text")
       (payload . ((text . "hello"))))))))

(defun qq-core-test-account ()
  "Return one online QQ account."
  (copy-tree
   '((account_id . "slot-a")
     (label . "Primary")
     (phase . "online")
     (uin . "10002")
     (uid . "u_self")
     (challenge)
     (problem))))

(defun qq-core-test-account-b ()
  "Return a second online QQ account."
  (copy-tree
   '((account_id . "slot-b")
     (label . "Secondary")
     (phase . "online")
     (uin . "20002")
     (uid . "u_self_b")
     (challenge)
     (problem))))

(defmacro qq-core-test-with-managed-account (&rest body)
  "Run BODY with one real managed account and account state partition."
  (declare (indent 0) (debug t))
  `(let ((qq-account--accounts (make-hash-table :test #'equal))
         (qq-account--account-order nil)
         (qq-account--current-account-id nil)
         (qq-account--gateway-instance-id "test-gateway")
         (qq-account-registry-changed-hook nil)
         (qq-account-selection-changed-hook nil)
         (qq-runtime--app nil)
         (qq-runtime--accounts (make-hash-table :test #'equal))
         (qq-state--partitions (make-hash-table :test #'equal))
         (qq-state--active-account-id nil))
     (unwind-protect
         (progn
           (qq-account--replace-accounts
            (list (qq-core-test-account)) 'ready "test-gateway")
           (qq-state-select-account "slot-a")
           (qq-state-reset)
           ,@body)
       (qq-runtime-stop)
       (qq-state-reset))))

(defun qq-core-test-prepared-image (attachment-id resource-id)
  "Return the identities reported for one prepared image."
  `((attachment_id . ,attachment-id)
    (resource_id . ,resource-id)))

(defun qq-core-test-prepared-video
    (attachment-id resource-id thumbnail-resource-id)
  "Return the identities reported for one prepared video."
  `((attachment_id . ,attachment-id)
    (resource_id . ,resource-id)
    (use . ((kind . "video")
            (thumbnail_resource_id . ,thumbnail-resource-id)))))

(defconst qq-core-test-members
  '(((user_id . "9007199254740999")
     (uid . "u_alice")
     (nickname . "Alice")
     (card . "Maintainer")
     (remark)
     (qid . "alice-qid")
     (title)
     (role . "admin")
     (robot)
     (group_id . "8209413637"))
    ((user_id . "10003")
     (uid . "u_bob")
     (nickname . "Bob")
     (card)
     (remark . "Friend")
     (qid)
     (title)
     (role . "member")
     (robot)
     (group_id . "8209413637")))
  "Mapped Gateway members used by backend search tests.")

(ert-deftest qq-core-connect-and-disconnect-use-only-native-transport ()
  (let (calls)
    (cl-letf (((symbol-function 'qq-core-activate)
               (lambda () (push 'activate calls)))
              ((symbol-function 'qq-server-start)
               (lambda () (push 'native-start calls)))
              ((symbol-function 'qq-server-stop)
               (lambda () (push 'native-stop calls))))
      (qq-core-connect)
      (qq-core-disconnect))
    (should (equal (nreverse calls) '(activate native-start native-stop)))))

(ert-deftest qq-core-events-project-the-owned-account-slot ()
  (qq-core-test-with-managed-account
    (let ((qq-message--peer-uin-by-uid
           (make-hash-table :test #'equal))
          (qq-message--pending-recalls
           (make-hash-table :test #'equal))
          (qq-message--pending-reactions
           (make-hash-table :test #'equal))
          (qq-message--pending-sends
           (make-hash-table :test #'equal))
          (qq-message--live-frontiers
           (make-hash-table :test #'equal))
          (qq-account-registry-changed-hook nil)
          observed)
      (let ((qq-message-event-hook
             (list (lambda (event _data) (setq observed event)))))
        (qq-message--handle-event
         "message.received" (qq-core-test-message-event)))
      (should (equal observed "message.received"))
      (should (qq-state-session "private:10001")))))

(ert-deftest qq-core-activation-bootstraps-all-managed-accounts ()
  (let (bootstrapped)
    (cl-letf (((symbol-function 'qq-core--maybe-bootstrap-all)
               (lambda (&rest _) (setq bootstrapped t))))
      (should (qq-core-activate))
      (should bootstrapped))))

(ert-deftest qq-core-recent-projects-private-and-temporary-c2c-rows ()
  (qq-core-test-with-managed-account
    (let* ((raw-message (alist-get 'message (qq-core-test-message-event)))
           (message (qq-server-wire-domain-copy raw-message))
           (temporary-message (copy-tree message))
           (page
            `((account_id . "slot-a")
              (conversations
               . (((conversation . ((kind . "private") (peer_uid . "u_peer")))
                   (pinned . t)
                   (activity_revision . "2")
                   (latest_message . ((kind . "native")
                                      (row_key . "2")
                                      (timeline_class . "authored")
                                      (recalled . :false)
                                      (message . ,message))))
                  ((conversation
                    . ((kind . "temporary")
                       (c2c_type . 100)
                       (peer_uid . "u_peer")
                       (from_tiny_id . "10")))
                   (pinned . t)
                   (activity_revision . "1")
                   (latest_message
                    . ((kind . "native")
                       (row_key . "1")
                       (timeline_class . "authored")
                       (recalled . :false)
                       (message . ,temporary-message))))))
              (truncated . :false))))
      (setf (alist-get 'conversation temporary-message nil nil #'eq)
            '((kind . "temp") (name . "Temporary")
              (from_tiny_id . "10"))
            (alist-get 'sub_type temporary-message nil nil #'eq) 100)
      (progn
        (let ((entry
               (qq-core--recent-row-state-entry
                page
                (car (alist-get 'conversations page))
                (qq-core-test-account))))
          (should-not (plist-member entry :read-cursor-known-p))
          (should-not (plist-member entry :read-cursor)))
        (let ((temporary-entry
               (qq-core--recent-row-state-entry
                page
                (cadr (alist-get 'conversations page))
                (qq-core-test-account))))
          (should (equal (plist-get temporary-entry :session-key)
                         "private:10001"))
          (should (equal (alist-get 'message-type
                                    (plist-get temporary-entry :message))
                         "temp")))
        (setf (alist-get 'conversations page nil nil #'eq)
              (list (cadr (alist-get 'conversations page))))
        (qq-core--apply-recent-page
         page (qq-state-session-summary-observation-start))
        (should (equal (qq-state-recent-session-keys)
                       '("private:10001")))
        (let ((session (qq-state-session "private:10001")))
          (should (eq (alist-get 'pinned session) t))
          (should (equal
                   (alist-get 'last-message-gateway-account-id session)
                   "slot-a"))
          (should-not (qq-state-session-messages "private:10001")))))))

(ert-deftest qq-core-recent-projects-dataline-through-unified-head ()
  (qq-core-test-with-managed-account
    (let* ((peer-uid "u_Wcc5rknRRqRO8y5gxMD6sA")
           (message-id "7348923749823749824")
           (page
            `((account_id . "slot-a")
              (conversations
               . (((conversation . ((kind . "dataline")
                                    (peer_uid . ,peer-uid)
                                    (variant . "desktop")))
                   (activity_revision . "5")
                   (latest_message
                    . ((kind . "dataline")
                       (message
                        . ((message_id . ,message-id)
                           (chat . ((peer_uid . ,peer-uid)
                                    (variant . "desktop")))
                           (direction . "received")
                           (sent_at . 1784700001)
                           (segments
                            . (((kind . "text")
                                (payload
                                 . ((text . "hello from phone")))))))))))))
              (truncated . :false))))
      (qq-message--recent-check-page page)
      (qq-core--apply-recent-page
       page (qq-state-session-summary-observation-start))
      (should (equal (qq-state-recent-session-keys)
                     (list (format "dataline:desktop:%s" peer-uid))))
      (let ((session
             (qq-state-session
              (format "dataline:desktop:%s" peer-uid))))
        (should (equal (alist-get 'title session) "My phone"))
        (should (equal (alist-get 'variant session) "desktop"))
        (should (equal (alist-get 'last-message-id session) message-id))
        (should (equal (alist-get 'last-message-preview session)
                       "hello from phone"))
        (should-not (assq 'pinned session))
        (should-not (alist-get 'last-message-seq session))))))

(ert-deftest qq-core-recent-accepts-idless-gray-tip-with-canonical-row-key ()
  (qq-core-test-with-managed-account
    (let* ((message
            (qq-server-wire-domain-copy
             (alist-get 'message (qq-core-test-message-event))))
           (row-key "42")
           row page entry)
      (setf (alist-get 'message_id message nil nil #'eq) nil
            (alist-get 'sender message nil nil #'eq)
            '((uin . "10001") (uid . "u_peer"))
            (alist-get 'recipient message nil nil #'eq)
            '((uin . "10002") (uid . "u_self"))
            (alist-get 'conversation message nil nil #'eq)
            '((kind . "group") (group_uin . "8209413637")
              (group_name . "Native Group"))
            (alist-get 'sender_presentation message nil nil #'eq) nil
            (alist-get 'message_type message nil nil #'eq) 732
            (alist-get 'sub_type message nil nil #'eq) 20
            (alist-get 'segments message nil nil #'eq)
            '(((kind . "gray_tip")
               (payload . ((kind . "poke")
                           (actor_uin . "10001")
                           (target_uin . "10002")
                           (actor_name . "Actor")
                           (target_name . "Target")
                           (action . "戳了戳"))))))
      (setq row
            `((conversation . ((kind . "group")
                               (group_uin . "8209413637")))
              (activity_revision . "3")
              (latest_message . ((kind . "native")
                                 (row_key . ,row-key)
                                 (timeline_class . "service")
                                 (recalled . :false)
                                 (message . ,message))))
            page `((account_id . "slot-a")
                   (conversations . (,row))
                   (truncated . :false))
            entry (qq-core--recent-row-state-entry
                   page row (qq-core-test-account)))
      (let ((normalized (plist-get entry :message)))
        (should-not (alist-get 'server-id normalized))
        (should (equal (alist-get 'canonical-row-key normalized) row-key))
        (should (equal (alist-get 'id normalized)
                       (format "timeline:slot-a:group:8209413637:%s" row-key))))
      ;; Core normalization must not annotate the Gateway-owned page in place.
      (should-not (alist-get 'canonical-row-key message))
      (qq-state-apply-recent-conversations (list entry) 1)
      (let ((session (qq-state-session "group:8209413637")))
        (should (equal (alist-get 'last-message-id session)
                       "timeline:slot-a:group:8209413637:42"))
        (should (equal (alist-get 'last-message-preview session)
                       "Actor 戳了戳 Target")))
      ;; Recall redacts the GrayTip body.  The canonical class must retain the
      ;; service identity so an empty sender presentation cannot invalidate
      ;; the whole atomic recent page.
      (let* ((recalled-message (copy-tree message))
             (recalled-row
              `((conversation . ((kind . "group")
                                 (group_uin . "8209413637")))
                (activity_revision . "4")
                (latest_message . ((kind . "native")
                                   (row_key . "43")
                                   (timeline_class . "service")
                                   (recalled . t)
                                   (message . ,recalled-message))))))
        (setf (alist-get 'segments recalled-message nil nil #'eq) nil)
        (qq-message--recent-check-page
         `((account_id . "slot-a")
           (conversations . (,recalled-row))
           (truncated . :false)))
        (let ((recalled
               (plist-get
                (qq-core--recent-row-state-entry
                 page recalled-row (qq-core-test-account))
                :message)))
          (should (eq (alist-get 'timeline-class recalled) 'service))
          (should (equal (alist-get 'sender-name recalled) "QQ"))
          (should (equal (alist-get 'preview recalled)
                         "[message recalled]")))))))

(ert-deftest qq-message-pure-normalizer-does-not-consume-projection-state ()
  (let* ((raw-message (alist-get 'message (qq-core-test-message-event)))
         (message (qq-server-wire-domain-copy raw-message))
         (qq-state--message-order-counter 41)
         (qq-message--pending-sends (make-hash-table :test #'equal))
         (qq-message--pending-recalls (make-hash-table :test #'equal))
         (qq-message--live-frontiers (make-hash-table :test #'equal)))
    (puthash '(sentinel) t qq-message--pending-sends)
    (let ((normalized
           (qq-message-normalize-snapshot
            message "slot-a" (qq-core-test-account))))
      (should (= qq-state--message-order-counter 41))
      (should (gethash '(sentinel) qq-message--pending-sends))
      (should (= (hash-table-count qq-message--pending-recalls) 0))
      (should (= (hash-table-count qq-message--live-frontiers) 0))
      (should-not (assq 'order normalized))
      (should (equal (alist-get 'gateway-account-id normalized) "slot-a")))))

(ert-deftest qq-core-directory-request-owns-cancellation-and-late-callback ()
  (let ((qq-account--current-account-id "slot-a") sent cancelled delivered)
    (cl-letf (((symbol-function 'qq-directory-refresh-friends)
               (lambda (callback errback refresh)
                 (setq sent (list callback errback refresh))
                 "gateway-request"))
              ((symbol-function 'qq-server-cancel)
               (lambda (token) (setq cancelled token))))
      (let ((request
              (qq-core-refresh-friend-categories
               (lambda (value) (setq delivered value)) #'ignore t)))
        (should (qq-request-p request))
        (should (equal (qq-request-token request) "gateway-request"))
        (should (functionp (nth 0 sent)))
        (should (functionp (nth 1 sent)))
        (should (eq (nth 2 sent) t))
        (should (qq-request-cancel request))
        (should-not (qq-request-cancel request))
        (should (eq (qq-request-state request) 'cancelled))
        (should (equal cancelled "gateway-request"))
        (funcall (nth 0 sent) 'late)
        (should-not delivered)))))

(ert-deftest qq-core-recent-request-owns-the-only-lifecycle ()
  (qq-core-test-with-managed-account
    (let ((qq-core--recent-requests (make-hash-table :test #'equal))
          cancelled)
      (cl-letf (((symbol-function 'qq-account-current-id)
                 (lambda () "slot-a"))
                ((symbol-function 'qq-state-session-summary-observation-start)
                 (lambda () '(recent-observation)))
                ((symbol-function 'qq-message-list-recent)
                 (lambda (_account-id &rest _arguments) "recent-request"))
                ((symbol-function 'qq-server-cancel)
                 (lambda (token) (setq cancelled token) t)))
        (let ((request (qq-core-refresh-recent-conversations)))
          (should (qq-request-active-p request))
          (should (equal (qq-request-token request) "recent-request"))
          (should (eq request
                      (gethash "slot-a" qq-core--recent-requests)))
          (should (qq-request-cancel request))
          (should (equal cancelled "recent-request"))
          (should (eq (qq-request-state request) 'cancelled)))))))

(ert-deftest qq-core-recent-replacement-revokes-old-projector ()
  (qq-core-test-with-managed-account
    (let ((qq-request--active (make-hash-table :test #'eq))
          (qq-core--recent-requests (make-hash-table :test #'equal))
          callbacks cancelled projected delivered (request-count 0))
      (cl-letf (((symbol-function 'qq-account-current-id)
                 (lambda () "slot-a"))
                ((symbol-function 'qq-state-session-summary-observation-start)
                 (lambda () (list 'observation (1+ request-count))))
                ((symbol-function 'qq-message-list-recent)
                 (lambda (account-id &rest arguments)
                   (cl-incf request-count)
                   (push (list account-id (plist-get arguments :callback))
                         callbacks)
                   (format "recent-%d" request-count)))
                ((symbol-function 'qq-core--apply-recent-page)
                 (lambda (page observation)
                   (push (list page observation) projected)
                   page))
                ((symbol-function 'qq-server-cancel)
                 (lambda (token) (push token cancelled) t)))
        (qq-core-refresh-recent-conversations
         (lambda (value) (push value delivered)) #'ignore)
        (qq-core-refresh-recent-conversations
         (lambda (value) (push value delivered)) #'ignore)
        (should (equal cancelled '("recent-1")))
        ;; Adapter callbacks can race transport cancellation.  The old product
        ;; request must reject the callback before state projection begins.
        (funcall (cadr (cadr callbacks)) 'old-page)
        (should-not projected)
        (funcall (cadr (car callbacks)) 'new-page)
        (should (equal projected
                       '((new-page (observation 2)))))
        (should (equal delivered '(new-page)))
        (should-not (gethash "slot-a" qq-core--recent-requests)))
      (should (= (hash-table-count qq-request--active) 0)))))

(ert-deftest qq-request-starter-nonlocal-exit-revokes-ownership ()
  (let ((qq-request--active (make-hash-table :test #'eq))
        caught)
    (condition-case error-data
        (qq-request-start
         (lambda (_success _failure) (signal 'quit nil))
         :owner nil)
      (quit (setq caught error-data)))
    (should (equal caught '(quit)))
    (should (= (hash-table-count qq-request--active) 0))))

(ert-deftest qq-request-appkit-lifecycle-owns-cancellation-and-retirement ()
  (let ((app (appkit-app-start qq-runtime--account-type :identity "request-test"))
        (qq-request--active (make-hash-table :test #'eq))
        canceled success delivered pending settled)
    (unwind-protect
        (cl-letf (((symbol-function 'qq-server-cancel)
                   (lambda (token) (push token canceled))))
          (setq pending
                (qq-request-start
                 (lambda (_resolve _reject) "cancel-token")
                 :owner nil :lifecycle-owner app))
          (setq settled
                (qq-request-start
                 (lambda (resolve _reject)
                   (setq success resolve) "settle-token")
                 :callback (lambda (value) (setq delivered value))
                 :owner nil :lifecycle-owner app))
          (funcall success 'ready)
          (should (eq delivered 'ready))
          (appkit-app-close app)
          (should (eq (qq-request-state pending) 'cancelled))
          (should (eq (qq-request-state settled) 'settled))
          (should (equal canceled '("cancel-token"))))
      (appkit-app-close app))))

(ert-deftest qq-request-revoke-all-cancels-active-requests ()
  (let ((qq-request--active (make-hash-table :test #'eq))
        cleaned requests)
    (dolist (name '(first second))
      (let ((identity name))
        (push
         (qq-request-create
          nil (lambda () (push identity cleaned)))
         requests)))
    (qq-request-revoke-all)
    (should (equal (sort cleaned
                         (lambda (left right)
                           (string< (symbol-name left) (symbol-name right))))
                   '(first second)))
    (should (cl-every
             (lambda (request)
               (eq (qq-request-state request) 'cancelled))
             requests))
    (should (= (hash-table-count qq-request--active) 0))))

(ert-deftest qq-request-slot-scope-survives-selection-until-removal ()
  (qq-core-test-with-managed-account
    (let ((qq-request--active (make-hash-table :test #'eq))
          success delivered cancelled)
      (cl-letf (((symbol-function 'qq-server-cancel)
                 (lambda (token) (push token cancelled))))
        (let ((request
                (qq-request-start
                 (lambda (callback _failure)
                   (setq success callback)
                   "slot-request")
                 :callback (lambda (value) (setq delivered value)))))
          (should (equal (qq-request-owner request) "slot-a"))
          ;; The stable slot remains the request's callback observation context.
          (funcall success 'current-page)
          (should (eq delivered 'current-page))
          (should (eq (qq-request-state request) 'settled)))
        (setq delivered nil)
        (let ((request
                (qq-request-start
                 (lambda (callback _failure)
                   (setq success callback)
                   "switched-slot-request")
                 :owner "slot-a"
                 :callback (lambda (value) (setq delivered value)))))
          (qq-account--upsert-account
           '((account_id . "slot-b") (label . "Other") (phase . "online")
             (uin . "10003") (uid . "u_other") (challenge) (problem))
           'changed)
          (qq-account-select "slot-b")
          (funcall success 'owned-page)
          (should (eq delivered 'owned-page))
          (should (eq (qq-request-state request) 'settled)))
        (setq delivered nil)
        (let ((request
                (qq-request-start
                 (lambda (callback _failure)
                   (setq success callback)
                   "removed-slot-request")
                 :owner "slot-a"
                 :callback (lambda (value) (setq delivered value)))))
          (qq-account--remove-account "slot-a" 'removed)
          (funcall success 'stale-page)
          (should-not delivered)
          (should (eq (qq-request-state request) 'cancelled))
          (should (equal cancelled '("removed-slot-request"))))))))

(ert-deftest qq-core-start-request-distinguishes-omitted-and-global-scope ()
  (let ((qq-request--active (make-hash-table :test #'eq)))
    (cl-letf (((symbol-function 'qq-account-current-id)
               (lambda () "slot-a")))
      (let ((slot-request
             (qq-core--start-request
              (lambda (_success _failure) "slot-token") nil nil))
            (global-request
             (qq-core--start-request
              (lambda (_success _failure) "global-token") nil nil
              :owner nil)))
        (should (equal (qq-request-owner slot-request) "slot-a"))
        (should-not (qq-request-owner global-request))
        (qq-request-cancel slot-request)
        (qq-request-cancel global-request)))))

(ert-deftest qq-core-start-request-requires-slot-unless-global-is-explicit ()
  (let ((qq-request--active (make-hash-table :test #'eq)))
    (cl-letf (((symbol-function 'qq-account-current-id)
               (lambda () nil)))
      (should-error
       (qq-core--start-request
        (lambda (_success _failure) "unreachable") nil nil)
       :type 'user-error)
      (let ((global-request
             (qq-core--start-request
              (lambda (_success _failure) "global-token") nil nil
              :owner nil)))
        (should-not (qq-request-owner global-request))
        (qq-request-cancel global-request)))))

(ert-deftest qq-request-rejects-malformed-owner-context ()
  (dolist (owner '("" account ("slot-a" . "runtime")
                   ("" . "runtime") ("slot-a" . 7)))
    (should-error (qq-request-create owner))))

(ert-deftest qq-core-group-profile-uses-owned-directory-state ()
  (let (profile refreshed)
    (unwind-protect
        (progn
          (qq-state-reset)
          (qq-state-apply-groups
           '(((group_id . "8209413637")
              (group_name . "Protocol Lab")
              (group_remark . "Lab")
              (member_count . 3)
              (max_member_count . 500)
              (created_at . 1700000000)
              (description . "Native Gateway")
              (announcement . "Welcome")
              (self_permission . "owner"))))
          (cl-letf (((symbol-function 'qq-core-refresh-joined-groups)
                     (lambda (&rest _arguments) (setq refreshed t))))
            (should-not
             (qq-core-get-group
              "8209413637" (lambda (value) (setq profile value))))
            (should-not refreshed)
            (should (equal (alist-get 'group_id profile) "8209413637"))
            (should (equal (alist-get 'name profile) "Protocol Lab"))
            (should (= (alist-get 'member_count profile) 3))))
      (qq-state-reset))))

(ert-deftest qq-core-group-settings-update-shared-directory-after-receipt ()
  (qq-core-test-with-managed-account
    (let ((qq-account--current-account-id "slot-a") calls callbacks)
      (unwind-protect
          (progn
            (qq-state-reset)
            (qq-state-apply-groups
             '(((group_id . "8209413637")
                (group_name . "Old")
                (group_remark . "Old Remark")
                (member_count . 3)
                (max_member_count . 500))))
            (cl-letf
                (((symbol-function 'qq-directory-set-group-name)
                  (lambda (group-id value callback &optional _errback)
                    (push (list 'name group-id value) calls)
                    (funcall callback '((name . "New")))
                    "name-request"))
                 ((symbol-function 'qq-directory-set-group-remark)
                  (lambda (group-id value callback &optional _errback)
                    (push (list 'remark group-id value) calls)
                    (funcall callback '((remark . "")))
                    "remark-request"))
                 ((symbol-function 'qq-directory-set-group-whole-mute)
                  (lambda (group-id value callback &optional _errback)
                    (push (list 'mute group-id value) calls)
                    (funcall callback '((enabled . t)))
                    "mute-request"))
                 ((symbol-function 'qq-directory-set-group-pinned)
                  (lambda (group-id value callback &optional _errback)
                    (push (list 'pinned group-id value) calls)
                    (funcall callback '((pinned . :false)))
                    "pinned-request"))
                 ((symbol-function 'qq-directory-clock-in-group)
                  (lambda (group-id callback &optional _errback)
                    (push (list 'clock-in group-id) calls)
                    (funcall callback '((title . "今日已打卡")))
                    "clock-in-request")))
              (let ((name-request
                     (qq-core-set-group-name
                      "8209413637" "New"
                      (lambda (_receipt) (push 'name callbacks))))
                    (remark-request
                     (qq-core-set-group-remark
                      "8209413637" ""
                      (lambda (_receipt) (push 'remark callbacks))))
                    (mute-request
                     (qq-core-set-group-whole-mute
                      "8209413637" t
                      (lambda (_receipt) (push 'mute callbacks))))
                    (pinned-request
                     (qq-core-set-group-pinned
                      "8209413637" nil
                      (lambda (_receipt) (push 'pinned callbacks))))
                    (clock-in-request
                     (qq-core-clock-in-group
                      "8209413637"
                      (lambda (_receipt) (push 'clock-in callbacks)))))
                (dolist (request (list name-request remark-request mute-request
                                       pinned-request clock-in-request))
                  (should (qq-request-p request))
                  (should (eq (qq-request-state request) 'settled))
                  (should-not (qq-request-token request)))))
            (let ((group (qq-state-group "8209413637")))
              (should (equal (alist-get 'group_name group) "New"))
              (should-not (alist-get 'group_remark group))
              (should (eq (alist-get 'pinned group) :false)))
            (should (equal (sort callbacks
                                 (lambda (left right)
                                   (string< (symbol-name left)
                                            (symbol-name right))))
                           '(clock-in mute name pinned remark)))
            (should
             (equal (nreverse calls)
                    '((name "8209413637" "New")
                      (remark "8209413637" "")
                      (mute "8209413637" t)
                      (pinned "8209413637" nil)
                      (clock-in "8209413637")))))
        (qq-state-reset)))))

(ert-deftest qq-core-friend-pinned-routes-exact-uin ()
  (qq-core-test-with-managed-account
    (let ((qq-account--current-account-id "slot-a") called callback-value)
      (cl-letf (((symbol-function 'qq-directory-set-friend-pinned)
                 (lambda (user-id pinned callback &optional _errback)
                   (setq called (list user-id pinned))
                   (funcall callback `((friend_uin . ,user-id) (pinned . t)))
                   "friend-pin")))
        (let ((request
                (qq-core-set-friend-pinned
                 "9007199254740999" t
                 (lambda (receipt) (setq callback-value receipt)))))
          (should (eq (qq-request-state request) 'settled))))
      (should (equal called '("9007199254740999" t)))
      (should (eq (alist-get 'pinned callback-value) t)))))

(ert-deftest qq-core-presence-targets-selected-account ()
  (qq-core-test-with-managed-account
    (let (called callback-value)
      (cl-letf (((symbol-function 'qq-account-current-id)
                 (lambda () "slot-a"))
                ((symbol-function 'qq-account-set-presence)
                 (lambda (account-id presence callback &optional _errback)
                   (setq called (list account-id presence))
                   (funcall callback
                            `((account_id . ,account-id)
                              (presence . ,presence)))
                   "presence")))
        (let* ((presence '((kind . "away")))
               (request
                 (qq-core-set-presence
                  presence (lambda (receipt) (setq callback-value receipt)))))
          (should (eq (qq-request-state request) 'settled))
          (should (equal called (list "slot-a" presence)))
          (should (equal (alist-get 'presence callback-value) presence)))))))

(ert-deftest qq-core-group-leave-converges-loaded-state ()
  (qq-core-test-with-managed-account
    (let ((qq-account--current-account-id "slot-a")
          (group-id "8209413637") called callback-value)
      (unwind-protect
          (progn
            (qq-state-reset)
            (qq-state-apply-groups
             `(((group_id . ,group-id) (group_name . "Leave"))
               ((group_id . "20002") (group_name . "Keep"))))
            (cl-letf (((symbol-function 'qq-directory-leave-group)
                       (lambda (candidate callback &optional _errback)
                         (setq called candidate)
                         (funcall callback `((group_uin . ,candidate)))
                         "leave")))
              (let ((request
                      (qq-core-leave-group
                       group-id
                       (lambda (receipt) (setq callback-value receipt)))))
                (should (eq (qq-request-state request) 'settled))))
            (should (equal called group-id))
            (should callback-value)
            (should-not (qq-state-group group-id))
            (should (qq-state-group "20002")))
        (qq-state-reset)))))

(ert-deftest qq-core-group-at-all-query-routes-wide-group-uin ()
  (qq-core-test-with-managed-account
    (let ((qq-account--current-account-id "slot-a") called callback-value)
      (cl-letf (((symbol-function
                  'qq-directory-get-group-at-all-remaining)
                 (lambda (candidate callback &optional _errback)
                   (setq called candidate)
                   (funcall callback
                            `((group_uin . ,candidate)
                              (can_at_all . t)
                              (remain_at_all_count_for_uin . 3)
                              (remain_at_all_count_for_group . 9)))
                   "at-all")))
        (let ((request
                (qq-core-get-group-at-all-remaining
                 "8209413637"
                 (lambda (receipt) (setq callback-value receipt)))))
          (should (eq (qq-request-state request) 'settled))))
      (should (equal called "8209413637"))
      (should (= (alist-get 'remain_at_all_count_for_uin callback-value) 3))
      (should (= (alist-get 'remain_at_all_count_for_group callback-value) 9)))))

(ert-deftest qq-core-group-leave-does-not-invent-unloaded-directory ()
  (unwind-protect
      (progn
        (qq-state-reset)
        (cl-letf (((symbol-function 'qq-account-current-id)
                   (lambda () "slot-a"))
                  ((symbol-function 'qq-directory-leave-group)
                   (lambda (_group-id callback &optional _errback)
                     (funcall callback '((status . "ok")))
                     "leave")))
          (qq-core-leave-group "8209413637"))
        (should-not (qq-state-groups-loaded-p)))
    (qq-state-reset)))

(ert-deftest qq-core-group-member-settings-route-wide-native-identities ()
  (qq-core-test-with-managed-account
    (let ((qq-account--current-account-id "slot-a") calls callbacks)
      (cl-letf
          (((symbol-function 'qq-directory-set-group-member-card)
            (lambda (group-id user-id value callback &optional _errback)
              (push (list 'card group-id user-id value) calls)
              (funcall callback '((card . "Ferris")))
              "card-request"))
           ((symbol-function
             'qq-directory-set-group-member-special-title)
            (lambda (group-id user-id value callback &optional _errback)
              (push (list 'title group-id user-id value) calls)
              (funcall callback '((special_title . "Maintainer")))
              "title-request"))
           ((symbol-function 'qq-directory-kick-group-member)
            (lambda (group-id user-id reject callback &optional _errback)
              (push (list 'kick group-id user-id reject) calls)
              (funcall callback '((reject_add_request . t)))
              "kick-request")))
        (let ((card-request
               (qq-core-set-group-member-card
                "8209413637" "9007199254741001" "Ferris"
                (lambda (_receipt) (push 'card callbacks))))
              (title-request
               (qq-core-set-group-member-special-title
                "8209413637" "9007199254741001" "Maintainer"
                (lambda (_receipt) (push 'title callbacks))))
              (kick-request
               (qq-core-kick-group-member
                "8209413637" "9007199254741001" t
                (lambda (_receipt) (push 'kick callbacks)))))
          (dolist (request (list card-request title-request kick-request))
            (should (eq (qq-request-state request) 'settled)))))
      (should (equal (nreverse calls)
                     '((card "8209413637" "9007199254741001" "Ferris")
                       (title "8209413637" "9007199254741001"
                              "Maintainer")
                       (kick "8209413637" "9007199254741001" t))))
      (should (equal (sort callbacks
                           (lambda (left right)
                             (string< (symbol-name left) (symbol-name right))))
                     '(card kick title)))
      (should (qq-protocol-group-uin-p "8209413637"))
      (should (qq-protocol-user-uin-p "9007199254741001")))))

(ert-deftest qq-core-member-search-filters-cached-exact-ids ()
  (let (result fetched)
    (cl-letf (((symbol-function 'qq-directory-group-member-page)
               (lambda (_group-id)
                 `((members . ,(copy-tree qq-core-test-members)))))
              ((symbol-function 'qq-directory-list-group-members)
               (lambda (&rest _) (setq fetched t))))
      (should-not
       (qq-core-search-group-members
        "8209413637" "alice" (lambda (members) (setq result members)) nil 1))
      (should-not fetched)
      (should (= (length result) 1))
      (should (equal (alist-get 'user_id (car result))
                     "9007199254740999"))
      (should (stringp (alist-get 'user_id (car result)))))))

(ert-deftest qq-core-member-search-fetches-on-cache-miss ()
  (qq-core-test-with-managed-account
    (let ((qq-account--current-account-id "slot-a") callback result)
      (cl-letf (((symbol-function 'qq-directory-group-member-page)
                 (lambda (_group-id) nil))
                ((symbol-function 'qq-directory-list-group-members)
                 (lambda (_group-id success _errback &optional _refresh)
                   (setq callback success)
                   "member-request")))
        (let ((request
                (qq-core-search-group-members
                 "8209413637" "friend"
                 (lambda (members) (setq result members)) nil 200)))
          (should (equal (qq-request-token request) "member-request"))
          (funcall callback (copy-tree qq-core-test-members))
          (should (= (length result) 1))
          (should (equal (alist-get 'user_id (car result)) "10003")))))))

(ert-deftest qq-core-send-routes-closed-segments ()
  (let ((qq-account--current-account-id "slot-a") sent)
    (cl-letf (((symbol-function 'qq-message-send)
               (lambda (session-key segments &optional raw callback errback)
                 (setq sent (list session-key segments raw callback errback))
                 "send-request")))
      (let ((segments
             '(((type . "reply")
                (data . ((id . "7348923749823749823"))))
               ((type . "at")
                (data . ((qq . "10002") (name . "Alice"))))
               ((type . "face") (data . ((id . "178"))))
               ((type . "text") (data . ((text . "hello")))))))
        (let ((request
                (qq-core-send-message
                 "group:8209413637" segments "optimistic")))
          (should (qq-request-p request))
          (should (equal (qq-request-token request) "send-request")))
        (should (equal (nth 0 sent) "group:8209413637"))
        (should (eq (nth 1 sent) segments))
        (should (equal (nth 2 sent) "optimistic"))))))

(ert-deftest qq-core-group-file-stages-publishes-and-releases-resource ()
  (qq-core-test-with-managed-account
    (let ((path (make-temp-file "qq-core-file-" nil ".txt" "hello"))
          staged sent released success)
      (unwind-protect
          (cl-letf
              (((symbol-function 'qq-resource-stage-local)
                (lambda (source name _sha callback _errback)
                  (setq staged (list source name))
                  (funcall
                   callback
                   '((resource_id . "res-group-file")
                     (phase . "ready")))
                  "stage-request"))
               ((symbol-function 'qq-message-send-file)
                (lambda (session resource-id callback _errback)
                  (setq sent (list session resource-id))
                  (funcall
                   callback
                   '((account_id . "slot-a")
                     (group_uin . "8209413637")
                     (fast_path . t)
                     (message_random . 7)))
                  "file-request"))
               ((symbol-function 'qq-core--release-send-resource)
                (lambda (resource-id)
                  (push resource-id released))))
            (let ((request
                    (qq-core-send-message
                     "group:8209413637"
                     `(((type . "file")
                        (data . ((file . ,path)
                                 (name . "notes.txt")))))
                     nil
                     (lambda (receipt) (setq success receipt)))))
              (should (eq (qq-request-state request) 'settled))
              (should (equal staged (list path "notes.txt")))
              (should
               (equal sent
                      '("group:8209413637" "res-group-file")))
              (should
               (equal (alist-get 'message_random success) 7))
              (should (equal released '("res-group-file")))))
        (delete-file path)))))

(ert-deftest qq-core-dataline-image-stages-as-one-file-transfer ()
  (qq-core-test-with-managed-account
    (let ((path (make-temp-file "qq-core-dataline-" nil ".png" "image"))
          staged sent released success)
      (unwind-protect
          (cl-letf
              (((symbol-function 'qq-resource-stage-local)
                (lambda (source name _sha callback _errback)
                  (setq staged (list source name))
                  (funcall callback
                           '((resource_id . "res-dataline-image")
                             (phase . "ready")))
                  "stage-request"))
               ((symbol-function 'qq-message-send-file)
                (lambda (session resource-id callback _errback segment)
                  (setq sent (list session resource-id segment))
                  (funcall
                   callback
                   '((account_id . "slot-a")
                     (message_id . "7348923749823749825")))
                  "dataline-file-request"))
               ((symbol-function 'qq-core--release-send-resource)
                (lambda (resource-id) (push resource-id released))))
            (let* ((segment
                    `((type . "image")
                      (data . ((file . ,path) (name . "photo.png")))))
                   (request
                     (qq-core-send-message
                      "dataline:mobile:u_Wcc5rknRRqRO8y5gxMD6sA"
                      (list segment) nil
                      (lambda (receipt) (setq success receipt)))))
              (should (eq (qq-request-state request) 'settled))
              (should (equal staged (list path "photo.png")))
              (should (equal (nth 0 sent)
                             "dataline:mobile:u_Wcc5rknRRqRO8y5gxMD6sA"))
              (should (equal (nth 1 sent) "res-dataline-image"))
              (should (equal (nth 2 sent) segment))
              (should (equal (alist-get 'message_id success)
                             "7348923749823749825"))
              (should (equal released '("res-dataline-image")))))
        (delete-file path)))))

(ert-deftest qq-core-dataline-file-waits-for-staged-resource-readiness ()
  (qq-core-test-with-managed-account
    (let ((path (make-temp-file "qq-core-dataline-wait-" nil ".png" "image"))
          ready-callback sent released success)
      (unwind-protect
          (cl-letf
              (((symbol-function 'qq-resource-stage-local)
                (lambda (_source _name _sha callback _errback)
                  (funcall callback
                           '((resource_id . "res-dataline-wait")
                             (phase . "staging")))
                  "stage-request"))
               ((symbol-function 'qq-resource-await-ready)
                (lambda (resource-id callback _errback)
                  (should (equal resource-id "res-dataline-wait"))
                  (setq ready-callback callback)
                  (qq-request-watch-create :active-p t)))
               ((symbol-function 'qq-message-send-file)
                (lambda (_session resource-id callback _errback _segment)
                  (setq sent resource-id)
                  (funcall callback
                           '((account_id . "slot-a")
                             (message_id . "7348923749823749825")))
                  "dataline-file-request"))
               ((symbol-function 'qq-core--release-send-resource)
                (lambda (resource-id) (push resource-id released))))
            (let* ((segment
                    `((type . "image")
                      (data . ((file . ,path) (name . "photo.png")))))
                   (request
                     (qq-core-send-message
                      "dataline:desktop:u_Wcc5rknRRqRO8y5gxMD6sA"
                      (list segment) nil
                      (lambda (receipt) (setq success receipt)))))
              (should (eq (qq-request-state request) 'active))
              (should ready-callback)
              (should-not sent)
              (funcall ready-callback
                       '((resource_id . "res-dataline-wait")
                         (phase . "ready")))
              (should (eq (qq-request-state request) 'settled))
              (should (equal sent "res-dataline-wait"))
              (should (equal (alist-get 'message_id success)
                             "7348923749823749825"))
              (should (equal released '("res-dataline-wait")))))
        (delete-file path)))))

(ert-deftest qq-core-private-file-stages-publishes-and-rejects-mixed-drafts ()
  (qq-core-test-with-managed-account
    (let ((path (make-temp-file "qq-core-file-" nil ".txt" "hello"))
          staged sent released)
      (unwind-protect
          (cl-letf
              (((symbol-function 'qq-resource-stage-local)
                (lambda (source name _sha callback _errback)
                  (setq staged (list source name))
                  (funcall callback
                           '((resource_id . "res-private-file")
                             (phase . "ready")))
                  "stage-request"))
               ((symbol-function 'qq-message-send-file)
                (lambda (session resource-id callback _errback)
                  (setq sent (list session resource-id))
                  (funcall callback
                           '((account_id . "slot-a")
                             (peer_uin . "10001")
                             (file_name . "notes.txt")))
                  "file-request"))
               ((symbol-function 'qq-core--release-send-resource)
                (lambda (resource-id) (push resource-id released))))
            (let ((request
                    (qq-core-send-message
                     "private:10001"
                     `(((type . "file")
                        (data . ((file . ,path) (name . "notes.txt"))))))))
              (should (eq (qq-request-state request) 'settled))
              (should (equal staged (list path "notes.txt")))
              (should (equal sent '("private:10001" "res-private-file")))
              (should (equal released '("res-private-file"))))
            (should-error
             (qq-core-send-message
              "private:10001"
              `(((type . "text") (data . ((text . "caption"))))
                ((type . "file") (data . ((file . ,path))))))
             :type 'user-error))
        (delete-file path)))))

(ert-deftest qq-core-file-cancel-retains-resource-until-publication-settles ()
  (qq-core-test-with-managed-account
    (let ((path (make-temp-file "qq-core-file-cancel-" nil ".txt" "hello"))
          publication-success released delivered)
      (unwind-protect
          (cl-letf
              (((symbol-function 'qq-resource-stage-local)
                (lambda (_source _name _sha callback _errback)
                  (funcall callback
                           '((resource_id . "res-private-file")
                             (phase . "ready")))
                  "stage-request"))
               ((symbol-function 'qq-message-send-file)
                (lambda (_session _resource-id callback _errback)
                  (setq publication-success callback)
                  "file-request"))
               ((symbol-function 'qq-core--release-send-resource)
                (lambda (resource-id) (push resource-id released))))
            (let ((request
                    (qq-core-send-message
                     "private:10001"
                     `(((type . "file")
                        (data . ((file . ,path) (name . "notes.txt")))))
                     nil (lambda (receipt) (setq delivered receipt)))))
              (should (qq-request-active-p request))
              (should publication-success)
              (should-not released)
              (should (qq-request-cancel request))
              (should (eq (qq-request-state request) 'cancelled))
              (should-not released)
              (funcall publication-success '((account_id . "slot-a")))
              (should (equal released '("res-private-file")))
              (should-not delivered)))
        (delete-file path)))))

(ert-deftest qq-core-local-images-finish-concurrently-but-send-in-draft-order ()
  (qq-core-test-with-managed-account
    (let ((path-a (make-temp-file "qq-core-image-a-" nil ".png" "aaa"))
          (path-b (make-temp-file "qq-core-image-b-" nil ".png" "bbb"))
          (owner "slot-a")
          staged sent released-resources)
      (unwind-protect
          (let ((segments
                 `(((type . "text") (data . ((text . "before"))))
                   ((type . "image")
                    (data . ((file . ,path-a)
                             (summary . "first")
                             (sub_type . 0))))
                   ((type . "text") (data . ((text . "between"))))
                   ((type . "image")
                    (data . ((file . ,path-b)
                             (summary . "second")
                             (sub_type . 1)))))))
            (cl-letf
                (((symbol-function 'qq-account-current-id)
                  (lambda () owner))
                 ((symbol-function
                   'qq-attachment-stage-and-prepare-image)
                  (lambda (session path summary sub-type callback errback)
                    (let ((operation
                           (qq-attachment-operation-create
                            :active-p t)))
                      (push (list session path summary sub-type callback
                                  errback operation)
                            staged)
                      operation)))
                 ((symbol-function 'qq-core--release-send-resource)
                  (lambda (resource-id)
                    (push resource-id released-resources)))
                 ((symbol-function 'qq-message-send)
                  (lambda (session ready-segments
                                   &optional raw callback errback optimistic)
                    (setq sent (list session ready-segments raw callback
                                     errback optimistic))
                    "send-request")))
              (let ((request
                      (qq-core-send-message
                       "group:8209413637" segments "optimistic")))
                (should (qq-request-p request))
                (should (= (length staged) 2))
                (let* ((entry-b (seq-find (lambda (entry)
                                            (equal (nth 1 entry) path-b))
                                          staged))
                       (operation-b (nth 6 entry-b)))
                  (setf (qq-attachment-operation-active-p operation-b)
                        nil)
                  (funcall
                   (nth 4 entry-b)
                   (qq-core-test-prepared-image
                    "att-bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb"
                    "res-image-b")))
                (should-not sent)
                (let* ((entry-a (seq-find (lambda (entry)
                                            (equal (nth 1 entry) path-a))
                                          staged))
                       (operation-a (nth 6 entry-a)))
                  (setf (qq-attachment-operation-active-p operation-a)
                        nil)
                  (funcall
                   (nth 4 entry-a)
                   (qq-core-test-prepared-image
                    "att-aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa"
                    "res-image-a")))
                (should (equal (qq-request-token request) "send-request"))
                (should (equal (car sent) "group:8209413637"))
                (should
                 (equal
                  (nth 1 sent)
                  '(((type . "text") (data . ((text . "before"))))
                    ((type . "image")
                     (data
                      . ((attachment_id
                          . "att-aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa"))))
                    ((type . "text") (data . ((text . "between"))))
                    ((type . "image")
                     (data
                      . ((attachment_id
                          . "att-bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb")))))))
                (should (equal (nth 2 sent) "optimistic"))
                (should (equal (nth 5 sent) segments))
                (should-not
                 (string-match-p "qq-core-image-"
                                 (prin1-to-string (nth 1 sent))))
                (should
                 (equal (sort released-resources #'string<)
                        '("res-image-a" "res-image-b"))))))
        (delete-file path-a)
        (delete-file path-b)))))

(ert-deftest qq-core-local-media-starter-signal-retires-and-releases ()
  (qq-core-test-with-managed-account
    (let ((path-a (make-temp-file "qq-core-image-ready-" nil ".png" "aaa"))
          (path-b (make-temp-file "qq-core-image-signal-" nil ".png" "bbb"))
          (qq-request--active (make-hash-table :test #'eq))
          (real-create (symbol-function 'qq-request-create))
          request caught sent failure
          released-resources released-attachments)
      (unwind-protect
          (cl-letf
              (((symbol-function 'qq-account-current-id)
                (lambda () "slot-a"))
               ((symbol-function 'qq-request-create)
                (lambda (&optional owner cancel-function)
                  (setq request
                        (funcall real-create owner cancel-function))))
               ((symbol-function
                 'qq-attachment-stage-and-prepare-image)
                (lambda (_session path _summary _sub-type callback _errback)
                  (if (equal path path-a)
                      (progn
                        ;; The first starter completes before returning.  Its
                        ;; attachment is consequently owned by the composite
                        ;; request when the next starter signals.
                        (funcall
                         callback
                         (qq-core-test-prepared-image
                          "att-aaaaaaaa-1111-4111-8111-aaaaaaaaaaaa"
                          "res-ready-before-signal"))
                        (qq-attachment-operation-create
                         :active-p nil))
                    (signal 'file-error
                            (list "staging exploded" path-b)))))
               ((symbol-function 'qq-core--release-send-resource)
                (lambda (resource-id) (push resource-id released-resources)))
               ((symbol-function 'qq-core--release-send-attachment)
                (lambda (attachment-id)
                  (push attachment-id released-attachments)))
               ((symbol-function 'qq-message-send)
                (lambda (&rest _arguments) (setq sent t))))
            (condition-case error-data
                (qq-core-send-message
                 "private:10001"
                 `(((type . "image") (data . ((file . ,path-a))))
                   ((type . "image") (data . ((file . ,path-b)))))
                 nil nil
                 (lambda (body reason) (setq failure (list body reason))))
              (file-error (setq caught error-data)))
            (should (equal caught
                           (list 'file-error "staging exploded" path-b)))
            (should (qq-request-p request))
            (should (eq (qq-request-state request) 'failed))
            (should-not (qq-request-active-p request))
            (should (= (hash-table-count qq-request--active) 0))
            (should-not sent)
            ;; A synchronous Lisp signal is not translated into the async
            ;; ERRBACK contract.
            (should-not failure)
            (should (equal released-resources
                           '("res-ready-before-signal")))
            (should
             (equal released-attachments
                    '("att-aaaaaaaa-1111-4111-8111-aaaaaaaaaaaa"))))
        (delete-file path-a)
        (delete-file path-b)))))

(ert-deftest qq-core-local-media-starter-quit-cleans-composite-ownership ()
  (qq-core-test-with-managed-account
    (let ((path-a (make-temp-file "qq-core-image-ready-" nil ".png" "aaa"))
          (path-b (make-temp-file "qq-core-image-ready-" nil ".png" "bbb"))
          (path-c (make-temp-file "qq-core-image-quit-" nil ".png" "ccc"))
          (qq-request--active (make-hash-table :test #'eq))
          (real-create (symbol-function 'qq-request-create))
          request operations late-ready caught sent failure
          released-resources released-attachments)
      (unwind-protect
          (cl-letf
              (((symbol-function 'qq-account-current-id)
                (lambda () "slot-a"))
               ((symbol-function 'qq-request-create)
                (lambda (&optional owner cancel-function)
                  (setq request
                        (funcall real-create owner cancel-function))))
               ((symbol-function
                 'qq-attachment-stage-and-prepare-image)
                (lambda (_session path _summary _sub-type callback _errback)
                  (cond
                   ((equal path path-c)
                    (setq late-ready callback)
                    (signal 'quit nil))
                   (t
                    (funcall
                     callback
                     (if (equal path path-a)
                         (qq-core-test-prepared-image
                          "att-aaaaaaaa-1111-4111-8111-aaaaaaaaaaaa"
                          "res-ready-a")
                       (qq-core-test-prepared-image
                        "att-bbbbbbbb-1111-4111-8111-bbbbbbbbbbbb"
                        "res-ready-b")))
                    (let ((operation
                           (qq-attachment-operation-create
                            :active-p t)))
                      (push operation operations)
                      operation)))))
               ((symbol-function 'qq-core--release-send-resource)
                (lambda (resource-id) (push resource-id released-resources)))
               ((symbol-function 'qq-core--release-send-attachment)
                (lambda (attachment-id)
                  (push attachment-id released-attachments)))
               ((symbol-function 'qq-message-send)
                (lambda (&rest _arguments) (setq sent t))))
            (condition-case error-data
                (qq-core-send-message
                 "private:10001"
                 `(((type . "image") (data . ((file . ,path-a))))
                   ((type . "image") (data . ((file . ,path-b))))
                   ((type . "image") (data . ((file . ,path-c)))))
                 nil nil
                 (lambda (body reason) (setq failure (list body reason))))
              (quit (setq caught error-data)))
            (should (equal caught '(quit)))
            (should (qq-request-p request))
            (should (eq (qq-request-state request) 'failed))
            (should-not (qq-request-active-p request))
            (should (= (hash-table-count qq-request--active) 0))
            (should (= (length operations) 2))
            (dolist (operation operations)
              (should-not
               (qq-attachment-operation-active-p operation)))
            (should-not sent)
            (should-not failure)
            ;; A late completion is inert and releases the objects it produced.
            (funcall
             late-ready
             (qq-core-test-prepared-image
              "att-cccccccc-1111-4111-8111-cccccccccccc"
              "res-ready-c"))
            (should-not sent)
            (should
             (equal (sort released-resources #'string<)
                    '("res-ready-a" "res-ready-b" "res-ready-c")))
            (should
             (equal
              (sort released-attachments #'string<)
              '("att-aaaaaaaa-1111-4111-8111-aaaaaaaaaaaa"
                "att-bbbbbbbb-1111-4111-8111-bbbbbbbbbbbb"
                "att-cccccccc-1111-4111-8111-cccccccccccc"))))
        (delete-file path-a)
        (delete-file path-b)
        (delete-file path-c)))))

(ert-deftest qq-core-favorite-materializes-through-image-attachment-in-order ()
  (qq-core-test-with-managed-account
    (let* ((favorite-id
            "10002_0_0_0_DEADBEEFDEADBEEFDEADBEEFDEADBEEF_0_0")
           (segments
            `(((type . "text") (data . ((text . "before"))))
              ((type . "favorite_emoji")
               (data . ((favorite_emoji_id . ,favorite-id))))
              ((type . "face") (data . ((id . "178"))))))
           operation prepared sent released-resources)
      (cl-letf
          (((symbol-function
             'qq-attachment-materialize-and-prepare-favorite)
            (lambda (session requested-id callback errback)
              (setq prepared (list session requested-id callback errback)
                    operation
                    (qq-attachment-operation-create :active-p t))
              operation))
           ((symbol-function 'qq-core--release-send-resource)
            (lambda (resource-id) (push resource-id released-resources)))
           ((symbol-function 'qq-message-send)
            (lambda (session ready-segments
                             &optional raw callback errback optimistic)
              (setq sent (list session ready-segments raw callback
                               errback optimistic))
              "send-favorite-request")))
        (let ((request
                (qq-core-send-message
                 "group:8209413637" segments "optimistic")))
          (should (qq-request-p request))
          (should (equal (seq-take prepared 2)
                         (list "group:8209413637" favorite-id)))
          (setf (qq-attachment-operation-active-p operation) nil)
          (funcall
           (nth 2 prepared)
           (qq-core-test-prepared-image
            "att-ffffffff-ffff-4fff-8fff-ffffffffffff"
            "res-favorite-ready"))
          (should
           (equal
            (nth 1 sent)
            '(((type . "text") (data . ((text . "before"))))
              ((type . "image")
               (data
                . ((attachment_id
                    . "att-ffffffff-ffff-4fff-8fff-ffffffffffff"))))
              ((type . "face") (data . ((id . "178")))))))
          (should (equal (nth 5 sent) segments))
          (should (equal released-resources '("res-favorite-ready")))
          (funcall (nth 3 sent) '((sent . t)))
          (should (eq (qq-request-state request) 'settled)))))))

(ert-deftest qq-core-local-record-is-prepared-before-message-send ()
  (qq-core-test-with-managed-account
    (let ((path (make-temp-file "qq-core-record-" nil ".wav" "pcm"))
          (owner "slot-a")
          operation prepared sent released-resources released-attachments)
      (unwind-protect
          (let ((segments
                 `(((type . "text") (data . ((text . "voice:"))))
                   ((type . "record")
                    (data . ((file . ,path) (name . "voice.wav")))))))
            (cl-letf
                (((symbol-function 'qq-account-current-id)
                  (lambda () owner))
                 ((symbol-function
                   'qq-attachment-stage-and-prepare-record)
                  (lambda (session record-path callback errback)
                    (setq prepared
                          (list session record-path callback errback)
                          operation
                          (qq-attachment-operation-create :active-p t))
                    operation))
                 ((symbol-function 'qq-core--release-send-resource)
                  (lambda (resource-id) (push resource-id released-resources)))
                 ((symbol-function 'qq-core--release-send-attachment)
                  (lambda (attachment-id)
                    (push attachment-id released-attachments)))
                 ((symbol-function 'qq-message-send)
                  (lambda (session ready-segments
                                   &optional raw callback errback optimistic)
                    (setq sent (list session ready-segments raw callback
                                     errback optimistic))
                    "send-record-request")))
              (let ((request
                      (qq-core-send-message
                       "private:10001" segments "optimistic")))
                (should (qq-request-p request))
                (should (equal (car prepared) "private:10001"))
                (should (equal (cadr prepared) path))
                (setf (qq-attachment-operation-active-p operation) nil)
                (funcall
                 (nth 2 prepared)
                 (qq-core-test-prepared-image
                  "att-dddddddd-dddd-4ddd-8ddd-dddddddddddd"
                  "res-record-ready"))
                (should (equal (qq-request-token request)
                               "send-record-request"))
                (should
                 (equal
                  (nth 1 sent)
                  '(((type . "text") (data . ((text . "voice:"))))
                    ((type . "record")
                     (data
                      . ((attachment_id
                          . "att-dddddddd-dddd-4ddd-8ddd-dddddddddddd")))))))
                (should (equal (nth 5 sent) segments))
                (should-not
                 (string-match-p "qq-core-record-"
                                 (prin1-to-string (nth 1 sent))))
                (should (equal released-resources '("res-record-ready")))
                (funcall (nth 3 sent) '((sent . t)))
                (should (eq (qq-request-state request) 'settled))
                (should-not released-attachments))))
        (delete-file path)))))

(ert-deftest qq-core-local-video-is-prepared-with-thumbnail-before-send ()
  (qq-core-test-with-managed-account
    (let ((path (make-temp-file "qq-core-video-" nil ".mp4" "mp4"))
          (owner "slot-a")
          operation prepared sent released-resources)
      (unwind-protect
          (let ((segments
                 `(((type . "text") (data . ((text . "video:"))))
                   ((type . "video")
                    (data . ((file . ,path) (name . "clip.mp4")))))))
            (cl-letf
                (((symbol-function 'qq-account-current-id)
                  (lambda () owner))
                 ((symbol-function
                   'qq-attachment-stage-and-prepare-video)
                  (lambda (session video-path callback errback)
                    (setq prepared
                          (list session video-path callback errback)
                          operation
                          (qq-attachment-operation-create :active-p t))
                    operation))
                 ((symbol-function 'qq-core--release-send-resource)
                  (lambda (resource-id) (push resource-id released-resources)))
                 ((symbol-function 'qq-message-send)
                  (lambda (session ready-segments
                                   &optional raw callback errback optimistic)
                    (setq sent (list session ready-segments raw callback
                                     errback optimistic))
                    "send-video-request")))
              (let ((request
                      (qq-core-send-message
                       "group:8209413637" segments "optimistic")))
                (should (qq-request-p request))
                (should (equal (car prepared) "group:8209413637"))
                (should (equal (cadr prepared) path))
                (setf (qq-attachment-operation-active-p operation) nil)
                (funcall
                 (nth 2 prepared)
                 (qq-core-test-prepared-video
                  "att-eeeeeeee-eeee-4eee-8eee-eeeeeeeeeeee"
                  "res-video-ready" "res-video-thumbnail-ready"))
                (should (equal (qq-request-token request)
                               "send-video-request"))
                (should
                 (equal
                  (nth 1 sent)
                  '(((type . "text") (data . ((text . "video:"))))
                    ((type . "video")
                     (data
                      . ((attachment_id
                          . "att-eeeeeeee-eeee-4eee-8eee-eeeeeeeeeeee")))))))
                (should (equal (nth 5 sent) segments))
                (should-not
                 (string-match-p "qq-core-video-"
                                 (prin1-to-string (nth 1 sent))))
                (should
                 (equal (sort released-resources #'string<)
                        '("res-video-ready"
                          "res-video-thumbnail-ready")))
                (funcall (nth 3 sent) '((sent . t)))
                (should (eq (qq-request-state request) 'settled)))))
        (delete-file path)))))

(ert-deftest qq-core-local-image-cancel-stops-before-message-dispatch ()
  (let ((path (make-temp-file "qq-core-image-cancel-" nil ".png" "abc"))
        (owner "slot-a")
        operation sent)
    (unwind-protect
        (cl-letf
            (((symbol-function 'qq-account-current-id)
              (lambda () owner))
             ((symbol-function
               'qq-attachment-stage-and-prepare-image)
              (lambda (&rest _arguments)
                (setq operation
                      (qq-attachment-operation-create :active-p t))))
             ((symbol-function 'qq-message-send)
              (lambda (&rest _arguments) (setq sent t))))
          (let ((request
                  (qq-core-send-message
                   "private:10001"
                   `(((type . "image") (data . ((file . ,path))))))))
            (should (qq-attachment-operation-active-p operation))
            (qq-request-cancel request)
            (should-not
             (qq-attachment-operation-active-p operation))
            (should-not sent)))
      (delete-file path))))

(ert-deftest qq-core-local-image-account-switch-releases-completion ()
  (let ((path (make-temp-file "qq-core-image-owner-" nil ".png" "abc"))
        (owner "slot-a")
        operation ready-callback sent failure
        released-resources released-attachments)
    (unwind-protect
        (cl-letf
            (((symbol-function 'qq-account-current-id)
              (lambda () owner))
             ((symbol-function
               'qq-attachment-stage-and-prepare-image)
              (lambda (_session _path _summary _sub-type callback _errback)
                (setq ready-callback callback
                      operation
                      (qq-attachment-operation-create :active-p t))
                operation))
             ((symbol-function 'qq-core--release-send-resource)
              (lambda (resource-id) (push resource-id released-resources)))
             ((symbol-function 'qq-core--release-send-attachment)
              (lambda (attachment-id)
                (push attachment-id released-attachments)))
             ((symbol-function 'qq-message-send)
              (lambda (&rest _arguments) (setq sent t))))
          (let ((request
                  (qq-core-send-message
                   "private:10001"
                   `(((type . "image") (data . ((file . ,path)))))
                   nil nil
                   (lambda (body reason) (setq failure (list body reason))))))
            (setq owner "slot-b")
            (setf (qq-attachment-operation-active-p operation) nil)
            (funcall
             ready-callback
             (qq-core-test-prepared-image
              "att-cccccccc-cccc-4ccc-8ccc-cccccccccccc"
              "res-image-owner"))
            (should (eq (qq-request-state request) 'cancelled)))
          (should-not sent)
          (should-not failure)
          (should (equal released-resources '("res-image-owner")))
          (should
           (equal released-attachments
                  '("att-cccccccc-cccc-4ccc-8ccc-cccccccccccc"))))
      (delete-file path))))

(ert-deftest qq-core-local-image-preflight-failure-releases-prepared-object ()
  (qq-core-test-with-managed-account
    (let ((path (make-temp-file "qq-core-image-preflight-" nil ".png" "abc"))
          ready-callback operation failure
          released-resources released-attachments)
      (unwind-protect
          (cl-letf
              (((symbol-function 'qq-account-current-id)
                (lambda () "slot-a"))
               ((symbol-function
                 'qq-attachment-stage-and-prepare-image)
                (lambda (_session _path _summary _sub-type callback _errback)
                  (setq ready-callback callback
                        operation
                        (qq-attachment-operation-create :active-p t))
                  operation))
               ((symbol-function 'qq-core--release-send-resource)
                (lambda (resource-id) (push resource-id released-resources)))
               ((symbol-function 'qq-core--release-send-attachment)
                (lambda (attachment-id)
                  (push attachment-id released-attachments)))
               ((symbol-function 'qq-message-send)
                (lambda (_session _segments &optional _raw _callback errback
                                  _optimistic)
                  (funcall errback
                           '((code . "capability_unavailable"))
                           "message.send unavailable")
                  nil)))
            (let ((request
                    (qq-core-send-message
                     "private:10001"
                     `(((type . "image") (data . ((file . ,path)))))
                     nil nil
                     (lambda (body reason) (setq failure (list body reason))))))
              (setf (qq-attachment-operation-active-p operation) nil)
              (funcall
               ready-callback
               (qq-core-test-prepared-image
                "att-eeeeeeee-eeee-4eee-8eee-eeeeeeeeeeee"
                "res-image-preflight"))
              (should (eq (qq-request-state request) 'failed))))
        (delete-file path))
      (should (equal released-resources '("res-image-preflight")))
      (should
       (equal released-attachments
              '("att-eeeeeeee-eeee-4eee-8eee-eeeeeeeeeeee")))
      (should (equal (cadr failure) "message.send unavailable")))))

(ert-deftest qq-core-local-image-async-send-failure-releases-submitted-object ()
  (qq-core-test-with-managed-account
    (let ((path (make-temp-file "qq-core-image-async-failure-" nil ".png" "abc"))
          ready-callback operation send-errback failure
          released-resources released-attachments)
      (unwind-protect
          (cl-letf
              (((symbol-function 'qq-account-current-id)
                (lambda () "slot-a"))
               ((symbol-function
                 'qq-attachment-stage-and-prepare-image)
                (lambda (_session _path _summary _sub-type callback _errback)
                  (setq ready-callback callback
                        operation
                        (qq-attachment-operation-create :active-p t))
                  operation))
               ((symbol-function 'qq-core--release-send-resource)
                (lambda (resource-id) (push resource-id released-resources)))
               ((symbol-function 'qq-core--release-send-attachment)
                (lambda (attachment-id)
                  (push attachment-id released-attachments)))
               ((symbol-function 'qq-message-send)
                (lambda (_session _segments &optional _raw _callback errback
                                  _optimistic)
                  (setq send-errback errback)
                  "send-async-failure")))
            (let ((request
                    (qq-core-send-message
                     "private:10001"
                     `(((type . "reply")
                        (data . ((id . "7348923749823749823"))))
                       ((type . "image") (data . ((file . ,path)))))
                     nil nil
                     (lambda (body reason) (setq failure (list body reason))))))
              (setf (qq-attachment-operation-active-p operation) nil)
              (funcall
               ready-callback
               (qq-core-test-prepared-image
                "att-ffffffff-eeee-4eee-8eee-ffffffffffff"
                "res-image-async-failure"))
              (should (equal (qq-request-token request)
                             "send-async-failure"))
              (should (equal released-resources
                             '("res-image-async-failure")))
              (should-not released-attachments)
              (funcall send-errback
                       '((code . "message_reference_unknown"))
                       "reply target is unknown")
              (should (eq (qq-request-state request) 'failed))
              (should
               (equal released-attachments
                      '("att-ffffffff-eeee-4eee-8eee-ffffffffffff")))
              (should (equal (cadr failure) "reply target is unknown"))
              ;; A duplicate terminal callback cannot release the ID twice.
              (funcall send-errback
                       '((code . "message_reference_unknown"))
                       "reply target is unknown")
              (should
               (equal released-attachments
                      '("att-ffffffff-eeee-4eee-8eee-ffffffffffff")))))
        (delete-file path)))))

(ert-deftest qq-core-url-only-image-fails-before-staging ()
  (let (staged sent)
    (cl-letf (((symbol-function
                'qq-attachment-stage-and-prepare-image)
               (lambda (&rest _arguments) (setq staged t)))
              ((symbol-function 'qq-message-send)
               (lambda (&rest _arguments) (setq sent t))))
      (should-error
       (qq-core-send-message
        "private:10001"
        '(((type . "image")
           (data . ((url . "https://example.invalid/changeable.png"))))))
       :type 'user-error)
      (should-not staged)
      (should-not sent))))

(ert-deftest qq-core-poke-routes-exact-target ()
  (let ((qq-account--current-account-id "slot-a") call)
    (cl-letf (((symbol-function 'qq-message-send-poke)
               (lambda (session-key target-id &optional callback errback)
                 (setq call (list session-key target-id callback errback))
                 "poke-request")))
      (let ((request
              (qq-core-send-poke "group:8209413637" "10002")))
        (should (qq-request-p request))
        (should (equal (qq-request-token request) "poke-request")))
      (should (equal (nth 0 call) "group:8209413637"))
      (should (equal (nth 1 call) "10002"))
      (should (functionp (nth 3 call))))))

(ert-deftest qq-core-reaction-routes-whole-message ()
  (let ((qq-account--current-account-id "slot-a") call)
    (cl-letf (((symbol-function 'qq-message-set-reaction)
               (lambda (message reaction set &optional callback errback)
                 (setq call (list message reaction set callback errback))
                 "reaction-request")))
      (let ((message
             '((session-key . "group:8209413637")
               (server-id . "7348923749823749823")
               (message-seq . "9007199254740999"))))
        (let ((request
                (qq-core-set-message-reaction
                 message '((emoji-id . "178") (emoji-type . "1")) t)))
          (should (qq-request-p request))
          (should (equal (qq-request-token request)
                         "reaction-request")))
        (should (eq (nth 0 call) message))
        (should (equal (nth 1 call)
                       '((emoji-id . "178") (emoji-type . "1"))))
        (should (eq (nth 2 call) t))
        (should (functionp (nth 4 call)))))))

(ert-deftest qq-core-read-reports-coalesce-to-newest-timeline-message ()
  (qq-core-test-with-managed-account
    (let ((qq-core--read-operations (make-hash-table :test #'equal))
          (qq-request--active (make-hash-table :test #'eq))
          calls completed timeline)
      (cl-letf (((symbol-function 'qq-account-current-id)
                 (lambda () "slot-a"))
                ((symbol-function 'qq-core-message-read-capable-p)
                 (lambda (message)
                   (equal (qq-account-current-id)
                          (alist-get 'gateway-account-id message))))
                ((symbol-function 'qq-state-session-messages)
                 (lambda (_session-key) timeline))
                ((symbol-function 'qq-message-mark-read)
                 (lambda (message &optional callback errback)
                   (setq calls
                         (append calls
                                 (list (list message callback errback))))
                   (format "read-%d" (length calls)))))
        (let* ((base
                '((session-key . "group:8209413637")
                  (server-id . "7348923749823749823")
                  (canonical-row-key . "101")
                  (group-id . "8209413637")
                  (gateway-account-id . "slot-a")))
               (newest (copy-tree base))
               (middle (copy-tree base)))
          (setf (alist-get 'server-id newest) nil
                (alist-get 'canonical-row-key newest) "103"
                (alist-get 'server-id middle) "7348923749823749824"
                (alist-get 'canonical-row-key middle) "102")
          (setq timeline (list base middle newest))
          (let ((request
                  (qq-core-mark-message-read
                   base (lambda (_receipt) (push 'base completed)))))
            (should (qq-request-p request))
            (should
             (eq
              (qq-core-mark-message-read
               newest (lambda (_receipt) (push 'newest completed)))
              request))
            ;; A later-but-not-newest intent cannot replace the queued frontier.
            (should
             (eq
              (qq-core-mark-message-read
               middle (lambda (_receipt) (push 'middle completed)))
              request)))
          (let ((other-account (copy-tree newest)))
            (setf (alist-get 'canonical-row-key other-account) "104"
                  (alist-get 'gateway-account-id other-account) "slot-b")
            (should-error
             (qq-core-mark-message-read other-account #'ignore)
             :type 'user-error))
          (should (= (length calls) 1))
          (funcall
           (nth 1 (car calls))
           '((account_id . "slot-a")
             (row_key . "101")))
          (should (= (length calls) 2))
          (should (eq (car (cadr calls)) newest))
          (funcall
           (nth 1 (cadr calls))
           '((account_id . "slot-a")
             (row_key . "103")))
          ;; MIDDLE never reached the wire and therefore owns no callback result.
          (should (equal (nreverse completed) '(base newest)))
          (should (= (hash-table-count qq-core--read-operations) 0)))))))

(ert-deftest qq-core-read-callback-reentry-sees-live-stable-successor ()
  (qq-core-test-with-managed-account
    (let ((qq-core--read-operations (make-hash-table :test #'equal))
          (qq-request--active (make-hash-table :test #'eq))
          calls cancelled reentered live-successor queued-callback
          reentered-active-p reentered-is-successor-p timeline)
      (cl-letf (((symbol-function 'qq-account-current-id)
                 (lambda () "slot-a"))
                ((symbol-function 'qq-core-message-read-capable-p)
                 (lambda (_message) t))
                ((symbol-function 'qq-server-cancel)
                 (lambda (token) (push token cancelled)))
                ((symbol-function 'qq-state-session-messages)
                 (lambda (_session-key) timeline))
                ((symbol-function 'qq-message-mark-read)
                 (lambda (message &optional callback errback)
                   (setq calls
                         (append calls
                                 (list (list message callback errback))))
                   (format "read-%d" (length calls)))))
        (let* ((base
                '((session-key . "group:8209413637")
                  (server-id . "7348923749823749823")
                  (canonical-row-key . "201")
                  (group-id . "8209413637")
                  (gateway-account-id . "slot-a")))
               (newest (copy-tree base))
               request)
          (setf (alist-get 'server-id newest) nil
                (alist-get 'canonical-row-key newest) "202")
          (setq timeline (list base newest))
          (setq request
                (qq-core-mark-message-read
                 base
                 (lambda (_receipt)
                   (setq live-successor
                         (gethash
                          (qq-core--read-operation-key
                           "slot-a" "group:8209413637")
                          qq-core--read-operations)
                         reentered
                         (qq-core-mark-message-read newest))
                   (setq reentered-is-successor-p
                         (and
                          (eq reentered request)
                          (eq reentered
                              (qq-core--read-operation-request
                               live-successor)))
                         reentered-active-p
                         (qq-request-active-p reentered))
                   (qq-request-cancel reentered))))
          (should
           (eq (qq-core-mark-message-read
                newest (lambda (_receipt) (setq queued-callback t)))
               request))
          (funcall (nth 1 (car calls))
                   '((account_id . "slot-a")
                     (row_key . "201")))
          (should (= (length calls) 2))
          (should (eq reentered request))
          (should reentered-is-successor-p)
          (should reentered-active-p)
          (should (equal cancelled '("read-2")))
          (should (eq (qq-request-state request) 'cancelled))
          (should-not queued-callback)
          (should (= (hash-table-count qq-core--read-operations) 0))
          ;; The canceled successor's late completion is inert and cannot
          ;; dispatch a hidden third request.
          (funcall (nth 1 (cadr calls))
                   '((account_id . "slot-a")
                     (row_key . "202")))
          (should (= (length calls) 2)))))))

(ert-deftest qq-core-account-selection-preserves-both-read-operations ()
  (qq-core-test-with-managed-account
    (qq-account--upsert-account (qq-core-test-account-b) 'added)
    (let ((qq-core--read-operations (make-hash-table :test #'equal))
          (qq-request--active (make-hash-table :test #'eq))
          calls canceled completed)
      (cl-letf
          (((symbol-function 'qq-core-message-read-capable-p)
            (lambda (message)
              (equal (qq-runtime-current-account-id)
                     (alist-get 'gateway-account-id message))))
           ((symbol-function 'qq-server-cancel)
            (lambda (request) (push request canceled)))
           ((symbol-function 'qq-message-mark-read)
            (lambda (message &optional callback errback)
              (setq calls
                    (append calls
                            (list (list message callback errback))))
              (format "read-%d" (length calls)))))
        (let ((message-a
               '((session-key . "group:8209413637")
                 (server-id . "7348923749823749823")
                 (canonical-row-key . "301")
                 (group-id . "8209413637")
                 (gateway-account-id . "slot-a")))
              (message-b
               '((session-key . "group:8209413637")
                 (server-id . "7348923749823749824")
                 (canonical-row-key . "302")
                 (group-id . "8209413637")
                 (gateway-account-id . "slot-b"))))
          (qq-runtime-with-account "slot-a"
            (qq-core-mark-message-read
             message-a (lambda (_receipt) (push 'a completed))))
          (qq-account--set-current-account "slot-b")
          (qq-runtime-with-account "slot-b"
            (qq-core-mark-message-read
             message-b (lambda (_receipt) (push 'b completed))))
          (should (= (length calls) 2))
          (should (= (hash-table-count qq-core--read-operations) 2))
          (funcall (nth 1 (car calls))
                   '((account_id . "slot-a")
                     (row_key . "301")))
          (should (equal completed '(a)))
          (should (= (hash-table-count qq-core--read-operations) 1))
          (funcall (nth 1 (cadr calls))
                   '((account_id . "slot-b")
                     (row_key . "302")))
          (should (equal completed '(b a)))
          (should-not canceled)
          (should (= (hash-table-count qq-core--read-operations) 0)))))))

(ert-deftest qq-core-read-coalescer-survives-same-slot-runtime-restart ()
  (qq-core-test-with-managed-account
    (let ((qq-core--read-operations (make-hash-table :test #'equal))
          (qq-request--active (make-hash-table :test #'eq))
          (current-account-id "slot-a")
          calls cancelled failures completed timeline)
      (cl-letf (((symbol-function 'qq-account-current-id)
                 (lambda () current-account-id))
                ((symbol-function 'qq-core-message-read-capable-p)
                 (lambda (message)
                   (equal current-account-id
                          (alist-get 'gateway-account-id message))))
                ((symbol-function 'qq-server-cancel)
                 (lambda (token) (push token cancelled)))
                ((symbol-function 'qq-state-session-messages)
                 (lambda (_session-key) timeline))
                ((symbol-function 'qq-message-mark-read)
                 (lambda (message &optional callback errback)
                   (setq calls
                         (append calls (list (list message callback errback))))
                   (format "read-%d" (length calls)))))
        (let* ((old
                '((session-key . "group:8209413637")
                  (server-id . "7348923749823749823")
                  (canonical-row-key . "401")
                  (group-id . "8209413637")
                  (gateway-account-id . "slot-a")))
               (new (copy-tree old))
               (request
                 (qq-core-mark-message-read
                  old #'ignore
                  (lambda (_body reason) (push reason failures)))))
          (setf (alist-get 'server-id new) nil
                (alist-get 'canonical-row-key new) "402")
          (setq timeline (list old new))
          ;; A registry update for the same stable slot must not revoke the
          ;; coalescer's outer request.
          (qq-core--revoke-stale-read-operations)
          (should (qq-request-active-p request))
          (should-not cancelled)
          (should (eq (qq-core-mark-message-read
                       new (lambda (_receipt) (push 'new completed)))
                      request))
          ;; An in-flight adapter failure reports through the stable outer
          ;; operation and advances its queued cursor.
          (funcall (nth 2 (car calls))
                   '((code . "stale_request")) "runtime changed")
          (should (equal failures '("runtime changed")))
          (should (= (length calls) 2))
          (should (eq (car (cadr calls)) new))
          (funcall (nth 1 (cadr calls))
                   '((account_id . "slot-a")
                     (row_key . "402")))
          (should (equal completed '(new)))
          (should (= (hash-table-count qq-core--read-operations) 0)))))))

(ert-deftest qq-core-read-capability-requires-current-account-slot ()
  (qq-core-test-with-managed-account
    (let ((message
           '((session-key . "private:10001")
             (server-id . "7348923749823749823")
             (canonical-row-key . "501")
             (gateway-account-id . "slot-a"))))
      (cl-letf (((symbol-function 'qq-core-ready-p) (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () '("message.mark_read")))
                ((symbol-function 'qq-account-current-id)
                 (lambda () "slot-a"))
                ((symbol-function 'qq-account-current)
                 (lambda () '((phase . "online")))))
        (should (qq-core-message-read-capable-p message))
        (let ((other-account (copy-tree message)))
          (setf (alist-get 'gateway-account-id other-account) "slot-b")
          (should-not (qq-core-message-read-capable-p other-account)))
        (let ((missing-row (copy-tree message)))
          (setf (alist-get 'canonical-row-key missing-row) nil)
          (should-not (qq-core-message-read-capable-p missing-row)))
        (let ((service (copy-tree message)))
          (setf (alist-get 'session-key service) "service:u_peer")
          (should-not (qq-core-message-read-capable-p service)))))))

(ert-deftest qq-core-capabilities-follow-negotiated-methods ()
  (cl-letf (((symbol-function 'qq-server-capabilities)
             (lambda () '("message.send" "message.mark_read"))))
    (should (qq-core-implemented-p 'presence))
    (should-not (qq-core-supports-p 'presence))
    (should (qq-core-supports-p 'send-message))
    (should (qq-core-supports-p 'read-receipt))
    (should-not (qq-core-supports-p 'dataline-file))
    (should-not (qq-core-implemented-p 'chat-action))
    (should-not (qq-core-supports-p 'chat-action)))
  (cl-letf (((symbol-function 'qq-server-capabilities)
             (lambda () '("dataline.send_file" "resource.stage_local"))))
    (should (qq-core-supports-p 'dataline-file))))

(ert-deftest qq-core-essence-routes-whole-message ()
  (let ((qq-account--current-account-id "slot-a") call)
    (cl-letf (((symbol-function 'qq-message-set-essence)
               (lambda (message set &optional callback errback)
                 (setq call (list message set callback errback))
                 "essence-request")))
      (let ((message
             '((session-key . "group:8209413637")
               (server-id . "7348923749823749823")
               (message-seq . "9007199254740999")
               (native-random . 7))))
        (let ((request (qq-core-set-message-essence message t)))
          (should (qq-request-p request))
          (should (equal (qq-request-token request)
                         "essence-request")))
        (should (eq (nth 0 call) message))
        (should (eq (nth 1 call) t))
        (should (functionp (nth 3 call)))))))

(ert-deftest qq-core-todo-routes-whole-message-and-operation ()
  (let ((qq-account--current-account-id "slot-a") calls)
    (cl-letf (((symbol-function 'qq-message-set-todo)
               (lambda (message operation &optional callback errback)
                 (push (list message operation callback errback) calls)
                 (format "todo-%s" operation))))
      (let ((message
             '((session-key . "group:8209413637")
               (server-id . "7348923749823749823")
               (message-seq . "9007199254740999"))))
        (dolist (operation '(set complete cancel))
          (let ((request (qq-core-set-message-todo message operation)))
            (should (qq-request-p request))
            (should (equal (qq-request-token request)
                           (format "todo-%s" operation)))))
        (dolist (call calls)
          (should (eq (nth 0 call) message))
          (should (functionp (nth 3 call)))))
      (should-error
       (qq-core-set-message-todo
        '((session-key . "group:8209413637")
          (server-id . "7348923749823749823"))
        'finish)
       :type 'user-error))))

(ert-deftest qq-core-poke-recall-routes-whole-message ()
  (let ((qq-account--current-account-id "slot-a") call)
    (cl-letf (((symbol-function 'qq-message-poke-recall-capable-p)
               (lambda (_message) t))
              ((symbol-function 'qq-message-recall-poke)
               (lambda (message &optional callback errback)
                 (setq call (list message callback errback))
                 "poke-recall-request")))
      (let ((message
             '((session-key . "group:8209413637")
               (server-id . "7348923749823749823")
               (timeline-class . service)
               (segments
                . (((type . "gray-tip")
                    (data . ((kind . "poke")))))))))
        (let ((request (qq-core-recall-poke message)))
          (should (qq-request-p request))
          (should (equal (qq-request-token request)
                         "poke-recall-request")))
        (should (eq (car call) message))
        (should (functionp (nth 2 call)))))))

(ert-deftest qq-core-recall-dispatches-the-owned-native-message ()
  (let ((qq-account--current-account-id "slot-a") call)
    (cl-letf (((symbol-function 'qq-message-recall-capable-p)
               (lambda (_message) t))
              ((symbol-function 'qq-message-recall)
               (lambda (session-key message &optional callback errback)
                 (setq call (list session-key message callback errback))
                 "recall-request")))
      (let ((message
             '((session-key . "group:8209413637")
               (server-id . "7348923749823749823")
               (message-seq . "99"))))
        (let ((request (qq-core-recall-message message)))
          (should (qq-request-p request))
          (should (equal (qq-request-token request)
                         "recall-request")))
        (should (equal (car call) "group:8209413637"))
        (should (eq (cadr call) message))))))

(ert-deftest qq-core-history-frontier-prefers-newer-live-sequence ()
  (progn
    (cl-letf (((symbol-function 'qq-state-group)
               (lambda (_group-id) '((latest_sequence . "9007199254740999"))))
              ((symbol-function 'qq-message-live-frontier)
               (lambda (_session-key)
                 '((sequence . "9007199254741001"))))
              ((symbol-function 'qq-state-session-messages)
               (lambda (_session-key) nil)))
      (let ((frontier
             (qq-core-history-frontier "group:8209413637")))
        (should (equal (plist-get frontier :sequence)
                       "9007199254741001"))
        (should-not (plist-get frontier :message-id))
        (should (eq (plist-get frontier :source) 'live-event))))))

(ert-deftest qq-core-latest-history-uses-one-conversation-neutral-port ()
  (qq-core-test-with-managed-account
    (let (call callback-meta)
      (cl-letf (((symbol-function 'qq-message--request-history-page)
                 (lambda (session cursor direction callback _errback count)
                   (setq call (list session cursor direction count))
                   (funcall
                    callback
                    (list :history-port-version qq-core-history-port-version
                          :history-account-id "slot-a"
                          :history-session-key session
                          :message-count 0))
                   "history-page")))
        (let ((request
                (qq-core-fetch-history-page
                 "group:8209413637" nil 'older
                 (lambda (meta) (setq callback-meta meta)) nil 20)))
          (should (qq-request-p request))
          (should
           (equal call '("group:8209413637" nil older 20)))
          (should (= (plist-get callback-meta :message-count) 0)))))))

(ert-deftest qq-core-history-response-retains-the-request-account ()
  (qq-core-test-with-managed-account
    (let (driver-callback callback-meta)
      (cl-letf (((symbol-function 'qq-message--request-history-page)
                 (lambda (_session _cursor _direction callback _errback _count)
                   (setq driver-callback callback)
                   "history-page")))
        (qq-core-fetch-history-page
         "group:8209413637" nil 'older
         (lambda (meta) (setq callback-meta meta)))
        ;; Response ownership must not be reconstructed from whichever
        ;; account happens to be selected when an asynchronous driver ends.
        (setq qq-account--current-account-id nil)
        (funcall
         driver-callback
         (list :history-port-version qq-core-history-port-version
               :history-account-id "slot-a"
               :history-session-key "group:8209413637"
               :history-older-cursor '((opaque . "older"))
               :history-newer-cursor '((opaque . "newer"))))
        (should (equal (plist-get callback-meta :history-account-id)
                       "slot-a"))
        (should
         (equal
          (plist-get callback-meta :history-older-cursor)
          '((opaque . "older"))))
        (should
         (equal
          (plist-get callback-meta :history-newer-cursor)
          '((opaque . "newer"))))))))

(ert-deftest qq-core-private-latest-does-not-select-a-driver ()
  (qq-core-test-with-managed-account
    (let (callback-meta call)
      (cl-letf (((symbol-function 'qq-message--request-history-page)
                 (lambda (session cursor direction callback _errback count)
                   (setq call (list session cursor direction count))
                   (funcall
                    callback
                    (list :history-port-version qq-core-history-port-version
                          :history-account-id "slot-a"
                          :history-session-key session
                          :message-count 0))
                   "history-page")))
        (let ((request
                (qq-core-fetch-history-page
                 "private:10001" nil 'older
                 (lambda (meta) (setq callback-meta meta))
                 nil 20)))
          (should (qq-request-p request)))
        (should
         (equal call
                '("private:10001" nil older 20)))
        (should (= (plist-get callback-meta :message-count) 0))
        (should
         (= (plist-get callback-meta :history-port-version)
            qq-core-history-port-version))))))

(ert-deftest qq-core-unified-history-keeps-gateway-cursors-opaque ()
  (qq-core-test-with-managed-account
    (let* ((session-key
            "dataline:mobile:u_Wcc5rknRRqRO8y5gxMD6sA")
           (older-cursor '((opaque . "older")))
           (newer-cursor '((opaque . "newer")))
           calls latest-meta older-meta)
      (cl-letf
          (((symbol-function 'qq-message--request-history-page)
            (lambda (session cursor direction callback _errback count)
              (push (list session cursor direction count) calls)
              (funcall
               callback
               (list :history-port-version qq-core-history-port-version
                     :history-account-id "slot-a"
                     :history-session-key session-key
                     :history-older-cursor (and (null cursor) older-cursor)
                     :history-newer-cursor newer-cursor
                     :history-has-older-p (null cursor)
                     :history-has-newer-materialized-p (and cursor t)
                     :message-count 2
                     :added-count 2
                     :batch-message-ids nil))
              "history-page")))
        (should
         (qq-request-p
          (qq-core-fetch-history-page
           session-key nil 'older
           (lambda (meta) (setq latest-meta meta)) nil 20)))
        (let ((returned-older
               (plist-get latest-meta :history-older-cursor)))
          (should (= (plist-get latest-meta :history-port-version)
                     qq-core-history-port-version))
          (should (equal returned-older older-cursor))
          (should (equal (plist-get latest-meta :history-newer-cursor)
                         newer-cursor))
          (should
           (qq-request-p
            (qq-core-fetch-history-page
             session-key returned-older 'older
             (lambda (meta) (setq older-meta meta)) nil 1)))
          (should-not
           (plist-get older-meta :history-has-older-p))
          (should-not (plist-get older-meta :history-older-cursor))))
      (should
       (equal
        (nreverse calls)
        (list (list session-key nil 'older 20)
              (list session-key older-cursor 'older 1)))))))

(ert-deftest qq-core-unified-history-around-routes-dataline-by-message-id ()
  (qq-core-test-with-managed-account
    (let* ((session-key
            "dataline:desktop:u_Wcc5rknRRqRO8y5gxMD6sA")
           (center "7348923749823749823")
           call meta)
      (cl-letf
          (((symbol-function 'qq-message--request-history-around)
            (lambda (session center callback _errback count)
              (setq call (list session center count))
              (funcall
               callback
               (list :history-port-version qq-core-history-port-version
                     :history-account-id "slot-a"
                     :history-session-key session-key
                     :history-older-cursor nil
                     :history-newer-cursor '((opaque . "tail"))
                     :history-has-older-p nil
                     :history-has-newer-materialized-p nil
                     :batch-message-ids (list center)
                     :message-count 1 :added-count 1))
              "history-around")))
        (qq-core-fetch-history-around
         session-key center (lambda (value) (setq meta value)) nil 21))
      (should
       (equal call
              (list session-key
                    `((kind . "message") (message_id . ,center))
                    21)))
      (should-not (plist-get meta :history-has-older-p))
      (should-not (plist-get meta :history-has-newer-materialized-p))
      (should-not (plist-get meta :history-older-cursor))
      (should (equal (plist-get meta :history-newer-cursor)
                     '((opaque . "tail")))))))

(ert-deftest qq-core-around-uses-message-id-without-cache-derived-hints ()
  (qq-core-test-with-managed-account
    (let (call)
      (cl-letf (((symbol-function 'qq-state-session-messages)
                 (lambda (&rest _args)
                   (ert-fail "around lookup consulted projected messages")))
                ((symbol-function 'qq-core-history-frontier)
                 (lambda (&rest _args)
                   (ert-fail "around lookup consulted a native frontier")))
                ((symbol-function 'qq-message--request-history-around)
                 (lambda (session center _callback _errback count)
                   (setq call (list session center count))
                   "history-around")))
        (let ((request
                (qq-core-fetch-history-around
                 "private:10001" "7348923749823749823" #'ignore nil 20)))
          (should (qq-request-p request)))
        (should
         (equal call
                '("private:10001"
                  ((kind . "message")
                   (message_id . "7348923749823749823"))
                  20)))))))

(ert-deftest qq-core-around-uses-authored-sequence-when-message-id-is-absent ()
  (qq-core-test-with-managed-account
    (let (call)
      (cl-letf (((symbol-function 'qq-message--request-history-around)
                 (lambda (session center _callback _errback count)
                   (setq call (list session center count))
                   "history-around")))
        (let ((request
                (qq-core-fetch-history-around
                 "group:8209413637" nil #'ignore nil 20
                 "9007199254740999")))
          (should (qq-request-p request)))
        (should
         (equal call
                '("group:8209413637"
                  ((kind . "sequence")
                   (sequence . "9007199254740999"))
                  20)))))))

(ert-deftest qq-core-bootstrap-completion-requires-opaque-attempt-token ()
  (let* ((old-token (list 'old-bootstrap))
         (current-token (list 'current-bootstrap))
         (qq-core--bootstraps (make-hash-table :test #'equal))
         (bootstrap
          (qq-core--bootstrap-create
           :owner "slot-a"
           :instance-id "gateway-a"
           :token current-token
           :pending 2))
         reported)
    (puthash "slot-a" bootstrap qq-core--bootstraps)
    (cl-letf (((symbol-function 'qq-core--default-error)
               (lambda (_body reason) (setq reported reason))))
      (qq-core--bootstrap-success "slot-a" old-token nil)
      (qq-core--bootstrap-failure
       "slot-a" old-token nil "superseded"))
    (should (= (qq-core--bootstrap-pending bootstrap) 2))
    (should-not reported)
    (qq-core--bootstrap-success "slot-a" current-token nil)
    (should (= (qq-core--bootstrap-pending bootstrap) 1))))

(ert-deftest qq-core-recent-bootstrap-waits-for-current-directory-bootstrap ()
  (qq-core-test-with-managed-account
    (let* ((token (list 'directory-bootstrap))
           (qq-core--bootstraps (make-hash-table :test #'equal))
           (qq-core--recent-bootstrap-instances
            (make-hash-table :test #'equal))
           (bootstrap
            (qq-core--bootstrap-create
             :owner "slot-a"
             :instance-id "gateway-a"
             :token token
             :pending 1))
           (recent-calls 0))
      (puthash "slot-a" bootstrap qq-core--bootstraps)
      (cl-letf
          (((symbol-function 'qq-server-ready-p) (lambda () t))
           ((symbol-function 'qq-server-gateway-instance-id)
            (lambda () "gateway-a"))
           ((symbol-function 'qq-core-supports-p)
            (lambda (_capability) t))
           ((symbol-function 'qq-core-refresh-recent-conversations)
            (lambda (&rest _arguments) (cl-incf recent-calls))))
        (qq-core--bootstrap-managed-recents)
        (should (= recent-calls 0))
        (qq-core--bootstrap-success "slot-a" token nil)
        (should (= recent-calls 1))
        (should (equal (gethash "slot-a"
                                qq-core--recent-bootstrap-instances)
                       "gateway-a"))))))

(ert-deftest qq-core-bootstrap-coalesces-one-owner ()
  (qq-core-test-with-managed-account
    (let ((qq-core--bootstraps (make-hash-table :test #'equal))
          calls)
      (cl-letf
          (((symbol-function 'qq-server-ready-p) (lambda () t))
           ((symbol-function 'qq-server-gateway-instance-id)
            (lambda () "gateway-a"))
           ((symbol-function 'qq-core-supports-p)
            (lambda (_capability) t))
           ((symbol-function 'qq-state-friend-categories-loaded-p)
            (lambda () nil))
           ((symbol-function 'qq-state-groups-loaded-p) (lambda () nil))
           ((symbol-function 'qq-core--bootstrap-managed-recents) #'ignore)
           ((symbol-function 'qq-core-refresh-friend-categories)
            (lambda (callback _errback &optional _refresh)
              (push 'friends calls)
              (funcall callback nil)))
           ((symbol-function 'qq-core-refresh-joined-groups)
            (lambda (callback _errback &optional _refresh)
              (push 'groups calls)
              (funcall callback nil))))
        (qq-core--maybe-bootstrap-account "slot-a")
        (qq-core--maybe-bootstrap-account "slot-a")
        (should (equal (sort calls
                             (lambda (left right)
                               (string-lessp (symbol-name left)
                                             (symbol-name right))))
                       '(friends groups)))
        (let ((bootstrap (gethash "slot-a" qq-core--bootstraps)))
          (should (equal (qq-core--bootstrap-owner bootstrap) "slot-a"))
          (should (equal (qq-core--bootstrap-instance-id bootstrap)
                         "gateway-a"))
          (should (= (qq-core--bootstrap-pending bootstrap) 0)))))))

(ert-deftest qq-core-bootstrap-refreshes-directories-after-runtime-restart ()
  (qq-core-test-with-managed-account
    (let ((qq-core--bootstraps (make-hash-table :test #'equal))
          (qq-core--observed-account-phases
           (make-hash-table :test #'equal))
          calls)
      (qq-state-apply-friend-categories
       '(((category_id . "1")
          (category_name . "Old friends")
          (friends . (((user_id . "10001")
                       (nickname . "Alice")))))))
      (qq-state-apply-groups
       '(((group_id . "8209413637")
          (group_name . "Old group"))))
      (puthash "slot-a" "stopped" qq-core--observed-account-phases)
      (puthash
       "slot-a"
       (qq-core--bootstrap-create
        :owner "slot-a"
        :instance-id "gateway-a"
        :token '(old-bootstrap)
        :pending 0)
       qq-core--bootstraps)
      (cl-letf
          (((symbol-function 'qq-server-ready-p)
            (lambda () t))
           ((symbol-function 'qq-server-gateway-instance-id)
            (lambda () "gateway-a"))
           ((symbol-function 'qq-core-supports-p)
            (lambda (_capability) t))
           ((symbol-function 'qq-core--bootstrap-managed-recents)
            #'ignore)
           ((symbol-function 'qq-core-refresh-friend-categories)
            (lambda (_callback _errback &optional refresh)
              (push (list 'friends refresh) calls)))
           ((symbol-function 'qq-core-refresh-joined-groups)
            (lambda (_callback _errback &optional refresh)
              (push (list 'groups refresh) calls))))
        (qq-core--handle-account-registry-change 'changed "slot-a")
        (should (member '(friends t) calls))
        (should (member '(groups t) calls))
        ;; Starting the replacement runtime's refresh preserves the useful
        ;; previous projection while the requests are still open.
        (should (equal (alist-get 'nickname (qq-state-friend "10001"))
                       "Alice"))
        (should (equal (alist-get 'group_name
                                  (qq-state-group "8209413637"))
                       "Old group"))))))

(ert-deftest qq-core-stopped-bootstrap-and-refresh-request-only-recent ()
  (qq-core-test-with-managed-account
    (let ((account (qq-core-test-account))
          (qq-core--bootstraps (make-hash-table :test #'equal))
          calls)
      (setf (alist-get 'phase account) "stopped")
      (qq-account--upsert-account account 'changed)
      (cl-letf
          (((symbol-function 'qq-server-ready-p) (lambda () t))
           ((symbol-function 'qq-server-gateway-instance-id)
            (lambda () "gateway-a"))
           ((symbol-function 'qq-core-supports-p)
            (lambda (_capability) t))
           ((symbol-function 'qq-core-refresh-recent-conversations)
            (lambda (&optional callback _errback _limit _account-id)
              (push 'recent calls)
              (when callback (funcall callback nil))))
           ((symbol-function 'qq-core-refresh-friend-categories)
            (lambda (&rest _) (push 'friends calls)))
           ((symbol-function 'qq-core-refresh-joined-groups)
            (lambda (&rest _) (push 'groups calls))))
        (qq-core--maybe-bootstrap-all)
        (should-not calls)
        (qq-runtime-with-account "slot-a"
          (qq-core-refresh))
        (should (equal calls '(recent)))))))

(ert-deftest qq-core-lagged-recent-resync-coalesces-in-flight-context ()
  (qq-core-test-with-managed-account
    (let ((qq-core--recent-resync-contexts
           (make-hash-table :test #'equal))
          (calls 0)
          success)
      (cl-letf
          (((symbol-function 'qq-server-ready-p) (lambda () t))
           ((symbol-function 'qq-core-supports-p)
            (lambda (_capability) t))
           ((symbol-function 'qq-server-gateway-instance-id)
            (lambda () "gateway-a"))
           ((symbol-function 'qq-core-refresh-recent-conversations)
            (lambda (callback _errback &optional _limit account-id)
              (should (equal account-id "slot-a"))
              (cl-incf calls)
              (setq success callback))))
        (qq-core--handle-desync)
        (qq-core--handle-desync)
        (should (= calls 1))
        (funcall success nil)
        (should-not (gethash "slot-a"
                             qq-core--recent-resync-contexts))
        (qq-core--handle-desync)
        (should (= calls 2))))))

(ert-deftest qq-command-opens-root-without-reentering-active-login ()
  (let ((active t)
        (root-opens 0)
        (login-starts 0))
    (cl-letf (((symbol-function 'qq-runtime-gateway-app) #'ignore)
              ((symbol-function 'qq-root-open-gateway)
               (lambda () (cl-incf root-opens)))
              ((symbol-function 'qq-login-active-p)
               (lambda () active))
              ((symbol-function 'qq-login)
               (lambda (&optional _account-id) (cl-incf login-starts))))
      (qq)
      (setq active nil)
      (qq))
    (should (= root-opens 2))
    (should (= login-starts 1))))

(ert-deftest qq-v2-has-no-backend-selector ()
  (should-not (boundp 'qq-backend))
  (should-not (fboundp 'qq-switch-backend)))

(provide 'qq-core-test)

;;; qq-core-test.el ends here
