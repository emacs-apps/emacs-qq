;;; qq-core-test.el --- Tests for the product operation boundary -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'qq-operation-test)
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
                 :owner nil
                 :lifecycle-owner app))
          (setq settled
                (qq-request-start
                 (lambda (resolve _reject)
                   (setq success resolve) "settle-token")
                 :callback (lambda (value) (setq delivered value))
                 :owner nil
                 :lifecycle-owner app))
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

(ert-deftest qq-core-member-search-filters-cached-exact-ids ()
  (let ((result (qq-core--filter-members qq-core-test-members "alice" 1)))
    (should (= (length result) 1))
    (should (equal (alist-get 'user_id (car result)) "9007199254740999"))))

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















(ert-deftest qq-core-app-bootstrap-waits-for-both-directory-results-and-coalesces ()
  (qq-operation-test-with-app
    (cl-letf (((symbol-function 'qq-server-ready-p) (lambda () t))
              ((symbol-function 'qq-core-supports-p) (lambda (_) t)))
      (qq-core--maybe-bootstrap-account "slot-a")
      (qq-core--maybe-bootstrap-account "slot-a")
      (qq-operation-test--drain)
      (should (= (length calls) 2))
      (qq-operation-test--resolve (car calls) '((account_id . "slot-a") (groups)))
      (qq-operation-test--drain)
      (should (= (length calls) 2))
      (qq-operation-test--resolve (cadr calls) '((account_id . "slot-a") (friends) (categories)))
      (qq-operation-test--drain)
      (should (equal (plist-get (car calls) :method) "conversation.list_recent"))
      (should (= (length calls) 3)))))

(ert-deftest qq-core-app-directory-phase-change-revokes-read-but-preserves-recent-work ()
  (qq-operation-test-with-app
    (let ((qq-core--observed-account-phases (make-hash-table :test #'equal)))
      (puthash "slot-a" "online" qq-core--observed-account-phases)
      (qq-runtime-request '(friends)) (qq-runtime-request '(recent))
      (qq-operation-test--drain)
      (let ((old-friends (cadr calls)) (recent (car calls)))
        (cl-letf (((symbol-function 'qq-account-get) (lambda (id) `((account_id . ,id) (phase . "stopped"))))
                  ((symbol-function 'qq-core--maybe-bootstrap-all) #'ignore))
          (qq-core--handle-account-registry-change 'changed "slot-a"))
        (qq-operation-test--drain)
        (should (memq (plist-get old-friends :token) cancelled))
        (should-not (memq (plist-get recent :token) cancelled))
        (qq-operation-test--resolve old-friends '((account_id . "slot-a") (friends) (categories)))
        (qq-operation-test--drain)
        (should-not (qq-state-friend-categories-loaded-p))))))

(ert-deftest qq-core-app-stopped-refresh-and-lagged-resync-use-recent-lane ()
  (qq-operation-test-with-app
    (cl-letf (((symbol-function 'qq-core-supports-p) (lambda (_) t))
              ((symbol-function 'qq-account-get) (lambda (id) `((account_id . ,id) (phase . "stopped")))))
      (qq-core-refresh) (qq-operation-test--drain)
      (should (= (length calls) 1))
      (should (equal (plist-get (car calls) :method) "conversation.list_recent")))
    (cl-letf (((symbol-function 'qq-server-ready-p) (lambda () t))
              ((symbol-function 'qq-core-supports-p) (lambda (_) t))
              ((symbol-function 'qq-account-list) (lambda () '(((account_id . "slot-a") (phase . "online"))))))
      (qq-core--handle-desync) (qq-core--handle-desync) (qq-operation-test--drain)
      (should (= (length calls) 1)))))

(ert-deftest qq-core-app-recent-replacement-revokes-commit-and-captures-observation ()
  (qq-operation-test-with-app
    (let ((counter 0) observations)
      (cl-letf (((symbol-function 'qq-state-session-summary-observation-start) (lambda () (cl-incf counter)))
                ((symbol-function 'qq-core--apply-recent-page)
                 (lambda (page observation) (push (list page observation) observations))))
        (qq-runtime-request '(recent)) (qq-operation-test--drain)
        (let ((old (car calls)))
          (qq-runtime-request '(recent) t) (qq-operation-test--drain)
          (qq-operation-test--resolve old '((conversations) (label . old)))
          (qq-operation-test--resolve (car calls) '((conversations) (label . fresh)))
          (should-not observations)
          (qq-operation-test--drain)
          (should (equal observations '((((conversations) (label . fresh)) 2)))))))))

(ert-deftest qq-core-app-read-frontier-keeps-only-newest-pending-row ()
  (qq-operation-test-with-app
    (let ((rows (mapcar (lambda (row) `((session-key . "group:8209413637")
                                      (canonical-row-key . ,row) (gateway-account-id . "slot-a")))
                        '("101" "102" "103"))))
      (cl-letf (((symbol-function 'qq-state-session-messages) (lambda (_) rows))
                ((symbol-function 'qq-message--canonical-row-request)
                 (lambda (message _owner _label)
                   (list :params (list (cons 'row_key (alist-get 'canonical-row-key message)))))))
        (qq-runtime-request (list 'read-message (nth 0 rows)))
        (qq-runtime-request (list 'read-message (nth 2 rows)))
        (qq-runtime-request (list 'read-message (nth 1 rows)))
        (qq-operation-test--drain)
        (should (= (length calls) 1))
        (qq-operation-test--resolve (car calls) '((account_id . "slot-a") (row_key . "101")))
        (qq-operation-test--drain)
        (should (= (length calls) 2))
        (should (equal (alist-get 'row_key (plist-get (car calls) :params)) "103"))
        (qq-operation-test--resolve (car calls) '((account_id . "slot-a") (row_key . "103")))
        (qq-operation-test--drain)
        (should (equal (alist-get 'row_key (plist-get (qq-runtime-operation '(read-message "group:8209413637")) :value)) "103"))))))


(provide 'qq-core-test)

;;; qq-core-test.el ends here
