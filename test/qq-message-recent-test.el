;;; qq-message-recent-test.el --- Recent conversation adapter tests -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'qq-message)

(defun qq-message-recent-test-account (&optional account-id uin uid phase)
  "Return a managed ACCOUNT-ID fixture for UIN, UID, and PHASE."
  `((account_id . ,(or account-id "slot-a"))
    (label . "Primary")
    (phase . ,(or phase "online"))
    (uin . ,(or uin "10002"))
    (uid . ,(or uid "u_self"))
    (challenge)
    (problem)))

(cl-defun qq-message-recent-test-message
    (&key
     (message-id "7348923749823749823")
     (sequence "9007199254740999")
     (sender '((uin . "10001") (uid . "u_peer")))
     (recipient '((uin . "10002") (uid . "u_self")))
     (conversation '((kind . "private") (name . "Peer"))))
  "Return a closed MESSAGE-ID snapshot.

SEQUENCE, SENDER, RECIPIENT, and CONVERSATION provide its native context."
  `((message_id . ,message-id)
    (sent_at . 1784700000)
    (sender . ,(copy-tree sender))
    (recipient . ,(copy-tree recipient))
    (conversation . ,(copy-tree conversation))
    (sequence . ,sequence)
    (client_sequence . "9007199254741001")
    (random . 7)
    (message_type . 166)
    (sub_type . 0)
    (segments . [((kind . "text") (payload . ((text . "hello"))))])))

(cl-defun qq-message-recent-test-row
    (&key
     (identity '((kind . "private")
                 (peer_uin . "10001") (peer_uid . "u_peer")))
     (revision "12")
     (row-key "17")
     (message (qq-message-recent-test-message))
     (recalled :false)
     (pinned 'absent))
  "Return a closed recent-conversation row for IDENTITY and REVISION.

ROW-KEY and MESSAGE form its canonical latest row.  RECALLED and PINNED
supply projection metadata.  The symbol `absent' omits an independently
unknown field."
  `((conversation . ,(copy-tree identity))
    ,@(unless (eq pinned 'absent) `((pinned . ,pinned)))
    (activity_revision . ,revision)
    (latest_message . ((kind . "native")
                       (row_key . ,row-key)
                       (timeline_class . "authored")
                       (recalled . ,recalled)
                       (message . ,(copy-tree message))))))

(cl-defun qq-message-recent-test-dataline-row
    (&key
     (variant "desktop")
     (peer-uid "u_Wcc5rknRRqRO8y5gxMD6sA")
     (revision "13")
     (message-id "7348923749823749824"))
  "Return one closed DataLine recent row for VARIANT and PEER-UID."
  `((conversation . ((kind . "dataline")
                     (peer_uid . ,peer-uid)
                     (variant . ,variant)))
    (activity_revision . ,revision)
    (latest_message
     . ((kind . "dataline")
        (message
         . ((message_id . ,message-id)
            (chat . ((peer_uid . ,peer-uid) (variant . ,variant)))
            (direction . "received")
            (sent_at . 1784700001)
            (segments . (((kind . "text")
                          (payload . ((text . "DataLine recent"))))))))))))

(cl-defun qq-message-recent-test-page
    (&key
     (account-id "slot-a")
     (rows (list (qq-message-recent-test-row)))
     (truncated :false))
  "Return ACCOUNT-ID's closed page containing ROWS.

TRUNCATED is its exact wire boolean."
  `((account_id . ,account-id)
    (conversations . ,(vconcat (mapcar #'copy-tree rows)))
    (truncated . ,truncated)))

(defmacro qq-message-recent-test-with-state (&rest body)
  "Run BODY with one isolated selected Gateway account."
  (declare (indent 0) (debug t))
  `(let ((qq-account--accounts (make-hash-table :test #'equal))
         (qq-account--account-order nil)
         (qq-account--current-account-id nil)
         (qq-account--gateway-instance-id nil)
         (qq-account--resync-request-id nil)
         (qq-account-registry-changed-hook nil)
         (qq-account-selection-changed-hook nil)
         (qq-account-desync-hook nil)
         (qq-account-projection-resync-hook nil)
         (qq-server--gateway-instance-id "gateway-a")
         (qq-server--capabilities '("conversation.list_recent"))
         (qq-server--state 'ready))
     (qq-account--replace-accounts
      (list (qq-message-recent-test-account)) 'ready "gateway-a")
     (cl-letf (((symbol-function 'qq-server-ready-p)
                (lambda () t))
               ((symbol-function 'qq-server-capabilities)
                (lambda () '("conversation.list_recent"))))
       ,@body)))

(ert-deftest qq-message-recent-requests-slot-and-returns-domain-page ()
  (qq-message-recent-test-with-state
    (let (method params success delivered)
      (cl-letf (((symbol-function 'qq-server-send)
                 (lambda (request-method request-params callback _errback
                          &optional _early)
                   (setq method request-method
                         params request-params
                         success callback)
                   "recent-1")))
        (should (equal
                 (qq-message-list-recent
                  "slot-a"
                  :callback (lambda (page) (setq delivered page))
                  :limit 37)
                 "recent-1"))
        (should (equal method "conversation.list_recent"))
        (should (equal params '((account_id . "slot-a") (limit . 37))))
        (funcall success (qq-message-recent-test-page)))
      (should (proper-list-p (alist-get 'conversations delivered)))
      (let* ((row (car (alist-get 'conversations delivered)))
             (head (alist-get 'latest_message row))
             (message (alist-get 'message head)))
        (should-not (assq 'latest_message_generation row))
        (should-not (assq 'read_cursor row))
        (should (equal (alist-get 'row_key head) "17"))
        (should (proper-list-p (alist-get 'segments message)))
        (should (equal (alist-get 'message_id message)
                       "7348923749823749823"))))))

(ert-deftest qq-message-recent-domain-page-preserves-optional-pinned-state ()
  (qq-message-recent-test-with-state
    (let* ((pinned-page
            (qq-message-recent-test-page
             :rows (list (qq-message-recent-test-row :pinned t))))
           (unknown-page
            (qq-message-recent-test-page
             :rows (list (qq-message-recent-test-row))))
           (pinned-row
            (car (alist-get 'conversations
                            (qq-message--recent-check-page
                             (qq-server-wire-domain-copy pinned-page)))))
           (unknown-row
            (car (alist-get 'conversations
                            (qq-message--recent-check-page
                             (qq-server-wire-domain-copy unknown-page))))))
      (should (eq (alist-get 'pinned pinned-row) t))
      (should-not (assq 'pinned unknown-row)))))

(ert-deftest qq-message-recent-projection-preserves-stable-account-page ()
  (qq-message-recent-test-with-state
    (let* ((page (qq-message-recent-test-page))
           (validated
            (qq-message--recent-check-page
             (qq-server-wire-domain-copy page)))
           (row (car (alist-get 'conversations validated))))
      (should (equal (alist-get 'account_id validated) "slot-a"))
      (should-not (assq 'generation validated))
      (should-not (assq 'latest_message_generation row)))))

(ert-deftest qq-message-recent-projection-allows-forward-compatible-fields ()
  (qq-message-recent-test-with-state
    (let* ((row
            (append
             (qq-message-recent-test-row)
             '((read_cursor
                . ((read_through_message_id
                    . "7348923749823749823"))))))
           (page
            (append
             (qq-message-recent-test-page :rows (list row))
             '((server_extension . t))))
           (domain
            (qq-message--recent-check-page
             (qq-server-wire-domain-copy page)))
           (projected-row (car (alist-get 'conversations domain))))
      (should (eq (alist-get 'server_extension domain) t))
      (should (assq 'read_cursor projected-row)))))

(ert-deftest qq-message-recent-projection-checks-identity-contract ()
  (qq-message-recent-test-with-state
    (let ()
      (let* ((message
              (qq-message-recent-test-message
               :conversation
               '((kind . "group") (group_uin . "8209413637"))))
             (row (qq-message-recent-test-row :message message))
             (page (qq-message-recent-test-page :rows (list row))))
        (should-error
         (qq-message--recent-check-page
          (qq-server-wire-domain-copy page))))
      (let* ((identity '((kind . "group") (group_uin . "8209413637")))
             (message
              (qq-message-recent-test-message
               :conversation
               '((kind . "group") (group_uin . "8209413638"))))
             (row (qq-message-recent-test-row
                   :identity identity :message message))
             (page (qq-message-recent-test-page :rows (list row))))
        (should-error
         (qq-message--recent-check-page
          (qq-server-wire-domain-copy page)))))))

(ert-deftest qq-message-recent-projection-keeps-dataline-variants-distinct ()
  (qq-message-recent-test-with-state
    (let* ((desktop (qq-message-recent-test-dataline-row))
           (mobile (qq-message-recent-test-dataline-row
                    :variant "mobile"
                    :revision "14"
                    :message-id "7348923749823749825"))
           (page (qq-message-recent-test-page
                  :rows (list desktop mobile))))
      (should (qq-message--recent-check-page
               (qq-server-wire-domain-copy page)))
      (let* ((mismatch (copy-tree desktop))
             (message (alist-get 'message
                                 (alist-get 'latest_message mismatch))))
        (setf (alist-get 'variant (alist-get 'chat message)) "mobile")
        (ert-info ("mismatched DataLine locator")
          (should-error
           (qq-message--recent-check-page
            (qq-server-wire-domain-copy
             (qq-message-recent-test-page :rows (list mismatch)))))))
      (ert-info ("duplicate DataLine locator")
        (should-error
         (qq-message--recent-check-page
          (qq-server-wire-domain-copy
           (qq-message-recent-test-page :rows (list desktop desktop)))))))))

(ert-deftest qq-message-recent-limit-is-closed-before-send ()
  (qq-message-recent-test-with-state
    (let ((sent 0)
          (qq-recent-contact-count 23))
      (cl-letf (((symbol-function 'qq-server-send)
                 (lambda (&rest _arguments) (cl-incf sent))))
        (dolist (invalid '(0 501 1.5 "10"))
          (should-error
           (qq-message-list-recent
            "slot-a" :limit invalid)
           :type 'user-error))
        (qq-message-list-recent "slot-a")
        (should (= sent 1))))))

(ert-deftest qq-message-recent-is-stateless-and-account-addressed ()
  (qq-message-recent-test-with-state
    (let (params success delivered)
      (cl-letf (((symbol-function 'qq-server-send)
                 (lambda (_method request-params callback _errback
                          &optional _early)
                   (setq params request-params
                         success callback)
                   "recent-explicit")))
        ;; Selection is product state and is intentionally irrelevant here.
        (setq qq-account--current-account-id nil)
        (should
         (equal
          (qq-message-list-recent
           "slot-b" :limit 11
           :callback (lambda (page) (setq delivered page)))
          "recent-explicit"))
        (should (equal params '((account_id . "slot-b") (limit . 11))))
        (funcall success
                 (qq-message-recent-test-page :account-id "slot-b")))
      (should (equal (alist-get 'account_id delivered) "slot-b")))))

(ert-deftest qq-message-recent-transport-signal-propagates ()
  (qq-message-recent-test-with-state
    (cl-letf (((symbol-function 'qq-server-send)
               (lambda (&rest _arguments) (error "transport exploded"))))
      (should-error
       (qq-message-list-recent
        "slot-a" :errback #'ignore :limit 10)))))

(provide 'qq-message-recent-test)
;;; qq-message-recent-test.el ends here
