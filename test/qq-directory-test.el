;;; qq-directory-test.el --- Tests for QQ contacts -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-directory)
(require 'qq-operation-test)
(require 'qq-message-recent-test)
(require 'qq-root)

(defconst qq-directory-test-capabilities
  '("contact.list_friends" "contact.list_groups"
    "contact.list_group_members"
    "group_request.list" "group_request.decide"
    "group.set_name" "group.set_remark"
    "friend.set_pinned" "group.set_whole_mute" "group.set_pinned" "group.set_member_card"
    "group.set_member_special_title" "group.kick_member" "group.clock_in"
    "group.get_at_all_remaining" "group.leave")
  "Native contact capabilities exercised by directory tests.")

(defun qq-directory-test-account
    (&optional account-id uin uid)
  "Return an online test account with exact identity fields."
  `((account_id . ,(or account-id "slot-a"))
    (label . "Primary")
    (phase . "online")
    (uin . ,(or uin "10002"))
    (uid . ,(or uid "u_self"))
    (challenge)
    (problem)))

(defun qq-directory-test-friends-result ()
  "Return one closed friend-list result."
  (qq-server-value-copy
   '((account_id . "slot-a")
     (friends
      . [((uid . "u_alice")
          (uin . "9007199254740999")
          (avatar_url
           . "https://q.qlogo.cn/headimg_dl?dst_uin=9007199254740999&spec=640&img_type=jpg")
          (category_id . 7)
          (nickname . "Alice")
          (remark . "A")
          (personal_sign . "hello")
          (qid . "alice-qid")
          (age . 20)
          (gender . 2))
         ((uid . "u_bob")
          (uin . "10003")
          (avatar_url
           . "https://q.qlogo.cn/headimg_dl?dst_uin=10003&spec=640&img_type=jpg")
          (category_id . 3)
          (nickname . "Bob")
          (remark)
          (personal_sign)
          (qid)
          (age)
          (gender))])
     (categories
      . [((id . 7) (name . "Work") (member_count . 1) (sort_id . 20))
         ((id . 3) (name . "Other") (member_count . 1) (sort_id . 21))]))))

(defun qq-directory-test-groups-result ()
  "Return one closed joined-group result."
  (qq-server-value-copy
   '((account_id . "slot-a")
     (groups
      . [((group_uin . "8209413637")
          (owner_uid . "u_self")
          (name . "Protocol Lab")
          (member_count . 3)
          (member_max . 500)
          (created_time . 1700000000)
          (description . "Native Gateway")
          (question)
          (announcement . "Welcome")
          (remark . "Lab")
          (last_speak_time . 1784700000)
          (latest_sequence . 123)
          (message_notify_mode . "receive"))]))))

(defun qq-directory-test-members-result ()
  "Return one closed group-member result."
  (qq-server-value-copy
   '((account_id . "slot-a")
     (group_uin . "8209413637")
     (members
      . [((uid . "u_owner")
          (uin . "10001")
          (nickname . "Owner")
          (member_card . "Boss")
          (special_title)
          (level . 10)
          (permission . ((kind . "owner")))
          (join_timestamp . 1)
          (last_message_timestamp . 2)
          (shut_up_timestamp . 0))
         ((uid . "u_self")
          (uin . "10002")
          (nickname . "Self")
          (member_card . "Admin")
          (special_title . "Maintainer")
          (level . 9)
          (permission . ((kind . "admin")))
          (join_timestamp . 3)
          (last_message_timestamp . 4)
          (shut_up_timestamp . 0))
         ((uid . "u_unknown")
          (uin . "10003")
          (nickname . "Unknown")
          (member_card)
          (special_title)
          (level . 1)
          (permission . ((kind . "unknown") (code . 99)))
          (join_timestamp . 5)
          (last_message_timestamp . 6)
          (shut_up_timestamp . 0))])
     (member_count . 3)
     (member_list_change_sequence . 11)
     (member_card_sequence . 12))))

(defmacro qq-directory-test-with-state (&rest body)
  "Run BODY with real account Apps and a manually completed RPC transport."
  (declare (indent 0))
  `(qq-operation-test-with-app
     (let ((qq-message--peer-uin-by-uid (make-hash-table :test #'equal))
           (qq-state-change-hook nil))
       (cl-letf (((symbol-function 'qq-account-get)
                  (lambda (owner) (qq-directory-test-account owner)))
                 ((symbol-function 'qq-core-supports-p) (lambda (_capability) nil)))
         ,@body))))

(defmacro qq-directory-test-fetch (request result &optional refresh)
  "Complete one REQUEST with RESULT through the real App reducer."
  `(progn
     (qq-runtime-request ,request ,refresh)
     (qq-operation-test--drain)
     (qq-operation-test--resolve (car calls) (qq-server-wire-domain-copy ,result))
     (qq-operation-test--drain)))

(ert-deftest qq-directory-app-friends-preserve-order-identities-and-count-drift ()
  (qq-directory-test-with-state
    (let ((raw (qq-directory-test-friends-result)))
      (setf (alist-get 'member_count (aref (alist-get 'categories raw) 0)) 40)
      (qq-directory-test-fetch '(friends) raw)
      (should (equal (plist-get (car calls) :params) '((account_id . "slot-a") (refresh . :false))))
      (should (equal (mapcar (lambda (c) (alist-get 'category_id c)) (qq-state-friend-categories)) '(7 3)))
      (should (equal (mapcar (lambda (c) (alist-get 'user_id c)) (qq-state-friends)) '("9007199254740999" "10003")))
      (should (= (alist-get 'member_count (car (qq-state-friend-categories))) 40))
      (should (equal (alist-get 'category_name (qq-state-friend "9007199254740999")) "Work"))
      (should (equal (gethash '("slot-a" "u_alice") qq-message--peer-uin-by-uid) "9007199254740999")))))

(ert-deftest qq-directory-app-rejects-unknown-category-atomically ()
  (qq-directory-test-with-state
    (let ((raw (qq-directory-test-friends-result)))
      (setf (alist-get 'category_id (aref (alist-get 'friends raw) 0)) 99)
      (qq-directory-test-fetch '(friends) raw)
      (should (string-match-p "unknown category" (plist-get (qq-runtime-operation '(friends)) :error)))
      (should-not (qq-state-friend-categories-loaded-p))
      (should (= (hash-table-count qq-message--peer-uin-by-uid) 0)))))

(ert-deftest qq-directory-app-groups-project-native-metadata ()
  (qq-directory-test-with-state
    (qq-state-upsert-session "group:8209413637" nil nil)
    (qq-directory-test-fetch '(groups) (qq-directory-test-groups-result) t)
    (let ((group (qq-state-group "8209413637")))
      (should (equal (alist-get 'group_name group) "Protocol Lab"))
      (should (equal (alist-get 'group_remark group) "Lab"))
      (should (equal (alist-get 'self_permission group) "owner"))
      (should (equal (alist-get 'latest_sequence group) "123"))
      (should (= (alist-get 'max_member_count group) 500))
      (should (eq (alist-get 'message-notify-mode group) 'receive)))
    (should (eq (alist-get 'muted-p (qq-state-session "group:8209413637")) t))))

(ert-deftest qq-directory-app-members-have-one-shared-page-and-enrich-groups ()
  (qq-directory-test-with-state
    (qq-directory-test-fetch '(groups) (qq-directory-test-groups-result))
    (let ((raw (qq-directory-test-members-result)))
      (setf (alist-get 'member_count raw) 40)
      (qq-directory-test-fetch '(group-members "8209413637") raw))
    (let* ((page (plist-get (qq-runtime-operation '(group-members "8209413637")) :value))
           (members (alist-get 'members page)))
      (should (= (alist-get 'member_count page) 40))
      (should (= (length members) 3))
      (should (equal (mapcar (lambda (m) (alist-get 'role m)) members) '("owner" "admin" nil)))
      (should (equal (alist-get 'native_permission (nth 2 members)) '((kind . "unknown") (code . 99))))
      (should (equal (alist-get 'owner_id (qq-state-group "8209413637")) "10001"))
      (should (equal (alist-get 'self_permission (qq-state-group "8209413637")) "admin"))
      (should (equal (alist-get 'user_id (qq-directory-group-member "8209413637" "10003")) "10003")))))

(ert-deftest qq-directory-app-member-mutations-revoke-old-page-without-losing-confirmed-data ()
  (qq-directory-test-with-state
    (qq-directory-test-fetch '(group-members "8209413637") (qq-directory-test-members-result))
    (qq-runtime-request '(group-members "8209413637") t)
    (qq-operation-test--drain)
    (let ((old (car calls)))
      (qq-directory-test-fetch '(member-setting "8209413637" "10002" title "New title")
                               '((account_id . "slot-a")))
      (qq-operation-test--resolve old (qq-server-wire-domain-copy (qq-directory-test-members-result)))
      (qq-operation-test--drain)
      (should (equal (alist-get 'title (qq-directory-group-member "8209413637" "10002")) "New title"))
      (should (memq (plist-get old :token) cancelled)))
    (qq-directory-test-fetch '(member-kick "8209413637" "10003" t) '((account_id . "slot-a")))
    (should-not (qq-directory-group-member "8209413637" "10003"))
    (should (= (alist-get 'member_count (plist-get (qq-runtime-operation '(group-members "8209413637")) :value)) 2))))

(ert-deftest qq-directory-app-group-mutations-share-one-write-lane-and-commit-state ()
  (qq-directory-test-with-state
    (qq-directory-test-fetch '(groups) (qq-directory-test-groups-result))
    (dolist (case '((name "New name" "group.set_name" group_name "New name")
                    (remark "" "group.set_remark" group_remark nil)
                    (pinned t "group.set_pinned" pinned t)))
      (qq-directory-test-fetch (list 'group-setting "8209413637" (nth 0 case) (nth 1 case))
                               '((account_id . "slot-a")))
      (should (equal (plist-get (car calls) :method) (nth 2 case)))
      (should (equal (alist-get (nth 3 case) (qq-state-group "8209413637")) (nth 4 case))))
    (qq-runtime-request '(group-setting "8209413637" name "Pending"))
    (qq-operation-test--drain)
    (should-error (qq-runtime-request '(group-setting "8209413637" remark "Other")) :type 'user-error)))

(ert-deftest qq-directory-app-leave-removes-members-and-respects-unloaded-directory ()
  (qq-directory-test-with-state
    (qq-operation-test--seed '(group-members "8209413637") :value '((members . nil)))
    (qq-directory-test-fetch '(group-leave "8209413637") '((account_id . "slot-a")))
    (should-not (qq-state-groups-loaded-p))
    (should-not (qq-runtime-operation '(group-members "8209413637")))
    (qq-directory-test-fetch '(groups) (qq-directory-test-groups-result))
    (qq-directory-test-fetch '(group-leave "8209413637") '((account_id . "slot-a")))
    (should (qq-state-groups-loaded-p))
    (should-not (qq-state-group "8209413637"))))

(ert-deftest qq-directory-app-account-switch-keeps-original-owner ()
  (qq-directory-test-with-state
    (qq-runtime-request '(friends))
    (qq-operation-test--drain)
    (qq-runtime-with-account "slot-b"
      (qq-operation-test--resolve (car calls) (qq-server-wire-domain-copy (qq-directory-test-friends-result))))
    (qq-operation-test--drain)
    (should (qq-state-friend "10003"))
    (qq-runtime-with-account "slot-b" (should-not (qq-state-friends)))))


(ert-deftest qq-directory-app-pin-changes-refresh-root-order-and-revoke-stale-pages ()
  (dolist (kind '(friend group))
    (qq-directory-test-with-state
      (cl-letf (((symbol-function 'qq-core-supports-p)
                 (lambda (capability) (eq capability 'recent-conversations))))
        (let* ((private (qq-message-recent-test-row :pinned :false))
               (group (qq-message-recent-test-row
                       :identity '((kind . "group") (group_uin . "8209413637"))
                       :row-key "18"
                       :pinned :false
                       :message (qq-message-recent-test-message
                                 :message-id "7348923749823749824"
                                 :conversation '((kind . "group")
                                                 (group_uin . "8209413637")))))
               (target (if (eq kind 'friend) private group))
               (other (if (eq kind 'friend) group private))
               (target-key (if (eq kind 'friend) "private:10001" "group:8209413637"))
               (other-key (if (eq kind 'friend) "group:8209413637" "private:10001"))
               (page (qq-message-recent-test-page :rows (list other target))))
          (setf (alist-get 'sent_at (alist-get 'message (alist-get 'latest_message target))) 100
                (alist-get 'sent_at (alist-get 'message (alist-get 'latest_message other))) 200)
          (qq-directory-test-fetch '(recent)
                                   (qq-message-recent-test-page :rows (list other target)))
          (dolist (pinned '(t nil))
            (let ((before (mapcar (lambda (s) (alist-get 'key s))
                                 (qq-root--recent-sessions))))
              (should (= (length before) 2))
              (qq-runtime-request '(recent) t)
              (qq-operation-test--drain)
              (let ((old (car calls)))
                (qq-runtime-request
                 (if (eq kind 'friend)
                     (list 'friend-pinned "10001" pinned)
                   (list 'group-setting "8209413637" 'pinned pinned)))
                (qq-operation-test--drain)
                (should (equal before (mapcar (lambda (s) (alist-get 'key s))
                                               (qq-root--recent-sessions))))
                (let ((write (car calls)))
                  (should (equal (plist-get write :method)
                                 (if (eq kind 'friend) "friend.set_pinned" "group.set_pinned")))
                  (should (eq (alist-get 'pinned (plist-get write :params))
                              (if pinned t :false)))
                  (qq-operation-test--resolve write
                                               `((account_id . "slot-a")
                                                 (pinned . ,(if pinned t :false)))))
                (qq-operation-test--drain)
                (should (memq (plist-get old :token) cancelled))
                (should (equal (plist-get (car calls) :method) "conversation.list_recent"))
                (should (equal (alist-get 'account_id (plist-get (car calls) :params)) "slot-a"))
                (setf (alist-get 'pinned target) (if pinned t :false))
                (let ((fresh (qq-message-recent-test-page
                              :rows (if pinned (list target other) (list other target)))))
                  (qq-operation-test--resolve (car calls) (qq-server-wire-domain-copy fresh))
                  (qq-operation-test--drain)
                  (should-not (plist-get (qq-runtime-operation '(recent)) :error))
                  ;; Even after the fresh result, an older page must stay revoked.
                  (qq-operation-test--resolve old (qq-server-wire-domain-copy page))
                  (qq-operation-test--drain)
                  (setq page fresh))
                (should (equal (mapcar (lambda (s) (alist-get 'key s))
                                       (qq-root--recent-sessions))
                               (if pinned (list target-key other-key) (list other-key target-key))))
                (should (eq (alist-get 'pinned (qq-state-session target-key))
                            (if pinned t :false)))
                (qq-runtime-with-account "slot-b"
                  (should-not (qq-root--recent-sessions)))))))))))

(ert-deftest qq-directory-app-rejected-pin-keeps-root-state ()
  (dolist (request '((friend-pinned "10001" t)
                     (group-setting "8209413637" pinned t)))
    (qq-directory-test-with-state
      (cl-letf (((symbol-function 'qq-core-supports-p)
                 (lambda (capability) (eq capability 'recent-conversations))))
        (qq-directory-test-fetch
         '(recent) (qq-message-recent-test-page
                    :rows (list (qq-message-recent-test-row :pinned :false))))
        (let ((before (qq-root--recent-sessions)))
          (should (= (length before) 1))
          (qq-runtime-request request)
          (qq-operation-test--drain)
          (let ((count (length calls)))
            (funcall (plist-get (plist-get (car calls) :options) :errback)
                     nil "pin rejected")
            (qq-operation-test--drain)
            (should (= count (length calls)))
            (should (equal before (qq-root--recent-sessions)))))))))

(provide 'qq-directory-test)

;;; qq-directory-test.el ends here
