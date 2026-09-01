;;; qq-directory-test.el --- Tests for QQ contacts -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-directory)

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
  "Run BODY with isolated selected Gateway and directory state."
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
         (qq-message--peer-uin-by-uid
          (make-hash-table :test #'equal))
         (qq-message--pending-recalls
          (make-hash-table :test #'equal))
         (qq-message--pending-sends
          (make-hash-table :test #'equal))
         (qq-directory--active-requests
          (make-hash-table :test #'equal))
         (qq-directory--member-pages
          (make-hash-table :test #'equal))
         (qq-directory--account-phases
          (make-hash-table :test #'equal))
         (qq-runtime--app nil)
         (qq-runtime--accounts (make-hash-table :test #'equal))
         (qq-state--partitions (make-hash-table :test #'equal))
         (qq-state--active-account-id nil)
         (qq-server--state 'ready)
         (qq-state-change-hook nil))
     (unwind-protect
         (progn
           (qq-account--replace-accounts
            (list (qq-directory-test-account)) 'ready "gateway-test")
           (qq-state-select-account "slot-a")
           (qq-state-reset)
           (qq-message--sync-account "slot-a")
           ,@body)
       (qq-runtime-stop)
       (qq-state-reset))))

(ert-deftest qq-directory-member-page-accessor-owns-strings ()
  (qq-directory-test-with-state
    (let ((result
           (qq-server-wire-domain-copy
            (qq-directory-test-members-result)))
          (owner "slot-a"))
      (qq-directory--project-members result owner "8209413637")
      (let* ((page
              (qq-directory-group-member-page "8209413637"))
             (first (car (alist-get 'members page))))
        (aset (alist-get 'user_id first) 0 ?X))
      (should (equal
               (alist-get
                'user_id
                (car
                 (alist-get
                  'members
                  (qq-directory-group-member-page "8209413637"))))
               "10001")))))

(ert-deftest qq-directory-friends-project-exact-order-and-identities ()
  (qq-directory-test-with-state
    (let (sent-method sent-params callback-value)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-directory-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (setq sent-method method sent-params params)
                   (funcall callback
                            (qq-directory-test-friends-result))
                   "request-friends")))
        (should
         (equal
          (qq-directory-refresh-friends
           (lambda (categories) (setq callback-value categories)))
          "request-friends"))
        (should (equal sent-method "contact.list_friends"))
        (should
         (equal sent-params
                '((account_id . "slot-a") (refresh . :false))))
        (should (equal (mapcar (lambda (category)
                                 (alist-get 'category_id category))
                               callback-value)
                       '(7 3)))
        (should (equal (mapcar (lambda (friend)
                                 (alist-get 'user_id friend))
                               (qq-state-friends))
                       '("9007199254740999" "10003")))
        (should (equal (alist-get 'category_name
                                  (qq-state-friend "9007199254740999"))
                       "Work"))
        (should
         (equal
          (alist-get 'avatar_url (qq-state-friend "9007199254740999"))
          "https://q.qlogo.cn/headimg_dl?dst_uin=9007199254740999&spec=640&img_type=jpg"))
        (should (equal (gethash '("slot-a" "u_alice")
                                qq-message--peer-uin-by-uid)
                       "9007199254740999"))
        (should (= (hash-table-count
                    qq-directory--active-requests)
                   0))))))

(ert-deftest qq-directory-friend-count-drift-is-preserved ()
  (qq-directory-test-with-state
    (let ((result (qq-directory-test-friends-result)) callback-value)
      (setf (alist-get 'member_count (aref (alist-get 'categories result) 0)) 2)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-directory-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (funcall callback result)
                   "request-friends")))
        (qq-directory-refresh-friends
         (lambda (categories) (setq callback-value categories)))
        (should (= (alist-get 'member_count (car callback-value)) 2))
        (should (= (length (alist-get 'friends (car callback-value))) 1))
        (should (qq-state-friend-categories-loaded-p))))))

(ert-deftest qq-directory-friend-unknown-category-is-atomic ()
  (qq-directory-test-with-state
    (let ((result (qq-directory-test-friends-result)) failure)
      (setf (alist-get 'category_id (aref (alist-get 'friends result) 0)) 99)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-directory-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (funcall callback result)
                   "request-friends")))
        (qq-directory-refresh-friends
         nil (lambda (_body reason) (setq failure reason)))
        (should (string-match-p "unknown category" failure))
        (should-not (qq-state-friend-categories-loaded-p))
        (should (= (hash-table-count
                    qq-message--peer-uin-by-uid)
                   0))))))

(ert-deftest qq-directory-groups-project-native-metadata ()
  (qq-directory-test-with-state
    (let (sent-params callback-value)
      (qq-state-upsert-session "group:8209413637" nil nil)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-directory-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (_method params callback _errback &optional _early)
                   (setq sent-params params)
                   (funcall callback
                            (qq-directory-test-groups-result))
                   "request-groups")))
        (qq-directory-refresh-groups
         (lambda (groups) (setq callback-value groups)) nil t)
        (should
         (equal sent-params
                '((account_id . "slot-a") (refresh . t))))
        (should (= (length callback-value) 1))
        (let ((group (qq-state-group "8209413637")))
          (should (equal (alist-get 'group_name group) "Protocol Lab"))
          (should (equal (alist-get 'group_remark group) "Lab"))
          (should (equal (alist-get 'self_permission group) "owner"))
          (should (equal (alist-get 'latest_sequence group) "123"))
          (should (= (alist-get 'max_member_count group) 500))
          (should (eq (alist-get 'message-notify-mode group) 'receive))
          (should (eq (alist-get 'muted-p group) t)))
        (let ((session (qq-state-session "group:8209413637")))
          (should (eq (alist-get 'message-notify-mode session) 'receive))
          (should (eq (alist-get 'muted-p session) t)))))))

(ert-deftest qq-directory-group-settings-send-domain-requests ()
  (qq-directory-test-with-state
    (let (calls receipts)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-directory-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (push (list method params) calls)
                   (funcall
                    callback
                    (pcase method
                      ("group.set_name"
                       '((account_id . "slot-a")
                         (group_uin . "8209413637") (name . "New Name")))
                      ("group.set_remark"
                       '((account_id . "slot-a")
                         (group_uin . "8209413637") (remark . "")))
                      ("group.set_whole_mute"
                       '((account_id . "slot-a")
                         (group_uin . "8209413637") (enabled . :false)))
                      ("group.set_pinned"
                       '((account_id . "slot-a")
                         (group_uin . "8209413637") (pinned . t)))))
                   method)))
        (qq-directory-set-group-name
         "8209413637" "New Name" (lambda (receipt) (push receipt receipts)))
        (qq-directory-set-group-remark
         "8209413637" "" (lambda (receipt) (push receipt receipts)))
        (qq-directory-set-group-whole-mute
         "8209413637" nil (lambda (receipt) (push receipt receipts)))
        (qq-directory-set-group-pinned
         "8209413637" t (lambda (receipt) (push receipt receipts))))
      (should (= (length receipts) 4))
      (should
       (equal
        (nreverse calls)
        '(("group.set_name"
           ((account_id . "slot-a") (group_uin . "8209413637")
            (name . "New Name")))
          ("group.set_remark"
           ((account_id . "slot-a") (group_uin . "8209413637")
            (remark . "")))
          ("group.set_whole_mute"
           ((account_id . "slot-a") (group_uin . "8209413637")
            (enabled . :false)))
          ("group.set_pinned"
           ((account_id . "slot-a") (group_uin . "8209413637")
            (pinned . t)))))))))

(ert-deftest qq-directory-friend-pinned-sends-exact-uin ()
  (qq-directory-test-with-state
    (let (sent callback-value)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-directory-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (setq sent (list method params))
                   (funcall
                    callback
                    '((account_id . "slot-a")
                      (friend_uin . "9007199254740999") (pinned . :false)))
                   "friend-pin-request")))
        (should
         (equal
          (qq-directory-set-friend-pinned
           "9007199254740999" nil
           (lambda (receipt) (setq callback-value receipt)))
          "friend-pin-request")))
      (should
       (equal sent
              '("friend.set_pinned"
                ((account_id . "slot-a")
                 (friend_uin . "9007199254740999")
                 (pinned . :false)))))
      (should (equal (alist-get 'friend_uin callback-value)
                     "9007199254740999"))
      (should (eq (alist-get 'pinned callback-value) :false)))))

(ert-deftest qq-directory-clock-in-delivers-domain-receipt ()
  (qq-directory-test-with-state
    (let (sent-method sent-params callback-value)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-directory-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (setq sent-method method sent-params params)
                   (funcall
                    callback
                    '((account_id . "slot-a")
                      (group_uin . "8209413637")
                      (title . "今日已打卡")
                      (keep_day_text . "连续 7 天")
                      (group_rank_text . "群排名 2")
                      (clock_in_timestamp . 1784700000)
                      (detail_url . "https://qun.qq.com/clock-in")))
                   "clock-in-request")))
        (should
         (equal
          (qq-directory-clock-in-group
           "8209413637" (lambda (receipt) (setq callback-value receipt)))
          "clock-in-request")))
      (should (equal sent-method "group.clock_in"))
      (should
       (equal sent-params
              '((account_id . "slot-a") (group_uin . "8209413637"))))
      (should (equal (alist-get 'title callback-value) "今日已打卡"))
      (should (= (alist-get 'clock_in_timestamp callback-value)
                 1784700000)))))

(ert-deftest qq-directory-at-all-query-delivers-domain-receipt ()
  (qq-directory-test-with-state
    (let (sent-method sent-params callback-value)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-directory-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (setq sent-method method sent-params params)
                   (funcall
                    callback
                    '((account_id . "slot-a")
                      (group_uin . "8209413637") (can_at_all . t)
                      (remain_at_all_count_for_uin . 3)
                      (remain_at_all_count_for_group . 9)))
                   "at-all-request")))
        (should
         (equal
          (qq-directory-get-group-at-all-remaining
           "8209413637" (lambda (receipt) (setq callback-value receipt)))
          "at-all-request")))
      (should (equal sent-method "group.get_at_all_remaining"))
      (should
       (equal sent-params
              '((account_id . "slot-a") (group_uin . "8209413637"))))
      (should (eq (alist-get 'can_at_all callback-value) t))
      (should (= (alist-get 'remain_at_all_count_for_uin callback-value) 3))
      (should (= (alist-get 'remain_at_all_count_for_group callback-value) 9)))))

(ert-deftest qq-directory-leave-revokes-owned-group-caches ()
  (qq-directory-test-with-state
    (let (sent-method sent-params callback-value)
      (puthash '("slot-a" "8209413637") '((member_count . 3))
               qq-directory--member-pages)
      (dolist (resource '(groups (group-members . "8209413637")))
        (puthash
         (qq-directory--request-key "slot-a" resource)
         (qq-directory--request-record-create
          :resource resource :owner "slot-a" :state 'active)
         qq-directory--active-requests))
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-directory-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (setq sent-method method sent-params params)
                   (funcall callback
                            '((account_id . "slot-a")
                              (group_uin . "8209413637")))
                   "leave-request")))
        (should
         (equal
          (qq-directory-leave-group
           "8209413637" (lambda (receipt) (setq callback-value receipt)))
          "leave-request")))
      (should (equal sent-method "group.leave"))
      (should
       (equal sent-params
              '((account_id . "slot-a") (group_uin . "8209413637"))))
      (should (equal (alist-get 'group_uin callback-value) "8209413637"))
      (should-not (gethash '("slot-a" "8209413637")
                           qq-directory--member-pages))
      (should-not (gethash '("slot-a" groups)
                           qq-directory--active-requests))
      (should-not (gethash
                   '("slot-a" (group-members . "8209413637"))
                   qq-directory--active-requests)))))

(ert-deftest qq-directory-group-member-settings-update-owned-page ()
  (qq-directory-test-with-state
    (let (calls receipts)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-directory-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (push (list method params) calls)
                   (funcall
                    callback
                    (pcase method
                      ("contact.list_group_members"
                       (qq-directory-test-members-result))
                      ("group.set_member_card"
                       '((account_id . "slot-a")
                         (group_uin . "8209413637")
                         (target_uin . "10002") (card . "")))
                      ("group.set_member_special_title"
                       '((account_id . "slot-a")
                         (group_uin . "8209413637")
                         (target_uin . "10002")
                         (special_title . "Lead")))))
                   method)))
        (qq-directory-list-group-members "8209413637" #'ignore)
        (qq-directory-set-group-member-card
         "8209413637" "10002" ""
         (lambda (receipt) (push receipt receipts)))
        (qq-directory-set-group-member-special-title
         "8209413637" "10002" "Lead"
         (lambda (receipt) (push receipt receipts))))
      (should (= (length receipts) 2))
      (let* ((page (qq-directory-group-member-page "8209413637"))
             (member
              (seq-find (lambda (candidate)
                          (equal (alist-get 'user_id candidate) "10002"))
                        (alist-get 'members page))))
        (should-not (alist-get 'card member))
        (should (equal (alist-get 'title member) "Lead")))
      (should
       (equal
        (nreverse calls)
        '(("contact.list_group_members"
           ((account_id . "slot-a") (group_uin . "8209413637")
            (refresh . :false)))
          ("group.set_member_card"
           ((account_id . "slot-a") (group_uin . "8209413637")
            (target_uin . "10002") (card . "")))
          ("group.set_member_special_title"
           ((account_id . "slot-a") (group_uin . "8209413637")
            (target_uin . "10002") (special_title . "Lead")))))))))

(ert-deftest qq-directory-kick-removes-cached-member-exactly-once ()
  (qq-directory-test-with-state
    (let (calls receipts)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-directory-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (push (list method params) calls)
                   (funcall
                    callback
                    (if (equal method "contact.list_group_members")
                        (qq-directory-test-members-result)
                      '((account_id . "slot-a")
                        (group_uin . "8209413637")
                        (target_uin . "10002")
                        (reject_add_request . t))))
                   method)))
        (qq-directory-list-group-members "8209413637" #'ignore)
        (dotimes (_ 2)
          (qq-directory-kick-group-member
           "8209413637" "10002" t
           (lambda (receipt) (push receipt receipts)))))
      (should (= (length receipts) 2))
      (let ((page (qq-directory-group-member-page "8209413637")))
        (should (= (alist-get 'member_count page) 2))
        (should (= (length (alist-get 'members page)) 2))
        (should-not
         (seq-find (lambda (member)
                     (equal (alist-get 'user_id member) "10002"))
                   (alist-get 'members page))))
      (should
       (equal
        (nreverse calls)
        '(("contact.list_group_members"
           ((account_id . "slot-a") (group_uin . "8209413637")
            (refresh . :false)))
          ("group.kick_member"
           ((account_id . "slot-a") (group_uin . "8209413637")
            (target_uin . "10002") (reject_add_request . t)))
          ("group.kick_member"
           ((account_id . "slot-a") (group_uin . "8209413637")
            (target_uin . "10002") (reject_add_request . t)))))))))

(ert-deftest qq-directory-members-map-and-enrich-group-ownership ()
  (qq-directory-test-with-state
    (qq-state-apply-groups
     '(((group_id . "8209413637")
        (group_name . "Protocol Lab")
        (member_count . 3)
        (max_member_count . 500))))
    (let (sent-method sent-params callback-value)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-directory-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (setq sent-method method sent-params params)
                   (funcall callback
                            (qq-directory-test-members-result))
                   "request-members")))
        (qq-directory-list-group-members
         "8209413637" (lambda (members) (setq callback-value members)))
        (should (equal sent-method "contact.list_group_members"))
        (should
         (equal sent-params
                '((account_id . "slot-a")
                  (group_uin . "8209413637")
                  (refresh . :false))))
        (should (equal (mapcar (lambda (member) (alist-get 'role member))
                               callback-value)
                       '("owner" "admin" nil)))
        (should (equal (alist-get 'native_permission (nth 2 callback-value))
                       '((kind . "unknown") (code . 99))))
        (let ((group (qq-state-group "8209413637")))
          (should (equal (alist-get 'owner_id group) "10001"))
          (should (equal (alist-get 'owner_uid group) "u_owner"))
          (should (equal (alist-get 'self_permission group) "admin")))
        (let ((page
               (qq-directory-group-member-page "8209413637")))
          (should (= (alist-get 'member_count page) 3))
          (should (= (length (alist-get 'members page)) 3)))
        (should (equal (gethash '("slot-a" "u_unknown")
                                qq-message--peer-uin-by-uid)
                       "10003"))))))

(ert-deftest qq-directory-member-count-drift-is-preserved ()
  (qq-directory-test-with-state
    (let ((result (qq-directory-test-members-result)) callback-value)
      (setf (alist-get 'member_count result) 4)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-directory-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (funcall callback result)
                   "request-members")))
        (qq-directory-list-group-members
         "8209413637" (lambda (members) (setq callback-value members)))
        (should (= (length callback-value) 3))
        (let ((page
               (qq-directory-group-member-page "8209413637")))
          (should (= (alist-get 'member_count page) 4))
          (should (= (length (alist-get 'members page)) 3)))))))

(ert-deftest qq-directory-newest-request-owns-projection ()
  (qq-directory-test-with-state
    (let (calls first-failure callbacks cancelled)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-directory-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params callback errback &optional _early)
                   (setq calls (append calls (list (cons callback errback))))
                   (format "request-%d" (length calls))))
                ((symbol-function 'qq-server-cancel)
                 (lambda (token) (push token cancelled) t)))
        (qq-directory-refresh-friends
         (lambda (_) (push 'first callbacks))
         (lambda (_body reason) (setq first-failure reason)))
        (qq-directory-refresh-friends
         (lambda (_) (push 'second callbacks)) #'ignore t)
        (should (equal cancelled '("request-1")))
        (funcall (car (nth 0 calls))
                 (qq-directory-test-friends-result))
        (should (string-match-p "superseded" first-failure))
        (should-not (qq-state-friend-categories-loaded-p))
        (funcall (car (nth 1 calls))
                 (qq-directory-test-friends-result))
        (should (equal callbacks '(second)))
        (should (qq-state-friend-categories-loaded-p))))))

(ert-deftest qq-directory-account-switch-preserves-owned-response ()
  (qq-directory-test-with-state
    (let (response-callback failure cancelled)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-directory-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (setq response-callback callback)
                   "request-friends"))
                ((symbol-function 'qq-server-cancel)
                 (lambda (token) (setq cancelled token) t)))
        (qq-directory-refresh-friends
         nil (lambda (_body reason) (setq failure reason)))
        (qq-account--upsert-account
         (qq-directory-test-account
          "slot-b" "10003" "u_other_self")
         'changed)
        (qq-account-select "slot-b")
        (should-not cancelled)
        (funcall response-callback
                 (qq-directory-test-friends-result))
        (should-not failure)
        (qq-state-with-account "slot-a"
          (should (qq-state-friend-categories-loaded-p)))
        (qq-state-with-account "slot-b"
          (should-not (qq-state-friend-categories-loaded-p)))))))

(ert-deftest qq-directory-online-phase-exit-cancels-native-session-cache ()
  (qq-directory-test-with-state
    (let (cancelled failure)
      (puthash "slot-a" "online"
               qq-directory--account-phases)
      (puthash '("slot-a" "8209413637") '((members . test))
               qq-directory--member-pages)
      (let ((request
              (qq-directory--request-record-create
               :resource 'friends :owner "slot-a" :state 'active
               :transport-token "old-token"
               :errback (lambda (_body reason) (setq failure reason)))))
        (puthash '("slot-a" friends) request
                 qq-directory--active-requests)
        (let ((account (qq-directory-test-account)))
          (setf (alist-get 'phase account) "stopped")
          (qq-account--upsert-account account 'changed))
        (cl-letf (((symbol-function 'qq-server-cancel)
                   (lambda (token) (setq cancelled token) t)))
          (qq-directory--handle-account-registry-change
           'changed "slot-a"))
        (should (equal cancelled "old-token"))
        (should (eq (qq-directory--request-record-state request)
                    'cancelled))
        (should (string-match-p "Native Session changed" failure))
        (should (= (hash-table-count
                    qq-directory--member-pages)
                   0))
        (should (= (hash-table-count
                    qq-directory--active-requests)
                   0))))))

(provide 'qq-directory-test)

;;; qq-directory-test.el ends here
