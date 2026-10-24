;;; qq-user-test.el --- Tests for qq-user -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'qq-operation-test)
(require 'cl-lib)
(require 'qq-user)
(require 'qq-chat)
(require 'qq-root)

(defconst qq-user-test--profile
  '((user_uin . "10001")
    (uid . "u_alice")
    (avatar_url . "https://q.qlogo.cn/headimg_dl?dst_uin=10001&spec=640")
    (nickname . "Alice")
    (remark . "A")
    (personal_sign . "hello from Emacs")
    (qid . "alice")
    (gender . "female")
    (age . 26)
    (birthday . ((year . 2000) (month . 7) (day . 10)))
    (location . ((country . "中国") (city . "上海")
                 (home_city) (district)))
    (school . "Example University")
    (labels . ("Emacs" "QQ"))
    (level . ((value . 42)
              (stars . 2)
              (moons . 2)
              (suns . 2)
              (crowns . 0)))
    (vip . ((kind . "svip") (level . 8) (annual . t)))
    (registration_time . 1700000000)
    (status . ((code . 10) (extended_code . 0)))
    (relationship . ((kind . "friend")
                     (category_id . 7)
                     (category_name . "Friends"))))
  "One complete native profile fixture.")

(defmacro qq-user-test-with-profile-view (&rest body)
  "Evaluate BODY in a live Appkit user-profile Surface.

BODY may refer to the lexical variables `buffer' and `view'."
  (declare (indent 0) (debug t))
  `(let ((qq-runtime--app nil)
         (qq-runtime--accounts (make-hash-table :test #'equal))
         (qq-state--partitions (make-hash-table :test #'equal))
         (qq-state--active-account-id nil)
         (qq-state-change-hook nil)
         (qq-media-cache-update-hook nil))
     (unwind-protect
         (with-temp-buffer
           (qq-user-mode)
           (setq-local qq-runtime--account-id "slot-a")
           (setq qq-user--user-id "10001")
           (let ((buffer (current-buffer))
                 (view (qq-user--ensure-view)))
             (ignore buffer view)
             ,@body))
       (qq-runtime-stop))))

(ert-deftest qq-user-render-shows-telega-style-profile-card nil
  (qq-operation-test-with-app
   (with-temp-buffer
     (qq-user-mode)
     (progn
       (setq qq-user--user-id "10001")
       (setf
	(plist-get
	 (qq-operation-test--entry (list 'profile qq-user--user-id))
	 :value)
	(copy-tree qq-user-test--profile))
       (setf
	(alist-get 'total_count
		   (plist-get
		    (qq-operation-test--entry
		     (list 'profile-likes qq-user--user-id))
		    :value))
	42))
     (cl-letf
	 (((symbol-function 'qq-media-avatar-display-string)
	   (lambda (_user-id) "@")))
       (qq-user-render))
     (let ((text (buffer-string)))
       (should (string-match-p "A" text))
       (should (string-match-p "QQ:[[:space:]]+10001" text))
       (should (string-match-p "关系:[[:space:]]+好友" text))
       (should (string-match-p "分组:[[:space:]]+Friends" text))
       (should
	(string-match-p "学校:[[:space:]]+Example University" text))
       (should (string-match-p "状态:[[:space:]]+在线" text))
       (should (string-match-p "标签:[[:space:]]+Emacs · QQ" text))
       (should (string-match-p "hello from Emacs" text))
       (should (string-match-p "获赞:[[:space:]]+42" text))
       (should-not (string-match-p "照片墙" text))
       (should (string-match-p "SVIP 8" text))
       (should (string-match-p "hello from Emacs" text))
       (should-not (string-match-p "\\[Open\\]" text))
       (should
	(equal (get-text-property (point-min) 'qq-user-profile-key)
	       '(user-profile "10001")))
       (goto-char (point-min)) (search-forward "发消息")
       (should (button-at (1- (point)))) (search-forward "点赞")
       (should (button-at (1- (point))))))))

(ert-deftest qq-user-render-omits-private-placeholder-values nil
  (qq-operation-test-with-app
   (with-temp-buffer
     (qq-user-mode)
     (progn
       (setq qq-user--user-id "10001")
       (setf
	(plist-get
	 (qq-operation-test--entry (list 'profile qq-user--user-id))
	 :value)
	'((user_uin . "10001") (nickname . "Alice")
	  (gender . "private") (age) (relationship (kind . "friend"))
	  (level) (registration_time))))
     (cl-letf
	 (((symbol-function 'qq-media-avatar-display-string)
	   (lambda (_user-id) "@")))
       (qq-user-render))
     (let ((text (buffer-string)))
       (should (string-match-p "关系:[[:space:]]+好友" text))
       (should (string-match-p "性别:[[:space:]]+保密" text))
       (should-not (string-match-p "年龄:" text))
       (should-not (string-match-p "等级:" text))
       (should-not (string-match-p "注册日期:" text))))))

(ert-deftest qq-user-render-exposes-profile-like-loading-and-errors
    nil
  (qq-operation-test-with-app
   (with-temp-buffer
     (qq-user-mode)
     (progn
       (setq qq-user--user-id "10001")
       (setf
	(plist-get
	 (qq-operation-test--entry (list 'profile qq-user--user-id))
	 :value)
	(copy-tree qq-user-test--profile))
       (setf
	(plist-get
	 (qq-operation-test--entry
	  (list 'profile-likes qq-user--user-id))
	 :pending)
	t))
     (cl-letf
	 (((symbol-function 'qq-media-avatar-display-string)
	   (lambda (_user-id) "@")))
       (qq-user-render))
     (should
      (string-match-p "获赞:[[:space:]]+加载中" (buffer-string)))
     (progn
       (setf
	(plist-get
	 (qq-operation-test--entry
	  (list 'profile-likes qq-user--user-id))
	 :pending)
	nil)
       (setf
	(plist-get
	 (qq-operation-test--entry
	  (list 'profile-likes qq-user--user-id))
	 :error)
	"native query failed"))
     (qq-user-render)
     (should
      (string-match-p "获赞:[[:space:]]+获取失败" (buffer-string))))))

(ert-deftest qq-chat-open-user-at-point-uses-sender-id ()
  (let (opened)
    (cl-letf (((symbol-function 'qq-chat--message-at-point)
               (lambda () '((sender-id . "10001"))))
              ((symbol-function 'qq-user-open)
               (lambda (user-id) (setq opened user-id))))
      (qq-chat-open-user-at-point)
      (should (equal opened "10001")))))

(ert-deftest qq-root-open-user-at-point-requires-private-session ()
  (let (opened)
    (cl-letf (((symbol-function 'qq-root--session-at-point)
               (lambda () '((type . private)
                            (target-id . "10001"))))
              ((symbol-function 'qq-user-open)
               (lambda (user-id) (setq opened user-id))))
      (qq-root-open-user-at-point)
      (should (equal opened "10001")))
    (cl-letf (((symbol-function 'qq-root--session-at-point)
               (lambda () '((type . group)
                            (target-id . "20001")))))
      (should-error (qq-root-open-user-at-point) :type 'user-error))))

(ert-deftest qq-user-entry-keymaps-are-discoverable ()
  (should (eq (lookup-key qq-chat-timeline-mode-map (kbd "i"))
              #'qq-chat-open-user-at-point))
  (should (eq (lookup-key qq-root-mode-map (kbd "i"))
              #'qq-root-open-info-at-point))
  (should (eq (lookup-key qq-user-mode-map (kbd "m"))
              #'qq-user-open-chat))
  (should (eq (lookup-key qq-user-mode-map (kbd "l"))
              #'qq-user-like))
  (should (eq (lookup-key qq-user-mode-map (kbd "p"))
              #'backward-button)))

(ert-deftest qq-user-profile-buffer-name-is-account-qualified ()
  (should
   (equal (qq-user--buffer-name "slot-a" "10001")
          "*qq-user:slot-a:10001*"))
  (should-not
   (equal (qq-user--buffer-name "slot-a" "10001")
          (qq-user--buffer-name "slot-b" "10001"))))

(ert-deftest qq-user-like-rejects-the-current-account ()
  (with-temp-buffer
    (qq-user-mode)
    (setq qq-user--user-id "10001")
    (cl-letf (((symbol-function 'qq-state-self-user-id) (lambda () "10001")))
      (should-error (qq-user-like) :type 'user-error))))

(ert-deftest qq-user-profile-survives-close-and-is-reused-by-renamed-view ()
  (qq-operation-test-with-app
    (with-temp-buffer
      (qq-user-mode) (qq-runtime-bind-account "slot-a")
      (setq qq-user--user-id "10002")
      (let* ((view (qq-user--ensure-view)) (hook (car qq-media-cache-update-hook)))
        (qq-user-refresh) (qq-operation-test--drain)
        (rename-buffer (generate-new-buffer-name " *renamed user*"))
        (should (eq view (qq-user--ensure-view)))
        (should (eq hook (car qq-media-cache-update-hook)))
        (appkit-surface-stop view)
        (dolist (call calls)
          (qq-operation-test--resolve
           call (if (equal (plist-get call :method) "profile.get")
                    '((account_id . "slot-a") (profile . ((user_uin . "10002") (nickname . "After close"))))
                  '((account_id . "slot-a") (summary . ((user_uin . "10002") (total_count . 42)))))))
        (qq-operation-test--drain)
        (should (equal (alist-get 'nickname (plist-get (qq-runtime-operation '(profile "10002")) :value)) "After close"))
        (qq-user--ensure-view)
        (should (= (length calls) 2))
        (should (= (alist-get 'total_count (qq-user--data 'profile-likes)) 42))))))

(ert-deftest qq-user-daily-limit-is-account-data-and-remains-after-reopen ()
  (qq-operation-test-with-app
    (qq-runtime-request '(profile-like "10002")) (qq-operation-test--drain)
    (qq-operation-test--resolve (car calls)
     '((account_id . "slot-a") (user_uin . "10002") (outcome . ((kind . "daily_limit")))))
    (qq-operation-test--drain)
    (with-temp-buffer
      (qq-user-mode) (qq-runtime-bind-account "slot-a") (setq qq-user--user-id "10002")
      (qq-user--ensure-view)
      (should (qq-user--like-limit-reached-p))
      (should-error (qq-user-like) :type 'user-error))
    (qq-runtime-with-account "slot-b" (should-not (qq-runtime-operation '(profile-like "10002"))))))

(ert-deftest qq-user-failed-profile-read-can-be-retried-without-retained-owner ()
  (qq-operation-test-with-app
    (with-temp-buffer
      (qq-user-mode) (qq-runtime-bind-account "slot-a") (setq qq-user--user-id "10002")
      (qq-user-refresh) (qq-operation-test--drain)
      (dolist (call calls) (funcall (plist-get (plist-get call :options) :errback) nil "offline"))
      (qq-operation-test--drain)
      (should (equal (qq-user--data 'profile :error) "offline"))
      (qq-user-refresh) (qq-operation-test--drain)
      (should (= (length calls) 4))
      (should (qq-user--data 'profile :pending)))))


(provide 'qq-user-test)

;;; qq-user-test.el ends here
