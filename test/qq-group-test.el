;;; qq-group-test.el --- Tests for qq-group -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'qq-operation-test)
(require 'cl-lib)
(require 'qq-group)
(require 'qq-contacts)
(require 'qq-chat)
(require 'qq-root)

(defconst qq-group-test--profile
  '((group_id . "20001")
    (name . "Emacs Users")
    (remark . "Work")
    (description . "Talk about Emacs")
    (announcement . "Read the rules")
    (owner_id . "10001")
    (member_count . 42)
    (max_member_count . 500)
    (active_member_count . 12)
    (created_at . 1000)
    (joined_at . 2000)
    (pinned . t)
    (mute . ((all_until) (self_until . 3000)))
    (join . ((mode . "question_answer")
             (mode_code . 4) (question . "Editor?") (answer)))
    (self_permission . "member")
    (self_permission_code . 2)
    (category . ((code . 7) (name . "Technology")))
    (grade . 3)
    (certification . ((type_code . 1) (text . "Official")))
    (school . ((location . "Shanghai") (grade_code . 2) (name . "Example")))
    (location . ((text . "Shanghai") (latitude . "31.2") (longitude . "121.5")))
    (has_custom_avatar . t))
  "One complete native group profile fixture.")

(defmacro qq-group-test-with-profile-view (&rest body)
  "Evaluate BODY in a live Appkit group-profile Surface.

BODY may refer to the lexical variables `buffer' and `view'."
  (declare (indent 0) (debug t))
  `(let ((qq-runtime--account-id
          (or (qq-state-store-account-id (qq-state-store-create)) (qq-runtime-current-account-id) "slot-a"))
         (qq-runtime--snapshots (qq-test--snapshots (qq-state-store-create))) (qq-runtime--app nil)
         (qq-runtime--accounts (make-hash-table :test #'equal)) (qq-runtime-change-hook nil)
         (qq-media-cache-update-hook nil))
     (unwind-protect
         (with-temp-buffer
           (qq-group-mode)
           (setq-local qq-runtime--account-id "slot-a")
           (setq qq-group--group-id "20001")
           (let ((buffer (current-buffer))
                 (view (qq-group--ensure-view)))
             (ignore buffer view)
             ,@body))
       (qq-runtime-stop-account "slot-a" t))))

(ert-deftest qq-group-render-shows-telega-style-card nil
  (qq-operation-test-with-app
   (with-temp-buffer
     (qq-group-mode)
     (setq qq-group--group-id "20001")
     (setf (qq-operation-test--group-profile qq-group--group-id)
	   (copy-tree qq-group-test--profile))
     (cl-letf
	 (((symbol-function 'qq-media-group-avatar-display-string)
	   (lambda (_group-id) "@")))
       (qq-group-render))
     (let ((text (buffer-string)))
       (should (string-match-p "Work" text))
       (should (string-match-p "群号:[[:space:]]+20001" text))
       (should (string-match-p "成员:[[:space:]]+42 / 500" text))
       (should (string-match-p "群主:[[:space:]]+10001" text))
       (should (string-match-p "群分类:[[:space:]]+Technology" text))
       (should (string-match-p "我的权限:[[:space:]]+成员" text))
       (should
	(string-match-p "入群模式:[[:space:]]+需正确回答问题" text))
       (should (string-match-p "认证:[[:space:]]+Official" text))
       (should-not (string-match-p "code [0-9]" text))
       (should (string-match-p "Talk about Emacs" text))
       (should (string-match-p "Read the rules" text))
       (should-not (string-match-p "\\[Open\\]" text))
       (should
	(equal (get-text-property (point-min) 'qq-group-profile-key)
	       '(group-profile "20001")))
       (goto-char (point-min)) (search-forward "打开群聊")
       (should (button-at (1- (point)))) (search-forward "搜索成员")
       (should (button-at (1- (point)))) (search-forward "取消置顶")
       (should (button-at (1- (point)))) (search-forward "@全体额度")
       (should (button-at (1- (point)))) (search-forward "退出群聊")
       (should (button-at (1- (point))))))))

(ert-deftest qq-group-render-omits-unresolved-raw-enums nil
  (qq-operation-test-with-app
   (with-temp-buffer
     (qq-group-mode)
     (setq qq-group--group-id "20001")
     (setf (qq-operation-test--group-profile qq-group--group-id)
	   '((group_id . "20001") (name . "Unknowns") (owner_id)
	     (member_count . 1) (max_member_count . 1)
	     (join (mode) (mode_code . 99)) (self_permission)
	     (self_permission_code . 99)
	     (category (code . 34) (name))
	     (certification (type_code . 99) (text))))
     (cl-letf
	 (((symbol-function 'qq-media-group-avatar-display-string)
	   (lambda (_group-id) "@")))
       (qq-group-render))
     (let ((text (buffer-string)))
       (should-not (string-match-p "code [0-9]" text))
       (should-not (string-match-p "我的权限:" text))
       (should-not (string-match-p "入群模式:" text))
       (should-not (string-match-p "群分类:" text))
       (should-not (string-match-p "认证:" text))))))

(ert-deftest qq-group-member-search-button-prompts-and-dispatches nil
  (qq-operation-test-with-app
   (with-temp-buffer
     (qq-group-mode)
     (setq qq-group--group-id "20001")
     (setf (qq-operation-test--group-profile qq-group--group-id)
	   (copy-tree qq-group-test--profile))
     (let (call)
       (cl-letf
	   (((symbol-function 'qq-media-group-avatar-display-string)
	     (lambda (_group-id) "@"))
	    ((symbol-function 'read-string)
	     (lambda (&rest _args) "synthetic member"))
	    ((symbol-function 'qq-contacts-search-group-members)
	     (lambda (group-id query)
	       (setq call (list group-id query)))))
	 (qq-group-render) (goto-char (point-min))
	 (search-forward "搜索成员")
	 (push-button (button-at (1- (point))))
	 (should (equal call '("20001" "synthetic member"))))))))

(ert-deftest qq-group-setting-buttons-call-their-commands-interactively ()
  (qq-operation-test-with-app
    (with-temp-buffer
      (qq-group-mode) (setq qq-group--group-id "20001")
      (setf (qq-operation-test--group-profile "20001") (copy-tree qq-group-test--profile))
      (let (intents)
        (cl-letf (((symbol-function 'qq-media-group-avatar-display-string) (lambda (_) "@"))
                  ((symbol-function 'read-string)
                   (lambda (prompt &rest _) (if (string-prefix-p "新群名称" prompt) "New Name" "New Remark")))
                  ((symbol-function 'y-or-n-p) (lambda (_) t))
                  ((symbol-function 'qq-runtime-request) (lambda (request &rest _) (push request intents))))
          (qq-group-render)
          (dolist (label '("修改群名" "修改备注" "全员禁言" "取消置顶"))
            (goto-char (point-min)) (search-forward label) (push-button (button-at (1- (point))))))
        (should (equal (nreverse intents) (list (qq-runtime-request-create 'group-setting :group-id "20001" :field 'name :new-value "New Name") (qq-runtime-request-create 'group-setting :group-id "20001" :field 'remark :new-value "New Remark") (qq-runtime-request-create 'group-setting :group-id "20001" :field 'whole-mute :new-value t) (qq-runtime-request-create 'conversation-pinned :session-key "group:20001" :pinned nil))))))))













(ert-deftest qq-group-leave-cancellation-sends-nothing ()
  (with-temp-buffer
    (qq-group-mode)
    (setq qq-group--group-id "20001"
          qq-group--profile (copy-tree qq-group-test--profile))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) nil))
              ((symbol-function 'qq-core-leave-group)
               (lambda (&rest _arguments)
                 (ert-fail "cancelled group leave was dispatched"))))
      (should-error (qq-group-leave) :type 'user-error))))





(ert-deftest qq-group-entry-commands-dispatch-by-session-type ()
  (let (opened-user opened-group)
    (cl-letf (((symbol-function 'qq-root--session-at-point)
               (lambda () '((type . group) (target-id . "20001"))))
              ((symbol-function 'qq-group-open)
               (lambda (group-id) (setq opened-group group-id))))
      (qq-root-open-info-at-point)
      (should (equal opened-group "20001")))
    (cl-letf (((symbol-function 'qq-root--session-at-point)
               (lambda () '((type . private)
                            (target-id . "10001")
                            (peer-uin . "10001"))))
              ((symbol-function 'qq-user-open)
               (lambda (user-id) (setq opened-user user-id))))
      (qq-root-open-info-at-point)
      (should (equal opened-user "10001")))))

(ert-deftest qq-group-entry-keymaps-match-chat-info-conventions ()
  (should (eq (lookup-key qq-root-mode-map (kbd "i"))
              #'qq-root-open-info-at-point))
  (should (eq (lookup-key qq-chat-timeline-mode-map (kbd "h"))
              #'qq-chat-open-peer-info))
  (should (eq (lookup-key qq-chat-mode-map (kbd "C-c i"))
              #'qq-chat-open-peer-info))
  (should (eq (lookup-key qq-group-mode-map (kbd "m"))
              #'qq-group-open-chat))
  (should (eq (lookup-key qq-group-mode-map (kbd "s"))
              #'qq-group-search-members))
  (should (eq (lookup-key qq-group-mode-map (kbd "N"))
              #'qq-group-set-name))
  (should (eq (lookup-key qq-group-mode-map (kbd "R"))
              #'qq-group-set-remark))
  (should (eq (lookup-key qq-group-mode-map (kbd "P"))
              #'qq-group-toggle-pinned))
  (should (eq (lookup-key qq-group-mode-map (kbd "M"))
              #'qq-group-set-whole-mute))
  (should (eq (lookup-key qq-group-mode-map (kbd "S"))
              #'qq-group-clock-in))
  (should (eq (lookup-key qq-group-mode-map (kbd "@"))
              #'qq-group-show-at-all-remaining))
  (should (eq (lookup-key qq-group-mode-map (kbd "L"))
              #'qq-group-leave)))

(ert-deftest qq-group-profile-buffer-name-is-account-qualified ()
  (should
   (equal (qq-group--buffer-name "slot-a" "20001")
          "*qq-group:slot-a:20001*"))
  (should-not
   (equal (qq-group--buffer-name "slot-a" "20001")
          (qq-group--buffer-name "slot-b" "20001"))))

(ert-deftest qq-group-settings-commit-to-shared-state-after-view-switch ()
  (qq-operation-test-with-app
    (with-temp-buffer
      (qq-group-mode) (qq-runtime-bind-account "slot-a") (setq qq-group--group-id "20001")
      (qq-group--ensure-view)
      (setf (qq-operation-test--group-profile "20001") (copy-tree qq-group-test--profile))
      (qq-group-set-name "Updated")
      (qq-operation-test--drain)
      (should (equal (plist-get (seq-find (lambda (call) (equal (plist-get call :method) "group.set_name")) calls) :params)
                     '((account_id . "slot-a") (group_uin . "20001") (name . "Updated"))))
      (qq-group--select-group "20002")
      (qq-operation-test--resolve (seq-find (lambda (call) (equal (plist-get call :method) "group.set_name")) calls) '((account_id . "slot-a")))
      (qq-operation-test--drain)
      (should (equal (alist-get 'name (qq-core--group-profile-from-state "20001")) "Updated"))
      (should-not (qq-core--group-profile-from-state "20002")))))

(ert-deftest qq-group-live-quota-and-clock-receipts-use-exact-surface-presentation ()
  (qq-operation-test-with-app
    (with-temp-buffer
      (qq-group-mode) (qq-runtime-bind-account "slot-a") (setq qq-group--group-id "20001")
      (qq-group--ensure-view)
      (let (displayed)
        (cl-letf (((symbol-function 'message) (lambda (format &rest args) (push (apply #'format format args) displayed))))
          (qq-group-show-at-all-remaining) (qq-operation-test--drain)
          (qq-operation-test--resolve (car calls)
           '((can_at_all . t) (remain_at_all_count_for_uin . 2) (remain_at_all_count_for_group . 8)))
          (qq-operation-test--drain)
          (should (seq-some (lambda (s) (string-match-p "个人剩余 2 次，群剩余 8 次" s)) displayed))
          (qq-group-clock-in) (qq-operation-test--drain)
          (qq-operation-test--resolve (car calls) '((title . "打卡") (keep_day_text . "3 days")))
          (qq-operation-test--drain)
          (should (seq-some (lambda (s) (string-match-p "3 days" s)) displayed)))))))

(ert-deftest qq-group-late-leave-commits-without-closing-reused-profile ()
  (qq-operation-test-with-app
    (with-temp-buffer
      (qq-group-mode) (qq-runtime-bind-account "slot-a") (setq qq-group--group-id "20001")
      (qq-group--ensure-view)
      (setf (qq-operation-test--group-profile "20001") (copy-tree qq-group-test--profile))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t))) (qq-group-leave))
      (qq-operation-test--drain)
      (qq-group--select-group "20002")
      (qq-operation-test--resolve (seq-find (lambda (call) (equal (plist-get call :method) "group.leave")) calls) '((account_id . "slot-a")))
      (qq-operation-test--drain)
      (should (buffer-live-p (current-buffer)))
      (should (equal qq-group--group-id "20002"))
      (should-not (qq-state-group (qq-runtime-state) "20001")))))

(ert-deftest qq-group-view-reuse-keeps-one-media-listener-and-shared-profile ()
  (qq-operation-test-with-app
    (with-temp-buffer
      (qq-group-mode) (qq-runtime-bind-account "slot-a") (setq qq-group--group-id "20001")
      (let* ((view (qq-group--ensure-view)) (hook (car qq-media-cache-update-hook)))
        (setf (qq-operation-test--group-profile "20001") (copy-tree qq-group-test--profile))
        (rename-buffer (generate-new-buffer-name " *renamed group*"))
        (should (eq view (qq-group--ensure-view)))
        (should (eq hook (car qq-media-cache-update-hook)))
        (appkit-surface-stop view)
        (should-not (memq hook qq-media-cache-update-hook))
        (should (equal (alist-get 'name (qq-core--group-profile-from-state "20001")) "Emacs Users"))))))

(provide 'qq-group-test)

;;; qq-group-test.el ends here
