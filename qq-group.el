;;; qq-group.el --- Native QQ group profile buffer -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; Telega-style group profile page backed by the native Gateway.  Unknown
;; native enum values remain visible as numeric codes.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-invalidation)
(require 'appkit-transaction)
(require 'qq-core)
(require 'qq-media)
(require 'qq-runtime)
(require 'qq-state)
(require 'qq-protocol)
(require 'appkit-ui)
(require 'appkit-presentation)
(require 'appkit-position)

(declare-function qq-chat-open "qq-chat" (session-key))
(declare-function qq-contacts-search-group-members
                  "qq-contacts" (group-id query))
(declare-function qq-user-open "qq-user" (user-id))
(defconst qq-group--view-id 'group-profile
  "Stable Appkit identity of the singleton group-profile view.")

(defface qq-group-action-button
  '((t :inherit mode-line-inactive :weight semi-bold
     :box (:line-width -1 :style released-button)))
  "Face used for action buttons on QQ group cards."
  :group 'qq)

(defface qq-group-card-title
  '((t :inherit bold :height 1.15))
  "Face used for the primary title on QQ group cards."
  :group 'qq)

(defface qq-group-destructive-action-button
  '((t :inherit (error qq-group-action-button)))
  "Face used for destructive QQ group actions."
  :group 'qq)

(defvar-local qq-group--group-id nil
  "Group code displayed by the current group buffer.")

(defvar-local qq-group--profile nil
  "Native group profile displayed by the current group buffer.")

(defvar-local qq-group--request nil
  "Active group-profile request token.")

(defvar-local qq-group--request-owner nil
  "Owner object for the active group-profile request.")

(defvar-local qq-group--loading nil
  "Non-nil while the group profile is loading.")

(defvar-local qq-group--error nil
  "Last group-profile loading error, or nil.")

(defvar-local qq-group--media-hook-function nil
  "View-owned media cache hook installed for this group buffer.")

(defun qq-group--buffer-name (account-id group-id)
  "Return ACCOUNT-ID-qualified profile buffer name for GROUP-ID."
  (qq-runtime-account-buffer-name "group" group-id account-id))

(defun qq-group--profile-key (&optional group-id)
  "Return the stable presentation key for opaque GROUP-ID.

GROUP-ID defaults to the identity selected in the current buffer."
  (list 'group-profile (or group-id qq-group--group-id)))

(defun qq-group--present-string (value)
  "Return non-empty string VALUE, or nil."
  (and (stringp value) (not (string-empty-p value)) value))

(defun qq-group--display-name ()
  "Return the best title for the current group profile."
  (or (qq-group--present-string (alist-get 'remark qq-group--profile))
      (qq-group--present-string (alist-get 'name qq-group--profile))
      qq-group--group-id
      "QQ group"))

(defun qq-group--header-line ()
  "Return the dynamic header line for the group profile."
  (format " QQ Group · %s (%s)%s"
          (qq-group--display-name)
          (or qq-group--group-id "unknown")
          (if qq-group--loading " · loading" "")))

(defun qq-group--insert-field (label value &optional face)
  "Insert group profile LABEL and VALUE when VALUE is present."
  (when (and value (not (equal value "")))
    (let ((start (point)))
      (insert (format "%-12s" (concat label ":")))
      (add-text-properties start (point) '(face bold)))
    (let ((start (point)))
      (insert (format "%s" value) "\n")
      (when face
        (add-text-properties start (point) (list 'face face))))))

(defun qq-group--timestamp-label (value)
  "Return a readable local timestamp for positive integer VALUE."
  (when (and (integerp value) (> value 0))
    (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time value))))

(defun qq-group--boolean-label (value)
  "Return an exact display label for JSON boolean VALUE."
  (cond ((eq value t) "是")
        ((eq value :false) "否")
        (t nil)))

(defun qq-group--permission-label (value)
  "Return a localized label for verified permission VALUE."
  (pcase value
    ("member" "成员")
    ("admin" "管理员")
    ("owner" "群主")))

(defun qq-group--join-mode-label (value)
  "Return a localized label for verified native join-mode VALUE."
  (pcase value
    ("open" "允许任何人加入")
    ("approval" "需管理员审核")
    ("closed" "不允许任何人加入")
    ("question_answer" "需正确回答问题")
    ("question_approval" "回答问题后由管理员审核")))

(defun qq-group-open-chat ()
  "Open the current group chat."
  (interactive)
  (unless qq-group--group-id
    (user-error "qq: this buffer has no group identity"))
  (qq-chat-open (qq-state-session-key 'group qq-group--group-id)))

(defun qq-group-open-avatar ()
  "Open the current group's avatar."
  (interactive)
  (unless qq-group--group-id
    (user-error "qq: this buffer has no group identity"))
  (qq-media-open-group-avatar qq-group--group-id))

(defun qq-group-open-owner ()
  "Open the current group owner's user page."
  (interactive)
  (let ((owner-id (alist-get 'owner_id qq-group--profile)))
    (unless (qq-protocol-user-uin-p owner-id)
      (user-error "qq: group owner identity is unavailable"))
    (require 'qq-user)
    (qq-user-open owner-id)))

(defun qq-group-copy-id ()
  "Copy the current group code."
  (interactive)
  (unless qq-group--group-id
    (user-error "qq: this buffer has no group identity"))
  (kill-new qq-group--group-id)
  (message "qq: copied group id %s" qq-group--group-id))

(defun qq-group-search-members (&optional query)
  "Search exact native members of the current group for QUERY."
  (interactive)
  (unless qq-group--group-id
    (user-error "qq: this buffer has no group identity"))
  (setq query (or query (read-string "搜索群成员: ")))
  (require 'qq-contacts)
  (qq-contacts-search-group-members qq-group--group-id query))


(defun qq-group--setting-current-p (buffer group-id)
  "Return non-nil when BUFFER still displays GROUP-ID."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'qq-group-mode)
              (equal qq-group--group-id group-id)))))

(defun qq-group--apply-setting (buffer group-id field value message-text _receipt)
  "Apply confirmed FIELD VALUE to GROUP-ID's profile in BUFFER."
  (when (qq-group--setting-current-p buffer group-id)
    (with-current-buffer buffer
      (setf (alist-get field qq-group--profile nil nil #'eq) value)
      (qq-group--request-sync)
      (message "qq: %s" message-text))))

(defun qq-group-set-name (name)
  "Set the current group's public NAME through the selected QQ account."
  (interactive
   (list (read-string "新群名称: "
                      (alist-get 'name qq-group--profile))))
  (unless qq-group--group-id
    (user-error "qq: this buffer has no group identity"))
  (unless (and (stringp name) (not (string-empty-p name)))
    (user-error "qq: group name must be a non-empty string"))
  (qq-core-set-group-name
   qq-group--group-id name
   (apply-partially #'qq-group--apply-setting
                    (current-buffer) qq-group--group-id 'name name
                    "群名称已更新")))

(defun qq-group-set-remark (remark)
  "Set or clear the current group's account-local REMARK."
  (interactive
   (list (read-string "群备注（留空清除）: "
                      (or (alist-get 'remark qq-group--profile) ""))))
  (unless qq-group--group-id
    (user-error "qq: this buffer has no group identity"))
  (unless (stringp remark)
    (user-error "qq: group remark must be a string"))
  (qq-core-set-group-remark
   qq-group--group-id remark
   (apply-partially #'qq-group--apply-setting
                    (current-buffer) qq-group--group-id 'remark
                    (and (not (string-empty-p remark)) remark)
                    (if (string-empty-p remark)
                        "群备注已清除"
                      "群备注已更新"))))

(defun qq-group--apply-whole-mute
    (buffer group-id enabled _receipt)
  "Apply confirmed whole-mute ENABLED state in BUFFER for GROUP-ID."
  (when (qq-group--setting-current-p buffer group-id)
    (with-current-buffer buffer
      (let ((mute (copy-tree (or (alist-get 'mute qq-group--profile) '()))))
        (setf (alist-get 'all_until mute nil nil #'eq)
              (and enabled #xffffffff))
        (setf (alist-get 'mute qq-group--profile nil nil #'eq) mute))
      (qq-group--request-sync)
      (message "qq: 已%s全员禁言" (if enabled "开启" "关闭")))))

(defun qq-group-set-whole-mute (enabled)
  "Set the current group's whole-group mute state to ENABLED."
  (interactive (list (y-or-n-p "开启全员禁言？")))
  (unless qq-group--group-id
    (user-error "qq: this buffer has no group identity"))
  (setq enabled (and enabled t))
  (qq-core-set-group-whole-mute
   qq-group--group-id enabled
   (apply-partially #'qq-group--apply-whole-mute
                    (current-buffer) qq-group--group-id enabled)))

(defun qq-group-toggle-pinned ()
  "Toggle the current group's conversation pinned state."
  (interactive)
  (unless qq-group--group-id
    (user-error "qq: this buffer has no group identity"))
  (let ((pinned (not (eq (alist-get 'pinned qq-group--profile) t))))
    (qq-core-set-group-pinned
     qq-group--group-id pinned
     (apply-partially #'qq-group--apply-setting
                      (current-buffer) qq-group--group-id 'pinned
                      (if pinned t :false)
                      (if pinned "群会话已置顶" "群会话已取消置顶")))))

(defun qq-group--finish-clock-in (buffer group-id receipt)
  "Report a successful clock-in RECEIPT when BUFFER still owns GROUP-ID."
  (when (qq-group--setting-current-p buffer group-id)
    (with-current-buffer buffer
      (let ((details
             (delq nil
                   (mapcar
                    #'qq-group--present-string
                    (list (alist-get 'title receipt)
                          (alist-get 'keep_day_text receipt)
                          (alist-get 'group_rank_text receipt))))))
        (message "qq: 群打卡成功%s"
                 (if details
                     (concat "：" (string-join details " · "))
                   ""))))))

(defun qq-group-clock-in ()
  "Clock the selected account into the current group."
  (interactive)
  (unless qq-group--group-id
    (user-error "qq: this buffer has no group identity"))
  (qq-core-clock-in-group
   qq-group--group-id
   (apply-partially #'qq-group--finish-clock-in
                    (current-buffer) qq-group--group-id)))

(defun qq-group--finish-at-all-remaining (buffer group-id receipt)
  "Report @all quota RECEIPT when BUFFER still owns exact GROUP-ID."
  (when (qq-group--setting-current-p buffer group-id)
    (with-current-buffer buffer
      (message
       "qq: %s使用 @全体；个人剩余 %d 次，群剩余 %d 次"
       (if (eq (alist-get 'can_at_all receipt) t) "可以" "当前不可")
       (alist-get 'remain_at_all_count_for_uin receipt)
       (alist-get 'remain_at_all_count_for_group receipt)))))

(defun qq-group-show-at-all-remaining ()
  "Show live @all availability and quotas for the current group."
  (interactive)
  (unless qq-group--group-id
    (user-error "qq: this buffer has no group identity"))
  (qq-core-get-group-at-all-remaining
   qq-group--group-id
   (apply-partially #'qq-group--finish-at-all-remaining
                    (current-buffer) qq-group--group-id)))

(defun qq-group--finish-leave (buffer group-id _receipt)
  "Close BUFFER after confirmed leave when it still owns exact GROUP-ID."
  (when (qq-group--setting-current-p buffer group-id)
    (message "qq: 已退出群聊 %s" group-id)
    (let ((kill-buffer-query-functions nil))
      (kill-buffer buffer))))

(defun qq-group-leave ()
  "Confirm and leave the current group through the selected QQ account."
  (interactive)
  (unless qq-group--group-id
    (user-error "qq: this buffer has no group identity"))
  (let* ((group-id qq-group--group-id)
         (group-name
          (or (qq-group--present-string (alist-get 'name qq-group--profile))
              (qq-group--present-string (alist-get 'remark qq-group--profile))
              "未命名群聊")))
    (unless (yes-or-no-p
             (format "确认退出群聊 %s（%s）？" group-name group-id))
      (user-error "qq: 已取消退出群聊"))
    (qq-core-leave-group
     group-id
     (apply-partially #'qq-group--finish-leave
                      (current-buffer) group-id))))

(defun qq-group--insert-action-buttons ()
  "Insert primary group action buttons."
  (insert "  ")
  (appkit-ui-insert-action-button
   " 打开群聊 " #'qq-group-open-chat
   :face 'qq-group-action-button :help-echo "打开群聊 (m)")
  (insert "  ")
  (appkit-ui-insert-action-button
   " 查看头像 " #'qq-group-open-avatar
   :face 'qq-group-action-button :help-echo "查看群头像 (a)")
  (insert "  ")
  (appkit-ui-insert-action-button
   " 搜索成员 " #'qq-group-search-members
   :face 'qq-group-action-button :help-echo "原生搜索群成员 (s)")
  (when (qq-protocol-user-uin-p (alist-get 'owner_id qq-group--profile))
    (insert "  ")
    (appkit-ui-insert-action-button
     " 群主资料 " #'qq-group-open-owner
     :face 'qq-group-action-button :help-echo "打开群主资料 (o)"))
  (insert "  ")
  (appkit-ui-insert-action-button
   " 复制群号 " #'qq-group-copy-id
   :face 'qq-group-action-button :help-echo "复制群号 (w)")
  (insert "\n  ")
  (appkit-ui-insert-action-button
   " 修改群名 " (lambda () (call-interactively #'qq-group-set-name))
   :face 'qq-group-action-button :help-echo "修改群名称 (N)")
  (insert "  ")
  (appkit-ui-insert-action-button
   " 修改备注 " (lambda () (call-interactively #'qq-group-set-remark))
   :face 'qq-group-action-button :help-echo "修改群备注 (R)")
  (insert "  ")
  (appkit-ui-insert-action-button
   (if (eq (alist-get 'pinned qq-group--profile) t)
       " 取消置顶 "
     " 置顶群聊 ")
   #'qq-group-toggle-pinned
   :face 'qq-group-action-button :help-echo "切换群会话置顶状态 (P)")
  (insert "  ")
  (appkit-ui-insert-action-button
   " 全员禁言 " (lambda () (call-interactively #'qq-group-set-whole-mute))
   :face 'qq-group-action-button :help-echo "开启或关闭全员禁言 (M)")
  (insert "  ")
  (appkit-ui-insert-action-button
   " 群打卡 " #'qq-group-clock-in
   :face 'qq-group-action-button :help-echo "群打卡 (S)")
  (insert "  ")
  (appkit-ui-insert-action-button
   " @全体额度 " #'qq-group-show-at-all-remaining
   :face 'qq-group-action-button :help-echo "查询 @全体剩余额度 (@)")
  (insert "  ")
  (appkit-ui-insert-action-button
   " 退出群聊 " #'qq-group-leave
   :face 'qq-group-destructive-action-button :help-echo "退出群聊 (L)")
  (insert "\n"))

(defun qq-group-render ()
  "Render the current group profile buffer."
  (interactive)
  (appkit-position-render-preserving
   (lambda ()
     (let ((inhibit-read-only t))
       (erase-buffer)
       (setq-local header-line-format '(:eval (qq-group--header-line)))
       (cond
        (qq-group--loading
         (appkit-presentation-insert-note-line "Loading group profile…"))
        (qq-group--error
         (appkit-presentation-insert-note-line qq-group--error :face 'error))
        ((null qq-group--profile)
         (appkit-presentation-insert-note-line "No group profile loaded."))
        (t
         (let ((avatar-start (point)))
           (insert (qq-media-group-avatar-display-string qq-group--group-id))
           (make-text-button
            avatar-start (point)
            'follow-link t
            'action (lambda (_button) (qq-group-open-avatar))
            'help-echo "查看群头像"
            'qq-group-id qq-group--group-id)
           (insert "  "
                   (propertize (qq-group--display-name)
                               'face 'qq-group-card-title)
                   "\n   "
                   (propertize
                    (format "%d / %d 位成员"
                            (alist-get 'member_count qq-group--profile)
                            (alist-get 'max_member_count qq-group--profile))
                    'face 'shadow)
                   "\n"))
         (insert "\n")
         (qq-group--insert-action-buttons)
         (appkit-presentation-insert-note-line
          "g 刷新 · N 群名 · R 备注 · P 置顶 · M 全员禁言 · S 群打卡 · @ 全体额度 · L 退出群聊 · s 成员（结果页 C 名片 / T 头衔 / K 移出） · q 关闭")
         (insert "\n")
         (appkit-presentation-insert-heading-line "资料" :face 'bold)
         (let ((name (qq-group--present-string
                      (alist-get 'name qq-group--profile)))
               (remark (qq-group--present-string
                        (alist-get 'remark qq-group--profile))))
           (when (and name remark (not (equal name remark)))
             (qq-group--insert-field "群名称" name)))
         (qq-group--insert-field "群号" qq-group--group-id)
         (qq-group--insert-field "群主" (alist-get 'owner_id qq-group--profile))
         (qq-group--insert-field
          "成员"
          (format "%d / %d"
                  (alist-get 'member_count qq-group--profile)
                  (alist-get 'max_member_count qq-group--profile)))
         (qq-group--insert-field
          "活跃成员" (alist-get 'active_member_count qq-group--profile))
         (qq-group--insert-field
          "创建时间" (qq-group--timestamp-label
                      (alist-get 'created_at qq-group--profile)))
         (qq-group--insert-field
          "入群时间" (qq-group--timestamp-label
                      (alist-get 'joined_at qq-group--profile)))
         (qq-group--insert-field
          "置顶" (qq-group--boolean-label (alist-get 'pinned qq-group--profile)))
         (qq-group--insert-field
          "群分类" (qq-group--present-string
                    (alist-get 'name (alist-get 'category qq-group--profile))))
         (qq-group--insert-field "群等级" (alist-get 'grade qq-group--profile))
         (qq-group--insert-field
          "我的权限" (qq-group--permission-label
                      (alist-get 'self_permission qq-group--profile)))
         (when-let* ((mute (alist-get 'mute qq-group--profile)))
           (qq-group--insert-field
            "全员禁言"
            (if (let ((until (alist-get 'all_until mute)))
                  (and (integerp until) (> until 0)))
                "是"
              "否"))
           (qq-group--insert-field
            "全员禁言至" (qq-group--timestamp-label (alist-get 'all_until mute)))
           (qq-group--insert-field
            "我的禁言至" (qq-group--timestamp-label (alist-get 'self_until mute))))
         (when-let* ((join (alist-get 'join qq-group--profile)))
           (qq-group--insert-field
            "入群模式" (qq-group--join-mode-label (alist-get 'mode join)))
           (qq-group--insert-field "入群问题" (alist-get 'question join))
           (qq-group--insert-field "我的答案" (alist-get 'answer join)))
         (qq-group--insert-field
          "认证" (qq-group--present-string
                  (alist-get 'text (alist-get 'certification qq-group--profile))))
         (when-let* ((school (alist-get 'school qq-group--profile)))
           (qq-group--insert-field
            "学校"
            (string-join
             (delq nil
                   (list (qq-group--present-string (alist-get 'name school))
                         (qq-group--present-string (alist-get 'location school))))
             " · ")))
         (when-let* ((location (alist-get 'location qq-group--profile)))
           (qq-group--insert-field "地点" (alist-get 'text location)))
         (dolist (section '(("群简介" . description)
                            ("群公告" . announcement)))
           (when-let* ((text (qq-group--present-string
                              (alist-get (cdr section) qq-group--profile))))
             (insert "\n")
             (appkit-presentation-insert-heading-line (car section) :face 'bold)
             (insert text "\n")))))
       (add-text-properties
        (point-min) (point-max)
        (list 'qq-group-profile-key (qq-group--profile-key)
              'rear-nonsticky '(qq-group-profile-key)))
       (goto-char (point-min))))
   :anchor-property 'qq-group-profile-key
   :preserve-window-start t))

(defun qq-group--view-current-p (view)
  "Return non-nil when VIEW still owns this group-profile buffer."
  (and (appkit-view-live-p view)
       (equal (appkit-view-id view) qq-group--view-id)
       (with-current-buffer (appkit-view-buffer view)
         (and (derived-mode-p 'qq-group-mode)
              (eq view (appkit-current-view))))))

(defun qq-group--live-current-view ()
  "Return the live group-profile view attached to this buffer, or nil."
  (let ((view (appkit-current-view)))
    (and (qq-group--view-current-p view) view)))

(cl-defun qq-group--request-sync (&optional view &key resource)
  "Request one coalesced group-profile sync for live VIEW.

RESOURCE identifies a presentation-only media dependency update."
  (when-let* ((view (or view (qq-group--live-current-view))))
    (if resource
        (appkit-request-sync
         view :entry (qq-group--profile-key) :resource resource)
      (appkit-request-sync view :structure t :part 'profile))))

(defun qq-group--sync-now (view)
  "Consume pending invalidations for live group-profile VIEW."
  (when (qq-group--view-current-p view)
    (appkit-sync-invalidations view)))

(defun qq-group--sync-invalidations (view invalidations _events)
  "Render group profile VIEW from coalesced INVALIDATIONS."
  (when (and (appkit-invalidations-affect-p invalidations '(profile))
             (qq-group--view-current-p view))
    (appkit-with-content-update view
      (qq-group-render))))

(defun qq-group--request-current-p (view buffer group-id owner)
  "Return non-nil when VIEW and OWNER still load GROUP-ID in BUFFER."
  (and (qq-group--view-current-p view)
       (eq (appkit-view-buffer view) buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'qq-group-mode)
              (eq view (appkit-current-view))
              (equal qq-group--group-id group-id)
              (eq qq-group--request-owner owner)))))

(defun qq-group-refresh ()
  "Refresh the current native group profile."
  (interactive)
  (unless qq-group--group-id
    (user-error "qq: this buffer has no group identity"))
  (let ((view (qq-group--ensure-view)))
    (when qq-group--request
      (qq-request-cancel qq-group--request))
    (let ((buffer (current-buffer))
          (group-id qq-group--group-id)
          (owner (list 'group-profile qq-group--group-id)))
      (setq qq-group--loading t
            qq-group--error nil
            qq-group--request nil
            qq-group--request-owner owner)
      (qq-group--request-sync view)
      (condition-case error-data
          (let ((request
                  (qq-core-get-group
                   group-id
                   (lambda (profile)
                     (when (qq-group--request-current-p
                            view buffer group-id owner)
                       (with-current-buffer buffer
                         (setq qq-group--profile profile
                               qq-group--loading nil
                               qq-group--error nil
                               qq-group--request nil
                               qq-group--request-owner nil)
                         (qq-group--request-sync view))))
                   (lambda (_response reason)
                     (when (qq-group--request-current-p
                            view buffer group-id owner)
                       (with-current-buffer buffer
                         (setq qq-group--loading nil
                               qq-group--error
                               (format "Unable to load group: %s"
                                       (or reason "unknown error"))
                               qq-group--request nil
                               qq-group--request-owner nil)
                         (qq-group--request-sync view)
                         (message "qq: %s" (or reason "native request failed"))))))))
            (when (eq qq-group--request-owner owner)
              (setq qq-group--request request)))
        (error
         (when (qq-group--request-current-p view buffer group-id owner)
           (setq qq-group--loading nil
                 qq-group--error
                 (format "Unable to load group: %s"
                         (error-message-string error-data))
                 qq-group--request nil
                 qq-group--request-owner nil)
           (qq-group--request-sync view))))
      (qq-group--sync-now view))))

(defun qq-group--cancel-request ()
  "Cancel the active group-profile request."
  (when qq-group--request
    (qq-request-cancel qq-group--request))
  (setq qq-group--request nil
        qq-group--request-owner nil
        qq-group--loading nil))

(defun qq-group--clear-view-data ()
  "Clear account-scoped data projected by the current group view."
  (setq qq-group--profile nil
        qq-group--error nil))

(defun qq-group--reset-buffer-work (buffer)
  "Reset requests, data, and media hook state retained by BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (derived-mode-p 'qq-group-mode)
        (when qq-group--media-hook-function
          (remove-hook 'qq-media-cache-update-hook
                       qq-group--media-hook-function))
        (setq qq-group--media-hook-function nil)
        (qq-group--cancel-request)
        (qq-group--clear-view-data)))))

(defun qq-group--release-view-work (view buffer)
  "Release BUFFER work when it is still owned by group-profile VIEW."
  (when (and (buffer-live-p buffer)
             (with-current-buffer buffer
               (eq view (appkit-current-view))))
    (qq-group--reset-buffer-work buffer)))

(defun qq-group--setup-view (view)
  "Reset replacement state and register lifecycle work for group VIEW."
  (let ((buffer (appkit-view-buffer view)))
    (qq-group--reset-buffer-work buffer)
    (appkit-register-handle
     view 'function
     (apply-partially #'qq-group--release-view-work view buffer))
    (let ((hook (apply-partially
                 #'qq-group--handle-media-cache-update view)))
      (with-current-buffer buffer
        (setq qq-group--media-hook-function hook))
      (appkit-register-handle
       view 'hook
       (list 'qq-media-cache-update-hook hook nil buffer))
      (add-hook 'qq-media-cache-update-hook hook))))

(defun qq-group--ensure-view ()
  "Return the live Appkit view owning the current group buffer."
  (unless qq-group--group-id
    (error "QQ: cannot attach a group view without a group identity"))
  (qq-runtime-ensure-account-view
   :id qq-group--view-id
   :mode 'qq-group-mode
   :sync-function #'qq-group--sync-invalidations
   :parts '(profile)
   :setup #'qq-group--setup-view))

(defun qq-group--select-group (group-id)
  "Prepare the shared group buffer to display GROUP-ID."
  (unless (equal qq-group--group-id group-id)
    (qq-group--cancel-request)
    (qq-group--clear-view-data))
  (setq qq-group--group-id group-id))

(defun qq-group-button-backward ()
  "Move point to the previous page button."
  (interactive)
  (forward-button -1))

(defvar qq-group-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'qq-group-refresh)
    (define-key map (kbd "m") #'qq-group-open-chat)
    (define-key map (kbd "a") #'qq-group-open-avatar)
    (define-key map (kbd "s") #'qq-group-search-members)
    (define-key map (kbd "o") #'qq-group-open-owner)
    (define-key map (kbd "w") #'qq-group-copy-id)
    (define-key map (kbd "N") #'qq-group-set-name)
    (define-key map (kbd "R") #'qq-group-set-remark)
    (define-key map (kbd "P") #'qq-group-toggle-pinned)
    (define-key map (kbd "M") #'qq-group-set-whole-mute)
    (define-key map (kbd "S") #'qq-group-clock-in)
    (define-key map (kbd "@") #'qq-group-show-at-all-remaining)
    (define-key map (kbd "L") #'qq-group-leave)
    (define-key map (kbd "TAB") #'forward-button)
    (define-key map (kbd "<backtab>") #'qq-group-button-backward)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `qq-group-mode'.")

(define-derived-mode qq-group-mode special-mode "QQ-Group"
  "Major mode for one native QQ group profile."
  (setq-local truncate-lines nil)
  (setq-local switch-to-buffer-preserve-window-point nil)
  (setq-local header-line-format '(:eval (qq-group--header-line)))
  (add-hook 'change-major-mode-hook #'qq-group--cancel-request nil t)
  (add-hook 'kill-buffer-hook #'qq-group--cancel-request nil t))

;;;###autoload
(defun qq-group-open (group-id)
  "Open the native group profile for exact native string GROUP-ID."
  (interactive "sQQ group number: ")
  (unless (qq-protocol-group-uin-p group-id)
    (user-error "qq: group profile requires an exact native group id"))
  (let* ((owner (qq-runtime-require-account-id "opening a group profile"))
         (view
          (qq-runtime-open-account-view
           :account-id owner
           :id qq-group--view-id
           :mode 'qq-group-mode
           :buffer-name (qq-group--buffer-name owner group-id)
           :sync-function #'qq-group--sync-invalidations
           :parts '(profile)
           :setup #'qq-group--setup-view))
         (buffer (appkit-view-buffer view)))
    (with-current-buffer buffer
      (qq-group--select-group group-id)
      (when (and (null qq-group--profile)
                 (not qq-group--loading))
        (qq-group-refresh))
      (qq-group--request-sync view)
      (qq-group--sync-now view))
    (pop-to-buffer buffer)
    buffer))

(defun qq-group--handle-media-cache-update (view media-key)
  "Request a targeted VIEW update after MEDIA-KEY changes."
  (when (and (stringp media-key) (qq-group--view-current-p view))
    (with-current-buffer (appkit-view-buffer view)
      (when (equal media-key
                   (format "group-avatar:%s" qq-group--group-id))
        (qq-group--request-sync view :resource media-key)))))

(provide 'qq-group)

;;; qq-group.el ends here
