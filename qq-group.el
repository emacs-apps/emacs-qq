;;; qq-group.el --- Native QQ group profile buffer -*- lexical-binding: t; -*-

;; Author: 0WD0 <me@0wd0.com>

;;; Commentary:

;; Telega-style group profile page backed by the native Gateway.  Unknown
;; native enum values remain visible as numeric codes.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-bindings)
(require 'appkit-projection)
(require 'appkit-transaction)
(require 'qq-core)
(require 'qq-media)
(require 'qq-runtime)
(require 'qq-state)
(require 'qq-protocol)

(autoload 'qq-chat-toggle-pin "qq-chat" nil t)
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
  (or (qq-group--present-string (alist-get 'remark (qq-core--group-profile-from-state qq-group--group-id)))
      (qq-group--present-string (alist-get 'name (qq-core--group-profile-from-state qq-group--group-id)))
      qq-group--group-id
      "QQ group"))

(defun qq-group--header-line ()
  "Return the dynamic header line for the group profile."
  (format " QQ Group · %s (%s)%s"
          (qq-group--display-name)
          (or qq-group--group-id "unknown")
          (if (plist-get (qq-runtime-operation '(groups)) :pending) " · loading" "")))

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
  (let ((owner-id (alist-get 'owner_id (qq-core--group-profile-from-state qq-group--group-id))))
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

(defun qq-group-set-name (name)
  "Set the current group's public NAME through the selected QQ account."
  (interactive
   (list (read-string "新群名称: "
                      (alist-get 'name (qq-core--group-profile-from-state qq-group--group-id)))))
  (unless qq-group--group-id
    (user-error "qq: this buffer has no group identity"))
  (unless (and (stringp name) (not (string-empty-p name)))
    (user-error "qq: group name must be a non-empty string"))
  (qq-runtime-request (list 'group-setting qq-group--group-id 'name name)))

(defun qq-group-set-remark (remark)
  "Set or clear the current group's account-local REMARK."
  (interactive
   (list (read-string "群备注（留空清除）: "
                      (or (alist-get 'remark (qq-core--group-profile-from-state qq-group--group-id)) ""))))
  (unless qq-group--group-id
    (user-error "qq: this buffer has no group identity"))
  (unless (stringp remark)
    (user-error "qq: group remark must be a string"))
  (qq-runtime-request (list 'group-setting qq-group--group-id 'remark remark)))

(defun qq-group-set-whole-mute (enabled)
  "Set the current group's whole-group mute state to ENABLED."
  (interactive (list (y-or-n-p "开启全员禁言？")))
  (unless qq-group--group-id
    (user-error "qq: this buffer has no group identity"))
  (setq enabled (and enabled t))
  (qq-runtime-request (list 'group-setting qq-group--group-id 'whole-mute enabled)))

(defun qq-group-toggle-pinned ()
  "Toggle the current group's conversation pinned state."
  (interactive)
  (qq-chat-toggle-pin (qq-state-session-key 'group qq-group--group-id)))

(defun qq-group-clock-in ()
  "Clock the selected account into the current group."
  (interactive)
  (unless qq-group--group-id
    (user-error "qq: this buffer has no group identity"))
  (qq-runtime-request (list 'group-clock-in qq-group--group-id) t 'group))

(defun qq-group-show-at-all-remaining ()
  "Show live @all availability and quotas for the current group."
  (interactive)
  (unless qq-group--group-id
    (user-error "qq: this buffer has no group identity"))
  (qq-runtime-request (list 'group-at-all qq-group--group-id) t 'group))

(defun qq-group-leave ()
  "Confirm and leave the current group through the selected QQ account."
  (interactive)
  (unless qq-group--group-id
    (user-error "qq: this buffer has no group identity"))
  (let* ((group-id qq-group--group-id)
         (group-name
          (or (qq-group--present-string (alist-get 'name (qq-core--group-profile-from-state qq-group--group-id)))
              (qq-group--present-string (alist-get 'remark (qq-core--group-profile-from-state qq-group--group-id)))
              "未命名群聊")))
    (unless (yes-or-no-p
             (format "确认退出群聊 %s（%s）？" group-name group-id))
      (user-error "qq: 已取消退出群聊"))
    (qq-runtime-request (list 'group-leave group-id) t 'group)))

(defun qq-group--insert-action-buttons ()
  "Insert primary group action buttons."
  (insert "  ")
  (appkit-ui-insert-action-button
   " 打开群聊 " #'qq-group-open-chat
   :face 'qq-group-action-button
   :help-echo "打开群聊 (m)")
  (insert "  ")
  (appkit-ui-insert-action-button
   " 查看头像 " #'qq-group-open-avatar
   :face 'qq-group-action-button
   :help-echo "查看群头像 (a)")
  (insert "  ")
  (appkit-ui-insert-action-button
   " 搜索成员 " #'qq-group-search-members
   :face 'qq-group-action-button
   :help-echo "原生搜索群成员 (s)")
  (when (qq-protocol-user-uin-p (alist-get 'owner_id (qq-core--group-profile-from-state qq-group--group-id)))
    (insert "  ")
    (appkit-ui-insert-action-button
     " 群主资料 " #'qq-group-open-owner
     :face 'qq-group-action-button
     :help-echo "打开群主资料 (o)"))
  (insert "  ")
  (appkit-ui-insert-action-button
   " 复制群号 " #'qq-group-copy-id
   :face 'qq-group-action-button
   :help-echo "复制群号 (w)")
  (insert "\n  ")
  (appkit-ui-insert-action-button
   " 修改群名 " (lambda () (call-interactively #'qq-group-set-name))
   :face 'qq-group-action-button
   :help-echo "修改群名称 (N)")
  (insert "  ")
  (appkit-ui-insert-action-button
   " 修改备注 " (lambda () (call-interactively #'qq-group-set-remark))
   :face 'qq-group-action-button
   :help-echo "修改群备注 (R)")
  (insert "  ")
  (appkit-ui-insert-action-button
   (if (eq (alist-get 'pinned (qq-core--group-profile-from-state qq-group--group-id)) t)
       " 取消置顶 "
     " 置顶群聊 ")
   #'qq-group-toggle-pinned
   :face 'qq-group-action-button
   :help-echo "切换群会话置顶状态 (P)")
  (insert "  ")
  (appkit-ui-insert-action-button
   " 全员禁言 " (lambda () (call-interactively #'qq-group-set-whole-mute))
   :face 'qq-group-action-button
   :help-echo "开启或关闭全员禁言 (M)")
  (insert "  ")
  (appkit-ui-insert-action-button
   " 群打卡 " #'qq-group-clock-in
   :face 'qq-group-action-button
   :help-echo "群打卡 (S)")
  (insert "  ")
  (appkit-ui-insert-action-button
   " @全体额度 " #'qq-group-show-at-all-remaining
   :face 'qq-group-action-button
   :help-echo "查询 @全体剩余额度 (@)")
  (insert "  ")
  (appkit-ui-insert-action-button
   " 退出群聊 " #'qq-group-leave
   :face 'qq-group-destructive-action-button
   :help-echo "退出群聊 (L)")
  (insert "\n"))

(defun qq-group-render ()
  "Render the current group profile buffer."
  (interactive)
  (appkit-position-render-preserving
   (lambda ()
     (let ((inhibit-read-only t)
           (profile (qq-core--group-profile-from-state qq-group--group-id))
           (request (qq-runtime-operation '(groups))))
       (erase-buffer)
       (setq-local header-line-format '(:eval (qq-group--header-line)))
       (cond
        ((plist-get request :pending)
         (appkit-presentation-insert-note-line "Loading group profile…"))
        ((plist-get request :error)
         (appkit-presentation-insert-note-line (plist-get request :error) :face 'error))
        ((null profile)
         (appkit-presentation-insert-note-line "No group profile loaded."))
        (t
         (let ((avatar-start (point)))
           (insert (qq-media-group-avatar-display-string qq-group--group-id))
           (make-text-button
            avatar-start (point)
            'follow-link t
            'keymap appkit-ui-button-map
            'action (lambda (_button) (qq-group-open-avatar))
            'help-echo "查看群头像"
            'qq-group-id qq-group--group-id)
           (insert "  "
                   (propertize (qq-group--display-name)
                               'face 'qq-group-card-title)
                   "\n   "
                   (propertize
                    (format "%d / %d 位成员"
                            (alist-get 'member_count profile)
                            (alist-get 'max_member_count profile))
                    'face 'shadow)
                   "\n"))
         (insert "\n")
         (qq-group--insert-action-buttons)
         (appkit-presentation-insert-note-line
          "g 刷新 · N 群名 · R 备注 · P 置顶 · M 全员禁言 · S 群打卡 · @ 全体额度 · L 退出群聊 · s 成员（结果页 C 名片 / T 头衔 / K 移出） · q 关闭")
         (insert "\n")
         (appkit-presentation-insert-heading-line "资料" :face 'bold)
         (let ((name (qq-group--present-string
                      (alist-get 'name profile)))
               (remark (qq-group--present-string
                        (alist-get 'remark profile))))
           (when (and name remark (not (equal name remark)))
             (appkit-presentation-insert-field "群名称" name)))
         (appkit-presentation-insert-field "群号" qq-group--group-id)
         (appkit-presentation-insert-field "群主" (alist-get 'owner_id profile))
         (appkit-presentation-insert-field
          "成员"
          (format "%d / %d"
                  (alist-get 'member_count profile)
                  (alist-get 'max_member_count profile)))
         (appkit-presentation-insert-field
          "活跃成员" (alist-get 'active_member_count profile))
         (appkit-presentation-insert-field
          "创建时间" (qq-group--timestamp-label
                      (alist-get 'created_at profile)))
         (appkit-presentation-insert-field
          "入群时间" (qq-group--timestamp-label
                      (alist-get 'joined_at profile)))
         (appkit-presentation-insert-field
          "置顶" (qq-group--boolean-label (alist-get 'pinned profile)))
         (appkit-presentation-insert-field
          "群分类" (qq-group--present-string
                    (alist-get 'name (alist-get 'category profile))))
         (appkit-presentation-insert-field "群等级" (alist-get 'grade profile))
         (appkit-presentation-insert-field
          "我的权限" (qq-group--permission-label
                      (alist-get 'self_permission profile)))
         (when-let* ((mute (alist-get 'mute profile)))
           (appkit-presentation-insert-field
            "全员禁言"
            (if (let ((until (alist-get 'all_until mute)))
                  (and (integerp until) (> until 0)))
                "是"
              "否"))
           (appkit-presentation-insert-field
            "全员禁言至" (qq-group--timestamp-label (alist-get 'all_until mute)))
           (appkit-presentation-insert-field
            "我的禁言至" (qq-group--timestamp-label (alist-get 'self_until mute))))
         (when-let* ((join (alist-get 'join profile)))
           (appkit-presentation-insert-field
            "入群模式" (qq-group--join-mode-label (alist-get 'mode join)))
           (appkit-presentation-insert-field "入群问题" (alist-get 'question join))
           (appkit-presentation-insert-field "我的答案" (alist-get 'answer join)))
         (appkit-presentation-insert-field
          "认证" (qq-group--present-string
                  (alist-get 'text (alist-get 'certification profile))))
         (when-let* ((school (alist-get 'school profile)))
           (appkit-presentation-insert-field
            "学校"
            (string-join
             (delq nil
                   (list (qq-group--present-string (alist-get 'name school))
                         (qq-group--present-string (alist-get 'location school))))
             " · ")))
         (when-let* ((location (alist-get 'location profile)))
           (appkit-presentation-insert-field "地点" (alist-get 'text location)))
         (dolist (section '(("群简介" . description)
                            ("群公告" . announcement)))
           (when-let* ((text (qq-group--present-string
                              (alist-get (cdr section) profile))))
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
  (and (appkit-surface-live-p view)
       (equal (appkit-surface-identity view) qq-group--view-id)
       (with-current-buffer (appkit-surface-buffer view)
         (and (derived-mode-p 'qq-group-mode)
              (eq view (appkit-current-surface))))))

(defun qq-group--render (surface _model change)
  "Render profile or inbox CHANGE owned by SURFACE."
  (when (and (qq-group--view-current-p surface)
             (or (appkit-projection-change-full-p change)
                 (appkit-projection-change-keys change)
                 (appkit-projection-change-resources change)
                 (appkit-projection-change-geometry-p change)))
    (appkit-with-content-update surface
      (qq-group-render)))
  nil)

(defun qq-group-refresh ()
  "Refresh the account's shared group directory."
  (interactive)
  (unless qq-group--group-id (user-error "qq: this buffer has no group identity"))
  (qq-group--ensure-view)
  (qq-runtime-request '(groups) t))

(defun qq-group--setup-view (view)
  "Register the media redraw hook for VIEW."
  (let ((hook (apply-partially #'qq-group--handle-media-cache-update view)))
    (appkit-register-handle view 'hook
                            (list 'qq-media-cache-update-hook hook nil (appkit-surface-buffer view)))
    (add-hook 'qq-media-cache-update-hook hook)))

(defun qq-group--surface-update (context model message)
  "Present this profile's operation replies and shared account updates."
  (pcase message
    (`(appkit-operation-completed group ,request ,receipt ,reason)
     (when (equal (cadr request) qq-group--group-id)
       (if reason (message "qq: %s" reason)
         (pcase (car request)
           ('group-clock-in
            (message "qq: 群打卡成功%s"
                     (let ((details (delq nil (mapcar #'qq-group--present-string
                                                      (list (alist-get 'title receipt)
                                                            (alist-get 'keep_day_text receipt)
                                                            (alist-get 'group_rank_text receipt))))))
                       (if details (concat "：" (string-join details " · ")) ""))))
           ('group-at-all
            (message "qq: %s使用 @全体；个人剩余 %d 次，群剩余 %d 次"
                     (if (eq (alist-get 'can_at_all receipt) t) "可以" "当前不可")
                     (alist-get 'remain_at_all_count_for_uin receipt)
                     (alist-get 'remain_at_all_count_for_group receipt)))
           ('group-leave
            (message "qq: 已退出群聊 %s" qq-group--group-id)
            ;; Killing the Surface's buffer is host work outside this update.
            (let ((buffer (current-buffer)))
              (run-at-time 0 nil (lambda ()
                                   (let ((kill-buffer-query-functions nil))
                                     (kill-buffer buffer)))))))))
     (appkit-next :model model :render appkit-render-none))
    (_ (qq-runtime--surface-update context model message))))

(defun qq-group--ensure-view ()
  "Return the live Appkit view owning the current group buffer."
  (unless qq-group--group-id
    (error "QQ: cannot attach a group view without a group identity"))
  (qq-runtime-ensure-account-surface
   :id qq-group--view-id
   :mode 'qq-group-mode
   :update #'qq-group--surface-update
   :render-function #'qq-group--render
   :setup #'qq-group--setup-view))

(defun qq-group--select-group (group-id)
  "Select GROUP-ID without copying its account data into the Surface."
  (setq qq-group--group-id group-id))

(defvar qq-group-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "m") #'qq-group-open-chat)
    (define-key map (kbd "a") #'qq-group-open-avatar)
    (define-key map (kbd "o") #'qq-group-open-owner)
    (define-key map (kbd "w") #'qq-group-copy-id)
    (define-key map (kbd "N") #'qq-group-set-name)
    (define-key map (kbd "R") #'qq-group-set-remark)
    (define-key map (kbd "P") #'qq-group-toggle-pinned)
    (define-key map (kbd "M") #'qq-group-set-whole-mute)
    (define-key map (kbd "S") #'qq-group-clock-in)
    (define-key map (kbd "@") #'qq-group-show-at-all-remaining)
    (define-key map (kbd "L") #'qq-group-leave)
    map)
  "Keymap for `qq-group-mode'.")

(appkit-bindings-define 'qq-group-mode-map 'read-only
  'activate #'push-button
  'refresh #'qq-group-refresh
  'search #'qq-group-search-members
  'next-item #'forward-button
  'previous-item #'backward-button
  'next-button #'forward-button
  'previous-button #'backward-button
  'quit #'quit-window
  'help #'describe-mode)

(define-derived-mode qq-group-mode special-mode "QQ-Group"
  "Major mode for one native QQ group profile."
  (setq-local truncate-lines nil)
  (setq-local switch-to-buffer-preserve-window-point nil)
  (setq-local header-line-format '(:eval (qq-group--header-line))))

;;;###autoload
(defun qq-group-open (group-id)
  "Open the native group profile for exact native string GROUP-ID."
  (interactive "sQQ group number: ")
  (unless (qq-protocol-group-uin-p group-id)
    (user-error "qq: group profile requires an exact native group id"))
  (let* ((owner (qq-runtime-require-account-id "opening a group profile"))
         (view
          (qq-runtime-open-account-surface
           :account-id owner
           :id qq-group--view-id
           :mode 'qq-group-mode
           :update #'qq-group--surface-update
           :buffer-name (qq-group--buffer-name owner group-id)
           :render-function #'qq-group--render
           :setup #'qq-group--setup-view))
         (buffer (appkit-surface-buffer view)))
    (with-current-buffer buffer
      (qq-group--select-group group-id)
      (when (and (null (qq-core--group-profile-from-state qq-group--group-id))
                 (not (plist-get (qq-runtime-operation '(groups)) :pending)))
        (qq-group-refresh))
      (qq-runtime-invalidate view :full-p t))
    (pop-to-buffer buffer)
    buffer))

(defun qq-group--handle-media-cache-update (view media-key)
  "Request a targeted VIEW update after MEDIA-KEY changes."
  (when (and (stringp media-key) (qq-group--view-current-p view))
    (with-current-buffer (appkit-surface-buffer view)
      (when (equal media-key
                   (format "group-avatar:%s" qq-group--group-id))
        (qq-runtime-invalidate view :keys (list (qq-group--profile-key)) :resources (list media-key))))))

(provide 'qq-group)

;;; qq-group.el ends here
