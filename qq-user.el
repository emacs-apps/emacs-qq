;;; qq-user.el --- Native QQ user profile buffers -*- lexical-binding: t; -*-

;; Author: 0WD0 <me@0wd0.com>

;;; Commentary:

;; Dedicated user profile view backed by the native Gateway profile method,
;; with a Telega-style summary card and asynchronously filled details.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-bindings)
(require 'appkit-projection)
(require 'appkit-transaction)
(require 'qq-core)
(require 'qq-media)
(require 'qq-request)
(require 'qq-runtime)
(require 'qq-state)
(require 'qq-protocol)
(require 'appkit-ui)
(require 'appkit-presentation)
(require 'appkit-position)

(declare-function qq-chat-open "qq-chat" (session-key))

(defconst qq-user--view-id 'user-profile
  "Stable Appkit identity of the singleton user-profile view.")

(defface qq-user-action-button
  '((t :inherit mode-line-inactive :weight semi-bold
     :box (:line-width -1 :style released-button)))
  "Face used for action buttons on QQ user cards."
  :group 'qq)

(defface qq-user-card-title
  '((t :inherit bold :height 1.15))
  "Face used for the primary title on QQ user cards."
  :group 'qq)
(defvar-local qq-user--user-id nil
  "QQ number displayed by the current user buffer.")

(defun qq-user--buffer-name (account-id user-id)
  "Return ACCOUNT-ID-qualified profile buffer name for USER-ID."
  (qq-runtime-account-buffer-name "user" user-id account-id))

(defun qq-user--profile-key (&optional user-id)
  "Return the stable presentation key for USER-ID's profile.

USER-ID defaults to the opaque identity selected in the current buffer."
  (list 'user-profile (or user-id qq-user--user-id)))

(defun qq-user--present-string (value)
  "Return non-empty string VALUE, or nil."
  (and (stringp value) (not (string-empty-p value)) value))

(defun qq-user--display-name ()
  "Return the best title for the current profile."
  (or (qq-user--present-string (alist-get 'remark (qq-user--data 'profile)))
      (qq-user--present-string (alist-get 'nickname (qq-user--data 'profile)))
      qq-user--user-id
      "QQ user"))

(defun qq-user--avatar-display-string ()
  "Return the current user's avatar."
  (if-let* ((url
             (qq-user--present-string
              (alist-get 'avatar_url (qq-user--data 'profile)))))
      (appkit-chat-avatar-one-line-string
       (qq-media-url-preview-image
        (format "avatar:%s" qq-user--user-id)
        url qq-media-avatar-image-height)
       "@")
    (qq-media-avatar-display-string qq-user--user-id)))

(defun qq-user--self-p ()
  "Return non-nil when the current profile belongs to this account."
  (and qq-user--user-id
       (equal qq-user--user-id (qq-state-self-user-id (qq-runtime-state)))))

(defun qq-user--like-limit-reached-p ()
  "Return non-nil after today's confirmed account-level like limit."
  (and (equal (alist-get 'kind (qq-user--data 'profile-like)) "daily_limit")
       (equal (qq-user--data 'profile-like :date) (format-time-string "%Y-%m-%d"))))

(defun qq-user--header-line ()
  "Return dynamic header line for the current user buffer."
  (format " QQ User · %s (%s)%s"
          (qq-user--display-name)
          (or qq-user--user-id "unknown")
          (if (qq-user--data 'profile :pending) " · loading" "")))

(defun qq-user--gender-label (gender)
  "Return display label for native GENDER string."
  (pcase gender
    ("male" "男")
    ("female" "女")
    ("private" "保密")
    ("unknown" "未知")
    (_ nil)))

(defun qq-user--birthday-label (birthday)
  "Return display label for BIRTHDAY object."
  (when (consp birthday)
    (let ((year (alist-get 'year birthday))
          (month (alist-get 'month birthday))
          (day (alist-get 'day birthday)))
      (when (and (integerp month) (<= 1 month 12)
                 (integerp day) (<= 1 day 31))
        (if (and (integerp year) (> year 0))
            (format "%04d-%02d-%02d" year month day)
          (format "%02d-%02d" month day))))))

(defun qq-user--location-label (location)
  "Return display label for LOCATION object."
  (when (consp location)
    (let ((parts (delq nil
                       (mapcar #'qq-user--present-string
                               (list (alist-get 'country location)
                                     (alist-get 'city location)
                                     (alist-get 'home_city location)
                                     (alist-get 'district location))))))
      (and parts (string-join parts " · ")))))

(defun qq-user--relationship-label (relationship)
  "Return display label for RELATIONSHIP object."
  (when (consp relationship)
    (pcase (alist-get 'kind relationship)
      ("self" "自己")
      ("friend" "好友")
      ("stranger" "陌生人"))))

(defun qq-user--level-label (level)
  "Return display label for native QQ LEVEL object."
  (when (consp level)
    (let* ((value (alist-get 'value level))
           (icons
            (cl-loop for (field label) in '((crowns "皇冠")
                                            (suns "太阳")
                                            (moons "月亮")
                                            (stars "星星"))
                     for count = (alist-get field level)
                     when (and (integerp count) (> count 0))
                     collect (format "%d %s" count label))))
      (when (and (integerp value) (>= value 0))
        (string-join
         (cons (number-to-string value) icons)
         " · ")))))

(defun qq-user--status-label (status)
  "Return display label for native STATUS object."
  (when (consp status)
    (let ((code (alist-get 'code status))
          (extended (alist-get 'extended_code status)))
      (when (and (integerp code) (integerp extended))
        (if (> extended 0)
            (format "在线 · 扩展状态 %d" extended)
          (pcase code
            (10 "在线")
            (20 "离线")
            (30 "离开")
            (40 "隐身")
            (50 "忙碌")
            (60 "Q我吧")
            (70 "请勿打扰")
            (_ (format "状态 %d" code))))))))

(defun qq-user--vip-label (vip)
  "Return display label for native VIP object."
  (when (consp vip)
    (let ((kind (alist-get 'kind vip))
          (level (alist-get 'level vip))
          (annual (alist-get 'annual vip)))
      (when (member kind '("vip" "svip"))
        (concat (upcase kind)
                (if (and (integerp level) (> level 0))
                    (format " %d" level)
                  "")
                (if (eq annual t) " · 年费" ""))))))

(defun qq-user--registration-time-label (timestamp)
  "Return a local date for native registration TIMESTAMP."
  (when (and (integerp timestamp) (> timestamp 0))
    (format-time-string "%Y-%m-%d" (seconds-to-time timestamp))))

(defun qq-user--insert-action-buttons ()
  "Insert the primary Telega-style user action row."
  (insert "  ")
  (appkit-ui-insert-action-button
   " 发消息 " #'qq-user-open-chat
   :face 'qq-user-action-button
   :help-echo "打开私聊 (m)")
  (unless (qq-user--self-p)
    (insert "  ")
    (appkit-ui-insert-action-button
     (cond ((qq-user--data 'profile-like :pending) " 点赞中… ")
           ((qq-user--like-limit-reached-p) " 今日已达上限 ")
           (t " 点赞 "))
     #'qq-user-like
     :face 'qq-user-action-button
     :help-echo (if (qq-user--like-limit-reached-p)
                    "今日对该用户的资料点赞已达上限"
                  "给资料卡点赞 (l)")))
  (insert "  ")
  (appkit-ui-insert-action-button
   " 查看头像 " #'qq-user-open-avatar
   :face 'qq-user-action-button
   :help-echo "查看头像 (a)")
  (insert "  ")
  (appkit-ui-insert-action-button
   " 复制 QQ " #'qq-user-copy-id
   :face 'qq-user-action-button
   :help-echo "复制 QQ 号 (w)")
  (insert "\n"))

(defun qq-user-render ()
  "Render the current user profile buffer."
  (interactive)
  (appkit-position-render-preserving
   (lambda ()
     (let ((inhibit-read-only t)
           (profile (qq-user--data 'profile))
           (likes (qq-user--data 'profile-likes)))
       (erase-buffer)
       (setq-local header-line-format '(:eval (qq-user--header-line)))
       (cond
        ((and (qq-user--data 'profile :pending) (null profile))
         (appkit-presentation-insert-note-line "Loading user profile…"))
        ((and (qq-user--data 'profile :error) (null profile))
         (appkit-presentation-insert-note-line (qq-user--data 'profile :error) :face 'error))
        ((null profile)
         (appkit-presentation-insert-note-line "No user profile loaded."))
        (t
         (let ((avatar-start (point)))
           (insert (qq-user--avatar-display-string))
           (make-text-button
            avatar-start (point)
            'follow-link t
            'keymap appkit-ui-button-map
            'action (lambda (_button) (qq-user-open-avatar))
            'help-echo "查看头像"
            'qq-user-id qq-user--user-id)
           (insert "  "
                   (propertize (qq-user--display-name)
                               'face 'qq-user-card-title)
                   "\n"))
         (insert "\n")
         (qq-user--insert-action-buttons)
         (when (qq-user--data 'profile :pending)
           (appkit-presentation-insert-note-line "正在加载完整用户资料…" :face 'shadow))
         (when (qq-user--data 'profile :error)
           (appkit-presentation-insert-note-line (qq-user--data 'profile :error) :face 'error))
         (appkit-presentation-insert-note-line
          (concat
           "g 刷新 · m 私聊"
           (unless (qq-user--self-p)
             (if (qq-user--like-limit-reached-p)
                 " · l 今日已达上限"
               " · l 点赞"))
           " · a 头像 · w 复制 · q 退出"))
         (insert "\n")
         (appkit-presentation-insert-heading-line "资料" :face 'bold)
         (when-let* ((nickname (qq-user--present-string
                                (alist-get 'nickname profile)))
                     (remark (qq-user--present-string
                              (alist-get 'remark profile)))
                     ((not (equal nickname remark))))
           (appkit-presentation-insert-field "昵称" nickname))
         (appkit-presentation-insert-field "QQ" qq-user--user-id)
         (appkit-presentation-insert-field "QID" (alist-get 'qid profile))
         (appkit-presentation-insert-field
          "关系" (qq-user--relationship-label
                  (alist-get 'relationship profile)))
         (when-let* ((relationship (alist-get 'relationship profile))
                     (name (qq-user--present-string
                            (alist-get 'category_name relationship))))
           (appkit-presentation-insert-field "分组" name))
         (appkit-presentation-insert-field
          "性别" (qq-user--gender-label
                  (alist-get 'gender profile)))
         (let ((age (alist-get 'age profile)))
           (when (and (integerp age) (> age 0))
             (appkit-presentation-insert-field "年龄" age)))
         (appkit-presentation-insert-field
          "生日" (qq-user--birthday-label (alist-get 'birthday profile)))
         (appkit-presentation-insert-field
          "地区" (qq-user--location-label (alist-get 'location profile)))
         (appkit-presentation-insert-field "学校" (alist-get 'school profile))
         (appkit-presentation-insert-field
          "等级" (qq-user--level-label (alist-get 'level profile)))
         (appkit-presentation-insert-field
          "会员" (qq-user--vip-label (alist-get 'vip profile)))
         (appkit-presentation-insert-field
          "注册日期"
          (qq-user--registration-time-label
           (alist-get 'registration_time profile)))
         (appkit-presentation-insert-field
          "状态" (qq-user--status-label (alist-get 'status profile)))
         (cond
          ((and (integerp (alist-get 'total_count likes))
                (>= (alist-get 'total_count likes) 0))
           (appkit-presentation-insert-field "获赞" (alist-get 'total_count likes)))
          ((qq-user--data 'profile-likes :pending)
           (appkit-presentation-insert-field "获赞" "加载中…" 'shadow))
          ((qq-user--data 'profile-likes :error)
           (appkit-presentation-insert-field
            "获赞"
            (propertize "获取失败" 'help-echo (qq-user--data 'profile-likes :error))
            'error)))
         (when-let* ((labels (alist-get 'labels profile))
                     ((listp labels))
                     ((not (null labels))))
           (appkit-presentation-insert-field "标签" (string-join labels " · ")))
         (when-let* ((signature (qq-user--present-string
                                 (alist-get 'personal_sign profile))))
           (insert "\n")
           (appkit-presentation-insert-heading-line "个性签名" :face 'bold)
           (insert signature "\n"))
         (insert "\n")))
       (add-text-properties
        (point-min) (point-max)
        (list 'qq-user-profile-key (qq-user--profile-key)
              'rear-nonsticky '(qq-user-profile-key)))
       (goto-char (point-min))))
   :anchor-property 'qq-user-profile-key
   :preserve-window-start t))

(defun qq-user--view-current-p (view)
  "Return non-nil when VIEW still owns this user-profile buffer."
  (and (appkit-surface-live-p view)
       (equal (appkit-surface-identity view) qq-user--view-id)
       (with-current-buffer (appkit-surface-buffer view)
         (and (derived-mode-p 'qq-user-mode)
              (eq view (appkit-current-surface))))))

(defun qq-user--render (surface _model change)
  "Render profile or inbox CHANGE owned by SURFACE."
  (when (and (qq-user--view-current-p surface)
             (or (appkit-projection-change-full-p change)
                 (appkit-projection-change-keys change)
                 (appkit-projection-change-resources change)
                 (appkit-projection-change-geometry-p change)))
    (appkit-with-content-update surface
      (qq-user-render)))
  nil)

(defun qq-user-refresh ()
  "Refresh account-owned data for the current user."
  (interactive)
  (unless qq-user--user-id (user-error "qq: this buffer has no user identity"))
  (qq-user--ensure-view)
  (dolist (kind '(profile profile-likes))
    (qq-runtime-request (qq-runtime-request-create kind :user-id qq-user--user-id) t)))

(defun qq-user-open-chat ()
  "Open a private chat with the current profile user."
  (interactive)
  (unless qq-user--user-id
    (user-error "qq: this buffer has no user identity"))
  (qq-chat-open (qq-state-session-key 'private qq-user--user-id)))

(defun qq-user-like ()
  "Add one QQ profile like through the account App."
  (interactive)
  (unless qq-user--user-id (user-error "qq: this buffer has no user identity"))
  (when (qq-user--self-p) (user-error "qq: cannot like your own profile"))
  (when (qq-user--like-limit-reached-p)
    (user-error "qq: 今日对该用户的资料点赞已达上限"))
  (when (qq-user--data 'profile-like :pending)
    (user-error "qq: profile like is already in progress"))
  (qq-user--ensure-view)
  (qq-runtime-request (qq-runtime-request-create 'profile-like :user-id qq-user--user-id)))

(defun qq-user-open-avatar ()
  "Open the current profile user's avatar."
  (interactive)
  (if-let* ((url
             (qq-user--present-string
              (alist-get 'avatar_url (qq-user--data 'profile)))))
      (qq-media-open-image-url (format "avatar:%s" qq-user--user-id) url)
    (qq-media-open-user-avatar qq-user--user-id)))

(defun qq-user-copy-id ()
  "Copy the current profile user's QQ number."
  (interactive)
  (unless qq-user--user-id
    (user-error "qq: this buffer has no user identity"))
  (kill-new qq-user--user-id)
  (message "qq: copied user id %s" qq-user--user-id))

(defun qq-user--setup-view (view)
  "Subscribe user VIEW to media presentation changes."
  (let ((hook (apply-partially #'qq-user--handle-media-cache-update view)))
    (appkit-register-handle view 'hook
                            (list 'qq-media-cache-update-hook hook nil nil))
    (add-hook 'qq-media-cache-update-hook hook)))

(defun qq-user--ensure-view ()
  "Return the live Appkit view owning the current user buffer."
  (unless qq-user--user-id
    (error "QQ: cannot attach a user view without a user identity"))
  (qq-runtime-ensure-account-surface
   :id qq-user--view-id
   :mode 'qq-user-mode
   :render-function #'qq-user--render
   :setup #'qq-user--setup-view))

(defun qq-user--select-user (user-id)
  "Select USER-ID's account-owned profile in this Surface."
  (setq qq-user--user-id user-id))

(defvar qq-user-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "m") #'qq-user-open-chat)
    (define-key map (kbd "l") #'qq-user-like)
    (define-key map (kbd "a") #'qq-user-open-avatar)
    (define-key map (kbd "w") #'qq-user-copy-id)
    map)
  "Keymap for `qq-user-mode'.")

(appkit-bindings-define 'qq-user-mode-map 'read-only
                        'activate #'push-button
                        'refresh #'qq-user-refresh
                        'next-item #'forward-button
                        'previous-item #'backward-button
                        'next-button #'forward-button
                        'previous-button #'backward-button
                        'quit #'quit-window
                        'help #'describe-mode)

(define-derived-mode qq-user-mode special-mode "QQ-User"
  "Major mode for one native QQ user profile."
  (setq-local truncate-lines nil)
  (setq-local switch-to-buffer-preserve-window-point nil)
  (setq-local header-line-format '(:eval (qq-user--header-line)))
  )

;;;###autoload
(defun qq-user-open (user-id)
  "Open the native user profile for decimal string USER-ID."
  (interactive "sQQ number: ")
  (unless (qq-protocol-user-uin-p user-id)
    (user-error "qq: user profile requires a decimal string user id"))
  (let* ((owner (qq-runtime-require-account-id "opening a user profile"))
         (view
          (qq-runtime-open-account-surface
           :account-id owner
           :id qq-user--view-id
           :mode 'qq-user-mode
           :buffer-name (qq-user--buffer-name owner user-id)
           :render-function #'qq-user--render
           :setup #'qq-user--setup-view))
         (buffer (appkit-surface-buffer view)))
    (with-current-buffer buffer
      (qq-user--select-user user-id)
      (when (and (null (qq-user--data 'profile))
                 (not (qq-user--data 'profile :pending)))
        (qq-user-refresh))
      (qq-runtime-invalidate view :full-p t))
    (pop-to-buffer buffer)
    buffer))

(defun qq-user--handle-media-cache-update (view media-key)
  "Request a targeted VIEW update after MEDIA-KEY changes."
  (when (and (stringp media-key) (qq-user--view-current-p view))
    (with-current-buffer (appkit-surface-buffer view)
      (when (equal media-key (format "avatar:%s" qq-user--user-id))
        (qq-runtime-invalidate view :keys (list (qq-user--profile-key)) :resources (list media-key))))))

(defun qq-user--data (kind &optional field)
  "Read FIELD of the current user's account operation KIND."
  (when qq-user--user-id
    (plist-get (qq-runtime-operation (list kind qq-user--user-id))
               (or field :value))))

(provide 'qq-user)

;;; qq-user.el ends here
