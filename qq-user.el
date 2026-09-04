;;; qq-user.el --- Native QQ user profile buffers -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; Dedicated user profile view backed by the native Gateway profile method,
;; with a Telega-style summary card and asynchronously filled details.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-core)
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

(defvar-local qq-user--profile nil
  "Native user profile displayed by the current user buffer.")

(defvar-local qq-user--request nil
  "Active profile request token for the current user buffer.")

(defvar-local qq-user--request-owner nil
  "Owner object for the current profile request.")

(defvar-local qq-user--loading nil
  "Non-nil while the current user profile is loading.")

(defvar-local qq-user--error nil
  "Last profile loading error string, or nil.")

(defvar-local qq-user--like-count nil
  "Verified received profile-like count, or nil when unavailable.")

(defvar-local qq-user--like-loading nil
  "Non-nil while the received profile-like count is loading.")

(defvar-local qq-user--like-error nil
  "Last profile-like loading error string, or nil.")

(defvar-local qq-user--like-request nil
  "Active profile-like request token for the current user buffer.")

(defvar-local qq-user--like-request-owner nil
  "Owner object for the active profile-like request.")

(defvar-local qq-user--send-like-request nil
  "Active request token for adding a profile like.")

(defvar-local qq-user--send-like-request-owner nil
  "Owner object for the active profile-like mutation.")

(defvar-local qq-user--like-limit-date nil
  "Local date on which QQ reported the current target's daily like limit.")

(defvar-local qq-user--media-hook-function nil
  "View-owned media cache hook installed for this user buffer.")

(defun qq-user--cancel-operation (request)
  "Cancel active REQUEST when present."
  (when request
    (qq-request-cancel request)))

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
  (or (qq-user--present-string (alist-get 'remark qq-user--profile))
      (qq-user--present-string (alist-get 'nickname qq-user--profile))
      qq-user--user-id
      "QQ user"))

(defun qq-user--avatar-display-string ()
  "Return the current user's avatar."
  (if-let* ((url
             (qq-user--present-string
              (alist-get 'avatar_url qq-user--profile))))
      (qq-media-url-preview-display-string
       (format "avatar:%s" qq-user--user-id)
       url "@" qq-media-avatar-image-height)
    (qq-media-avatar-display-string qq-user--user-id)))

(defun qq-user--self-p ()
  "Return non-nil when the current profile belongs to this account."
  (and qq-user--user-id
       (equal qq-user--user-id (qq-state-self-user-id))))

(defun qq-user--like-limit-reached-p ()
  "Return non-nil when QQ reported today's limit for the current profile."
  (equal qq-user--like-limit-date (format-time-string "%Y-%m-%d")))

(defun qq-user--header-line ()
  "Return dynamic header line for the current user buffer."
  (format " QQ User · %s (%s)%s"
          (qq-user--display-name)
          (or qq-user--user-id "unknown")
          (if qq-user--loading " · loading" "")))

(defun qq-user--insert-field (label value &optional face)
  "Insert profile LABEL and VALUE when VALUE is present."
  (when (and value (not (equal value "")))
    (let ((start (point)))
      (insert (format "%-12s" (concat label ":")))
      (add-text-properties start (point) '(face bold)))
    (let ((start (point)))
      (insert (format "%s" value) "\n")
      (when face
        (add-text-properties start (point) (list 'face face))))))

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
   :face 'qq-user-action-button :help-echo "打开私聊 (m)")
  (unless (qq-user--self-p)
    (insert "  ")
    (appkit-ui-insert-action-button
     (cond (qq-user--send-like-request-owner " 点赞中… ")
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
   :face 'qq-user-action-button :help-echo "查看头像 (a)")
  (insert "  ")
  (appkit-ui-insert-action-button
   " 复制 QQ " #'qq-user-copy-id
   :face 'qq-user-action-button :help-echo "复制 QQ 号 (w)")
  (insert "\n"))

(defun qq-user-render ()
  "Render the current user profile buffer."
  (interactive)
  (appkit-position-render-preserving
   (lambda ()
     (let ((inhibit-read-only t))
       (erase-buffer)
       (setq-local header-line-format '(:eval (qq-user--header-line)))
       (cond
        ((and qq-user--loading (null qq-user--profile))
         (appkit-presentation-insert-note-line "Loading user profile…"))
        ((and qq-user--error (null qq-user--profile))
         (appkit-presentation-insert-note-line qq-user--error :face 'error))
        ((null qq-user--profile)
         (appkit-presentation-insert-note-line "No user profile loaded."))
        (t
         (let ((avatar-start (point)))
           (insert (qq-user--avatar-display-string))
           (make-text-button
            avatar-start (point)
            'follow-link t
            'action (lambda (_button) (qq-user-open-avatar))
            'help-echo "查看头像"
            'qq-user-id qq-user--user-id)
           (insert "  "
                   (propertize (qq-user--display-name)
                               'face 'qq-user-card-title)
                   "\n"))
         (insert "\n")
         (qq-user--insert-action-buttons)
         (when qq-user--loading
           (appkit-presentation-insert-note-line "正在加载完整用户资料…" :face 'shadow))
         (when qq-user--error
           (appkit-presentation-insert-note-line qq-user--error :face 'error))
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
                                (alist-get 'nickname qq-user--profile)))
                     (remark (qq-user--present-string
                              (alist-get 'remark qq-user--profile)))
                     ((not (equal nickname remark))))
           (qq-user--insert-field "昵称" nickname))
         (qq-user--insert-field "QQ" qq-user--user-id)
         (qq-user--insert-field "QID" (alist-get 'qid qq-user--profile))
         (qq-user--insert-field
          "关系" (qq-user--relationship-label
                  (alist-get 'relationship qq-user--profile)))
         (when-let* ((relationship (alist-get 'relationship qq-user--profile))
                     (name (qq-user--present-string
                            (alist-get 'category_name relationship))))
           (qq-user--insert-field "分组" name))
         (qq-user--insert-field
          "性别" (qq-user--gender-label
                  (alist-get 'gender qq-user--profile)))
         (let ((age (alist-get 'age qq-user--profile)))
           (when (and (integerp age) (> age 0))
             (qq-user--insert-field "年龄" age)))
         (qq-user--insert-field
          "生日" (qq-user--birthday-label (alist-get 'birthday qq-user--profile)))
         (qq-user--insert-field
          "地区" (qq-user--location-label (alist-get 'location qq-user--profile)))
         (qq-user--insert-field "学校" (alist-get 'school qq-user--profile))
         (qq-user--insert-field
          "等级" (qq-user--level-label (alist-get 'level qq-user--profile)))
         (qq-user--insert-field
          "会员" (qq-user--vip-label (alist-get 'vip qq-user--profile)))
         (qq-user--insert-field
          "注册日期"
          (qq-user--registration-time-label
           (alist-get 'registration_time qq-user--profile)))
         (qq-user--insert-field
          "状态" (qq-user--status-label (alist-get 'status qq-user--profile)))
         (cond
          ((and (integerp qq-user--like-count)
                (>= qq-user--like-count 0))
           (qq-user--insert-field "获赞" qq-user--like-count))
          (qq-user--like-loading
           (qq-user--insert-field "获赞" "加载中…" 'shadow))
          (qq-user--like-error
           (qq-user--insert-field
            "获赞"
            (propertize "获取失败" 'help-echo qq-user--like-error)
            'error)))
         (when-let* ((labels (alist-get 'labels qq-user--profile))
                     ((listp labels))
                     ((not (null labels))))
           (qq-user--insert-field "标签" (string-join labels " · ")))
         (when-let* ((signature (qq-user--present-string
                                 (alist-get 'personal_sign qq-user--profile))))
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

(defun qq-user--live-current-view ()
  "Return the live user-profile view attached to this buffer, or nil."
  (let ((view (appkit-current-surface)))
    (and (qq-user--view-current-p view) view)))

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

(defun qq-user--request-current-p (view buffer user-id owner)
  "Return non-nil when VIEW and OWNER still load USER-ID in BUFFER."
  (and (qq-user--view-current-p view)
       (eq (appkit-surface-buffer view) buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'qq-user-mode)
              (eq view (appkit-current-surface))
              (equal qq-user--user-id user-id)
              (eq qq-user--request-owner owner)))))

(defun qq-user-refresh ()
  "Refresh the current native user profile."
  (interactive)
  (unless qq-user--user-id
    (user-error "qq: this buffer has no user identity"))
  (let ((view (qq-user--ensure-view)))
    (when qq-user--request
      (qq-user--cancel-operation qq-user--request))
    (let ((buffer (current-buffer))
          (user-id qq-user--user-id)
          (owner (list 'user-profile qq-user--user-id)))
      (setq qq-user--loading t
            qq-user--error nil
            qq-user--request nil
            qq-user--request-owner owner)
      (appkit-surface-send view (list 'qq-render (appkit-projection-change-create :full-p t)))
      (condition-case error-data
          (let ((request
                  (qq-core-get-user-profile
                   user-id
                   (lambda (profile)
                     (when (qq-user--request-current-p
                            view buffer user-id owner)
                       (with-current-buffer buffer
                         (setq qq-user--profile profile
                               qq-user--loading nil
                               qq-user--error nil
                               qq-user--request nil
                               qq-user--request-owner nil)
                         (appkit-surface-send view (list 'qq-render (appkit-projection-change-create :full-p t))))))
                   (lambda (_response reason)
                     (when (qq-user--request-current-p
                            view buffer user-id owner)
                       (with-current-buffer buffer
                         (setq qq-user--loading nil
                               qq-user--error
                               (format "Unable to load profile: %s"
                                       (or reason "unknown error"))
                               qq-user--request nil
                               qq-user--request-owner nil)
                         (appkit-surface-send view (list 'qq-render (appkit-projection-change-create :full-p t)))
                         (message "qq: %s"
                                  (or reason "native request failed"))))))))
            (when (eq qq-user--request-owner owner)
              (setq qq-user--request request)))
        (error
         (when (qq-user--request-current-p view buffer user-id owner)
           (setq qq-user--loading nil
                 qq-user--error
                 (format "Unable to load profile: %s"
                         (error-message-string error-data))
                 qq-user--request nil
                 qq-user--request-owner nil)
           (appkit-surface-send view (list 'qq-render (appkit-projection-change-create :full-p t)))))))
    (qq-user--refresh-like view)))

(defun qq-user--like-request-current-p (view buffer user-id owner)
  "Return non-nil when VIEW and OWNER load likes for USER-ID in BUFFER."
  (and (qq-user--view-current-p view)
       (eq (appkit-surface-buffer view) buffer)
       (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'qq-user-mode)
              (eq view (appkit-current-surface))
              (equal qq-user--user-id user-id)
              (eq qq-user--like-request-owner owner)))))

(defun qq-user--cancel-like-request ()
  "Cancel and forget the active received-like request."
  (let ((request qq-user--like-request))
    (setq qq-user--like-request nil
          qq-user--like-request-owner nil
          qq-user--like-loading nil)
    (when request
      (qq-user--cancel-operation request))))

(defun qq-user--apply-like-event (event)
  "Apply one owner-checked received-like EVENT to domain state."
  (pcase (plist-get event :type)
    ('success
     (setq qq-user--like-count (plist-get event :count)
           qq-user--like-loading nil
           qq-user--like-error nil
           qq-user--like-request nil
           qq-user--like-request-owner nil))
    ('error
     (setq qq-user--like-count nil
           qq-user--like-loading nil
           qq-user--like-error (plist-get event :error)
           qq-user--like-request nil
           qq-user--like-request-owner nil))
    (type
     (error "QQ: unknown received-like event %S" type))))

(defun qq-user--accept-like-event (view buffer user-id owner event)
  "Settle EVENT in BUFFER for exact VIEW's received-like request generation."
  (when (qq-user--like-request-current-p view buffer user-id owner)
    (with-current-buffer buffer
      (when (qq-user--like-request-current-p view buffer user-id owner)
        (qq-user--apply-like-event event)
        (appkit-surface-send view (list 'qq-render (appkit-projection-change-create :full-p t)))
        t))))

(defun qq-user--refresh-like (&optional view)
  "Refresh received profile-like count using the current user VIEW."
  (setq view (or view (qq-user--ensure-view)))
  (qq-user--cancel-like-request)
  (let ((buffer (current-buffer))
        (user-id qq-user--user-id)
        (owner (list 'user-like qq-user--user-id)))
    (setq qq-user--like-count nil
          qq-user--like-loading t
          qq-user--like-error nil
          qq-user--like-request nil
          qq-user--like-request-owner owner)
    (appkit-surface-send view (list 'qq-render (appkit-projection-change-create :full-p t)))
    (condition-case error-data
        (let ((request
                (qq-core-get-profile-like-summary
                 user-id
                 (lambda (summary)
                   (qq-user--accept-like-event
                    view buffer user-id owner
                    (list :type 'success
                          :count (alist-get 'total_count summary))))
                 (lambda (_response reason)
                   (qq-user--accept-like-event
                    view buffer user-id owner
                    (list :type 'error
                          :error (or reason "unknown error")))))))
          ;; A local transport may settle synchronously before returning.
          (when (eq qq-user--like-request-owner owner)
            (setq qq-user--like-request request)))
      ((error quit)
       (qq-user--accept-like-event
        view buffer user-id owner
        (list :type 'error
              :error (format "dispatch failed: %s"
                             (error-message-string error-data))))
       (when (eq (car error-data) 'quit)
         (setq quit-flag nil)
         (signal (car error-data) (cdr error-data)))))))

(defun qq-user-open-chat ()
  "Open a private chat with the current profile user."
  (interactive)
  (unless qq-user--user-id
    (user-error "qq: this buffer has no user identity"))
  (qq-chat-open (qq-state-session-key 'private qq-user--user-id)))

(defun qq-user--send-like-request-current-p (view buffer user-id owner)
  "Return non-nil when VIEW and OWNER still like USER-ID in BUFFER."
  (and (qq-user--view-current-p view)
       (eq (appkit-surface-buffer view) buffer)
       (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'qq-user-mode)
              (eq view (appkit-current-surface))
              (equal qq-user--user-id user-id)
              (eq qq-user--send-like-request-owner owner)))))

(defun qq-user--accept-send-like-event
    (view buffer user-id owner event)
  "Settle EVENT for the exact profile-like mutation generation.

Return EVENT when accepted, and nil when VIEW, BUFFER, USER-ID, or OWNER is
stale."
  (when (qq-user--send-like-request-current-p
         view buffer user-id owner)
    (with-current-buffer buffer
      (when (qq-user--send-like-request-current-p
             view buffer user-id owner)
        (setq qq-user--send-like-request nil
              qq-user--send-like-request-owner nil)
        (when (equal (plist-get event :outcome) "daily_limit")
          (setq qq-user--like-limit-date (format-time-string "%Y-%m-%d")))
        (appkit-surface-send view (list 'qq-render (appkit-projection-change-create :full-p t)))
        event))))

(defun qq-user-like ()
  "Add one native QQ profile like to the current user."
  (interactive)
  (unless qq-user--user-id
    (user-error "qq: this buffer has no user identity"))
  (when (qq-user--self-p)
    (user-error "qq: cannot like your own profile"))
  (when (qq-user--like-limit-reached-p)
    (user-error "qq: 今日对该用户的资料点赞已达上限"))
  (when qq-user--send-like-request-owner
    (user-error "qq: profile like is already in progress"))
  (let ((view (qq-user--ensure-view)))
    (let ((buffer (current-buffer))
          (user-id qq-user--user-id)
          (owner (list 'send-user-like qq-user--user-id)))
      (setq qq-user--send-like-request nil
            qq-user--send-like-request-owner owner)
      (appkit-surface-send view (list 'qq-render (appkit-projection-change-create :full-p t)))
      (condition-case error-data
          (let ((request
                  (qq-core-send-profile-like
                   user-id
                   (lambda (outcome)
                     (let ((kind (alist-get 'kind outcome)))
                       (when (qq-user--accept-send-like-event
                              view buffer user-id owner
                              (list :type 'success :outcome kind))
                         (when (equal kind "liked")
                           (qq-user--refresh-like view)))))
                   (lambda (_response reason)
                     (when (qq-user--accept-send-like-event
                            view buffer user-id owner
                            (list :type 'error :reason reason))
                       (message "qq: %s"
                                (or reason "native request failed")))))))
            ;; A local transport may settle synchronously before returning.
            (when (eq qq-user--send-like-request-owner owner)
              (setq qq-user--send-like-request request)))
        ((error quit)
         (qq-user--accept-send-like-event
          view buffer user-id owner
          (list :type 'dispatch-error
                :reason (error-message-string error-data)))
         (when (eq (car error-data) 'quit)
           (setq quit-flag nil))
         (signal (car error-data) (cdr error-data)))))))

(defun qq-user-open-avatar ()
  "Open the current profile user's avatar."
  (interactive)
  (if-let* ((url
             (qq-user--present-string
              (alist-get 'avatar_url qq-user--profile))))
      (qq-media-open-image-url (format "avatar:%s" qq-user--user-id) url)
    (qq-media-open-user-avatar qq-user--user-id)))

(defun qq-user-copy-id ()
  "Copy the current profile user's QQ number."
  (interactive)
  (unless qq-user--user-id
    (user-error "qq: this buffer has no user identity"))
  (kill-new qq-user--user-id)
  (message "qq: copied user id %s" qq-user--user-id))

(defun qq-user--cancel-request ()
  "Cancel asynchronous work owned by the current user view."
  (let ((requests
         (delq nil
               (list qq-user--request
                     qq-user--like-request
                     qq-user--send-like-request))))
    (setq qq-user--request nil
          qq-user--request-owner nil
          qq-user--like-request nil
          qq-user--like-request-owner nil
          qq-user--send-like-request nil
          qq-user--send-like-request-owner nil
          qq-user--loading nil
          qq-user--like-loading nil)
    (dolist (request requests)
      (qq-user--cancel-operation request))))

(defun qq-user--clear-view-data ()
  "Clear account-scoped data projected by the current user view."
  (setq qq-user--profile nil
        qq-user--error nil
        qq-user--like-count nil
        qq-user--like-error nil
        qq-user--like-limit-date nil))

(defun qq-user--reset-buffer-work (buffer)
  "Reset requests, data, and media hook state retained by BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (derived-mode-p 'qq-user-mode)
        (when qq-user--media-hook-function
          (remove-hook 'qq-media-cache-update-hook
                       qq-user--media-hook-function))
        (setq qq-user--media-hook-function nil)
        (qq-user--cancel-request)
        (qq-user--clear-view-data)))))

(defun qq-user--release-view-work (view buffer)
  "Release BUFFER work when it is still owned by user-profile VIEW."
  (when (and (buffer-live-p buffer)
             (with-current-buffer buffer
               (eq view qq-runtime--surface-owner)))
    (qq-user--reset-buffer-work buffer)))

(defun qq-user--setup-view (view)
  "Reset replacement state and register lifecycle work for user VIEW."
  (let ((buffer (appkit-surface-buffer view)))
    (qq-user--reset-buffer-work buffer)
    (appkit-register-handle
     view 'function
     (apply-partially #'qq-user--release-view-work view buffer))
    (let ((hook (apply-partially
                 #'qq-user--handle-media-cache-update view)))
      (with-current-buffer buffer
        (setq qq-user--media-hook-function hook))
      (appkit-register-handle
       view 'hook
       (list 'qq-media-cache-update-hook hook nil buffer))
      (add-hook 'qq-media-cache-update-hook hook))))

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
  "Prepare the shared user buffer to display USER-ID."
  (unless (equal qq-user--user-id user-id)
    (qq-user--cancel-request)
    (qq-user--clear-view-data))
  (setq qq-user--user-id user-id))

(defun qq-user-button-backward ()
  "Move point to the previous page button."
  (interactive)
  (forward-button -1))

(defvar qq-user-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'qq-user-refresh)
    (define-key map (kbd "m") #'qq-user-open-chat)
    (define-key map (kbd "l") #'qq-user-like)
    (define-key map (kbd "a") #'qq-user-open-avatar)
    (define-key map (kbd "TAB") #'forward-button)
    (define-key map (kbd "<backtab>") #'qq-user-button-backward)
    (define-key map (kbd "w") #'qq-user-copy-id)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `qq-user-mode'.")

(define-derived-mode qq-user-mode special-mode "QQ-User"
  "Major mode for one native QQ user profile."
  (setq-local truncate-lines nil)
  (setq-local switch-to-buffer-preserve-window-point nil)
  (setq-local header-line-format '(:eval (qq-user--header-line)))
  (add-hook 'change-major-mode-hook #'qq-user--cancel-request nil t)
  (add-hook 'kill-buffer-hook #'qq-user--cancel-request nil t))

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
      (when (and (null qq-user--profile)
                 (not qq-user--loading))
        (qq-user-refresh))
      (appkit-surface-send view (list 'qq-render (appkit-projection-change-create :full-p t))))
    (pop-to-buffer buffer)
    buffer))

(defun qq-user--handle-media-cache-update (view media-key)
  "Request a targeted VIEW update after MEDIA-KEY changes."
  (when (and (stringp media-key) (qq-user--view-current-p view))
    (with-current-buffer (appkit-surface-buffer view)
      (when (equal media-key (format "avatar:%s" qq-user--user-id))
        (appkit-surface-send view (list 'qq-render (appkit-projection-change-create :keys (list (qq-user--profile-key)) :resources (list media-key))))))))

(provide 'qq-user)

;;; qq-user.el ends here
