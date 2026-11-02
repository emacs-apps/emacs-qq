;;; qq-guild-compose.el --- QQ forum composition on Appkit -*- lexical-binding: t; -*-
;;; Code:
(require 'qq-guild-forum)
(autoload 'qq-guild-share-post "qq-guild-share" nil t)
(require 'appkit-compose)
(require 'appkit-bindings)
(require 'transient)

(defvar-local qq-guild-compose--account nil)
(defvar-local qq-guild-compose--app nil)
(defvar-local qq-guild-compose--target nil)
(defvar-local qq-guild-compose--title nil)
(defvar-local qq-guild-compose--source nil)
(defvar-local qq-guild-compose--outcome nil)
(defvar-local qq-guild-compose--notice nil)

(defun qq-guild-compose--state (app)
  "Read APP's committed guild state."
  (qq-state-partition-guild (appkit-app-model app)))

(defun qq-guild-compose--scope ()
  "Capture a reading Surface and semantic row, never a future point."
  (unless (derived-mode-p 'qq-guild-forum-mode)
    (user-error "请先打开论坛或帖子"))
  (list :surface (appkit-current-surface)
        :key (appkit-discussion-key-at-point)))

(defun qq-guild-compose--resolve-scope (scope)
  "Resolve SCOPE's exact row in the current canonical account snapshot."
  (let ((surface (plist-get scope :surface)))
    (unless (appkit-surface-live-p surface)
      (user-error "原论坛视图已关闭"))
    (let* ((model (appkit-surface-model surface))
           (app (appkit-surface-app surface))
           (state (qq-guild-compose--state app))
           (key (plist-get scope :key))
           (item (seq-find (lambda (item) (equal key (plist-get item :key)))
                           (qq-guild-forum--items state model))))
      (when (and key (null item))
        (user-error "原帖子或回复已不在当前视图中"))
      (list state model item))))

(defun qq-guild-compose--channel-title (state channel)
  "Describe the native CHANNEL using committed STATE."
  (let* ((guild (alist-get 'guild_id channel))
         (name (alist-get 'name
                          (seq-find (lambda (item) (equal guild (alist-get 'guild_id item)))
                                    (plist-get state :guilds))))
         (directory (qq-guild--get state :directories guild))
         (item (seq-find (lambda (item) (qq-guild--same-channel-p channel (alist-get 'channel item)))
                         (alist-get 'channels directory))))
    (concat (or name "QQ 频道") " · "
            (or (alist-get 'name item) (concat "子频道 " (alist-get 'channel_id channel))))))

(defun qq-guild-compose--prepare (action scope)
  "Prepare an immutable ACTION target from captured SCOPE."
  (let* ((resolved (qq-guild-compose--resolve-scope scope))
         (state (nth 0 resolved)) (model (nth 1 resolved)) (item (nth 2 resolved))
         (value (plist-get item :value))
         (post (if (eq (plist-get item :kind) 'post) value
                 (and (eq (plist-get model :kind) 'comments)
                      (qq-guild--find-post state (plist-get model :target)))))
         (identity (alist-get 'identity post))
         target context)
    (if (eq action 'post)
        (let ((channel (if (eq (plist-get model :kind) 'feed)
                           (let* ((guild (alist-get 'guild_id (plist-get model :target)))
                                  (directory (qq-guild--get state :directories guild))
                                  (choices (mapcar
                                            (lambda (item)
                                              (cons (format "%s · %s" (alist-get 'name item)
                                                            (alist-get 'channel_id (alist-get 'channel item)))
                                                    (alist-get 'channel item)))
                                            (seq-filter (lambda (item) (= 7 (alist-get 'channel_type item)))
                                                        (alist-get 'channels directory)))))
                             (unless choices (user-error "请先在频道目录加载论坛子频道"))
                             (cdr (assoc (completing-read "发布到: " choices nil t) choices)))
                         (qq-guild-forum--channel model))))
          (unless (qq-guild--channel-p channel) (user-error "缺少发布子频道"))
          (setq target (list '(kind . "post") (cons 'channel channel))))
      (unless (qq-guild--post-p identity) (user-error "请先加载原帖"))
      (setq target (list (cons 'kind (if (or (eq action 'comment)
                                             (eq (plist-get item :kind) 'post))
                                         "comment" "reply"))
                         (cons 'post identity)
                         (cons 'post_timestamp (alist-get 'timestamp post))))
      (when (equal (alist-get 'kind target) "reply")
        (unless (memq (plist-get item :kind) '(comment reply))
          (user-error "请在要回复的评论上操作"))
        (let* ((parent (if (eq (plist-get item :kind) 'comment) value
                         (plist-get
                          (seq-find (lambda (entry)
                                      (equal (plist-get item :parent-key) (plist-get entry :key)))
                                    (qq-guild-forum--items state model)) :value)))
               (author (alist-get 'tiny_id (alist-get 'author parent))))
          (unless (and (stringp (alist-get 'comment_id parent)) author)
            (user-error "原评论已不可用"))
          (setq target
                (append target
                        (list (cons 'comment_id (alist-get 'comment_id parent))
                              (cons 'comment_author_tiny_id author)
                              (cons 'reply_id (when (eq (plist-get item :kind) 'reply)
                                                (alist-get 'comment_id value)))
                              (cons 'target_tiny_id (alist-get 'tiny_id (alist-get 'author value))))))))
      (let ((quoted (if (equal (alist-get 'kind target) "comment") post value)))
        (setq context (concat (or (alist-get 'name (alist-get 'author quoted)) "") "\n"
                              (truncate-string-to-width (replace-regexp-in-string "\n" "\n> "
                                                                                  (or (alist-get 'text quoted) "")) 240 nil nil "…")))))
    (list :account (plist-get model :account) :target (copy-tree target)
          :source (appkit-surface-buffer (plist-get scope :surface))
          :title (qq-guild-compose--channel-title
                  state (or (alist-get 'channel target) (alist-get 'channel (alist-get 'post target))))
          :context context)))

(defun qq-guild-forum-new-post (&optional scope)
  "Compose a body-only post in a chosen forum channel."
  (interactive)
  (qq-guild-compose--open (qq-guild-compose--prepare 'post (or scope (qq-guild-compose--scope)))))

(defun qq-guild-forum-comment (&optional scope)
  "Comment on the original post, including while reading a reply branch."
  (interactive)
  (qq-guild-compose--open (qq-guild-compose--prepare 'comment (or scope (qq-guild-compose--scope)))))

(defun qq-guild-forum-reply (&optional scope)
  "Comment on a post or reply to the precise comment at point."
  (interactive)
  (qq-guild-compose--open (qq-guild-compose--prepare 'reply (or scope (qq-guild-compose--scope)))))

(defun qq-guild-compose--label ()
  "Describe the kind of the current draft."
  (pcase (alist-get 'kind qq-guild-compose--target)
    ("post" "发帖") ("comment" "评论") (_ "回复")))

(defun qq-guild-compose--changed (&optional _session)
  "Render compose context and operation status without modifying the body."
  (setq-local header-line-format
              (concat qq-guild-compose--title " · " (qq-guild-compose--label)
                      (when-let* ((status (or qq-guild-compose--notice
                                              (appkit-compose-status-text))))
                        (unless (string-empty-p status) (concat " · " status)))))
  (force-mode-line-update))

(defun qq-guild-compose--snapshot ()
  "Capture owned request data, excluding all generated context."
  (list (cons 'target (copy-tree qq-guild-compose--target))
        (cons 'text (buffer-substring-no-properties (point-min) (point-max)))))

(defun qq-guild-compose--current-p (buffer owner)
  "Whether BUFFER still owns this exact publication OWNER."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer (appkit-compose-operation-current-p owner))))

(defun qq-guild-compose--settle (buffer owner outcome notice)
  "Settle OWNER without losing the editable draft in BUFFER."
  (when (qq-guild-compose--current-p buffer owner)
    (with-current-buffer buffer
      (appkit-compose-operation-finish owner)
      (setq-local buffer-read-only nil
                  qq-guild-compose--outcome outcome
                  qq-guild-compose--notice notice)
      (qq-guild-compose--changed))))

(defun qq-guild-compose--cancel (buffer owner cancel)
  "Cancel waiting for OWNER, retaining publication uncertainty and draft."
  (when cancel (funcall cancel))
  (qq-guild-compose--settle buffer owner 'unknown "已停止等待，发布结果未知"))

(defun qq-guild-compose--same-target-p (expected actual)
  "Compare the exact semantic write route, independent of alist ordering."
  (and (equal (alist-get 'kind expected) (alist-get 'kind actual))
       (if (equal (alist-get 'kind expected) "post")
           (qq-guild--same-channel-p (alist-get 'channel expected) (alist-get 'channel actual))
         (and (qq-guild--same-post-p (alist-get 'post expected) (alist-get 'post actual))
              (cl-every (lambda (key) (equal (alist-get key expected) (alist-get key actual)))
                        '(post_timestamp comment_id comment_author_tiny_id reply_id target_tiny_id))))))

(defun qq-guild-compose--result (buffer owner capture epoch result)
  "Apply only an account-bound RESULT for OWNER's immutable CAPTURE."
  (when (qq-guild-compose--current-p buffer owner)
    (with-current-buffer buffer
      (let* ((receipt (ignore-errors (alist-get 'receipt result)))
             (outcome (ignore-errors (alist-get 'outcome receipt)))
             (status (ignore-errors (alist-get 'status outcome)))
             (target (alist-get 'target (plist-get capture :value))))
        (cond
         ((not (and (appkit-app-live-p qq-guild-compose--app)
                    (equal epoch (plist-get (qq-guild-compose--state qq-guild-compose--app) :epoch))
                    (equal qq-guild-compose--account (ignore-errors (alist-get 'account_id result)))
                    (ignore-errors (qq-guild-compose--same-target-p target (alist-get 'target receipt)))))
          (qq-guild-compose--settle buffer owner 'unknown "会话或回执不匹配，发布结果未知"))
         ((and (equal status "accepted")
               (stringp (alist-get 'id outcome)) (not (string-empty-p (alist-get 'id outcome))))
          (appkit-app-send qq-guild-compose--app (list 'qq-guild-publication target))
          (appkit-compose-operation-finish owner)
          (setq-local qq-guild-compose--outcome 'accepted qq-guild-compose--notice "已发布")
          (if (= (plist-get capture :generation) (appkit-compose-generation))
              (progn (set-buffer-modified-p nil) (kill-buffer buffer))
            (setq-local buffer-read-only nil)
            (qq-guild-compose--changed)))
         ((and (equal status "rejected") (integerp (alist-get 'code outcome))
               (stringp (alist-get 'message outcome)))
          (qq-guild-compose--settle buffer owner nil (concat "发布失败：" (alist-get 'message outcome))))
         (t (qq-guild-compose--settle buffer owner 'unknown "发布结果未知，请先查看原帖")))))))

(defun qq-guild-compose--failure (buffer owner body reason)
  "Distinguish proven pre-publication failure from an uncertain write."
  (if (member (alist-get 'code body)
              '("invalid_params" "unsupported_client_profile" "account_not_found" "account_not_online"
                "invalid_guild_publication" "guild_self_identity_unavailable"
                "guild_transport_failed" "guild_operation_rejected"))
      (qq-guild-compose--settle buffer owner nil (concat "未发布：" reason))
    (qq-guild-compose--settle buffer owner 'unknown "未收到发布回执，请先查看原帖")))

(defun qq-guild-compose-submit ()
  "Publish the captured plain-text draft once, fenced by Appkit ownership."
  (interactive)
  (when (appkit-compose-operation-active-p) (user-error "正在等待发布结果"))
  (when (eq qq-guild-compose--outcome 'accepted) (user-error "原稿已发布，请另建草稿"))
  (unless (and (appkit-app-live-p qq-guild-compose--app)
               (eq qq-guild-compose--app (qq-runtime-app qq-guild-compose--account)))
    (user-error "原账号已关闭，草稿已保留"))
  (unless (qq-rpc-method-available-p "guild.forum.write")
    (user-error "当前网关未提供论坛发布功能"))
  (unless (equal "online" (alist-get 'phase (qq-account-get qq-guild-compose--account)))
    (user-error "账号离线，草稿已保留"))
  (let* ((capture (appkit-compose-capture))
         (request (plist-get capture :value))
         (text (alist-get 'text request))
         (buffer (current-buffer))
         (epoch (plist-get (qq-guild-compose--state qq-guild-compose--app) :epoch)))
    (when (string-empty-p (string-trim text)) (user-error "正文不能为空"))
    (when (or (> (string-bytes text) 65536) (string-search "\0" text))
      (user-error "正文过长或包含无效字符"))
    (when (and (eq qq-guild-compose--outcome 'unknown)
               (not (yes-or-no-p "上次发布可能已经成功，仍要再次发布？")))
      (user-error "草稿已保留"))
    (let ((owner (appkit-compose-operation-begin
                  'submitting :generation (plist-get capture :generation) :label "正在发布…"))
          cancel)
      (setq-local buffer-read-only t qq-guild-compose--notice nil)
      (qq-guild-compose--changed)
      (condition-case err
          (progn
            (setq cancel
                  (qq-runtime-call-with-account
                   qq-guild-compose--account
                   (lambda ()
                     (qq-runtime--rpc-fetch (qq-runtime-current-account-id)
                      "guild.forum.write" (list (cons 'request request))
                      (lambda (result) (qq-guild-compose--result buffer owner capture epoch result))
                      (lambda (body reason)
                        (qq-guild-compose--failure buffer owner body reason))))))
            (when (qq-guild-compose--current-p buffer owner)
              (appkit-compose-operation-update
               owner :cancel-function (lambda () (qq-guild-compose--cancel buffer owner cancel)))))
        (error (qq-guild-compose--settle buffer owner 'unknown (error-message-string err)))))))

(defun qq-guild-compose-cancel ()
  "Stop waiting, or discard an idle draft through the normal buffer close flow."
  (interactive)
  (if (appkit-compose-operation-active-p)
      (appkit-compose-cancel-operation)
    (kill-buffer (current-buffer))))

(defun qq-guild-compose--confirm-kill ()
  "Ask before discarding an unpublished modified draft."
  (or (not (buffer-modified-p))
      (yes-or-no-p "丢弃这份论坛草稿？")))

(defun qq-guild-compose--owner-stopped (buffer)
  "Retain BUFFER's draft when its owning account shuts down."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (appkit-compose-operation-active-p) (appkit-compose-cancel-operation))
      (setq-local qq-guild-compose--notice
                  (if (eq qq-guild-compose--outcome 'unknown)
                      "账号已关闭，发布结果未知；草稿已保留" "账号已关闭，草稿已保留"))
      (qq-guild-compose--changed))))

(defun qq-guild-compose--open (prepared)
  "Open or reuse PREPARED's exact account and target composer."
  (unless (qq-rpc-method-available-p "guild.forum.write")
    (user-error "当前网关未提供论坛发布功能"))
  (let* ((account (plist-get prepared :account))
         (app (qq-runtime-app account))
         (target (plist-get prepared :target))
         (existing (seq-find
                    (lambda (buffer)
                      (with-current-buffer buffer
                        (and (derived-mode-p 'qq-guild-compose-mode)
                             (eq app qq-guild-compose--app)
                             (not (eq qq-guild-compose--outcome 'accepted))
                             (qq-guild-compose--same-target-p target qq-guild-compose--target))))
                    (buffer-list))))
    (if existing (progn (pop-to-buffer existing) existing)
      (let ((buffer (generate-new-buffer
                     (qq-runtime-account-buffer-name "guild-compose" (plist-get prepared :title) account))))
        (with-current-buffer buffer
          (qq-guild-compose-mode)
          (setq-local qq-guild-compose--account account qq-guild-compose--app app
                      qq-guild-compose--target (copy-tree target)
                      qq-guild-compose--title (plist-get prepared :title)
                      qq-guild-compose--source (plist-get prepared :source))
          (qq-runtime-bind-account account)
          (appkit-compose-setup :snapshot-function #'qq-guild-compose--snapshot
                                :state-change-function #'qq-guild-compose--changed)
          (when-let* ((context (plist-get prepared :context)))
            (let ((overlay (make-overlay (point-min) (point-min) nil nil nil)))
              (overlay-put overlay 'before-string (propertize (concat context "\n\n") 'face 'shadow))))
          (let ((handle (appkit-register-handle app 'guild-compose buffer #'qq-guild-compose--owner-stopped)))
            (add-hook 'kill-buffer-hook (lambda () (appkit-retire-handle handle)) nil t))
          (add-hook 'kill-buffer-query-functions #'qq-guild-compose--confirm-kill nil t)
          (set-buffer-modified-p nil))
        (pop-to-buffer buffer)
        buffer))))

(defvar-keymap qq-guild-compose-mode-map :parent text-mode-map)
(appkit-bindings-define 'qq-guild-compose-mode-map 'input
                        'submit #'qq-guild-compose-submit 'cancel #'qq-guild-compose-cancel
                        'app-menu #'qq-guild-compose-transient)

(define-derived-mode qq-guild-compose-mode text-mode "QQ-Compose"
  "Compose a plain-text QQ forum post or reply."
  (setq-local require-final-newline nil)
  (appkit-evil-normalize-keymaps))

(defun qq-guild-forum--menu-command (function &rest _arguments)
  "Invoke FUNCTION against the menu's captured reading scope."
  (funcall function (transient-scope 'qq-guild-forum-transient)))

(transient-define-prefix qq-guild-forum-transient ()
  "Forum operations on the captured reading row."
  [["撰写"
    :advice qq-guild-forum--menu-command
    :inapt-if (lambda () (not (qq-rpc-method-available-p "guild.forum.write")))
    ("c" "发帖" qq-guild-forum-new-post)
    ("r" "回复当前条目" qq-guild-forum-reply
     :inapt-if (lambda () (or (not (qq-rpc-method-available-p "guild.forum.write"))
                              (null (plist-get (transient-scope) :key)))))
    ("R" "评论原帖" qq-guild-forum-comment
     :inapt-if (lambda () (or (not (qq-rpc-method-available-p "guild.forum.write"))
                              (null (plist-get (transient-scope) :key)))))]
   ["转发"
    :advice qq-guild-forum--menu-command
    ("f" "转发到私聊／群聊" qq-guild-share-post
     :inapt-if (lambda () (or (not (qq-rpc-method-available-p "guild.share.preview"))
                              (null (plist-get (transient-scope) :key)))))]
   ["阅读"
    ("g" "刷新" qq-guild-forum-refresh)
    (">" "加载更多" qq-guild-forum-load-more)
    ("q" "关闭窗口" quit-window)]]
  (interactive)
  (transient-setup 'qq-guild-forum-transient nil nil :scope (qq-guild-compose--scope)))

(transient-define-prefix qq-guild-compose-transient ()
  "Draft operations without persistent key hints."
  [["草稿"
    ("c" "发布" qq-guild-compose-submit)
    ("k" "停止等待 / 丢弃草稿" qq-guild-compose-cancel)
    ("q" "收起窗口" quit-window)]])

(provide 'qq-guild-compose)
;;; qq-guild-compose.el ends here
