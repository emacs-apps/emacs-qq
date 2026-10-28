;;; qq-guild-share.el --- Guild shares in ordinary QQ conversations -*- lexical-binding: t; -*-

(require 'qq-guild)
(require 'qq-runtime)
(require 'qq-message)

(defun qq-guild-share-card-p (card)
  "Whether CARD is the closed, display-only native forum share."
  (and (qq-api--exact-object-keys-p card '(post guild_name channel_name author text) '(image_url))
       (qq-guild--post-p (alist-get 'post card))
       (or (not (assq 'image_url card))
           (and (stringp (alist-get 'image_url card))
                (not (string-empty-p (alist-get 'image_url card)))))
       (cl-every (lambda (key) (stringp (alist-get key card)))
                 '(guild_name channel_name author text))))

(defun qq-guild-share-segment (card)
  "Project CARD into the shared chat renderer without duplicating a title."
  (unless (qq-guild-share-card-p card)
    (error "无效的频道分享卡片"))
  (list '(type . "card")
        (cons 'data
              (list '(kind . "guild")
                    (cons 'guild-share (copy-tree card))
                    (cons 'source (string-join
                                   (seq-filter (lambda (s) (not (string-empty-p s)))
                                               (list (alist-get 'guild_name card)
                                                     (alist-get 'channel_name card))) " / "))
                    (cons 'content (alist-get 'text card))
                    (cons 'image (alist-get 'image_url card))
                    (cons 'summary (alist-get 'author card))))))

(defun qq-guild-share-open (card account)
  "Open CARD's native post using the originating chat ACCOUNT."
  (unless (qq-guild-share-card-p card) (user-error "无效的频道分享"))
  (require 'qq-guild-forum)
  (qq-runtime-call-with-account
   account
   (lambda ()
     (qq-guild-forum--open 'comments (alist-get 'post card) "帖子详情" account))))

(defun qq-guild-share-post (&optional scope)
  "Forward the captured forum post to a private or group conversation."
  (interactive)
  (require 'qq-chat)
  (require 'qq-guild-compose)
  (setq scope (or scope (qq-guild-compose--scope)))
  (let* ((resolved (qq-guild-compose--resolve-scope scope))
         (state (nth 0 resolved)) (model (nth 1 resolved)) (item (nth 2 resolved))
         (surface (plist-get scope :surface))
         (account (plist-get model :account))
         (app (appkit-surface-app surface))
         (value (if (eq (plist-get item :kind) 'post) (plist-get item :value)
                  (and (eq (plist-get model :kind) 'comments)
                       (qq-guild--find-post state (plist-get model :target)))))
         (post (copy-tree (alist-get 'identity value))))
    (unless (qq-guild--post-p post) (user-error "请在要转发的帖子上操作"))
    (unless (qq-rpc-method-available-p "guild.share.preview")
      (user-error "当前网关未提供频道转发功能"))
    (qq-runtime-call-with-account
     account
     (lambda ()
       (let* ((target (qq-chat--read-forward-target 'individual 1))
              (latest (qq-guild-compose--resolve-scope scope))
              (card (list (cons 'post post) '(guild_name . "")
                          (cons 'channel_name (qq-guild-forum--source-name state value))
                          (cons 'author (alist-get 'name (alist-get 'author value)))
                          (cons 'text (alist-get 'text value)))))
         (unless (and (eq app (qq-runtime-app account))
                      (qq-guild--find-post (car latest) post))
           (user-error "原帖已不在当前频道会话中"))
         (qq-message-send
          target
          (list (list '(type . "guild-share") (cons 'data (list (cons 'post post)))))
          "[频道帖子]"
          (lambda (&rest _)
            (setq qq-chat--last-forward-target-key target)
            (message "已转发频道帖子"))
          (lambda (_body reason) (message "频道转发失败：%s" reason))
          (list (qq-guild-share-segment card))))))))

(provide 'qq-guild-share)
