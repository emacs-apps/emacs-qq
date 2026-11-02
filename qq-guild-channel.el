;;; qq-guild-channel.el --- Guild channel type dispatch -*- lexical-binding: t; -*-
;;; Code:
(require 'qq-guild)
(require 'appkit-projection)
(require 'appkit-bindings)

(defun qq-guild-channel-label (type)
  "Describe native channel TYPE, preserving unknown values."
  (or (cdr (assq type '((1 . "文字") (2 . "语音") (5 . "直播") (7 . "论坛"))))
      (format "类型 %s" type)))

(defun qq-guild-channel-icon (type)
  "Return a compact directory marker for TYPE."
  (or (cdr (assq type '((1 . "#") (2 . "♫") (5 . "◉") (7 . "▤")))) "◇"))

(defun qq-guild-channel--project (surface view model)
  "Read the current channel directory record for an inspect Surface."
  (qq-runtime-bind-account (plist-get model :account))
  (setq-local qq-runtime--surface-owner surface)
  (let* ((route (plist-get model :channel))
         (state (qq-guild--from-view view))
         (directory (qq-guild--get state :directories (alist-get 'guild_id route)))
         (item (seq-find (lambda (entry) (qq-guild--same-channel-p route (alist-get 'channel entry)))
                         (alist-get 'channels directory))))
    (list (appkit-projection-row-create :key 'channel
                                       :payload (list :channel route :item item :type (plist-get model :type)
                                                      :title (plist-get model :title))))))

(defun qq-guild-channel--print (_surface _view row)
  "Show known metadata and the precise capability of this channel."
  (let* ((data (appkit-projection-row-payload row))
         (route (plist-get data :channel))
         (item (plist-get data :item))
         (type (or (alist-get 'channel_type item) (plist-get data :type))))
    (insert (propertize (or (alist-get 'name item) (plist-get data :title)) 'face 'bold) "\n\n"
            "子频道类型：" (qq-guild-channel-label type) "\n"
            "频道 ID：" (alist-get 'guild_id route) "\n"
            "子频道 ID：" (alist-get 'channel_id route) "\n\n"
            (propertize (pcase type
                          (2 "语音子频道：目前可查看目录，尚未接入语音会话。")
                          (5 "直播子频道：目前可查看目录，尚未接入直播播放。")
                          (_ "此类型目前可查看目录与原生身份，内容接口尚未接入。")) 'face 'shadow) "\n")))

(define-derived-mode qq-guild-channel-mode special-mode "QQ-Channel"
  "Inspect native channel types that do not yet have a content adapter.")

(defconst qq-guild-channel--type
  (appkit-surface-type-create
   :name 'qq-guild-channel :mode #'qq-guild-channel-mode
   :init (lambda (_context input)
           (appkit-next :model input :render (appkit-projection-change-create :full-p t)))
   :update (lambda (_context model _message)
             (appkit-next :model model :render (appkit-projection-change-create :full-p t)))
   :subscriptions (lambda (_model)
                    (list (appkit-subscription-create :topic 'qq-guild-directory :notify (lambda (_) 'changed))))
   :renderer-factory
   (lambda (_surface)
     (appkit-projection-renderer-create :project-all #'qq-guild-channel--project
                                       :printer #'qq-guild-channel--print :anchor-property 'qq-guild-channel-row))))

(defun qq-guild-channel-open (channel title account type)
  "Dispatch the native TYPE to its own content adapter."
  (pcase type
    (1 (require 'qq-guild-chat) (qq-guild-chat-open channel title account))
    (7 (require 'qq-guild-forum) (qq-guild-forum-open channel title account))
    (_ (appkit-surface-buffer
        (qq-guild--open-surface
         qq-guild-channel--type account (cons 'guild-channel (qq-guild--key channel))
         (list :channel channel :title title :type type) title)))))

(provide 'qq-guild-channel)
;;; qq-guild-channel.el ends here
