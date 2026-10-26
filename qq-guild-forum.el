;;; qq-guild-forum.el --- Native Guild Discussion surfaces -*- lexical-binding: t; -*-
;;; Code:
(require 'qq-guild)
(require 'qq-guild-avatar)
(require 'appkit-discussion)
(require 'appkit-projection)
(require 'appkit-bindings)

(defun qq-guild-forum--page (state kind target)
  "Read the canonical page for KIND and TARGET."
  (qq-guild--get state :pages (qq-guild--operation-key kind target)))

(defun qq-guild-forum--request (context model kind target cursor)
  "Request one opaque CURSOR page from the account App."
  (let ((token (make-symbol "guild-forum")))
    (appkit-next
     :model (qq-guild-forum--with model :token token :error nil :request-kind kind :request-target target)
     :render (appkit-projection-change-create :full-p t :frame-p t)
     :commands (list (qq-guild--request-command context kind target (list :cursor cursor) token)))))

(defun qq-guild-forum--init (context input)
  "Initialize a forum or detail Surface from INPUT."
  (qq-guild-forum--request context input (plist-get input :kind) (plist-get input :target) ""))

(defun qq-guild-forum--update (context model message)
  "Handle forum navigation; canonical pages remain in the account App."
  (pcase message
    ('refresh (qq-guild-forum--init context model))
    (`(more ,kind ,target)
     (let* ((state (qq-guild--from-view (appkit-transition-context-app-read-view context)))
            (page (qq-guild-forum--page state kind target)))
       (if (or (plist-get model :token) (and page (null (alist-get 'next_cursor page))))
           (appkit-next :model model :render appkit-render-none)
         (qq-guild-forum--request context model kind target (or (alist-get 'next_cursor page) "")))))
    (`(qq-guild-reply ,_kind ,token ,_value ,reason)
     (if (eq token (plist-get model :token))
         (appkit-next :model (qq-guild-forum--with model :token nil :error reason)
                      :render (appkit-projection-change-create :full-p t :frame-p t))
       (appkit-next :model model :render appkit-render-none)))
    (`(session ,phase)
     (if (equal phase "online") (qq-guild-forum--init context model)
       (appkit-next :model (qq-guild-forum--with model :token nil :error "账号离线")
                    :render (appkit-projection-change-create :full-p t :frame-p t))))
    ('changed (appkit-next :model model :render (appkit-projection-change-create :full-p t :frame-p t)))
    ((pred appkit-projection-change-p) (appkit-next :model model :render message))
    ((pred appkit-delivery-report-p)
     (appkit-next :model (qq-guild-forum--with model :token nil :error "频道请求未能送达")
                  :render (appkit-projection-change-create :full-p t :frame-p t)))
    (_ (appkit-next-reject 'unknown-forum-message))))

(defun qq-guild-forum--channel (model)
  "Return MODEL's exact native channel."
  (if (eq (plist-get model :kind) 'forum) (plist-get model :target)
    (alist-get 'channel (plist-get model :target))))

(defun qq-guild-forum--row (state model kind value &optional parent footer)
  "Project VALUE to a stable Discussion row."
  (let* ((post-p (eq kind 'post))
         (id (if post-p (alist-get 'post_id (alist-get 'identity value)) (alist-get 'comment_id value)))
         (key (if parent (list 'reply parent id) (list kind id)))
         (qq-guild-avatar--epoch (plist-get state :epoch))
         (avatar (qq-guild-avatar-key
                  (alist-get 'guild_id (qq-guild-forum--channel model))
                  (alist-get 'tiny_id (alist-get 'author value)))))
    (appkit-projection-row-create
     :key key :payload (list :kind kind :value value :parent parent :footer footer :avatar avatar)
     :dependencies (list avatar)
     :resource-demands (when (display-graphic-p) (list (qq-guild-avatar-demand avatar))))))

(defun qq-guild-forum--project (surface view model)
  "Build discussion rows from the pass-scoped VIEW."
  (qq-runtime-bind-account (plist-get model :account))
  (setq-local qq-runtime--surface-owner surface)
  (let* ((state (qq-guild--from-view view))
         (kind (plist-get model :kind))
         (target (plist-get model :target))
         (page (qq-guild-forum--page state kind target))
         rows)
    (if (eq kind 'forum)
        (dolist (post (alist-get 'posts page))
          (push (qq-guild-forum--row state model 'post post nil
                                    (format "%d 条评论 · RET 打开" (alist-get 'comment_count post))) rows))
      (let* ((directory (qq-guild-forum--page state 'forum (qq-guild-forum--channel model)))
             (post (seq-find (lambda (p) (qq-guild--same-post-p target (alist-get 'identity p)))
                             (alist-get 'posts directory))))
        (when post (push (qq-guild-forum--row state model 'post post) rows)))
      (dolist (comment (alist-get 'comments page))
        (let* ((id (alist-get 'comment_id comment))
               (reply-target `((post . ,target) (comment_id . ,id)))
               (reply-page (qq-guild-forum--page state 'replies reply-target))
               (replies (if reply-page (alist-get 'replies reply-page) (alist-get 'replies comment)))
               (total (if reply-page (alist-get 'total_count reply-page) (alist-get 'reply_count comment)))
               (more (if reply-page (alist-get 'next_cursor reply-page) (> total (length replies)))))
          (push (qq-guild-forum--row
                 state model 'comment comment nil
                 (when (> total 0) (format "%d/%d 条回复%s" (length replies) total
                                          (if more " · RET 加载更多" "")))) rows)
          (dolist (reply replies)
            (push (qq-guild-forum--row state model 'reply reply id) rows)))))
    (nreverse rows)))

(defun qq-guild-forum--print (surface _view row)
  "Print one resource-backed Discussion ROW without performing I/O."
  (let* ((payload (appkit-projection-row-payload row))
         (value (plist-get payload :value))
         (key (appkit-projection-row-key row))
         (parent (plist-get payload :parent))
         (author (alist-get 'author value))
         (title (alist-get 'title value)))
    (appkit-discussion-insert-entry
     (appkit-discussion-entry-create
      :key key :parent-key (and parent (list 'comment parent)) :depth (if parent 1 0)
      :avatar (qq-guild-avatar-image (plist-get payload :avatar)) :avatar-fallback "@"
      :heading (if (string-empty-p (alist-get 'name author))
                   (concat "Tiny " (alist-get 'tiny_id author)) (alist-get 'name author))
      :heading-face (appkit-name-color-face (alist-get 'tiny_id author))
      :time (format-time-string "%m-%d %H:%M" (seconds-to-time (alist-get 'timestamp value)))
      :time-face 'shadow
      :body-inserter
      (lambda (prefix _properties)
        (let ((begin (point)))
          (when (and title (not (string-empty-p title)))
            (insert (propertize title 'face 'bold) "\n"))
          (insert (alist-get 'text value))
          (when (> (alist-get 'unsupported_elements value) 0)
            (insert (propertize (format " [暂未支持的内容 × %d]" (alist-get 'unsupported_elements value)) 'face 'shadow)))
          (insert "\n")
          (appkit-ui-apply-line-prefix begin (point) prefix)))
      :footer (plist-get payload :footer) :footer-face 'shadow
      :properties (list 'qq-guild-forum-item payload))
     :width (or (appkit-surface-responsive-width surface) fill-column))))

(defun qq-guild-forum--frame (_surface view model)
  "Project loading, failure and opaque-pagination status."
  (let* ((state (qq-guild--from-view view))
         (page (qq-guild-forum--page state (plist-get model :kind) (plist-get model :target)))
         (empty (null (alist-get (if (eq (plist-get model :kind) 'forum) 'posts 'comments) page)))
         (status (cond ((plist-get model :token) "正在加载…")
                       ((plist-get model :error))
                       ((and page empty) "暂无内容")
                       ((and page (null (alist-get 'next_cursor page))) "已加载全部")
                       (t "按 > 加载更多"))))
    (setq-local header-line-format (plist-get model :title))
    (cons "" (propertize (concat "\n" status "\n") 'face 'shadow))))

(defun qq-guild-forum--model ()
  "Return this Surface's navigation model."
  (appkit-surface-model (appkit-current-surface)))

(defun qq-guild-forum-refresh ()
  "Refresh the first post or comment page."
  (interactive)
  (appkit-surface-send (appkit-current-surface) 'refresh))

(defun qq-guild-forum-load-more ()
  "Continue the current list using its authoritative opaque cursor."
  (interactive)
  (let ((model (qq-guild-forum--model)))
    (appkit-surface-send (appkit-current-surface)
                         (list 'more (plist-get model :kind) (plist-get model :target)))))

(defun qq-guild-forum-open-at-point ()
  "Open a post or expand replies beneath the selected comment."
  (interactive)
  (let* ((item (get-text-property (point) 'qq-guild-forum-item))
         (value (plist-get item :value))
         (model (qq-guild-forum--model)))
    (pcase (plist-get item :kind)
      ('post
       (when (eq (plist-get model :kind) 'forum)
         (qq-guild-forum--open 'comments (alist-get 'identity value)
                               (or (alist-get 'title value) "论坛帖子") (plist-get model :account))))
      ('comment
       (appkit-surface-send
        (appkit-current-surface)
        (list 'more 'replies `((post . ,(plist-get model :target))
                               (comment_id . ,(alist-get 'comment_id value))))))
      (_ (user-error "请在帖子或顶层评论上操作")))))

(defvar-keymap qq-guild-forum-mode-map
  :parent special-mode-map
  "RET" #'qq-guild-forum-open-at-point "<return>" #'qq-guild-forum-open-at-point
  ">" #'qq-guild-forum-load-more
  "n" #'appkit-discussion-next-entry "p" #'appkit-discussion-previous-entry)

(appkit-bindings-define 'qq-guild-forum-mode-map 'read-only
                        'refresh #'qq-guild-forum-refresh 'activate #'qq-guild-forum-open-at-point
                        'quit #'quit-window)

(define-derived-mode qq-guild-forum-mode appkit-discussion-mode "QQ-Discussion"
  "Read native QQ forum posts, comments and nested replies."
  (setq truncate-lines nil)
  (setq-local revert-buffer-function (lambda (&rest _) (qq-guild-forum-refresh))))

(defconst qq-guild-forum--type
  (appkit-surface-type-create
   :name 'qq-guild-forum :mode #'qq-guild-forum-mode
   :init #'qq-guild-forum--init :update #'qq-guild-forum--update
   :subscriptions
   (lambda (model)
     (list (appkit-subscription-create
            :topic (cons 'qq-guild-channel (qq-guild--key (qq-guild-forum--channel model)))
            :notify (lambda (_) 'changed))
           (appkit-subscription-create :topic 'qq-guild-session
                                       :notify (lambda (phase) (list 'session phase)))))
   :renderer-factory
   (lambda (_surface)
     (appkit-projection-renderer-create
      :project-all #'qq-guild-forum--project :project-frame #'qq-guild-forum--frame
      :printer #'qq-guild-forum--print :anchor-property 'appkit-discussion-key
      :geometry-mode 'redraw :no-separator-p t))))

(defun qq-guild-forum--open (kind target title account)
  "Open an account-owned Discussion for KIND and TARGET."
  (unless (qq-rpc-method-available-p (if (eq kind 'forum) "guild.forum" "guild.comments"))
    (user-error "当前网关未提供论坛读取功能"))
  (appkit-surface-buffer
   (qq-guild--open-surface
    qq-guild-forum--type account (cons 'guild-forum (qq-guild--operation-key kind target))
    (list :kind kind :target target :title title) title)))

(defun qq-guild-forum-open (channel title account)
  "Open native forum CHANNEL under ACCOUNT."
  (qq-guild-forum--open 'forum channel title account))




(defun qq-guild-forum--with (model &rest properties)
  "Copy MODEL and replace navigation PROPERTIES without accumulating shadows."
  (setq model (copy-sequence model))
  (while properties (setq model (plist-put model (pop properties) (pop properties))))
  model)

(provide 'qq-guild-forum)
;;; qq-guild-forum.el ends here
