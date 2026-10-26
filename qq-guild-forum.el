;;; qq-guild-forum.el --- Native Guild Discussion surfaces -*- lexical-binding: t; -*-
;;; Code:
(require 'qq-guild)
(require 'qq-guild-avatar)
(require 'qq-guild-media)
(require 'appkit-discussion)
(require 'appkit-projection)
(require 'appkit-bindings)

(autoload 'qq-guild-forum-new-post "qq-guild-compose" nil t)
(autoload 'qq-guild-forum-reply "qq-guild-compose" nil t)
(autoload 'qq-guild-forum-transient "qq-guild-compose" nil t)

(declare-function qq-guild-home--header "qq-guild-home" (model))
(declare-function qq-guild-home--label "qq-guild-home" (view))
(autoload 'qq-guild-home-all "qq-guild-home" nil t)
(autoload 'qq-guild-home-hot "qq-guild-home" nil t)
(autoload 'qq-guild-home-essence "qq-guild-home" nil t)



(defun qq-guild-forum--page (state kind target)
  "Read the canonical page for KIND and TARGET."
  (qq-guild--get state :pages (qq-guild--operation-key kind target)))

(defun qq-guild-forum--request (context model kind target cursor)
  "Request one opaque CURSOR page from the account App."
  (if (and (eq kind 'feed) (not (qq-rpc-method-available-p "guild.feed")))
      (appkit-next
       :model (qq-guild-forum--with model :token nil
                                   :error "当前网关未提供频道首页功能，请更新网关后重连")
       :render (appkit-projection-change-create :full-p t :frame-p t))
    (let ((token (make-symbol "guild-forum")))
      (appkit-next
       :model (qq-guild-forum--with model :token token :error nil :request-kind kind :request-target target)
       :render (appkit-projection-change-create :full-p t :frame-p t)
       :commands (list (qq-guild--request-command context kind target (list :cursor cursor) token))))))


(defun qq-guild-forum--publication-version (state model)
  "Return the account's publication revision for MODEL's guild."
  (or (qq-guild--get state :publications
                     (if (eq (plist-get model :kind) 'feed)
                         (alist-get 'guild_id (plist-get model :target))
                       (alist-get 'guild_id (qq-guild-forum--channel model))))
      0))

(defun qq-guild-forum--init (context input)
  "Initialize a forum or detail Surface from INPUT."
  (qq-guild-forum--request
   context (qq-guild-forum--with
            input :publication
            (qq-guild-forum--publication-version
             (qq-guild--from-view (appkit-transition-context-app-read-view context)) input))
   (plist-get input :kind) (plist-get input :target) ""))







(defun qq-guild-forum--update (context model message)
  "Handle forum navigation; canonical pages remain in the account App."
  (pcase message
    ((and (pred consp) (guard (eq (car message) 'qq-media)))
     (qq-media-update context model message))
    ('refresh (qq-guild-forum--init context model))
    (`(navigate ,action ,key) (qq-guild-forum--navigate context model action key))
    (`(more ,kind ,target)
     (let* ((state (qq-guild--from-view (appkit-transition-context-app-read-view context)))
            (page (qq-guild-forum--page state kind target)))
       (if (or (plist-get model :token) (and page (null (alist-get 'next_cursor page))))
           (appkit-next :model model :render appkit-render-none)
         (qq-guild-forum--request context model kind target (or (alist-get 'next_cursor page) "")))))
    (`(qq-guild-reply ,_kind ,token ,_value ,reason)
     (if (eq token (plist-get model :token))
         (appkit-next :model (qq-guild-forum--reconcile-focus
                             context (qq-guild-forum--with model :token nil :error reason))
                      :render (appkit-projection-change-create :full-p t :frame-p t))
       (appkit-next :model model :render appkit-render-none)))
    (`(session ,phase)
     (if (equal phase "online") (qq-guild-forum--init context model)
       (appkit-next :model (qq-guild-forum--with model :token nil :error "账号离线")
                    :render (appkit-projection-change-create :full-p t :frame-p t))))
    ('changed
     (if (= (or (plist-get model :publication) 0)
            (qq-guild-forum--publication-version
             (qq-guild--from-view (appkit-transition-context-app-read-view context)) model))
         (appkit-next :model (qq-guild-forum--reconcile-focus context model)
                      :render (appkit-projection-change-create :full-p t :frame-p t))
       (qq-guild-forum--init context model)))
    ((pred appkit-projection-change-p) (appkit-next :model model :render message))
    ((pred appkit-delivery-report-p)
     (appkit-next :model (qq-guild-forum--with model :token nil :error "频道请求未能送达")
                  :render (appkit-projection-change-create :full-p t :frame-p t)))
    (_ (appkit-next-reject 'unknown-forum-message))))

(defun qq-guild-forum--channel (model)
  "Return MODEL's exact native channel."
  (if (eq (plist-get model :kind) 'forum) (plist-get model :target)
    (alist-get 'channel (plist-get model :target))))

(defun qq-guild-forum--source-name (state post)
  "Return POST's source subchannel label from the account directory."
  (let* ((channel (alist-get 'channel (alist-get 'identity post)))
         (directory (qq-guild--get state :directories (alist-get 'guild_id channel)))
         (entry (seq-find (lambda (entry) (qq-guild--same-channel-p channel (alist-get 'channel entry)))
                          (alist-get 'channels directory))))
    (or (alist-get 'name entry) (concat "子频道 " (alist-get 'channel_id channel)))))



(defun qq-guild-forum--items (state model)
  "Read canonical content as parent-linked items without presentation state."
  (let* ((kind (plist-get model :kind))
         (target (plist-get model :target))
         (page (qq-guild-forum--page state kind target))
         (post-key (and (eq kind 'comments) (list 'post (alist-get 'post_id target))))
         items)
    (if (memq kind '(forum feed))
        (dolist (post (alist-get 'posts page))
          (push (list :key (if (eq kind 'feed)
                                (cons 'post (cdr (qq-guild--operation-key 'comments (alist-get 'identity post))))
                              (list 'post (alist-get 'post_id (alist-get 'identity post))))
                      :kind 'post :value post
                      :footer (concat (when (eq kind 'feed)
                                        (concat (qq-guild-forum--source-name state post) " · "))
                                      (format "%d 条评论" (alist-get 'comment_count post))))
                items))
      (let ((post (qq-guild--find-post state target)))
        (when post
          (push (list :key post-key :kind 'post :value post
                      :reply-count (or (alist-get 'total_count page) (alist-get 'comment_count post))
                      :footer (format "%d/%d 条评论" (length (alist-get 'comments page))
                                      (or (alist-get 'total_count page) (alist-get 'comment_count post))))
                items)))
      (dolist (comment (alist-get 'comments page))
        (let* ((id (alist-get 'comment_id comment))
               (key (list 'comment id))
               (reply-target (list (cons 'post target) (cons 'comment_id id)))
               (reply-page (qq-guild-forum--page state 'replies reply-target))
               (replies (if reply-page (alist-get 'replies reply-page) (alist-get 'replies comment)))
               (total (if reply-page (alist-get 'total_count reply-page) (alist-get 'reply_count comment)))
               (more (if reply-page (alist-get 'next_cursor reply-page) (> total (length replies)))))
          (push (list :key key :parent-key post-key :kind 'comment :value comment
                      :reply-count total :more-p more
                      :footer (when (> total 0)
                                (format "%d/%d 条回复%s" (length replies) total
                                        (if more " · 还有更多" ""))))
                items)
          (dolist (reply replies)
            (push (list :key (list 'reply id (alist-get 'comment_id reply))
                        :parent-key key :kind 'reply :value reply) items)))))
    (nreverse items)))

(defun qq-guild-forum--ancestors (item index)
  "Return ITEM's available ancestors in INDEX, ordered root to parent."
  (let ((parent (plist-get item :parent-key))
        (seen (list (plist-get item :key)))
        result)
    (while (and parent (not (member parent seen)) (gethash parent index))
      (push parent seen)
      (let ((ancestor (gethash parent index)))
        (push ancestor result)
        (setq parent (plist-get ancestor :parent-key))))
    result))

(defun qq-guild-forum--layout (model items)
  "Project focused discussion scope and folds from MODEL over ITEMS.
As in Chidu and Chirp, ancestors and focus share a depth-zero spine;
only descendants below the focus consume horizontal nesting depth."
  (let ((index (make-hash-table :test #'equal)) layouts)
    (dolist (item items) (puthash (plist-get item :key) item index))
    (let* ((focus (or (gethash (plist-get model :focus-key) index)
                      (and (eq (plist-get model :kind) 'comments)
                           (gethash (list 'post (alist-get 'post_id (plist-get model :target))) index))))
           (focus-key (plist-get focus :key))
           (chain (and focus (mapcar (lambda (item) (plist-get item :key))
                                    (qq-guild-forum--ancestors focus index)))))
      (dolist (item items (nreverse layouts))
        (let* ((key (plist-get item :key))
               (ancestors (qq-guild-forum--ancestors item index))
               (ancestor-keys (mapcar (lambda (ancestor) (plist-get ancestor :key)) ancestors))
               (chain-p (member key chain))
               (focus-p (equal key focus-key))
               (descendant (and focus-key (member focus-key ancestor-keys)))
               (role (cond (focus-p 'focus) (chain-p 'chain) (t 'tree)))
               (override (assoc key (plist-get model :body-overrides))))
          (when (and (or (null focus) focus-p chain-p descendant)
                     (not (seq-some (lambda (ancestor)
                                      (member ancestor (plist-get model :collapsed-replies)))
                                    ancestor-keys)))
            (push (qq-guild-forum--with
                   item :role role
                   :depth (cond ((or chain-p focus-p) 0)
                                (descendant (length descendant))
                                (t (length ancestors)))
                   :connector (cond (chain-p 'continue) ((and focus-p chain) 'end))
                   :body-visible-p (if override (cdr override) (not chain-p))
                   :replies-collapsed-p (and (member key (plist-get model :collapsed-replies)) t)
                   :parent-name (alist-get 'name (alist-get 'author (plist-get (car (last ancestors)) :value))))
                  layouts)))))))

(defun qq-guild-forum--reconcile-focus (context model)
  "Drop a focus that disappeared from the committed account read view."
  (let ((focus (plist-get model :focus-key)))
    (if (and focus
             (not (seq-find
                   (lambda (item) (equal focus (plist-get item :key)))
                   (qq-guild-forum--items
                    (qq-guild--from-view (appkit-transition-context-app-read-view context)) model))))
        (qq-guild-forum--with model :focus-key nil)
      model)))

(defun qq-guild-forum--navigate (context model action key)
  "Apply local reading ACTION to KEY without changing the account App."
  (let* ((state (qq-guild--from-view (appkit-transition-context-app-read-view context)))
         (items (qq-guild-forum--items state model))
         (layout (seq-find (lambda (item) (equal key (plist-get item :key)))
                           (qq-guild-forum--layout model items)))
         (next model))
    (if (or (eq action 'show-all) layout)
        (progn
          (pcase action
            ('show-all (setq next (qq-guild-forum--with model :focus-key nil :collapsed-replies nil)))
            ('focus
             (let ((index (make-hash-table :test #'equal)))
               (dolist (item items) (puthash (plist-get item :key) item index))
               (let ((opened (cons key (mapcar (lambda (item) (plist-get item :key))
                                              (qq-guild-forum--ancestors layout index)))))
                 (setq next (qq-guild-forum--with
                             model :focus-key key
                             :collapsed-replies
                             (seq-remove (lambda (fold) (member fold opened))
                                         (plist-get model :collapsed-replies)))))))
            ('toggle-body
             (setq next (qq-guild-forum--with
                         model :body-overrides
                         (cons (cons key (not (plist-get layout :body-visible-p)))
                               (assoc-delete-all key (copy-tree (plist-get model :body-overrides)))))))
            ('toggle-replies
             (when (> (or (plist-get layout :reply-count) 0) 0)
               (let ((folds (plist-get model :collapsed-replies)))
                 (setq next (qq-guild-forum--with
                             model :collapsed-replies
                             (if (member key folds) (remove key folds) (cons key folds))))))))
          (appkit-next :model next
                       :render (appkit-projection-change-create
                                :full-p t :frame-p t :position (if (eq action 'show-all) 'first key))))
      (appkit-next :model model :render appkit-render-none))))

(defun qq-guild-forum--row (state model item)
  "Add owned avatar and visible body resources to discussion ITEM."
  (let* ((qq-guild-avatar--epoch (plist-get state :epoch))
         (avatar (qq-guild-avatar-key
                  (alist-get 'guild_id (or (alist-get 'channel (alist-get 'identity (plist-get item :value)))
                                          (qq-guild-forum--channel model)))
                  (alist-get 'tiny_id (alist-get 'author (plist-get item :value)))))
         (plan (qq-guild-media-plan state model item))
         (specs (append (plist-get plan :media) (plist-get plan :emoji))))
    (appkit-projection-row-create
     :key (plist-get item :key)
     :payload (append (qq-guild-forum--with item :avatar avatar) plan)
     :dependencies (cons avatar (mapcar (lambda (spec) (plist-get spec :key)) specs))
     :resource-demands
     (when (display-graphic-p)
       (cons (qq-guild-avatar-demand avatar)
             (delq nil (mapcar #'qq-guild-media-demand specs)))))))

(defun qq-guild-forum--project (surface view model)
  "Build focused discussion rows from the pass-scoped VIEW."
  (qq-runtime-bind-account (plist-get model :account))
  (setq-local qq-runtime--surface-owner surface)
  (let ((state (qq-guild--from-view view)))
    (mapcar (lambda (item) (qq-guild-forum--row state model item))
            (qq-guild-forum--layout model (qq-guild-forum--items state model)))))



(defun qq-guild-forum--print (surface _view row)
  "Print one normalized Discussion ROW without performing I/O."
  (let* ((payload (appkit-projection-row-payload row))
         (value (plist-get payload :value))
         (author (alist-get 'author value))
         (visible (plist-get payload :body-visible-p))
         (folded (plist-get payload :replies-collapsed-p))
         (footer (plist-get payload :footer)))
    (appkit-discussion-insert-entry
     (appkit-discussion-entry-create
      :key (appkit-projection-row-key row)
      :parent-key (plist-get payload :parent-key)
      :depth (plist-get payload :depth)
      :connector (plist-get payload :connector)
      :context (when (and (> (plist-get payload :depth) 1) (plist-get payload :parent-name))
                 (concat "回复 " (plist-get payload :parent-name)))
      :context-face 'shadow
      :avatar (qq-guild-avatar-image (plist-get payload :avatar)) :avatar-fallback "@"
      :heading (concat (propertize (if visible "▾ " "▸ ") 'face 'shadow)
                       (when (> (or (plist-get payload :reply-count) 0) 0)
                         (propertize (if folded "⊞ " "⊟ ") 'face 'shadow))
                       (if (string-empty-p (alist-get 'name author))
                           (concat "Tiny " (alist-get 'tiny_id author)) (alist-get 'name author)))
      :heading-face (appkit-name-color-face (alist-get 'tiny_id author))
      :time (format-time-string (if (eq (plist-get payload :role) 'focus)
                                    "%Y-%m-%d %H:%M" "%m-%d %H:%M")
                                (seconds-to-time (alist-get 'timestamp value)))
      :time-face 'shadow
      :body-inserter
      (lambda (prefix _properties)
        (let ((begin (point)))
          (if visible
              (progn
                (qq-guild-media-insert-text surface value (plist-get payload :emoji))
                (when (> (alist-get 'unsupported_elements value) 0)
                  (insert (propertize
                           (format " [暂未支持的内容 × %d]" (alist-get 'unsupported_elements value))
                           'face 'shadow)))
                (qq-guild-media-insert
                 surface (plist-get payload :media)
                 (max 1 (- (or (appkit-surface-responsive-width surface) fill-column)
                           (string-width (appkit-ui-prefix-string prefix))))))
            (insert (propertize "正文已折叠" 'face 'shadow)))
          (insert "\n")
          (appkit-ui-apply-line-prefix begin (point) prefix)))
      :footer (if folded
                  (format "%d 条%s已折叠" (plist-get payload :reply-count)
                          (if (eq (plist-get payload :kind) 'post) "评论" "回复"))
                footer)
      :footer-face 'shadow
      :properties (list 'qq-guild-forum-item payload))
     :width (or (appkit-surface-responsive-width surface) fill-column)
     :indent-width 3)))

(defun qq-guild-forum--list-target (model)
  "Return the pageable kind and target for MODEL's current reading scope."
  (let* ((focus (plist-get model :focus-key))
         (comment (and (eq (plist-get model :kind) 'comments)
                       (memq (car-safe focus) '(comment reply))
                       (cadr focus))))
    (if comment
        (list 'replies (list (cons 'post (plist-get model :target)) (cons 'comment_id comment)))
      (list (plist-get model :kind) (plist-get model :target)))))

(defun qq-guild-forum--frame (_surface view model)
  "Project loading, failure and opaque-pagination status for this scope."
  (let* ((state (qq-guild--from-view view))
         (scope (qq-guild-forum--list-target model))
         (kind (car scope))
         (page (qq-guild-forum--page state kind (cadr scope)))
         (empty (null (alist-get (pcase kind ((or 'forum 'feed) 'posts) ('comments 'comments) (_ 'replies)) page)))
         (status (cond ((plist-get model :token) "正在加载…")
                       ((plist-get model :error))
                       ((and page empty) "暂无内容")
                       ((and page (null (alist-get 'next_cursor page))) "已加载全部"))))
    (setq-local header-line-format
                (concat (pcase (plist-get model :kind)
                          ('feed (require 'qq-guild-home) (qq-guild-home--header model))
                          ('forum (plist-get model :title))
                          (_ "帖子详情"))
                        (when (eq kind 'replies) " · 聚焦回复")))
    (cons "" (if status (propertize (concat "\n" status "\n") 'face 'shadow) ""))))

(defun qq-guild-forum--model ()
  "Return this Surface's navigation model."
  (appkit-surface-model (appkit-current-surface)))

(defun qq-guild-forum-refresh ()
  "Refresh the first post or comment page."
  (interactive)
  (appkit-surface-send (appkit-current-surface) 'refresh))

(defun qq-guild-forum-load-more ()
  "Continue the focused reply branch or current list using its opaque cursor."
  (interactive)
  (let ((scope (qq-guild-forum--list-target (qq-guild-forum--model))))
    (appkit-surface-send (appkit-current-surface) (cons 'more scope))))

(defun qq-guild-forum-open-at-point ()
  "Open a post, expand a folded branch, or fetch the next reply page."
  (interactive)
  (let* ((item (get-text-property (point) 'qq-guild-forum-item))
         (value (plist-get item :value))
         (model (qq-guild-forum--model)))
    (cond
     ((and (eq (plist-get item :kind) 'post) (memq (plist-get model :kind) '(forum feed)))
      (qq-guild-forum--open 'comments (alist-get 'identity value)
                            "帖子详情" (plist-get model :account)))
     ((plist-get item :replies-collapsed-p) (qq-guild-forum-toggle-replies))
     ((and (eq (plist-get item :kind) 'comment) (plist-get item :more-p))
      (appkit-surface-send
       (appkit-current-surface)
       (list 'more 'replies (list (cons 'post (plist-get model :target))
                                  (cons 'comment_id (alist-get 'comment_id value))))))
     (item (qq-guild-forum-focus))
     (t (user-error "请在帖子或评论上操作")))))

(defun qq-guild-forum--read-action (action)
  "Send local reading ACTION for the entry at point."
  (let ((key (appkit-discussion-key-at-point)))
    (unless (or key (eq action 'show-all)) (user-error "请在帖子或评论上操作"))
    (appkit-surface-send (appkit-current-surface) (list 'navigate action key))))

(defun qq-guild-forum-toggle-body ()
  "Show or hide the body at point without fetching or changing account data."
  (interactive)
  (qq-guild-forum--read-action 'toggle-body))

(defun qq-guild-forum-toggle-replies ()
  "Fold or unfold the reply subtree at point without making a request."
  (interactive)
  (unless (> (or (plist-get (get-text-property (point) 'qq-guild-forum-item) :reply-count) 0) 0)
    (user-error "当前条目没有回复"))
  (qq-guild-forum--read-action 'toggle-replies))

(defun qq-guild-forum-focus ()
  "Read only this entry, its ancestors and its descendants."
  (interactive)
  (when (memq (plist-get (qq-guild-forum--model) :kind) '(forum feed))
    (user-error "请先打开一个帖子"))
  (qq-guild-forum--read-action 'focus))

(defun qq-guild-forum-show-all ()
  "Return to the complete loaded discussion and open its reply folds."
  (interactive)
  (qq-guild-forum--read-action 'show-all))

(defun qq-guild-forum-focus-parent ()
  "Focus the parent of the entry at point, or return to the whole discussion."
  (interactive)
  (let ((parent (get-text-property (point) 'appkit-discussion-parent-key)))
    (if (and parent (appkit-position-find-property-value
                     (point-min) (point-max) 'appkit-discussion-key parent))
        (appkit-surface-send (appkit-current-surface) (list 'navigate 'focus parent))
      (qq-guild-forum-show-all))))

(defvar-keymap qq-guild-forum-mode-map
  :parent special-mode-map
  "TAB" #'qq-guild-forum-toggle-replies "<tab>" #'qq-guild-forum-toggle-replies
  "v" '(menu-item "" qq-guild-forum-toggle-body :filter appkit-bindings--native-readonly)
  "f" '(menu-item "" qq-guild-forum-focus :filter appkit-bindings--native-readonly)
  "u" '(menu-item "" qq-guild-forum-focus-parent :filter appkit-bindings--native-readonly)
  "C-c C-a" #'qq-guild-forum-show-all
  "C-c 1" #'qq-guild-home-all
  "C-c 2" #'qq-guild-home-hot
  "C-c 3" #'qq-guild-home-essence)

(appkit-bindings-define 'qq-guild-forum-mode-map 'read-only
                        'compose #'qq-guild-forum-new-post 'reply #'qq-guild-forum-reply
                        'app-menu #'qq-guild-forum-transient
                        'refresh #'qq-guild-forum-refresh 'activate #'qq-guild-forum-open-at-point
                        'next-item #'appkit-discussion-next-entry
                        'previous-item #'appkit-discussion-previous-entry
                        'history-newer #'qq-guild-forum-load-more
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
            :topic (if (eq (plist-get model :kind) 'feed)
                       (cons 'qq-guild-home (alist-get 'guild_id (plist-get model :target)))
                     (cons 'qq-guild-channel (qq-guild--key (qq-guild-forum--channel model))))
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
  (unless (qq-rpc-method-available-p (pcase kind ('feed "guild.feed") ('forum "guild.forum") (_ "guild.comments")))
    (user-error "当前网关未提供论坛读取功能"))
  (appkit-surface-buffer
   (qq-guild--open-surface
    qq-guild-forum--type account (cons 'guild-forum (qq-guild--operation-key kind target))
    (list :kind kind :target target :title title)
    (if (eq kind 'feed)
        (concat title " · " (qq-guild-home--label (alist-get 'view target))) title))))

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
