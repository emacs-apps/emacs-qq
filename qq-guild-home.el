;;; qq-guild-home.el --- Guild-wide home feeds -*- lexical-binding: t; -*-

;;; Code:
(require 'qq-guild-forum)

(defconst qq-guild-home--views
  '(("all" . "全部") ("hot" . "热门") ("essence" . "精华")))

(defun qq-guild-home--label (view)
  "Return the display label for VIEW."
  (or (cdr (assoc view qq-guild-home--views)) (error "Unknown guild feed view: %s" view)))

(defun qq-guild-home-open (guild title account &optional view)
  "Open GUILD's home VIEW under ACCOUNT, retaining each view's reading position."
  (unless (qq-rpc-method-available-p "guild.feed")
    (user-error "当前网关未提供频道首页功能，请更新网关后重连"))
  (setq view (or view "all"))
  (qq-guild-home--label view)
  (qq-guild-forum--open 'feed (list (cons 'guild_id guild) (cons 'view view)) title account))

(defun qq-guild-home-select-view (view)
  "Select VIEW in the current guild home."
  (interactive
   (list (car (rassoc (completing-read "首页视图: " (mapcar #'cdr qq-guild-home--views) nil t)
                      qq-guild-home--views))))
  (let ((model (qq-guild-forum--model)))
    (unless (eq (plist-get model :kind) 'feed)
      (user-error "请从频道目录打开频道首页"))
    (qq-guild-home-open (alist-get 'guild_id (plist-get model :target))
                        (plist-get model :title) (plist-get model :account) view)))

(defun qq-guild-home-all ()
  "Select all posts in the guild home."
  (interactive)
  (qq-guild-home-select-view "all"))

(defun qq-guild-home-hot ()
  "Select hot posts in the guild home."
  (interactive)
  (qq-guild-home-select-view "hot"))

(defun qq-guild-home-essence ()
  "Select essence posts in the guild home."
  (interactive)
  (qq-guild-home-select-view "essence"))

(defun qq-guild-home--header (model)
  "Build clickable tabs retaining MODEL's account and guild ownership."
  (concat
   (plist-get model :title) " · 首页   "
   (mapconcat
    (lambda (entry)
      (let* ((view (car entry))
             (target (plist-get model :target))
             (guild (alist-get 'guild_id target))
             (title (plist-get model :title))
             (account (plist-get model :account))
             (active (equal view (alist-get 'view target)))
             (map (make-sparse-keymap))
             (number (1+ (seq-position qq-guild-home--views entry))))
        (define-key map [header-line mouse-1]
                    (lambda (event)
                      (interactive "e")
                      (let ((window (posn-window (event-start event))))
                        (when (windowp window) (select-window window)))
                      (qq-guild-home-open guild title account view)))
        (propertize (format "%s%s" (if active "● " "") (cdr entry))
                    'face (if active 'mode-line-emphasis 'shadow)
                    'mouse-face 'highlight
                    'help-echo (format "切换到%s（C-c %d）" (cdr entry) number)
                    'local-map map)))
    qq-guild-home--views "   ")
   "    C-c 1/2/3 切换"))

(provide 'qq-guild-home)
;;; qq-guild-home.el ends here
