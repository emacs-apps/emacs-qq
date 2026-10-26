;;; qq-guilds.el --- QQ Guild directory on Appkit -*- lexical-binding: t; -*-

;;; Commentary:
;; Reuses the hierarchical Directory design from the pre-v1-removal frontend.
;; Native directory data belongs to the account App; this Surface keeps only
;; its filter and request token.  Subchannels load when a Guild is expanded.

;;; Code:
(require 'qq-guild)
(require 'qq-guild-avatar)
(require 'qq-guild-channel)
(require 'appkit-directory)
(require 'appkit-bindings)

(defun qq-guilds--request (context model kind target)
  "Request KIND for TARGET and return a Directory transition."
  (let ((token (make-symbol "guild-directory")))
    (appkit-next
     :model (plist-put (copy-sequence model) :request token)
     :render (appkit-projection-change-create
              :full-p t)
     :commands (list (qq-guild--request-command context kind target nil token)))))

(defun qq-guilds--init (context input)
  "Initialize the directory from INPUT."
  (qq-guilds--request context input 'list nil))

(defun qq-guilds--update (context model message)
  "Handle directory navigation MESSAGE."
  (pcase message
    (`(refresh ,guild) (qq-guilds--request context model (if guild 'channels 'list) guild))
    (`(expand ,guild ,expanded)
     (let ((state (qq-guild--from-view (appkit-transition-context-app-read-view context))))
       (if (and expanded (not (qq-guild--get state :directories guild))
                (not (plist-get (qq-guild--get state :operations (list 'channels guild)) :pending)))
           (qq-guilds--request context model 'channels guild)
         (appkit-next
          :model model
          :render (appkit-projection-change-create
                   :full-p t)))))
    (`(filter ,query)
     (appkit-next
      :model (plist-put (copy-sequence model) :query query)
      :render (appkit-projection-change-create
               :full-p t)))
    (`(qq-guild-reply ,_kind ,token ,_value ,_reason)
     (appkit-next
      :model (if (eq token (plist-get model :request))
                 (plist-put (copy-sequence model) :request nil) model)
      :render (appkit-projection-change-create
               :full-p t)))
    (`(session ,phase)
     (if (equal phase "online") (qq-guilds--request context model 'list nil)
       (appkit-next
        :model (plist-put (copy-sequence model) :request nil)
        :render (appkit-projection-change-create
                 :full-p t))))
    ('changed (appkit-next
               :model model
               :render (appkit-projection-change-create
                        :full-p t)))
    ((pred appkit-projection-change-p) (appkit-next
                                        :model model
                                        :render message))
    ((pred appkit-delivery-report-p)
     (appkit-next
      :model (plist-put (copy-sequence model) :request nil)
      :render (appkit-projection-change-create
               :full-p t)))
    (_ (appkit-next-reject 'unknown-guild-directory-message))))

(defun qq-guilds--note (key label &optional section)
  "Create a stable note with KEY and LABEL under SECTION."
  (appkit-directory-entry-create
   :key key
   :role 'note
   :label label
   :section-key section
   :indent (if section 2 0)
   :face 'shadow))

(defun qq-guilds--project (state model)
  "Project account STATE through this directory MODEL."
  (let ((query (or (plist-get model :query) ""))
        (guilds (plist-get state :guilds))
        (list-op (qq-guild--get state :operations '(list nil)))
        entries)
    (when (or (plist-get list-op :pending) (plist-get list-op :error))
      (push (qq-guilds--note 'status (or (plist-get list-op :error) "正在加载频道目录…")) entries))
    (dolist (guild guilds)
      (let* ((id (alist-get 'guild_id guild))
             (key (list 'guild id))
             (name (alist-get 'name guild))
             (snapshot (qq-guild--get state :directories id))
             (channels (alist-get 'channels snapshot))
             (op (qq-guild--get state :operations (list 'channels id)))
             (guild-match (or (string-empty-p query)
                              (string-match-p (regexp-quote (downcase query)) (downcase name))))
             (matching (if guild-match channels
                         (seq-filter (lambda (item)
                                       (string-match-p (regexp-quote (downcase query))
                                                       (downcase (alist-get 'name item))))
                                     channels)))
             (expanded (appkit-directory-fold-expanded-p
                        (appkit-directory-surface) key nil (not (string-empty-p query)))))
        (when (or guild-match matching)
          (push (appkit-directory-entry-create
                 :key key
                 :role 'section
                 :label (concat (if (alist-get 'avatar_url guild)
                                    (concat (appkit-chat-avatar-one-line-string
                                             (qq-guild-avatar-image
                                              (qq-guild-avatar-key id nil (alist-get 'avatar_url guild))) "#") " ") "") name)
                 :face 'bold
                 :foldable-p t
                 :fold-key key
                 :fold-default-expanded-p nil
                 :expanded-p expanded
                 :fold-locked-reason (unless (string-empty-p query) "请先清除过滤条件")
                 :payload id
                 :properties (list 'qq-guild-id id
                                   'qq-guild-avatar-key
                                   (when (alist-get 'avatar_url guild)
                                     (qq-guild-avatar-key id nil (alist-get 'avatar_url guild))))) entries)
          (when expanded
            (cond
             ((plist-get op :error)
              (push (qq-guilds--note (list 'error id) (plist-get op :error) key) entries))
             ((or (plist-get op :pending) (null snapshot))
              (push (qq-guilds--note (list 'loading id)
                                     (if (plist-get op :pending) "正在加载子频道…"
                                       "子频道尚未加载；在频道上按 g 加载") key) entries)))
            (dolist (item matching)
              (let* ((route (alist-get 'channel item))
                     (channel-key (qq-guild--key route))
                     (cursor (qq-guild--get state :states channel-key))
                     (read (max (string-to-number (or (alist-get 'read_sequence cursor) "0"))
                                (string-to-number (or (qq-guild--get state :reads channel-key) "0"))))
                     (unread (and (= (alist-get 'channel_type item) 1) cursor (> (string-to-number (alist-get 'latest_sequence cursor)) read))))
                (push
                 (appkit-directory-entry-create
                  :key (cons 'channel channel-key)
                  :role 'item
                  :item-p t
                  :section-key key
                  :indent 2
                  :label (concat (qq-guild-channel-icon (alist-get 'channel_type item)) " " (alist-get 'name item)
                                 "  · " (qq-guild-channel-label (alist-get 'channel_type item)) (if unread "  · 未读" ""))
                  :unread-p unread
                  :payload (list route (concat name " · " (alist-get 'name item)) (alist-get 'channel_type item))
                  :help-echo (concat "打开" (qq-guild-channel-label (alist-get 'channel_type item)) "子频道")
                  :properties (list 'qq-guild-id id 'qq-guild-channel-id (alist-get 'channel_id route)))
                 entries)))
            (when (and snapshot (null matching) (not (plist-get op :pending)))
              (push (qq-guilds--note (list 'empty id) "没有匹配的子频道" key) entries))))))
    (unless entries
      (push (qq-guilds--note
             'empty (cond ((plist-get model :request) "正在加载频道目录…")
                          ((not (string-empty-p query)) "没有匹配的频道或已加载子频道")
                          (t "没有可见的 QQ 频道"))) entries))
    (nreverse entries)))

(defun qq-guilds--configure ()
  "Install native directory activation and fold callbacks."
  (appkit-directory-configure
   (appkit-directory-surface)
   :activate-function
   (lambda (_directory entry)
     (pcase-let ((`(,channel ,title ,type) (appkit-directory-entry-payload entry)))
       (qq-guild-channel-open channel title (plist-get (appkit-surface-model (appkit-current-surface)) :account) type)))
   :fold-function
   (lambda (_directory entry expanded)
     (appkit-surface-send (appkit-current-surface)
                          (list 'expand (appkit-directory-entry-payload entry) expanded)))))

(defun qq-guilds--render (_surface view model change)
  "Reconcile stable directory rows from committed VIEW and MODEL."
  (let* ((state (qq-guild--from-view view))
         (qq-guild-avatar--epoch (plist-get state :epoch))
         (entries (qq-guilds--project state model)))
    (setq-local header-line-format
                (concat "QQ 频道"
                        (when-let* ((query (plist-get model :query)) ((not (string-empty-p query))))
                          (concat " · 过滤：" query))))
    (appkit-directory-reconcile
     (appkit-directory-surface) entries
     :force-keys (and (appkit-projection-change-p change) (appkit-projection-change-keys change)))
    (qq-guild-avatar-result
     (when (display-graphic-p)
       (delq nil (mapcar
                  (lambda (entry)
                    (when-let* ((key (plist-get (appkit-directory-entry-properties entry) 'qq-guild-avatar-key)))
                      (cons (appkit-directory-entry-key entry) key)))
                  entries))))))

(defun qq-guilds--renderer (_surface)
  "Create the directory renderer with a fresh-cache recovery."
  (appkit-generated-renderer-create
   :unmount #'ignore
   :mount (lambda (surface _view model)
            (qq-runtime-bind-account (plist-get model :account))
            (setq-local qq-runtime--surface-owner surface)
            (qq-guilds--configure))
   :resource-request (lambda (keys) (appkit-projection-change-create :resources keys :full-p t))
   :merge #'appkit-projection-change-merge
   :render #'qq-guilds--render
   :recover (lambda (surface view model _condition)
              (let ((folds (appkit-directory-surface-fold-state (appkit-directory-surface))))
                (appkit-directory-initialize
                 :fold-state folds)
                (qq-guilds--configure)
                (qq-guilds--render surface view model nil)))))

(defun qq-guilds-refresh (&optional all)
  "Refresh the Guild at point, or the directory with prefix ALL."
  (interactive "P")
  (appkit-surface-send (appkit-current-surface)
                       (list 'refresh (unless all (get-text-property (point) 'qq-guild-id)))))

(defun qq-guilds-filter (query)
  "Filter the directory by QUERY, preserving the user's fold state."
  (interactive (list (read-string "过滤频道或已加载子频道: ")))
  (appkit-surface-send (appkit-current-surface) (list 'filter query)))

(defun qq-guilds-clear-filter ()
  "Clear the directory filter."
  (interactive)
  (qq-guilds-filter ""))

(defvar-keymap qq-guilds-mode-map
  :doc "QQ Guild directory actions."
  :parent appkit-directory-mode-map
  "/" #'qq-guilds-filter
  "C-c C-k" #'qq-guilds-clear-filter)

(appkit-bindings-define 'qq-guilds-mode-map 'read-only
                        'refresh #'qq-guilds-refresh 'filter-search #'qq-guilds-filter
                        'filter-cancel #'qq-guilds-clear-filter 'quit #'quit-window)

(define-derived-mode qq-guilds-mode appkit-directory-mode "QQ-Guilds"
  "Hierarchical account-owned QQ Guild directory."
  (setq-local revert-buffer-function (lambda (&rest _) (qq-guilds-refresh t))))

(defconst qq-guilds--type
  (appkit-surface-type-create
   :name 'qq-guilds
   :mode #'qq-guilds-mode
   :init #'qq-guilds--init
   :update #'qq-guilds--update
   :subscriptions (lambda (_model)
                    (list (appkit-subscription-create
                           :topic 'qq-guild-directory
                           :notify (lambda (_) 'changed))
                          (appkit-subscription-create
                           :topic 'qq-guild-session
                           :notify (lambda (phase) (list 'session phase)))))
   :renderer-factory #'qq-guilds--renderer))

;;;###autoload
(defun qq-guilds-open ()
  "Open or select the current account's Guild directory."
  (interactive)
  (appkit-surface-buffer
   (qq-guild--open-surface qq-guilds--type (qq-runtime-require-account-id "打开 QQ 频道")
                           '(guild-directory) nil "频道目录")))

(provide 'qq-guilds)
;;; qq-guilds.el ends here
