;;; qq-contacts.el --- Native QQ contacts and joined groups -*- lexical-binding: t; -*-

;; Author: 0WD0 <me@0wd0.com>

;;; Commentary:

;; A persistent, keyed-EWOC directory for the complete native friend-category
;; and joined-group snapshots.  The recent-session root remains a compact
;; activity view; this buffer is the authoritative place to find peers which
;; are absent from that recent snapshot.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'ewoc)
(require 'seq)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-ewoc)
(require 'appkit-projection)
(require 'appkit-position)
(require 'appkit-transaction)
(require 'appkit-ui)
(require 'appkit-presentation)
(require 'qq-core)
(require 'qq-media)
(require 'qq-runtime)
(require 'qq-state)

(autoload 'qq-chat-open "qq-chat" nil t)
(autoload 'qq-group-open "qq-group" nil t)
(autoload 'qq-root-open "qq-root" nil t)
(autoload 'qq-user-open "qq-user" nil t)

(declare-function qq-chat-open "qq-chat" (session-key))
(declare-function qq-group-open "qq-group" (group-id))
(declare-function qq-root-open "qq-root" ())
(declare-function qq-user-open "qq-user" (user-id))

(defgroup qq-contacts nil
  "Native friend and joined-group directory for emacs-qq."
  :group 'qq)

(defface qq-contacts-category
  '((t :inherit header-line :weight semi-bold :extend t))
  "Face for friend-category rows."
  :group 'qq-contacts)

(defface qq-contacts-section
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for directory section headings."
  :group 'qq-contacts)

(defface qq-contacts-navigation-button
  '((t :inherit mode-line-inactive :weight semi-bold
     :box (:line-width -1 :style released-button)))
  "Face for inactive directory navigation buttons."
  :group 'qq-contacts)

(defface qq-contacts-navigation-button-selected
  '((t :inherit mode-line-emphasis :weight bold
     :box (:line-width -1 :style pressed-button)))
  "Face for the selected directory navigation button."
  :group 'qq-contacts)

(defcustom qq-contacts-margin-columns 1
  "Columns reserved at the right edge of contact rows."
  :type 'integer
  :group 'qq-contacts)

(defconst qq-contacts-buffer-name "*qq-contacts*"
  "Name of the singleton QQ contacts buffer.")

(defconst qq-contacts--icon-slot-width 4
  "Width reserved for friend and group avatars.")

(cl-defstruct (qq-contacts--entry
               (:constructor qq-contacts--entry-create))
  key
  type
  object
  title
  count
  expanded
  view
  query
  width)

(defvar qq-contacts-search-history nil
  "Minibuffer history for native QQ directory searches.")

(defvar-local qq-contacts--ewoc nil)
(defvar-local qq-contacts--node-table nil)
(defvar-local qq-contacts--view 'friends)
(defvar-local qq-contacts--previous-view 'friends)
(defvar-local qq-contacts--query nil)
(defvar-local qq-contacts--collapsed-categories nil)
(defvar-local qq-contacts--fill-column nil)
(defvar-local qq-contacts--header-line-cache "")
(defvar-local qq-contacts--rendering nil)
(defvar-local qq-contacts--render-pending nil)
(defvar-local qq-contacts--dirty nil)
(defvar-local qq-contacts--pending-force-keys nil)
(defvar-local qq-contacts--loading nil)
(defvar-local qq-contacts--error nil)
(defvar-local qq-contacts--refresh-owner nil)
(defvar-local qq-contacts--refresh-pending 0)
(defvar-local qq-contacts--refresh-parts nil)
(defvar-local qq-contacts--friend-request nil)
(defvar-local qq-contacts--group-request nil)
(defvar-local qq-contacts--search-owner nil)
(defvar-local qq-contacts--search-pending nil)
(defvar-local qq-contacts--search-errors nil)
(defvar-local qq-contacts--search-members nil)
(defvar-local qq-contacts--member-group-id nil)
(defvar-local qq-contacts--search-member-request nil)

(defconst qq-contacts--view-id 'contacts
  "Appkit identity of the singleton contacts directory view.")

(defun qq-contacts--present-string (value)
  "Return non-empty string VALUE, or nil."
  (and (stringp value) (not (string-empty-p value)) value))

(defun qq-contacts--friend-name (friend)
  "Return the best display name for FRIEND."
  (or (qq-contacts--present-string (alist-get 'remark friend))
      (qq-contacts--present-string (alist-get 'nickname friend))
      (alist-get 'user_id friend)))

(defun qq-contacts--group-name (group)
  "Return the best display name for GROUP."
  (or (qq-contacts--present-string (alist-get 'group_remark group))
      (qq-contacts--present-string (alist-get 'group_name group))
      (qq-contacts--present-string (alist-get 'remark group))
      (qq-contacts--present-string (alist-get 'name group))
      (alist-get 'group_id group)))

(defun qq-contacts--navigation-entry ()
  "Return the navigation entry for the current view."
  (qq-contacts--entry-create
   :key 'navigation
   :type 'navigation
   :view qq-contacts--view
   :query qq-contacts--query
   :width qq-contacts--fill-column))

(defun qq-contacts--category-entry (category)
  "Return one friend CATEGORY entry."
  (let ((category-id (alist-get 'category_id category)))
    (qq-contacts--entry-create
     :key (cons 'category category-id)
     :type 'category
     :object category
     :title (alist-get 'name category)
     :count (length (alist-get 'friends category))
     :expanded (not (gethash category-id qq-contacts--collapsed-categories))
     :width qq-contacts--fill-column)))

(defun qq-contacts--friend-entry (friend)
  "Return one FRIEND entry."
  (qq-contacts--entry-create
   :key (cons 'friend (alist-get 'user_id friend))
   :type 'friend
   :object friend
   :width qq-contacts--fill-column))

(defun qq-contacts--group-entry (group)
  "Return one joined GROUP entry."
  (qq-contacts--entry-create
   :key (cons 'group (alist-get 'group_id group))
   :type 'group
   :object group
   :width qq-contacts--fill-column))

(defun qq-contacts--member-entry (member)
  "Return one native group MEMBER search entry."
  (qq-contacts--entry-create
   :key (cons 'member (alist-get 'user_id member))
   :type 'member
   :object member
   :width qq-contacts--fill-column))

(defun qq-contacts--section-entry (key title count)
  "Return section KEY with TITLE and COUNT."
  (qq-contacts--entry-create
   :key (cons 'section key)
   :type 'section
   :title title
   :count count
   :width qq-contacts--fill-column))

(defun qq-contacts--note-entry (key title &optional type)
  "Return status note KEY with TITLE and optional TYPE."
  (qq-contacts--entry-create
   :key (cons 'note key)
   :type (or type 'note)
   :title title
   :width qq-contacts--fill-column))

(defun qq-contacts--search-error (kind)
  "Return the current native search error for KIND."
  (alist-get kind qq-contacts--search-errors))

(defun qq-contacts--group-recent-p (group)
  "Return non-nil when GROUP has an entry in the recent-session snapshot."
  (qq-state-session-recent-p
   (qq-state-session-key 'group (alist-get 'group_id group))))

(defun qq-contacts--project-friends ()
  "Project the ordered friend-category view."
  (let ((categories (qq-state-friend-categories))
        entries)
    (push (qq-contacts--navigation-entry) entries)
    (dolist (category categories)
      (let ((category-entry (qq-contacts--category-entry category)))
        (push category-entry entries)
        (when (qq-contacts--entry-expanded category-entry)
          (dolist (friend (alist-get 'friends category))
            (push (qq-contacts--friend-entry friend) entries)))))
    (unless categories
      (push (qq-contacts--note-entry
             'empty-friends
             (cond
              (qq-contacts--loading "正在加载好友分组…")
              ((not (qq-state-friend-categories-loaded-p))
               "好友分组尚未加载。")
              (t "好友分组为空。")))
            entries))
    (nreverse entries)))

(defun qq-contacts--project-groups (&optional not-recent-p)
  "Project joined groups, restricted to non-recent groups when NOT-RECENT-P."
  (let* ((all-groups (qq-state-groups))
         (groups (if not-recent-p
                     (seq-remove #'qq-contacts--group-recent-p all-groups)
                   all-groups))
         entries)
    (push (qq-contacts--navigation-entry) entries)
    (dolist (group groups)
      (push (qq-contacts--group-entry group) entries))
    (unless groups
      (push (qq-contacts--note-entry
             (if not-recent-p 'empty-not-recent 'empty-groups)
             (cond
              (qq-contacts--loading "正在加载群列表…")
              ((not (qq-state-groups-loaded-p)) "群列表尚未加载。")
              (not-recent-p "所有已加入群都出现在当前近期会话快照中。")
              (t "已加入群列表为空。")))
            entries))
    (nreverse entries)))

(defun qq-contacts--project-members ()
  "Project exact native group-member search results."
  (let (entries)
    (push (qq-contacts--navigation-entry) entries)
    (when qq-contacts--search-members
      (push (qq-contacts--section-entry
             'members "群成员" (length qq-contacts--search-members))
            entries)
      (dolist (member qq-contacts--search-members)
        (push (qq-contacts--member-entry member) entries)))
    (when (memq 'members qq-contacts--search-pending)
      (push (qq-contacts--note-entry 'searching-members "正在搜索群成员…")
            entries))
    (when-let* ((reason (qq-contacts--search-error 'members)))
      (push (qq-contacts--note-entry 'search-members-error reason 'error-note)
            entries))
    (unless (or qq-contacts--search-members
                qq-contacts--search-pending
                qq-contacts--search-errors)
      (push (qq-contacts--note-entry
             'empty-member-search
             (format "群 %s 中没有与 “%s” 匹配的成员。"
                     (or qq-contacts--member-group-id "")
                     (or qq-contacts--query "")))
            entries))
    (nreverse entries)))

(defun qq-contacts--project-entries ()
  "Return the current ordered directory projection."
  (let ((entries
         (pcase qq-contacts--view
           ('friends (qq-contacts--project-friends))
           ('groups (qq-contacts--project-groups))
           ('not-recent (qq-contacts--project-groups t))
           ('members (qq-contacts--project-members))
           (_ (error "qq: unknown contacts view %S" qq-contacts--view)))))
    (when qq-contacts--error
      (setq entries
            (append entries
                    (list (qq-contacts--note-entry
                           'refresh-error qq-contacts--error 'error-note)))))
    entries))

(defun qq-contacts--selected-navigation-face (view)
  "Return navigation face for VIEW."
  (if (eq qq-contacts--view view)
      'qq-contacts-navigation-button-selected
    'qq-contacts-navigation-button))

(defun qq-contacts--insert-navigation-button (label view action help)
  "Insert navigation LABEL for VIEW, invoking ACTION with HELP text."
  (appkit-ui-insert-action-button
   label action
   :face (qq-contacts--selected-navigation-face view)
   :help-echo help))

(defun qq-contacts--insert-navigation (_entry)
  "Insert the directory navigation row."
  (insert " ")
  (qq-contacts--insert-navigation-button
   " 好友分组 " 'friends #'qq-contacts-show-friends "显示完整好友分组 (f)")
  (insert "  ")
  (qq-contacts--insert-navigation-button
   " 全部群 " 'groups #'qq-contacts-show-groups "显示所有已加入群 (G)")
  (insert "  ")
  (qq-contacts--insert-navigation-button
   " 未在近期 " 'not-recent #'qq-contacts-show-not-recent-groups
   "显示未出现在当前近期会话快照中的群 (I)")
  (insert "  ")
  (appkit-ui-insert-action-button
   " 刷新 " #'qq-contacts-refresh
   :face 'qq-contacts-navigation-button
   :help-echo "从 Linux QQ 刷新精确通讯录快照 (g)")
  (insert "\n\n"))

(defun qq-contacts--insert-category (entry)
  "Insert friend-category ENTRY."
  (let ((start (point))
        (category-id (alist-get 'category_id (qq-contacts--entry-object entry))))
    (insert " " (if (qq-contacts--entry-expanded entry) "▾" "▸") " "
            (or (qq-contacts--entry-title entry) "未命名分组")
            (format "  (%d)\n" (or (qq-contacts--entry-count entry) 0)))
    (add-text-properties
     start (point)
     (list 'face 'qq-contacts-category
           'qq-contacts-key (qq-contacts--entry-key entry)
           'qq-contacts-row-type 'category
           'qq-contacts-category-id category-id))
    (appkit-ui-make-action-row
     start (point) entry #'qq-contacts--activate-entry
     :help-echo "mouse-1 or RET: 折叠/展开好友分组"
     :mouse-face 'highlight)))

(defun qq-contacts--friend-preview (friend)
  "Return secondary line text for FRIEND."
  (let ((nickname (qq-contacts--present-string (alist-get 'nickname friend)))
        (remark (qq-contacts--present-string (alist-get 'remark friend)))
        (qid (qq-contacts--present-string (alist-get 'qid friend)))
        (category (qq-contacts--present-string
                   (alist-get 'category_name friend)))
        (user-id (alist-get 'user_id friend)))
    (string-join
     (delq nil
           (list (and nickname (not (equal nickname remark)) nickname)
                 (and qid (format "QID %s" qid))
                 (format "QQ %s" user-id)
                 category))
     " · ")))

(defun qq-contacts--insert-friend (entry)
  "Insert actionable friend ENTRY."
  (let* ((friend (qq-contacts--entry-object entry))
         (user-id (alist-get 'user_id friend))
         (start (point)))
    (appkit-presentation-insert-one-line-row
     (appkit-presentation-one-line-row-create
      :icon-inserter (lambda ()
                       (insert
                        (qq-media-avatar-cached-display-string user-id)))
      :context (qq-contacts--friend-name friend) :preview (appkit-ui-one-line-preview-create :text (qq-contacts--friend-preview friend)) :line-properties
      (list 'qq-contacts-key (qq-contacts--entry-key entry)
            'qq-contacts-row-type 'friend
            'qq-contacts-object friend
            'qq-contacts-item-id user-id)
      :help-echo "mouse-1 or RET: 打开私聊")
     :indent 2
     :width (or (qq-contacts--entry-width entry) 80)
     :icon-slot-width qq-contacts--icon-slot-width
     :context-width-spec '(0.45 18 42))
    (appkit-ui-make-action-row
     start (point) entry #'qq-contacts--activate-entry
     :help-echo "mouse-1 or RET: 打开私聊"
     :mouse-face 'highlight)))

(defun qq-contacts--permission-label (permission)
  "Return concise label for native group PERMISSION."
  (pcase permission
    ("owner" "群主")
    ("admin" "管理员")
    ("member" "成员")
    (_ "")))

(defun qq-contacts--search-hit-label (hits)
  "Return a concise description of non-empty group-search HITS."
  (let (labels)
    (dolist (spec '((group_id . "群号") (name . "群名") (remark . "备注")))
      (when (alist-get (car spec) hits)
        (push (cdr spec) labels)))
    (when labels
      (format "命中%s" (string-join (nreverse labels) "、")))))

(defun qq-contacts--matched-label (prefix values field)
  "Return PREFIX plus up to three non-empty FIELD values from VALUES."
  (let ((names
         (seq-take
          (delq nil
                (mapcar (lambda (value)
                          (qq-contacts--present-string
                           (alist-get field value)))
                        values))
          3)))
    (when names
      (format "%s%s" prefix (string-join names "、")))))

(defun qq-contacts--group-preview (group)
  "Return secondary line text for GROUP."
  (let ((name (qq-contacts--present-string (alist-get 'group_name group)))
        (remark (qq-contacts--present-string (alist-get 'group_remark group)))
        (group-id (alist-get 'group_id group))
        (member-count (alist-get 'member_count group))
        (matched-members (alist-get 'matched_members group))
        (matched-discussions (alist-get 'matched_discussions group))
        (matched-member-cards (alist-get 'matched_member_cards group))
        (reason (qq-contacts--present-string
                 (alist-get 'recall_reason group))))
    (string-join
     (delq nil
           (list (and name (not (equal name remark)) name)
                 (format "群 %s" group-id)
                 (and (integerp member-count)
                      (format "%d 位成员" member-count))
                 (qq-contacts--search-hit-label
                  (alist-get 'search_hits group))
                 (qq-contacts--matched-label
                  "命中讨论组 " matched-discussions 'name)
                 (when matched-members
                   (format
                    "命中成员 %s"
                    (string-join
                     (seq-take
                      (mapcar
                       (lambda (member)
                         (or (qq-contacts--present-string
                              (alist-get 'card member))
                             (qq-contacts--present-string
                              (alist-get 'remark member))
                             (qq-contacts--present-string
                              (alist-get 'nickname member))
                             (alist-get 'user_id member)))
                       matched-members)
                      3)
                     "、")))
                 (qq-contacts--matched-label
                  "命中群名片 " matched-member-cards 'card)
                 (and reason (format "匹配原因 %s" reason))))
     " · ")))

(defun qq-contacts--insert-group (entry)
  "Insert actionable joined-group ENTRY."
  (let* ((group (qq-contacts--entry-object entry))
         (group-id (alist-get 'group_id group))
         (start (point)))
    (appkit-presentation-insert-one-line-row
     (appkit-presentation-one-line-row-create
      :icon-inserter (lambda ()
                       (insert
                        (qq-media-group-avatar-cached-display-string group-id)))
      :context (qq-contacts--group-name group)
      :context-trail (qq-contacts--permission-label
                      (alist-get 'self_permission group))
      :context-trail-face 'shadow :preview (appkit-ui-one-line-preview-create :text (qq-contacts--group-preview group)) :line-properties
      (list 'qq-contacts-key (qq-contacts--entry-key entry)
            'qq-contacts-row-type 'group
            'qq-contacts-object group
            'qq-contacts-item-id group-id)
      :help-echo "mouse-1 or RET: 打开群聊")
     :indent 2
     :width (or (qq-contacts--entry-width entry) 80)
     :icon-slot-width qq-contacts--icon-slot-width
     :context-width-spec '(0.45 18 42))
    (appkit-ui-make-action-row
     start (point) entry #'qq-contacts--activate-entry
     :help-echo "mouse-1 or RET: 打开群聊"
     :mouse-face 'highlight)))

(defun qq-contacts--member-name (member)
  "Return the best exact display name for group MEMBER."
  (or (qq-contacts--present-string (alist-get 'card member))
      (qq-contacts--present-string (alist-get 'remark member))
      (qq-contacts--present-string (alist-get 'nickname member))
      (alist-get 'user_id member)))

(defun qq-contacts--member-preview (member)
  "Return secondary line text for native group MEMBER."
  (let ((card (qq-contacts--present-string (alist-get 'card member)))
        (remark (qq-contacts--present-string (alist-get 'remark member)))
        (nickname (qq-contacts--present-string (alist-get 'nickname member)))
        (title (qq-contacts--present-string (alist-get 'title member))))
    (string-join
     (delq nil
           (list (and nickname (not (equal nickname card)) nickname)
                 (and remark (not (member remark (list card nickname))) remark)
                 (and title (format "头衔 %s" title))
                 (format "QQ %s" (alist-get 'user_id member))
                 (and (eq (alist-get 'is_friend member) t) "好友")))
     " · ")))

(defun qq-contacts--insert-member (entry)
  "Insert actionable native group MEMBER search ENTRY."
  (let* ((member (qq-contacts--entry-object entry))
         (user-id (alist-get 'user_id member))
         (start (point)))
    (appkit-presentation-insert-one-line-row
     (appkit-presentation-one-line-row-create
      :icon-inserter (lambda ()
                       (insert
                        (qq-media-avatar-cached-display-string user-id)))
      :context (qq-contacts--member-name member)
      :context-trail (qq-contacts--present-string
                      (alist-get 'group_name member))
      :context-trail-face 'shadow :preview (appkit-ui-one-line-preview-create :text (qq-contacts--member-preview member)) :line-properties
      (list 'qq-contacts-key (qq-contacts--entry-key entry)
            'qq-contacts-row-type 'member
            'qq-contacts-object member
            'qq-contacts-item-id user-id)
      :help-echo "RET: 打开私聊 · C: 修改群名片 · T: 修改专属头衔 · K: 移出群聊")
     :indent 2
     :width (or (qq-contacts--entry-width entry) 80)
     :icon-slot-width qq-contacts--icon-slot-width
     :context-width-spec '(0.45 18 42))
    (appkit-ui-make-action-row
     start (point) entry #'qq-contacts--activate-entry
     :help-echo "RET: 打开私聊 · C: 修改群名片 · T: 修改专属头衔 · K: 移出群聊"
     :mouse-face 'highlight)))

(defun qq-contacts--insert-section (entry)
  "Insert search section ENTRY."
  (let ((start (point)))
    (insert (format "%s  (%d)\n"
                    (or (qq-contacts--entry-title entry) "")
                    (or (qq-contacts--entry-count entry) 0)))
    (add-text-properties
     start (point)
     (list 'face 'qq-contacts-section
           'qq-contacts-key (qq-contacts--entry-key entry)))))

(defun qq-contacts--insert-note (entry)
  "Insert status note ENTRY."
  (appkit-presentation-insert-note-line
   (or (qq-contacts--entry-title entry) "")
   :face (if (eq (qq-contacts--entry-type entry) 'error-note)
             'error
           'shadow)
   :line-properties (list 'qq-contacts-key (qq-contacts--entry-key entry))))

(defun qq-contacts--ewoc-printer (entry)
  "Insert one directory ENTRY."
  (pcase (qq-contacts--entry-type entry)
    ('navigation (qq-contacts--insert-navigation entry))
    ('category (qq-contacts--insert-category entry))
    ('friend (qq-contacts--insert-friend entry))
    ('group (qq-contacts--insert-group entry))
    ('member (qq-contacts--insert-member entry))
    ('section (qq-contacts--insert-section entry))
    ((or 'note 'error-note) (qq-contacts--insert-note entry))
    (type (error "qq: unknown contacts entry type %S" type))))

(defun qq-contacts--usable-width ()
  "Return current directory row width."
  (or (when-let* ((widths
                   (delq nil
                         (mapcar
                          (lambda (window)
                            (appkit-geometry-window-width
                             window qq-contacts-margin-columns))
                          (get-buffer-window-list (current-buffer) nil t)))))
        (apply #'min widths))
      qq-contacts--fill-column
      80))

(defun qq-contacts--view-label ()
  "Return human-readable label for the selected directory view."
  (pcase qq-contacts--view
    ('friends "好友分组")
    ('groups "全部已加入群")
    ('not-recent "未在近期会话中的群")
    ('members (format "群 %s 的成员 · “%s”"
                      (or qq-contacts--member-group-id "")
                      (or qq-contacts--query "")))
    (_ "通讯录")))

(defun qq-contacts--refresh-header-line ()
  "Refresh cached directory header text."
  (setq qq-contacts--header-line-cache
        (format " QQ 通讯录 · %s  %d 位好友 · %d 个群%s"
                (qq-contacts--view-label)
                (qq-state-friend-count)
                (qq-state-group-count)
                (if qq-contacts--loading " · 正在刷新" "")))
  (force-mode-line-update))

(defun qq-contacts--queue-force-keys (keys)
  "Retain stable row KEYS until their next successful redraw."
  (dolist (key keys)
    (cl-pushnew key qq-contacts--pending-force-keys :test #'equal)))

(defun qq-contacts--invalidate-keys (keys)
  "Redraw existing EWOC rows identified by stable KEYS."
  (let ((snapshot
         (appkit-position-capture
          :anchor-property 'qq-contacts-key
          :preserve-window-start t))
        succeeded)
    (unwind-protect
        (progn
          (let ((inhibit-read-only t)
                (buffer-undo-list t))
            (with-silent-modifications
              (dolist (key keys)
                (appkit-ewoc-invalidate-key
                 qq-contacts--ewoc qq-contacts--node-table key))))
          (setq succeeded t))
      (when snapshot
        (ignore-errors (appkit-position-restore snapshot)))
      (unless succeeded
        (qq-contacts--queue-force-keys keys)
        (setq qq-contacts--dirty t)))))

(defun qq-contacts--reconcile (&optional force-keys)
  "Reconcile the persistent directory, forcing FORCE-KEYS."
  (qq-contacts--queue-force-keys force-keys)
  (if qq-contacts--rendering
      (setq qq-contacts--render-pending t)
    (let ((qq-contacts--rendering t)
          (forced qq-contacts--pending-force-keys)
          succeeded
          (snapshot
           (appkit-position-capture
            :anchor-property 'qq-contacts-key
            :preserve-window-start t)))
      (setq qq-contacts--pending-force-keys nil)
      (unwind-protect
          (progn
            (setq qq-contacts--fill-column (qq-contacts--usable-width))
            (let ((inhibit-read-only t)
                  (buffer-undo-list t))
              (with-silent-modifications
                (setq qq-contacts--node-table
                      (appkit-ewoc-reconcile
                       qq-contacts--ewoc
                       (qq-contacts--project-entries)
                       #'qq-contacts--entry-key
                       :force-keys forced))))
            (setq qq-contacts--dirty nil)
            (qq-contacts--refresh-header-line)
            (dolist (window (get-buffer-window-list (current-buffer) nil t))
              (qq-contacts--ensure-window-avatars window))
            (setq succeeded t))
        (when snapshot
          (ignore-errors (appkit-position-restore snapshot)))
        (unless succeeded
          (qq-contacts--queue-force-keys forced)
          (setq qq-contacts--dirty t))
        (setq qq-contacts--rendering nil))
      (when qq-contacts--render-pending
        (setq qq-contacts--render-pending nil)
        (qq-contacts--reconcile)))))

(defun qq-contacts--displayed-p ()
  "Return non-nil when the directory has a live display window."
  (window-live-p (get-buffer-window (current-buffer) t)))

(defun qq-contacts--view-current-p (view buffer)
  "Return non-nil when VIEW still owns contacts BUFFER."
  (and (buffer-live-p buffer)
       (appkit-surface-live-p view)
       (eq (appkit-surface-buffer view) buffer)
       (equal qq-contacts--view-id (appkit-surface-identity view))
       (with-current-buffer buffer
         (and (derived-mode-p 'qq-contacts-mode)
              (eq view (appkit-current-surface))))))

(defun qq-contacts--live-current-view ()
  "Return this buffer's live contacts view without creating one."
  (let ((view (appkit-current-surface)))
    (and (qq-contacts--view-current-p view (current-buffer)) view)))

(defun qq-contacts--live-view (&optional account-id)
  "Return ACCOUNT-ID's existing live Appkit contacts view, or nil.

ACCOUNT-ID defaults to the current UI context.  This lookup deliberately
does not create an account runtime.  State and
media hooks must not start a QQ application merely to discover that the
directory is closed, and the registered view remains authoritative when its
owning buffer has been renamed."
  (when-let* ((owner (or account-id (qq-runtime-current-account-id)))
              (runtime (qq-runtime-account owner))
              (app (qq-runtime-account-app runtime))
              (view (appkit-app-surface app qq-contacts--view-id)))
    (and (qq-contacts--view-current-p view (appkit-surface-buffer view)) view)))

(defun qq-contacts--live-views ()
  "Return every live account-scoped contacts view."
  (delq nil
        (mapcar
         (lambda (runtime)
           (qq-contacts--live-view (qq-runtime-account-id runtime)))
         (qq-runtime-accounts))))

(defun qq-contacts--ensure-window-avatars (window)
  "Start avatar work only for directory rows visible in WINDOW."
  (when (and (qq-contacts--live-current-view)
             (window-live-p window)
             (eq (window-buffer window) (current-buffer)))
    (let ((position (window-start window))
          (limit (or (window-end window t) (point-max)))
          (seen (make-hash-table :test #'equal)))
      (while (< position limit)
        (let* ((row-type (get-text-property position 'qq-contacts-row-type))
               (object (get-text-property position 'qq-contacts-object))
               (identity
                (pcase row-type
                  ((or 'friend 'member)
                   (alist-get 'user_id object))
                  ('group (alist-get 'group_id object)))))
          (when (and identity (not (gethash (cons row-type identity) seen)))
            (puthash (cons row-type identity) t seen)
            (condition-case error-data
                (pcase row-type
                  ((or 'friend 'member)
                   (qq-media-avatar-image identity))
                  ('group (qq-media-group-avatar-image identity)))
              (error
               (message "qq: failed to prepare directory avatar %s: %s"
                        identity (error-message-string error-data)))))
          (setq position
                (next-single-property-change
                 position 'qq-contacts-row-type nil limit)))))))

(defun qq-contacts--window-scroll (window _display-start)
  "Start media for newly visible directory rows after WINDOW scrolls."
  (when (and (window-live-p window)
             (eq (window-buffer window) (current-buffer)))
    (qq-contacts--ensure-window-avatars window)))

(defun qq-contacts--ensure-view ()
  "Return the live Appkit view owning the current contacts buffer."
  (qq-runtime-ensure-account-surface
   :id qq-contacts--view-id
   :mode 'qq-contacts-mode
   :render-function #'qq-contacts--render
   :setup #'qq-contacts--setup-view))

(defun qq-contacts--reset-buffer-work (buffer)
  "Reset requests and account-scoped view state retained by BUFFER.

This is a view-lifecycle boundary, not ordinary live-view reuse.  Member
queries and results must not survive attachment to a replacement runtime."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (derived-mode-p 'qq-contacts-mode)
        (unwind-protect
            (progn
              (qq-contacts--cancel-refresh)
              (qq-contacts--cancel-search))
          ;; State release is unconditional: transport cancellation may signal,
          ;; while Appkit still has to make a replacement runtime account-clean.
          (qq-contacts--clear-search-results)
          (setq qq-contacts--query nil
                qq-contacts--view 'friends
                qq-contacts--previous-view 'friends
                qq-contacts--loading nil
                qq-contacts--error nil
                qq-contacts--refresh-owner nil
                qq-contacts--refresh-pending 0
                qq-contacts--refresh-parts nil
                qq-contacts--friend-request nil
                qq-contacts--group-request nil
                qq-contacts--search-owner nil
                qq-contacts--search-pending nil
                qq-contacts--search-member-request nil))))))

(defun qq-contacts--release-view-work (view buffer)
  "Release BUFFER work while it is still owned by contacts VIEW."
  (when (and (buffer-live-p buffer)
             (with-current-buffer buffer
               (eq view qq-runtime--surface-owner)))
    (qq-contacts--reset-buffer-work buffer)))

(defun qq-contacts--setup-view (view)
  "Register exact-view lifecycle cleanup for newly attached contacts VIEW."
  (let ((buffer (appkit-surface-buffer view)))
    (qq-contacts--reset-buffer-work buffer)
    (appkit-register-handle
     view 'function
     (apply-partially #'qq-contacts--release-view-work view buffer))))

(defun qq-contacts--request-reconcile (&optional force-keys)
  "Render the directory, forcing presentation rows FORCE-KEYS when non-nil."
  (appkit-surface-send
   (qq-contacts--ensure-view)
   (list 'qq-render
         (if force-keys
             (appkit-projection-change-create :keys force-keys)
           (appkit-projection-change-create :full-p t)))))

(defun qq-contacts--render (surface _model change)
  "Render native projection CHANGE in contacts SURFACE."
  (when (appkit-surface-live-p surface)
    (let* ((full-p (or (appkit-projection-change-full-p change)
                       (appkit-projection-change-geometry-p change)
                       (appkit-projection-change-frame-p change)))
           (force-keys
            (if (and full-p (appkit-projection-change-resources change))
                (hash-table-keys qq-contacts--node-table)
              (appkit-projection-change-keys change))))
      (when (or full-p force-keys)
        (if (qq-contacts--displayed-p)
            (appkit-with-content-update surface
              (let ((width (qq-contacts--usable-width)))
                (if (and (not full-p)
                         force-keys
                         (not qq-contacts--dirty)
                         (not qq-contacts--rendering)
                         (= width (or qq-contacts--fill-column 0)))
                    (qq-contacts--invalidate-keys force-keys)
                  (qq-contacts--reconcile force-keys))))
          (qq-contacts--queue-force-keys force-keys)
          (setq qq-contacts--dirty t)))))
  nil)

(defun qq-contacts--window-buffer-change (window)
  "Flush deferred updates when WINDOW displays the directory."
  (when-let* ((view (qq-contacts--live-current-view)))
    (when (and (window-live-p window)
               (eq (window-buffer window) (current-buffer)))
      (let ((width (qq-contacts--usable-width)))
        (when (or qq-contacts--dirty
                  qq-contacts--pending-force-keys
                  (/= width (or qq-contacts--fill-column 0)))
          (appkit-surface-send view (list 'qq-render (appkit-projection-change-create :full-p t))))))))

(defun qq-contacts--window-size-change (&optional _frame)
  "Reflow this directory after a visible window size change."
  (when-let* ((view (qq-contacts--live-current-view)))
    (when (qq-contacts--displayed-p)
      (let ((width (qq-contacts--usable-width)))
        (when (/= width (or qq-contacts--fill-column 0))
          (appkit-surface-send view (list 'qq-render (appkit-projection-change-create :full-p t))))))))

(defun qq-contacts--set-view (view)
  "Select directory VIEW and reconcile."
  (qq-contacts--ensure-view)
  (when (and (eq qq-contacts--view 'members)
             (not (eq view 'members)))
    (qq-contacts--cancel-search)
    (qq-contacts--clear-search-results))
  (unless (eq view 'members)
    (setq qq-contacts--query nil))
  (setq qq-contacts--view view)
  (qq-contacts--request-reconcile))

(defun qq-contacts-show-friends ()
  "Show authoritative friend categories."
  (interactive)
  (qq-contacts--set-view 'friends))

(defun qq-contacts-show-groups ()
  "Show all authoritative joined groups."
  (interactive)
  (qq-contacts--set-view 'groups))

(defun qq-contacts-show-not-recent-groups ()
  "Show joined groups absent from the current recent-session snapshot."
  (interactive)
  (qq-contacts--set-view 'not-recent))

(defun qq-contacts--clear-search-results ()
  "Clear the current group-member search projection."
  (setq qq-contacts--search-errors nil
        qq-contacts--search-members nil
        qq-contacts--member-group-id nil))

(defun qq-contacts--cancel-search ()
  "Cancel the current native group-member search."
  (setq qq-contacts--search-owner nil
        qq-contacts--search-pending nil)
  (when qq-contacts--search-member-request
    (qq-request-cancel qq-contacts--search-member-request))
  (setq qq-contacts--search-member-request nil))

(defun qq-contacts--search-current-p (view buffer owner kind)
  "Return non-nil when VIEW and OWNER still own search KIND in BUFFER."
  (and (qq-contacts--view-current-p view buffer)
       (with-current-buffer buffer
         (and (eq owner qq-contacts--search-owner)
              (memq kind qq-contacts--search-pending)))))

(defun qq-contacts--finish-search-page
    (view buffer owner kind _append-p page)
  "Apply native group-member search PAGE owned by VIEW and OWNER."
  (unless (eq kind 'members)
    (error "qq: unsupported directory search result kind %S" kind))
  (when (qq-contacts--search-current-p view buffer owner kind)
    (with-current-buffer buffer
      (let ((items (alist-get 'results page)))
        (unless (proper-list-p items)
          (error "qq: group-member search result must be a list"))
        (setq qq-contacts--search-members (copy-tree items)
              qq-contacts--search-member-request nil
              qq-contacts--search-errors
              (assq-delete-all kind qq-contacts--search-errors)
              qq-contacts--search-pending
              (delq kind qq-contacts--search-pending))
        (appkit-surface-send view (list 'qq-render (appkit-projection-change-create :full-p t)))))))

(defun qq-contacts--fail-search-page
    (view buffer owner kind _response reason)
  "Record group-member search failure REASON for VIEW and OWNER."
  (unless (eq kind 'members)
    (error "qq: unsupported directory search failure kind %S" kind))
  (when (qq-contacts--search-current-p view buffer owner kind)
    (with-current-buffer buffer
      (setq qq-contacts--search-member-request nil
            qq-contacts--search-errors
            (cons (cons kind reason)
                  (assq-delete-all kind qq-contacts--search-errors))
            qq-contacts--search-pending
            (delq kind qq-contacts--search-pending))
      (appkit-surface-send view (list 'qq-render (appkit-projection-change-create :full-p t))))))

(defun qq-contacts--issue-search-request (view kind cursor append-p)
  "Issue one native group-member search through captured VIEW."
  (unless (eq kind 'members)
    (error "qq: unsupported directory search kind %S" kind))
  (when (or cursor append-p)
    (error "qq: native group-member search has no continuation"))
  (let* ((buffer (current-buffer))
         (owner qq-contacts--search-owner)
         (success
          (apply-partially #'qq-contacts--finish-search-page
                           view buffer owner kind nil))
         (failure
          (apply-partially #'qq-contacts--fail-search-page
                           view buffer owner kind)))
    (unless (qq-contacts--search-current-p view buffer owner kind)
      (error "QQ: directory search lost its dispatch view"))
    (condition-case error-data
        (let ((request
                (qq-core-search-group-members
                 qq-contacts--member-group-id qq-contacts--query
                 (lambda (members)
                   (funcall success `((results . ,members) (next_cursor))))
                 failure)))
          (when (qq-contacts--search-current-p view buffer owner kind)
            (setq qq-contacts--search-member-request request)))
      (error
       (qq-contacts--fail-search-page
        view buffer owner kind nil (error-message-string error-data))))))

;;;###autoload
(defun qq-contacts-search-group-members (group-id query)
  "Open exact native member search for GROUP-ID and QUERY."
  (interactive
   (list (read-string "群号: ")
         (read-string "搜索群成员: " nil 'qq-contacts-search-history)))
  (unless (qq-protocol-group-uin-p group-id)
    (user-error "qq: group member search requires an exact backend group id"))
  (setq query (string-trim (or query "")))
  (when (string-empty-p query)
    (user-error "qq: group member search query cannot be empty"))
  (let* ((view
          (or (qq-contacts--live-view)
              ;; The canonical open path initializes mode and lifecycle before
              ;; we capture the exact view used by all member-search callbacks.
              (let ((buffer (qq-contacts-open)))
                (with-current-buffer buffer
                  (qq-contacts--ensure-view)))))
         (buffer (appkit-surface-buffer view)))
    (with-current-buffer buffer
      (unless (eq qq-contacts--view 'members)
        (setq qq-contacts--previous-view qq-contacts--view))
      (qq-contacts--cancel-search)
      (qq-contacts--clear-search-results)
      (setq qq-contacts--view 'members
            qq-contacts--query query
            qq-contacts--member-group-id group-id
            qq-contacts--search-owner
            (list 'native-group-member-search group-id query)
            qq-contacts--search-pending '(members))
      (appkit-surface-send view (list 'qq-render (appkit-projection-change-create :full-p t)))
      (qq-contacts--issue-search-request view 'members nil nil))
    (pop-to-buffer buffer)
    buffer))

(defun qq-contacts-clear-search ()
  "Clear group-member search and restore the previous directory view."
  (interactive)
  (when (eq qq-contacts--view 'members)
    (qq-contacts--cancel-search)
    (qq-contacts--clear-search-results)
    (setq qq-contacts--query nil
          qq-contacts--view
          (if (eq qq-contacts--previous-view 'members)
              'friends
            qq-contacts--previous-view))
    (qq-contacts--request-reconcile)))

(defun qq-contacts--line-property (property)
  "Return line PROPERTY at point."
  (or (get-text-property (point) property)
      (get-text-property (line-beginning-position) property)))

(defun qq-contacts-toggle-category (&optional category-id)
  "Toggle friend CATEGORY-ID or the category at point."
  (interactive)
  (setq category-id
        (or category-id (qq-contacts--line-property 'qq-contacts-category-id)))
  (unless (integerp category-id)
    (user-error "qq: point is not on a friend category"))
  (if (gethash category-id qq-contacts--collapsed-categories)
      (remhash category-id qq-contacts--collapsed-categories)
    (puthash category-id t qq-contacts--collapsed-categories))
  (qq-contacts--request-reconcile))

(defun qq-contacts--activate-entry (entry)
  "Activate exact directory ENTRY stored by an action row."
  (pcase (qq-contacts--entry-type entry)
    ('category
     (qq-contacts-toggle-category
      (alist-get 'category_id (qq-contacts--entry-object entry))))
    ((or 'friend 'member)
     (qq-chat-open
      (qq-state-session-key
       'private (alist-get 'user_id (qq-contacts--entry-object entry)))))
    ('group
     (qq-chat-open
      (qq-state-session-key
       'group (alist-get 'group_id (qq-contacts--entry-object entry)))))
    (_ (user-error "qq: this directory row is not actionable"))))

(defun qq-contacts-open-at-point ()
  "Open or toggle the exact directory row at point."
  (interactive)
  (if-let* ((button (button-at (point))))
      (push-button button)
    (pcase (qq-contacts--line-property 'qq-contacts-row-type)
      ('category (qq-contacts-toggle-category))
      ((or 'friend 'member)
       (let ((friend (qq-contacts--object-at-point)))
         (qq-chat-open
          (qq-state-session-key 'private (alist-get 'user_id friend)))))
      ('group
       (let ((group (qq-contacts--object-at-point)))
         (qq-chat-open
          (qq-state-session-key 'group (alist-get 'group_id group)))))
      (_ (user-error "qq: point is not on a directory item")))))

(defun qq-contacts-mouse-open-at-point (event)
  "Open the directory row selected by mouse EVENT."
  (interactive "e")
  (mouse-set-point event)
  (qq-contacts-open-at-point))

(defun qq-contacts--object-at-point ()
  "Return the exact directory object represented at point."
  (or (qq-contacts--line-property 'qq-contacts-object)
      (user-error "qq: point is not on a directory item")))

(defun qq-contacts-open-info-at-point ()
  "Open native user or group details for the item at point."
  (interactive)
  (let ((object (qq-contacts--object-at-point)))
    (pcase (qq-contacts--line-property 'qq-contacts-row-type)
      ((or 'friend 'member) (qq-user-open (alist-get 'user_id object)))
      ('group (qq-group-open (alist-get 'group_id object)))
      (_ (user-error "qq: point has no profile page")))))

(defun qq-contacts-open-avatar-at-point ()
  "Open the native avatar for the item at point."
  (interactive)
  (let ((object (qq-contacts--object-at-point)))
    (pcase (qq-contacts--line-property 'qq-contacts-row-type)
      ((or 'friend 'member)
       (qq-media-open-user-avatar (alist-get 'user_id object)))
      ('group (qq-media-open-group-avatar (alist-get 'group_id object)))
      (_ (user-error "qq: point has no avatar")))))

(defun qq-contacts-copy-id-at-point ()
  "Copy the exact QQ or group identity at point."
  (interactive)
  (let* ((object (qq-contacts--object-at-point))
         (id (pcase (qq-contacts--line-property 'qq-contacts-row-type)
               ((or 'friend 'member)
                (alist-get 'user_id object))
               ('group (alist-get 'group_id object)))))
    (unless (stringp id)
      (user-error "qq: point has no exact identity"))
    (kill-new id)
    (message "qq: copied %s" id)))

(defun qq-contacts--group-member-at-point ()
  "Return the exact group member at point, or signal a user error."
  (unless (eq (qq-contacts--line-property 'qq-contacts-row-type) 'member)
    (user-error "qq: point is not on a group member"))
  (qq-contacts--object-at-point))

(defun qq-contacts--apply-group-member-setting
    (buffer member field value message-text _receipt)
  "Apply confirmed FIELD VALUE to MEMBER when BUFFER still owns it."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (derived-mode-p 'qq-contacts-mode)
                 (memq member qq-contacts--search-members))
        (setf (alist-get field member nil nil #'eq)
              (and (not (string-empty-p value)) value))
        (when-let* ((surface (qq-contacts--live-current-view)))
          (appkit-surface-send surface (list 'qq-render (appkit-projection-change-create :full-p t))))
        (message "qq: %s" message-text)))))

(defun qq-contacts-set-member-card-at-point (card)
  "Set or clear the group member CARD at point."
  (interactive
   (let ((member (qq-contacts--group-member-at-point)))
     (list (read-string "群名片（留空清除）: "
                        (or (alist-get 'card member) "")))))
  (let* ((member (qq-contacts--group-member-at-point))
         (group-id (alist-get 'group_id member))
         (user-id (alist-get 'user_id member)))
    (qq-core-set-group-member-card
     group-id user-id card
     (apply-partially
      #'qq-contacts--apply-group-member-setting
      (current-buffer) member 'card card
      (if (string-empty-p card) "群名片已清除" "群名片已更新")))))

(defun qq-contacts-set-member-special-title-at-point (special-title)
  "Set or clear the group member SPECIAL-TITLE at point."
  (interactive
   (let ((member (qq-contacts--group-member-at-point)))
     (list (read-string "专属头衔（留空清除）: "
                        (or (alist-get 'title member) "")))))
  (let* ((member (qq-contacts--group-member-at-point))
         (group-id (alist-get 'group_id member))
         (user-id (alist-get 'user_id member)))
    (qq-core-set-group-member-special-title
     group-id user-id special-title
     (apply-partially
      #'qq-contacts--apply-group-member-setting
      (current-buffer) member 'title special-title
      (if (string-empty-p special-title)
          "专属头衔已清除"
        "专属头衔已更新")))))

(defun qq-contacts--apply-group-member-kick
    (buffer member display-name _receipt)
  "Remove confirmed MEMBER from BUFFER when it still owns that exact row."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (derived-mode-p 'qq-contacts-mode)
                 (memq member qq-contacts--search-members))
        (setq qq-contacts--search-members
              (delq member qq-contacts--search-members))
        (when-let* ((surface (qq-contacts--live-current-view)))
          (appkit-surface-send surface (list 'qq-render (appkit-projection-change-create :full-p t))))
        (message "qq: 已将 %s 移出群聊" display-name)))))

(defun qq-contacts-kick-member-at-point (reject-add-request)
  "Remove the group member at point after an explicit confirmation.

When REJECT-ADD-REQUEST is non-nil, reject a later application from the same
user as part of the same backend operation."
  (interactive
   (let* ((member (qq-contacts--group-member-at-point))
          (display-name (qq-contacts--member-name member))
          (user-id (alist-get 'user_id member))
          (group-name (or (qq-contacts--present-string
                           (alist-get 'group_name member))
                          (alist-get 'group_id member))))
     (unless (yes-or-no-p
              (format "确认将 %s（QQ %s）移出群聊 %s？ "
                      display-name user-id group-name))
       (user-error "qq: 已取消移出群聊"))
     (list (y-or-n-p "同时拒绝该用户再次申请加群？ "))))
  (let* ((member (qq-contacts--group-member-at-point))
         (group-id (alist-get 'group_id member))
         (user-id (alist-get 'user_id member))
         (display-name (qq-contacts--member-name member)))
    (qq-core-kick-group-member
     group-id user-id reject-add-request
     (apply-partially
      #'qq-contacts--apply-group-member-kick
      (current-buffer) member display-name))))

(defun qq-contacts--item-positions ()
  "Return ordered buffer positions of actionable directory rows."
  (let ((position (point-min)) positions)
    (while (< position (point-max))
      (when (get-text-property position 'qq-contacts-item-id)
        (push position positions))
      (setq position
            (next-single-property-change
             position 'qq-contacts-item-id nil (point-max))))
    (nreverse (seq-uniq positions #'=))))

(defun qq-contacts--move-item (direction)
  "Move to next item in DIRECTION, where positive means forward."
  (let* ((positions (qq-contacts--item-positions))
         (origin (line-beginning-position))
         (target
          (if (> direction 0)
              (seq-find (lambda (position) (> position origin)) positions)
            (car (last (seq-filter
                        (lambda (position) (< position origin)) positions))))))
    (if target
        (goto-char target)
      (message "qq: no %s directory item"
               (if (> direction 0) "next" "previous")))))

(defun qq-contacts-next-item ()
  "Move to the next friend or group row."
  (interactive)
  (qq-contacts--move-item 1))

(defun qq-contacts-previous-item ()
  "Move to the previous friend or group row."
  (interactive)
  (qq-contacts--move-item -1))

(defun qq-contacts-button-backward ()
  "Move to the previous real directory button."
  (interactive)
  (forward-button -1))

(defun qq-contacts-open-root ()
  "Return to the emacs-qq root buffer."
  (interactive)
  (qq-root-open))

(defun qq-contacts--cancel-refresh ()
  "Cancel requests owned by the current directory refresh."
  (setq qq-contacts--refresh-owner nil
        qq-contacts--refresh-pending 0
        qq-contacts--refresh-parts nil
        qq-contacts--loading nil)
  (when qq-contacts--friend-request
    (qq-request-cancel qq-contacts--friend-request))
  (when qq-contacts--group-request
    (qq-request-cancel qq-contacts--group-request))
  (setq qq-contacts--friend-request nil
        qq-contacts--group-request nil))

(defun qq-contacts--refresh-current-p (view buffer owner)
  "Return non-nil when VIEW and OWNER still own refresh in BUFFER."
  (and (qq-contacts--view-current-p view buffer)
       (with-current-buffer buffer
         (eq owner qq-contacts--refresh-owner))))

(defun qq-contacts--refresh-part-current-p (view buffer owner kind)
  "Return non-nil when VIEW and OWNER still own refresh KIND in BUFFER."
  (and (qq-contacts--refresh-current-p view buffer owner)
       (with-current-buffer buffer
         (memq kind qq-contacts--refresh-parts))))

(defun qq-contacts--finish-refresh-part
    (view buffer owner kind &optional reason)
  "Finish refresh KIND owned by VIEW and OWNER in BUFFER, recording REASON."
  (when (qq-contacts--refresh-part-current-p view buffer owner kind)
    (with-current-buffer buffer
      (pcase kind
        ('friends (setq qq-contacts--friend-request nil))
        ('groups (setq qq-contacts--group-request nil)))
      (when reason
        (setq qq-contacts--error
              (if qq-contacts--error
                  (concat qq-contacts--error " · " reason)
                reason)))
      (setq qq-contacts--refresh-parts
            (delq kind qq-contacts--refresh-parts))
      (setq qq-contacts--refresh-pending
            (length qq-contacts--refresh-parts))
      (when (= qq-contacts--refresh-pending 0)
        (setq qq-contacts--refresh-owner nil
              qq-contacts--loading nil))
      (appkit-surface-send view (list 'qq-render (appkit-projection-change-create :full-p t))))))

(defun qq-contacts-refresh ()
  "Refresh exact friend categories and joined groups from Linux QQ."
  (interactive)
  (let ((view (qq-contacts--ensure-view)))
    (qq-contacts--cancel-refresh)
    (let ((buffer (current-buffer))
          (owner (list 'contacts-refresh (float-time))))
      (setq qq-contacts--refresh-owner owner
            qq-contacts--refresh-pending 2
            qq-contacts--refresh-parts '(friends groups)
            qq-contacts--loading t
            qq-contacts--error nil)
      (appkit-surface-send view (list 'qq-render (appkit-projection-change-create :full-p t)))
      (condition-case error-data
          (let ((request
                  (qq-core-refresh-friend-categories
                   (lambda (_categories)
                     (qq-contacts--finish-refresh-part
                      view buffer owner 'friends))
                   (lambda (_response reason)
                     (qq-contacts--finish-refresh-part
                      view buffer owner 'friends reason)))))
            (when (qq-contacts--refresh-part-current-p
                   view buffer owner 'friends)
              (setq qq-contacts--friend-request request)))
        (error
         (qq-contacts--finish-refresh-part
          view buffer owner 'friends (error-message-string error-data))))
      (condition-case error-data
          (let ((request
                  (qq-core-refresh-joined-groups
                   (lambda (_groups)
                     (qq-contacts--finish-refresh-part
                      view buffer owner 'groups))
                   (lambda (_response reason)
                     (qq-contacts--finish-refresh-part
                      view buffer owner 'groups reason)))))
            (when (qq-contacts--refresh-part-current-p
                   view buffer owner 'groups)
              (setq qq-contacts--group-request request)))
        (error
         (qq-contacts--finish-refresh-part
          view buffer owner 'groups (error-message-string error-data)))))))

(defun qq-contacts--handle-state-change (event)
  "Invalidate the open directory after relevant state EVENT."
  (when (memq (plist-get event :type)
              '(reset friends-refreshed groups-refreshed sessions-refreshed))
    (when-let* ((owner (plist-get event :account-id))
                (view (qq-contacts--live-view owner)))
      (with-current-buffer (appkit-surface-buffer view)
        (appkit-surface-send view (list 'qq-render (appkit-projection-change-create :full-p t)))))))

(defun qq-contacts--handle-media-cache-update (media-key)
  "Invalidate the directory row identified by avatar MEDIA-KEY."
  (when (stringp media-key)
    (let (keys)
      (cond
       ((string-match "\\`avatar:\\([1-9][0-9]*\\)\\'" media-key)
        (setq keys (list (cons 'friend (match-string 1 media-key))
                         (cons 'member (match-string 1 media-key)))))
       ((string-match "\\`group-avatar:\\([1-9][0-9]*\\)\\'" media-key)
        (setq keys (list (cons 'group (match-string 1 media-key))))))
      (when keys
        (dolist (view (qq-contacts--live-views))
          (with-current-buffer (appkit-surface-buffer view)
            (appkit-surface-send view (list 'qq-render (appkit-projection-change-create :keys keys :resources (list media-key))))))))))

(defvar qq-contacts-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'qq-contacts-refresh)
    (define-key map (kbd "f") #'qq-contacts-show-friends)
    (define-key map (kbd "G") #'qq-contacts-show-groups)
    (define-key map (kbd "I") #'qq-contacts-show-not-recent-groups)
    (define-key map (kbd "C-c C-k") #'qq-contacts-clear-search)
    (define-key map (kbd "RET") #'qq-contacts-open-at-point)
    (define-key map (kbd "m") #'qq-contacts-open-at-point)
    (define-key map (kbd "i") #'qq-contacts-open-info-at-point)
    (define-key map (kbd "a") #'qq-contacts-open-avatar-at-point)
    (define-key map (kbd "w") #'qq-contacts-copy-id-at-point)
    (define-key map (kbd "C") #'qq-contacts-set-member-card-at-point)
    (define-key map (kbd "T") #'qq-contacts-set-member-special-title-at-point)
    (define-key map (kbd "K") #'qq-contacts-kick-member-at-point)
    (define-key map (kbd "t") #'qq-contacts-toggle-category)
    (define-key map (kbd "n") #'qq-contacts-next-item)
    (define-key map (kbd "p") #'qq-contacts-previous-item)
    (define-key map (kbd "TAB") #'forward-button)
    (define-key map (kbd "<backtab>") #'qq-contacts-button-backward)
    (define-key map (kbd "b") #'qq-contacts-open-root)
    (define-key map [mouse-1] #'qq-contacts-mouse-open-at-point)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `qq-contacts-mode'.")

(define-derived-mode qq-contacts-mode special-mode "QQ-Contacts"
  "Major mode for the native QQ contacts directory."
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (setq-local switch-to-buffer-preserve-window-point nil)
  (buffer-disable-undo)
  (setq-local buffer-undo-list t)
  (setq-local qq-contacts--node-table (make-hash-table :test #'equal))
  (setq-local qq-contacts--collapsed-categories
              (make-hash-table :test #'eql))
  (setq-local qq-contacts--view 'friends)
  (setq-local qq-contacts--previous-view 'friends)
  (setq-local qq-contacts--query nil)
  (setq-local qq-contacts--fill-column nil)
  (setq-local qq-contacts--header-line-cache "")
  (setq-local qq-contacts--rendering nil)
  (setq-local qq-contacts--render-pending nil)
  (setq-local qq-contacts--dirty nil)
  (setq-local qq-contacts--pending-force-keys nil)
  (setq-local qq-contacts--loading nil)
  (setq-local qq-contacts--error nil)
  (setq-local qq-contacts--refresh-owner nil)
  (setq-local qq-contacts--refresh-pending 0)
  (setq-local qq-contacts--refresh-parts nil)
  (setq-local qq-contacts--friend-request nil)
  (setq-local qq-contacts--group-request nil)
  (setq-local qq-contacts--search-owner nil)
  (setq-local qq-contacts--search-pending nil)
  (setq-local qq-contacts--search-errors nil)
  (setq-local qq-contacts--search-members nil)
  (setq-local qq-contacts--member-group-id nil)
  (setq-local qq-contacts--search-member-request nil)
  (setq-local header-line-format 'qq-contacts--header-line-cache)
  (setq-local revert-buffer-function
              (lambda (&rest _ignored) (qq-contacts-refresh)))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq-local qq-contacts--ewoc
                (ewoc-create #'qq-contacts--ewoc-printer nil nil t)))
  (add-hook 'window-buffer-change-functions
            #'qq-contacts--window-buffer-change nil t)
  (add-hook 'window-size-change-functions
            #'qq-contacts--window-size-change nil t)
  (add-hook 'window-scroll-functions #'qq-contacts--window-scroll nil t)
  (add-hook 'change-major-mode-hook #'qq-contacts--cancel-refresh nil t)
  (add-hook 'change-major-mode-hook #'qq-contacts--cancel-search nil t)
  (add-hook 'kill-buffer-hook #'qq-contacts--cancel-refresh nil t)
  (add-hook 'kill-buffer-hook #'qq-contacts--cancel-search nil t))

;;;###autoload
(defun qq-contacts-open ()
  "Open the persistent native QQ contacts directory."
  (interactive)
  (let* ((owner
          (or (qq-runtime-current-account-id)
              (user-error "qq: select a QQ account first")))
         (account (qq-account-get owner))
         (_ (unless account
              (user-error "qq: QQ account does not exist: %s" owner)))
         (surface
          (qq-runtime-open-account-surface
           :account-id owner
           :id qq-contacts--view-id
           :mode 'qq-contacts-mode
           :buffer-name
           (qq-runtime-account-buffer-name "contacts" nil owner)
           :render-function #'qq-contacts--render
           :setup #'qq-contacts--setup-view
           :select t))
         (buffer (appkit-surface-buffer surface)))
    (with-current-buffer buffer
      (appkit-surface-send
       surface (list 'qq-render (appkit-projection-change-create :full-p t)))
      (when (and (not qq-contacts--loading)
                 (or (not (qq-state-friend-categories-loaded-p))
                     (not (qq-state-groups-loaded-p)))
                 (eq (qq-state-connection-status) 'ready))
        (qq-contacts-refresh)))
    buffer))

(add-hook 'qq-state-change-hook #'qq-contacts--handle-state-change)
(add-hook 'qq-media-cache-update-hook #'qq-contacts--handle-media-cache-update)

(provide 'qq-contacts)

;;; qq-contacts.el ends here
