;;; qq-search.el --- Search saved QQ chat records -*- lexical-binding: t; -*-

;;; Commentary:
;; Search results are detached snapshots.  Opening a hit resolves its exact
;; canonical row through the ordinary around-history port.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'subr-x)
(require 'appkit-bindings)
(require 'appkit-projection)
(require 'appkit-scroll)
(require 'qq-api)
(require 'qq-chat)
(require 'qq-message)
(require 'qq-request)
(require 'qq-runtime)

(autoload 'qq-search-preview-hit "qq-search-preview" nil t)
(declare-function qq-search-preview-cancel "qq-search-preview" (&optional promote))

(defvar qq-search-history nil
  "History of literal chat-record search queries.")

(defvar-local qq-search--account-id nil)
(defvar-local qq-search--instance-id nil)
(defvar-local qq-search--session-key nil)
(defvar-local qq-search--query "")
(defvar-local qq-search--hits nil)
(defvar-local qq-search--cursor nil)
(defvar-local qq-search--loaded-p nil)
(defvar-local qq-search--loading nil)
(defvar-local qq-search--error nil)


(defvar-local qq-search--view nil)
(defvar-local qq-search--scroll-observer nil)

(defvar-local qq-search--inline-p nil
  "Non-nil while this chat displays a filtered search timeline.")

(defun qq-search--hit-at-point ()
  "Return the search hit represented by the current message."
  (or (get-text-property (point) 'qq-search-hit)
      (when qq-search--inline-p
        (let ((anchor (get-text-property (point) 'qq-chat-message-anchor)))
          (seq-find (lambda (hit)
                      (equal anchor (qq-state-message-anchor (plist-get hit :message))))
                    qq-search--hits)))))

(defun qq-search--inline-messages ()
  "Return current matches in ordinary chronological chat order."
  (let ((latest (make-hash-table :test #'equal)))
    (dolist (message (qq-state-session-messages qq-search--session-key))
      (puthash (qq-state-message-anchor message) message latest))
    (mapcar
     (lambda (hit)
       (let* ((message (plist-get hit :message))
              (anchor (qq-state-message-anchor message)))
         (qq-state-message-apply-tombstones
          qq-search--session-key (or (gethash anchor latest) message))))
     (reverse qq-search--hits))))

(defun qq-search--inline-status ()
  "Return search state for the normal chat header."
  (when qq-search--inline-p
    (format "搜索「%s」 · %s · %s"
            (string-replace "%" "%%" (qq-state-preview-one-line qq-search--query))
            (cond (qq-search--error (concat "失败：" qq-search--error))
                  (qq-search--loading "加载中…")
                  (t (format "%d 条已加载%s" (length qq-search--hits)
                             (if qq-search--cursor "" " · 已到末尾"))))
            (substitute-command-keys
             "\\<qq-chat-mode-map>\\[qq-chat-cancel-dwim] 取消过滤"))))

(defun qq-search--can-load-more-p ()
  "Whether this exact search can request its older continuation."
  (and qq-search--loaded-p qq-search--cursor
       (not qq-search--loading) (not qq-search--error)
       (appkit-surface-live-p qq-search--view)
       (equal qq-search--instance-id (qq-server-gateway-instance-id))))

(defun qq-search--load-more ()
  "Load another older result page when the current query can continue."
  (when (qq-search--can-load-more-p) (qq-search--request-page t)))

(defun qq-search--highlight-region (start end)
  "Highlight literal query matches in the rendered message from START to END."
  (let ((text (buffer-substring-no-properties start end)))
    (dolist (range (qq-search-literal-match-ranges text qq-search--query))
      (add-face-text-property (+ start (car range)) (+ start (cdr range))
                              'match t))))

(defun qq-search-cancel-filter (&optional no-jump)
  "Restore this chat's ordinary timeline, retaining its draft and reply.
Unless NO-JUMP, retain the selected result by loading its normal context."
  (interactive)
  (when qq-search--inline-p
    (let ((hit (qq-search--hit-at-point)))
      (qq-search--cancel)
      (setq qq-search--inline-p nil)
      (qq-chat-render)
      (when (and hit (not no-jump))
        (qq-chat-goto-message (plist-get hit :center) t)))))

(defun qq-search--start-inline (query session-key)
  "Show QUERY as a message filter in SESSION-KEY's existing chat."
  (require 'qq-chat)
  (let ((buffer (qq-chat--open-buffer session-key)))
    (with-current-buffer buffer
      (qq-search--cancel)
      (qq-chat-search-cancel)
      (qq-chat--cancel-jump qq-chat--jump-owner)
      (qq-chat--cancel-history-request)
      (unless (eq qq-search--view (qq-chat--live-current-view))
        (let ((view (qq-chat--live-current-view)))
          (appkit-register-handle
           view 'function
           (lambda ()
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (when (eq qq-search--view view)
                   (qq-search--cancel)
                   (setq qq-search--inline-p nil qq-search--view nil))))))))
      (setq qq-search--account-id qq-runtime--account-id
            qq-search--session-key session-key
            qq-search--view (qq-chat--live-current-view)
            qq-search--query query
            qq-search--inline-p t)
      (add-hook 'kill-buffer-hook #'qq-search--cancel nil t)
      (add-hook 'change-major-mode-hook #'qq-search--cancel nil t)
      (qq-search-refresh))
    (unless (eq (current-buffer) buffer) (pop-to-buffer buffer))
    buffer))

(defun qq-search--conversation-session (conversation)
  "Return the exact private/group session in search CONVERSATION."
  (pcase (alist-get 'kind conversation)
    ("private"
     (unless (and (qq-server-wire-exact-object-keys-p conversation '(kind peer_uin))
                  (qq-protocol-uint64-decimal-p (alist-get 'peer_uin conversation)))
       (error "qq: Invalid private search conversation"))
     (qq-state-session-key 'private (alist-get 'peer_uin conversation)))
    ("group"
     (unless (and (qq-server-wire-exact-object-keys-p conversation '(kind group_uin))
                  (qq-protocol-uint64-decimal-p (alist-get 'group_uin conversation)))
       (error "qq: Invalid group search conversation"))
     (qq-state-session-key 'group (alist-get 'group_uin conversation)))
    (_ (error "qq: Search supports private and group conversations only"))))

(defun qq-search--sender-fallback (native)
  "Keep saved NATIVE sender identity readable without fetching presentation."
  (let ((presentation (alist-get 'sender_presentation native))
        (sender (alist-get 'sender native)))
    (unless (seq-some (lambda (field)
                        (qq-message--present-string (alist-get field presentation)))
                      '(member_name remark nickname))
      (setf (alist-get 'sender_presentation native)
            `((nickname . ,(or (alist-get 'uin sender)
                               (alist-get 'uid sender) "QQ")))))
    native))

(defun qq-search--check-hit (item account-id session-key)
  "Normalize saved search ITEM for ACCOUNT-ID and optional SESSION-KEY."
  (let* ((conversation (alist-get 'conversation item))
         (session (qq-search--conversation-session conversation))
         (native (qq-message--history-item
                  (assq-delete-all 'conversation (copy-tree item))
                  "native" "Search result"))
         (message
          (qq-message-normalize-snapshot
           (qq-search--sender-fallback native) account-id (qq-account-get account-id)
           (alist-get 'canonical-recalled-p native) t))
         (row (alist-get 'row_key item)))
    (unless (and (or (null session-key) (equal session-key session))
                 (equal session (alist-get 'session-key message)))
      (error "qq: Search hit contradicts its conversation"))
    (list :message message :conversation conversation
          :session-key session :center `((kind . "row") (row_key . ,row)))))

(defun qq-search--check-page (page account-id query session-key cursor instance-id)
  "Check detached search PAGE for ACCOUNT-ID, QUERY, SESSION-KEY and CURSOR."
  (unless (and (equal (alist-get 'account_id page) account-id)
               (equal (alist-get 'gateway_instance_id page) instance-id)
               (assq 'messages page)
               (equal (alist-get 'query page) query)
               (equal (alist-get 'scope page) "local")
               (proper-list-p (alist-get 'messages page))
               (assq 'next_cursor page))
    (error "qq: Invalid local search envelope"))
  (let ((next (alist-get 'next_cursor page))
        (seen (make-hash-table :test #'equal))
        hits)
    (unless (or (null next)
                (and (qq-api--single-alist-p next)
                     (qq-api--json-safe-value-p next)))
      (error "qq: Invalid search continuation"))
    (when (and cursor next (equal next cursor))
      (error "qq: Search continuation did not advance"))
    (dolist (item (alist-get 'messages page))
      (let* ((hit (qq-search--check-hit item account-id session-key))
             (key (qq-search--hit-key hit)))
        (when (gethash key seen)
          (error "qq: Search page repeats a canonical row"))
        (puthash key t seen)
        (push hit hits)))
    (list :hits (nreverse hits) :next-cursor (copy-tree next))))

(defun qq-search--cancel (&optional keep-preview)
  "Stop presenting the pending page; KEEP-PREVIEW retains the context preview."
  (unless keep-preview
    (when (fboundp 'qq-search-preview-cancel) (qq-search-preview-cancel)))
  (setq qq-search--page nil qq-search--loading nil))



(defun qq-search--redisplay (&optional change)
  "Schedule projection CHANGE through the owning account surface."
  (when (appkit-surface-live-p qq-search--view)
    (appkit-surface-send
     qq-search--view
     (list 'qq-render
           (or change (appkit-projection-change-create :frame-p t))))))

(defun qq-search--scroll-end (_window position end)
  "Load another page when the visible POSITION approaches END."
  (qq-reply-check-viewport)
  (when (appkit-scroll-near-end-p position end 2000)
    (qq-search--load-more)))

(defun qq-search--install-scroll-observer (view)
  "Observe scrolling in VIEW once for its surface lifetime."
  (unless (and (appkit-scroll-observer-p qq-search--scroll-observer)
               (appkit-scroll-observer-active-p qq-search--scroll-observer))
    (setq qq-search--scroll-observer
          (appkit-scroll-observer-install view :end-function #'qq-search--scroll-end))))

(defun qq-search--check-scroll ()
  "Recheck visible result edges after rendering or opening a search."
  (when (appkit-scroll-observer-p qq-search--scroll-observer)
    (appkit-scroll-observer-check qq-search--scroll-observer)))

(defun qq-search--hit-key (hit)
  "Return the exact search HIT identity."
  (list (plist-get hit :session-key) (plist-get hit :center)))





(defun qq-search--request-page (append-p)
  "Request one shared search page and retain only the Surface's window."
  (qq-search--cancel append-p)
  (setq qq-search--page
        (list 'search qq-search--query qq-search--session-key
              (and append-p qq-search--cursor) qq-search--instance-id)
        qq-search--loading t qq-search--error nil)
  (qq-search--redisplay
   (appkit-projection-change-create
    :full-p (not append-p) :frame-p t
    :position (if (or append-p qq-search--inline-p) 'preserve 'first)))
  (qq-runtime-request qq-search--page t #'qq-search--present-page))

(defun qq-search-refresh ()
  "Restart the literal search against locally saved chat records."
  (interactive)
  (unless qq-search--account-id
    (user-error "qq: Open a chat-record search first"))
  (setq qq-search--instance-id (qq-server-gateway-instance-id)
        qq-search--hits nil
        qq-search--cursor nil
        qq-search--loaded-p nil)
  (qq-search--request-page nil))

(defun qq-search-retry ()
  "Retry the failed search page without discarding accepted results."
  (interactive)
  (cond
   ((not (equal qq-search--instance-id (qq-server-gateway-instance-id)))
    (user-error "qq: Gateway changed; refresh this search before retrying"))
   (qq-search--loading (user-error "qq: Search is already loading"))
   ((not qq-search--error) (user-error "qq: No failed search page to retry"))
   (t (qq-search--request-page (and qq-search--loaded-p qq-search--cursor t)))))

(defun qq-search-edit-query (query)
  "Replace this buffer's literal search QUERY."
  (interactive (list (read-string "搜索本地聊天记录: " qq-search--query
                                  'qq-search-history)))
  (setq query (qq-search--validate-query query))
  (qq-search--cancel)
  (setq qq-search--query query)
  (qq-search-refresh))

(defun qq-search--require-hit (&optional hit)
  "Return the selected HIT after checking its search account and lifetime."
  (setq hit (or hit (qq-search--hit-at-point)))
  (unless hit (user-error "qq: Put point on a search result"))
  (unless (and (appkit-surface-live-p qq-search--view)
               (eq (appkit-surface-buffer qq-search--view) (current-buffer)))
    (user-error "qq: This search view is no longer available"))
  (unless (equal qq-search--instance-id (qq-server-gateway-instance-id))
    (user-error "qq: Gateway changed; refresh this search before opening a result"))
  (unless (qq-account-get qq-search--account-id)
    (user-error "qq: Search account is no longer available"))
  hit)

(defun qq-search--session-title (session-key &optional message)
  "Return a readable, single-line title for SESSION-KEY and saved MESSAGE."
  (qq-state-preview-one-line
   (or (qq-message--present-string (alist-get 'title (qq-state-session session-key)))
       (qq-message--present-string (alist-get 'peer-name message))
       session-key)))



(defun qq-search-query-for-session (session-key)
  "Return the current account's saved list query for SESSION-KEY, if any."
  (when-let* ((account-id (qq-runtime-current-account-id))
              (view (appkit-app-surface (qq-runtime-app account-id)
                                       (if session-key (list 'chat session-key) '(search nil))))
              ((appkit-surface-live-p view)))
    (buffer-local-value 'qq-search--query (appkit-surface-buffer view))))

(defun qq-search-open-hit (&optional hit)
  "Open exact search HIT in its conversation and show the surrounding context."
  (interactive)
  (setq hit (or hit (qq-search--hit-at-point)))
  (if (and (null hit) (button-at (point)))
      (push-button (point))
    (setq hit (qq-search--require-hit hit))
    (when (fboundp 'qq-search-preview-cancel) (qq-search-preview-cancel t))
    (let ((account-id qq-search--account-id)
          (session (plist-get hit :session-key))
          (center (plist-get hit :center)))
      (qq-runtime-with-account account-id
        (let ((buffer (qq-chat--open-buffer session)))
          (pop-to-buffer buffer)
          (with-current-buffer buffer
            (qq-chat-goto-message center)))))))

(defun qq-search-literal-match-ranges (text query)
  "Return zero-based literal match ranges of QUERY in TEXT.
Fold ASCII letter case, matching the Gateway's local record search semantics."
  (let* ((fold (lambda (value)
                 (apply #'string
                        (mapcar (lambda (character)
                                  (if (<= ?A character ?Z)
                                      (+ character 32) character))
                                (string-to-list value)))))
         (haystack (funcall fold text))
         (needle (funcall fold query))
         (offset 0)
         found ranges)
    (unless (string-empty-p needle)
      (while (setq found (string-search needle haystack offset))
        (push (cons found (+ found (length needle))) ranges)
        (setq offset (+ found (length needle)))))
    (nreverse ranges)))



(defun qq-search--project-rows (_surface _app-view _model)
  "Project saved hits into stable Appkit rows."
  (appkit-projection-project
   qq-search--hits #'qq-search--hit-key
   :dependencies-function
   (lambda (hit)
     (let ((qq-chat--session-key (plist-get hit :session-key)))
       (qq-chat--message-dependency-keys (plist-get hit :message))))
   :context-function
   (lambda (_previous hit)
     (list qq-search--query
           (qq-search--session-title (plist-get hit :session-key)
                                     (plist-get hit :message))))))

(defun qq-search--project-frame (_surface _app-view _model)
  "Return the result view's header and status footer."
  (cons
   (concat
    (propertize (format "搜索本地聊天记录 · %s\n" qq-search--query) 'face 'bold)
    (format "%s · 账号 %s\n"
            (if qq-search--session-key
                (format "当前会话 %s" (qq-search--session-title qq-search--session-key))
              "全部好友和群聊")
            qq-search--account-id)
    "仅搜索本地保存的记录，按消息时间倒序显示；尚未保存的历史不会命中。\n"
    "向下滚动自动加载    RET: 进入聊天    M-RET: 预览上下文    g: 刷新    s: 修改关键词\n\n")
   (let ((loading qq-search--loading)
         (loaded-p qq-search--loaded-p)
         (error-reason qq-search--error)
         (hits qq-search--hits)
         (cursor qq-search--cursor))
     (with-temp-buffer
       (when loading
         (insert (propertize (if loaded-p "正在加载更多…\n\n" "正在搜索…\n\n")
                             'face 'shadow)))
       (when error-reason
         (insert (propertize (format "加载失败：%s\n" error-reason) 'face 'error))
         (insert-text-button "重试" 'follow-link t
                             'action (lambda (_button) (qq-search-retry)))
         (insert "当前页，或按 g 重新搜索。\n\n"))
       (when loaded-p
         (insert (format "已显示 %d 条记录%s\n" (length hits)
                         (if cursor "" " · 已到末尾")))
         (unless hits
           (insert "没有找到本地记录。\n")))
       (buffer-string)))))

(defun qq-search--print-row (_surface _app-view row)
  "Render a global result ROW with the ordinary QQ message presentation."
  (require 'qq-chat)
  (let* ((hit (appkit-projection-row-payload row))
         (message (plist-get hit :message))
         (qq-chat--session-key (plist-get hit :session-key))
         (start (point)))
    (insert-text-button
     (qq-search--session-title qq-chat--session-key message)
     'follow-link t 'action (lambda (_button) (qq-search-open-hit hit)))
    (insert "\n")
    (qq-chat--render-message message
                             (qq-chat--compute-message-render-context nil message nil))
    (qq-search--highlight-region start (point))
    (add-text-properties start (point)
                         (list 'qq-search-key (appkit-projection-row-key row)
                               'qq-search-hit hit))))


(defun qq-search--media-updated (&optional media-key)
  "Redisplay global result rows whose ordinary media presentation changed."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'qq-search-mode)
        (qq-search--redisplay
         (if media-key
             (appkit-projection-change-create :resources (list (list :media media-key)))
           (appkit-projection-change-create
            :keys (mapcar #'qq-search--hit-key qq-search--hits))))))))

(add-hook 'qq-media-cache-update-hook #'qq-search--media-updated)

(defun qq-search--renderer-create (_surface)
  "Create the standard keyed result Renderer with scroll edge checks."
  (let* ((renderer
          (appkit-projection-renderer-create
           :project-all #'qq-search--project-rows
           :project-frame #'qq-search--project-frame
           :printer #'qq-search--print-row
           :anchor-property 'qq-search-key
           :no-separator-p t))
         (render (appkit-generated-renderer-render renderer)))
    (setf (appkit-generated-renderer-render renderer)
          (lambda (surface app-view model change)
            (funcall render surface app-view model change)
            (qq-search--check-scroll)
            (let ((messages (mapcar (lambda (hit) (plist-get hit :message))
                                   qq-search--hits)))
              (qq-media-render-result surface messages
                                      (qq-reply-render-result surface messages)))))
    renderer))

(defvar-keymap qq-search-mode-map
  :doc "Keymap for saved QQ chat-record search."
  "RET" #'qq-search-open-hit)

(appkit-bindings-define 'qq-search-mode-map 'read-only
                        'activate #'qq-search-open-hit
                        'preview #'qq-search-preview-hit
                        'refresh #'qq-search-refresh
                        'search #'qq-search-edit-query
                        'next-item #'forward-button
                        'previous-item #'backward-button
                        'quit #'quit-window
                        'help #'describe-mode)

(define-derived-mode qq-search-mode special-mode "QQ-Search"
  "Search locally saved private and group messages.
Scroll toward the end to load more matches.  RET opens the exact message in
context or activates a retry button.  M-RET previews context in another
window while keeping focus here; g refreshes and s changes the keyword."
  (setq-local truncate-lines nil)
  (add-hook 'kill-buffer-hook #'qq-search--cancel nil t)
  (add-hook 'change-major-mode-hook #'qq-search--cancel nil t))

(defun qq-search--validate-query (query)
  "Return trimmed nonempty literal QUERY."
  (unless (stringp query)
    (user-error "qq: Search query must be a string"))
  (setq query (string-trim query))
  (when (string-empty-p query)
    (user-error "qq: Search keyword cannot be empty"))
  (when (> (string-bytes (encode-coding-string query 'utf-8)) 1024)
    (user-error "qq: Search keyword exceeds 1024 UTF-8 bytes"))
  query)

(defun qq-search-messages (query &optional session-key)
  "Search QUERY in saved messages, optionally only within SESSION-KEY.
The Gateway searches local private and group records using literal keywords."
  (interactive (list (read-string "搜索全部本地聊天记录: "
                                  (qq-search-query-for-session nil) 'qq-search-history)))
  (setq query (qq-search--validate-query query))
  (when (and session-key
             (not (memq (qq-state-session-key-type session-key) '(private group))))
    (user-error "qq: Local record search supports private and group chats only"))
  (if session-key
      (qq-search--start-inline query session-key)
    (let* ((account-id (qq-runtime-require-account-id "searching saved messages"))
         (view
          (qq-runtime-open-account-surface
           :account-id account-id
           :id (list 'search session-key)
           :mode 'qq-search-mode
           :state session-key
           :buffer-name (format "*QQ 搜索: %s:%s*" account-id
                                (or session-key "全部"))
           :renderer-factory #'qq-search--renderer-create
           :setup (lambda (surface)
                    (setq qq-search--view surface
                          qq-search--account-id account-id
                          qq-search--session-key session-key)
                    (qq-search--install-scroll-observer surface))))
         (buffer (appkit-surface-buffer view)))
    (with-current-buffer buffer
      (qq-search--cancel)
      (setq qq-search--query query)
      (qq-search-refresh))
    (pop-to-buffer buffer)
    (with-current-buffer buffer (qq-search--check-scroll))
    buffer)))

(defun qq-search-chat (query)
  "Search literal QUERY within the current private or group conversation."
  (interactive
   (progn
     (unless (and (boundp 'qq-chat--session-key)
                  qq-chat--session-key
                  (memq (qq-state-session-key-type qq-chat--session-key)
                        '(private group)))
       (user-error "qq: Local record search supports private and group chats only"))
     (list (read-string "搜索此会话的本地记录: "
                        (or (and (boundp 'qq-chat-search--query) qq-chat-search--query)
                            (qq-search-query-for-session qq-chat--session-key))
                        'qq-search-history))))
  (unless (and (boundp 'qq-chat--session-key) qq-chat--session-key)
    (user-error "qq: Open a private or group chat first"))
  (qq-search-messages query qq-chat--session-key))

(defvar-local qq-search--page nil
  "The page selected by this Surface's current search window.")

(defun qq-search--present-page (request page reason)
  "Present an App-accepted PAGE only in the still-selected search window."
  (when (eq request qq-search--page)
    (setq qq-search--page nil qq-search--loading nil qq-search--error reason)
    (unless reason
      (let* ((append-p (nth 3 request))
             (seen (make-hash-table :test #'equal))
             (hits (and append-p (copy-sequence qq-search--hits))))
        (dolist (hit hits) (puthash (qq-search--hit-key hit) t seen))
        (dolist (hit (plist-get page :hits))
          (unless (gethash (qq-search--hit-key hit) seen)
            (puthash (qq-search--hit-key hit) t seen)
            (setq hits (nconc hits (list hit)))))
        (setq qq-search--hits hits qq-search--cursor (plist-get page :next-cursor)
              qq-search--loaded-p t)))
    (qq-search--redisplay
     (appkit-projection-change-create
      :full-p t :frame-p t
      :position (if (or (nth 3 request) qq-search--inline-p) 'preserve 'first)))))

(qq-runtime-define-operation search (query session-key cursor instance-id)
  :method "message.search"
  :params `((query . ,query)
            ,@(when session-key
                `((conversation . ,(qq-message--history-conversation-params session-key))))
            ,@(when cursor `((cursor . ,cursor))) (count . 50))
  :commit
  (let ((page (qq-search--check-page result owner query session-key cursor instance-id)))
    (when session-key
      (dolist (hit (plist-get page :hits))
        (setf (plist-get hit :message)
              (or (car (qq-state--merge-normalized-message
                        session-key (plist-get hit :message) nil 'history))
                  (plist-get hit :message)))))
    page))

(provide 'qq-search)
;;; qq-search.el ends here
