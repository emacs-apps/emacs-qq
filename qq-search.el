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
(defvar-local qq-search--request nil)
(defvar-local qq-search--owner nil)
(defvar-local qq-search--view nil)
(defvar-local qq-search--scroll-observer nil)

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

(defun qq-search--cancel ()
  "Revoke the current search before cancellation can invoke callbacks."
  (setq qq-search--owner nil
        qq-search--loading nil)
  (when qq-search--request
    (qq-request-cancel qq-search--request)
    (setq qq-search--request nil)))

(defun qq-search--current-p (buffer view owner)
  "Whether BUFFER's exact VIEW still owns request OWNER."
  (and (buffer-live-p buffer)
       (appkit-surface-live-p view)
       (eq (appkit-surface-buffer view) buffer)
       (eq (buffer-local-value 'qq-search--view buffer) view)
       (eq (buffer-local-value 'qq-search--owner buffer) owner)
       (equal (buffer-local-value 'qq-search--instance-id buffer)
              (qq-server-gateway-instance-id))))

(defun qq-search--redisplay (&optional change)
  "Schedule projection CHANGE through the owning account surface."
  (when (appkit-surface-live-p qq-search--view)
    (appkit-surface-send
     qq-search--view
     (list 'qq-render
           (or change (appkit-projection-change-create :frame-p t))))))

(defun qq-search--scroll-end (_window position end)
  "Load another page when the visible POSITION approaches END."
  (when (and qq-search--loaded-p qq-search--cursor
             (not qq-search--loading) (not qq-search--error)
             (appkit-surface-live-p qq-search--view)
             (equal qq-search--instance-id (qq-server-gateway-instance-id))
             (appkit-scroll-near-end-p position end 2000))
    (qq-search--request-page t)))

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

(defun qq-search--settle (buffer view owner append-p page)
  "Accept checked PAGE only for BUFFER's current VIEW and OWNER."
  (when (qq-search--current-p buffer view owner)
    (with-current-buffer buffer
      (let ((seen (make-hash-table :test #'equal))
            (hits (and append-p (copy-sequence qq-search--hits))))
        (dolist (hit hits)
          (puthash (qq-search--hit-key hit) t seen))
        (dolist (hit (plist-get page :hits))
          (unless (gethash (qq-search--hit-key hit) seen)
            (puthash (qq-search--hit-key hit) t seen)
            (setq hits (nconc hits (list hit)))))
        (setq qq-search--hits hits
              qq-search--cursor (plist-get page :next-cursor)
              qq-search--loaded-p t
              qq-search--loading nil
              qq-search--request nil
              qq-search--owner nil
              qq-search--error nil)
        (qq-search--redisplay
         (appkit-projection-change-create
          :full-p t :frame-p t :position (if append-p 'preserve 'first)))))))

(defun qq-search--fail (buffer view owner _body reason)
  "Keep accepted results and the retry cursor after current request failure."
  (when (qq-search--current-p buffer view owner)
    (with-current-buffer buffer
      (setq qq-search--loading nil
            qq-search--request nil
            qq-search--owner nil
            qq-search--error reason)
      (qq-search--redisplay))))

(defun qq-search--request-page (append-p)
  "Request the first page, or APPEND-P the current continuation."
  (let* ((buffer (current-buffer))
         (view qq-search--view)
         (account-id qq-search--account-id)
         (query qq-search--query)
         (instance-id qq-search--instance-id)
         (session-key qq-search--session-key)
         (conversation (and session-key
                            (qq-message--history-conversation-params session-key)))
         (cursor (and append-p qq-search--cursor))
         (owner (list 'search query cursor)))
    (qq-search--cancel)
    (setq qq-search--owner owner
          qq-search--loading t
          qq-search--error nil)
    (qq-search--redisplay
     (appkit-projection-change-create
      :full-p (not append-p) :frame-p t
      :position (if append-p 'preserve 'first)))
    (condition-case error-data
        (let ((request
               (qq-request-start
                (lambda (success failure)
                  (qq-message--call
                   "message.search" account-id
                   `((query . ,query)
                     ,@(when conversation `((conversation . ,conversation)))
                     ,@(when cursor `((cursor . ,cursor)))
                     (count . 50))
                   :projector (lambda (result)
                                (qq-search--check-page
                                 result account-id query session-key cursor instance-id))
                   :callback success
                   :errback failure))
                :owner account-id
                :lifecycle-owner view
                :callback (apply-partially #'qq-search--settle
                                           buffer view owner append-p)
                :errback (apply-partially #'qq-search--fail
                                          buffer view owner))))
          ;; A capability/preflight failure may settle synchronously.
          (when (qq-search--current-p buffer view owner)
            (setq qq-search--request request)))
      (error
       (qq-search--fail buffer view owner nil
                        (error-message-string error-data))))))

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

(defun qq-search-open-hit (&optional hit)
  "Open exact search HIT in its conversation and fetch the surrounding context."
  (interactive)
  (setq hit (or hit (get-text-property (point) 'qq-search-hit)))
  (if (null hit)
      (if (button-at (point))
          (push-button (point))
        (user-error "qq: Put point on a search result"))
    (unless (equal qq-search--instance-id (qq-server-gateway-instance-id))
      (user-error "qq: Gateway changed; refresh this search before opening a result"))
    (let ((account-id qq-search--account-id)
          (session (plist-get hit :session-key))
          (center (plist-get hit :center)))
      (unless (qq-account-get account-id)
        (user-error "qq: Search account is no longer available"))
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

(defun qq-search--insert-preview (text)
  "Insert TEXT and highlight literal keywords with ASCII-only case folding."
  (let ((start (point))
        (text (qq-state-preview-one-line text)))
    (insert text)
    (dolist (range (qq-search-literal-match-ranges text qq-search--query))
      (add-face-text-property (+ start (car range)) (+ start (cdr range))
                              'match t))))

(defun qq-search--project-rows (_surface _app-view _model)
  "Project saved hits into stable Appkit rows."
  (appkit-projection-project
   qq-search--hits #'qq-search--hit-key
   :context-function (lambda (_previous _hit) qq-search--query)))

(defun qq-search--project-frame (_surface _app-view _model)
  "Return the result view's header and status footer."
  (cons
   (concat
    (propertize (format "搜索本地聊天记录 · %s\n" qq-search--query) 'face 'bold)
    (format "%s · 账号 %s\n"
            (if qq-search--session-key
                (format "当前会话 %s" qq-search--session-key)
              "全部好友和群聊")
            qq-search--account-id)
    "仅搜索本地保存的记录，按入库顺序显示；尚未保存的历史不会命中。\n"
    "向下滚动自动加载    RET: 查看上下文    g: 刷新    s: 修改关键词\n\n")
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
  "Insert one result ROW with a stable anchor and literal highlighting."
  (let* ((hit (appkit-projection-row-payload row))
         (message (plist-get hit :message))
         (start (point))
         (time (alist-get 'time message))
         (session (plist-get hit :session-key)))
    (insert-text-button
     (format "%s · %s · %s"
             (or (alist-get 'peer-name message) session)
             (or (alist-get 'sender-name message) "")
             (if (numberp time)
                 (format-time-string "%F %T" (seconds-to-time time))
               ""))
     'follow-link t
     'action (lambda (_button) (qq-search-open-hit hit)))
    (insert "\n  ")
    (qq-search--insert-preview (or (alist-get 'preview message) ""))
    (insert "\n\n")
    (add-text-properties
     start (point)
     (list 'qq-search-key (appkit-projection-row-key row)
           'qq-search-hit hit))))

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
            (prog1 (funcall render surface app-view model change)
              (qq-search--check-scroll))))
    renderer))

(defvar-keymap qq-search-mode-map
  :doc "Keymap for saved QQ chat-record search."
  "RET" #'qq-search-open-hit)

(appkit-bindings-define 'qq-search-mode-map 'read-only
                        'activate #'qq-search-open-hit
                        'refresh #'qq-search-refresh
                        'search #'qq-search-edit-query
                        'next-item #'forward-button
                        'previous-item #'backward-button
                        'quit #'quit-window
                        'help #'describe-mode)

(define-derived-mode qq-search-mode special-mode "QQ-Search"
  "Search locally saved private and group messages.
Scroll toward the end to load more matches.  RET opens the exact message in
context or activates a retry button; g refreshes and s changes the keyword."
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
  (interactive (list (read-string "搜索全部本地聊天记录: " nil 'qq-search-history)))
  (setq query (qq-search--validate-query query))
  (when (and session-key
             (not (memq (qq-state-session-key-type session-key) '(private group))))
    (user-error "qq: Local record search supports private and group chats only"))
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
    buffer))

(defun qq-search-chat (query)
  "Search literal QUERY within the current private or group conversation."
  (interactive
   (progn
     (unless (and (boundp 'qq-chat--session-key)
                  qq-chat--session-key
                  (memq (qq-state-session-key-type qq-chat--session-key)
                        '(private group)))
       (user-error "qq: Local record search supports private and group chats only"))
     (list (read-string "搜索此会话的本地记录: " nil 'qq-search-history))))
  (unless (and (boundp 'qq-chat--session-key) qq-chat--session-key)
    (user-error "qq: Open a private or group chat first"))
  (qq-search-messages query qq-chat--session-key))

(provide 'qq-search)
;;; qq-search.el ends here
