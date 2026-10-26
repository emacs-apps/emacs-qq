;;; qq-guild.el --- QQ Guild directories and text channels -*- lexical-binding: t; -*-

;;; Commentary:
;; Guild IDs, channel IDs, tiny IDs and channel sequences stay in their own
;; domain.  Each view belongs to one account Surface.  History is an explicit
;; sequence range; a short visible page does not imply history exhaustion.

;;; Code:
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'button)
(require 'qq-core)
(require 'qq-rpc)
(require 'qq-runtime)
(require 'qq-account)
(require 'qq-protocol)

(defvar-local qq-guild--guild-id nil)
(defvar-local qq-guild--channel nil)
(defvar-local qq-guild--title "QQ 频道")
(defvar-local qq-guild--items nil)
(defvar-local qq-guild--messages nil)
(defvar-local qq-guild--begin nil)
(defvar-local qq-guild--read-sequence nil)
(defvar-local qq-guild--notice "")
(defvar-local qq-guild--requests nil)
(defvar-local qq-guild--generation 0)
(defvar-local qq-guild--phase nil)
(defvar-local qq-guild--source nil)

(defun qq-guild--channel-p (value)
  "Return non-nil for an exact guild/channel VALUE."
  (and (qq-server-wire-exact-object-keys-p value '(guild_id channel_id))
       (qq-protocol-uint64-decimal-p (alist-get 'guild_id value))
       (qq-protocol-uint64-decimal-p (alist-get 'channel_id value))))

(defun qq-guild--message-p (value)
  "Validate channel message VALUE before presentation or read selection."
  (and (qq-guild--channel-p (alist-get 'channel value))
       (qq-protocol-uint64-decimal-p (alist-get 'sequence value))
       (qq-protocol-uint64-decimal-p (alist-get 'count_sequence value) t)
       (qq-protocol-uint64-decimal-p (alist-get 'sender_tiny_id value))
       (stringp (alist-get 'sender_name value))
       (stringp (alist-get 'text value))
       (natnump (alist-get 'timestamp value))
       (natnump (alist-get 'unsupported_elements value))))

(defun qq-guild--same-channel-p (a b)
  "Compare exact channel routes A and B independently of alist order."
  (and (qq-guild--channel-p a) (qq-guild--channel-p b)
       (equal (alist-get 'guild_id a) (alist-get 'guild_id b))
       (equal (alist-get 'channel_id a) (alist-get 'channel_id b))))

(defun qq-guild--cancel ()
  "Revoke every request owned by this view."
  (cl-incf qq-guild--generation)
  (when qq-guild--requests
    (maphash (lambda (_key marker)
               (when (cdr marker) (qq-request-cancel (cdr marker))))
             qq-guild--requests)
    (clrhash qq-guild--requests)))

(defun qq-guild--call (method params callback)
  "Call METHOD with PARAMS in this Surface, delivering CALLBACK once."
  (unless qq-guild--requests
    (setq qq-guild--requests (make-hash-table :test #'equal)))
  (when-let* ((old (gethash method qq-guild--requests)) (request (cdr old)))
    (qq-request-cancel request))
  (let* ((buffer (current-buffer))
         (owner (qq-runtime-require-account-id method))
         (generation qq-guild--generation)
         (marker (cons method nil)))
    (puthash method marker qq-guild--requests)
    (setcdr
     marker
     (qq-core--start-request
      (lambda (success failure)
        (qq-rpc-call
         method (append (list (cons 'account_id owner)) params)
         :current-p (lambda ()
                      (and (buffer-live-p buffer)
                           (with-current-buffer buffer
                             (and (= generation qq-guild--generation)
                                  (eq marker (gethash method qq-guild--requests))))))
         :projector (lambda (result)
                      (unless (equal (alist-get 'account_id result) owner)
                        (error "qq: Guild response belongs to another account"))
                      result)
         :callback success :errback failure))
      (lambda (result)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (remhash method qq-guild--requests)
            (setq qq-guild--notice "")
            (condition-case err
                (funcall callback result)
              (error
               (setq qq-guild--notice (error-message-string err))
               (qq-guild--render))))))
      (lambda (_body reason)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (remhash method qq-guild--requests)
            (setq qq-guild--notice reason)
            (if (derived-mode-p 'qq-guild-compose-mode)
                (message "qq: %s（草稿已保留）" reason)
              (qq-guild--render)))))
      :owner owner :lifecycle-owner qq-runtime--surface-owner))
    (when (qq-request-active-p (cdr marker))
      (setq qq-guild--notice "加载中…")
      (unless (derived-mode-p 'qq-guild-compose-mode) (qq-guild--render)))
    (cdr marker)))

(defun qq-guild--merge (messages)
  "Validate and merge MESSAGES into this channel by exact sequence."
  (unless (and (listp messages)
               (cl-every (lambda (msg)
                           (and (qq-guild--message-p msg)
                                (qq-guild--same-channel-p
                                 qq-guild--channel (alist-get 'channel msg))))
                         messages))
    (error "qq: Invalid or misrouted guild messages"))
  (unless qq-guild--messages (setq qq-guild--messages (make-hash-table :test #'equal)))
  (dolist (msg messages)
    (puthash (alist-get 'sequence msg) (copy-tree msg) qq-guild--messages)))

(defun qq-guild--rows ()
  "Return loaded messages in channel sequence order."
  (sort (and qq-guild--messages (hash-table-values qq-guild--messages))
        (lambda (a b) (< (string-to-number (alist-get 'sequence a))
                         (string-to-number (alist-get 'sequence b))))))

(defun qq-guild--render (&rest _ignored)
  "Render this channel or directory while retaining the selected message."
  (unless (derived-mode-p 'qq-guild-compose-mode)
    (let ((inhibit-read-only t)
          (position (point))
          (sequence (get-text-property (point) 'qq-guild-sequence))
          (at-end (= (point) (point-max))))
      (erase-buffer)
      (insert (propertize qq-guild--title 'face 'bold) "\n")
      (insert (if qq-guild--channel
                  "g 刷新  h 更早消息  s 写消息  R 标记读到光标  q 返回\n"
                "g 刷新  RET 打开  q 返回\n"))
      (unless (string-empty-p qq-guild--notice)
        (insert (propertize qq-guild--notice 'face 'warning) "\n"))
      (when qq-guild--read-sequence
        (insert (format "已读至 %s\n" qq-guild--read-sequence)))
      (insert "\n")
      (if qq-guild--channel
          (dolist (msg (qq-guild--rows))
            (let ((start (point))
                  (name (alist-get 'sender_name msg))
                  (unsupported (alist-get 'unsupported_elements msg)))
              (insert (propertize
                       (format "%s · %s · #%s\n"
                               (if (string-empty-p name)
                                   (concat "Tiny " (alist-get 'sender_tiny_id msg)) name)
                               (format-time-string "%m-%d %H:%M"
                                                   (seconds-to-time (alist-get 'timestamp msg)))
                               (alist-get 'sequence msg))
                       'face 'font-lock-keyword-face))
              (insert (alist-get 'text msg))
              (when (> unsupported 0)
                (insert (format " [未支持的内容 × %d]" unsupported)))
              (insert "\n\n")
              (add-text-properties start (point)
                                   (list 'qq-guild-sequence (alist-get 'sequence msg)))))
        (let ((owner (qq-runtime-require-account-id)))
          (dolist (item qq-guild--items)
            (let ((entry (copy-tree item)))
              (when (or (null qq-guild--guild-id) (= (alist-get 'channel_type item) 1))
                (insert-text-button
                 (or (alist-get 'name item) "未命名频道")
                 'follow-link t
                 'action (if qq-guild--guild-id
                             (lambda (_button)
                               (qq-guild-open-channel (alist-get 'channel entry)
                                                      (alist-get 'name entry) owner))
                           (lambda (_button)
                             (qq-guild-open-channels (alist-get 'guild_id entry)
                                                     (alist-get 'name entry) owner))))
                (insert "\n"))))))
      (cond
       ((and sequence
             (text-property-any (point-min) (point-max) 'qq-guild-sequence sequence))
        (goto-char (text-property-any (point-min) (point-max) 'qq-guild-sequence sequence)))
       (at-end (goto-char (point-max)))
       (t (goto-char (min position (point-max)))))))
  nil)

(defun qq-guild--history (end)
  "Fetch one history range ending at END, or the observed head when nil."
  (qq-guild--call
   "guild.history" (append (list (cons 'channel qq-guild--channel) '(limit . 50))
                           (when end (list (cons 'end_sequence end))))
   (lambda (result)
     (let* ((history (alist-get 'history result))
            (begin (alist-get 'begin_sequence history))
            (end (alist-get 'end_sequence history)))
       (unless (and (qq-guild--same-channel-p qq-guild--channel (alist-get 'channel history))
                    (qq-protocol-uint64-decimal-p begin t)
                    (qq-protocol-uint64-decimal-p end t)
                    (<= (string-to-number begin) (string-to-number end)))
         (error "qq: Invalid guild history boundaries"))
       (qq-guild--merge (alist-get 'messages history))
       (when (or (null qq-guild--begin)
                 (< (string-to-number begin) (string-to-number qq-guild--begin)))
         (setq qq-guild--begin begin))
       (setq qq-guild--notice
             (format "本页序号 %s–%s%s" begin end
                     (if (> (or (alist-get 'unsupported_messages history) 0) 0)
                         "（包含暂未支持的消息）" "")))
       (qq-guild--render)))))

(defun qq-guild-refresh ()
  "Refresh this exact account's guild directory or channel history."
  (interactive)
  (cond
   (qq-guild--channel (qq-guild--history nil))
   (qq-guild--guild-id
    (qq-guild--call
     "guild.channels" (list (cons 'guild_id qq-guild--guild-id))
     (lambda (result)
       (let ((list (alist-get 'list result)))
         (unless (and (equal (alist-get 'guild_id list) qq-guild--guild-id)
                      (listp (alist-get 'channels list))
                      (cl-every (lambda (c)
                                  (and (qq-guild--channel-p (alist-get 'channel c))
                                       (equal (alist-get 'guild_id (alist-get 'channel c))
                                              qq-guild--guild-id)
                                       (stringp (alist-get 'name c))
                                       (natnump (alist-get 'channel_type c))))
                                (alist-get 'channels list)))
           (error "qq: Invalid guild channel directory"))
         (setq qq-guild--items (alist-get 'channels list))
         (qq-guild--render)))))
   (t
    (qq-guild--call
     "guild.list" nil
     (lambda (result)
       (unless (and (listp (alist-get 'guilds result))
                    (cl-every (lambda (g)
                                (and (qq-protocol-uint64-decimal-p (alist-get 'guild_id g))
                                     (stringp (alist-get 'name g))))
                              (alist-get 'guilds result)))
         (error "qq: Invalid guild directory"))
       (setq qq-guild--items (alist-get 'guilds result))
       (qq-guild--render))))))

(defun qq-guild-history-older ()
  "Load the range preceding the earliest returned boundary."
  (interactive)
  (unless qq-guild--channel (user-error "qq: 请先打开文字子频道"))
  (unless qq-guild--begin (user-error "qq: 请先加载当前消息"))
  (if (<= (string-to-number qq-guild--begin) 1)
      (message "qq: 已到序号起点")
    (qq-guild--history (number-to-string (1- (string-to-number qq-guild--begin))))))

(defun qq-guild-mark-read ()
  "Mark read through the loaded message at point after native confirmation."
  (interactive)
  (let ((sequence (get-text-property (point) 'qq-guild-sequence))
        (channel (copy-tree qq-guild--channel)))
    (unless sequence (user-error "qq: 请把光标放到要读到的消息上"))
    (qq-guild--call
     "guild.mark_read" (list (cons 'channel channel) (cons 'sequence sequence))
     (lambda (result)
       (let ((receipt (alist-get 'receipt result)))
         (unless (and (qq-guild--same-channel-p channel (alist-get 'channel receipt))
                      (qq-protocol-uint64-decimal-p (alist-get 'sequence receipt)))
           (error "qq: Invalid guild read receipt"))
         (setq qq-guild--read-sequence (alist-get 'sequence receipt))
         (qq-guild--render))))))

(defvar qq-guild-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'qq-guild-refresh)
    (define-key map (kbd "h") #'qq-guild-history-older)
    (define-key map (kbd "s") #'qq-guild-compose)
    (define-key map (kbd "R") #'qq-guild-mark-read)
    map))

(define-derived-mode qq-guild-mode special-mode "QQ频道"
  "Browse QQ Guild directories and text channels."
  (add-hook 'kill-buffer-hook #'qq-guild--cancel nil t)
  (setq-local revert-buffer-function (lambda (&rest _) (qq-guild-refresh))))

(defun qq-guild--open (owner id title guild channel)
  "Open account OWNER's Surface ID with TITLE, GUILD and CHANNEL."
  (unless (qq-rpc-method-available-p "guild.list")
    (user-error "qq: 当前网关未提供 QQ 频道功能"))
  (let* ((surface
          (qq-runtime-open-account-surface
           :account-id owner :id id :mode 'qq-guild-mode
           :buffer-name (qq-runtime-account-buffer-name "guild" (format "%s" id) owner)
           :render-function #'qq-guild--render :select t
           :setup (lambda (_surface)
                    (setq qq-guild--guild-id guild
                          qq-guild--channel channel qq-guild--title title
                          qq-guild--phase (alist-get 'phase (qq-account-get owner))))))
         (buffer (appkit-surface-buffer surface)))
    (with-current-buffer buffer (qq-guild-refresh))
    buffer))

;;;###autoload
(defun qq-guild-open ()
  "Open the selected account's joined QQ Guild directory."
  (interactive)
  (qq-guild--open (qq-runtime-require-account-id "打开 QQ 频道")
                  '(guild-directory) "QQ 频道" nil nil))

(defun qq-guild-open-channels (guild title &optional owner)
  "Open GUILD's text channels with TITLE for OWNER."
  (unless (qq-protocol-uint64-decimal-p guild) (user-error "qq: Invalid guild ID"))
  (qq-guild--open (or owner (qq-runtime-require-account-id))
                  (list 'guild-channels guild) (concat title " · 文字子频道") guild nil))

(defun qq-guild-open-channel (channel title &optional owner)
  "Open exact text CHANNEL with TITLE for OWNER."
  (unless (qq-guild--channel-p channel) (user-error "qq: Invalid channel route"))
  (qq-guild--open (or owner (qq-runtime-require-account-id))
                  (list 'guild-channel (alist-get 'guild_id channel)
                        (alist-get 'channel_id channel))
                  title (alist-get 'guild_id channel) (copy-tree channel)))

(defvar qq-guild-compose-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map (kbd "C-c C-c") #'qq-guild-send)
    (define-key map (kbd "C-c C-k") #'quit-window)
    map))

(define-derived-mode qq-guild-compose-mode text-mode "QQ频道草稿"
  "Compose text for one exact guild channel; C-c C-c sends."
  (setq-local header-line-format "C-c C-c 发送 · 失败时保留草稿")
  (add-hook 'kill-buffer-hook #'qq-guild--cancel nil t))

(defun qq-guild-compose ()
  "Open a multiline draft for the current text channel."
  (interactive)
  (unless qq-guild--channel (user-error "qq: 请先打开文字子频道"))
  (let ((channel (copy-tree qq-guild--channel))
        (title qq-guild--title)
        (source (current-buffer))
        (owner (qq-runtime-require-account-id)))
    (qq-runtime-open-account-surface
     :account-id owner :id (list 'guild-draft channel)
     :mode 'qq-guild-compose-mode :render-function #'ignore :select t
     :buffer-name (qq-runtime-account-buffer-name "guild-draft" title owner)
     :setup (lambda (_surface)
              (setq qq-guild--channel channel qq-guild--title title qq-guild--source source)))))

(defun qq-guild-send ()
  "Send this draft once; keep it on failure or an uncertain outcome."
  (interactive)
  (unless (derived-mode-p 'qq-guild-compose-mode) (user-error "qq: 请在频道草稿中发送"))
  (when (and qq-guild--requests (gethash "guild.send_text" qq-guild--requests))
    (user-error "qq: 正在等待发送结果"))
  (let ((text (buffer-substring-no-properties (point-min) (point-max)))
        (channel (copy-tree qq-guild--channel))
        (source qq-guild--source))
    (when (string-empty-p (string-trim text)) (user-error "qq: 消息不能为空"))
    (qq-guild--call
     "guild.send_text" (list (cons 'channel channel) (cons 'text text))
     (lambda (result)
       (let* ((receipt (alist-get 'receipt result))
              (sequence (alist-get 'sequence receipt)))
         (unless (and (qq-guild--same-channel-p channel (alist-get 'channel receipt))
                      (qq-protocol-uint64-decimal-p sequence))
           (error "qq: Invalid guild send receipt"))
         ;; Edits made while the send was in flight belong to the next draft.
         (when (equal text (buffer-substring-no-properties (point-min) (point-max)))
           (erase-buffer) (set-buffer-modified-p nil))
         (message "qq: 频道消息已发送，序号 %s" sequence)
         (when (buffer-live-p source)
           (with-current-buffer source (qq-guild--history sequence))))))))

(defun qq-guild--event (_event data)
  "Apply account-scoped guild observation DATA to existing views."
  (let ((owner (alist-get 'account_id data))
        (messages (alist-get 'messages data))
        (channels (alist-get 'channels data)))
    (when (and (stringp owner) (listp messages) (listp channels)
               (cl-every #'qq-guild--message-p messages))
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (and (derived-mode-p 'qq-guild-mode) qq-guild--channel
                     (equal owner qq-runtime--account-id))
            (let ((matching
                   (seq-filter (lambda (msg)
                                 (qq-guild--same-channel-p qq-guild--channel
                                                          (alist-get 'channel msg)))
                               messages))
                  (state (seq-find (lambda (state)
                                     (qq-guild--same-channel-p
                                      qq-guild--channel (alist-get 'channel state)))
                                   channels)))
              (when matching (qq-guild--merge matching))
              (when (and state (qq-protocol-uint64-decimal-p
                                (alist-get 'read_sequence state) t))
                (let ((read (alist-get 'read_sequence state)))
                  (when (>= (string-to-number read)
                            (string-to-number (or qq-guild--read-sequence "0")))
                    (setq qq-guild--read-sequence read))))
              (when (or matching state) (qq-guild--render))
              (when (and state (null qq-guild--begin)
                         (not (and qq-guild--requests
                                   (gethash "guild.history" qq-guild--requests))))
                (qq-guild--history nil)))))))))

(defun qq-guild--account-changed (reason account-id)
  "Revoke stale work and refresh open guild views on account lifecycle changes."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (or (derived-mode-p 'qq-guild-mode)
                     (derived-mode-p 'qq-guild-compose-mode))
                 (or (null account-id) (equal account-id qq-runtime--account-id)))
        (let ((phase (alist-get 'phase (qq-account-get qq-runtime--account-id))))
          (when (or (memq reason '(ready resync removed))
                    (not (equal phase qq-guild--phase)))
            (qq-guild--cancel)
            (setq qq-guild--phase phase)
            (unless (derived-mode-p 'qq-guild-compose-mode)
              (setq qq-guild--notice "频道状态待刷新")
              (qq-guild--render)
              (when (and (equal phase "online")
                         (qq-rpc-method-available-p "guild.list"))
                (qq-guild-refresh)))))))))

(qq-rpc-register-event "guild.observed" #'qq-guild--event)
(add-hook 'qq-account-registry-changed-hook #'qq-guild--account-changed)
(provide 'qq-guild)
;;; qq-guild.el ends here
