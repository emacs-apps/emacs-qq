;;; qq-guild-chat.el --- Guild timelines on Appkit -*- lexical-binding: t; -*-
;;; Commentary:
;; Protocol objects belong to the account App.  The Surface owns range edges
;; and request tokens; Appkit owns keyed rows, composer contents and positions.
;;; Code:
(require 'qq-guild)
(require 'qq-guild-avatar)
(require 'appkit-chatbuf)
(require 'appkit-chat-timeline)
(require 'appkit-chat-read)
(require 'appkit-bindings)

(defun qq-guild-chat--request (context model kind input)
  "Request KIND with INPUT for this channel MODEL."
  (let ((token (make-symbol "guild-request")))
    (setq model (plist-put (copy-sequence model) (intern (format ":%s-token" kind)) token))
    (when (eq kind 'history)
      (setq model (plist-put model :history-error nil))
      (setq model (plist-put model :history-kind (plist-get input :kind))))
    (appkit-next
     :model (plist-put model :notice nil)
     :render (appkit-projection-change-create
              :full-p t
              :frame-p t)
     :commands (list (qq-guild--request-command context kind (plist-get model :channel) input token)))))

(defun qq-guild-chat--composer-effect (capture)
  "Settle CAPTURE in this exact composer after the App confirmed sending."
  (let ((buffer (current-buffer)) (surface (appkit-current-surface)))
    (appkit-command-start-effect
     (appkit-effect-create
      :key 'guild-composer-settle
      :input capture
      :start
      (lambda (_context value _observe resolve _reject)
        (when (and (appkit-surface-live-p surface) (buffer-live-p buffer))
          (with-current-buffer buffer
            (when (and (eq surface (appkit-current-surface))
                       (= (plist-get value :revision) (appkit-chatbuf-composer-revision))
                       (equal (plist-get value :text) (appkit-chatbuf-input-string)))
              (appkit-chatbuf-input-history-push (plist-get value :text))
              (appkit-with-edit-transaction surface (appkit-chatbuf-input-replace "")))))
        (funcall resolve) nil)
      :success (lambda (_input) 'composer-settled)
      :failure (lambda (_input &rest _) 'composer-settled)))))

(defun qq-guild-chat--update (context model message)
  "Handle navigation, App replies and state notifications."
  (pcase message
    ('refresh (qq-guild-chat--request context model 'history '(:kind latest)))
    ('older
     (if (or (plist-get model :history-token) (null (plist-get model :begin))
             (<= (string-to-number (plist-get model :begin)) 1))
         (appkit-next-reject 'history-not-pageable)
       (qq-guild-chat--request
        context model 'history
        (list :kind 'older :end (number-to-string (1- (string-to-number (plist-get model :begin))))))))
    (`(send ,capture)
     (if (plist-get model :send-token) (appkit-next-reject 'send-in-flight)
       (qq-guild-chat--request context (plist-put (copy-sequence model) :capture capture) 'send capture)))
    (`(read ,sequence) (qq-guild-chat--request context model 'read (list :sequence sequence)))
    (`(qq-guild-reply ,kind ,token ,value ,reason)
     (let ((key (intern (format ":%s-token" kind))))
       (if (not (eq token (plist-get model key)))
           (appkit-next
            :model model
            :render appkit-render-none)
         (setq model (plist-put (copy-sequence model) key nil))
         (if reason
             (progn
               (when (eq kind 'history) (setq model (plist-put model :history-error t)))
               (appkit-next
                :model (plist-put model :notice
                                  (if (eq kind 'send)
                                      (concat "发送结果未确认；草稿已保留，请确认后再发送。 " reason)
                                    reason))
                :render (appkit-projection-change-create
                         :frame-p t)))
           (pcase kind
             ('history
              (let ((begin (alist-get 'begin_sequence value)))
                (setq model (plist-put model :begin
                                       (if (and (not (eq (plist-get model :history-kind) 'latest))
                                                (plist-get model :begin))
                                           (number-to-string
                                            (min (string-to-number begin)
                                                 (string-to-number (plist-get model :begin))))
                                         begin))
                      model (plist-put model :end (alist-get 'end_sequence value))
                      model (plist-put model :notice
                                       (when (> (alist-get 'unsupported_messages value) 0)
                                         "本页包含暂未支持的消息；分页仍按服务端序号继续。")))
                (appkit-next
                 :model model
                 :render (appkit-projection-change-create
                          :full-p t
                          :frame-p t)
                 :commands (list (qq-guild-chat--viewport-effect)))))
             ('send
              (let ((next (qq-guild-chat--request context model 'history
                                                  (list :kind 'sent :end (alist-get 'sequence value)))))
                (appkit-next
                 :model (appkit-next-model next)
                 :render (appkit-next-render next)
                 :commands (cons (qq-guild-chat--composer-effect (plist-get model :capture))
                                 (appkit-next-commands next)))))
             (_ (appkit-next
                 :model model
                 :render (appkit-projection-change-create
                          :full-p t
                          :frame-p t))))))))
    (`(session ,phase)
     (setq model (copy-sequence model))
     (dolist (key '(:history-token :send-token :read-token :begin :end))
       (setq model (plist-put model key nil)))
     (if (equal phase "online") (qq-guild-chat--request context model 'history '(:kind latest))
       (appkit-next
        :model (plist-put model :notice "账号未在线；草稿已保留")
        :render (appkit-projection-change-create
                 :full-p t
                 :frame-p t))))
    ('changed
     (let* ((state (qq-guild--from-view (appkit-transition-context-app-read-view context)))
            (cursor (qq-guild--get state :states (qq-guild--key (plist-get model :channel))))
            (head (alist-get 'latest_sequence cursor)))
       ;; Retry a previously unknown head once per first-view cursor.
       (if (and (null (plist-get model :begin)) (null (plist-get model :history-token))
                head (not (equal head (plist-get model :attempted-head))))
           (qq-guild-chat--request context
                                   (plist-put (copy-sequence model) :attempted-head head)
                                   'history '(:kind latest))
         (appkit-next
          :model model
          :render (appkit-projection-change-create
                   :full-p t
                   :frame-p t)))))
    ((pred appkit-projection-change-p) (appkit-next
                                        :model model
                                        :render message))
    ((pred appkit-delivery-report-p)
     (let ((token (appkit-delivery-report-correlation message)))
       (setq model (copy-sequence model))
       (dolist (key '(:history-token :send-token :read-token))
         (when (eq token (plist-get model key)) (setq model (plist-put model key nil)))))
     (appkit-next
      :model (plist-put model :notice "请求未能交付；请刷新重试")
      :render (appkit-projection-change-create
               :frame-p t)))
    ((or 'composer-settled 'viewport-checked) (appkit-next
                                               :model model
                                               :render appkit-render-none))
    (_ (appkit-next-reject 'unknown-guild-chat-message))))

(defun qq-guild-chat--messages (state model)
  "Read messages inside this Surface's established history range."
  (when-let* ((begin (plist-get model :begin)))
    (seq-filter (lambda (msg) (>= (string-to-number (alist-get 'sequence msg))
                                  (string-to-number begin)))
                (qq-guild--get state :messages (qq-guild--key (plist-get model :channel))))))

(defun qq-guild-chat--read-sequence (state channel)
  "Return confirmed read progress, or nil when unknown."
  (let* ((key (qq-guild--key channel))
         (reported (alist-get 'read_sequence (qq-guild--get state :states key)))
         (receipt (qq-guild--get state :reads key)))
    (when (or receipt reported)
      (number-to-string (max (string-to-number (or receipt "0"))
                             (string-to-number (or reported "0")))))))

(defun qq-guild-chat--row-context (previous message boundary)
  "Build grouping and divider context for MESSAGE following PREVIOUS."
  (let* ((time (alist-get 'timestamp message))
         (day (format-time-string "%Y-%m-%d" (seconds-to-time time)))
         (previous-day (and previous (format-time-string "%Y-%m-%d"
                                                         (seconds-to-time (alist-get 'timestamp previous)))))
         (unread (equal boundary (alist-get 'sequence message)))
         (new-day (not (equal day previous-day))))
    (list :day (when new-day day) :unread unread
          :compact (and previous (not unread) (not new-day)
                        (equal (alist-get 'sender_tiny_id previous) (alist-get 'sender_tiny_id message))
                        (<= 0 (- time (alist-get 'timestamp previous)) 300)))))

(defun qq-guild-chat--avatar-key (message)
  "Return the sender resource key for MESSAGE."
  (qq-guild-avatar-key (alist-get 'guild_id (alist-get 'channel message))
                       (alist-get 'sender_tiny_id message)))

(defun qq-guild-chat--print (row)
  "Print message ROW with shared Appkit chat presentation leaves."
  (let* ((msg (appkit-chat-timeline-row-payload row))
         (context (appkit-chat-timeline-row-context row))
         (width (or (appkit-surface-responsive-width (appkit-current-surface)) fill-column))
         (start (point)))
    (when (plist-get context :day)
      (appkit-chat-ins-insert-full-width-divider (plist-get context :day) 'shadow width '(appkit-chat-read-divider t)))
    (when (plist-get context :unread) (appkit-chat-read-insert-divider width))
    (unless (plist-get context :compact)
      (let ((heading (point)) (name (alist-get 'sender_name msg)))
        (insert (appkit-chat-avatar-one-line-string
                 (qq-guild-avatar-image (qq-guild-chat--avatar-key msg)) "@") " ")
        (insert (propertize (if (string-empty-p name) (concat "Tiny " (alist-get 'sender_tiny_id msg)) name)
                            'face (appkit-name-color-face (alist-get 'sender_tiny_id msg)))
                (propertize (format "  %s\n" (format-time-string "%H:%M" (seconds-to-time (alist-get 'timestamp msg))))
                            'face 'shadow))
        (add-face-text-property heading (point) 'appkit-chat-heading t)))
    (let ((body (point)))
      (insert (alist-get 'text msg))
      (when (> (alist-get 'unsupported_elements msg) 0)
        (insert (propertize (format " [暂未支持的内容 × %d]" (alist-get 'unsupported_elements msg)) 'face 'shadow)))
      (insert "\n")
      (appkit-ui-apply-line-prefix body (point) "  "))
    (insert "\n")
    (add-text-properties start (point) (list 'qq-guild-sequence (alist-get 'sequence msg)))))

(defun qq-guild-chat--mount (surface _view model)
  "Mount a fresh timeline while retaining canonical composer contents."
  (qq-runtime-bind-account (plist-get model :account))
  (setq-local qq-runtime--surface-owner surface)
  (appkit-chatbuf-with-generated-update
    (appkit-chatbuf-clear-prompt-and-input)
    (appkit-chat-timeline-ensure
     :printer #'qq-guild-chat--print
     :anchor-property 'qq-guild-sequence
     :after-mutation-function #'appkit-chatbuf-update-context-mode)
    (appkit-chatbuf-install-prompt "消息 > ")
    (appkit-chatbuf-input-replace (appkit-chatbuf-input-state))
    (appkit-chatbuf-protect-generated-content)
    (appkit-chatbuf-input-apply-text-properties)))

(defun qq-guild-chat--render (_surface view model change)
  "Project committed VIEW and MODEL without rewriting the composer."
  (let* ((state (qq-guild--from-view view))
         (qq-guild-avatar--epoch (plist-get state :epoch))
         (channel (plist-get model :channel))
         (messages (qq-guild-chat--messages state model))
         (read (qq-guild-chat--read-sequence state channel))
         (cursor (qq-guild--get state :states (qq-guild--key channel)))
         (first (and read (plist-get model :begin)
                     (<= (string-to-number (plist-get model :begin)) (1+ (string-to-number read)))
                     (seq-find (lambda (msg) (> (string-to-number (alist-get 'sequence msg))
                                                (string-to-number read))) messages)))
         (boundary (appkit-chat-read-update
                    (mapcar (lambda (msg) (alist-get 'sequence msg)) messages)
                    :known-p (and read first t)
                    :first-unread-key (alist-get 'sequence first)
                    :all-read-p (and read cursor
                                     (>= (string-to-number read) (string-to-number (alist-get 'latest_sequence cursor))))))
         (send-operation (qq-guild--get state :operations (qq-guild--operation-key 'send channel)))
         (status (cond ((plist-get model :send-token) "发送中…")
                       ((plist-get model :read-token) "正在确认已读…")
                       ((plist-get model :history-token) "正在加载历史…")
                       ((plist-get model :notice))
                       ((plist-get send-operation :unknown) (plist-get send-operation :error))
                       ((null (plist-get model :begin)) "历史尚未就绪")
                       ((null messages) "当前范围内没有文字消息")
                       (t ""))))
    (setq-local header-line-format (concat (plist-get model :title) (when read (format " · 已读至 %s" read))))
    (appkit-chat-timeline-sync
     (appkit-chat-timeline-project
      messages (lambda (msg) (alist-get 'sequence msg))
      :context-function (lambda (previous msg) (qq-guild-chat--row-context previous msg boundary))
      :dependencies-function (lambda (msg) (list (qq-guild-chat--avatar-key msg))))
     :changed-resources (and (appkit-projection-change-p change) (appkit-projection-change-resources change)))
    (appkit-chat-timeline-set-frame
     (propertize (concat (if (and (plist-get model :begin) (<= (string-to-number (plist-get model :begin)) 1))
                             "已到历史起点" "向上滚动加载更早消息") "\n\n") 'face 'shadow)
     (concat (unless (string-empty-p status) (propertize (concat status "\n") 'face 'shadow)) "\n"))
    (appkit-chatbuf-protect-generated-content)
    (appkit-chatbuf-input-apply-text-properties)
    (qq-guild-avatar-result
     (when (display-graphic-p)
       (mapcar (lambda (msg) (cons (alist-get 'sequence msg) (qq-guild-chat--avatar-key msg))) messages)))))

(defun qq-guild-chat--recover (surface view model _condition)
  "Discard damaged rows, retaining semantic positions and the canonical draft."
  (let ((position (appkit-position-capture
                   :anchor-property 'qq-guild-sequence
                   :preserve-window-start t))
        (input-offset (when (appkit-chatbuf-point-in-input-p)
                        (- (point) (appkit-chatbuf-input-start-position))))
        (windows (appkit-chatbuf-capture-window-input-offsets)))
    (appkit-chat-timeline-reset)
    (qq-guild-chat--mount surface view model)
    (qq-guild-chat--render surface view model nil)
    (appkit-position-restore position)
    (when input-offset
      (goto-char (min (point-max) (+ (appkit-chatbuf-input-start-position) input-offset))))
    (appkit-chatbuf-restore-window-input-offsets windows))
  nil)

(defun qq-guild-chat--renderer (_surface)
  "Create a shared timeline renderer with canonical draft recovery."
  (appkit-generated-renderer-create
   :mount #'qq-guild-chat--mount
   :unmount #'ignore
   :merge #'appkit-projection-change-merge
   :render #'qq-guild-chat--render
   :recover #'qq-guild-chat--recover))

(defun qq-guild-chat--model ()
  "Return the current Guild Surface model."
  (unless (derived-mode-p 'qq-guild-chat-mode) (user-error "qq: 请先打开文字子频道"))
  (appkit-surface-model (appkit-current-surface)))

(defun qq-guild-refresh ()
  "Refresh the channel's latest history window."
  (interactive) (qq-guild-chat--model)
  (appkit-surface-send (appkit-current-surface) 'refresh))

(defun qq-guild-history-older ()
  "Load the exact range preceding the visible window."
  (interactive) (qq-guild-chat--model)
  (appkit-surface-send (appkit-current-surface) 'older))

(defun qq-guild-compose ()
  "Enter the inline channel composer."
  (interactive) (qq-guild-chat--model) (appkit-chatbuf-enter-input))

(defun qq-guild-send ()
  "Submit the captured draft once, preserving later edits."
  (interactive)
  (let ((model (qq-guild-chat--model)) (text (appkit-chatbuf-input-string)))
    (when (plist-get model :send-token) (user-error "qq: 正在等待发送结果"))
    (when (or (not text) (string-empty-p (string-trim text))) (user-error "qq: 消息不能为空"))
    (when (appkit-chatbuf-input-has-objects-p) (user-error "qq: 当前频道仅支持纯文本发送"))
    (when (> (string-bytes text) 16000) (user-error "qq: 文字消息超过 16000 字节"))
    (appkit-surface-send (appkit-current-surface)
                         (list 'send (list :text (substring-no-properties text)
                                           :revision (appkit-chatbuf-composer-revision))))))

(defun qq-guild-mark-read ()
  "Mark through the exact row at point, or the newest loaded composer row."
  (interactive) (qq-guild-chat--model)
  (let ((row (appkit-chat-read-target)))
    (unless row (user-error "qq: 请把光标放在消息或输入框中"))
    (appkit-surface-send (appkit-current-surface) (list 'read (appkit-chat-timeline-row-key row)))))

(defun qq-guild-chat-return ()
  "Dispatch Return by timeline/composer geometry."
  (interactive) (appkit-chatbuf-dispatch-return #'qq-guild-send))

(defun qq-guild-chat-copy-text ()
  "Copy the original message text at point."
  (interactive)
  (let* ((key (appkit-chat-timeline-key-at-point)) (row (and key (appkit-chat-timeline-row key))))
    (unless row (user-error "qq: 光标处没有消息"))
    (kill-new (alist-get 'text (appkit-chat-timeline-row-payload row)))))

(defun qq-guild-chat--move (delta)
  "Move DELTA messages through the shared timeline."
  (let* ((keys (appkit-chat-timeline-keys))
         (index (cl-position (appkit-chat-timeline-key-at-point) keys :test #'equal))
         (next (if index (+ index delta) (if (> delta 0) 0 (1- (length keys))))))
    (when-let* ((key (and (<= 0 next) (< next (length keys)) (nth next keys)))
                (position (appkit-chat-timeline-key-position key)))
      (goto-char position))))

(defun qq-guild-chat-next () (interactive) (qq-guild-chat--move 1))

(defun qq-guild-chat-previous () (interactive) (qq-guild-chat--move -1))

(defvar-keymap qq-guild-chat-timeline-mode-map
  :doc "Guild message actions."
  "R" #'qq-guild-mark-read)
(appkit-bindings-define 'qq-guild-chat-timeline-mode-map 'read-only
                        'refresh #'qq-guild-refresh 'history-older #'qq-guild-history-older
                        'compose #'qq-guild-compose 'mark-read #'qq-guild-mark-read
                        'next-item #'qq-guild-chat-next 'previous-item #'qq-guild-chat-previous 'quit #'quit-window)

(define-minor-mode qq-guild-chat-timeline-mode
  "Message actions outside the editable Guild composer."
  :init-value nil
  :lighter nil
  :keymap qq-guild-chat-timeline-mode-map)

(defvar-keymap qq-guild-chat-mode-map
  :doc "Guild chat and composer actions."
  :parent appkit-chatbuf-mode-map
  "C-c r" #'qq-guild-mark-read
  "C-c w" #'qq-guild-chat-copy-text
  "M-p" #'appkit-chatbuf-input-history-prev
  "M-n" #'appkit-chatbuf-input-history-next)
(appkit-bindings-define 'qq-guild-chat-mode-map 'input
                        'return #'qq-guild-chat-return 'submit #'qq-guild-send 'newline #'newline 'latest #'qq-guild-refresh)

(define-derived-mode qq-guild-chat-mode appkit-chatbuf-mode "QQ-Guild"
  "QQ text channel with a shared timeline and inline composer."
  (setq-local line-spacing 0)
  (appkit-chatbuf-reset-state 32)
  (appkit-chatbuf-use-timeline-mode #'qq-guild-chat-timeline-mode))

(defconst qq-guild-chat--type
  (appkit-surface-type-create
   :name 'qq-guild-chat
   :mode #'qq-guild-chat-mode
   :init (lambda (context input) (qq-guild-chat--request context input 'history '(:kind latest)))
   :update #'qq-guild-chat--update
   :subscriptions
   (lambda (model)
     (list (appkit-subscription-create
            :topic (cons 'qq-guild-channel (qq-guild--key (plist-get model :channel)))
            :notify (lambda (_) 'changed))
           (appkit-subscription-create
            :topic 'qq-guild-session
            :notify (lambda (phase) (list 'session phase)))))
   :renderer-factory #'qq-guild-chat--renderer))

(defvar-local qq-guild-chat--observer nil
  "Presentation observer owned by the exact channel Surface.")

(defun qq-guild-chat--observe (surface)
  "Attach the shared scroll observer independently of timeline cache recovery."
  (unless (and (appkit-scroll-observer-p qq-guild-chat--observer)
               (appkit-scroll-observer-active-p qq-guild-chat--observer))
    (setq-local
     qq-guild-chat--observer
     (appkit-scroll-observer-install
      surface
      :end-boundary-function #'appkit-chat-timeline-footer-start-position
      :start-function
      (lambda (_window position _start)
        (let ((model (appkit-surface-model surface)))
          (when (and (not (appkit-chatbuf-point-in-input-p))
                     (plist-get model :begin) (not (plist-get model :history-token))
                     (not (plist-get model :history-error))
                     (> (string-to-number (plist-get model :begin)) 1)
                     (appkit-scroll-near-start-p position (point-min) 300))
            (appkit-surface-send surface 'older)))))))
  qq-guild-chat--observer)

(defun qq-guild-chat--viewport-effect ()
  "Check the committed viewport outside its rendering/transition pass."
  (let ((surface (appkit-current-surface)) (buffer (current-buffer)))
    (appkit-command-start-effect
     (appkit-effect-create
      :key 'guild-check-viewport
      :start
      (lambda (_context _input _observe resolve _reject)
        (let ((timer
               (run-at-time
                0 nil
                (lambda ()
                  (when (and (appkit-surface-live-p surface) (buffer-live-p buffer))
                    (with-current-buffer buffer
                      (appkit-scroll-observer-check (qq-guild-chat--observe surface))))
                  (funcall resolve)))))
          (appkit-cancellation-create
           :kind 'logical
           :cancel (lambda () (cancel-timer timer)))))
      :success (lambda (_) 'viewport-checked)
      :failure (lambda (_ &rest _) 'viewport-checked)))))

(defun qq-guild-chat-open (channel title &optional owner)
  "Open or reuse native text CHANNEL for OWNER."
  (unless (qq-guild--channel-p channel) (user-error "qq: 无效的频道标识"))
  (let* ((owner (or owner (qq-runtime-require-account-id)))
         (surface (qq-guild--open-surface
                   qq-guild-chat--type owner (cons 'guild-channel (qq-guild--key channel))
                   (list :channel (copy-tree channel) :title title) title)))
    (with-current-buffer (appkit-surface-buffer surface)
      (qq-guild-chat--observe surface)
      (appkit-surface-enable-responsive-geometry
       surface (lambda (surface _width)
                 (appkit-surface-post surface (appkit-projection-change-create
                                               :geometry-p t))))
      (appkit-surface-refresh-responsive-geometry surface))
    (appkit-surface-buffer surface)))

(provide 'qq-guild-chat)
;;; qq-guild-chat.el ends here
