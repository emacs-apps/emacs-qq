;;; qq-reply.el --- Shared reply sources and previews -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'seq)
(require 'appkit-resource)
(require 'appkit-scroll)
(require 'qq-message)
(require 'qq-request)
(require 'qq-runtime)

(declare-function qq-chat--reply-preview-text "qq-chat" (message))
(declare-function qq-chat--message-sender-color-key "qq-chat" (message))

(defvar-local qq-reply--viewport nil)

(defun qq-reply-data (message)
  "Return MESSAGE's normalized native reply metadata."
  (alist-get 'data (seq-find (lambda (s) (equal (alist-get 'type s) "reply"))
                             (alist-get 'segments message))))

(defun qq-reply-locator (message &optional session)
  "Return MESSAGE's resolved canonical source row locator, or nil."
  (when-let* ((source (qq-reply-source message session)))
    (qq-message-history-locator source)))

(defun qq-reply-key (message &optional session)
  "Return the account-scoped source resource key for canonical reply MESSAGE."
  (let ((session (or session (alist-get 'session-key message)))
        (row (alist-get 'canonical-row-key message)))
    (when (and session (qq-reply-data message)
               (memq (qq-state-session-key-type session) '(private group))
               (qq-protocol-uint64-decimal-p row))
      (list :reply (qq-runtime-current-account-id) (qq-server-gateway-instance-id)
            session row))))

(defun qq-reply-source (message &optional session)
  "Return MESSAGE's backend-resolved source retained by this Surface."
  (when-let* ((surface (appkit-current-surface))
              (key (qq-reply-key message session))
              (state (appkit-resource-state surface key))
              ((eq (appkit-resource-state-status state) 'ready)))
    (qq-state-message-apply-tombstones
     (qq-runtime-state) (nth 3 key) (appkit-resource-state-value state))))

(defun qq-reply-preview-model (data source)
  "Build shared inline presentation from SOURCE or DATA's native quote.
An embedded quote supplies presentation only, never a synthetic message."
  (let* ((sender (or (and source (alist-get 'sender-name source))
                     (alist-get 'sender_name data)
                     (alist-get 'uin (alist-get 'sender data))))
         (preview (if source (qq-chat--reply-preview-text source)
                    (alist-get 'preview data))))
    (list :sender sender
          :sender-key (or (and source (qq-chat--message-sender-color-key source))
                          (alist-get 'uin (alist-get 'sender data))
                          (alist-get 'uid (alist-get 'sender data))
                          sender)
          :body (if (and (stringp preview) (not (string-empty-p preview)))
                    preview
                  "[原消息暂不可用]"))))

(defun qq-reply--check-source (state result key)
  "Validate the reply record scope and canonical source item in RESULT."
  (pcase-let ((`(:reply ,account ,instance ,session ,row-key) key))
    (unless (equal instance (qq-server-gateway-instance-id))
      (error "qq: Reply source belongs to an obsolete Gateway"))
    (unless (and (qq-server-wire-exact-object-keys-p
                  result '(account_id conversation reply_row_key source))
                 (equal account (alist-get 'account_id result))
                 (equal row-key (alist-get 'reply_row_key result))
                 (qq-message--history-conversation-equal-p
                  (alist-get 'conversation result)
                  (qq-message--history-conversation-params session)))
      (error "qq: Reply source response contradicts its record scope"))
    (let* ((native (qq-message--history-item (alist-get 'source result) "native" "Reply source"))
           (message (qq-message-normalize-snapshot
                     state native account (qq-account-get account)
                     (alist-get 'canonical-recalled-p native) t)))
      (unless (equal session (alist-get 'session-key message))
        (error "qq: Reply source belongs to another conversation"))
      message)))

(defun qq-reply--load (_context key success failure)
  "Acquire KEY's canonical source from the backend's message record."
  (pcase-let ((`(:reply ,account ,instance ,session ,row-key) key))
    (if (not (equal instance (qq-server-gateway-instance-id)))
        (progn (funcall failure "Gateway changed")
               (appkit-cancellation-create :cancel #'ignore))
      (let ((request
             (qq-request-start
              (lambda (ok bad)
                (qq-runtime-with-account account
                  (qq-message--call
                   "message.get_reply_source" account
                   `((conversation . ,(qq-message--history-conversation-params session))
                     (row_key . ,row-key))
                   :projector (lambda (state result)
                                (cons state (qq-reply--check-source state result key)))
                   :callback ok :errback bad)))
              :owner account :callback success
              :errback (lambda (_body reason) (funcall failure reason)))))
        (appkit-cancellation-create :cancel (lambda () (qq-request-cancel request)))))))

(defun qq-reply--visible-anchors ()
  "Return message anchors intersecting this buffer's visible windows."
  (let (anchors)
    (dolist (window (get-buffer-window-list (current-buffer) nil t))
      (let ((position (window-start window))
            (end (or (window-end window t) (point-max))))
        (while (< position end)
          (when-let* ((anchor (get-text-property position 'qq-chat-message-anchor)))
            (cl-pushnew anchor anchors :test #'equal))
          (setq position (next-single-property-change
                          position 'qq-chat-message-anchor nil end)))))
    (nreverse anchors)))

(defun qq-reply-check-viewport (&rest _ignored)
  "Ask the current Surface to update reply interests when the viewport changes."
  (when-let* ((surface (appkit-current-surface))
              ((appkit-surface-live-p surface)))
    (let ((anchors (qq-reply--visible-anchors)))
      (unless (equal anchors qq-reply--viewport)
        (setq qq-reply--viewport anchors)
        (qq-runtime-invalidate surface :frame-p t)))))

(defun qq-reply-render-result (_surface messages &optional session)
  "Request backend-resolved sources for visible canonical reply records."
  (setq qq-reply--viewport (qq-reply--visible-anchors))
  (let ((demands (make-hash-table :test #'equal))
        (interests (make-hash-table :test #'equal)))
    (dolist (message messages)
      (let ((row (qq-state-message-anchor message)))
        (when (member row qq-reply--viewport)
          (when-let* ((key (qq-reply-key message session)))
            (when (or (gethash key demands) (< (hash-table-count demands) 64))
              (puthash key (appkit-resource-demand-create
                            :key key :input key :loader #'qq-reply--load
                            :acquisition-identity key :sharing-policy 'app-private
                            :cache-policy 'while-interested) demands)
              (puthash key (cons row (gethash key interests)) interests))))))
    (appkit-render-result-create
     :resource-demands (hash-table-values demands)
     :resource-interest-update
     (appkit-resource-interest-update-create
      :mode 'replace
      :entries (mapcar (lambda (key)
                         (appkit-resource-interest-create
                          :key key :row-keys (delete-dups (gethash key interests))))
                       (hash-table-keys interests))))))

(provide 'qq-reply)
;;; qq-reply.el ends here
