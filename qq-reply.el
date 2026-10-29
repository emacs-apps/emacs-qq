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

(defun qq-reply-source-matches-p (data message kind)
  "Whether MESSAGE has the source facts in reply DATA for conversation KIND."
  (let ((sender (alist-get 'sender data))
        (sequence (or (alist-get 'message_seq data) (alist-get 'sequence data)))
        (id (or (alist-get 'message_id data) (alist-get 'id data))))
    (and (not (qq-state-service-message-p message))
         (if sequence
             (equal sequence (alist-get (if (eq kind 'private)
                                            'native-client-sequence 'message-seq)
                                        message))
           (and id (equal id (alist-get 'server-id message))))
         (or (null (alist-get 'uin sender))
             (equal (alist-get 'uin sender) (alist-get 'sender-id message)))
         (or (alist-get 'uin sender) (null (alist-get 'uid sender))
             (equal (alist-get 'uid sender) (alist-get 'sender-native-id message)))
         (or (null (alist-get 'sent_at data))
             (equal (alist-get 'sent_at data) (alist-get 'time message))))))

(defun qq-reply-find-source (data messages kind)
  "Find one unambiguous source for DATA in MESSAGES of conversation KIND."
  (let ((matches (seq-filter (lambda (m) (qq-reply-source-matches-p data m kind))
                             messages)))
    (when (= (length matches) 1) (car matches))))

(defun qq-reply-locator (session data &optional messages)
  "Return a native history locator for SESSION and reply DATA.
Private client sequences require sender and time; never use them as cursors."
  (let* ((kind (qq-state-session-key-type session))
         (sequence (or (alist-get 'message_seq data) (alist-get 'sequence data)))
         (id (or (alist-get 'message_id data) (alist-get 'id data)))
         (sender (alist-get 'uin (alist-get 'sender data)))
         (time (alist-get 'sent_at data)))
    (cond
     ((and sequence (eq kind 'group))
      `((kind . "sequence") (sequence . ,sequence)))
     ((and sequence (eq kind 'private) sender time)
      `((kind . "private_reply") (client_sequence . ,sequence)
        (sender_uin . ,sender) (sent_at . ,time)))
     ((and sequence (eq kind 'private))
      (when-let* ((source (qq-reply-find-source data messages kind)))
        (qq-message-history-locator source)))
     ((qq-protocol-message-id-p id) `((kind . "message") (message_id . ,id))))))

(defun qq-reply-key (session data &optional messages)
  "Return the account and Gateway scoped presentation key for a reply."
  (when (and session data)
    (when-let* ((center (or (qq-reply-locator session data)
                            (qq-reply-locator session data
                                              (or messages (qq-state-session-messages session))))))
      (list :reply (qq-runtime-current-account-id) (qq-server-gateway-instance-id)
            session center (copy-tree (alist-get 'sender data)) (alist-get 'sent_at data)))))

(defun qq-reply-resource-source (session data)
  "Return a detached source retained by the current Appkit Surface."
  (when-let* ((surface (appkit-current-surface))
              (key (qq-reply-key session data))
              (state (appkit-resource-state surface key))
              ((eq (appkit-resource-state-status state) 'ready)))
    (qq-state-message-apply-tombstones session (appkit-resource-state-value state))))

(defun qq-reply-source (session data &optional messages)
  "Resolve DATA from SESSION cache or an Appkit-owned historical source."
  (or (qq-reply-find-source data (or messages (qq-state-session-messages session))
                            (qq-state-session-key-type session))
      (qq-reply-resource-source session data)))

(defun qq-reply-preview-model (data source)
  "Build shared inline presentation from SOURCE or DATA's native quote.
An embedded quote supplies presentation only, never a synthetic message."
  (let* ((sender (or (and source (alist-get 'sender-name source))
                     (alist-get 'sender_name data)
                     (alist-get 'uin (alist-get 'sender data))))
         (preview (if source (qq-chat--reply-preview-text source)
                    (alist-get 'preview data))))
    (list :sender sender
          :sender-key (if source (qq-chat--message-sender-color-key source) sender)
          :body (if (and (stringp preview) (not (string-empty-p preview)))
                    preview
                  "[原消息暂不可用]"))))

(defun qq-reply--check-source (result key)
  "Validate a detached single-source history RESULT against KEY."
  (pcase-let ((`(:reply ,account ,instance ,session ,center ,sender ,sent-at) key))
    (unless (equal instance (qq-server-gateway-instance-id))
      (error "qq: Reply source belongs to an obsolete Gateway"))
    (let* ((conversation (qq-message--history-conversation-params session))
           (meta (qq-message--check-unified-history-around
                  session result account conversation center))
           (row (seq-find (lambda (row)
                            (equal (alist-get 'row_key row)
                                   (plist-get meta :history-center-row-key)))
                          (alist-get 'messages result)))
           (native (qq-message--history-item row "native" "Reply source"))
           (message (qq-message-normalize-snapshot
                     native account (qq-account-get account)
                     (alist-get 'canonical-recalled-p native) t)))
      (unless (and (equal session (alist-get 'session-key message))
                   (or (null (alist-get 'uin sender))
                       (equal (alist-get 'uin sender) (alist-get 'sender-id message)))
                   (or (alist-get 'uin sender) (null (alist-get 'uid sender))
                       (equal (alist-get 'uid sender) (alist-get 'sender-native-id message)))
                   (or (null sent-at) (equal sent-at (alist-get 'time message))))
        (error "qq: Reply source contradicts its sender or timestamp"))
      message)))

(defun qq-reply--load (_context key success failure)
  "Acquire one detached source for KEY using the existing history port."
  (pcase-let ((`(:reply ,account ,instance ,session ,center . ,_) key))
    (if (not (equal instance (qq-server-gateway-instance-id)))
        (progn (funcall failure "Gateway changed")
               (appkit-cancellation-create :cancel #'ignore))
      (let ((request
             (qq-request-start
              (lambda (ok bad)
                (qq-runtime-with-account account
                  (qq-message--call
                   "message.get_history_around" account
                   `((conversation . ,(qq-message--history-conversation-params session))
                     (center . ,center) (count . 1))
                   :projector (lambda (result) (qq-reply--check-source result key))
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
  "Describe visible reply-source demands to Appkit after a successful render.
The shared broker coalesces requests, bounds concurrency and owns cancellation.
Source snapshots never enter the chat's contiguous history or read frontier."
  (setq qq-reply--viewport (qq-reply--visible-anchors))
  (let ((demands (make-hash-table :test #'equal))
        (interests (make-hash-table :test #'equal))
        (histories (make-hash-table :test #'equal)))
    (dolist (message messages)
      (let* ((row (qq-state-message-anchor message))
             (session (or session (alist-get 'session-key message)))
             (data (qq-reply-data message)))
        (when (and session data (member row qq-reply--viewport))
          (unless (or (qq-reply-find-source data (or (gethash session histories)
                                                     (puthash session (qq-state-session-messages session) histories))
                                            (qq-state-session-key-type session))
                      (qq-message--present-string (alist-get 'preview data)))
            (when-let* ((key (qq-reply-key session data)))
              ;; A single viewport is bounded even with exceptionally short rows.
              (when (or (gethash key demands) (< (hash-table-count demands) 64))
                (puthash key (appkit-resource-demand-create
                              :key key :input key :loader #'qq-reply--load
                              :acquisition-identity key :sharing-policy 'app-private
                              :cache-policy 'while-interested) demands)
                (puthash key (cons row (gethash key interests)) interests)))))))
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
