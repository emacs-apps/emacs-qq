;;; qq-forward.el --- Merged-forward message viewer for emacs-qq -*- lexical-binding: t; -*-

;; Author: 0WD0 <me@0wd0.com>

;;; Commentary:

;; QQ merged-forward records are trees, not ordinary timeline messages.  This
;; module keeps their transient messages local to a `special-mode' buffer and
;; deliberately does not merge them into `qq-state'.  Timeline rendering can
;; use the public segment predicates/inserter without introducing a load cycle:
;; `qq-chat' autoloads those entry points, while this module requires
;; `qq-chat' to reuse its body and media renderer.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'subr-x)
(require 'qq-api)
(require 'qq-chat)
(require 'qq-core)
(require 'qq-media)
(require 'qq-message)
(require 'qq-request)
(require 'qq-state)
(require 'qq-runtime)
(require 'appkit-core)
(require 'appkit-projection)
(require 'appkit-chat-timeline)
(require 'appkit-ui)
(require 'appkit-presentation)

(declare-function qq-core-get-forward
                  "qq-core" (resource-id scene callback &optional errback))
(declare-function qq-api--exact-object-keys-p
                  "qq-api" (object required &optional optional))
(declare-function qq-api--signal-schema-error
                  "qq-api" (protocol-p format-string &rest args))
(declare-function qq-api--validate-native-forward-message
                  "qq-api" (message context protocol-p))
(declare-function qq-api--validate-native-forward-segment
                  "qq-api" (segment context protocol-p))
(declare-function qq-api--session-emacs-locator "qq-api" (session-key))
(declare-function qq-api-validate-forward-source
                  "qq-api" (source &optional context protocol-p))
(declare-function qq-api-validate-native-forward-messages
                  "qq-api" (messages &optional context protocol-p))
(declare-function qq-api-validate-resource-id
                  "qq-api" (value &optional context protocol-p))
(declare-function qq-chat--insert-message-body
                  "qq-chat" (message prefix-state properties))
(declare-function qq-chat--message-media-cache-keys "qq-chat" (message))
(declare-function qq-chat-message-layout
                  "qq-chat" (message &rest keys))
(declare-function qq-chat-insert-message-heading
                  "qq-chat" (message properties layout &rest keys))
(declare-function qq-message--segment-to-internal
                  "qq-message" (segment))

(defvar-local qq-forward--buffer-key nil
  "Explicit remote reference or local inline identity for this viewer.")

(defvar-local qq-forward--lookup-id nil
  "Display identity from the native source represented by this viewer.")

(defvar-local qq-forward--lookup-kind nil
  "Native source kind: `message', `resource', `context', or nil inline.")

(defvar-local qq-forward--source nil
  "Validated native remote source for this viewer, or nil inline.")

(defvar-local qq-forward--messages nil
  "Viewer-local normalized messages rendered in the current buffer.")

(defvar-local qq-forward--loading nil
  "Non-nil while this forward viewer is awaiting an API response.")

(defvar-local qq-forward--loaded-p nil
  "Non-nil after this viewer has accepted an inline or remote result.")

(defvar-local qq-forward--error nil
  "Last loading error string for this forward viewer, or nil.")

(defvar-local qq-forward--unsupported-message-count 0
  "Number of forward entries omitted because their envelope is unsupported.")

(defvar-local qq-forward--request nil
  "Active native forward request token for this viewer.")

(defvar-local qq-forward--request-owner nil
  "Owner object identifying the active native forward request.")

(defvar-local qq-forward--inline-content nil
  "Native inline snapshot array supplied by the parent forward segment.")

(defvar-local qq-forward--inline-p nil
  "Non-nil when `qq-forward--inline-content' is authoritative.

This separate flag distinguishes an explicit empty inline message array from
a remote reference that must be fetched.")

(defconst qq-forward--status-row-key :qq-forward-status
  "Stable key for the synthetic loading, error, or empty timeline row.")

(defun qq-forward--present-string (value)
  "Return VALUE when it is a non-empty string, else nil."
  (and (stringp value)
       (not (string-empty-p value))
       value))

(defun qq-forward--single-alist-p (value)
  "Return non-nil when VALUE is one symbol-keyed alist."
  (and (consp value)
       (consp (car value))
       (symbolp (car (car value)))))

(defun qq-forward--source-id (source)
  "Return identity string from validated native SOURCE."
  (pcase (alist-get 'kind source)
    ("message" (alist-get 'message_id source))
    ("resource" (alist-get 'resource_id source))
    ("context"
     (format "%s/%s"
             (alist-get 'root_message_id source)
             (alist-get 'parent_message_id source)))))

(defun qq-forward--validate-resource-source (source)
  "Validate and copy one v2 long-message resource SOURCE."
  (unless
      (qq-api--exact-object-keys-p source '(kind resource_id scene))
    (error "qq: v2 forward resource has invalid fields"))
  (unless (equal (alist-get 'kind source) "resource")
    (error "qq: v2 forward resource has invalid kind"))
  (qq-api-validate-resource-id
   (alist-get 'resource_id source) "v2 forward resource" t)
  (unless (member (alist-get 'scene source)
                  '("group" "private" "group_temp"))
    (error "qq: v2 forward resource has invalid scene"))
  (copy-tree source))

(defun qq-forward--canonical-source (source)
  "Return validated SOURCE in one stable field order."
  (setq source
        (if (equal (alist-get 'kind source) "resource")
            (qq-forward--validate-resource-source source)
          (qq-api-validate-forward-source source "forward viewer source")))
  (pcase (alist-get 'kind source)
    ("message"
     (let* ((chat (alist-get 'chat source))
            (canonical-chat
             (qq-api--session-emacs-locator
              (qq-api-session-key-from-locator chat))))
       (list (cons 'kind "message")
             (cons 'message_id (alist-get 'message_id source))
             (cons 'chat canonical-chat))))
    ("resource"
     (list (cons 'kind "resource")
           (cons 'resource_id (alist-get 'resource_id source))
           (cons 'scene (alist-get 'scene source))))
    ("context"
     (let ((peer (alist-get 'peer source)))
       (list
        (cons 'kind "context")
        (cons 'peer
              (list (cons 'chat_type (alist-get 'chat_type peer))
                    (cons 'peer_uid (alist-get 'peer_uid peer))
                    (cons 'guild_id (alist-get 'guild_id peer))))
        (cons 'root_message_id (alist-get 'root_message_id source))
        (cons 'parent_message_id (alist-get 'parent_message_id source)))))))

(defun qq-forward--source-buffer-key (source)
  "Return collision-free canonical buffer identity for SOURCE."
  (list 'remote (qq-forward--canonical-source source)))

(defun qq-forward--source-label (source)
  "Return a concise user-facing label for SOURCE."
  (if (equal (alist-get 'kind source) "context")
      (format "Nested · %s · %s"
              (alist-get 'peer_uid (alist-get 'peer source))
              (alist-get 'parent_message_id source))
    (qq-forward--source-id source)))

(defun qq-forward--source-reference (source)
  "Return (KIND . ID) for validated native SOURCE."
  (pcase (alist-get 'kind source)
    ("message" (cons 'message (alist-get 'message_id source)))
    ("resource" (cons 'resource (alist-get 'resource_id source)))
    ("context" (cons 'context (qq-forward--source-id source)))))

(defun qq-forward--validate-content (content &optional protocol-p)
  "Validate canonical nested forward CONTENT union."
  (unless (qq-api--single-alist-p content)
    (qq-api--signal-schema-error
     protocol-p "qq: forward content must be an object"))
  (pcase (alist-get 'kind content)
    ("inline"
     (unless (qq-api--exact-object-keys-p content '(kind messages))
       (qq-api--signal-schema-error
        protocol-p "qq: inline forward content has invalid fields"))
     (qq-api-validate-native-forward-messages
      (alist-get 'messages content) "inline forward messages" protocol-p))
    ("remote"
     (unless (qq-api--exact-object-keys-p content '(kind reference))
       (qq-api--signal-schema-error
        protocol-p "qq: remote forward content has invalid fields"))
     (qq-api-validate-forward-source
      (alist-get 'reference content) "remote forward reference" protocol-p))
    (_
     (qq-api--signal-schema-error
      protocol-p "qq: forward content has invalid kind")))
  (copy-tree content))

;;;###autoload
(defun qq-forward-segment-p (segment)
  "Return non-nil when SEGMENT is a canonical internal forward card."
  (condition-case nil
      (when (qq-forward--single-alist-p segment)
        (let ((type (alist-get 'type segment))
              (data (alist-get 'data segment)))
          (cond
           ((and (equal type "forward")
                 (qq-api--exact-object-keys-p data '(content)))
            (qq-forward--validate-content (alist-get 'content data))
            t)
           ((and (equal type "card")
                 (qq-api--exact-object-keys-p
                  data '(kind reference presentation))
                 (equal (alist-get 'kind data) "forward"))
            (let ((reference
                   (qq-forward--validate-resource-source
                    (alist-get 'reference data)))
                  (presentation (alist-get 'presentation data)))
              (unless (equal (alist-get 'kind reference) "resource")
                (error
                 "qq: v2 forward card requires a resource reference"))
              (unless
                  (qq-api--exact-object-keys-p
                   presentation nil '(source content summary prompt))
                (error
                 "qq: v2 forward card presentation has invalid fields"))
              (dolist (key '(source content summary prompt))
                (when (and (assq key presentation)
                           (not (stringp (alist-get key presentation))))
                  (error
                   "qq: v2 forward card presentation.%s must be a string"
                   key))))
            t))))
    (error nil)))

(defun qq-forward--segment-source (segment)
  "Return remote native source from canonical SEGMENT, or nil for inline."
  (unless (qq-forward-segment-p segment)
    (user-error "qq: segment is not a canonical forward record"))
  (let* ((type (alist-get 'type segment))
         (data (alist-get 'data segment)))
    (if (equal type "forward")
        (let ((content (qq-forward--validate-content
                        (alist-get 'content data))))
          (when (equal (alist-get 'kind content) "remote")
            (qq-api-validate-forward-source
             (alist-get 'reference content)
             "forward segment reference")))
      (qq-forward--validate-resource-source
       (alist-get 'reference data)))))

;;;###autoload
(defun qq-forward-reference-id (segment)
  "Return canonical remote identity from SEGMENT, or nil for inline."
  (when-let* ((source (qq-forward--segment-source segment)))
    (qq-forward--source-id source)))

(defun qq-forward--inline-cell (segment)
  "Return native inline messages cell from canonical SEGMENT."
  (when (and (qq-forward-segment-p segment)
             (equal (alist-get 'type segment) "forward"))
    (let ((content
           (qq-forward--validate-content
            (alist-get 'content (alist-get 'data segment)))))
      (when (equal (alist-get 'kind content) "inline")
        (assq 'messages content)))))

(defun qq-forward--inline-message-count (inline-content)
  "Return number of inline forward messages in INLINE-CONTENT, or nil.

Important: in Elisp `(listp nil)' is non-nil and `(length nil)' is 0.
Remote forward-cards have no inline snapshot, so callers must treat nil
as \"unknown\", not zero — otherwise the UI always shows
\"0 forwarded messages\"."
  (cond
   ((vectorp inline-content)
    (length inline-content))
   ((null inline-content)
    nil)
   ((proper-list-p inline-content)
    (length inline-content))
   (t nil)))

(defun qq-forward--count-from-presentation (presentation)
  "Best-effort message count from PRESENTATION summary text, or nil.

QQ Ark summaries often look like \"查看3条转发消息\" when the nested
snapshot is remote-only."
  (let ((summary (qq-forward--present-string
                  (and (listp presentation) (alist-get 'summary presentation)))))
    (cond
     ((and (stringp summary)
           (string-match "查看\\([0-9]+\\)条" summary))
      (string-to-number (match-string 1 summary)))
     ((and (stringp summary)
           (string-match "\\([0-9]+\\)\\s-*条转发" summary))
      (string-to-number (match-string 1 summary)))
     ((and (stringp summary)
           (string-match "\\([0-9]+\\)\\s-*forwarded" summary))
      (string-to-number (match-string 1 summary)))
     (t nil))))

(defconst qq-forward--gateway-segment-kinds
  '("text" "face" "at" "reply" "record" "video" "image"
    "market_face" "forward_card" "light_app" "group_file" "unsupported")
  "Closed segment union returned by `message.get_forward'.")

(defun qq-forward--validate-gateway-segment (segment context)
  "Validate one v2 Gateway SEGMENT at CONTEXT."
  (unless (qq-api--exact-object-keys-p segment '(kind payload))
    (error "qq: %s must contain only kind and payload" context))
  (let ((kind (alist-get 'kind segment))
        (payload (alist-get 'payload segment)))
    (unless (member kind qq-forward--gateway-segment-kinds)
      (error "qq: %s has unsupported Gateway kind %S" context kind))
    (unless (qq-api--single-alist-p payload)
      (error "qq: %s payload must be an object" context)))
  segment)

(defun qq-forward-native-segment-to-internal (segment)
  "Validate Gateway SEGMENT and map it through the shared v2 message model."
  (qq-forward--validate-gateway-segment
   segment "Gateway forward segment")
  (qq-message--segment-to-internal segment))

(defun qq-forward--validate-gateway-message (message context)
  "Validate one v2 Gateway forward MESSAGE at CONTEXT."
  (unless
      (qq-api--exact-object-keys-p
       message '(entry_id sequence state sent_at sender origin segments)
       '(message_id))
    (error "qq: %s has invalid fields" context))
  (unless (qq-api-entry-id-p (alist-get 'entry_id message))
    (error "qq: %s entry_id must be a dotted decimal path" context))
  (when (assq 'message_id message)
    (qq-api-validate-message-id
     (alist-get 'message_id message) context t))
  (unless (qq-protocol-uint64-decimal-p (alist-get 'sequence message))
    (error "qq: %s sequence must be canonical uint64 text" context))
  (unless (and (integerp (alist-get 'sent_at message))
               (>= (alist-get 'sent_at message) 0))
    (error "qq: %s sent_at must be a non-negative integer" context))
  (unless (equal (alist-get 'state message) "live")
    (error "qq: %s state must be live" context))
  (let ((sender (alist-get 'sender message)))
    (pcase (and (qq-api--single-alist-p sender)
                (alist-get 'kind sender))
      ("user"
       (unless
           (and
            (qq-api--exact-object-keys-p
             sender '(kind user_id name avatar_url))
            (qq-protocol-user-uin-p (alist-get 'user_id sender))
            (stringp (alist-get 'name sender))
            (qq-protocol-non-empty-string-p (alist-get 'avatar_url sender)))
         (error "qq: %s user sender is invalid" context)))
      ("anonymous"
       (unless
           (and (qq-api--exact-object-keys-p sender '(kind name))
                (stringp (alist-get 'name sender)))
         (error "qq: %s anonymous sender is invalid" context)))
      (_ (error "qq: %s sender has invalid kind" context))))
  (let ((origin (alist-get 'origin message)))
    (pcase (and (qq-api--single-alist-p origin)
                (alist-get 'kind origin))
      ("private"
       (unless
           (and (qq-api--exact-object-keys-p origin '(kind peer_uin))
                (qq-protocol-uint64-decimal-p
                 (alist-get 'peer_uin origin)))
         (error "qq: %s private origin is invalid" context)))
      ("group"
       (unless
           (and (qq-api--exact-object-keys-p origin '(kind group_uin))
                (qq-protocol-uint64-decimal-p
                 (alist-get 'group_uin origin)))
         (error "qq: %s group origin is invalid" context)))
      ("unknown"
       (unless (qq-api--exact-object-keys-p origin '(kind))
         (error "qq: %s unknown origin has invalid fields" context)))
      (_ (error "qq: %s origin has invalid kind" context))))
  (let ((segments (alist-get 'segments message)))
    (unless (or (vectorp segments) (proper-list-p segments))
      (error "qq: %s segments must be an array" context))
    (unless (> (length segments) 0)
      (error "qq: %s requires at least one segment" context))
    (cl-loop
     for segment across (vconcat segments)
     for index from 0
     do (qq-forward--validate-gateway-segment
         segment (format "%s.segments[%d]" context index))))
  message)

(defun qq-forward--native-sender-fields (sender)
  "Return internal sender display tuple from native SENDER."
  (pcase (alist-get 'kind sender)
    ("user"
     (let ((id (alist-get 'user_id sender))
           (name (alist-get 'name sender))
           (avatar-url (alist-get 'avatar_url sender)))
       (list id (or (qq-forward--present-string name) id "unknown")
             name nil avatar-url)))
    ("anonymous"
     (let ((name (alist-get 'name sender))
           (avatar-url (alist-get 'avatar_url sender)))
       (list nil (or (qq-forward--present-string name) "anonymous")
             name nil avatar-url)))))

(defun qq-forward--normalized-message
    (source segments entry-id server-id time sender-fields state origin)
  "Build one viewer-local message from a validated native snapshot."
  (pcase-let ((`(,sender-id ,sender-name ,sender-nickname ,sender-card
                 ,sender-avatar-url)
               sender-fields))
    (let* ((segments (copy-tree segments))
           (recalled (equal state "recalled"))
           (preview (if recalled
                        "[message recalled]"
                      (qq-state-message-preview-from-segments segments))))
      `((id . ,entry-id)
        (server-id . ,server-id)
        (message-seq . ,(alist-get 'sequence source))
        (time . ,time)
        (sender-id . ,sender-id)
        (sender-name . ,sender-name)
        (sender-secondary-name . nil)
        (sender-card . ,sender-card)
        (sender-nickname . ,sender-nickname)
        (sender-remark . nil)
        (sender-avatar-url . ,sender-avatar-url)
        (self-p . nil)
        (status . ,(if recalled 'recalled 'received))
        (segments . ,(if recalled nil segments))
        (raw-message . ,preview)
        (preview . ,preview)
        (message-type . ,(alist-get 'kind origin))
        (origin . ,(copy-tree origin))
        (raw-event . ,(copy-tree source))))))

(defun qq-forward-native-message-to-internal (message)
  "Validate a v2 Gateway MESSAGE and map it to viewer-local timeline shape."
  (qq-forward--validate-gateway-message
   message "Gateway forward message")
  (let ((segments
         (mapcar #'qq-forward-native-segment-to-internal
                 (let ((value (alist-get 'segments message)))
                   (if (vectorp value) (append value nil) value)))))
    (qq-forward--normalized-message
     message
     segments
     (alist-get 'entry_id message)
     (alist-get 'message_id message)
     (alist-get 'sent_at message)
     (qq-forward--native-sender-fields (alist-get 'sender message))
     (alist-get 'state message)
     (alist-get 'origin message))))

(defun qq-forward--normalize-messages (raw-messages)
  "Validate and map v2 Gateway RAW-MESSAGES for the viewer."
  (unless (or (vectorp raw-messages) (proper-list-p raw-messages))
    (error "qq: Gateway forward messages must be an array"))
  (mapcar #'qq-forward-native-message-to-internal
          (append raw-messages nil)))

(defun qq-forward--legacy-presentation (data)
  "Extract safe presentation fields from legacy Ark DATA."
  (let (presentation)
    (dolist (key '(source title content summary prompt))
      (when-let* ((value (alist-get key data))
                  (_ (stringp value)))
        (push (cons key value) presentation)))
    (nreverse presentation)))

;;;###autoload
(defun qq-forward-event-segment-to-internal (segment session-key)
  "Translate one root-chat legacy forward SEGMENT at the event boundary.

Canonical native-derived internal segments pass through unchanged.  Legacy
inline content is deliberately ignored: native snapshots are loaded through
the native forward action using an explicit locator-qualified reference."
  (cond
   ((qq-forward-segment-p segment)
    (copy-tree segment))
   ((and (qq-forward--single-alist-p segment)
         (equal (alist-get 'type segment) "forward"))
    (let* ((data (alist-get 'data segment))
           (message-id
            (qq-api-validate-message-id
             (and (listp data) (alist-get 'message_id data))
             "legacy forward event"))
           (chat (qq-api--session-emacs-locator session-key)))
      `((type . "forward")
        (data
         . ((content
             . ((kind . "remote")
                (reference
                 . ((kind . "message")
                    (message_id . ,message-id)
                    (chat . ,chat))))))))))
   ((and (qq-forward--single-alist-p segment)
         (equal (alist-get 'type segment) "card")
         (equal (alist-get 'kind (alist-get 'data segment)) "forward"))
    (let* ((data (alist-get 'data segment))
           (resource-id
            (qq-api-validate-resource-id
             (and (listp data) (alist-get 'res_id data))
             "legacy Ark forward event")))
      `((type . "card")
        (data
         . ((kind . "forward")
            (reference
             . ((kind . "resource") (resource_id . ,resource-id)))
            (presentation . ,(qq-forward--legacy-presentation data)))))))
   (t nil)))

(defun qq-forward--buffer-name (account-id name-key)
  "Return ACCOUNT-ID-qualified forward buffer name for NAME-KEY."
  (qq-runtime-account-buffer-name "forward" name-key account-id))

(defun qq-forward--view-id (buffer-key)
  "Return the exact appkit view identity for canonical BUFFER-KEY."
  (list 'forward buffer-key))

(defun qq-forward--orphan-buffer (account-id buffer-key)
  "Return ACCOUNT-ID's detached forward BUFFER-KEY buffer, or nil."
  (cl-find-if
   (lambda (buffer)
     (and (buffer-live-p buffer)
          (with-current-buffer buffer
            (and (derived-mode-p 'qq-forward-mode)
                 (equal qq-runtime--account-id account-id)
                 (equal qq-forward--buffer-key buffer-key)
                 (not (appkit-surface-live-p (appkit-current-surface)))))))
   (buffer-list)))

(defun qq-forward--live-current-view ()
  "Return the live canonical forward view in the current buffer, or nil."
  (let ((view (appkit-current-surface)))
    (and qq-forward--buffer-key
         (derived-mode-p 'qq-forward-mode)
         (appkit-surface-live-p view)
         (eq (appkit-surface-buffer view) (current-buffer))
         (equal (appkit-surface-identity view)
                (qq-forward--view-id qq-forward--buffer-key))
         (equal (plist-get (appkit-surface-model view) :state) qq-forward--buffer-key)
         view)))

(defun qq-forward--entry-properties (message)
  "Return text properties for normalized MESSAGE."
  (list 'qq-forward-message message
        'qq-forward-entry-id (alist-get 'id message)
        'rear-nonsticky '(qq-forward-message qq-forward-entry-id)))

(defun qq-forward--message-reply-target (message)
  "Return native reply target plist from normalized MESSAGE, or nil."
  (when-let* ((reply
               (seq-find (lambda (segment)
                           (equal (alist-get 'type segment) "reply"))
                         (alist-get 'segments message))))
    (let ((data (alist-get 'data reply)))
      (when-let* ((sequence (alist-get 'message_seq data)))
        (list :kind 'native
              :sequence sequence
              :sender-name (alist-get 'sender_name data))))))

(defun qq-forward--messages-by-entry (messages)
  "Return an equal-tested native entry id index for MESSAGES."
  (let ((index (make-hash-table :test #'equal)))
    (dolist (message messages index)
      (puthash (alist-get 'id message) message index))))

(defun qq-forward--messages-by-sequence (messages)
  "Return a unique native sequence index for MESSAGES.

Conflicting sequence claims are removed instead of choosing one entry."
  (let ((index (make-hash-table :test #'equal))
        (conflicts (make-hash-table :test #'equal)))
    (dolist (message messages)
      (let ((sequence (alist-get 'message-seq message)))
        (when sequence
          (if (gethash sequence index)
              (progn
                (remhash sequence index)
                (puthash sequence t conflicts))
            (unless (gethash sequence conflicts)
              (puthash sequence message index))))))
    index))

(defun qq-forward--reply-view-model
    (message messages-by-entry messages-by-sequence)
  "Return a stable viewer-local reply model for MESSAGE.

The model snapshots the target sender and preview into the projected row
context.  A target-content change therefore changes the reply row context and
causes appkit to redraw that row without a global message-state dependency.
MESSAGES-BY-ENTRY and MESSAGES-BY-SEQUENCE are projection-local indexes."
  (when-let* ((target (qq-forward--message-reply-target message)))
    (let* ((target-kind (plist-get target :kind))
           (target-id (plist-get target :id))
           (source
            (pcase target-kind
              ('entry (gethash target-id messages-by-entry))
              ('native
               (gethash (plist-get target :sequence)
                        messages-by-sequence))))
           (sender (and source
                        (qq-forward--present-string
                         (alist-get 'sender-name source))))
           (preview (and source
                         (string-trim
                          (or (alist-get 'preview source)
                              (qq-state-message-preview source)
                              ""))))
           (body (cond
                  ((and sender preview (not (string-empty-p preview)))
                   (format "%s: %s" sender
                           (truncate-string-to-width preview 56 nil nil t)))
                  ((and preview (not (string-empty-p preview)))
                   (truncate-string-to-width preview 64 nil nil t))
                  ((and (eq target-kind 'entry) target-id)
                   (format "entry %s" target-id))
                  ((plist-get target :sender-name)
                   (format "%s's message"
                           (plist-get target :sender-name)))
                  ((plist-get target :sequence)
                   (format "native message #%s"
                           (plist-get target :sequence)))
                  (t "native reply"))))
      (list :target-kind target-kind
            :target-id target-id
            :jump-entry-id (and source (alist-get 'id source))
            :body body))))

(defun qq-forward--insert-reply-preview-line
    (view-model prefix-state properties)
  "Insert reply preview VIEW-MODEL using PREFIX-STATE and PROPERTIES."
  (let* ((target-id (plist-get view-model :target-id))
         (jump-entry-id (plist-get view-model :jump-entry-id))
         (body (plist-get view-model :body))
         (start (point)))
    (insert (format "↪ %s\n" body))
    (let ((reply-properties
           (append properties
                   (list 'face 'qq-msg-inline-reply
                         'qq-forward-reply-id
                         target-id)
                   (when jump-entry-id
                     (let ((action
                            (lambda ()
                              (qq-forward--goto-entry-id jump-entry-id)))
                           (map (make-sparse-keymap)))
                       (set-keymap-parent map button-map)
                       (define-key map (kbd "RET")
                                   (lambda () (interactive) (funcall action)))
                       (define-key map [mouse-1]
                                   (lambda () (interactive) (funcall action)))
                       (list 'mouse-face 'highlight
                             'help-echo "Jump to replied forwarded message"
                             'follow-link t
                             'keymap map
                             'button t
                             'category 'default-button
                             'action (lambda (_button)
                                       (funcall action))))))))
      (add-text-properties start (point) reply-properties))
    (appkit-ui-apply-line-prefix start (point) prefix-state)))

(defun qq-forward--insert-message (message context)
  "Insert one normalized forward MESSAGE using projected CONTEXT.

The heading and two-line avatar geometry use the shared QQ presentation API."
  (let* ((start (point))
         (properties (qq-forward--entry-properties message))
         (layout (qq-chat-message-layout message))
         (prefix-state
          (qq-chat-insert-message-heading message properties layout)))
    (when-let* ((reply-view-model (plist-get context :reply-view-model)))
      (qq-forward--insert-reply-preview-line
       reply-view-model prefix-state properties))
    (qq-chat--insert-message-body message prefix-state properties)
    (insert "\n")
    (add-text-properties start (point) properties)))

(defun qq-forward--goto-entry-id (entry-id)
  "Move point to the row keyed by native ENTRY-ID."
  (when-let* ((position (appkit-chat-timeline-key-position entry-id)))
    (goto-char position)
    t))

(defun qq-forward--message-dependency-keys (message)
  "Return appkit resource dependencies for normalized MESSAGE."
  (delete-dups
   (mapcar (lambda (media-key) (list :media media-key))
           (qq-chat--message-media-cache-keys message))))

(defun qq-forward--status-row (kind text)
  "Return one synthetic status row of KIND displaying TEXT."
  (appkit-chat-timeline-row-create
   :key qq-forward--status-row-key
   :payload (list :status kind :text text)
   :context nil
   :dependencies nil))

(defun qq-forward--omitted-status-row ()
  "Return the status row describing unsupported omitted entries."
  (qq-forward--status-row
   'warning
   (format "%d unsupported forwarded message%s omitted"
           qq-forward--unsupported-message-count
           (if (= qq-forward--unsupported-message-count 1) "" "s"))))

(defun qq-forward--project-timeline ()
  "Project accepted messages and transient status into appkit rows."
  (let ((messages-by-entry
         (qq-forward--messages-by-entry qq-forward--messages))
        (messages-by-sequence
         (qq-forward--messages-by-sequence qq-forward--messages)))
    (let ((message-rows
           (appkit-chat-timeline-project
            qq-forward--messages
            (lambda (message) (alist-get 'id message))
            :context-function
            (lambda (_previous message)
              (when-let* ((reply-view-model
                           (qq-forward--reply-view-model
                            message messages-by-entry
                            messages-by-sequence)))
                (list :reply-view-model (copy-tree reply-view-model))))
            :dependencies-function #'qq-forward--message-dependency-keys)))
      (cond
       (message-rows
        (cond
         (qq-forward--error
          (append message-rows
                  (list (qq-forward--status-row
                         'error
                         (format "Unable to refresh chat history: %s"
                                 qq-forward--error)))))
         ((> qq-forward--unsupported-message-count 0)
          (append
           message-rows
           (list (qq-forward--omitted-status-row))))
         (t message-rows)))
       (qq-forward--loading
        (list (qq-forward--status-row 'loading "Loading chat history…")))
       (qq-forward--error
        (list (qq-forward--status-row
               'error
               (format "Unable to load chat history: %s" qq-forward--error))))
       (qq-forward--loaded-p
        (list
         (if (> qq-forward--unsupported-message-count 0)
             (qq-forward--omitted-status-row)
           (qq-forward--status-row 'empty "(empty chat history)"))))
       (t
        (list (qq-forward--status-row
               'loading "Loading chat history…")))))))

(defun qq-forward--header-text ()
  "Return the static EWOC header for the current viewer."
  (concat
   (propertize "Chat History\n" 'face 'bold)
   (propertize
    (format "%s\n\n"
            (pcase qq-forward--lookup-kind
              ('message (format "message_id: %s" qq-forward--lookup-id))
              ('resource (format "resource_id: %s" qq-forward--lookup-id))
              ('context qq-forward--lookup-id)
              (_ "inline content")))
    'face 'shadow)))

(defun qq-forward--row-printer (row)
  "Render exactly one projected forward timeline ROW."
  (let ((key (appkit-chat-timeline-row-key row))
        (payload (appkit-chat-timeline-row-payload row))
        (context (appkit-chat-timeline-row-context row)))
    (if (eq key qq-forward--status-row-key)
        (let* ((start (point))
               (kind (plist-get payload :status))
               (face (pcase kind
                       ('error 'error)
                       ('warning 'warning)
                       (_ 'shadow))))
          (insert (propertize (concat (plist-get payload :text) "\n")
                              'face face))
          (add-text-properties
           start (point)
           (list 'qq-forward-entry-id qq-forward--status-row-key
                 'rear-nonsticky '(qq-forward-entry-id))))
      (qq-forward--insert-message payload context))))

(defun qq-forward--cancel-request ()
  "Cancel and forget the active native forward request."
  (let ((request qq-forward--request))
    ;; Relinquish ownership before transport cancellation.  A synchronous
    ;; cancellation callback must already be stale, even while the view lives.
    (setq qq-forward--request nil
          qq-forward--request-owner nil
          qq-forward--loading nil)
    (when request
      (qq-request-cancel request))))

(defun qq-forward--clear-view-data ()
  "Clear account-scoped data projected by the current forward view."
  (setq qq-forward--messages nil
        qq-forward--unsupported-message-count 0
        qq-forward--loaded-p nil
        qq-forward--error nil))

(defun qq-forward--reset-buffer-work (buffer)
  "Reset requests and projected data retained by forward BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (derived-mode-p 'qq-forward-mode)
        (unwind-protect
            (qq-forward--cancel-request)
          (qq-forward--clear-view-data))))))

(defun qq-forward--release-view-work (view buffer)
  "Release BUFFER work while it is still owned by forward VIEW."
  (when (and (buffer-live-p buffer)
             (with-current-buffer buffer
               (eq view qq-runtime--surface-owner)))
    (qq-forward--reset-buffer-work buffer)))

(defun qq-forward--setup-view (view)
  "Reset replacement state and register cleanup for forward VIEW."
  (let ((buffer (appkit-surface-buffer view)))
    (appkit-register-handle
     view 'function
     (apply-partially #'qq-forward--release-view-work view buffer))
    ;; A detached buffer is a presentation shell, not an account cache.  A
    ;; replacement runtime must project a loading state and fetch again.
    (qq-forward--reset-buffer-work buffer)))

(defun qq-forward--ensure-view ()
  "Return the live Appkit view owning the current forward buffer."
  (unless qq-forward--buffer-key
    (error "qq: forward buffer has no canonical identity"))
  (let ((view
         (qq-runtime-ensure-account-surface
          :id (qq-forward--view-id qq-forward--buffer-key)
          :mode 'qq-forward-mode
          :state qq-forward--buffer-key
          :render-function #'qq-forward--render
          :setup #'qq-forward--setup-view)))
    (appkit-surface-enable-responsive-geometry
     view
     (lambda (surface _width)
       (appkit-surface-send
        surface (list 'qq-render
                      (appkit-projection-change-create :geometry-p t)))))
    view))

(defun qq-forward--ensure-timeline ()
  "Ensure the current forward view owns one projected appkit timeline."
  (qq-forward--ensure-view)
  (appkit-chat-timeline-ensure
   :printer #'qq-forward--row-printer
   :anchor-property 'qq-forward-entry-id
   :header (qq-forward--header-text)
   :footer nil))

(cl-defun qq-forward--sync-timeline (&key force-keys changed-resources rekeys)
  "Synchronize projected forward rows through appkit."
  (force-mode-line-update)
  (qq-forward--ensure-timeline)
  (appkit-chat-timeline-sync
   (qq-forward--project-timeline)
   :force-keys force-keys
   :changed-resources changed-resources
   :rekeys rekeys))

(defun qq-forward--render (surface model change)
  "Render SURFACE's committed MODEL using native projection CHANGE."
  (let* ((geometry-p (appkit-projection-change-geometry-p change))
         (force-keys
          (delete-dups
           (append (copy-sequence (appkit-projection-change-keys change))
                   (and geometry-p (appkit-chat-timeline-live-p)
                        (appkit-chat-timeline-keys)))))
         (resources (appkit-projection-change-resources change)))
    (when (appkit-surface-live-p surface)
      (dolist (event (plist-get model :events))
        (qq-forward--apply-load-event event))
      (when geometry-p
        (when-let* ((next
                     (appkit-surface-responsive-width
                      surface qq-chat-auto-fill-margin-columns)))
          (setq-local fill-column next)))
      (when (or (appkit-projection-change-full-p change)
                (appkit-projection-change-frame-p change)
                geometry-p force-keys resources
                (appkit-projection-change-rekeys change)
                (plist-get model :events))
        (qq-forward--sync-timeline
         :force-keys force-keys
         :changed-resources resources
         :rekeys (appkit-projection-change-rekeys change)))))
  (appkit-render-result-create))

(defun qq-forward--request-timeline-sync (view)
  "Request one coalesced structural timeline sync for live VIEW."
  (appkit-surface-send
   view (list 'qq-render (appkit-projection-change-create :full-p t))))

(defun qq-forward--apply-load-event (event)
  "Apply one owner-checked forward load EVENT to viewer-local domain state."
  (pcase (plist-get event :type)
    ('load-start
     (setq qq-forward--loading t
           qq-forward--error nil))
    ('load-success
     (setq qq-forward--messages (plist-get event :messages)
           qq-forward--unsupported-message-count
           (or (plist-get event :unsupported-message-count) 0)
           qq-forward--loaded-p t
           qq-forward--loading nil
           qq-forward--error nil
           qq-forward--request nil
           qq-forward--request-owner nil))
    ('load-error
     (setq qq-forward--loading nil
           qq-forward--error (plist-get event :error)
           qq-forward--request nil
           qq-forward--request-owner nil))
    (type
     (error "qq: unknown forward load event %S" type))))

(defun qq-forward--request-current-p (view buffer source owner)
  "Return non-nil when live VIEW still owns SOURCE and OWNER in BUFFER."
  (and (appkit-surface-live-p view)
       (eq buffer (appkit-surface-buffer view))
       (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'qq-forward-mode)
              (eq view (appkit-current-surface))
              (equal qq-forward--source source)
              (eq qq-forward--request-owner owner)))))

(defun qq-forward--accept-request-event
    (view buffer source owner event)
  "Apply terminal EVENT for VIEW, BUFFER, SOURCE, and OWNER.

The event is accepted only while VIEW still owns SOURCE and OWNER in BUFFER.
The callback boundary updates viewer-local domain state and requests an Appkit
sync; timeline mutation remains owned by `qq-forward--render'."
  (when (qq-forward--request-current-p view buffer source owner)
    (with-current-buffer (appkit-surface-buffer view)
      (when (qq-forward--request-current-p view buffer source owner)
        (qq-forward--apply-load-event event)
        (qq-forward--request-timeline-sync view)
        t))))

(defun qq-forward--error-text (response reason)
  "Return readable viewer error from RESPONSE and REASON."
  (format "%s"
          (or reason
              (and (listp response)
                   (or (alist-get 'message response)
                       (alist-get 'wording response)))
              "unknown error")))

(defun qq-forward--load-remote ()
  "Start an owner-scoped remote load for the current viewer."
  (let* ((view (qq-forward--ensure-view))
         (buffer (current-buffer))
         (source (copy-tree qq-forward--source))
         (owner (list 'forward-request source)))
    (unless source
      (user-error "qq: this viewer has no remote forward reference"))
    (qq-forward--cancel-request)
    (setq qq-forward--request-owner owner)
    (qq-forward--apply-load-event '(:type load-start))
    (qq-forward--request-timeline-sync view)
    (condition-case error-data
        (let ((request
                (qq-core-get-forward
                 (pcase (alist-get 'kind source)
                   ("resource" (alist-get 'resource_id source))
                   (_
                    (user-error
                     "qq: v2 merged-forward loading requires a resource reference")))
                 (alist-get 'scene source)
                 (lambda (page)
                   (when (qq-forward--request-current-p
                          view buffer source owner)
                     (let ((event
                            (condition-case error-data
                                (list
                                 :type 'load-success
                                 :messages
                                 (qq-forward--normalize-messages
                                  (plist-get page :messages))
                                 :unsupported-message-count
                                 (plist-get page :unsupported-message-count))
                              (error
                               (list
                                :type 'load-error
                                :error
                                (error-message-string error-data))))))
                       (qq-forward--accept-request-event
                        view buffer source owner event))))
                 (lambda (response reason)
                   (qq-forward--accept-request-event
                    view buffer source owner
                    (list :type 'load-error
                          :error
                          (qq-forward--error-text response reason)))))))
          ;; A synchronous callback may already have released OWNER.
          (when (eq qq-forward--request-owner owner)
            (setq qq-forward--request request)))
      ((error quit)
       (qq-forward--accept-request-event
        view buffer source owner
        (list :type 'load-error
              :error (error-message-string error-data)))
       (when (eq (car error-data) 'quit)
         (setq quit-flag nil)
         (signal (car error-data) (cdr error-data)))))))

(defun qq-forward--load-inline ()
  "Render the authoritative inline subtree in the current viewer."
  (let ((view (qq-forward--ensure-view)))
    (qq-forward--cancel-request)
    (qq-forward--apply-load-event
     (list :type 'load-success
           :messages
           (qq-forward--normalize-messages qq-forward--inline-content)
           :unsupported-message-count 0))
    (qq-forward--request-timeline-sync view)))

(defun qq-forward-refresh ()
  "Refresh the current merged-forward viewer.

Inline subtrees are normalized again locally and never refetched.  Remote
records issue a fresh `emacs_get_forward' request."
  (interactive)
  (unless (derived-mode-p 'qq-forward-mode)
    (user-error "qq: not in a forward viewer"))
  (if qq-forward--inline-p
      (qq-forward--load-inline)
    (qq-forward--load-remote)))

(defun qq-forward--move-message (count)
  "Move COUNT entries in the current forward viewer."
  (let* ((keys (delete qq-forward--status-row-key
                       (appkit-chat-timeline-keys)))
         (total (length keys))
         (current (appkit-chat-timeline-key-at-point))
         (current-index (cl-position current keys :test #'equal))
         (base (if current-index
                   current-index
                 (if (> count 0) -1 total)))
         (target (+ base count)))
    (if (and (>= target 0) (< target total))
        (qq-forward--goto-entry-id (nth target keys))
      (message "qq: no %s forwarded message"
               (if (> count 0) "next" "previous")))))

(defun qq-forward-next-message (&optional count)
  "Move to the next COUNT forwarded messages."
  (interactive "p")
  (let ((count (or count 1)))
    (if (< count 0)
        (qq-forward-previous-message (- count))
      (qq-forward--move-message count))))

(defun qq-forward-previous-message (&optional count)
  "Move to the previous COUNT forwarded messages."
  (interactive "p")
  (let ((count (or count 1)))
    (if (< count 0)
        (qq-forward-next-message (- count))
      (qq-forward--move-message (- count)))))

(defvar qq-forward-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g") #'qq-forward-refresh)
    (define-key map (kbd "n") #'qq-forward-next-message)
    (define-key map (kbd "p") #'qq-forward-previous-message)
    map)
  "Keymap for `qq-forward-mode'.")

(define-derived-mode qq-forward-mode special-mode "QQ-Forward"
  "Major mode for one QQ merged-forward message tree."
  (setq-local truncate-lines nil
              line-spacing 0)
  (add-hook 'kill-buffer-hook #'qq-forward--cancel-request nil t)
  (add-hook 'change-major-mode-hook #'qq-forward--cancel-request nil t)
  (setq-local header-line-format
              '(:eval
                (format
                 " QQ Chat History · %s%s"
                 (or qq-forward--lookup-id "inline")
                 (cond
                  (qq-forward--loading " · loading")
                  (qq-forward--error
                   (format " · error: %s" qq-forward--error))
                  (t ""))))))

(defun qq-forward--open-buffer (source inline-p inline-content)
  "Open a viewer for native SOURCE or authoritative INLINE-CONTENT."
  (when source
    (setq source (qq-forward--canonical-source source)))
  (when inline-p
    (setq inline-content
          (qq-api-validate-native-forward-messages
           inline-content "native forward inline content")))
  (unless (or inline-p source)
    (user-error "qq: this chat history has neither inline content nor source"))
  (let* ((buffer-key
          (if inline-p
              (list 'inline (copy-tree inline-content))
            (qq-forward--source-buffer-key source)))
         (reference (and source (qq-forward--source-reference source)))
         (lookup-kind (car reference))
         (lookup-id (and source (qq-forward--source-label source)))
         (name-key
          (if inline-p
              (format "inline:%x" (sxhash-equal buffer-key))
            (format "%s:%s:%x"
                    (car reference) (cdr reference)
                    (sxhash-equal buffer-key))))
         (owner (qq-runtime-require-account-id "opening forwarded messages"))
         (app (qq-runtime-app owner))
         (view-id (qq-forward--view-id buffer-key))
         (existing (appkit-app-surface app view-id))
         (orphan (and (null existing)
                      (qq-forward--orphan-buffer owner buffer-key)))
         (fresh-p (and (null existing) (null orphan)))
         (view existing)
         (buffer (or (and existing (appkit-surface-buffer existing))
                     orphan
                     (generate-new-buffer
                      (qq-forward--buffer-name owner name-key)))))
    (with-current-buffer buffer
      (unless existing
        (when fresh-p
          (qq-forward-mode)
          (qq-runtime-bind-account owner)
          (setq qq-forward--buffer-key buffer-key
                qq-forward--source (copy-tree source)
                qq-forward--lookup-id lookup-id
                qq-forward--lookup-kind lookup-kind
                qq-forward--messages nil
                qq-forward--loading nil
                qq-forward--loaded-p nil
                qq-forward--error nil
                qq-forward--unsupported-message-count 0
                qq-forward--request nil
                qq-forward--request-owner nil
                qq-forward--inline-p inline-p
                qq-forward--inline-content (and inline-p
                                                (copy-tree inline-content))))
        (setq view (qq-forward--ensure-view)))
      (unless (equal qq-forward--buffer-key buffer-key)
        (error "qq: appkit returned a mismatched forward view"))
      (if inline-p
          (unless qq-forward--loaded-p
            (qq-forward--load-inline))
        (when (and (not qq-forward--loaded-p)
                   (not qq-forward--loading)
                   (null qq-forward--error))
          (qq-forward--load-remote))))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (appkit-surface-refresh-responsive-geometry (appkit-current-surface)))
    buffer))

;;;###autoload
(defun qq-forward-open (source)
  "Open a remote merged-forward viewer for explicit native SOURCE."
  (qq-forward--open-buffer source nil nil))

;;;###autoload
(defun qq-forward-open-segment (segment)
  "Open canonical internal merged-forward SEGMENT."
  (interactive
   (list (get-text-property (point) 'qq-forward-segment)))
  (unless (qq-forward-segment-p segment)
    (user-error "qq: segment is not a canonical merged-forward record"))
  (let* ((inline-cell (qq-forward--inline-cell segment))
         (inline-p (and inline-cell t))
         (inline-content
          (and inline-cell
               (qq-api-validate-native-forward-messages
                (cdr inline-cell) "native forward inline content")))
         (source (qq-forward--segment-source segment)))
    (qq-forward--open-buffer source inline-p inline-content)))

;;;###autoload
(defun qq-forward-insert-segment (segment prefix-state properties)
  "Insert canonical forward SEGMENT as a whole-card action."
  (unless (qq-forward-segment-p segment)
    (error "qq: cannot render non-forward segment"))
  (let* ((data (alist-get 'data segment))
         (presentation
          (and (equal (alist-get 'type segment) "card")
               (alist-get 'presentation data)))
         (source-label
          (qq-forward--present-string (alist-get 'source presentation)))
         (title (qq-forward--present-string (alist-get 'title presentation)))
         (content-preview
          (qq-forward--present-string (alist-get 'content presentation)))
         (summary
          (qq-forward--present-string (alist-get 'summary presentation)))
         (prompt
          (qq-forward--present-string (alist-get 'prompt presentation)))
         (details (delete-dups
                   (delq nil (list content-preview summary prompt))))
         (inline-cell (qq-forward--inline-cell segment))
         (inline-content
          (and inline-cell
               (qq-api-validate-native-forward-messages
                (cdr inline-cell) "native forward inline content")))
         (count (or (qq-forward--inline-message-count inline-content)
                    (qq-forward--count-from-presentation presentation)))
         (source (qq-forward--segment-source segment))
         (reference (and source (qq-forward--source-reference source)))
         (open-action (lambda () (qq-forward-open-segment segment)))
         (map (let ((map (make-sparse-keymap)))
                (set-keymap-parent map button-map)
                (define-key map (kbd "RET")
                            (lambda () (interactive) (funcall open-action)))
                (define-key map [mouse-1]
                            (lambda () (interactive) (funcall open-action)))
                map))
         (card-properties
          (append properties
                  (list 'qq-forward-segment (copy-tree segment)
                        'mouse-face 'highlight
                        'help-echo
                        (if reference
                            (format "Open merged chat history (%s %s)"
                                    (pcase (car reference)
                                      ('resource "resource_id")
                                      ('message "message_id")
                                      ('context "context"))
                                    (cdr reference))
                          "Open inline merged chat history")
                        'follow-link t
                        'keymap map
                        'button t
                        'category 'default-button
                        'action (lambda (_button) (funcall open-action)))))
         (start (point))
         (appkit-ui-card-indent-prefix-state prefix-state)
         (card-prefix-state (appkit-ui-card-prefix-state)))
    (appkit-ui-insert-prefixed-lines
     card-prefix-state
     (if source-label
         (format "Chat History · %s" source-label)
       "Chat History")
     :face 'bold
     :properties card-properties)
    (when title
      (appkit-ui-insert-prefixed-lines
       card-prefix-state title :properties card-properties))
    (dolist (detail details)
      (unless (equal detail title)
        (appkit-ui-insert-prefixed-lines
         card-prefix-state detail :face 'shadow :properties card-properties)))
    (when (and (numberp count) (> count 0))
      (appkit-ui-insert-prefixed-lines
       card-prefix-state
       (format "%d forwarded message%s" count (if (= count 1) "" "s"))
       :face 'shadow
       :properties card-properties))
    (add-text-properties start (point) card-properties)
    (point)))

(defun qq-forward--handle-media-cache-update (&optional media-key)
  "Request MEDIA-KEY resource rendering for every live forward Surface."
  (when (stringp media-key)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when-let* ((view (qq-forward--live-current-view)))
          (appkit-surface-send
           view (list 'qq-render
                      (appkit-projection-change-create
                       :resources (list (list :media media-key))))))))))

(add-hook 'qq-media-cache-update-hook #'qq-forward--handle-media-cache-update)

(provide 'qq-forward)

;;; qq-forward.el ends here
