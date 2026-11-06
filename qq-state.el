;;; qq-state.el --- Immutable account values and reducers -*- lexical-binding: t; -*-

;; Author: 0WD0 <me@0wd0.com>

;;; Commentary:

;; Readers take an account snapshot; reducers return (NEW-STATE . VALUE).
;; Appkit commits the result.  Message entities share unchanged map branches,
;; while Gateway windows own display order.

;;; Code:

(require 'cl-lib)
(require 'appkit-chat-action)
(require 'appkit-data)
(require 'seq)
(require 'subr-x)
(require 'qq-customize)
(require 'qq-protocol)

(cl-defstruct (qq-state-history (:constructor qq-state-history-create) (:copier nil))
  "Message entities and indexes for one conversation; ordering belongs to views."
  (rows appkit-data-map-empty :read-only t)
  (aliases appkit-data-map-empty :read-only t)
  (sequences appkit-data-map-empty :read-only t)
  (outgoing nil :read-only t)
  (pending nil :read-only t)
  (pokes nil :read-only t)
  (oldest nil :read-only t)
  (latest nil :read-only t))

(cl-defstruct (qq-state-store (:constructor qq-state-store-create) (:copier nil))
  "One account's canonical domain data, retained independently of its App."
  (account-id nil :read-only t)
  (connection-status 'disconnected :read-only t)
  (last-heartbeat nil :read-only t)
  (self-info nil :read-only t)
  (status nil :read-only t)
  (sessions appkit-data-map-empty :read-only t)
  (recent-session-keys nil :read-only t)
  (recent-session-key-set appkit-data-map-empty :read-only t)
  (messages-by-session appkit-data-map-empty :read-only t)
  (message-patch-journal appkit-data-map-empty :read-only t)
  (friends-by-id appkit-data-map-empty :read-only t)
  (friend-order nil :read-only t)
  (friend-categories nil :read-only t)
  (friend-categories-loaded-p nil :read-only t)
  (groups-by-id appkit-data-map-empty :read-only t)
  (group-order nil :read-only t)
  (groups-loaded-p nil :read-only t)
  (requests nil :read-only t)
  (message-session-index appkit-data-map-empty :read-only t)
  (local-message-session-index appkit-data-map-empty :read-only t)
  (message-order-counter 0 :read-only t)
  (local-message-counter 0 :read-only t)
  (session-summary-observation-clock 0 :read-only t)
  (actions appkit-data-map-empty :read-only t)
  (input-status-revisions appkit-data-map-empty :read-only t)
  (pending-input-status appkit-data-map-empty :read-only t)
  (peer-uins appkit-data-map-empty :read-only t)
  (pending-recalls appkit-data-map-empty :read-only t)
  (pending-reactions appkit-data-map-empty :read-only t)
  (pending-essences appkit-data-map-empty :read-only t)
  (pending-sends appkit-data-map-empty :read-only t)
  (read-clock 0 :read-only t)
  (read-observations appkit-data-map-empty :read-only t))

(cl-defstruct (qq-state-partition (:constructor qq-state-partition--create) (:copier nil))
  "One account snapshot, including operation and timeline state."
  (store nil :read-only t)
  (operations appkit-data-map-empty :read-only t)
  (timelines appkit-data-map-empty :read-only t)
  (guild nil :read-only t))

(defun qq-state-partition-account-id (partition)
  "Return the stable account ID owning PARTITION's domain store."
  (qq-state-store-account-id (qq-state-partition-store partition)))

(defun qq-state-active-account-id (state)
  "Return the account owning the current domain store, or nil."
  (when-let* ((id (qq-state-store-account-id state)))
    (copy-sequence id)))

(defun qq-state-message-anchor (message)
  "Return stable timeline anchor for MESSAGE.

Prefer the Gateway NT snowflake `server-id', then `local-id', then `id'."
  (and message
       (or (alist-get 'server-id message)
           (alist-get 'local-id message)
           (alist-get 'id message))))

(defun qq-state--normalize-id (value)
  "Return VALUE normalized as a string ID, or nil."
  (and value (format "%s" value)))

(defun qq-state--normalize-time (value)
  "Return VALUE normalized as an integer UNIX timestamp."
  (cond
   ((integerp value) value)
   ((floatp value) (truncate value))
   ((stringp value)
    (truncate (string-to-number value)))
   (t 0)))

(defun qq-state--normalize-reaction-count (value)
  "Return reaction count VALUE as a non-negative integer."
  (max 0
       (cond
        ((integerp value) value)
        ((numberp value) (truncate value))
        ((and (stringp value)
              (string-match-p "\\`[0-9]+\\'" value))
         (string-to-number value))
        (t 0))))

(defun qq-state--infer-reaction-emoji-type (emoji-id)
  "Infer QQ reaction type from string EMOJI-ID."
  (if (> (length (or emoji-id "")) 3) "2" "1"))

(defun qq-state-message-reactions (message)
  "Return normalized reactions stored on MESSAGE."
  (or (and (listp message) (alist-get 'reactions message)) '()))

(defun qq-state--present-string (value)
  "Return VALUE when it is a non-empty string, else nil."
  (and (stringp value)
       (not (string-empty-p value))
       value))

(defun qq-state--first-present-string (&rest values)
  "Return the first non-empty string in VALUES, or nil."
  (seq-find #'qq-state--present-string values))

(defun qq-state--next-message-order (state)
  "Allocate one local ordering token in STATE."
  (let ((value (1+ (qq-state-store-message-order-counter state))))
    (cons (appkit-data-with state (() :message-order-counter value)) value)))

(defun qq-state-reset (state)
  "Return empty account data and nil, retaining the read observation clock."
  (cons (qq-state-store-create
         :account-id (qq-state-store-account-id state)
         :read-clock (qq-state-store-read-clock state))
        nil))

(defun qq-state-connection-status (state)
  "Return current transport connection status symbol."
  (qq-state-store-connection-status state))

(defun qq-state-session-summary-observation-start (state)
  "Allocate a summary freshness token before asynchronous dispatch."
  (let ((value (1+ (qq-state-store-session-summary-observation-clock state))))
    (cons (appkit-data-with state (() :session-summary-observation-clock value)) value)))

(defun qq-state-set-connection-status (state status)
  "Return STATE with connection STATUS, and STATUS."
  (cons (appkit-data-with state (() :connection-status status)) status))

(defun qq-state-self-info (state)
  "Return current self info object."
  (copy-tree (qq-state-store-self-info state)))

(defun qq-state-set-self-info (state info)
  "Return STATE with normalized self INFO, and INFO."
  (cons (appkit-data-with state (() :self-info info)) info))

(defun qq-state-status (state)
  "Return current status object."
  (copy-tree (qq-state-store-status state)))

(defun qq-state-self-user-id (state)
  "Return the current canonical QQ user UIN, or nil."
  (let ((value (alist-get 'user_id (qq-state-store-self-info state))))
    (cond
     ((null value) nil)
     ((qq-protocol-user-uin-p value) value)
     (t (error "qq: self identity is not a canonical uint64 UIN: %S" value)))))

(defun qq-state--canonical-peer-uid (value context)
  "Return opaque peer UID VALUE unchanged after validating CONTEXT."
  (unless (and (stringp value) (not (string-empty-p value)))
    (error "qq: %s requires a non-empty native peer UID, got %S"
           context value))
  value)

(defun qq-state--canonical-dataline-variant (variant)
  "Return canonical DataLine VARIANT string, or signal an error."
  (pcase variant
    ((or 'desktop "desktop") "desktop")
    ((or 'mobile "mobile") "mobile")
    (_ (error "qq: dataline session requires desktop or mobile variant, got %S"
              variant))))

(defun qq-state-guild-channel-session-key (guild-id channel-id)
  "Build a canonical channel session key from GUILD-ID and CHANNEL-ID."
  (unless (qq-protocol-uint64-decimal-p guild-id)
    (error "qq: Guild identity requires a canonical uint64 string"))
  (unless (qq-protocol-uint64-decimal-p channel-id)
    (error "qq: Guild channel identity requires a canonical uint64 string"))
  (format "guild:%s:channel:%s" guild-id channel-id))

(defun qq-state-session-key (type target-id &optional variant)
  "Build a canonical session key from TYPE, TARGET-ID, and VARIANT.

Private and group identities must already be canonical uint64 text.  DataLine
keys require VARIANT to be `desktop' or `mobile'.  TARGET-ID is an opaque
native peer UID for DataLine and service sessions; it is preserved
byte-for-byte, including any colon characters."
  (pcase type
    ((or 'private "private")
     (when variant
       (error "qq: private session does not accept a variant"))
     (unless (qq-protocol-user-uin-p target-id)
       (error "qq: private session requires a canonical uint64 user UIN"))
     (format "private:%s" target-id))
    ((or 'group "group")
     (when variant
       (error "qq: group session does not accept a variant"))
     (unless (qq-protocol-group-uin-p target-id)
       (error "qq: group session requires a canonical uint64 group UIN"))
     (format "group:%s" target-id))
    ((or 'dataline "dataline")
     (format "dataline:%s:%s"
             (qq-state--canonical-dataline-variant variant)
             (qq-state--canonical-peer-uid target-id "dataline session")))
    ((or 'service "service")
     (when variant
       (error "qq: service session does not accept a variant"))
     (format "service:%s"
             (qq-state--canonical-peer-uid target-id "service session")))
    (_ (error "qq: unsupported session type %S" type))))

(defun qq-state-session-key-identity (session-key)
  "Decode canonical SESSION-KEY into its complete immutable identity.

The result contains `type', `target-id', `chat-type', `peer-uid', and
`variant'.  Opaque peer UIDs are decoded only by removing their fixed prefix;
they are never split, normalized, escaped, or reconstructed from metadata."
  (unless (stringp session-key)
    (error "qq: session key must be a string, got %S" session-key))
  (let (type target-id chat-type peer-uid variant guild-id)
    (cond
     ((string-prefix-p "private:" session-key)
      (setq type 'private
            target-id (substring session-key (length "private:"))
            chat-type "1"))
     ((string-prefix-p "group:" session-key)
      (setq type 'group
            target-id (substring session-key (length "group:"))
            chat-type "2"))
     ((string-match
       "\\`guild:\\([1-9][0-9]*\\):channel:\\([1-9][0-9]*\\)\\'"
       session-key)
      (setq type 'guild-channel
            guild-id (match-string 1 session-key)
            target-id (match-string 2 session-key)
            chat-type "4"
            peer-uid target-id
            variant nil))
     ((string-prefix-p "dataline:desktop:" session-key)
      (setq type 'dataline
            target-id (substring session-key (length "dataline:desktop:"))
            chat-type "8"
            peer-uid target-id
            variant "desktop"))
     ((string-prefix-p "dataline:mobile:" session-key)
      (setq type 'dataline
            target-id (substring session-key (length "dataline:mobile:"))
            chat-type "134"
            peer-uid target-id
            variant "mobile"))
     ((string-prefix-p "service:" session-key)
      (setq type 'service
            target-id (substring session-key (length "service:"))
            chat-type "103"
            peer-uid target-id))
     (t
      (error "qq: unsupported canonical session key %S" session-key)))
    (pcase type
      ('private
       (unless (qq-protocol-user-uin-p target-id)
         (error "qq: malformed canonical private session key %S" session-key)))
      ('group
       (unless (qq-protocol-group-uin-p target-id)
         (error "qq: malformed canonical group session key %S" session-key)))
      ('guild-channel
       (unless (and (qq-protocol-uint64-decimal-p guild-id)
                    (qq-protocol-uint64-decimal-p target-id))
         (error "qq: malformed canonical Guild channel session key %S"
                session-key)))
      ((or 'dataline 'service)
       (unless (and (stringp peer-uid) (not (string-empty-p peer-uid)))
         (error "qq: malformed canonical %s session key %S"
                type session-key))))
    `((type . ,type)
      (target-id . ,target-id)
      (chat-type . ,chat-type)
      (peer-uid . ,peer-uid)
      (variant . ,variant)
      ,@(when (eq type 'guild-channel)
          `((guild-id . ,guild-id)
            (channel-id . ,target-id))))))

(defun qq-state-session-key-type (session-key)
  "Return session type symbol extracted from SESSION-KEY."
  (alist-get 'type (qq-state-session-key-identity session-key)))

(defun qq-state-session-sendable-p (session-key)
  "Return non-nil when SESSION-KEY supports outbound messages."
  (condition-case nil
      (let* ((identity (qq-state-session-key-identity session-key))
             (type (alist-get 'type identity)))
        (pcase type
          ((or 'private 'group 'dataline) t)
          (_ nil)))
    (error nil)))

(defun qq-state-session-key-target-id (session-key)
  "Return target id extracted from SESSION-KEY."
  (alist-get 'target-id (qq-state-session-key-identity session-key)))

(defun qq-state--cached-private-title (state target-id)
  "Return best cached title for private TARGET-ID."
  (let ((friend (appkit-data-map-get (qq-state-store-friends-by-id state) (qq-state--normalize-id target-id))))
    (or (and (listp friend)
             (let ((remark (alist-get 'remark friend))
                   (nickname (alist-get 'nickname friend)))
               (cond
                ((and (stringp remark) (not (string-empty-p remark))) remark)
                ((and (stringp nickname) (not (string-empty-p nickname))) nickname)
                (t nil))))
        (qq-state--normalize-id target-id))))

(defun qq-state--cached-group-title (state target-id)
  "Return best cached title for group TARGET-ID."
  (let ((group (appkit-data-map-get (qq-state-store-groups-by-id state) (qq-state--normalize-id target-id))))
    (or (and (listp group)
             (let ((name (alist-get 'group_name group)))
               (and (stringp name)
                    (not (string-empty-p name))
                    name)))
        (qq-state--normalize-id target-id))))

(defun qq-state--default-session-title (state session)
  "Return default title for SESSION using local caches."
  (let ((target-id (alist-get 'target-id session)))
    (pcase (alist-get 'type session)
      ('group
       (qq-state--cached-group-title state target-id))
      ('dataline
       (or (qq-state--first-present-string
            (alist-get 'peer-name session)
            (alist-get 'remark session))
           "我的手机"))
      ('service
       (or (qq-state--first-present-string
            (alist-get 'peer-name session)
            (alist-get 'remark session))
           (qq-state--cached-private-title state
                                           (or (alist-get 'peer-uin session) target-id))))
      (_
       (qq-state--cached-private-title state target-id)))))

(defun qq-state--session-template (session-key)
  "Return base session object for SESSION-KEY."
  (let* ((identity (qq-state-session-key-identity session-key))
         (target-id (alist-get 'target-id identity)))
    `((key . ,session-key)
      ,@identity
      (title . ,target-id)
      ;; Nil is the first-class "unknown" state. Ordinary Message Count and
      ;; the active adapter's explicitly named Badge Count are independent.
      (unread-message-count . nil)
      (unread-badge-count . nil)
      ;; Retained only for protocol-specific projections not yet migrated to
      ;; the ordinary-message/named-badge split (currently guild navigation).
      (unread-count . nil)
      (unread-at-me-message-id . nil)
      (unread-at-me-message-seq . nil)
      (unread-at-all-message-id . nil)
      (unread-at-all-message-seq . nil)
      (muted-p . nil)
      (message-notify-mode . unspecified)
      (first-unread-message-id . nil)
      (first-unread-message-seq . nil)
      (read-position-available . nil)
      (read-latest-message-id . nil)
      (last-message-time . 0)
      (last-message-preview . "")
      (last-message-sender-id . nil)
      (last-message-sender-name . nil)
      (last-message-self-p . nil)
      (last-message-summary-token . 0)
      (last-message-local-id . nil)
      (last-message-order . nil)
      (last-message-seq . nil)
      (last-message-id . nil)
      (last-message-gateway-account-id . nil)
      (last-message-status . nil)
      (oldest-message-id . nil))))

(defun qq-state--merge-alists (old new)
  "Return OLD merged with NEW by symbol key.

Values from NEW replace values in OLD."
  (let ((merged (copy-tree old)))
    (dolist (pair new)
      (setf (alist-get (car pair) merged nil nil #'eq) (cdr pair)))
    merged))

(defun qq-state-message-recalled-p (message)
  "Return non-nil when local MESSAGE has status `recalled'."
  (eq (alist-get 'status message) 'recalled))

(defun qq-state-service-message-p (message)
  "Return non-nil when MESSAGE is a service-owned timeline row."
  (and (listp message)
       (eq (alist-get 'timeline-class message) 'service)))

(defun qq-state-gray-tip-message-data (message)
  "Return MESSAGE's normalized GrayTip semantic data, or nil."
  (when (listp message)
    (when-let* ((segment
                 (seq-find
                  (lambda (candidate)
                    (equal (alist-get 'type candidate) "gray-tip"))
                  (alist-get 'segments message))))
      (alist-get 'data segment))))

(defun qq-state-gray-tip-message-p (message)
  "Return non-nil when MESSAGE contains one typed GrayTip segment."
  (and (qq-state-service-message-p message)
       (qq-state-gray-tip-message-data message)
       t))

(defun qq-state-poke-message-data (message)
  "Return typed Poke GrayTip data from MESSAGE, or nil."
  (when-let* ((data (qq-state-gray-tip-message-data message))
              ((equal (alist-get 'kind data) "poke")))
    data))

(defun qq-state--poke-display-names (state data)
  "Return (ACTOR . TARGET) display names for poke DATA.

Mobile-style self wording is shared by every presentation surface: ACTOR is
\"你\" when the initiating UIN is the current account; TARGET is \"自己\" for a
self-initiated self-targeted poke and \"你\" when the target is the current
account.  Falls back to the wire names, either of which may be nil."
  (let* ((actor-id (qq-state--present-string (alist-get 'actor-id data)))
         (target-id (qq-state--present-string (alist-get 'target-id data)))
         (self-uin (qq-state-self-user-id state))
         (actor-self-p (and actor-id self-uin (equal actor-id self-uin)))
         (target-self-p (and target-id self-uin (equal target-id self-uin)))
         (actor (if actor-self-p
                    "你"
                  (qq-state--present-string (alist-get 'actor-name data))))
         (target (cond
                  ((and actor-self-p target-self-p) "自己")
                  (target-self-p "你")
                  (t (qq-state--present-string
                      (alist-get 'target-name data))))))
    (cons actor target)))

(defun qq-state-poke-message-p (message)
  "Return non-nil when MESSAGE is a typed Poke GrayTip service row."
  (and (qq-state-poke-message-data message) t))

(defun qq-state--as-recalled-message (message)
  "Return MESSAGE with recalled stub fields (kept in store for optional display)."
  (qq-state--merge-alists
   message
   '((status . recalled)
     (segments . nil)
     (raw-message . "[message recalled]")
     (preview . "[message recalled]"))))

(defun qq-state-session (state session-key)
  "Return session object for SESSION-KEY."
  (copy-tree (appkit-data-map-get (qq-state-store-sessions state) session-key)))

(defun qq-state--hydrate-session (state session)
  "Return SESSION enriched with confirmed contact metadata from STATE."
  (let* ((title (alist-get 'title session))
         (target (alist-get 'target-id session))
         (default (qq-state--default-session-title state session))
         (group (and (eq (alist-get 'type session) 'group)
                     (appkit-data-map-get (qq-state-store-groups-by-id state) target)))
         (mode (assq 'message-notify-mode group)))
    (qq-state--merge-alists
     session
     (append (when (and default (or (null title) (equal title target) (string-empty-p title)))
               (list (cons 'title default)))
             (when mode (list mode (cons 'muted-p (and (not (eq (cdr mode) 'notify)) t))))))))

(defun qq-state-upsert-session (state session-key fields)
  "Return STATE with SESSION-KEY's FIELDS merged, and the new session value."
  (let* ((identity (qq-state-session-key-identity session-key))
         (routing (seq-filter (lambda (field)
                                (or (memq (car field) '(type target-id chat-type variant))
                                    (cdr field)))
                              identity))
         (old (or (appkit-data-map-get (qq-state-store-sessions state) session-key)
                  (qq-state--session-template session-key)))
         (session (qq-state--merge-alists old fields)))
    ;; Routing comes exclusively from the key; metadata cannot retarget a row.
    (setq session (qq-state--hydrate-session
                   state (qq-state--merge-alists session routing)))
    (cons (appkit-data-with state ((:sessions) session-key session)) session)))

(defun qq-state-sessions (state)
  "Return all known sessions sorted by last activity."
  (let (sessions)
    (appkit-data-map-each (lambda (_key session)
                            (push (copy-tree session) sessions)) (qq-state-store-sessions state))
    (sort sessions
          (lambda (left right)
            (let ((left-time (qq-state--normalize-time (alist-get 'last-message-time left)))
                  (right-time (qq-state--normalize-time (alist-get 'last-message-time right))))
              (if (/= left-time right-time)
                  (> left-time right-time)
                (string-lessp (or (alist-get 'title left) "")
                              (or (alist-get 'title right) ""))))))))

(defun qq-state--short-media-label (value &optional fallback)
  "Return a short label for media VALUE, never a full URL or CQ blob.

Prefer basename of local paths; drop http(s) URLs entirely."
  (let ((fallback (or fallback "media")))
    (cond
     ((not (stringp value)) fallback)
     ((string-empty-p (string-trim value)) fallback)
     ((string-match-p "\\`https?://" value) fallback)
     ((string-match-p "\\`\\[CQ:" value) fallback)
     ((or (file-name-absolute-p value)
          (string-match-p "/" value)
          (string-match-p "\\\\" value))
      (let ((base (file-name-nondirectory value)))
        (if (or (string-empty-p base)
                (string-match-p "\\`https?:" base))
            fallback
          (truncate-string-to-width base 28 nil nil t))))
     (t (truncate-string-to-width (string-trim value) 28 nil nil t)))))

(defun qq-state-preview-one-line (value)
  "Return VALUE as a compact single-line message preview.

Like telega's `telega-ins--one-lined', line breaks and repeated horizontal
whitespace are presentation details rather than part of a root-row preview."
  (if (stringp value)
      (string-trim
       (replace-regexp-in-string "[[:space:]\u00a0]+" " " value))
    ""))

(defun qq-state--unsupported-preview (data)
  "Return preview text for an unsupported segment DATA.

The gateway deliberately models an element it cannot fully parse as
`unsupported' so it never claims, for example, that a mention is plain text.
That is a constraint on the *model*, not an instruction to hide readable text
from the user: when the gateway kept the element's visible text in
`fallback_text', show it and mark it, rather than replacing the whole element
with a diagnostic.  The `summary' stays available as a tooltip for debugging."
  (let* ((fallback (alist-get 'fallback_text data))
         (fallback (and (stringp fallback)
                        (let ((trimmed (qq-state-preview-one-line fallback)))
                          (and (not (string-empty-p trimmed)) trimmed))))
         (summary (alist-get 'summary data))
         (summary (if (stringp summary)
                      (replace-regexp-in-string "[\n\r\t ]+" " " summary)
                    "unknown element")))
    (if fallback
        (propertize (concat fallback " ⁇")
                    'help-echo (format "unsupported QQ element: %s" summary))
      (format "[unsupported QQ element: %s]"
              (truncate-string-to-width summary 80 nil nil t)))))

(defun qq-state-message-preview-from-segments (state segments)
  "Return a human-readable plain-text preview for message SEGMENTS.

Never emit legacy CQ wire strings.  Reply segments are omitted (shown via
reply chrome elsewhere).  Media becomes short placeholders like
`[image]' / `[face:178]'."
  (qq-state-preview-one-line
   (mapconcat
    (lambda (segment)
      (let ((type (alist-get 'type segment))
            (data (alist-get 'data segment)))
        (pcase type
          ("text" (or (alist-get 'text data) ""))
          ("at" (concat "@" (or (alist-get 'name data)
                                (alist-get 'qq data)
                                "mention")))
          ;; Reply chrome is rendered separately in chatbuf / composer.
          ("reply" "")
          ("__unsupported" (qq-state--unsupported-preview data))
          ;; Native gateway app card.  Display only: never follow an Ark
          ;; action or open a URL from this segment.
          ("light_app"
           (or (qq-state--present-string (alist-get 'prompt data))
               (let ((app (qq-state--present-string (alist-get 'app data))))
                 (if (equal app "com.tencent.multimsg")
                     "[聊天记录]"
                   (format "[card:%s]" (or app "app"))))))
          ;; Native gateway group file.  `file_size' is a decimal string.
          ("group_file"
           (format "[file:%s]"
                   (qq-state--short-media-label
                    (alist-get 'file_name data) "file")))
          ("face"
           (let* ((raw (alist-get 'raw data))
                  (text (or (alist-get 'description data)
                            (alist-get 'faceText data)
                            (alist-get 'face_text data)
                            (and (listp raw) (alist-get 'faceText raw))
                            (and (listp raw) (alist-get 'face_text raw))))
                  (id (or (alist-get 'id data)
                          (alist-get 'faceIndex data)
                          (and (listp raw) (alist-get 'faceIndex raw))))
                  (named (and id
                              (fboundp 'qq-media-face-text-fallback)
                              (qq-media-face-text-fallback id))))
             (cond
              ((and (stringp text) (not (string-empty-p text))) text)
              ((and (stringp named) (not (string-empty-p named))) named)
              (id (format "[face:%s]" id))
              (t "[face]"))))
          ;; Compact previews stay telega-short; chat body uses media cards.
          ("image" "[image]")
          ((or "mface" "favorite_emoji") "[sticker]")
          ("file"
           (format "[file:%s]"
                   (qq-state--short-media-label
                    (or (alist-get 'file_name data)
                        (alist-get 'name data)
                        (alist-get 'file data))
                    "file")))
          ("record" "[voice]")
          ("video" "[video]")
          ("mail"
           (or (qq-state--present-string (alist-get 'prompt data))
               (let ((parts (delq nil
                                  (list (qq-state--present-string
                                         (alist-get 'sender data))
                                        (qq-state--present-string
                                         (alist-get 'subject data))))))
                 (and parts (string-join parts ": ")))
               "[mail]"))
          ("wallet"
           (let* ((receiver (alist-get 'receiver data))
                  (sender (alist-get 'sender data))
                  (kind (alist-get 'wallet_kind data)))
             (or (qq-state--present-string (alist-get 'notice receiver))
                 (qq-state--present-string (alist-get 'title receiver))
                 (qq-state--present-string (alist-get 'notice sender))
                 (qq-state--present-string (alist-get 'title sender))
                 (pcase kind
                   ("transfer" "[转账]")
                   ("red-packet" "[QQ红包]")
                   ("password-red-packet" "[口令红包]")
                   (_ "[QQ钱包]")))))
          ("card"
           (or (qq-state--present-string (alist-get 'prompt data))
               (qq-state--present-string (alist-get 'title data))
               (qq-state--present-string (alist-get 'content data))
               "[card]"))
          ("json" "[card]")
          ("xml" "[xml]")
          ("gray-tip"
           (if (equal (alist-get 'kind data) "poke")
               (let* ((names (qq-state--poke-display-names state data))
                      (actor (car names))
                      (action
                       (qq-state--present-string (alist-get 'action data)))
                      (target (cdr names))
                      (detail
                       (qq-state--present-string (alist-get 'detail data))))
                 (or (and actor action target
                          (concat actor " " action " " target detail))
                     (and actor action (concat actor " " action detail))
                     (and action target (concat action " " target detail))
                     (and action (concat action detail))
                     (and target (concat "戳了戳 " target))
                     "[poke]"))
             (or (qq-state--present-string (alist-get 'text data))
                 "QQ system notice")))
          ("dice" "[dice]")
          ("rps" "[rps]")
          ("share" "[share]")
          ("location" "[location]")
          ("music" "[music]")
          ("forward" "[forward]")
          ("node" "[forward]")
          ("markdown" (or (alist-get 'content data)
                          (alist-get 'text data)
                          "[markdown]"))
          (_ (format "[%s]" (or type "message"))))))
    (or segments '())
    " ")))

(defun qq-state--mention-kinds-from-segments (state segments)
  "Return native QQ mention kinds found in SEGMENTS.

`at-me' denotes a direct mention of the current account and `at-all' denotes
QQ's @全体成员.  Ordinary mentions of another member are deliberately ignored."
  (let ((self-id (qq-state-self-user-id state))
        kinds)
    (dolist (segment (or segments '()))
      (when (equal (alist-get 'type segment) "at")
        (let* ((data (alist-get 'data segment))
               (target (qq-state--normalize-id (alist-get 'qq data))))
          (cond
           ((equal target "all") (cl-pushnew 'at-all kinds))
           ((and self-id (equal target self-id))
            (cl-pushnew 'at-me kinds))))))
    (nreverse kinds)))

(defun qq-state-message-mention-kinds (message)
  "Return normalized native mention kinds for MESSAGE."
  (copy-sequence (or (alist-get 'mention-kinds message) '())))

(defun qq-state-message-mentions-self-p (message)
  "Return non-nil when MESSAGE directly mentions the current account."
  (and (memq 'at-me (qq-state-message-mention-kinds message)) t))

(defun qq-state-message-mentions-all-p (message)
  "Return non-nil when MESSAGE contains QQ's @全体成员."
  (and (memq 'at-all (qq-state-message-mention-kinds message)) t))

(defun qq-state--cq-looks-p (string)
  "Return non-nil when STRING contains a legacy CQ wire token."
  (and (stringp string)
       (string-match-p "\\[CQ:" string)))

(defun qq-state-message-preview (state message)
  "Return human-readable preview text for normalized MESSAGE.

Native messages carry structured segments.  Suppress legacy CQ text rather
than parsing a second, lossy message protocol in the UI."
  (qq-state-preview-one-line
   (or (let ((from-segments
              (qq-state-message-preview-from-segments state
                                                      (alist-get 'segments message))))
         (and (stringp from-segments)
              (not (string-empty-p from-segments))
              from-segments))
       (let ((stored (alist-get 'preview message)))
         (and (stringp stored)
              (not (string-empty-p (string-trim stored)))
              (not (qq-state--cq-looks-p stored))
              stored))
       (let ((raw (alist-get 'raw-message message)))
         (and (stringp raw)
              (not (string-empty-p (string-trim raw)))
              (not (qq-state--cq-looks-p raw))
              raw))
       "")))

(defun qq-state--gray-tip-user-name (state user-id explicit-name)
  "Return a GrayTip display name for USER-ID, preferring EXPLICIT-NAME.
An explicit name equal to the UIN is a placeholder; fall back to self/friend
metadata before displaying the bare UIN."
  (let* (
         (user-id (qq-state--normalize-id user-id))
         (friend (and user-id (appkit-data-map-get (qq-state-store-friends-by-id state) user-id)))
         (self-p (and user-id (equal user-id (qq-state-self-user-id state))))
         (explicit (and (stringp explicit-name)
                        (not (string-empty-p explicit-name))
                        (not (equal explicit-name user-id))
                        explicit-name)))
    (or explicit
        (and self-p (qq-state--present-string (alist-get 'nickname (qq-state-store-self-info state))))
        (and friend (qq-state--present-string (alist-get 'remark friend)))
        (and friend (qq-state--present-string (alist-get 'nickname friend)))
        user-id "某人")))

(defun qq-state--native-reply-data (target)
  "Map one validated native reply TARGET to timeline segment data."
  (unless (and (listp target)
               (equal (alist-get 'kind target) "native")
               (or (not (assq 'preview target)) (stringp (alist-get 'preview target)))
               (qq-protocol-uint64-decimal-p
                (alist-get 'sequence target)))
    (error "qq: native reply target is invalid"))
  `((message_seq . ,(alist-get 'sequence target))
    ,@(when (assq 'preview target) `((preview . ,(alist-get 'preview target))))
    ,@(when (assq 'sender target)
        `((sender . ,(copy-tree (alist-get 'sender target)))))
    ,@(when (assq 'sender_name target)
        `((sender_name . ,(alist-get 'sender_name target))))
    ,@(when (assq 'sent_at target)
        `((sent_at . ,(alist-get 'sent_at target))))))

(defun qq-state--pending-message (state session-key segments &optional raw-message)
  "Return (STATE . MESSAGE) allocating one optimistic local message."
  (let* ((counter (1+ (qq-state-store-local-message-counter state)))
         (local-id (format "local-%d" counter))
         (name (or (qq-state--first-present-string
                    (alist-get 'nickname (qq-state-store-self-info state)))
                   (qq-state-self-user-id state) "me"))
         (preview (qq-state-message-preview-from-segments state segments))
         (order (appkit-data-step state (qq-state--next-message-order state))))
    ;; Backquote may share constant cons cells such as `(status . pending)'
    ;; across calls.  Timeline messages are mutable projection objects, so
    ;; return an entirely owned tree before any operation enriches one.
    (cons (appkit-data-with state (() :local-message-counter counter))
          (copy-tree
           `((id . ,local-id) (local-id . ,local-id) (session-key . ,session-key)
             (time . ,(truncate (float-time))) (order . ,order)
             (sender-id . ,(qq-state-self-user-id state)) (sender-name . ,name)
             (sender-secondary-name . nil) (sender-card . nil) (sender-nickname . ,name)
             (sender-remark . nil) (self-p . t) (status . pending) (timeline-class . authored)
             (segments . ,segments) (raw-message . ,(or raw-message preview "")) (preview . ,preview))))))

(defun qq-state--message-sort< (left right)
  "Return non-nil when LEFT should sort before RIGHT."
  (let ((left-time (qq-state--normalize-time (alist-get 'time left)))
        (right-time (qq-state--normalize-time (alist-get 'time right))))
    (if (/= left-time right-time)
        (< left-time right-time)
      (< (or (alist-get 'order left) 0)
         (or (alist-get 'order right) 0)))))

(defun qq-state--find-message (messages predicate)
  "Return first element in MESSAGES matching PREDICATE."
  (seq-find predicate messages))

(defun qq-state--native-message-correlation-key (message)
  "Return authored MESSAGE's transport correlation key, or nil.

The key is scoped by the caller's session and is not a public identity.  It
exists only to correlate an id-less authored history row with its live form.
Service Timeline Messages are independently addressable rows and may share a
sequence, so they must converge only by exact row identity."
  (let ((sequence (alist-get 'message-seq message))
        (random (alist-get 'native-random message)))
    (when (and (not (qq-state-service-message-p message))
               (qq-protocol-uint64-decimal-p sequence))
      (if (equal (alist-get 'message-type message) "group")
          ;; Group sequence is itself the conversation-scoped native locator
          ;; used by history, reply seek, and recall.  Some history builds omit
          ;; random, so requiring it would leave a duplicate when a live push
          ;; later supplies the exact snowflake.
          (cons 'group sequence)
        (when (and (integerp random)
                   (<= 0 random #xffffffff))
          (list 'private sequence random))))))

(defun qq-state--pending-segment-signature (segment)
  "Return stable optimistic reconciliation signature for one SEGMENT.

Only fields preserved across send payloads and self websocket events are used.
Media resource ids and paths are deliberately excluded because the kernel
rewrites them during upload; equal media-only sends are reconciled FIFO by the
surrounding message matcher."
  (let* ((type (alist-get 'type segment))
         (data (alist-get 'data segment)))
    (pcase type
      ("text"
       (let ((text (alist-get 'text data)))
         (and (stringp text) (list type text))))
      ("at"
       (when-let* ((target (qq-state--normalize-id (alist-get 'qq data))))
         (list type target)))
      ("reply"
       (when-let* ((message-id (alist-get 'id data)))
         (list type message-id)))
      ("face"
       (when-let* ((face-id (qq-state--normalize-id (alist-get 'id data))))
         (list type face-id)))
      ("image"
       (list type (and (member (alist-get 'sub_type data) '(1 "1")) t)))
      ("favorite_emoji" (list "image" t))
      ((or "mface" "video" "record" "file")
       (list type))
      (_ nil))))

(defun qq-state--pending-message-signature (message)
  "Return stable segment signature for optimistic reconciliation of MESSAGE."
  (when-let* ((segments (alist-get 'segments message))
              ((listp segments)))
    (let ((signature (mapcar #'qq-state--pending-segment-signature segments)))
      (and (not (memq nil signature)) signature))))

(defun qq-state--weak-pending-match (messages message)
  "Return weak pending match in MESSAGES for self-sent MESSAGE."
  (let ((self-p (alist-get 'self-p message))
        (raw-message (alist-get 'raw-message message))
        (signature (qq-state--pending-message-signature message))
        (time (qq-state--normalize-time (alist-get 'time message))))
    (when self-p
      (qq-state--find-message
       messages
       (lambda (it)
         (and (alist-get 'self-p it)
              (alist-get 'local-id it)
              ;; Weak matching is FIFO ownership among unresolved local rows.
              ;; A settled row is matched directly by server-id; admitting it
              ;; here lets the next equal media event overwrite the first.
              (eq (alist-get 'status it) 'pending)
              (null (alist-get 'server-id it))
              (let ((pending-signature
                     (qq-state--pending-message-signature it)))
                ;; Stable structure wins whenever both sides expose it.  Raw
                ;; preview text is only a compatibility path for unsupported
                ;; segment kinds; equal display names must not merge distinct
                ;; @targets, replies, or base faces.
                (if (and signature pending-signature)
                    (equal pending-signature signature)
                  (equal (alist-get 'raw-message it) raw-message)))
              (<= (abs (- time (qq-state--normalize-time (alist-get 'time it))))
                  qq-self-message-dedupe-window)))))))

(defun qq-state--poke-echo-match (messages message)
  "Return the nearest opposite-provenance poke echo for MESSAGE.

The Gateway may deliver the push before or after the `message.send_poke'
response.  Match exactly one local/remote pair by actor, target and time while
leaving repeated pokes as distinct timeline records."
  (when (qq-state-poke-message-p message)
    (let* ((sender-id (alist-get 'sender-id message))
           (target-id (alist-get 'target-id message))
           (time (qq-state--normalize-time (alist-get 'time message)))
           (local-p (alist-get 'local-poke-p message))
           (candidates
            (seq-filter
             (lambda (it)
               (and (qq-state-poke-message-p it)
                    (not (alist-get 'poke-echo-reconciled-p it))
                    (not (eq (and (alist-get 'local-poke-p it) t)
                             (and local-p t)))
                    (equal (alist-get 'sender-id it) sender-id)
                    (equal (alist-get 'target-id it) target-id)
                    (<= (abs (- time
                                (qq-state--normalize-time
                                 (alist-get 'time it))))
                        qq-self-message-dedupe-window)))
             messages)))
      (car
       (sort candidates
             (lambda (left right)
               (< (abs (- time
                          (qq-state--normalize-time (alist-get 'time left))))
                  (abs (- time
                          (qq-state--normalize-time
                           (alist-get 'time right)))))))))))

(defun qq-state--index-message (state message)
  "Return STATE with MESSAGE's exact identities indexed, and nil."
  (let ((server-id (alist-get 'server-id message))
        (local-id (alist-get 'local-id message))
        (session-key (alist-get 'session-key message)))
    (when (and server-id session-key (not (qq-state-service-message-p message)))
      (setq state (appkit-data-with state ((:message-session-index) server-id session-key))))
    (when (and local-id session-key)
      (setq state (appkit-data-with state ((:local-message-session-index) local-id session-key))))
    (cons state nil)))

(defun qq-state--unindex-message (state message)
  "Return STATE with MESSAGE's owned index entries removed, and nil."
  (let ((server-id (alist-get 'server-id message))
        (local-id (alist-get 'local-id message))
        (session-key (alist-get 'session-key message)))
    (when (and server-id session-key (not (qq-state-service-message-p message))
               (equal session-key
                      (appkit-data-map-get (qq-state-store-message-session-index state) server-id)))
      (setq state (appkit-data-with state ((:message-session-index) server-id appkit-data-delete))))
    (when (and local-id session-key
               (equal session-key
                      (appkit-data-map-get (qq-state-store-local-message-session-index state) local-id)))
      (setq state (appkit-data-with state ((:local-message-session-index) local-id appkit-data-delete))))
    (cons state nil)))

(defun qq-state--session-summary-position-compare
    (fields session &optional current-local-resolved-p)
  "Compare candidate summary FIELDS with current SESSION position.

Return -1, 0, or 1.  Compare server timestamps first and exact per-session
message sequences inside the same second.  NT message ids are identities, not
ordering keys: self and incoming messages can occupy incomparable id ranges.
Local insertion order and freshness tokens break otherwise unknown ties."
  (let ((candidate-id (alist-get 'last-message-id fields))
        (candidate-local-id (alist-get 'last-message-local-id fields))
        (candidate-seq (alist-get 'last-message-seq fields))
        (candidate-order (or (alist-get 'last-message-order fields) 0))
        (current-id (alist-get 'last-message-id session))
        (current-seq (alist-get 'last-message-seq session))
        (current-order (or (alist-get 'last-message-order session) 0))
        (candidate-time
         (qq-state--normalize-time (alist-get 'last-message-time fields)))
        (current-time
         (qq-state--normalize-time (alist-get 'last-message-time session))))
    (cond
     ((< candidate-time current-time) -1)
     ((> candidate-time current-time) 1)
     ((and (qq-protocol-uint64-decimal-p candidate-seq)
           (qq-protocol-uint64-decimal-p current-seq))
      (qq-protocol-decimal-string-compare candidate-seq current-seq))
     ;; The same canonical identity denotes the same frontier even when one
     ;; observation carries less sequence metadata than the other.
     ((and candidate-id (equal candidate-id current-id)) 0)
     ;; A promotion is the same optimistic row acquiring its server id.
     ((and (stringp current-id)
           (string-prefix-p "local-" current-id)
           (equal candidate-local-id current-id))
      0)
     ;; Once canonical storage proves that the current optimistic row acquired
     ;; a server id, its local frontier no longer shields equal-second server
     ;; messages.  The caller supplies the latest sequence/time candidate from
     ;; a cache which includes that promoted row.
     ((and current-local-resolved-p
           (qq-protocol-uint64-decimal-p candidate-id)
           (stringp current-id)
           (string-prefix-p "local-" current-id))
      1)
     ((and (stringp candidate-id) (string-prefix-p "local-" candidate-id)
           (stringp current-id) (string-prefix-p "local-" current-id))
      (cond ((< candidate-order current-order) -1)
            ((> candidate-order current-order) 1)
            (t 0)))
     ;; Equal-second server snapshots cannot displace an incomparable local
     ;; optimistic row.  Conversely, a newly inserted local row follows the
     ;; last known server message even when their integer timestamps tie.
     ((and (stringp current-id) (string-prefix-p "local-" current-id)) -1)
     ((and (stringp candidate-id) (string-prefix-p "local-" candidate-id)) 1)
     ((< candidate-order current-order) -1)
     ((> candidate-order current-order) 1)
     (t 0))))

(defun qq-state--apply-session-summary
    (state session-key fields &optional observation-token low-information-p current-local-resolved-p)
  "Apply FIELDS only when their observation and message frontier are current."
  (let* ((token (or observation-token
                    (appkit-data-step state (qq-state-session-summary-observation-start state))))
         (session (or (qq-state-session state session-key)
                      (qq-state--session-template session-key)))
         (position (qq-state--session-summary-position-compare
                    fields session current-local-resolved-p))
         (accepted (and (> token (or (alist-get 'last-message-summary-token session) 0))
                        (>= position 0))))
    (unless (and (integerp token) (> token 0))
      (error "qq: session summary observation token must be positive"))
    (when accepted
      (when (and low-information-p (= position 0)
                 (equal (alist-get 'last-message-id fields) (alist-get 'last-message-id session)))
        (setq fields (seq-remove (lambda (entry)
                                   (memq (car entry) '(last-message-local-id last-message-order
                                                                             last-message-self-p last-message-sender-name
                                                                             last-message-sender-id))) fields)))
      (appkit-data-step state
        (qq-state-upsert-session state session-key
                                 (cons (cons 'last-message-summary-token token) fields))))
    (cons state accepted)))

(defun qq-state--message-summary-fields (state message)
  "Return root latest-message summary fields for normalized MESSAGE."
  (let ((special-p (qq-state-service-message-p message)))
    `((last-message-time
       . ,(qq-state--normalize-time (alist-get 'time message)))
      (last-message-id . ,(or (alist-get 'server-id message)
                              (alist-get 'id message)))
      (last-message-seq . ,(or (alist-get 'root-message-seq message)
                               (alist-get 'message-seq message)))
      (last-message-local-id . ,(alist-get 'local-id message))
      (last-message-order . ,(alist-get 'order message))
      (last-message-preview . ,(qq-state-message-preview state message))
      (last-message-sender-id
       . ,(unless special-p
            (or (alist-get 'sender-id message)
                (alist-get 'sender-native-id message))))
      (last-message-sender-name
       . ,(unless special-p (alist-get 'sender-name message)))
      (last-message-self-p
       . ,(and (not special-p) (alist-get 'self-p message) t))
      (last-message-gateway-account-id
       . ,(alist-get 'gateway-account-id message))
      (last-message-status . ,(alist-get 'status message)))))

(defun qq-state--latest-summary-message (state messages)
  "Return the newest summary candidate in normalized MESSAGES.

Compare timestamps before materializing summary presentation.  Only messages
in the same second need the native sequence and optimistic-row tie breakers;
history pages must not rebuild every cached message's preview for each row."
  (let (latest latest-time latest-fields)
    (dolist (message messages latest)
      (let ((time (qq-state--normalize-time (alist-get 'time message))))
        (cond
         ((or (null latest) (> time latest-time))
          (setq latest message latest-time time latest-fields nil))
         ((= time latest-time)
          (let* ((candidate-fields (qq-state--message-summary-fields state message))
                 (position
                  (qq-state--session-summary-position-compare
                   candidate-fields
                   (or latest-fields
                       (setq latest-fields (qq-state--message-summary-fields state latest))))))
            (when (or (> position 0)
                      (and (= position 0)
                           (> (or (alist-get 'order message) 0)
                              (or (alist-get 'order latest) 0))))
              (setq latest message latest-fields candidate-fields)))))))))

(defun qq-state--sync-session-summary (state session-key &optional observation-token)
  "Update SESSION-KEY's bounds and summary from its indexed extrema."
  (let* ((history (qq-state--history state session-key))
         (latest (appkit-data-map-get (qq-state-history-rows history)
                                      (qq-state-history-latest history)))
         (current-id (alist-get 'last-message-id (qq-state-session state session-key)))
         (resolved (and (stringp current-id) (string-prefix-p "local-" current-id)
                        (alist-get 'server-id (qq-state--session-message state session-key current-id)))))
    (appkit-data-step state
      (qq-state-upsert-session state session-key
                               `((oldest-message-id . ,(qq-state-history-oldest history)))))
    (when latest
      (appkit-data-step state
        (qq-state--apply-session-summary state session-key
                                         (qq-state--message-summary-fields state latest)
                                         observation-token nil resolved)))
    (cons state nil)))

(defun qq-state--set-session-summary (state session-key latest)
  "Authoritatively set SESSION-KEY's conversation head to LATEST or nil."
  (let ((token (appkit-data-step state (qq-state-session-summary-observation-start state))))
    (qq-state-upsert-session
     state session-key
     (append
      (if latest (qq-state--message-summary-fields state latest)
        '((last-message-time . 0) (last-message-preview . "") (last-message-sender-id . nil)
          (last-message-sender-name . nil) (last-message-self-p . nil) (last-message-local-id . nil)
          (last-message-order . nil) (last-message-seq . nil) (last-message-id . nil)
          (last-message-gateway-account-id . nil) (last-message-status . nil)))
      `((last-message-summary-token . ,token)
        (oldest-message-id . ,(qq-state-history-oldest (qq-state--history state session-key))))))))

(defun qq-state-delete-local-message (state session-key row-key)
  "Remove the exact durable ROW-KEY from STATE, returning its previous row."
  (let ((previous (qq-state--row (qq-state--history state session-key)
                                 (cons 'canonical-row-key row-key))))
    (when previous
      (appkit-data-step state (qq-state--put-message state session-key nil previous)))
    (cons state previous)))

(defun qq-state-session-messages (state session-key)
  "Return SESSION-KEY's messages in display order from STATE."
  (sort (copy-tree (appkit-data-map-values (qq-state-history-rows (qq-state--history state session-key))))
        #'qq-state--message-sort<))

(defun qq-state--session-message (state session-key &rest identities)
  "Read the first exact server, local or opaque ID in IDENTITIES from STATE."
  (let ((history (qq-state--history state session-key)))
    (seq-some (lambda (id)
                (and id
                     (or (qq-state--row history (cons 'server-id id))
                         (qq-state--row history (cons 'local-id id))
                         (qq-state--row history (cons 'id id)))))
              identities)))

(defun qq-state-session-oldest-message-id (state session-key)
  "Return oldest server-backed message id for SESSION-KEY."
  (alist-get 'oldest-message-id (appkit-data-map-get (qq-state-store-sessions state) session-key)))

(defun qq-state-insert-pending-message (state session-key segments &optional raw-message)
  "Insert one optimistic message into STATE and return it."
  (let ((message (appkit-data-step state
                   (qq-state--pending-message state session-key segments raw-message))))
    (appkit-data-step state (qq-state--put-message state session-key message))
    (appkit-data-step state (qq-state--sync-session-summary state session-key))
    (cons state message)))

(defun qq-state--message-patch-journal-key (session-key message-anchor)
  "Return the journal key for stable MESSAGE-ANCHOR in SESSION-KEY."
  (cons session-key message-anchor))



(defun qq-state--journal-message-patch
    (state session-key anchor patch)
  "Retain permanent recall tombstones in STATE."
  (cons (pcase (plist-get patch :kind)
          ('recall
           (appkit-data-with state
             ((:message-patch-journal) (qq-state--message-patch-journal-key session-key anchor)
              '(:recalled-p t))))
          ('emoji-like state)
          (kind (error "qq: unsupported message patch kind %S" kind)))
        nil))

(defun qq-state--materialize-message-patches
    (state session-key message)
  "Apply the permanent recall tombstone for MESSAGE in SESSION-KEY."
  (let ((entry (appkit-data-map-get
                (qq-state-store-message-patch-journal state)
                (qq-state--message-patch-journal-key session-key (qq-state-message-anchor message)))))
    (if (plist-get entry :recalled-p) (qq-state--as-recalled-message message) message)))

(defun qq-state--merge-local-segment-presentation
    (existing incoming merged)
  "Overlay safe client-local media fields from EXISTING onto MERGED.

INCOMING remains authoritative for segment kind and remote identities.  This
only preserves absolute source paths from a correlated optimistic row, keeping
local presentation independent of the authoritative opaque `media_id'."
  (let ((existing-segments (and existing (alist-get 'segments existing)))
        (incoming-segments (alist-get 'segments incoming)))
    (if (not (and (alist-get 'local-id existing)
                  (alist-get 'server-id incoming)
                  (= (length existing-segments) (length incoming-segments))
                  (cl-every
                   (lambda (pair)
                     (let ((old (car pair))
                           (new (cdr pair)))
                       (and (equal (alist-get 'type old)
                                   (alist-get 'type new))
                            (member (alist-get 'type new)
                                    '("image" "file" "video" "record")))))
                   (cl-mapcar #'cons existing-segments incoming-segments))))
        merged
      (let ((preserved (copy-tree merged)))
        (cl-mapc
         (lambda (old new)
           (let ((old-data (alist-get 'data old))
                 (new-data (alist-get 'data new)))
             (dolist (key '(file path))
               (let ((value (alist-get key old-data)))
                 (when (and (stringp value)
                            (file-name-absolute-p value)
                            (null (assq key new-data)))
                   (setf (alist-get key new-data nil nil #'eq) value))))
             (setf (alist-get 'data new nil nil #'eq) new-data)))
         existing-segments (alist-get 'segments preserved))
        preserved))))

(defun qq-state--merge-normalized-message
    (state session-key message &optional summary-observation-token source)
  "Merge MESSAGE by exact identity, then transport correlation or pending echo.
Return (STATE MESSAGE MUTATION PREVIOUS-ANCHOR)."
  ;; Sorting owns the list spine.  Existing rows are read-only and replaced
  ;; by fresh merged alists, so unrelated message trees need no deep copy.
  (let* ((history (qq-state--history state session-key))
         (rows (qq-state-history-rows history))
         (direct (or (when-let* ((row-key (alist-get 'canonical-row-key message)))
                       (qq-state--row history (cons 'canonical-row-key row-key)))
                     (qq-state--session-message state session-key
                                                (alist-get 'server-id message)
                                                (alist-get 'local-id message)
                                                (alist-get 'id message))))
         (correlated (when-let* ((key (qq-state--native-message-correlation-key message)))
                       (qq-state--row history (cons 'native key))))
         (correlated (unless (eq correlated direct) correlated))
         (history-p (eq source 'history))
         (identity (cond ((null direct) correlated)
                         ((not (and history-p correlated)) direct)
                         ((alist-get 'local-id direct) direct)
                         ((alist-get 'local-id correlated) correlated)
                         (t direct)))
         (redundant (and direct correlated
                         (or (not history-p) (alist-get 'local-id direct)
                             (alist-get 'local-id correlated))
                         (if (eq identity direct) correlated direct)))
         (poke (and (null direct) (null correlated)
                    (qq-state--poke-echo-match
                     (mapcar (lambda (key) (appkit-data-map-get rows key))
                             (qq-state-history-pokes history)) message)))
         (existing (or identity poke
                       (qq-state--weak-pending-match
                        (mapcar (lambda (key) (appkit-data-map-get rows key))
                                (qq-state-history-pending history)) message)))
         (merged (cond ((and poke (alist-get 'local-poke-p message)
                             (not (alist-get 'local-poke-p existing))) existing)
                       (existing (qq-state--merge-alists existing message))
                       (t message))))
    (when (and existing
               (or history-p
                   (and (alist-get 'canonical-row-key message)
                        (equal (alist-get 'canonical-row-key existing)
                               (alist-get 'canonical-row-key message)))))
      ;; Keep established identity through an older/ID-less snapshot, while
      ;; allowing a canonical local row to acquire its first remote Message ID.
      (setq merged (qq-state--merge-alists
                    merged (seq-filter (lambda (entry)
                                         (and (memq (car entry) '(id server-id local-id))
                                              (cdr entry)))
                                       existing))))
    (setq merged (qq-state--merge-local-segment-presentation existing message merged))
    (when poke (setq merged (qq-state--merge-alists merged '((poke-echo-reconciled-p . t)))))
    (setq merged
          (qq-state--merge-alists
           merged `((order . ,(or (alist-get 'order existing)
                                  (alist-get 'order merged)
                                  (appkit-data-step state (qq-state--next-message-order state)))))))
    (when (and existing (qq-state-message-recalled-p existing))
      (setq merged (qq-state--as-recalled-message merged)))
    (setq merged (qq-state--materialize-message-patches state session-key merged))
    (unless (and (equal existing merged) (null redundant) (null summary-observation-token))
      (when redundant (appkit-data-step state (qq-state--put-message state session-key nil redundant)))
      (appkit-data-step state (qq-state--put-message state session-key merged existing))
      (appkit-data-step state
        (qq-state-upsert-session
         state session-key
         (seq-filter (lambda (entry) (and (memq (car entry) '(peer-name peer-uid peer-uin))
                                          (cdr entry))) merged)))
      (appkit-data-step state (qq-state--sync-session-summary state session-key summary-observation-token)))
    (cons state
          (list merged (if existing 'update 'create)
                (or (and redundant (qq-state-message-anchor redundant))
                    (and existing
                         (not (equal (qq-state-message-anchor existing) (qq-state-message-anchor merged)))
                         (qq-state-message-anchor existing)))))))

(defun qq-state-mark-pending-message-sent
    (state session-key local-id message-id)
  "Promote LOCAL-ID to exact MESSAGE-ID without changing earlier snapshots."
  (let* ((previous (qq-state--session-message state session-key local-id))
         (id (qq-protocol-optional-message-id message-id "send_msg response"))
         updated)
    (when (and previous id)
      (setq updated
            (qq-state--materialize-message-patches
             state session-key
             (qq-state--merge-alists previous `((id . ,id) (server-id . ,id)
                                                (status . sent) (error . nil)))))
      (appkit-data-step state (qq-state--put-message state session-key updated previous))
      (appkit-data-step state (qq-state--sync-session-summary state session-key)))
    (cons state updated)))

(defun qq-state-mark-pending-message-failed (state session-key local-id reason)
  "Fail a still unresolved local send; confirmed rows retain their authority."
  (let ((previous (qq-state--session-message state session-key local-id)) updated)
    (when (and previous (eq (alist-get 'status previous) 'pending)
               (null (alist-get 'server-id previous)))
      (setq updated (qq-state--merge-alists previous `((status . failed) (error . ,reason))))
      (appkit-data-step state (qq-state--put-message state session-key updated previous))
      (appkit-data-step state (qq-state--sync-session-summary state session-key)))
    (cons state updated)))

(defconst qq-state--session-read-projection-keys
  '(unread-message-count unread-badge-count
    first-unread-message-id first-unread-message-seq
    unread-at-me-message-id unread-at-me-message-seq
    unread-at-all-message-id unread-at-all-message-seq
    read-position-available read-latest-message-id)
  "Session fields written by an authoritative unread materialization.")

(defun qq-state--session-read-projection (session)
  "Return the authoritative read projection of SESSION."
  (mapcar (lambda (key) (cons key (alist-get key session)))
          qq-state--session-read-projection-keys))

(defun qq-state--validate-session-read-projection (projection)
  "Return an isolated normalized read PROJECTION, or signal an error."
  (let ((keys (mapcar #'car projection))
        (message-count (alist-get 'unread-message-count projection))
        (badge-count (alist-get 'unread-badge-count projection)))
    (unless (and (proper-list-p projection)
                 (= (length projection)
                    (length qq-state--session-read-projection-keys))
                 (null (seq-difference
                        keys qq-state--session-read-projection-keys))
                 (= (length keys) (length (delete-dups (copy-sequence keys)))))
      (error "qq: invalid internal session read projection"))
    (dolist (count (list message-count badge-count))
      (unless (or (null count) (and (integerp count) (>= count 0)))
        (error "qq: internal unread count must be nil or non-negative")))
    (when (and message-count badge-count (< badge-count message-count))
      (error "qq: exact badge count cannot be below exact message count"))
    (dolist (key '(first-unread-message-id
                   unread-at-me-message-id unread-at-all-message-id
                   read-latest-message-id))
      (qq-protocol-optional-message-id
       (alist-get key projection) "internal read projection"))
    (dolist (key '(first-unread-message-seq
                   unread-at-me-message-seq unread-at-all-message-seq))
      (let ((sequence (alist-get key projection)))
        (unless (or (null sequence)
                    (qq-protocol-message-sequence-p sequence))
          (error "qq: internal read projection has invalid sequence"))))
    (unless (memq (alist-get 'read-position-available projection) '(nil t))
      (error "qq: internal read-position availability must be boolean"))
    (let* ((first-id (alist-get 'first-unread-message-id projection))
           (first-seq (alist-get 'first-unread-message-seq projection))
           (at-me-id (alist-get 'unread-at-me-message-id projection))
           (at-me-seq (alist-get 'unread-at-me-message-seq projection))
           (at-all-id (alist-get 'unread-at-all-message-id projection))
           (at-all-seq (alist-get 'unread-at-all-message-seq projection))
           (available (alist-get 'read-position-available projection))
           (unread-position-values
            (list first-id first-seq at-me-id at-me-seq at-all-id at-all-seq)))
      (unless (and (or (null at-me-id) at-me-seq)
                   (or (null at-all-id) at-all-seq)
                   (cond
                    ((and message-count (zerop message-count))
                     (and (not available)
                          (seq-every-p #'null unread-position-values)))
                    (t
                     (and (or (null first-id) first-seq)
                          (eq available (and first-id t))))))
        (error "qq: inconsistent internal session read projection"))))
  (copy-tree projection))

(defun qq-state-apply-session-read-projection (state session-key projection)
  "Apply the authoritative read PROJECTION, preserving STATE on an equal value."
  (qq-state-upsert-session state session-key (qq-state--validate-session-read-projection projection)))

(defun qq-state-validate-message-session (state session-key message-id)
  "Reject known SESSION-KEY contradictions for exact MESSAGE-ID.

Return the validated NT snowflake string.  This cache-index check is not an
ownership proof for an unknown id; outbound mutations must carry a closed
locator-qualified message reference."
  (qq-state-session-key-identity session-key)
  (setq message-id
        (or (qq-protocol-optional-message-id
             message-id "message/session validation")
            (error "qq: message/session validation requires message_id")))
  (let ((indexed (appkit-data-map-get (qq-state-store-message-session-index state) message-id)))
    (when (and indexed (not (equal indexed session-key)))
      (error "qq: message patch session %s contradicts indexed session %s"
             session-key indexed)))
  message-id)

(defun qq-state-apply-recall (state session-key message-id)
  "Recall MESSAGE-ID in SESSION-KEY, retaining a permanent tombstone."
  (let ((id (qq-state-validate-message-session state session-key message-id)))
    (if (and session-key id)
        (qq-state--apply-message-patch state session-key id '(:kind recall))
      (cons state nil))))

(defun qq-state-apply-row-recall (state session-key row-key)
  "Recall canonical ROW-KEY in SESSION-KEY after a checked acknowledgement."
  (if-let* ((message (qq-state--row
                     (qq-state--history state session-key)
                     (cons 'canonical-row-key row-key))))
      (qq-state--apply-message-patch
       state session-key (qq-state-message-anchor message) '(:kind recall))
    (cons state nil)))

(defun qq-state-apply-group-sequence-recall (state session-key sequence)
  "Recall the authored group row identified by native SEQUENCE."
  (unless (and (eq (qq-state-session-key-type session-key) 'group)
               (qq-protocol-message-sequence-p sequence))
    (error "qq: sequence recall requires a group and a canonical nonzero sequence"))
  (if-let* ((row (qq-state--row (qq-state--history state session-key)
                               (cons 'native (cons 'group sequence)))))
      (qq-state--apply-message-patch state session-key (qq-state-message-anchor row) '(:kind recall))
    (cons state nil)))

(defun qq-state--reaction-with-notice (reactions like is-add own-operation-p)
  "Return REACTIONS after applying one emoji LIKE notice.

IS-ADD identifies add versus remove.  OWN-OPERATION-P updates the local
`chosen-p' flag; reactions by other users leave that flag unchanged."
  (let* ((emoji-id (qq-state--normalize-id
                    (or (alist-get 'emoji_id like)
                        (alist-get 'emojiId like))))
         (existing (and emoji-id
                        (seq-find
                         (lambda (reaction)
                           (equal (alist-get 'emoji-id reaction) emoji-id))
                         reactions))))
    (if (null emoji-id)
        reactions
      (let* ((has-count (or (assq 'count like) (assq 'likes_cnt like)))
             (old-count (qq-state--normalize-reaction-count
                         (alist-get 'count existing)))
             (already-applied-p
              (and own-operation-p
                   existing
                   (eq (and (alist-get 'chosen-p existing) t)
                       (and is-add t))))
             (next-count
              (if has-count
                  (qq-state--normalize-reaction-count
                   (or (alist-get 'count like) (alist-get 'likes_cnt like)))
                (if already-applied-p
                    old-count
                  (max 0 (+ old-count (if is-add 1 -1))))))
             (emoji-type
              (qq-state--normalize-id
               (or (alist-get 'emoji_type like)
                   (alist-get 'emojiType like)
                   (alist-get 'emoji-type existing)
                   (qq-state--infer-reaction-emoji-type emoji-id))))
             (chosen-p (if own-operation-p
                           is-add
                         (and existing (alist-get 'chosen-p existing))))
             (next-item `((emoji-id . ,emoji-id)
                          (emoji-type . ,emoji-type)
                          (count . ,next-count)
                          (chosen-p . ,(and chosen-p t))))
             (next nil)
             (replaced nil))
        (dolist (reaction reactions)
          (if (equal (alist-get 'emoji-id reaction) emoji-id)
              (progn
                (setq replaced t)
                (when (> next-count 0)
                  (push next-item next)))
            (push reaction next)))
        (when (and (not replaced) (> next-count 0))
          (push next-item next))
        (nreverse next)))))

(defun qq-state--message-with-emoji-like-notice (state message notice)
  "Return a copy of MESSAGE after applying emoji-like NOTICE."
  (let* ((is-add (qq-protocol-json-true-p (alist-get 'is_add notice)))
         (operator-id (qq-state--normalize-id (alist-get 'user_id notice)))
         (own-operation-p
          (and operator-id
               (equal operator-id (qq-state-self-user-id state))))
         (reactions (copy-tree (qq-state-message-reactions message))))
    (dolist (like (or (alist-get 'likes notice) '()))
      (setq reactions
            (qq-state--reaction-with-notice
             reactions like is-add own-operation-p)))
    (let ((updated (copy-tree message)))
      (setf (alist-get 'reactions updated nil nil #'eq) reactions)
      updated)))

(defun qq-state-message-apply-patch (state message patch)
  "Return a copy of normalized MESSAGE after applying closed PATCH.

This pure boundary is shared by canonical storage and buffer-owned filtered
snapshots.  PATCH is a plist whose `:kind' is `recall' or `emoji-like'."
  (unless (listp message)
    (error "qq: message patch requires a normalized message"))
  (pcase (plist-get patch :kind)
    ('recall (qq-state--as-recalled-message message))
    ('emoji-like
     (let ((notice (plist-get patch :notice)))
       (unless (listp notice)
         (error "qq: emoji-like message patch requires a notice"))
       (qq-state--message-with-emoji-like-notice state message notice)))
    (kind (error "qq: unsupported message patch kind %S" kind))))

(defun qq-state-apply-emoji-like-notice
    (state session-key notice &optional target-anchor)
  "Apply an authoritative aggregate or one reaction delta to an exact row."
  (let* ((id (qq-protocol-optional-message-id (alist-get 'message_id notice) "reaction notice"))
         (anchor (or id target-anchor))
         (identity (qq-state-session-key-identity session-key)))
    (unless (and anchor
                 (or (null id) (null target-anchor) (equal id target-anchor))
                 (eq (alist-get 'type identity) 'group)
                 (equal (qq-state--normalize-id (alist-get 'group_id notice))
                        (alist-get 'target-id identity)))
      (error "qq: emoji-like notice requires its exact group and message"))
    (if id (qq-state-validate-message-session state session-key id)
      (unless (qq-state--session-message state session-key anchor)
        (error "qq: emoji-like notice target is not materialized")))
    (qq-state--apply-message-patch state session-key anchor (list :kind 'emoji-like :notice notice))))

(defun qq-state--closed-plist-p (value required optional)
  "Return non-nil when plist VALUE has unique REQUIRED and OPTIONAL keys."
  (and (proper-list-p value)
       (zerop (% (length value) 2))
       (let ((keys (cl-loop for (key _item) on value by #'cddr
                            collect key)))
         (and (cl-every #'keywordp keys)
              (= (length keys) (length (delete-dups (copy-sequence keys))))
              (cl-every (lambda (key) (memq key keys)) required)
              (cl-every (lambda (key) (memq key (append required optional)))
                        keys)))))

(defun qq-state--prepare-native-recent-conversation (entry)
  "Validate and isolate one normalized native recent conversation ENTRY."
  (unless (qq-state--closed-plist-p
           entry
           '(:session-key :message :activity-revision :pinned-known-p :pinned)
           nil)
    (error "qq: native recent conversation has invalid domain fields"))
  (let* ((session-key (plist-get entry :session-key))
         (identity (qq-state-session-key-identity session-key))
         (session-type (alist-get 'type identity))
         (message (copy-tree (plist-get entry :message)))
         (server-id (alist-get 'server-id message))
         (canonical-row-key (alist-get 'canonical-row-key message))
         (presentation-id (alist-get 'id message))
         (revision (plist-get entry :activity-revision))
         (pinned-known-p (plist-get entry :pinned-known-p))
         (pinned (plist-get entry :pinned)))
    (unless (memq session-type '(private group dataline))
      (error "qq: native recent conversation has unsupported session %S"
             session-key))
    (let ((message-sequence (alist-get 'message-seq message)))
      (unless (and (listp message)
                   (equal (alist-get 'session-key message) session-key)
                   (or (qq-protocol-message-id-p server-id)
                       (and (null server-id)
                            (qq-protocol-uint64-decimal-p canonical-row-key)
                            (stringp presentation-id)
                            (not (string-empty-p presentation-id))))
                   (integerp (alist-get 'time message))
                   (>= (alist-get 'time message) 0)
                   (if (eq session-type 'dataline)
                       (or (null message-sequence)
                           (qq-protocol-uint64-decimal-p message-sequence t))
                     (qq-protocol-uint64-decimal-p message-sequence t))
                   (stringp (alist-get 'gateway-account-id message))
                   (not (string-empty-p
                         (alist-get 'gateway-account-id message))))
        (error "qq: native recent conversation has malformed normalized message")))
    (unless (qq-protocol-uint64-decimal-p revision)
      (error "qq: native recent conversation revision is malformed"))
    (unless (memq pinned-known-p '(nil t))
      (error "qq: native recent conversation pin ownership is malformed"))
    (when (and pinned-known-p (not (memq pinned '(t :false))))
      (error "qq: native recent conversation pin state is malformed"))
    (when (and (eq session-type 'dataline) pinned-known-p)
      (error "qq: DataLine recent conversation cannot invent pin state"))
    (let ((peer-name (qq-state--present-string
                      (alist-get 'peer-name message))))
      (list
       :session-key session-key
       :message message
       :metadata
       `(,@(when peer-name `((title . ,peer-name)
                             (peer-name . ,peer-name)))
         ,@(when-let* ((peer-uid (alist-get 'peer-uid message)))
             `((peer-uid . ,peer-uid)))
         ,@(when-let* ((peer-uin (alist-get 'peer-uin message)))
             `((peer-uin . ,peer-uin)))
         (recent-activity-revision . ,revision)
         ,@(when pinned-known-p `((pinned . ,pinned))))))))

(defun qq-state-apply-recent-conversations
    (state entries &optional summary-observation-token)
  "Apply an authoritative page as one immutable account value."
  (unless (proper-list-p entries)
    (error "qq: native recent conversations must be a proper list"))
  (let* ((prepared (mapcar #'qq-state--prepare-native-recent-conversation entries))
         (keys (mapcar (lambda (entry) (plist-get entry :session-key)) prepared))
         (token (or summary-observation-token
                    (appkit-data-step state (qq-state-session-summary-observation-start state)))))
    (unless (= (length keys) (length (delete-dups (copy-sequence keys))))
      (error "qq: native recent conversations duplicate a session"))
    ;; Only sessions change during staging.  Upsert copies each touched row,
    ;; so a failed batch cannot mutate the canonical table or its values.
    (dolist (entry prepared)
      (let ((key (plist-get entry :session-key)))
        (appkit-data-step state (qq-state-upsert-session state key (plist-get entry :metadata)))
        (appkit-data-step state
          (qq-state--apply-session-summary
           state key (qq-state--message-summary-fields state (plist-get entry :message)) token))))
    (setq state (appkit-data-with state
                  (() :recent-session-keys keys
                   :recent-session-key-set
                   (appkit-data-map-edit appkit-data-map-empty
                                         (mapcar (lambda (key) (cons key t)) keys)))))
    (cons state (qq-state-sessions state))))

(defun qq-state--bump-recent-session (state session-key)
  "Record live activity while preserving the order of pinned conversations."
  (let* ((keys (qq-state-store-recent-session-keys state))
         (session (qq-state-session state session-key))
         (without (remove session-key keys))
         (index (or (cl-position-if
                     (lambda (key) (not (eq (alist-get 'pinned (qq-state-session state key)) t)))
                     without)
                    (length without)))
         (ordered (if (and (member session-key keys) (eq (alist-get 'pinned session) t)) keys
                    (append (seq-take without index) (list session-key) (seq-drop without index)))))
    (cons (appkit-data-with state
            (() :recent-session-keys ordered)
            ((:recent-session-key-set) session-key t))
          nil)))

(defun qq-state-recent-session-keys (state)
  "Return keys from the latest authoritative recent-contact snapshot."
  (copy-sequence (qq-state-store-recent-session-keys state)))

(defun qq-state-session-recent-p (state session-key)
  "Return non-nil when SESSION-KEY belongs to the latest recent snapshot."
  (and (appkit-data-map-get (qq-state-store-recent-session-key-set state) session-key) t))

(defun qq-state--refresh-session-titles (state)
  "Hydrate session titles once after a contact snapshot changes."
  (let (updates)
    (appkit-data-map-each
     (lambda (key session) (push (cons key (qq-state--hydrate-session state session)) updates))
     (qq-state-store-sessions state))
    (cons (appkit-data-with state
            (() :sessions (appkit-data-map-edit (qq-state-store-sessions state) updates)))
          nil)))

(defun qq-state-apply-friend-categories (state categories)
  "Replace the authoritative friend categories and their indexed projection."
  (let* ((friends (apply #'append (mapcar (lambda (category) (alist-get 'friends category)) categories)))
         (entries (mapcar (lambda (friend) (cons (alist-get 'user_id friend) friend)) friends)))
    (setq state (appkit-data-with state
                  (() :friend-categories categories :friend-categories-loaded-p t
                   :friend-order (mapcar #'car entries)
                   :friends-by-id (appkit-data-map-edit appkit-data-map-empty entries))))
    (appkit-data-step state (qq-state--refresh-session-titles state))
    (cons state categories)))

(defun qq-state-apply-groups (state groups)
  "Replace groups and enrich existing conversation metadata in the same value."
  (let (entries)
    (dolist (group groups)
      (let* ((id (qq-state--normalize-id (alist-get 'group_id group)))
             (key (qq-state-session-key 'group id))
             (mode (assq 'message-notify-mode group))
             (pin (assq 'pinned group)))
        (when (and mode (not (memq (cdr mode) '(notify assistant shield receive))))
          (error "qq: group directory has an invalid notification mode"))
        (when (and pin (not (memq (cdr pin) '(t :false))))
          (error "qq: group directory has an invalid pin state"))
        (push (cons id group) entries)
        (when (qq-state-session state key)
          (appkit-data-step state
            (qq-state-upsert-session state key
                                     (append (when mode (list mode (cons 'muted-p (and (not (eq (cdr mode) 'notify)) t))))
                                             (when pin (list pin))))))))
    (setq entries (nreverse entries)
          state (appkit-data-with state
                  (() :groups-loaded-p t :group-order (mapcar #'car entries)
                   :groups-by-id (appkit-data-map-edit appkit-data-map-empty entries))))
    (appkit-data-step state (qq-state--refresh-session-titles state))
    (cons state groups)))

(defun qq-state-friend (state user-id)
  "Return cached friend object for USER-ID."
  (copy-tree (appkit-data-map-get (qq-state-store-friends-by-id state) (qq-state--normalize-id user-id))))

(defun qq-state-friends (state)
  "Return all cached friend objects."
  (mapcar (lambda (user-id)
            (copy-tree (appkit-data-map-get (qq-state-store-friends-by-id state) user-id)))
          (qq-state-store-friend-order state)))

(defun qq-state-friend-categories (state)
  "Return ordered authoritative friend categories."
  (copy-tree (qq-state-store-friend-categories state)))

(defun qq-state-friend-categories-loaded-p (state)
  "Return non-nil after the native friend-category snapshot has loaded."
  (qq-state-store-friend-categories-loaded-p state))

(defun qq-state-friend-count (state)
  "Return the number of friends in the authoritative snapshot."
  (length (qq-state-store-friend-order state)))

(defun qq-state-group (state group-id)
  "Return cached group object for GROUP-ID."
  (copy-tree (appkit-data-map-get (qq-state-store-groups-by-id state) (qq-state--normalize-id group-id))))

(defun qq-state-groups (state)
  "Return all cached group objects."
  (mapcar (lambda (group-id)
            (copy-tree (appkit-data-map-get (qq-state-store-groups-by-id state) group-id)))
          (qq-state-store-group-order state)))

(defun qq-state-groups-loaded-p (state)
  "Return non-nil after the native joined-group snapshot has loaded."
  (qq-state-store-groups-loaded-p state))

(defun qq-state-group-count (state)
  "Return the number of joined groups in the authoritative snapshot."
  (length (qq-state-store-group-order state)))

(defun qq-state--action-live-p (action &optional now)
  "Return whether ephemeral ACTION is still live at NOW."
  (appkit-chat-action-live-p action now))

(defun qq-state-session-actions (state session-key)
  "Return live actions for SESSION-KEY without changing STATE."
  (seq-filter (lambda (entry) (qq-state--action-live-p (cdr entry)))
              (appkit-data-map-get (qq-state-store-actions state) session-key)))

(defun qq-state-action-text (state session-key)
  "Return the first live action's presentation text for SESSION-KEY."
  (when-let* ((actions (qq-state-session-actions state session-key))
              (text (plist-get (cdar actions) :text)))
    (qq-state--present-string text)))

(defun qq-state-clear-session-actions (state session-key)
  "Remove SESSION-KEY's action value from STATE."
  (cons (appkit-data-with state ((:actions) session-key appkit-data-delete))
        (and (appkit-data-map-get (qq-state-store-actions state) session-key) t)))

(defun qq-state-clear-sender-action (state session-key sender-id)
  "Remove SENDER-ID's action from STATE."
  (let* ((actions (appkit-data-map-get (qq-state-store-actions state) session-key))
         (id (qq-state--normalize-id sender-id))
         (next (seq-remove (lambda (entry) (equal (car entry) id)) actions)))
    (cons (appkit-data-with state ((:actions) session-key (or next appkit-data-delete)))
          (and (assoc id actions) t))))

(defun qq-state-message-apply-tombstones (state session-key message)
  "Apply permanent SESSION-KEY tombstones to normalized MESSAGE.

This public projection boundary applies permanent message patches to an
otherwise buffer-owned snapshot.  It does not store MESSAGE or replay
request-scoped reaction observations; recall is the only permanent patch."
  (qq-state-session-key-identity session-key)
  (unless (listp message)
    (error "qq: message tombstones require a normalized message"))
  (qq-state--materialize-message-patches state session-key message))

(defun qq-state--history (state session-key)
  "Return SESSION-KEY's immutable message collection in STATE."
  (or (appkit-data-map-get (qq-state-store-messages-by-session state) session-key)
      (qq-state-history-create)))

(defun qq-state--row (history key)
  "Resolve exact index KEY in HISTORY, returning its borrowed entity."
  (appkit-data-map-get
   (qq-state-history-rows history)
   (appkit-data-map-get (qq-state-history-aliases history) key)))

(defun qq-state--message-aliases (message)
  "Return every exact identity owned by MESSAGE."
  (append
   (delq nil
         (mapcar (lambda (field)
                   (when-let* ((value (alist-get field message)))
                     (cons field value)))
                 '(id server-id local-id canonical-row-key)))
   (when-let* ((key (qq-state--native-message-correlation-key message)))
     (list (cons 'native key)))))

(defun qq-state--put-message (state session-key message &optional previous)
  "Replace PREVIOUS with MESSAGE in STATE; nil MESSAGE removes PREVIOUS."
  (if (equal previous message) (cons state previous)
    (let* ((history (qq-state--history state session-key))
           (rows (qq-state-history-rows history))
           (aliases (qq-state-history-aliases history))
           (sequences (qq-state-history-sequences history))
           (old-anchor (and previous (qq-state-message-anchor previous)))
           (anchor (and message (qq-state-message-anchor message)))
           (rekey (not (equal old-anchor anchor)))
           (old-aliases (qq-state--message-aliases previous))
           (new-aliases (qq-state--message-aliases message))
           (aliases-changed (or rekey (not (equal old-aliases new-aliases))))
           (sequence-fields '(message-seq native-client-sequence))
           (sequences-changed
            (or rekey (seq-some (lambda (field)
                                 (not (equal (alist-get field previous) (alist-get field message))))
                               sequence-fields)))
           (oldest (qq-state-history-oldest history))
           (latest (qq-state-history-latest history))
           (pending (qq-state-history-pending history))
           (outgoing (qq-state-history-outgoing history))
           (pokes (remove old-anchor (qq-state-history-pokes history))))
      (when (and previous rekey)
        (setq rows (appkit-data-map-remove rows old-anchor)))
      (when aliases-changed
        (dolist (key old-aliases)
          (when (equal (appkit-data-map-get aliases key) old-anchor)
            (setq aliases (appkit-data-map-remove aliases key))))
        (setq aliases (appkit-data-map-edit aliases (mapcar (lambda (key) (cons key anchor)) new-aliases)))
        (when previous (appkit-data-step state (qq-state--unindex-message state previous)))
        (when message (appkit-data-step state (qq-state--index-message state message))))
      (when sequences-changed
        (dolist (field sequence-fields)
          (when-let* ((value (alist-get field previous)))
            (let* ((key (cons field value))
                   (remaining (remove old-anchor (appkit-data-map-get sequences key))))
              (setq sequences (if remaining (appkit-data-map-set sequences key remaining)
                                (appkit-data-map-remove sequences key)))))
          (when-let* ((value (alist-get field message)))
            (let ((key (cons field value)))
              (setq sequences (appkit-data-map-set
                               sequences key (cons anchor (remove anchor (appkit-data-map-get sequences key)))))))))
      (setq pending
            (if (and message (eq (alist-get 'status message) 'pending)
                     (alist-get 'local-id message) (null (alist-get 'server-id message)))
                (if (member old-anchor pending)
                    (if rekey (mapcar (lambda (key) (if (equal key old-anchor) anchor key)) pending) pending)
                  (append pending (list anchor)))
              (remove old-anchor pending))
            outgoing
            (cond
             ((null message) (remove old-anchor outgoing))
             ((member old-anchor outgoing)
              (if rekey (mapcar (lambda (key) (if (equal key old-anchor) anchor key)) outgoing) outgoing))
             ((and (alist-get 'local-id message) (not (alist-get 'local-id previous)))
              (append outgoing (list anchor)))
             (t outgoing)))
      (when message
        (setq rows (appkit-data-map-set rows anchor message))
        (when (and (qq-state-poke-message-p message)
                   (not (alist-get 'poke-echo-reconciled-p message)))
          (push anchor pokes)))
      ;; Only moving or removing an extremum scans the conversation.
      ;; Content updates reuse every identity index and the cached bounds.
      (if (and previous (equal old-anchor oldest)
               (or (null message) (not (alist-get 'server-id message))
                   (qq-state--message-sort< previous message)))
          (setq oldest
                (qq-state-message-anchor
                 (seq-find (lambda (row) (alist-get 'server-id row))
                           (sort (appkit-data-map-values rows) #'qq-state--message-sort<))))
        (when (and message (alist-get 'server-id message)
                   (or (null oldest) (equal oldest old-anchor)
                       (qq-state--message-sort< message (appkit-data-map-get rows oldest))))
          (setq oldest anchor)))
      (if (and previous (equal old-anchor latest)
               (or (null message) (qq-state--message-sort< message previous)
                   (and (or aliases-changed
                            (not (equal (alist-get 'root-message-seq previous)
                                        (alist-get 'root-message-seq message))))
                        (eq (qq-state--latest-summary-message state (list message previous)) previous))))
          (setq latest (qq-state-message-anchor
                        (qq-state--latest-summary-message state (appkit-data-map-values rows))))
        (when (and message (not (equal latest anchor)))
          (setq latest
                (qq-state-message-anchor
                 (qq-state--latest-summary-message
                  state (delq nil (list message (appkit-data-map-get rows latest))))))))
      (let ((cutoff (- (qq-state--normalize-time
                       (alist-get 'time (appkit-data-map-get rows latest)))
                      qq-self-message-dedupe-window)))
        (setq pokes (seq-filter
                     (lambda (key) (>= (qq-state--normalize-time
                                        (alist-get 'time (appkit-data-map-get rows key))) cutoff))
                     pokes)))
      (cons (appkit-data-with state
              ((:messages-by-session) session-key
               (appkit-data-with history
                 (() :rows rows :aliases aliases :sequences sequences
                  :pending pending :outgoing outgoing :pokes pokes :oldest oldest :latest latest))))
            message))))

(defun qq-state--apply-message-patch (state session-key anchor patch)
  "Journal PATCH and update its exact cached row, when present."
  (let* ((previous (qq-state--session-message state session-key anchor))
         (updated (when previous
                    (qq-state-message-apply-patch
                     state (qq-state--materialize-message-patches state session-key previous) patch))))
    (appkit-data-step state (qq-state--journal-message-patch state session-key anchor patch))
    (when (and updated (not (equal updated previous)))
      (appkit-data-step state (qq-state--put-message state session-key updated previous))
      (appkit-data-step state (qq-state--sync-session-summary state session-key)))
    (cons state updated)))

(defun qq-state-changes (before after source)
  "Describe changed entities between two account snapshots.
Shared branches are skipped.  Every event borrows the committed AFTER value."
  (let ((owner (qq-state-store-account-id after)) events)
    (cl-labels
        ((emit (type &rest fields)
           (push (append (list :type type :account-id owner :state after :source source) fields) events))
         (present (value) (unless (eq value appkit-data-absent) value)))
      (dolist (group '((connection :connection-status)
                       (self-info :self-info)
                       (friends-refreshed :friends-by-id :friend-order :friend-categories :friend-categories-loaded-p)
                       (groups-refreshed :groups-by-id :group-order :groups-loaded-p)
                       (sessions-refreshed :recent-session-key-set)
                       (recent-order :recent-session-keys)))
        (when (seq-some (lambda (field)
                         (not (eq (appkit-data-get before field) (appkit-data-get after field))))
                       (cdr group))
          (emit (car group))))
      (dolist (change (appkit-data-map-diff (qq-state-store-sessions before)
                                            (qq-state-store-sessions after)))
        (let ((old (present (nth 1 change))) (new (present (nth 2 change))))
          (emit 'session :session-key (car change) :session new
                :mutation (if (seq-some (lambda (key) (not (equal (alist-get key old) (alist-get key new))))
                                        qq-state--session-read-projection-keys)
                              'read 'session))))
      (dolist (change (appkit-data-map-diff (qq-state-store-messages-by-session before)
                                            (qq-state-store-messages-by-session after)))
        (let* ((session (car change))
               (old (or (present (nth 1 change)) (qq-state-history-create)))
               (new (or (present (nth 2 change)) (qq-state-history-create))))
          (dolist (row (appkit-data-map-diff (qq-state-history-rows old) (qq-state-history-rows new)))
            (let* ((anchor (car row)) (previous (present (nth 1 row))) (message (present (nth 2 row)))
                   (local (and message (alist-get 'local-id message)))
                   (promoted (and local (qq-state--row old (cons 'local-id local))))
                   (replacement (and previous
                                     (qq-state--row new (cons 'local-id (alist-get 'local-id previous))))))
              (unless (and (null message) replacement)
                (emit 'message :session-key session :message (or message previous)
                      :message-anchor anchor
                      :previous-anchor (and promoted (not (equal anchor (qq-state-message-anchor promoted)))
                                            (qq-state-message-anchor promoted))
                      :mutation (cond ((null message) 'delete)
                                      ((or previous promoted) 'update) (t 'create))))))))
      (dolist (change (appkit-data-map-diff (qq-state-store-message-patch-journal before)
                                            (qq-state-store-message-patch-journal after)))
        (let* ((key (car change)) (entry (present (nth 2 change))))
          (when (and (plist-get entry :recalled-p)
                     (not (qq-state--session-message after (car key) (cdr key))))
            (emit 'message :session-key (car key) :message-anchor (cdr key)
                  :mutation 'update :message-patch '(:kind recall)))))
      (dolist (change (appkit-data-map-diff (qq-state-store-actions before) (qq-state-store-actions after)))
        (emit 'action :session-key (car change) :actions (present (nth 2 change)) :mutation 'update)))
    (nreverse events)))

(defun qq-state-session-message (state session-key &rest identities)
  "Return an owned copy of the first exact message identity in STATE."
  (copy-tree (apply #'qq-state--session-message state session-key identities)))

(defun qq-state-message-candidates (state session-key field value)
  "Return owned rows matching FIELD and VALUE in SESSION-KEY.
Sequence indexes retain collisions; callers must check sender and time."
  (when value
    (let* ((history (qq-state--history state session-key))
           (key (cons field value))
           (anchors (if (memq field '(message-seq native-client-sequence))
                        (appkit-data-map-get (qq-state-history-sequences history) key)
                      (when-let* ((anchor (appkit-data-map-get (qq-state-history-aliases history) key)))
                        (list anchor)))))
      (mapcar (lambda (anchor) (copy-tree (appkit-data-map-get (qq-state-history-rows history) anchor)))
              anchors))))

(defun qq-state-session-outgoing-messages (state session-key)
  "Return local sends not yet represented by a Gateway window."
  (let ((history (qq-state--history state session-key)))
    (mapcar (lambda (anchor) (copy-tree (appkit-data-map-get (qq-state-history-rows history) anchor)))
            (qq-state-history-outgoing history))))

(defun qq-state-acknowledge-window (state session-key messages)
  "Stop projecting confirmed MESSAGES separately from their Gateway window."
  (let* ((history (qq-state--history state session-key))
         (outgoing (qq-state-history-outgoing history)))
    (dolist (message messages)
      (unless (memq (alist-get 'status message) '(pending failed))
        (setq outgoing (remove (qq-state-message-anchor message) outgoing))))
    (cons (appkit-data-with state
            ((:messages-by-session) session-key
             (appkit-data-with history (() :outgoing outgoing))))
          messages)))

(provide 'qq-state)
;;; qq-state.el ends here
