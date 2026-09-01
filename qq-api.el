;;; qq-api.el --- Native forward wire contracts -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; Closed validators and adapters for native merged-forward payloads.  This
;; module owns no transport and never accepts removed OneBot action names.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'qq-customize)
(require 'qq-protocol)

(require 'qq-state)

(defun qq-api-resource-id-p (value)
  "Return non-nil when VALUE is an opaque native resource_id string."
  (qq-protocol-non-empty-string-p value))


(defconst qq-api--uint32-max #xffffffff
  "Largest exact unsigned 32-bit integer accepted by native directory data.")

(defun qq-api--uint32-p (value)
  "Return non-nil when VALUE is an exact unsigned 32-bit integer."
  (and (integerp value)
       (<= 0 value qq-api--uint32-max)))

(defun qq-api-entry-id-p (value)
  "Return non-nil when VALUE is a canonical native snapshot entry id."
  (and (stringp value)
       (string-match-p "\\`[0-9]+\\(?:\\.[0-9]+\\)*\\'" value)))

(defun qq-api--validate-identity (predicate value field context protocol-p)
  "Validate VALUE with PREDICATE for FIELD used in CONTEXT.

Signal `error' for malformed protocol data when PROTOCOL-P is non-nil;
otherwise signal `user-error' for malformed caller input.  Return VALUE
unchanged so callers cannot accidentally canonicalize a lossy numeric id."
  (unless (funcall predicate value)
    (signal (if protocol-p 'error 'user-error)
            (list (format "qq: %s requires %s as an original string, got %S"
                          (or context "protocol input") field value))))
  value)

(defun qq-api-validate-message-id (value &optional context protocol-p)
  "Return canonical message-id VALUE or signal an identity error.

CONTEXT is included in the diagnostic.  PROTOCOL-P selects a protocol
`error'; otherwise invalid user/API input signals `user-error'."
  (qq-api--validate-identity #'qq-protocol-message-id-p value
                             "canonical nonzero decimal message_id"
                             context protocol-p))

(defun qq-api-validate-resource-id (value &optional context protocol-p)
  "Return canonical resource_id VALUE or signal an identity error.

CONTEXT is included in the diagnostic.  PROTOCOL-P selects a protocol
`error'; otherwise invalid user/API input signals `user-error'."
  (qq-api--validate-identity #'qq-api-resource-id-p value
                             "non-empty resource_id"
                             context protocol-p))

(defun qq-api--single-alist-p (value)
  "Return non-nil when VALUE is one symbol-keyed alist object."
  (and (consp value)
       (proper-list-p value)
       (cl-every (lambda (cell)
                   (and (consp cell) (symbolp (car cell))))
                 value)))

(defun qq-api--finite-number-p (value)
  "Return non-nil when VALUE is a finite JSON number."
  (and (numberp value)
       (or (integerp value)
           (and (floatp value)
                (not (isnan value))
                (not (string-match-p
                      "INF" (upcase (format "%S" value))))))))

(defun qq-api--validate-string-fields
    (object fields context protocol-p)
  "Require every present member of FIELDS in OBJECT to be a string."
  (dolist (key fields)
    (when (and (assq key object)
               (not (stringp (alist-get key object))))
      (qq-api--signal-schema-error
       protocol-p "qq: %s.%s must be a string" context key))))

(defun qq-api--validate-native-file-payload
    (payload context protocol-p &optional image-p)
  "Validate common native file PAYLOAD, with image fields when IMAGE-P."
  (let ((optional
         (append '(file_id path url name thumb file_size)
                 (when image-p '(summary sub_type)))))
    (unless (qq-api--exact-object-keys-p payload '(file) optional)
      (qq-api--signal-schema-error
       protocol-p "qq: %s file payload has invalid fields" context))
    (unless (stringp (alist-get 'file payload))
      (qq-api--signal-schema-error
       protocol-p "qq: %s.file must be a string" context))
    (qq-api--validate-string-fields
     payload '(file_id path url name thumb) context protocol-p)
    (when (assq 'file_size payload)
      (let ((size (alist-get 'file_size payload)))
        (unless (or (stringp size) (qq-api--finite-number-p size))
          (qq-api--signal-schema-error
           protocol-p "qq: %s.file_size must be a string or finite number"
           context))))
    (when image-p
      (qq-api--validate-string-fields payload '(summary) context protocol-p)
      (when (and (assq 'sub_type payload)
                 (not (qq-api--finite-number-p
                       (alist-get 'sub_type payload))))
        (qq-api--signal-schema-error
         protocol-p "qq: %s.sub_type must be a finite number" context)))))

(defun qq-api--exact-object-keys-p (object required &optional optional)
  "Return non-nil when OBJECT has exactly REQUIRED and OPTIONAL keys."
  ;; `json-parse-buffer' represents an empty JSON object as nil when objects
  ;; are requested as alists.  Accept that representation only when the
  ;; schema has no required fields (for example an empty presentation).
  (when (or (qq-api--single-alist-p object)
            (and (null object) (null required)))
    (let ((keys (mapcar #'car object))
          (allowed (append required optional)))
      (and (= (length keys)
              (length (delete-dups (copy-sequence keys))))
           (cl-every (lambda (key) (assq key object)) required)
           (null (seq-difference keys allowed))))))

(defun qq-api--json-safe-value-p (value &optional seen)
  "Return non-nil when VALUE is a finite JSON-safe decoded value.

SEEN guards against cyclic Lisp containers, which cannot originate in JSON
and must not be retained as an `unsupported.raw' diagnostic payload."
  (cond
   ((or (null value) (eq value t) (eq value :false)
        (stringp value) (qq-api--finite-number-p value))
    t)
   ((or (vectorp value) (and (consp value) (proper-list-p value)))
    (unless (memq value seen)
      (let ((seen (cons value seen)))
        (if (vectorp value)
            (cl-every (lambda (item)
                        (qq-api--json-safe-value-p item seen))
                      (append value nil))
          (if (and (consp (car value))
                   (cl-every (lambda (cell)
                               (and (consp cell)
                                    (or (symbolp (car cell))
                                        (stringp (car cell)))))
                             value))
              (cl-every (lambda (cell)
                          (qq-api--json-safe-value-p (cdr cell) seen))
                        value)
            (cl-every (lambda (item)
                        (qq-api--json-safe-value-p item seen))
                      value))))))
   (t nil)))

(defun qq-api--signal-schema-error (protocol-p format-string &rest args)
  "Signal a schema error described by FORMAT-STRING and ARGS."
  (signal (if protocol-p 'error 'user-error)
          (list (apply #'format format-string args))))

(defun qq-api-validate-chat-locator (chat &optional context protocol-p)
  "Validate and return a copied native CHAT locator."
  (let ((context (or context "chat locator")))
    (unless (qq-api--single-alist-p chat)
      (qq-api--signal-schema-error
       protocol-p "qq: %s must be an object" context))
    (pcase (alist-get 'kind chat)
      ("group"
       (unless (qq-api--exact-object-keys-p chat '(kind group_id))
         (qq-api--signal-schema-error
          protocol-p "qq: %s group locator has invalid fields" context))
       (unless (qq-protocol-group-uin-p (alist-get 'group_id chat))
         (qq-api--signal-schema-error
          protocol-p "qq: %s group_id must be a canonical uint32 group UIN"
          context)))
      ("private"
       (unless (qq-api--exact-object-keys-p chat '(kind user_id))
         (qq-api--signal-schema-error
          protocol-p "qq: %s private locator has invalid fields" context))
       (unless (qq-protocol-uint64-decimal-p (alist-get 'user_id chat))
         (qq-api--signal-schema-error
          protocol-p
          "qq: %s user_id must be a canonical nonzero decimal string"
          context)))
      (_
       (qq-api--signal-schema-error
        protocol-p "qq: %s has invalid kind %S"
        context (alist-get 'kind chat))))
    (copy-tree chat)))

(defun qq-api-validate-forward-session-locator
    (locator &optional context protocol-p)
  "Validate a session LOCATOR usable for forward lookup and identity."
  (qq-protocol-validate-emacs-session-locator
   locator
   (or context "forward session locator")
   (if protocol-p 'error 'user-error)))

(defun qq-api-validate-send-forward-source-locator
    (kind locator &optional context protocol-p)
  "Validate LOCATOR as a send-forward source for exact request KIND.

DataLine remains part of the general session locator union.  The current
`emacs_send_forward' adapter exposes only `individual' from the desktop
variant.  Mobile still needs operation-specific normalization to the type-8
DataLine path and sender-local result persistence; this validator must not be
read as evidence for a second or unsupported 134 wire route."
  (unless (member kind '("individual" "merged"))
    (qq-api--signal-schema-error
     protocol-p "qq: %s has invalid forwarding kind %S"
     (or context "send-forward source") kind))
  (let ((validated
         (qq-api-validate-forward-session-locator
          locator (or context "send-forward source") protocol-p)))
    (when (and (equal (alist-get 'kind validated) "dataline")
               (not (and (equal kind "individual")
                         (equal (alist-get 'variant validated) "desktop"))))
      (qq-api--signal-schema-error
       protocol-p "qq: %s does not support %s from DataLine %s"
       (or context "send-forward source") kind
       (alist-get 'variant validated)))
    validated))


(defun qq-api-validate-forward-source (source &optional context protocol-p)
  "Validate and return a copied native query SOURCE union."
  (let ((context (or context "forward source")))
    (unless (qq-api--single-alist-p source)
      (qq-api--signal-schema-error
       protocol-p "qq: %s must be a source object" context))
    (pcase (alist-get 'kind source)
      ("message"
       (unless (qq-api--exact-object-keys-p
                source '(kind message_id chat))
         (qq-api--signal-schema-error
          protocol-p
          "qq: %s message source requires kind, message_id, and chat"
          context))
       (qq-api-validate-message-id
        (alist-get 'message_id source) context protocol-p)
       (qq-api-validate-forward-session-locator
        (alist-get 'chat source) (format "%s chat" context) protocol-p))
      ("resource"
       (unless (qq-api--exact-object-keys-p
                source '(kind resource_id))
         (qq-api--signal-schema-error
          protocol-p
          "qq: %s resource source requires only kind and resource_id"
          context))
       (qq-api-validate-resource-id
        (alist-get 'resource_id source) context protocol-p))
      ("context"
       (unless (qq-api--exact-object-keys-p
                source '(kind peer root_message_id parent_message_id))
         (qq-api--signal-schema-error
          protocol-p
          (concat "qq: %s context source requires only kind, peer, "
                  "root_message_id, and parent_message_id")
          context))
       (let ((peer (alist-get 'peer source)))
         (unless (qq-api--exact-object-keys-p
                  peer '(chat_type peer_uid guild_id))
           (qq-api--signal-schema-error
            protocol-p
            (concat "qq: %s context peer requires only chat_type, "
                    "peer_uid, and guild_id")
            context))
         (let ((chat-type (alist-get 'chat_type peer)))
           (unless (and (qq-api--finite-number-p chat-type)
                        (> chat-type 0)
                        (= chat-type (truncate chat-type)))
             (qq-api--signal-schema-error
              protocol-p
              "qq: %s context peer.chat_type must be a positive integer"
              context)))
         (unless (qq-protocol-non-empty-string-p (alist-get 'peer_uid peer))
           (qq-api--signal-schema-error
            protocol-p
            "qq: %s context peer.peer_uid must be a non-empty string"
            context))
         (unless (stringp (alist-get 'guild_id peer))
           (qq-api--signal-schema-error
            protocol-p "qq: %s context peer.guild_id must be a string"
            context)))
       (dolist (key '(root_message_id parent_message_id))
         (let ((message-id (alist-get key source)))
           (unless (and (stringp message-id)
                        (string-match-p "\\`[1-9][0-9]*\\'" message-id))
             (qq-api--signal-schema-error
              protocol-p
              (concat "qq: %s context %s must be a positive decimal string "
                      "without leading zeros")
              context key)))))
      (_
       (qq-api--signal-schema-error
        protocol-p "qq: %s has invalid kind %S"
        context (alist-get 'kind source))))
    (let ((copy (copy-tree source)))
      (when (equal (alist-get 'kind copy) "context")
        (setf (alist-get 'chat_type (alist-get 'peer copy))
              (truncate (alist-get 'chat_type (alist-get 'peer copy)))))
      copy)))



(defun qq-api--validate-native-peer (peer context protocol-p)
  "Validate and copy exact Linux QQ PEER for CONTEXT."
  (unless (qq-api--exact-object-keys-p
           peer '(chat_type peer_uid guild_id))
    (qq-api--signal-schema-error
     protocol-p
     (concat "qq: %s peer requires only chat_type, peer_uid, and "
             "guild_id")
     context))
  (let ((chat-type (alist-get 'chat_type peer)))
    (unless (and (integerp chat-type) (> chat-type 0))
      (qq-api--signal-schema-error
       protocol-p "qq: %s peer.chat_type must be a positive integer"
       context)))
  (unless (qq-protocol-non-empty-string-p (alist-get 'peer_uid peer))
    (qq-api--signal-schema-error
     protocol-p "qq: %s peer.peer_uid must be a non-empty string"
     context))
  (unless (stringp (alist-get 'guild_id peer))
    (qq-api--signal-schema-error
     protocol-p "qq: %s peer.guild_id must be a string" context))
  (copy-tree peer))

(defun qq-api-validate-video-resolver
    (resolver &optional context protocol-p)
  "Validate and copy an exact native video RESOLVER.

The two locator domains are deliberately disjoint: live messages use their
own native peer/message/element identity, while forward snapshots use their
own peer/file UUID.  No parent id or generic file token is accepted."
  (let ((context (or context "video resolver")))
    (unless (qq-api--single-alist-p resolver)
      (qq-api--signal-schema-error
       protocol-p "qq: %s must be an object" context))
    (pcase (alist-get 'kind resolver)
      ("message"
       (unless (qq-api--exact-object-keys-p
                resolver '(kind peer message_id element_id))
         (qq-api--signal-schema-error
          protocol-p
          (concat "qq: %s message resolver requires only kind, peer, "
                  "message_id, and element_id")
          context))
       (qq-api--validate-native-peer
        (alist-get 'peer resolver) context protocol-p)
       (dolist (key '(message_id element_id))
         (let ((value (alist-get key resolver)))
           (unless (and (stringp value)
                        (string-match-p "\\`[1-9][0-9]*\\'" value))
             (qq-api--signal-schema-error
              protocol-p
              "qq: %s.%s must be a positive decimal identity string"
              context key)))))
      ("snapshot"
       (unless (qq-api--exact-object-keys-p
                resolver '(kind peer file_uuid))
         (qq-api--signal-schema-error
          protocol-p
          (concat "qq: %s snapshot resolver requires only kind, peer, "
                  "and file_uuid")
          context))
       (qq-api--validate-native-peer
        (alist-get 'peer resolver) context protocol-p)
       (unless (qq-protocol-non-empty-string-p (alist-get 'file_uuid resolver))
         (qq-api--signal-schema-error
          protocol-p "qq: %s.file_uuid must be a non-empty string"
          context)))
      (_
       (qq-api--signal-schema-error
        protocol-p "qq: %s has invalid kind %S"
        context (alist-get 'kind resolver))))
    (copy-tree resolver)))

(defun qq-api--validate-native-video-remote
    (remote context protocol-p &optional allow-resolvable-p)
  "Validate native video REMOTE discriminant for CONTEXT.

ALLOW-RESOLVABLE-P permits the wire capability carried by forward video
segments.  A resolve action result itself must be terminal or available."
  (unless (qq-api--single-alist-p remote)
    (qq-api--signal-schema-error
     protocol-p "qq: %s video remote must be an object" context))
  (pcase (alist-get 'state remote)
    ("available"
     (unless (and (qq-api--exact-object-keys-p remote '(state url))
                  (qq-protocol-non-empty-string-p (alist-get 'url remote)))
       (qq-api--signal-schema-error
        protocol-p
        "qq: %s available video remote requires only non-empty string url"
        context)))
    ("resolvable"
     (unless allow-resolvable-p
       (qq-api--signal-schema-error
        protocol-p "qq: %s resolve result cannot remain resolvable" context))
     (unless (qq-api--exact-object-keys-p remote '(state resolver))
       (qq-api--signal-schema-error
        protocol-p
        "qq: %s resolvable video remote requires only state and resolver"
        context))
     (qq-api-validate-video-resolver
      (alist-get 'resolver remote) (format "%s resolver" context) protocol-p))
    ((or "expired" "unavailable" "unresolved")
     (unless (qq-api--exact-object-keys-p remote '(state))
       (qq-api--signal-schema-error
        protocol-p
        "qq: %s non-available video remote may contain only state"
        context)))
    (_
     (qq-api--signal-schema-error
      protocol-p "qq: %s video remote has invalid state %S"
      context (alist-get 'state remote))))
  remote)

(defun qq-api--validate-native-forward-segment (segment context protocol-p)
  "Validate one native forward SEGMENT."
  (unless (qq-api--exact-object-keys-p segment '(kind payload))
    (qq-api--signal-schema-error
     protocol-p "qq: %s must contain only kind and payload" context))
  (let ((kind (alist-get 'kind segment))
        (payload (alist-get 'payload segment)))
    (unless (and (stringp kind) (not (string-empty-p kind)))
      (qq-api--signal-schema-error
       protocol-p "qq: %s kind must be a non-empty string" context))
    (unless (member
             kind
             '("text" "at" "reply" "image" "file" "record" "face"
               "mface" "mail" "wallet" "music" "poke" "dice" "rps" "contact"
               "location" "json" "card" "xml" "markdown" "miniapp"
               "onlinefile" "flashtransfer" "video" "forward"
               "forward-card" "gray-tip" "unsupported"))
      (qq-api--signal-schema-error
       protocol-p "qq: %s has unsupported native kind %S" context kind))
    (pcase kind
      ("text"
       (unless (and (qq-api--exact-object-keys-p payload '(text))
                    (stringp (alist-get 'text payload)))
         (qq-api--signal-schema-error
          protocol-p "qq: %s text payload is invalid" context)))
      ("at"
       (unless (qq-api--exact-object-keys-p payload '(qq) '(name))
         (qq-api--signal-schema-error
          protocol-p "qq: %s at payload has invalid fields" context))
       (unless (stringp (alist-get 'qq payload))
         (qq-api--signal-schema-error
          protocol-p "qq: %s at.qq must be a string" context))
       (qq-api--validate-string-fields payload '(name) context protocol-p))
      ("image"
       (qq-api--validate-native-file-payload
        payload context protocol-p t))
      ((or "file" "record")
       (qq-api--validate-native-file-payload
        payload context protocol-p nil))
      ("face"
       (unless (qq-api--exact-object-keys-p
                payload '(id)
                '(description face_type sticker_id sticker_pack_id
                  sticker_type resultId chainCount))
         (qq-api--signal-schema-error
          protocol-p "qq: %s face payload has invalid fields" context))
       (unless (stringp (alist-get 'id payload))
         (qq-api--signal-schema-error
          protocol-p "qq: %s face.id must be a string" context))
       (qq-api--validate-string-fields
        payload '(description sticker_id sticker_pack_id resultId)
        context protocol-p)
       (dolist (key '(face_type sticker_type))
         (when (and (assq key payload)
                    (not (qq-api--finite-number-p (alist-get key payload))))
           (qq-api--signal-schema-error
            protocol-p "qq: %s face.%s must be a finite number"
            context key)))
       (when (and (assq 'chainCount payload)
                  (not (qq-api--finite-number-p
                        (alist-get 'chainCount payload))))
         (qq-api--signal-schema-error
          protocol-p "qq: %s face.chainCount must be a finite number"
          context)))
      ("mface"
       (unless (qq-api--exact-object-keys-p
                payload '(emoji_package_id emoji_id key summary))
         (qq-api--signal-schema-error
          protocol-p "qq: %s mface payload has invalid fields" context))
       (unless (qq-api--finite-number-p
                (alist-get 'emoji_package_id payload))
         (qq-api--signal-schema-error
          protocol-p "qq: %s mface.emoji_package_id must be a finite number"
          context))
       (qq-api--validate-string-fields
        payload '(emoji_id key summary) context protocol-p))
      ("mail"
       (unless (qq-api--exact-object-keys-p
                payload nil '(sender subject content prompt detail url))
         (qq-api--signal-schema-error
          protocol-p "qq: %s mail payload has invalid fields" context))
       (qq-api--validate-string-fields
        payload '(sender subject content prompt detail url)
        context protocol-p))
      ("wallet"
       (unless (qq-api--exact-object-keys-p
                payload
                '(wallet_kind message_type session_type red_type red_channel grab_state
                  grabbed_amount sender receiver))
         (qq-api--signal-schema-error
          protocol-p "qq: %s wallet payload has invalid fields" context))
       (unless (member (alist-get 'wallet_kind payload)
                       '("transfer" "red-packet" "password-red-packet" "unknown"))
         (qq-api--signal-schema-error
          protocol-p "qq: %s wallet.wallet_kind is invalid" context))
       (dolist (key '(message_type session_type red_type red_channel grab_state))
         (unless (integerp (alist-get key payload))
           (qq-api--signal-schema-error
            protocol-p "qq: %s wallet.%s must be an integer" context key)))
       (unless (qq-protocol-user-uin-p (alist-get 'grabbed_amount payload))
         (qq-api--signal-schema-error
          protocol-p "qq: %s wallet.grabbed_amount must be decimal" context))
       (dolist (key '(sender receiver))
         (let ((presentation (alist-get key payload)))
           (unless (qq-api--exact-object-keys-p
                    presentation
                    '(background icon title sub_title content notice))
             (qq-api--signal-schema-error
              protocol-p "qq: %s wallet.%s has invalid fields"
              context key))
           (dolist (number-key '(background icon))
             (unless (integerp (alist-get number-key presentation))
               (qq-api--signal-schema-error
                protocol-p "qq: %s wallet.%s.%s must be an integer"
                context key number-key)))
           (qq-api--validate-string-fields
           presentation '(title sub_title content notice)
           context protocol-p))))
      ("gray-tip"
       (unless (qq-api--exact-object-keys-p
                payload '(gray_tip_kind text native_id))
         (qq-api--signal-schema-error
          protocol-p "qq: %s gray-tip payload has invalid fields" context))
       (unless (equal (alist-get 'gray_tip_kind payload) "revoke")
         (qq-api--signal-schema-error
          protocol-p "qq: %s gray-tip kind is invalid" context))
       (dolist (key '(text native_id))
         (unless (qq-protocol-non-empty-string-p (alist-get key payload))
           (qq-api--signal-schema-error
            protocol-p "qq: %s gray-tip.%s must be non-empty"
            context key))))
      ("video"
       (unless (qq-api--exact-object-keys-p
                payload '(file remote)
                '(local_path size name thumb))
         (qq-api--signal-schema-error
          protocol-p "qq: %s video payload has invalid fields" context))
       (unless (qq-protocol-non-empty-string-p (alist-get 'file payload))
         (qq-api--signal-schema-error
          protocol-p "qq: %s video file must be a non-empty string" context))
       (when (assq 'local_path payload)
         (unless (stringp (alist-get 'local_path payload))
           (qq-api--signal-schema-error
            protocol-p "qq: %s video local_path must be a string" context)))
       (when (assq 'size payload)
         (unless (and (integerp (alist-get 'size payload))
                      (>= (alist-get 'size payload) 0))
           (qq-api--signal-schema-error
            protocol-p "qq: %s video size must be a non-negative integer"
            context)))
       (dolist (key '(name thumb))
         (when (and (assq key payload)
                    (not (stringp (alist-get key payload))))
           (qq-api--signal-schema-error
            protocol-p "qq: %s video %s must be a string" context key)))
       (qq-api--validate-native-video-remote
        (alist-get 'remote payload) context protocol-p t))
      ("reply"
       (unless (qq-api--exact-object-keys-p payload '(target))
         (qq-api--signal-schema-error
          protocol-p "qq: %s reply payload requires only target" context))
       (let ((target (alist-get 'target payload)))
         (pcase (and (qq-api--single-alist-p target)
                     (alist-get 'kind target))
           ("entry"
            (unless (and (qq-api--exact-object-keys-p
                          target '(kind entry_id))
                         (qq-api-entry-id-p
                          (alist-get 'entry_id target)))
              (qq-api--signal-schema-error
               protocol-p "qq: %s reply entry target is invalid" context)))
           ("native"
            (unless (qq-api--exact-object-keys-p
                     target '(kind sequence)
                     '(sender sender_name sent_at))
              (qq-api--signal-schema-error
               protocol-p "qq: %s native reply target is invalid" context))
            (unless (qq-protocol-uint64-decimal-p
                     (alist-get 'sequence target))
              (qq-api--signal-schema-error
               protocol-p
               "qq: %s native reply sequence must be canonical nonzero uint64 text"
               context))
            (when (assq 'sender target)
              (let ((sender (alist-get 'sender target)))
                (unless
                    (and
                     (qq-api--exact-object-keys-p sender nil '(uin uid))
                     (or (assq 'uin sender) (assq 'uid sender))
                     (or (not (assq 'uin sender))
                         (qq-protocol-uint64-decimal-p
                          (alist-get 'uin sender)))
                     (or (not (assq 'uid sender))
                         (qq-protocol-non-empty-string-p
                          (alist-get 'uid sender))))
                  (qq-api--signal-schema-error
                   protocol-p "qq: %s native reply sender is invalid"
                   context))))
            (when (and (assq 'sender_name target)
                       (not (qq-protocol-non-empty-string-p
                             (alist-get 'sender_name target))))
              (qq-api--signal-schema-error
               protocol-p "qq: %s native reply sender_name is invalid"
               context))
            (when (and (assq 'sent_at target)
                       (not (and (qq-api--uint32-p
                                  (alist-get 'sent_at target))
                                 (> (alist-get 'sent_at target) 0))))
              (qq-api--signal-schema-error
               protocol-p "qq: %s native reply sent_at is invalid"
               context)))
           (_
            (qq-api--signal-schema-error
             protocol-p "qq: %s reply target has invalid kind" context)))))
      ("forward"
       (unless (qq-api--exact-object-keys-p payload '(content))
         (qq-api--signal-schema-error
          protocol-p "qq: %s forward payload requires only content" context))
       (let ((content (alist-get 'content payload)))
         (pcase (and (qq-api--single-alist-p content)
                     (alist-get 'kind content))
           ("inline"
            (unless (qq-api--exact-object-keys-p
                     content '(kind messages))
              (qq-api--signal-schema-error
               protocol-p "qq: %s inline content has invalid fields" context))
            (qq-api-validate-native-forward-messages
             (alist-get 'messages content)
             (format "%s inline messages" context) protocol-p))
           ("remote"
            (unless (qq-api--exact-object-keys-p
                     content '(kind reference))
              (qq-api--signal-schema-error
               protocol-p "qq: %s remote content has invalid fields" context))
            (qq-api-validate-forward-source
             (alist-get 'reference content)
             (format "%s reference" context) protocol-p))
           (_
            (qq-api--signal-schema-error
             protocol-p "qq: %s forward content has invalid kind" context)))))
      ("forward-card"
       (unless (qq-api--exact-object-keys-p
                payload '(reference presentation))
         (qq-api--signal-schema-error
          protocol-p
          "qq: %s forward-card requires only reference and presentation"
          context))
       (let ((reference
              (qq-api-validate-forward-source
               (alist-get 'reference payload)
               (format "%s reference" context) protocol-p)))
         (when (equal (alist-get 'kind reference) "message")
           (qq-api--signal-schema-error
            protocol-p
            "qq: %s forward-card reference must be resource or context"
            context)))
       (let ((presentation (alist-get 'presentation payload)))
         (unless (qq-api--exact-object-keys-p
                  presentation nil
                  '(source title content summary prompt))
           (qq-api--signal-schema-error
            protocol-p "qq: %s presentation has invalid fields" context))
         (dolist (key '(source title content summary prompt))
           (when (and (assq key presentation)
                      (not (stringp (alist-get key presentation))))
             (qq-api--signal-schema-error
              protocol-p "qq: %s presentation.%s must be a string"
              context key)))))
      ("unsupported"
       (unless (qq-api--exact-object-keys-p
                payload '(native_keys summary) '(raw))
         (qq-api--signal-schema-error
          protocol-p "qq: %s unsupported payload has invalid fields" context))
       (let ((native-keys (alist-get 'native_keys payload)))
         (unless (and (or (listp native-keys) (vectorp native-keys))
                      (cl-every #'stringp
                                (if (vectorp native-keys)
                                    (append native-keys nil)
                                  native-keys)))
           (qq-api--signal-schema-error
            protocol-p "qq: %s native_keys must be a string array" context)))
       (unless (stringp (alist-get 'summary payload))
         (qq-api--signal-schema-error
          protocol-p "qq: %s unsupported summary must be a string" context))
       (when (and (assq 'raw payload)
                  (not (qq-api--json-safe-value-p
                        (alist-get 'raw payload))))
         (qq-api--signal-schema-error
          protocol-p "qq: %s unsupported raw must be JSON-safe" context)))
      ((or "music" "poke" "dice" "rps" "contact" "location"
           "json" "card" "xml" "markdown" "miniapp" "onlinefile"
           "flashtransfer")
       (unless (or (null payload) (qq-api--single-alist-p payload))
         (qq-api--signal-schema-error
          protocol-p "qq: %s passthrough payload must be an object" context))
       (unless (qq-api--json-safe-value-p payload)
         (qq-api--signal-schema-error
          protocol-p "qq: %s passthrough payload must be JSON-safe" context))))
    (copy-tree segment)))

(defun qq-api--validate-native-forward-message (message context protocol-p)
  "Validate one native forward MESSAGE."
  (unless (qq-api--exact-object-keys-p
           message '(entry_id state sent_at sender origin segments)
           '(message_id))
    (qq-api--signal-schema-error
     protocol-p "qq: %s has invalid native message fields" context))
  (unless (qq-api-entry-id-p (alist-get 'entry_id message))
    (qq-api--signal-schema-error
     protocol-p "qq: %s entry_id must be a dotted decimal path" context))
  (when (assq 'message_id message)
    (qq-api-validate-message-id
     (alist-get 'message_id message) context protocol-p))
  (unless (and (integerp (alist-get 'sent_at message))
               (>= (alist-get 'sent_at message) 0))
    (qq-api--signal-schema-error
     protocol-p "qq: %s sent_at must be a non-negative integer" context))
  (unless (member (alist-get 'state message) '("live" "recalled"))
    (qq-api--signal-schema-error
     protocol-p "qq: %s state must be live or recalled" context))
  (let ((sender (alist-get 'sender message)))
    (pcase (and (qq-api--single-alist-p sender) (alist-get 'kind sender))
      ("user"
       (unless (qq-api--exact-object-keys-p
                sender '(kind user_id name) '(avatar_url))
         (qq-api--signal-schema-error
          protocol-p "qq: %s user sender has invalid fields" context))
       (unless (qq-protocol-user-uin-p (alist-get 'user_id sender))
         (qq-api--signal-schema-error
          protocol-p "qq: %s sender.user_id must be decimal" context))
       (unless (stringp (alist-get 'name sender))
         (qq-api--signal-schema-error
          protocol-p "qq: %s sender.name must be a string" context))
       (when (assq 'avatar_url sender)
         (unless (and (qq-protocol-non-empty-string-p
                       (alist-get 'avatar_url sender))
                      (string-match-p
                       "\\`https://" (alist-get 'avatar_url sender)))
           (qq-api--signal-schema-error
            protocol-p "qq: %s sender.avatar_url must be HTTPS" context))))
      ("anonymous"
       (unless (and (qq-api--exact-object-keys-p
                     sender '(kind name) '(avatar_url))
                    (stringp (alist-get 'name sender))
                    (or (not (assq 'avatar_url sender))
                        (and (qq-protocol-non-empty-string-p
                              (alist-get 'avatar_url sender))
                             (string-match-p
                              "\\`https://" (alist-get 'avatar_url sender)))))
         (qq-api--signal-schema-error
          protocol-p "qq: %s anonymous sender is invalid" context)))
      (_
       (qq-api--signal-schema-error
        protocol-p "qq: %s sender has invalid kind" context))))
  (let ((origin (alist-get 'origin message)))
    (if (equal (and (qq-api--single-alist-p origin)
                    (alist-get 'kind origin))
               "unknown")
        (unless (qq-api--exact-object-keys-p origin '(kind))
          (qq-api--signal-schema-error
           protocol-p "qq: %s unknown origin has invalid fields" context))
      (qq-api-validate-chat-locator
       origin (format "%s origin" context) protocol-p)))
  (let* ((segments (alist-get 'segments message))
         (state (alist-get 'state message)))
    (unless (or (listp segments) (vectorp segments))
      (qq-api--signal-schema-error
       protocol-p "qq: %s segments must be an array" context))
    (let ((items (if (vectorp segments)
                     (append segments nil)
                   segments)))
      (pcase state
        ("live"
         (unless (consp items)
           (qq-api--signal-schema-error
            protocol-p "qq: %s live message requires non-empty segments"
            context)))
        ("recalled"
         (when (consp items)
           (qq-api--signal-schema-error
            protocol-p "qq: %s recalled message requires empty segments"
            context))))
      (cl-loop for segment in items
             for index from 0
             do (qq-api--validate-native-forward-segment
                 segment (format "%s.segments[%d]" context index)
                 protocol-p))))
  (copy-tree message))

(defun qq-api-validate-native-forward-messages
    (messages &optional context protocol-p)
  "Validate and copy a native forward message array."
  (unless (or (listp messages) (vectorp messages))
    (qq-api--signal-schema-error
     protocol-p "qq: %s must be a native message array"
     (or context "forward messages")))
  (let ((items (if (vectorp messages) (append messages nil) messages))
        (context (or context "forward messages"))
        (entry-ids (make-hash-table :test #'equal)))
    (cl-loop for message in items
             for index from 0
             do (progn
                  (qq-api--validate-native-forward-message
                   message (format "%s[%d]" context index) protocol-p)
                  (let ((entry-id (alist-get 'entry_id message)))
                    (when (gethash entry-id entry-ids)
                      (qq-api--signal-schema-error
                       protocol-p "qq: %s has duplicate entry_id %S"
                       context entry-id))
                    (puthash entry-id t entry-ids))))
    (copy-tree items)))






























(defun qq-api--session-emacs-locator (session-key)
  "Return the closed Emacs protocol locator for SESSION-KEY."
  (let* ((identity (qq-state-session-key-identity session-key))
         (type (alist-get 'type identity))
         (target-id (alist-get 'target-id identity)))
    (pcase type
      ('group
       `((kind . "group") (group_id . ,target-id)))
      ('private
       `((kind . "private") (user_id . ,target-id)))
      ('dataline
       `((kind . "dataline")
         (peer_uid . ,(alist-get 'peer-uid identity))
         (variant . ,(alist-get 'variant identity))))
      ('service
       `((kind . "service")
         (peer_uid . ,(alist-get 'peer-uid identity))))
      ('guild-channel
       `((kind . "guild-channel")
         (guild_id . ,(alist-get 'guild-id identity))
         (channel_id . ,(alist-get 'channel-id identity))))
      (_ (error "qq: unsupported Emacs session type %s" type)))))






(defun qq-api-session-key-from-locator (locator)
  "Return the unique local session key represented by LOCATOR.

LOCATOR must satisfy the fork's closed `EmacsSessionLocator' union.  Opaque
peer UIDs stay strings and are never interpreted as QQ numbers."
  (setq locator
        (qq-protocol-validate-emacs-session-locator
         locator "Emacs session locator"))
  (pcase (alist-get 'kind locator)
    ("group"
     (qq-state-session-key 'group (alist-get 'group_id locator)))
    ("private"
     (qq-state-session-key 'private (alist-get 'user_id locator)))
    ("guild-channel"
     (qq-state-guild-channel-session-key
      (alist-get 'guild_id locator)
      (alist-get 'channel_id locator)))
    ("dataline"
     (qq-state-session-key 'dataline
                           (alist-get 'peer_uid locator)
                           (alist-get 'variant locator)))
    ("service"
     (qq-state-session-key 'service (alist-get 'peer_uid locator)))
    ;; The validator makes this unreachable.  Keep the branch explicit so a
    ;; future locator kind cannot silently map to the wrong session namespace.
    (_ (error "qq: unsupported Emacs session locator %S" locator))))










































































































(provide 'qq-api)

;;; qq-api.el ends here
