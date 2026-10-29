;;; qq-chat.el --- Chat buffers for emacs-qq -*- lexical-binding: t; -*-

;; Author: 0WD0 <me@0wd0.com>

;;; Commentary:

;; Chat buffer built on appkit's shared timeline and presentation primitives,
;; while borrowing
;; telega.el-style naming and the most familiar chat input bindings.

;;; Code:

(defvar-local qq-chat--preview-p nil
  "Non-nil for an auxiliary context preview chat.")

(defvar-local qq-search--inline-p nil)

(require 'cl-lib)
(require 'ring)
(require 'button)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-projection)
(require 'appkit-chat-avatar)
(require 'appkit-chatbuf)
(require 'appkit-bindings)
(require 'appkit-selection)
(require 'appkit-chat-history)
(require 'appkit-chat-completion)
(require 'appkit-chat-timeline)
(require 'appkit-chat-read)
(require 'appkit-scroll)
(require 'appkit-chat-ins)
(require 'appkit-name-color)
(require 'appkit-media)
(require 'appkit-media-inline)
(require 'appkit-media-grid)
(require 'appkit-ui)
(require 'appkit-presentation)
(require 'appkit-translate)
(require 'qq-core)
(require 'qq-reply)
(require 'qq-completion)
(require 'qq-customize)
(require 'qq-directory)
(require 'qq-media)
(require 'qq-protocol)
(require 'qq-runtime)
(require 'qq-timeline)
(require 'qq-state)
(require 'qq-request)

;; `qq-forward' requires this module to reuse the message-body renderer, so
;; keep the reverse dependency lazy and avoid a load cycle.
(autoload 'qq-forward-segment-p "qq-forward")
(autoload 'qq-forward-insert-segment "qq-forward")
(autoload 'qq-forward-event-segment-to-internal "qq-forward")
(autoload 'qq-user-open "qq-user" nil t)
(autoload 'qq-group-open "qq-group" nil t)
(autoload 'qq-guild-share-open "qq-guild-share")

(autoload 'qq-search-chat "qq-search" nil t)
(autoload 'qq-search-preview-hit "qq-search-preview" nil t)
(autoload 'qq-chat-search "qq-search-navigation" nil t)
(autoload 'qq-chat-search-forward "qq-search-navigation" nil t)
(autoload 'qq-chat-search-previous "qq-search-navigation" nil t)
(autoload 'qq-chat-search-next "qq-search-navigation" nil t)
(autoload 'qq-chat-search-cancel "qq-search-navigation" nil t)

(declare-function qq-root--session-key-at-point "qq-root" (&optional pos))
(declare-function qq-search--operation-update "qq-search" (context model message))
(declare-function qq-chat-search-status "qq-search-navigation")
(declare-function qq-transient-msg-operate "qq-transient" (&optional target))
(declare-function qq-chat-transient "qq-transient" ())
(declare-function qq-chat-composer-transient "qq-transient" ())
(declare-function qq-chat-forward-transient "qq-transient" (plan))
(declare-function qq-chat-delete-transient "qq-transient" (&optional target))
(declare-function qq-message-poke-recall-capable-p
                  "qq-message" (message))

(defun qq-chat--default-error (_response reason)
  "Display a chat operation failure REASON."
  (message "qq: %s" (or reason "chat operation failed")))

(defvar-local qq-chat--session-key nil
  "Session key associated with the current chat buffer.")

(defvar-local qq-chat--translation-owner nil
  "Surface owning this buffer's explicitly requested translations.")

(defvar-local qq-chat--translation-sources nil
  "Requested translation sources, keyed by account, session and message.")

(defvar-local qq-chat--my-action nil
  "Local outgoing chat-action for this buffer.

Non-nil means we currently advertise typing via `set_input_status'.  This
mirrors telega's `telega-chatbuf--my-action'.")

(defvar-local qq-chat--send-sync-request nil
  "Failed-send composer state awaiting safe presentation.

An asynchronous failure schedules only the exact live canonical view resolved
at restoration time.  The state remains pending so a later replacement can
materialize the restored composer before synchronizing its editable tail back
into canonical state.")

(defvar qq-chat-group-messages t
  "When non-nil, compact consecutive messages from the same sender.")

(defvar qq-chat-group-messages-timespan 300
  "Maximum time gap in seconds used for compact message grouping.")

(defconst qq-chat--empty-placeholder :qq-chat-empty-placeholder
  "Sentinel EWOC payload used for the empty timeline note.")

;; The chat Surface model owns its timeline: `:timeline' (the Gateway id, or
;; `dataline' for page-loaded DataLine chats), `:rows' ((ROW-KEY . ANCHOR) in
;; order), `:has-older' and `:has-newer', the focus being `:opening' and the
;; direction being `:paging'.  Commands read it; only Surface updates change it.

(defun qq-chat--model ()
  "Return the current chat Surface model, or nil when detached."
  (when-let* ((view (appkit-current-surface)))
    (appkit-surface-model view)))

(defun qq-chat--with (model &rest properties)
  "Return a copy of plist MODEL with PROPERTIES set."
  (let ((model (copy-sequence model)))
    (while properties
      (setq model (plist-put model (pop properties) (pop properties))))
    model))

(defun qq-chat--history-known-p ()
  "Return non-nil once this chat's timeline is open."
  (and (plist-get (qq-chat--model) :timeline) t))

(defun qq-chat--loading ()
  "Return the focus being opened or the direction being paged, or nil."
  (let ((model (qq-chat--model)))
    (or (plist-get model :opening) (plist-get model :paging))))

(defun qq-chat--window ()
  "Return this chat's timeline as an `appkit-chat-window' for presentation."
  (let ((model (qq-chat--model)))
    (if (plist-get model :timeline)
        (appkit-chat-window-with-older-loaded
         (appkit-chat-window-set appkit-chat-window-unknown nil (plist-get model :has-newer))
         (not (plist-get model :has-older)))
      appkit-chat-window-unknown)))

(defvar-local qq-chat--pending-jump-center nil
  "Typed history center to jump to after loading, or nil.")

(defvar-local qq-chat--jump-owner nil
  "Unique fence for the latest message jump, including its history request.")

(defvar-local qq-chat-after-render-hook nil
  "Hook run after the projected chat timeline has been synchronized.")

(defvar-local qq-chat-message-jump-hook nil
  "Hook called with the exact history center after a successful message jump.")

(defvar-local qq-chat--messages-pop-ring nil
  "Ring of typed history locators for jump-back.

This mirrors telega's `telega-chatbuf--messages-pop-ring'.")

(cl-defstruct
    (qq-chat-message-selection
     (:constructor qq-chat--make-message-selection (anchor owner message)))
  "One stable message-selection membership.

OWNER is an opaque identity token.  A completed asynchronous operation may
remove this membership only while the same OWNER still belongs to ANCHOR."
  anchor
  owner
  message)

(defvar-local qq-chat--message-selection nil
  "Ordered list of `qq-chat-message-selection' memberships.")

(defvar-local qq-chat--forward-presentation nil
  "Selection memberships awaiting presentation of an account-owned forward.")

(defvar-local qq-chat--forward-sync-request nil
  "Settled forward callback awaiting its Appkit presentation transaction.")

(defvar-local qq-chat--forward-plan-owner nil
  "Opaque runtime/account owner captured by newly created forward plans.")

(defvar qq-chat--last-forward-target-key nil
  "Most recent session key that accepted a forwarded message.")

(defvar qq-chat-forward-target-history nil
  "Minibuffer history for QQ forwarding destinations.")

(cl-defstruct
    (qq-chat-forward-plan
     (:constructor qq-chat--make-forward-plan
                   (buffer session-key anchors messages memberships
                           plan-owner surface)))
  "Immutable source snapshot passed through the forwarding transient."
  buffer
  session-key
  anchors
  messages
  memberships
  plan-owner
  surface)

(defvar-keymap qq-chat-timeline-mode-map
  :doc "Message actions active on the timeline, never in the composer."
  "a" #'qq-chat-open-avatar-at-point
  "i" #'qq-chat-open-user-at-point
  "h" #'qq-chat-open-peer-info
  "G" #'qq-chat-goto-reply
  "x" #'qq-chat-goto-pop-message
  "P" #'qq-chat-poke-sender)

(appkit-bindings-define 'qq-chat-timeline-mode-map 'read-only
  'return #'qq-chat-return-dwim
  'preview #'qq-search-preview-hit
  'app-menu #'qq-chat-transient
  'refresh #'qq-chat-refresh
  'filter-search #'qq-search-chat
  'next-item #'qq-chat-next-message
  'previous-item #'qq-chat-previous-message
  'message-menu #'qq-transient-msg-operate
  'reply #'qq-chat-reply-to-message
  'forward #'qq-chat-forward-transient
  'delete #'qq-chat-delete-transient
  'mark #'qq-chat-toggle-message-selection
  'clear-marks #'qq-chat-clear-message-selection
  'help #'describe-mode
  'quit #'quit-window
  'quit-cancel #'evil-quit)

(defvar-keymap qq-chat-group-timeline-mode-map
  :doc "Group-only message actions active outside the composer.")

(appkit-bindings-define 'qq-chat-group-timeline-mode-map 'read-only
  'react #'qq-chat-react-to-message)

(define-minor-mode qq-chat-group-timeline-mode
  "Group-only bindings accompanying `qq-chat-timeline-mode'."
  :init-value nil
  :lighter nil
  :keymap qq-chat-group-timeline-mode-map)

(define-minor-mode qq-chat-timeline-mode
  "Buffer-local navigation bindings active outside the draft region."
  :init-value nil
  :lighter nil
  :keymap qq-chat-timeline-mode-map
  (qq-chat--update-group-timeline-mode))

(defun qq-chat--session ()
  "Return current chat session object."
  (and qq-chat--session-key
       (qq-state-session qq-chat--session-key)))

(defun qq-chat--update-group-timeline-mode ()
  "Keep group-only actions in the active group's timeline context."
  (let ((enabled (and qq-chat-timeline-mode
                      (eq (alist-get 'type (qq-chat--session)) 'group))))
    (unless (eq qq-chat-group-timeline-mode enabled)
      (qq-chat-group-timeline-mode (if enabled 1 -1))
      (when (fboundp 'appkit-evil-normalize-keymaps)
        (appkit-evil-normalize-keymaps)))))

(defun qq-chat--single-line-presentation (value)
  "Return VALUE as trimmed single-line presentation text."
  (string-trim
   (replace-regexp-in-string "[\n\r\t ]+" " " (format "%s" value))))

(defun qq-chat--buffer-name-available-p (name owner session-key)
  "Return non-nil when NAME is free or already owns OWNER's SESSION-KEY."
  (let ((buffer (get-buffer name)))
    (or (not (buffer-live-p buffer))
        (and
         (equal (buffer-local-value 'qq-runtime--account-id buffer) owner)
         (equal (buffer-local-value 'qq-chat--session-key buffer)
                session-key)))))

(defun qq-chat--buffer-name (session-key)
  "Return a readable, globally unique buffer name for SESSION-KEY.

The stable Appkit view identity remains account-qualified and independent of
this presentation name.  Account and protocol identities appear only when a
human title collides with another live buffer."
  (let* ((owner (qq-runtime-current-account-id))
         (session (qq-state-session session-key))
         (type (qq-state-session-key-type session-key))
         (target-id (qq-state-session-key-target-id session-key))
         (session-title (alist-get 'title session))
         (title
          (qq-chat--single-line-presentation
           (if (and (stringp session-title)
                    (not (string-empty-p
                          (string-trim session-title))))
               session-title
             (or target-id session-key))))
         (prefix
          (qq-chat--single-line-presentation qq-chat-buffer-name-prefix))
         (brackets
          (pcase type
            ('private '("{" . "}"))
            ((or 'group 'guild-channel) '("[" . "]"))
            ('dataline '("<" . ">"))
            ('service '("(" . ")"))
            (_ '("[" . "]"))))
         (base
          (format "%s%s%s%s"
                  prefix
                  (car brackets) title (cdr brackets)))
         (account-name
          (and owner
               (qq-chat--single-line-presentation
                (qq-runtime-account-display-name owner))))
         (candidates
          (delete-dups
           (delq nil
                 (list
                  base
                  (and account-name
                       (format "%s<%s>" base account-name))
                  (and account-name target-id
                       (format "%s<%s:%s>" base account-name target-id))
                  (and owner
                       (format "%s<%s:%s>" base owner session-key))))))
         (available
          (cl-find-if
           (lambda (candidate)
             (qq-chat--buffer-name-available-p
              candidate owner session-key))
           candidates)))
    (or available
        (generate-new-buffer-name (car (last candidates))))))

(defun qq-chat--ensure-buffer-name ()
  "Rename current chat buffer to reflect latest session title."
  (when qq-chat--session-key
    (let ((name (if qq-chat--preview-p
                    (format "*QQ 上下文: %s:%s*" qq-runtime--account-id qq-chat--session-key)
                  (qq-chat--buffer-name qq-chat--session-key))))
      (unless (equal (buffer-name) name)
        (rename-buffer name)))))

(defun qq-chat--header-line ()
  "Return unread progress and useful transient chat state, or nil."
  (let* ((status (qq-state-connection-status))
         (selected-count (length qq-chat--message-selection))
         (parts
          (delq nil
                (list
                 (unless (memq status '(connected ready))
                   (format "[%s]" status))
                 (appkit-chat-read-count-text
                  (alist-get 'unread-message-count (qq-chat--session)))
                 (when (> selected-count 0)
                   (format "%d selected" selected-count))
                 (when (plist-get (qq-runtime-operation (list 'send-forward qq-chat--session-key)) :pending) "forwarding…")
                 (when qq-search--inline-p (qq-search--inline-status))
                 (when (fboundp 'qq-chat-search-status)
                   (qq-chat-search-status))))))
    (when parts
      (concat " " (string-join parts "  · ")))))

(defun qq-chat--format-time (timestamp)
  "Return display string for TIMESTAMP."
  (if (and timestamp (> timestamp 0))
      (format-time-string "%m-%d %H:%M:%S" (seconds-to-time timestamp))
    ""))

(defun qq-chat--format-time-short (timestamp)
  "Return compact display string for TIMESTAMP."
  (if (and timestamp (> timestamp 0))
      (format-time-string "%H:%M" (seconds-to-time timestamp))
    ""))

(defun qq-chat--line-fill-column ()
  "Return the usable timeline width for the current chat buffer."
  (or (appkit-surface-responsive-width
       (appkit-current-surface) qq-chat-auto-fill-margin-columns)
      (and (integerp fill-column) (> fill-column 0) fill-column)
      80))

(cl-defun qq-chat--insert-right-aligned-time
    (time &optional left-prefix-width (overflow-newline-p t))
  "Insert TIME at the shared appkit timeline right edge.

LEFT-PREFIX-WIDTH reserves a display-only prefix applied by the caller.
OVERFLOW-NEWLINE-P controls whether an overlong row may move TIME to a new
line; nil keeps service rows such as poke strictly one-line."
  (when (and (stringp time) (not (string-empty-p time)))
    (appkit-chat-ins-insert-right-aligned-text
     time
     (qq-chat--line-fill-column)
     :face 'qq-msg-status
     :left-prefix-width left-prefix-width
     :overflow-newline-p overflow-newline-p)))

(defun qq-chat--message-day-key (message)
  "Return local calendar day key string for MESSAGE, or nil."
  (let ((timestamp (alist-get 'time message)))
    (when (and timestamp (> timestamp 0))
      (format-time-string "%Y-%m-%d" (seconds-to-time timestamp)))))

(defun qq-chat--message-day-label (day-key)
  "Return the configured date-break label for DAY-KEY (YYYY-MM-DD)."
  (if (not (stringp day-key))
      "Unknown date"
    (condition-case _
        (format-time-string
         qq-chat-date-break-format
         (date-to-time (concat day-key "T00:00:00")))
      (error day-key))))

(defun qq-chat--present-string (value)
  "Return VALUE when it is a non-empty string, else nil."
  (and (stringp value)
       (not (string-empty-p value))
       value))

(defun qq-chat--message-sender-display-parts (message)
  "Return normalized sender display parts for MESSAGE.

The message boundary has already selected PRIMARY and the optional SECONDARY
from LinuxQQ's authored sender presentation.  Rendering must not re-resolve
those historical values through the current friend directory, conversation
title, or numeric identity."
  (let* ((primary
          (or (qq-chat--present-string (alist-get 'sender-name message))
              (error "qq: normalized message has no sender presentation")))
         (secondary
          (qq-chat--present-string
           (alist-get 'sender-secondary-name message))))
    (cons primary
          (and secondary
               (not (equal primary secondary))
               secondary))))

(defun qq-chat--message-sender-name (message)
  "Return primary sender display name for MESSAGE."
  (car (qq-chat--message-sender-display-parts message)))

(defun qq-chat--open-message-sender-profile (message)
  "Open MESSAGE sender by its canonical QQ user UIN."
  (let ((sender-id (alist-get 'sender-id message)))
    (unless (qq-protocol-user-uin-p sender-id)
      (user-error "qq: sender has no user profile"))
    (qq-user-open sender-id)))

(defun qq-chat--insert-message-sender (message face)
  "Insert sender label for MESSAGE using FACE.

The primary name is shown first, with a telega-like secondary name trail when
available."
  (let* ((parts (qq-chat--message-sender-display-parts message))
         (primary (car parts))
         (secondary (cdr parts))
         (action (lambda () (qq-chat--open-message-sender-profile message)))
         (help-echo "Open sender profile")
         (properties '(read-only t front-sticky t rear-nonsticky (read-only))))
    (appkit-ui-insert-action-button
     primary
     action
     :face face
     :help-echo help-echo
     :properties properties)
    (when secondary
      (let ((separator-start (point)))
        (insert " • ")
        (add-text-properties separator-start (point)
                             (append properties '(face shadow))))
      (appkit-ui-insert-action-button
       secondary
       action
       :face face
       :help-echo help-echo
       :properties properties))))

(defun qq-chat--same-sender-p (left right)
  "Return non-nil when LEFT and RIGHT messages share sender identity."
  (let ((left-id (alist-get 'sender-id left))
        (right-id (alist-get 'sender-id right)))
    (if (and left-id right-id)
        (equal left-id right-id)
      (let ((left-name (qq-chat--message-sender-name left))
            (right-name (qq-chat--message-sender-name right)))
        (and left-name right-name
             (equal left-name right-name))))))

(defun qq-chat--canonical-forward-segment (segment)
  "Translate SEGMENT at the root-event boundary, or return nil."
  (let ((type (and (listp segment) (alist-get 'type segment)))
        (data (and (listp segment) (alist-get 'data segment))))
    (and (or (equal type "forward")
             (and (equal type "card")
                  (equal (alist-get 'kind data) "forward")))
         (qq-forward-event-segment-to-internal
          segment qq-chat--session-key))))

(defun qq-chat--forward-segment-p (segment)
  "Return non-nil when SEGMENT translates to a forward record."
  (and (qq-chat--canonical-forward-segment segment) t))

(defun qq-chat--message-has-block-segments-p (message)
  "Return non-nil when MESSAGE contains block-like render segments."
  (let ((segments (alist-get 'segments message))
        found)
    (while (and segments (not found))
      (let ((segment (car segments)))
        (when (or (qq-chat--forward-segment-p segment)
                  (qq-chat--animated-face-segment-p segment)
                  (qq-chat--poke-segment-p segment)
                  (qq-chat--gray-tip-segment-p segment)
                  (qq-chat--mail-segment-p segment)
                  (qq-chat--card-segment-p segment)
                  (qq-chat--link-preview-segment-p segment)
                  (qq-chat--media-segment-p segment))
          (setq found t))
        (setq segments (cdr segments))))
    found))

(defun qq-chat--messages-compact-group-p (previous current)
  "Return non-nil when CURRENT should be compact-grouped under PREVIOUS."
  (and qq-chat-group-messages
       (listp previous)
       (listp current)
       ;; Service rows have no sender heading to continue on either side.
       (not (qq-state-service-message-p previous))
       (not (qq-state-service-message-p current))
       (not (qq-chat--message-has-block-segments-p current))
       (qq-chat--same-sender-p previous current)
       (let ((previous-time (or (alist-get 'time previous) 0))
             (current-time (or (alist-get 'time current) 0)))
         (and (> previous-time 0)
              (> current-time 0)
              (<= (abs (- current-time previous-time))
                  (max 0 qq-chat-group-messages-timespan))))))

(defun qq-chat--status-suffix (message)
  "Return display suffix representing MESSAGE state."
  (pcase (alist-get 'status message)
    ('pending
     (propertize "  …" 'face 'qq-msg-status 'help-echo "pending"))
    ('failed
     (propertize (format "  !%s"
                         (or (alist-get 'error message) "failed"))
                 'face 'error
                 'help-echo (or (alist-get 'error message) "send failed")))
    ('recalled
     (propertize "  ↩" 'face 'qq-msg-status 'help-echo "recalled"))
    (_ "")))

(defun qq-chat--message-body (message)
  "Return body text for MESSAGE."
  (if (qq-state-message-recalled-p message)
      "[message recalled]"
    (or (qq-state-message-preview message) "")))

(defun qq-chat--message-anchor (message)
  "Return stable anchor value for MESSAGE.

Prefer the Gateway NT snowflake `server-id' once known.  Pending optimistic
rows still use `local-id' until send succeeds and the node is rekeyed (see
`qq-chat--rekey-message-node-if-needed')."
  (qq-state-message-anchor message))

(defun qq-chat--translation-key (message)
  "Return the account/session-qualified native identity of MESSAGE."
  (list 'qq (alist-get 'gateway-account-id message)
        (alist-get 'session-key message) (qq-chat--message-anchor message)))

(defun qq-chat--message-translatable-p (message)
  "Whether MESSAGE has loaded prose belonging to this live chat.
Mention labels accompany prose, but are not themselves a reason to export a
message.  Previews, service notices and structured payloads are never prose."
  (and (qq-chat--live-current-view)
       (qq-chat--message-current-account-p message)
       (equal (alist-get 'session-key message) qq-chat--session-key)
       (qq-chat--message-anchor message)
       (not (qq-state-message-recalled-p message))
       (not (qq-state-service-message-p message))
       (seq-some
        (lambda (segment)
          (let ((text (and (equal (alist-get 'type segment) "text")
                           (alist-get 'text (alist-get 'data segment)))))
            (and (stringp text) (string-match-p "[^[:space:]]" text))))
        (alist-get 'segments message))))

(defun qq-chat--translation-version (message)
  "Return MESSAGE's exact loaded translation body representation.
Text and visible mention labels follow inline rendering.  Omitted objects
separate prose with a newline; reply previews do not contribute anything.
No media URL, forwarded body, card field or Gateway metadata is inspected."
  (let (parts)
    (dolist (segment (alist-get 'segments message))
      (let ((data (alist-get 'data segment)))
        (pcase (alist-get 'type segment)
          ("text"
           (push (substring-no-properties (or (alist-get 'text data) "")) parts))
          ("at"
           (let ((target (and (alist-get 'qq data)
                              (format "%s" (alist-get 'qq data)))))
             (push (concat "@" (or (alist-get 'name data)
                                  (and (equal target "all") "全体成员")
                                  target "mention"))
                   parts)))
          ("reply" nil)
          (_ (unless (equal (car parts) "\n") (push "\n" parts))))))
    (nreverse parts)))

(defun qq-chat--translation-source (message &optional text-p)
  "Describe MESSAGE's body, extracting plain text only when TEXT-P."
  (when (qq-chat--message-translatable-p message)
    (let* ((version (qq-chat--translation-version message))
           (source (list :key (qq-chat--translation-key message)
                         :version version)))
      (if text-p
          (plist-put source :text
                     (substring-no-properties (mapconcat #'identity version "")))
        source))))

(defun qq-chat--translation-prune (messages)
  "Revoke requested sources no longer represented by loaded MESSAGES.
Rekeying a pending local row deliberately requires a fresh explicit request;
an old local-id result never inherits authority over its server-id row."
  (when (and qq-chat--translation-sources
             (eq qq-chat--translation-owner (qq-chat--live-current-view)))
    (let ((current (make-hash-table :test #'equal))
          stale)
      (dolist (message messages)
        (let ((key (qq-chat--translation-key message)))
          (when (gethash key qq-chat--translation-sources)
            (puthash key (qq-chat--translation-source message) current))))
      (maphash
       (lambda (key source)
         (unless (equal (plist-get source :version)
                        (plist-get (gethash key current) :version))
           (push (cons key source) stale)))
       qq-chat--translation-sources)
      (dolist (entry stale)
        ;; Revoke notification authority before cancellation can call back.
        (remhash (car entry) qq-chat--translation-sources)
        (appkit-translate-hide (cdr entry) qq-chat--translation-owner)))))

(defun qq-chat-translate-message (&optional message)
  "Translate the loaded prose of MESSAGE or the sole operation target."
  (interactive)
  (let* ((message (or message (qq-chat--message-for-interactive)))
         (view (qq-chat--live-current-view))
         (message (and message
                       (qq-chat--message-in-view
                        view (qq-chat--message-anchor message))))
         (source (qq-chat--translation-source message t))
         (buffer (current-buffer)))
    (unless source (user-error "qq: this message has no loaded text to translate"))
    (unless (eq view qq-chat--translation-owner)
      (setq qq-chat--translation-owner view
            qq-chat--translation-sources (make-hash-table :test #'equal)))
    (puthash (plist-get source :key) source qq-chat--translation-sources)
    (appkit-translate-request
     source nil nil nil
     (lambda (key)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (when (and (eq view qq-chat--translation-owner)
                      (qq-chat--captured-view-current-p view))
             (qq-runtime-with-account (nth 1 key)
               (qq-chat--translation-prune
                (qq-state-session-messages qq-chat--session-key))
               (when (gethash key qq-chat--translation-sources)
                 (qq-runtime-invalidate view :keys (list (nth 3 key)))))))))
     view)))

(defun qq-chat--insert-translation (message prefix-state)
  "Insert an explicitly requested translation after MESSAGE's original body."
  (when (and qq-chat--translation-sources
             (eq qq-chat--translation-owner (qq-chat--live-current-view))
             (gethash (qq-chat--translation-key message)
                      qq-chat--translation-sources))
    (when-let* ((source (qq-chat--translation-source message)))
      (appkit-translate-insert source prefix-state nil qq-chat--translation-owner))))

(defun qq-chat--message-reply-data (message)
  "Return normalized reply segment data from MESSAGE, or nil."
  (qq-reply-data message))

(defun qq-chat--message-reply-id (message &optional reply-sequence-index)
  "Return MESSAGE's resolved reply anchor, or nil.
REPLY-SEQUENCE-INDEX holds candidate lists from one projection snapshot."
  (when-let* ((data (qq-reply-data message)))
    (let* ((sequence (or (alist-get 'message_seq data) (alist-get 'sequence data)))
           (source (or (qq-reply-find-source
                        data
                        (if (and sequence reply-sequence-index)
                            (gethash sequence reply-sequence-index)
                          (qq-state-session-messages qq-chat--session-key))
                        (and qq-chat--session-key
                             (qq-state-session-key-type qq-chat--session-key)))
                       (and qq-chat--session-key
                            (qq-reply-resource-source qq-chat--session-key data)))))
      (or (and source (qq-state-message-anchor source))
          (and (null sequence)
               (or (alist-get 'message_id data) (alist-get 'id data)))))))

(defun qq-chat--reply-data-locator (data)
  "Return an exact history locator from native reply DATA."
  (or (qq-reply-locator qq-chat--session-key data)
      (qq-reply-locator qq-chat--session-key data
                        (qq-state-session-messages qq-chat--session-key))))

(defun qq-chat--message-reply-locator (message)
  "Return MESSAGE's typed reply locator, preserving native source facts."
  (when-let* ((data (qq-chat--message-reply-data message)))
    (qq-chat--reply-data-locator data)))

(defun qq-chat--message-line-properties (message anchor)
  "Return shared line properties for MESSAGE using ANCHOR."
  (list 'qq-chat-message-anchor anchor
        'qq-chat-message-id (alist-get 'server-id message)
        'qq-chat-message-local-id (alist-get 'local-id message)
        'qq-chat-session-key qq-chat--session-key
        'qq-chat-message-selected
        (and (qq-chat-message-selected-p message) t)
        'read-only t
        'front-sticky '(read-only)
        'rear-nonsticky '(read-only)))

(defun qq-chat--message-at-point (&optional position)
  "Return message object under POSITION or point, or nil."
  (let* ((position (or position (point)))
         (anchor
          (or (get-text-property position 'qq-chat-message-anchor)
              (save-excursion
                (goto-char position)
                (get-text-property
                 (line-beginning-position) 'qq-chat-message-anchor)))))
    (seq-find
     (lambda (message)
       (equal (qq-chat--message-anchor message) anchor))
     (qq-state-session-messages qq-chat--session-key))))

(defun qq-chat--message-in-view (view anchor)
  "Resolve ANCHOR in its exact live VIEW and account, or signal `user-error'.
This is also the post-prompt ownership check for destructive message actions."
  (unless (appkit-surface-live-p view)
    (user-error "qq: the message view has closed"))
  (with-current-buffer (appkit-surface-buffer view)
    (unless (and (qq-chat--captured-view-current-p view)
                 (equal qq-runtime--account-id
                        (appkit-app-identity (appkit-surface-app view))))
      (user-error "qq: the message view has been replaced"))
    (qq-runtime-with-account qq-runtime--account-id
      (or (and anchor
               (seq-find
                (lambda (message)
                  (equal anchor (qq-chat--message-anchor message)))
                (qq-state-session-messages qq-chat--session-key)))
          (user-error "qq: the message is no longer loaded")))))

(defun qq-chat--message-index (message-id messages)
  "Return MESSAGE-ID's zero-based position in ordered MESSAGES, or nil."
  (seq-position
   messages message-id
   (lambda (message candidate-id)
     (equal (alist-get 'server-id message) candidate-id))))

(defun qq-chat--canonical-row-index (row-key messages)
  "Return ROW-KEY's zero-based position in ordered MESSAGES, or nil."
  (seq-position
   messages row-key
   (lambda (message candidate-row)
     (equal (alist-get 'canonical-row-key message) candidate-row))))

(defun qq-chat--read-target-needed-p (message)
  "Return non-nil when canonical MESSAGE can advance the known read position.

First-unread is compared by native sequence because the message owning that
position may have been locally deleted and is therefore absent from the local
timeline; a server-id lookup would then fail closed forever."
  (let* ((messages (qq-state-session-messages qq-chat--session-key))
         (row-key (alist-get 'canonical-row-key message))
         (target-index (qq-chat--canonical-row-index row-key messages))
         (last-index
          (and (alist-get 'row_key (plist-get (qq-runtime-operation (list 'read-message qq-chat--session-key)) :value))
               (qq-chat--canonical-row-index
                (alist-get 'row_key (plist-get (qq-runtime-operation (list 'read-message qq-chat--session-key)) :value)) messages)))
         (session (qq-state-session qq-chat--session-key))
         (unread-count (alist-get 'unread-message-count session))
         (first-unread (alist-get 'first-unread-message-id session))
         (read-latest (alist-get 'read-latest-message-id session)))
    (and target-index
         (or (null (alist-get 'row_key (plist-get (qq-runtime-operation (list 'read-message qq-chat--session-key)) :value)))
             (and last-index (> target-index last-index)))
         (cond
          ((and (integerp unread-count) (= unread-count 0) read-latest)
           (when-let* ((read-index
                        (qq-chat--message-index read-latest messages)))
             (> target-index read-index)))
          ((and (integerp unread-count) (> unread-count 0) first-unread)
           (when-let* ((target-seq (alist-get 'message-seq message))
                       (first-seq
                        (alist-get 'first-unread-message-seq session)))
             (not (qq-protocol-decimal-less-p target-seq first-seq))))
          (t t)))))

(defun qq-chat--mark-message-viewed (message)
  "Ask the App to advance its shared read frontier through visible MESSAGE."
  (when (and message (qq-core-message-read-capable-p message)
             (qq-chat--read-target-needed-p message))
    (qq-runtime-request (list 'read-message message))))

(defun qq-chat--manage-read-position (&optional position window)
  "Advance read state to the message represented by POSITION in WINDOW.
Point in the composer represents the newest loaded message.  A
passive context preview waits until the user operates its window."
  (let ((window (or window (selected-window))))
    (when (and (not qq-search--inline-p) qq-auto-mark-read qq-chat--session-key
               (not (eq (car-safe (window-parameter window 'qq-chat-preview))
                        (current-buffer))))
      (when-let* ((row
                   (appkit-chat-read-target
                    position
                    (lambda (candidate)
                      (qq-core-message-read-capable-p
                       (appkit-chat-timeline-row-payload candidate))))))
        (qq-chat--mark-message-viewed
         (appkit-chat-timeline-row-payload row))))))

(defun qq-chat--message-forwardable-p (message)
  "Return non-nil when MESSAGE can be forwarded by its server ID."
  (and (listp message)
       (qq-protocol-message-id-p (alist-get 'server-id message))
       (not (qq-state-service-message-p message))
       (not (qq-state-message-recalled-p message))))

(defun qq-chat--forward-source-supported-p
    (&optional style session-key)
  "Return non-nil when SESSION-KEY may submit native forwarding.

STYLE is nil while discovering a plan, `individual', or `merged'."
  (condition-case nil
      (let ((type
             (qq-state-session-key-type
              (or session-key qq-chat--session-key))))
        (and (memq style '(nil individual merged))
             (memq type '(private group))))
    (error nil)))

(defun qq-chat--validate-forward-source-session (style session-key)
  "Return SESSION-KEY when it supports native forwarding STYLE."
  (unless (qq-chat--forward-source-supported-p style session-key)
    (user-error "qq: %s forwarding is unavailable from %s"
                (or style 'native) session-key))
  session-key)

(defun qq-chat--message-selection-anchors ()
  "Return the selected message anchors in membership insertion order."
  (mapcar #'qq-chat-message-selection-anchor qq-chat--message-selection))

(defun qq-chat--message-selection-find (anchor)
  "Return the current selection membership for ANCHOR, or nil."
  (seq-find
   (lambda (membership)
     (equal anchor (qq-chat-message-selection-anchor membership)))
   qq-chat--message-selection))

(defun qq-chat--stable-message-order (messages)
  "Return MESSAGES in stable QQ timeline order.

Server time is compared first and exact per-session message sequence second.
When any message in one timestamp bucket lacks a sequence, that entire bucket
uses state insertion order instead.  This makes the comparator transitive
while retaining the only ordering evidence shared by every item.  The
decorated input position settles ties without treating snowflake IDs as
ordering numbers."
  (let ((sequence-complete-by-time (make-hash-table :test #'eql))
        (missing (make-symbol "missing"))
        (indexed
         (cl-loop for message in messages
                  for index from 0
                  collect (cons index message))))
    (dolist (item indexed)
      (let* ((message (cdr item))
             (time
              (qq-state--normalize-time (alist-get 'time message)))
             (previous
              (gethash time sequence-complete-by-time missing))
             (sequence-p
              (qq-protocol-uint64-decimal-p (alist-get 'message-seq message))))
        (puthash time
                 (and (or (eq previous missing) previous) sequence-p)
                 sequence-complete-by-time)))
    (mapcar
     #'cdr
     (sort indexed
           (lambda (left right)
             (let ((left-message (cdr left))
                   (right-message (cdr right))
                   (left-sequence (alist-get 'message-seq (cdr left)))
                   (right-sequence (alist-get 'message-seq (cdr right)))
                   (left-time
                    (qq-state--normalize-time
                     (alist-get 'time (cdr left))))
                   (right-time
                    (qq-state--normalize-time
                     (alist-get 'time (cdr right)))))
               (cond
                ((< left-time right-time) t)
                ((> left-time right-time) nil)
                ((and (gethash left-time sequence-complete-by-time)
                      (not (equal left-sequence right-sequence)))
                 (< (qq-protocol-decimal-string-compare
                     left-sequence right-sequence)
                    0))
                ((qq-state--message-sort< left-message right-message) t)
                ((qq-state--message-sort< right-message left-message) nil)
                (t (< (car left) (car right))))))))))

(defun qq-chat--message-candidates ()
  "Return materialized operation candidates in stable timeline order."
  (let ((messages (qq-state-session-messages qq-chat--session-key)))
    (dolist (membership qq-chat--message-selection)
      (let ((anchor (qq-chat-message-selection-anchor membership))
            (snapshot (qq-chat-message-selection-message membership)))
        (when (listp snapshot)
          (setq snapshot
                (qq-state-message-apply-tombstones
                 qq-chat--session-key snapshot)))
        (when (and
               (listp snapshot)
               (not
                (seq-some
                 (lambda (message)
                   (equal anchor (qq-chat--message-anchor message)))
                 messages)))
          (setq messages (append messages (list snapshot))))))
    (qq-chat--stable-message-order messages)))

(defun qq-chat-message-selected-p (message)
  "Return non-nil when normalized MESSAGE belongs to the selection set."
  (and (qq-chat--message-selection-find (qq-chat--message-anchor message)) t))

(defun qq-chat-selected-messages ()
  "Return selected, forwardable messages in stable timeline order."
  (seq-filter
   (lambda (message)
     (and (qq-chat--message-forwardable-p message)
          (qq-chat-message-selected-p message)))
   (qq-chat--message-candidates)))

(defun qq-chat--prune-message-selection ()
  "Drop memberships whose authoritative candidate is no longer forwardable.

The state accessor intentionally returns a defensive deep copy, so only pay
that cost while a non-empty message selection needs validation.  Header
formatting then reads the buffer-local membership list in constant time.
Canonical candidates remain visible to this validation while a filter owns
the rendered projection; selection-owned snapshots bridge refresh and cancel
gaps before filter-only rows enter canonical history."
  (when qq-chat--message-selection
    (let ((forwardable (make-hash-table :test #'equal)))
      (dolist (message (qq-chat--message-candidates))
        (when (qq-chat--message-forwardable-p message)
          (puthash (qq-chat--message-anchor message) t forwardable)))
      (setq qq-chat--message-selection
            (seq-filter
             (lambda (membership)
               (gethash (qq-chat-message-selection-anchor membership)
                        forwardable))
             qq-chat--message-selection)))))

(defun qq-chat--rekey-message-selection (previous-anchor anchor)
  "Move selection from PREVIOUS-ANCHOR to canonical ANCHOR."
  (when (and previous-anchor anchor (not (equal previous-anchor anchor)))
    (when-let* ((previous
                 (qq-chat--message-selection-find previous-anchor)))
      (if (qq-chat--message-selection-find anchor)
          ;; Preserve the canonical membership's newer opaque owner.
          (setq qq-chat--message-selection
                (delq previous qq-chat--message-selection))
        (setq qq-chat--message-selection
              (mapcar
               (lambda (membership)
                 (if (eq membership previous)
                     (let ((message
                            (copy-tree
                             (qq-chat-message-selection-message membership))))
                       (setf (alist-get 'server-id message) anchor
                             (alist-get 'id message) anchor)
                       (qq-chat--make-message-selection
                        anchor
                        (qq-chat-message-selection-owner membership)
                        message))
                   membership))
               qq-chat--message-selection))))))

(defun qq-chat--forwardable-target-sessions ()
  "Return all sendable forward targets known from sessions and contacts."
  (let ((by-key (make-hash-table :test #'equal))
        targets)
    (dolist (session (qq-state-sessions))
      (when (member (format "%s"
                            (or (alist-get 'type session)
                                (qq-state-session-key-type
                                 (alist-get 'key session))))
                    '("private" "group"))
        (puthash (alist-get 'key session) session by-key)))
    (dolist (friend (qq-state-friends))
      (when-let* ((id (alist-get 'user_id friend))
                  (key (qq-state-session-key 'private id)))
        (unless (gethash key by-key)
          (puthash
           key
           `((key . ,key)
             (type . private)
             (target-id . ,(format "%s" id))
             (peer-uin . ,(format "%s" id))
             (title . ,(or (qq-chat--present-string
                            (alist-get 'remark friend))
                           (qq-chat--present-string
                            (alist-get 'nickname friend))
                           (format "%s" id))))
           by-key))))
    (dolist (group (qq-state-groups))
      (when-let* ((id (alist-get 'group_id group))
                  (key (qq-state-session-key 'group id)))
        (unless (gethash key by-key)
          (puthash
           key
           `((key . ,key)
             (type . group)
             (target-id . ,(format "%s" id))
             (title . ,(or (qq-chat--present-string
                            (alist-get 'group_name group))
                           (format "%s" id))))
           by-key))))
    (maphash (lambda (_key target) (push target targets)) by-key)
    (sort targets
          (lambda (left right)
            (let ((left-title (or (alist-get 'title left) ""))
                  (right-title (or (alist-get 'title right) "")))
              (if (equal left-title right-title)
                  (string-lessp (alist-get 'key left)
                                (alist-get 'key right))
                (string-lessp left-title right-title)))))))

(defun qq-chat--forward-style-label (style)
  "Return the user-facing label for forward STYLE."
  (pcase style
    ('individual "逐条转发")
    ('merged "合并转发")
    (_ (error "qq: unknown forward style %S" style))))

(defun qq-chat--forward-target-type-label (session)
  "Return a concise destination type label for SESSION."
  (pcase (format "%s" (or (alist-get 'type session)
                          (qq-state-session-key-type
                           (alist-get 'key session))))
    ("private" "好友")
    ("group" "群聊")
    (_ "会话")))

(defun qq-chat--read-forward-target (style count)
  "Read a destination for forwarding COUNT messages using STYLE."
  (let* ((sessions (qq-chat--forwardable-target-sessions))
         (choices
          (mapcar
           (lambda (session)
             (let ((key (alist-get 'key session)))
               (cons (format "%s  · %s  [%s]"
                             (or (alist-get 'title session) key)
                             (qq-chat--forward-target-type-label session)
                             key)
                     key)))
           sessions))
         (default
          (car (rassoc qq-chat--last-forward-target-key choices))))
    (unless choices
      (user-error "qq: no forwarding destination is available"))
    (let ((selected
           (completing-read
            (format "%s %d 条消息到: "
                    (qq-chat--forward-style-label style) count)
            choices nil t nil 'qq-chat-forward-target-history default)))
      (or (cdr (assoc selected choices))
          (user-error "qq: invalid forwarding destination")))))

(defun qq-chat-toggle-message-selection ()
  "Toggle the message at point in the stable selection and move forward."
  (interactive)
  ;; Keep the direct `m' command behind the same closed source capability
  ;; gate as plan creation and submission.  Unsupported sessions must never
  ;; accumulate a selection that no forwarding style can consume.
  (qq-chat--validate-forward-source-session nil qq-chat--session-key)
  (let* ((message (or (qq-chat--message-at-point)
                      (user-error "qq: no message at point")))
         (anchor (qq-chat--message-anchor message))
         (membership (qq-chat--message-selection-find anchor)))
    (unless (qq-chat--message-forwardable-p message)
      (user-error "qq: this message cannot be forwarded"))
    (if membership
        (setq qq-chat--message-selection
              (delq membership qq-chat--message-selection))
      (setq qq-chat--message-selection
            (append
             qq-chat--message-selection
             (list
              (qq-chat--make-message-selection
               anchor
               (list 'message-selection anchor)
               (copy-tree message))))))
    (qq-chat--request-row-redisplay (list anchor))
    (qq-chat--header-line-update)
    (message "qq: 已选择 %d 条消息"
             (length qq-chat--message-selection))
    ;; Match telega: repeated `m' walks through the timeline, but the final
    ;; message never drops point into the composer.
    (let ((position (qq-chat--current-message-position)))
      (when (and position
                 (seq-some (lambda (candidate) (> candidate position))
                           (qq-chat--message-positions)))
        (qq-chat-next-message)))))

(defun qq-chat-clear-message-selection (&optional quiet)
  "Clear the current message selection.

Suppress the status message when QUIET is non-nil."
  (interactive)
  (let ((anchors (qq-chat--message-selection-anchors)))
    (setq qq-chat--message-selection nil)
    (when anchors
      (qq-chat--request-row-redisplay anchors))
    (qq-chat--header-line-update)
    (unless quiet
      (message "qq: 已清除消息选择"))))

(defun qq-chat--operation-target-keys ()
  "Capture the active message region, marks, or point without retargeting."
  (if (use-region-p)
      (appkit-selection-target-keys
       :region (appkit-chat-timeline-keys-in-range
                (region-beginning) (region-end)))
    (let ((marked (qq-chat--message-selection-anchors)))
      (appkit-selection-target-keys
       :marked marked
       :point (unless (or marked (appkit-chatbuf-point-in-input-p))
                (when-let* ((message (qq-chat--message-at-point)))
                  (qq-chat--message-anchor message)))))))

(defun qq-chat--operation-messages ()
  "Resolve current operation targets in timeline order, including marked snapshots."
  (when-let* ((keys (qq-chat--operation-target-keys)))
    (let ((selected (make-hash-table :test #'equal)))
      (dolist (key keys) (puthash key t selected))
      (let ((messages
             (seq-filter
              (lambda (message)
                (gethash (qq-chat--message-anchor message) selected))
              (qq-chat--message-candidates))))
        (unless (= (length messages) (hash-table-count selected))
          (user-error "qq: a selected message is no longer available"))
        messages))))

(defun qq-chat--message-for-interactive ()
  "Resolve the sole operation target, rejecting a multiple-message selection."
  (let ((messages (qq-chat--operation-messages)))
    (unless (= (length messages) 1)
      (user-error "qq: this action requires exactly one message"))
    (qq-chat--message-in-view
     (qq-chat--live-current-view) (qq-chat--message-anchor (car messages)))))

(defun qq-chat--current-forward-plan (&optional messages)
  "Capture MESSAGES, or the active operation targets, as a stable forward plan."
  (qq-chat--validate-forward-source-session nil qq-chat--session-key)
  (let* ((messages (or messages (qq-chat--operation-messages)))
         (anchors (mapcar #'qq-chat--message-anchor messages))
         (memberships
          (delq nil
                (mapcar
                 (lambda (anchor)
                   (when-let* ((membership
                                (qq-chat--message-selection-find anchor)))
                     (cons anchor
                           (qq-chat-message-selection-owner membership))))
                 anchors))))
    (unless messages
      (user-error "qq: select a message to forward"))
    (dolist (message messages)
      (unless (qq-chat--message-forwardable-p message)
        (user-error "qq: selected message %s cannot be forwarded"
                    (qq-chat--message-anchor message))))
    (qq-chat--make-forward-plan
     (current-buffer)
     qq-chat--session-key
     anchors
     (copy-tree messages)
     memberships
     qq-chat--forward-plan-owner
     (or (qq-chat--live-current-view)
         (user-error "qq: forwarding requires a live message view")))))

(defun qq-chat--forward-plan-messages (plan)
  "Return PLAN's immutable message snapshots in their original order."
  (unless (qq-chat-forward-plan-p plan)
    (user-error "qq: invalid forwarding plan"))
  (let ((buffer (qq-chat-forward-plan-buffer plan))
        (session-key (qq-chat-forward-plan-session-key plan))
        (anchors (qq-chat-forward-plan-anchors plan))
        (messages (qq-chat-forward-plan-messages plan))
        (plan-owner (qq-chat-forward-plan-plan-owner plan))
        (surface (qq-chat-forward-plan-surface plan)))
    (unless (buffer-live-p buffer)
      (user-error "qq: forwarding source buffer no longer exists"))
    (with-current-buffer buffer
      (unless (and (derived-mode-p 'qq-chat-mode)
                   (equal qq-chat--session-key session-key))
        (user-error "qq: forwarding source buffer changed sessions"))
      (unless (and (qq-chat--captured-view-current-p surface)
                   (equal qq-runtime--account-id
                          (appkit-app-identity (appkit-surface-app surface))))
        (user-error "qq: forwarding source view has closed or been replaced"))
      (unless (and plan-owner (eq plan-owner qq-chat--forward-plan-owner))
        (user-error "qq: forwarding plan belongs to a stale runtime"))
      (unless (and (consp messages)
                   (= (length anchors) (length messages)))
        (user-error "qq: forwarding plan has inconsistent messages"))
      (qq-runtime-with-account qq-runtime--account-id
        (let ((projected
               (mapcar
                (lambda (message)
                  (qq-state-message-apply-tombstones session-key message))
                messages)))
          (cl-loop for anchor in anchors
                   for message in projected
                   unless (and (equal anchor (qq-chat--message-anchor message))
                               (qq-chat--message-forwardable-p message))
                   do (user-error
                       "qq: forwarding plan message %s is invalid" anchor))
          (copy-tree projected))))))

(defun qq-chat--clear-forward-presentation ()
  "Release this Surface's selection presentation without cancelling App sends."
  (setq qq-chat--forward-presentation nil qq-chat--forward-sync-request nil))

(defun qq-chat--consume-forward-memberships (memberships)
  "Remove only the still-owned selection MEMBERSHIPS and return changed keys."
  (let (removed)
    (dolist (snapshot memberships)
      (let* ((anchor (car snapshot))
             (membership (qq-chat--message-selection-find anchor)))
        (when (and membership
                   (eq (qq-chat-message-selection-owner membership)
                       (cdr snapshot)))
          (setq qq-chat--message-selection
                (delq membership qq-chat--message-selection))
          (push anchor removed))))
    (nreverse removed)))

(defun qq-chat--submit-forward (style plan &optional target-session-key)
  "Submit the selected PLAN to the account App using STYLE."
  (unless (qq-chat-forward-plan-p plan) (user-error "qq: invalid forwarding plan"))
  (with-current-buffer (qq-chat-forward-plan-buffer plan)
    (let* ((source (qq-chat-forward-plan-session-key plan))
           (target (or target-session-key
                       (qq-chat--read-forward-target style (length (qq-chat-forward-plan-anchors plan))))))
      ;; Recheck the selection after the minibuffer has allowed other events.
      (qq-chat--validate-forward-source-session style source)
      (let ((request (list 'send-forward source target (qq-chat--forward-plan-messages plan) style)))
        (setq qq-chat--forward-presentation
              (list :request request :memberships (copy-sequence (qq-chat-forward-plan-memberships plan))
                    :view (qq-chat-forward-plan-surface plan)))
        (condition-case condition
            (progn (qq-runtime-request request nil 'forward) request)
          ((error quit)
           (setq qq-chat--forward-presentation nil)
           (signal (car condition) (cdr condition))))))))

(defun qq-chat-forward-individually (&optional plan target-session-key)
  "Forward PLAN as separate ordinary messages to TARGET-SESSION-KEY."
  (interactive)
  (setq plan (or plan (qq-chat--current-forward-plan)))
  (unless (and (qq-chat-forward-plan-p plan)
               (appkit-surface-live-p (qq-chat-forward-plan-surface plan)))
    (user-error "qq: forwarding source view has closed"))
  (let ((surface (qq-chat-forward-plan-surface plan)))
    (with-current-buffer (appkit-surface-buffer surface)
      (qq-runtime-with-account
          (appkit-app-identity (appkit-surface-app surface))
        (qq-chat--submit-forward 'individual plan target-session-key)))))

(defun qq-chat-forward-merged (&optional plan target-session-key)
  "Forward PLAN as one merged-forward card to TARGET-SESSION-KEY."
  (interactive)
  (setq plan (or plan (qq-chat--current-forward-plan)))
  (unless (and (qq-chat-forward-plan-p plan)
               (appkit-surface-live-p (qq-chat-forward-plan-surface plan)))
    (user-error "qq: forwarding source view has closed"))
  (let ((surface (qq-chat-forward-plan-surface plan)))
    (with-current-buffer (appkit-surface-buffer surface)
      (qq-runtime-with-account
          (appkit-app-identity (appkit-surface-app surface))
        (qq-chat--submit-forward 'merged plan target-session-key)))))

(defun qq-chat--message-positions ()
  "Return list of message start positions in the current buffer."
  (when (appkit-chat-timeline-live-p)
    (delq nil
          (mapcar #'appkit-chat-timeline-key-position
                  (appkit-chat-timeline-keys)))))

(defun qq-chat--current-message-position ()
  "Return the start position of the message at point, or nil."
  (when (appkit-chat-timeline-live-p)
    (when-let* ((anchor (appkit-chat-timeline-key-at-point)))
      (appkit-chat-timeline-key-position anchor))))

(defun qq-chat-next-message ()
  "Move point to the next rendered message block."
  (interactive)
  (let* ((positions (qq-chat--message-positions))
         (current (or (qq-chat--current-message-position) (point)))
         (next (seq-find (lambda (pos) (> pos current)) positions)))
    (if next
        (goto-char next)
      (message "qq: no later message"))))

(defun qq-chat-previous-message ()
  "Move point to the previous rendered message block."
  (interactive)
  (let* ((positions (qq-chat--message-positions))
         (current (or (qq-chat--current-message-position) (point)))
         (previous nil))
    (dolist (pos positions)
      (when (< pos current)
        (setq previous pos)))
    (if previous
        (goto-char previous)
      (message "qq: no earlier message"))))

(defun qq-chat--current-draft-string ()
  "Return canonical draft as plain text."
  (appkit-chatbuf-string-plain-text (appkit-chatbuf-input-state)))

(defun qq-chat--composer-boundary-valid-p ()
  "Return non-nil when live input is wholly after the EWOC footer."
  (let ((input (appkit-chatbuf-input-start-position)))
    (or (not (appkit-chat-timeline-live-p))
        (null input)
        (let ((footer (appkit-chat-timeline-footer-start-position))
              (prompt (appkit-chatbuf-prompt-start-position)))
          (and footer prompt (<= footer prompt input))))))

(defun qq-chat--canonical-input-contaminated-p ()
  "Return non-nil when canonical input contains timeline-owned text."
  (let ((state (appkit-chatbuf-input-state)))
    (and (> (length state) 0)
         (text-property-not-all
          0 (length state) 'qq-chat-message-anchor nil state))))

(defun qq-chat--assert-canonical-input-clean ()
  "Reject canonical input polluted with rendered timeline rows."
  (when (qq-chat--canonical-input-contaminated-p)
    (error "qq: canonical input contains rendered message rows")))

(defun qq-chat--sync-draft-from-buffer ()
  "Sync canonical draft from the editable buffer region."
  (let ((result
         (if (qq-chat--composer-boundary-valid-p)
             (appkit-chatbuf-input-state-sync)
           ;; Never let a transient/corrupted marker turn the timeline into a
           ;; canonical draft.  A later frame invariant will surface the bad
           ;; boundary without multiplying message text on every rebuild.
           (list :changed-p nil
                 :value (appkit-chatbuf-input-state)
                 :invalid-boundary-p t))))
    (when (plist-get result :changed-p)
      ;; Do not let pending failed-send presentation overwrite a newer live
      ;; edit before it is synchronized into canonical state.
      (setq qq-chat--send-sync-request nil)
      (qq-chat--maybe-update-my-action-from-input))
    (qq-chat--prune-clipboard-files)
    (plist-get result :value)))

(defun qq-chat--reply-message ()
  "Return current reply target message from shared aux state, or nil."
  (let ((state (appkit-chatbuf-aux-state)))
    (and (eq (plist-get state :aux-type) 'reply)
         (plist-get state :aux-msg))))

(defun qq-chat--set-reply-message (message)
  "Set shared reply aux state to MESSAGE, or clear it when nil."
  (if message
      (appkit-chatbuf-aux-set
       (list :aux-type 'reply
             :aux-msg message
             :message-id (alist-get 'server-id message)))
    (appkit-chatbuf-aux-reset)))

(defun qq-chat--flush-deferred-node-redisplay ()
  "Flush any node redisplay deferred while a region was active."
  (when (appkit-chat-timeline-live-p)
    (let ((result (appkit-chat-timeline-flush-deferred)))
      (when result
        (run-hooks 'qq-chat-after-render-hook))
      result)))

(defun qq-chat--maybe-auto-load-older (&optional position)
  "Load older history or search matches as POSITION approaches the top."
  (let ((position (or position (point))))
    (when (and qq-chat--session-key
               (not (appkit-chatbuf-point-in-input-p))
               (if qq-search--inline-p
                   (and (qq-search--can-load-more-p)
                        (appkit-scroll-near-start-p
                         position (point-min) qq-chat-history-auto-load-threshold))
                 (appkit-chat-window-autoload-older-p
                  (qq-chat--window) (qq-chat--loading)
                  position (point-min) qq-chat-history-auto-load-threshold)))
      (when-let* ((view (qq-chat--live-current-view)))
        (qq-chat--request-callback-sync
         view (lambda () (qq-chat-load-older-messages t)))))))

(defun qq-chat--maybe-auto-load-newer (&optional position)
  "Load a newer page when POSITION approaches the timeline footer."
  (let ((position (or position (point)))
        (footer (or (appkit-chat-timeline-footer-start-position)
                    (appkit-chatbuf-input-start-position)
                    (point-max))))
    (when (and (not qq-search--inline-p) qq-chat--session-key
               (appkit-chat-window-autoload-newer-p
                (qq-chat--window) (qq-chat--loading) position footer
                qq-chat-history-auto-load-threshold (appkit-chatbuf-composer-idle-p)))
      (when-let* ((view (qq-chat--live-current-view)))
        (qq-chat--request-callback-sync
         view (lambda () (qq-chat-load-newer-messages t)))))))

(defun qq-chat--install-scroll-observer (view)
  "Install VIEW's shared timeline history observer if needed."
  (let ((observer (appkit-chat-timeline-scroll-observer)))
    (unless (and (appkit-scroll-observer-p observer)
                 (appkit-scroll-observer-active-p observer))
      (appkit-chat-timeline-install-history-observer
       view
       :start-function
       (lambda (_window position _start)
         (qq-reply-check-viewport)
         (qq-chat--maybe-auto-load-older position))
       :end-function
       (lambda (window position _end)
         ;; The selected window's post-command path owns its point-based read.
         (unless (eq window (selected-window))
           (qq-chat--manage-read-position (window-point window) window))
         (qq-reply-check-viewport)
         (qq-chat--maybe-auto-load-newer position))))))

(defun qq-chat--post-command ()
  "Maintain QQ-specific timeline behavior after each command."
  (unless (appkit-chatbuf-rendering-p)
    (when (eq (window-buffer (selected-window)) (current-buffer))
      (set-window-parameter (selected-window) 'qq-chat-preview nil))
    (qq-chat--flush-deferred-node-redisplay)
    (qq-chat--manage-read-position)))

(defun qq-chat--prompt-text ()
  "Return the destination-aware prompt for the current chat buffer."
  (let* ((session (qq-chat--session))
         (avatar
          (and qq-chat-show-prompt-avatar
               session
               (qq-media-session-avatar-display-string session))))
    (concat
     (if (and (stringp avatar) (not (string-empty-p avatar)))
         (concat
          (propertize
           avatar
           'help-echo
           (format "Message destination: %s"
                   (or (alist-get 'title session)
                       (alist-get 'target-id session)
                       qq-chat--session-key)))
          " ")
       "")
     ">>> ")))

(defun qq-chat--prompt-avatar-cache-key ()
  "Return the media cache key affecting the current composer prompt."
  (and qq-chat-show-prompt-avatar
       (qq-media-session-avatar-cache-key (qq-chat--session))))

(defun qq-chat--refresh-prompt ()
  "Refresh generated prompt presentation without rebinding composer input."
  (when (and (qq-chat--composer-visible-p)
             (appkit-chatbuf-prompt-button-live-p))
    (appkit-chatbuf-prompt-update (qq-chat--prompt-text))))

(defun qq-chat--composer-visible-p ()
  "Return non-nil when the current session has a writable composer."
  (qq-state-session-sendable-p qq-chat--session-key))

(defun qq-chat--ensure-composer-visible ()
  "Signal a user error unless the current session has a composer."
  (unless (qq-chat--composer-visible-p)
    (user-error "qq: this session is read-only")))

(defun qq-chat--bind-input-region-from-footer ()
  "Bind the shared persistent tail input to canonical draft state."
  (qq-chat--assert-canonical-input-clean)
  (appkit-chatbuf-init-state 32)
  (appkit-chatbuf-bind-input-region
   :visible-p (qq-chat--composer-visible-p)
   :prompt (qq-chat--prompt-text)
   :input-text (appkit-chatbuf-input-state)
   :post-bind-function #'appkit-chatbuf-input-apply-text-properties))

(defun qq-chat--header-text ()
  "Return the empty EWOC header used by the QQ chat timeline.

Like telega, paging state belongs to the prompt delimiter rather than a
persistent prose row above the message history."
  "")

(defun qq-chat--action-indicator-text ()
  "Return telega-style peer action indicator for the current chat, or nil.

Mirrors `telega-chatbuf-footer-ins-prompt-delim' + `telega-ins--actions':
actions are shown on the footer delimiter line above the composer."
  (when (and qq-chat-show-peer-actions
             qq-chat--session-key)
    (when-let* ((text (qq-state-action-text qq-chat--session-key)))
      (propertize
       (format "(%s%s)" (or qq-chat-action-prefix ".. ") text)
       'face 'shadow))))

(defun qq-chat--history-delimiter-line ()
  "Return one full-width history/composer delimiter line."
  (appkit-chat-window-delimiter
   (qq-chat--window) (qq-chat--loading) (max 8 (qq-chat--line-fill-column))
   :loading-text "加载中…"))

(defun qq-chat--set-my-action (action)
  "Set outgoing chatbuf ACTION like telega `telega-chatbuf--set-action'.

ACTION is `typing', `cancel', or nil.  The local state is retained for
composer behavior; no wire request is sent until the native protocol exposes
a closed chat-action operation."
  (let* ((want (cond
                ((eq action 'typing) 'typing)
                ((memq action '(cancel nil)) nil)
                ((equal action "Typing") 'typing)
                ((equal action "Cancel") nil)
                (t nil)))
         (session (qq-chat--session))
         (type (and session (alist-get 'type session)))
         (target (and session (alist-get 'target-id session))))
    (unless (eq qq-chat--my-action want)
      (setq qq-chat--my-action want)
      (when (and (qq-core-supports-p 'chat-action)
                 qq-chat-send-typing
                 (eq type 'private)
                 target)
        (ignore target want)))))

(defun qq-chat--maybe-update-my-action-from-input ()
  "Advertise typing when private-chat draft is non-empty (telega parity)."
  (let ((input-p
         (not (string-empty-p
               (appkit-chatbuf-string-plain-text
                (appkit-chatbuf-input-state))))))
    (cond
     ((and (not qq-chat--my-action) input-p)
      (qq-chat--set-my-action 'typing))
     ((and qq-chat--my-action (not input-p))
      (qq-chat--set-my-action 'cancel)))))

(defun qq-chat--input-footer-context-text ()
  "Return dynamic footer text shown above the composer prompt.

The typing indicator precedes the reply card.  The shared Appkit renderer owns
the card chrome and cancel action; do not blanket-propertize it."
  (let ((action-text (qq-chat--action-indicator-text))
        (reply-context (qq-chat--reply-context-text)))
    (concat
     (qq-chat--history-delimiter-line)
     "\n"
     (if action-text
         (concat action-text "\n")
       "")
     (if (string-empty-p reply-context)
         ""
       reply-context))))

(defun qq-chat--footer-text (&optional _draft)
  "Build read-only EWOC footer text for the current chat state."
  (let ((text (qq-chat--input-footer-context-text)))
    (add-text-properties
     0 (length text)
     '(read-only t
       front-sticky (read-only)
       rear-nonsticky (read-only))
     text)
    text))

(defun qq-chat--first-unread-anchor (messages)
  "Return the stable reading-session boundary loaded in MESSAGES.
Only the Gateway's exact message identity and native sequence establish a
boundary; badge counts and unknown sequence positions never locate one."
  (let* ((session (qq-chat--session))
         (count (alist-get 'unread-message-count session))
         (exact (and (alist-get 'first-unread-message-seq session)
                     (alist-get 'first-unread-message-id session))))
    (appkit-chat-read-update
     (mapcar #'qq-chat--message-anchor messages)
     :first-unread-key exact
     :known-p (or (and exact t) (eql count 0))
     :unread-count count)))

(defun qq-chat--compute-message-render-context (previous-message message
                                                                 first-unread-anchor)
  "Project render context for MESSAGE after PREVIOUS-MESSAGE."
  ;; Validate ordinary row presentation before handing it to the generated
  ;; printer: even compact rows, which omit their sender heading, require it.
  (unless (or (qq-state-service-message-p message)
              (qq-chat--present-string (alist-get 'sender-name message)))
    (error "qq: normalized message has no sender presentation"))
  (let* ((day-key (qq-chat--message-day-key message))
         (previous-day-key
          (and previous-message (qq-chat--message-day-key previous-message)))
         (insert-date
          (and qq-chat-use-date-breaks
               previous-day-key
               day-key
               (not (equal day-key previous-day-key))
               day-key))
         (anchor (qq-chat--message-anchor message))
         (insert-unread
          (and first-unread-anchor
               anchor
               (equal anchor first-unread-anchor))))
    (list :compact
          (and previous-message
               (not insert-date)
               (not insert-unread)
               (qq-chat--messages-compact-group-p previous-message message)
               t)
          :insert-date insert-date
          :insert-unread insert-unread)))

(defun qq-chat--message-media-cache-keys (message)
  "Return media cache keys that can affect MESSAGE rendering."
  (let (keys)
    (when-let* ((avatar-key
                 (qq-media-message-avatar-cache-key message)))
      (push avatar-key keys))
    (dolist (part (alist-get 'parts
                             (qq-state-gray-tip-message-data message)))
      (when (equal (alist-get 'type part) "user")
        (when-let* ((user-id (alist-get 'user-id part)))
          (push (format "avatar:%s" user-id) keys))))
    (dolist (segment (alist-get 'segments message))
      (setq keys (nconc (qq-media-segment-cache-keys segment) keys)))
    (delete-dups (delq nil keys))))

(defun qq-chat--message-dependency-keys (message &optional reply-sequence-index)
  "Return opaque resource keys that can change rendered MESSAGE.
REPLY-SEQUENCE-INDEX resolves reply targets within one projection snapshot."
  (let (dependencies)
    (when-let* ((reply-id
                 (qq-chat--message-reply-id message reply-sequence-index)))
      (push (list :message reply-id) dependencies))
    (when-let* ((key (qq-reply-key qq-chat--session-key (qq-reply-data message))))
      (push key dependencies))
    (dolist (media-key (qq-chat--message-media-cache-keys message))
      (push (list :media media-key) dependencies))
    (delete-dups dependencies)))

(defun qq-chat--message-visible-in-timeline-p (message)
  "Return non-nil when MESSAGE belongs in the projected timeline."
  (or qq-chat-show-recalled-messages
      (not (qq-state-message-recalled-p message))))

(defun qq-chat--timeline-messages ()
  "Return visible rows from the active search or this chat's timeline.
Timeline rows come in order, followed by sends not yet confirmed."
  (seq-filter
   #'qq-chat--message-visible-in-timeline-p
   (let ((model (qq-chat--model)))
     (cond
      (qq-search--inline-p (qq-search--inline-messages))
      ((not (plist-get model :timeline)) nil)
      ((eq (plist-get model :timeline) 'dataline)
       (qq-state-session-messages qq-chat--session-key))
      (t
       (let ((messages (qq-state-session-messages qq-chat--session-key))
             (by-anchor (make-hash-table :test #'equal))
             (rows (make-hash-table :test #'equal)))
         (dolist (message messages)
           (puthash (qq-chat--message-anchor message) message by-anchor))
         (dolist (row (plist-get model :rows))
           (puthash (cdr row) t rows))
         (let* ((timeline (delq nil (mapcar (lambda (row) (gethash (cdr row) by-anchor))
                                            (plist-get model :rows))))
                (newest (or (alist-get 'time (car (last timeline))) 0))
                (live (not (plist-get model :has-newer))))
           (append timeline
                   ;; Sends whose Gateway row has not arrived yet.  A confirmed
                   ;; send keeps its local id; it waits here only while it is
                   ;; newer than the window, never as an old row after latest.
                   (seq-filter (lambda (message)
                                 (and (alist-get 'local-id message)
                                      (not (gethash (qq-chat--message-anchor message) rows))
                                      (or (memq (alist-get 'status message) '(pending failed))
                                          (and live (>= (or (alist-get 'time message) 0)
                                                        newest)))))
                               messages)))))))))

(defun qq-chat--project-timeline (messages)
  "Project visible QQ MESSAGES into shared timeline rows."
  (if (null messages)
      (let* ((state (if qq-search--inline-p
                        (cond (qq-search--error 'search-error)
                              (qq-search--loading 'searching)
                              (t 'no-match))
                      'normal))
             (_boundary (unless qq-search--inline-p (qq-chat--first-unread-anchor nil)))
             (placeholder (list :kind qq-chat--empty-placeholder
                                :state state)))
        (list (appkit-chat-timeline-row-create
               :key qq-chat--empty-placeholder
               :payload placeholder
               :context (list :state state))))
    (let ((first-unread (unless qq-search--inline-p
                          (qq-chat--first-unread-anchor messages)))
          (reply-sequence-index (make-hash-table :test #'equal))
          (sequence-field
           (if (and qq-chat--session-key
                    (eq (qq-state-session-key-type qq-chat--session-key)
                        'private))
               'native-client-sequence
             'message-seq)))
      ;; Preserve all candidates: private client sequences can collide.
      (when (and qq-chat--session-key (seq-some #'qq-reply-data messages))
        (dolist (source (qq-state-session-messages qq-chat--session-key))
          (when-let* ((sequence (alist-get sequence-field source)))
            (puthash sequence (cons source (gethash sequence reply-sequence-index))
                     reply-sequence-index))))
      (appkit-chat-timeline-project
       messages
       #'qq-chat--message-anchor
       :context-function
       (lambda (previous message)
         (append (qq-chat--compute-message-render-context
                  previous message first-unread)
                 (when-let* ((data (qq-reply-data message)))
                   (let* ((sequence (or (alist-get 'message_seq data)
                                        (alist-get 'sequence data)))
                          (source
                           (or (qq-reply-find-source
                                data
                                (if sequence (gethash sequence reply-sequence-index)
                                  (qq-state-session-messages qq-chat--session-key))
                                (qq-state-session-key-type qq-chat--session-key))
                               (qq-reply-resource-source qq-chat--session-key data))))
                     ;; The EWOC printer also runs on media-only redraws.
                     ;; Capture quote presentation once instead of copying the
                     ;; entire conversation for every row it prints.
                     (list :reply-preview (qq-reply-preview-model data source)
                           :reply-center (qq-reply-locator
                                          qq-chat--session-key data
                                          (and source (list source))))))
                 (when qq-search--inline-p
                   (list :search-query qq-search--query))))
       :dependencies-function
       (lambda (message)
         (qq-chat--message-dependency-keys message reply-sequence-index))))))

(defun qq-chat--ensure-timeline ()
  "Ensure current QQ chat owns one shared projected timeline."
  (qq-chat--ensure-view)
  ;; Killing a view retires its timeline controller but intentionally leaves
  ;; the reusable buffer and composer state intact.  Before a replacement
  ;; controller creates a fresh EWOC at `point-max', remove the old generated
  ;; prompt/tail; otherwise that new footer would be born after the composer.
  ;; Canonical input is buffer-local and has already been synchronized by user
  ;; edits (or restored by the failed-send barrier), so this loses no draft.
  (when (and (not (appkit-chat-timeline-live-p))
             (or (appkit-chatbuf-prompt-start-position)
                 (appkit-chatbuf-input-start-position)))
    (appkit-chatbuf-with-generated-update
      (appkit-chatbuf-clear-prompt-and-input)))
  (appkit-chat-timeline-ensure
   :printer #'qq-chat--row-printer
   :anchor-property 'qq-chat-message-anchor
   :header (qq-chat--header-text)
   :footer (qq-chat--footer-text)
   :after-mutation-function #'appkit-chatbuf-update-context-mode)
  (qq-chat--install-scroll-observer (appkit-current-surface))
  (appkit-chat-timeline-ewoc))

(defun qq-chat--view-id ()
  "Return the opaque appkit view id for the current QQ chat."
  (unless qq-chat--session-key
    (error "qq: chat buffer has no session key"))
  (list (if qq-chat--preview-p 'chat-preview 'chat) qq-chat--session-key))

(defun qq-chat--live-current-view ()
  "Return this buffer's live canonical chat view, or nil when detached."
  (let ((view (appkit-current-surface)))
    (and qq-chat--session-key
         (derived-mode-p 'qq-chat-mode)
         (appkit-surface-live-p view)
         (eq (appkit-surface-buffer view) (current-buffer))
         (equal (appkit-surface-identity view) (qq-chat--view-id))
         (equal (plist-get (appkit-surface-model view) :state) qq-chat--session-key)
         view)))

(defun qq-chat--captured-view-current-p (view)
  "Return non-nil when VIEW is this buffer's live canonical chat view."
  (and view (eq view (qq-chat--live-current-view))))

(defun qq-chat--request-callback-sync (view &optional action)
  "Project accepted callback state through captured VIEW, then run ACTION.
The send renders synchronously, so ACTION sees the projection and runs outside
the Surface pass where it may send again.  A replacement or detached view is
inert."
  (when (and (qq-chat--captured-view-current-p view)
             (or (null action) (functionp action)))
    (qq-runtime-invalidate view :full-p t :frame-p t :position (and action 'preserve))
    (when (and action (qq-chat--captured-view-current-p view))
      (funcall action))))

(defun qq-chat--apply-state-event (event)
  "Project committed QQ state EVENT inside the Surface render boundary."
  (let ((event-session-key (plist-get event :session-key))
        (event-type (plist-get event :type))
        (event-mutation (plist-get event :mutation)))
    (pcase event-type
      ('message
       (when (equal event-session-key qq-chat--session-key)
         (qq-chat--apply-message-state-change event)))
      ('history
       (when (equal event-session-key qq-chat--session-key)
         (qq-chat--header-line-update)
         (qq-chat--update-frame)
         (qq-chat--sync-timeline)))
      ('group-members-refreshed
       (when (equal event-session-key qq-chat--session-key)
         (qq-chat--sync-timeline
          :force-keys (and (appkit-chat-timeline-live-p)
                           (appkit-chat-timeline-keys)))))
      ('connection
       (qq-chat--header-line-update))
      ('reset
       (qq-chat-render))
      ('session
       (when (equal event-session-key qq-chat--session-key)
         (if (eq event-mutation 'read)
             (qq-chat--apply-read-state-change)
           (qq-chat--header-line-update)
           (qq-chat--update-frame)
           (qq-chat--refresh-prompt))))
      ('action
       (when (equal event-session-key qq-chat--session-key)
         (qq-chat--update-frame)))
      ('sessions-refreshed
       (qq-chat--header-line-update)
       (qq-chat--refresh-prompt))
      ((or 'friends-refreshed 'groups-refreshed)
       (qq-chat--header-line-update)
       (qq-chat--refresh-prompt)))))

(defun qq-chat--render (surface model change)
  "Render SURFACE's committed MODEL using native projection CHANGE."
  (let* ((view surface)
         (events (plist-get model :events))
         (geometry-p (appkit-projection-change-geometry-p change))
         (composer-p (and (null events)
                          (appkit-projection-change-frame-p change)))
         (force-keys
          (delete-dups
           (append (copy-sequence (appkit-projection-change-keys change))
                   (and geometry-p (appkit-chat-timeline-live-p)
                        (appkit-chat-timeline-keys)))))
         (changed-resources (appkit-projection-change-resources change))
         (rekeys (appkit-projection-change-rekeys change))
         (raw-forward-sync-request qq-chat--forward-sync-request)
         (forward-sync-request
          (and (or (null (plist-get raw-forward-sync-request :view))
                   (eq view (plist-get raw-forward-sync-request :view)))
               raw-forward-sync-request))
         (raw-send-sync-request qq-chat--send-sync-request)
         (send-sync-request raw-send-sync-request))
    (when geometry-p
      (when-let* ((next
                   (appkit-surface-responsive-width
                    surface qq-chat-auto-fill-margin-columns)))
        (setq-local fill-column next)))
    (dolist (event events)
      (when (appkit-surface-live-p view)
        (qq-chat--apply-state-event event)))
    (when (appkit-surface-live-p view)
      (let (rendered-p)
        (cond
         (geometry-p
          (qq-chat--sync-timeline
           :force-keys force-keys
           :changed-resources changed-resources
           :rekeys rekeys)
          (qq-chat--update-frame))
         (forward-sync-request
          (when (or force-keys changed-resources rekeys)
            (qq-chat--sync-timeline
             :force-keys force-keys
             :changed-resources changed-resources
             :rekeys rekeys))
          (qq-chat--update-frame))
         (send-sync-request
          (qq-chat--update-frame))
         ((and changed-resources
               (not rekeys)
               (not (appkit-projection-change-full-p change))
               (cl-every (lambda (resource) (eq (car-safe resource) :media))
                         changed-resources)
               (appkit-chat-timeline-live-p))
          ;; Media completion changes presentation, not message order, payload,
          ;; grouping, or dependencies.  Retain the projection and every
          ;; unrelated row (including its running inline player).
          (qq-chat--request-row-redisplay
           (append force-keys
                   (appkit-chat-timeline-dependent-keys changed-resources)))
          (when composer-p
            (qq-chat--update-frame)))
         ((and (null events)
               (or (appkit-projection-change-full-p change)
                   (appkit-projection-change-frame-p change)
                   (appkit-projection-change-position change)))
          (qq-chat-render)
          (setq rendered-p t))
         ((or force-keys changed-resources rekeys)
          (qq-chat--sync-timeline
           :force-keys force-keys
           :changed-resources changed-resources
           :rekeys rekeys)))
        (when (and rendered-p (or force-keys changed-resources rekeys))
          (qq-chat--sync-timeline
           :force-keys force-keys
           :changed-resources changed-resources
           :rekeys rekeys))
        (when composer-p
          (qq-chat--refresh-prompt))
        (when send-sync-request
          (qq-chat--materialize-pending-send-restoration))
        (when forward-sync-request
          (qq-chat--header-line-update))
        (when (eq raw-forward-sync-request qq-chat--forward-sync-request)
          (setq qq-chat--forward-sync-request nil))
        (when (eq raw-send-sync-request qq-chat--send-sync-request)
          (setq qq-chat--send-sync-request nil))
        (when send-sync-request
          (appkit-chatbuf-focus-input))
        (when-let* ((presentation (plist-get model :history-presentation)))
          (qq-chat--history-present presentation))
        (when-let* ((observer (appkit-chat-timeline-scroll-observer)))
          (appkit-scroll-observer-check observer)))))
  (let ((messages (qq-chat--timeline-messages)))
    (qq-media-render-result
     surface messages (qq-reply-render-result surface messages qq-chat--session-key))))

(defun qq-chat--recover-render (surface _model _condition)
  "Rebuild presentation without rereading damaged text or replaying events."
  (appkit-chat-timeline-reset)
  (qq-chat-render)
  (qq-reply-render-result surface (qq-chat--timeline-messages) qq-chat--session-key))

(defun qq-chat--operation-update (context model message)
  "Present an operation reply belonging to this chat, or return nil."
  (pcase message
    (`(appkit-operation-completed forward ,request ,_receipt ,reason)
     (if (not (eq request (plist-get qq-chat--forward-presentation :request)))
         (appkit-next :model model :render appkit-render-none)
       (let* ((entry (qq-runtime-operation (list 'send-forward (nth 1 request))))
              (style (nth 4 request))
              (count (if (eq style 'merged)
                         (if reason 0 (length (nth 3 request)))
                       (plist-get entry :completed)))
              (removed (qq-chat--consume-forward-memberships
                        (seq-take (plist-get qq-chat--forward-presentation :memberships) count)))
              (view (plist-get qq-chat--forward-presentation :view)))
         (setq qq-chat--forward-presentation nil
               qq-chat--forward-sync-request (list :kind 'forward-settlement :view view))
         (if reason
             (message "qq: 转发已完成 %d/%d：%s" count (length (nth 3 request)) reason)
           (setq qq-chat--last-forward-target-key (nth 2 request))
           (message "qq: 已%s %d 条消息到 %s" (qq-chat--forward-style-label style) count (nth 2 request)))
         (appkit-next :model model
                      :render (appkit-projection-change-create :frame-p t :keys removed)))))
    (`(appkit-operation-completed read-all ,request ,receipt ,reason)
     (cond
      ((not (equal (cadr request) qq-chat--session-key))
       (appkit-next :model model :render appkit-render-none))
      (reason
       (message "qq: %s" reason)
       (appkit-next :model model :render appkit-render-none))
      (t
       (appkit-chat-read-clear)
       (setq qq-chat--pending-jump-center nil)
       (message "qq: conversation read through %s" (alist-get 'read_sequence receipt))
       (if (and (plist-get model :timeline) (not (plist-get model :has-newer)))
           (let ((presentation '(:kind latest)))
             (appkit-next
              :model (qq-chat--with model :history-presentation presentation)
              :render (appkit-projection-change-create :full-p t :frame-p t :position 'preserve)
              :commands
              (list (appkit-command-post-message
                     :target (appkit-transition-context-owner-address context)
                     :message (list 'qq-history-presented presentation)
                     :delivery 'report))))
         (qq-chat-timeline-update context model '(qq-timeline-start (latest)))))))
    (_ (and (featurep 'qq-search)
            (qq-search--operation-update context model message)))))

(defun qq-chat--surface-update (context model message)
  "Commit chat history and state MESSAGE for this Surface."
  (or
   (pcase message
     ((pred appkit-delivery-report-p)
      (when (eq (appkit-delivery-report-correlation message) 'qq-timeline)
        (setq qq-chat--pending-jump-center nil)
        (message "qq: history request failed: %s" (appkit-delivery-report-outcome message))
        (qq-chat-timeline-update context model '(qq-timeline-cancel))))
     ((pred (lambda (value)
              (memq (car-safe value)
                    '(qq-timeline-start qq-timeline-loaded qq-timeline-failed
                      qq-timeline-changed qq-timeline-page qq-timeline-paged
                      qq-timeline-cancel qq-history-result qq-history-presented))))
      (qq-chat-timeline-update context model message))
     (`(qq-history-changed ,session-key)
      (appkit-next
       :model model :render
       (if (equal session-key (plist-get model :state))
           (appkit-projection-change-create :full-p t :frame-p t)
         appkit-render-none)))
     (`(qq-state-event ,event)
      (when (eq (plist-get event :type) 'reset)
        (setq model (copy-sequence model))
        (dolist (key '(:timeline :rows :has-older :has-newer :older-cursor
                      :opening :paging :history-presentation))
          (setq model (plist-put model key nil))))
      (appkit-next
       :model (plist-put (copy-sequence model) :events
                        (append (plist-get model :events) (list event)))
       :render (appkit-projection-change-create :frame-p t)
       :commands
       (list
        (appkit-command-post-message
         :target (appkit-transition-context-owner-address context)
         :message (list 'qq-events-rendered (list event))
         :delivery 'report))))
     (`(qq-events-rendered ,events)
      (let ((pending (plist-get model :events)))
        (appkit-next
         :model (if (equal events (seq-take pending (length events)))
                    (plist-put (copy-sequence model) :events
                               (nthcdr (length events) pending))
                  model)
         :render appkit-render-none))))
   (qq-chat--operation-update context model message)
   (qq-runtime--surface-update context model message)))

(defun qq-chat--ensure-view ()
  "Return the live Appkit view owning the current QQ chat buffer."
  (let ((owner
         (or (qq-runtime-current-account-id)
             (user-error "qq: chat buffer has no account owner"))))
    (qq-runtime-bind-account owner)
    (let ((view
           (qq-runtime-ensure-account-surface
            :id (qq-chat--view-id)
            :mode 'qq-chat-mode
            :update #'qq-chat--surface-update
            :state qq-chat--session-key
            :render-function #'qq-chat--render
            :recover-function #'qq-chat--recover-render
            :setup #'qq-timeline-bind-surface)))
      (appkit-surface-enable-responsive-geometry
       view
       (lambda (surface _width)
         (qq-runtime-invalidate surface :geometry-p t)))
      view)))

(defun qq-chat--header-line-update ()
  "Update chat header line and buffer name."
  (qq-chat--ensure-buffer-name)
  (qq-chat--prune-message-selection)
  (setq-local header-line-format (qq-chat--header-line))
  (force-mode-line-update))

(defun qq-chat--update-frame ()
  "Synchronize QQ chat header, footer, prompt, and canonical composer."
  (qq-chat--ensure-timeline)
  (appkit-chat-timeline-set-frame
   (qq-chat--header-text)
   (qq-chat--footer-text)
   :bind-input-function #'qq-chat--bind-input-region-from-footer
   :composer-visible-p (qq-chat--composer-visible-p)))

(cl-defun qq-chat--sync-timeline
    (&key (messages nil messages-p) force-keys changed-resources rekeys)
  "Synchronize QQ rows through the shared projected timeline controller."
  (qq-chat--ensure-timeline)
  (when (and qq-chat--translation-sources
             (not (eq qq-chat--translation-owner (qq-chat--live-current-view))))
    ;; The buffer's retained rows outlive a stopped translation owner.
    (maphash (lambda (key _source) (push (nth 3 key) force-keys))
             qq-chat--translation-sources)
    (setq qq-chat--translation-owner nil
          qq-chat--translation-sources nil))
  (qq-chat--translation-prune
   (and qq-chat--translation-sources
        (qq-state-session-messages qq-chat--session-key)))
  (prog1
      (appkit-chat-timeline-sync
       (qq-chat--project-timeline
        (if messages-p messages (qq-chat--timeline-messages)))
       :force-keys force-keys
       :changed-resources changed-resources
       :rekeys rekeys)
    (run-hooks 'qq-chat-after-render-hook)))

(defun qq-chat--message-affects-composer-context-p (message-id)
  "Return non-nil when MESSAGE-ID is the active reply target."
  (equal message-id (appkit-chatbuf-aux-message-id)))

(defun qq-chat--refresh-composer-context-for-message (message-id)
  "Refresh the active reply composer context after MESSAGE-ID changes."
  (when (qq-chat--message-affects-composer-context-p message-id)
    (when-let* ((message (qq-chat--message-by-server-id message-id)))
      (let ((state (copy-sequence (appkit-chatbuf-aux-state))))
        (setq state (plist-put state :aux-msg message))
        (appkit-chatbuf-aux-set state)))
    (qq-chat--update-frame)))

(defun qq-chat--request-row-redisplay (anchors)
  "Redisplay projected ANCHORS, deferring while a region is active."
  (when (and anchors (appkit-chat-timeline-live-p))
    (let ((result (appkit-chat-timeline-invalidate
                   anchors :defer-while-mark-active t)))
      (when (eq result t)
        (run-hooks 'qq-chat-after-render-hook))
      result)))

(defun qq-chat--render-empty-placeholder (state)
  "Insert the empty timeline placeholder row for STATE."
  (let ((start (point)))
    (appkit-presentation-insert-note-line
     (pcase state
       ('searching "Searching messages…")
       ('search-error "Search failed; refresh to retry.")
       ('no-match "No matching messages.")
       ('normal "No messages loaded yet.")
       (_ (error "qq: invalid empty timeline state %S" state))))
    (add-text-properties
     start (point)
     '(read-only t
       front-sticky (read-only)
       rear-nonsticky (read-only)
       qq-chat-internal empty-placeholder))))

(defun qq-chat--row-printer (row)
  "EWOC pretty-printer for one projected QQ chat ROW."
  (let ((message (appkit-chat-timeline-row-payload row))
        (context (appkit-chat-timeline-row-context row)))
    (cond
     ((and (listp message)
           (eq (plist-get message :kind) qq-chat--empty-placeholder))
      (qq-chat--render-empty-placeholder
       (or (plist-get context :state)
           (error "qq: empty timeline row lacks state context"))))
     (t
      (let ((start (point)))
        (qq-chat--render-message message context)
        (when qq-search--inline-p
          (qq-search--highlight-region start (point))))))))

(defun qq-chat--render-canonical-input ()
  "Replace the live composer from canonical input state explicitly."
  (qq-chat--assert-canonical-input-clean)
  (appkit-chatbuf-with-generated-update
    (appkit-chatbuf-input-replace (appkit-chatbuf-input-state))
    (appkit-chatbuf-input-apply-text-properties)))

(defun qq-chat--materialize-pending-send-restoration ()
  "Materialize a restored canonical draft before live-tail synchronization.

Return non-nil when a pending failed-send restoration was consumed.  Clearing
the marker happens only after the generated update succeeds, so a failed view
transaction can retry without losing structured input properties."
  (when-let* ((request qq-chat--send-sync-request))
    (qq-chat--render-canonical-input)
    (when (eq request qq-chat--send-sync-request)
      (setq qq-chat--send-sync-request nil))
    t))

(defun qq-chat--set-draft (text)
  "Set canonical draft TEXT and update the shared tail composer."
  (setq qq-chat--send-sync-request nil)
  (appkit-chatbuf-input-state-set text :reset-history-p t)
  (qq-chat--prune-clipboard-files)
  (qq-chat--render-canonical-input)
  (appkit-chatbuf-focus-input))

(defun qq-chat--push-input-history (text)
  "Insert TEXT into input history when appropriate."
  (unless (appkit-chatbuf-input-has-objects-p)
    (appkit-chatbuf-input-history-push text)))

(defun qq-chat--guess-file-segment-type (path)
  "Return best QQ send segment type for local PATH."
  (let ((ext (downcase (or (file-name-extension path) ""))))
    (cond
     ((member ext '("png" "jpg" "jpeg" "gif" "webp" "bmp" "svg")) "image")
     ((member ext '("mp4" "mov" "mkv" "webm" "avi" "flv")) "video")
     ((member ext '("amr" "mp3" "wav" "ogg" "m4a" "aac" "flac" "opus")) "record")
     (t "file"))))

(defun qq-chat--segment-object-label (segment)
  "Return visible input label for structured SEGMENT.

Face segments use the same inline image (or `/名称') as the timeline.
Favorite drafts remain durable identity objects until send-time materialization."
  (let* ((type (or (alist-get 'type segment) "segment"))
         (data (alist-get 'data segment)))
    (pcase type
      ("at"
       (let ((target (alist-get 'qq data))
             (name (alist-get 'name data)))
         (concat "@" (or name
                         (and (equal target "all") "全体成员")
                         target
                         "mention"))))
      ("face"
       (let ((id (or (alist-get 'id data) "?")))
         (qq-media-face-display-string id)))
      ("favorite_emoji" "[收藏表情]")
      ("mface"
       (let* ((summary (or (alist-get 'summary data) "[商城表情]"))
              (file (or (alist-get 'file data)
                        (alist-get 'path data)))
              (image (and file
                          (appkit-media-file-present-p file)
                          (qq-media--image-from-file
                           file
                           (max qq-media-face-image-height 32)))))
         (appkit-media-image-display-string image (format "[mface:%s]" summary))))
      ("image"
       (let* ((summary (alist-get 'summary data))
              (sub (alist-get 'sub_type data))
              (file (or (alist-get 'file data)
                        (alist-get 'path data)))
              (sticker-p (member sub '(1 "1")))
              (image (and file
                          (appkit-media-file-present-p file)
                          (if sticker-p
                              (qq-media--image-from-file
                               file (max qq-media-face-image-height 32))
                            (qq-media--one-line-preview-image-from-file file))))
              (name (or (alist-get 'name data)
                        (and file (file-name-nondirectory file))
                        summary
                        "image"))
              (size (and file
                         (file-exists-p file)
                         (file-size-human-readable
                          (file-attribute-size (file-attributes file)))))
              (preview
               (and image
                    (appkit-media-one-line-image-display-string image "▧"))))
         (cond
          (sticker-p
           (appkit-media-image-display-string
            image
            (or summary
                name
                "[image]")))
          ((or preview file)
           (concat "[image] "
                   (if preview (concat preview " ") "")
                   (propertize name 'help-echo file)
                   (if size (format " (%s)" size) "")))
          (t
           (format "[image:%s]" (or summary "item"))))))
      (_
       (let* ((path (or (alist-get 'path data)
                        (alist-get 'file data)
                        (alist-get 'name data)
                        "item"))
              (name (file-name-nondirectory (format "%s" path))))
         (format "[%s:%s]" type name))))))

(defun qq-chat--segment-input-object (segment)
  "Return chatbuf input object plist for outbound SEGMENT."
  (list :kind 'qq-segment
        :segment (copy-tree segment)
        :label (qq-chat--segment-object-label segment)))

(defun qq-chat--input-object-segment (object)
  "Return outbound QQ segment represented by chatbuf OBJECT, or nil."
  (when (eq (plist-get object :kind) 'qq-segment)
    (copy-tree (plist-get object :segment))))

(defun qq-chat--insert-input-segment-object (segment &optional visible-label)
  "Insert outbound QQ SEGMENT into the composer as one object.

VISIBLE-LABEL overrides the segment-derived label, for example to retain the
catalog label while a favorite draft stores only its durable identity."
  ;; Structured insertion synchronizes canonical input explicitly after the
  ;; generated object mutation; discard only stale pending presentation.
  (setq qq-chat--send-sync-request nil)
  (qq-chat--ensure-composer-visible)
  (unless (appkit-chatbuf-point-in-input-p)
    (appkit-chatbuf-focus-input))
  ;; Appkit preserves the exact object start as a boundary-before position,
  ;; but an unexpected point inside an intangible object belongs after it.
  ;; Normalize before adding a leading separator so we never edit the old
  ;; object's protected interior on the way to inserting the new one.
  (when-let* ((bounds (appkit-chatbuf-input-object-bounds-at-point)))
    (unless (= (point) (car bounds))
      (goto-char (cdr bounds))))
  (when (and (appkit-chatbuf-point-in-input-p)
             (> (point) (or (appkit-chatbuf-input-start-position) (point-min)))
             (let ((ch (char-before)))
               (and ch (not (memq ch '(32 9 10))))))
    (insert " "))
  (let* ((object (qq-chat--segment-input-object segment))
         (object (if visible-label
                     (plist-put object :label visible-label)
                   object))
         (label (plist-get object :label)))
    (appkit-chatbuf-input-insert label :object object)
    (appkit-chatbuf-input-apply-text-properties)))

(defun qq-chat-attach-file (path &optional segment-type)
  "Insert local PATH into the chat input as a structured segment object.

When SEGMENT-TYPE is nil, infer the most useful QQ segment type from PATH.
An interactive universal prefix stages a generic file instead of media."
  (interactive
   (list (appkit-media-read-file-name "Attach media/file: ")
         (when current-prefix-arg "file")))
  (unless (file-regular-p path)
    (user-error "qq: attachment is not a regular file: %s" path))
  (unless (file-readable-p path)
    (user-error "qq: file is not readable: %s" path))
  (let* ((type (or segment-type (qq-chat--guess-file-segment-type path)))
         (segment `((type . ,type)
                    (data . ((file . ,path)
                             (name . ,(file-name-nondirectory path)))))))
    (qq-chat--insert-input-segment-object segment)
    (qq-chat--sync-draft-from-buffer)
    (message "qq: attached %s as %s"
             (file-name-nondirectory path)
             type)))

(defun qq-chat-attach-image (path)
  "Prompt for PATH and attach it explicitly as a QQ image."
  (interactive (list (appkit-media-read-file-name "Attach image: ")))
  (qq-chat-attach-file path "image"))

(defun qq-chat-attach-video (path)
  "Prompt for PATH and attach it explicitly as a QQ video."
  (interactive (list (appkit-media-read-file-name "Attach video: ")))
  (qq-chat-attach-file path "video"))

(defun qq-chat-attach-document (path)
  "Prompt for PATH and attach it explicitly as a generic QQ file."
  (interactive (list (appkit-media-read-file-name "Attach file: ")))
  (qq-chat-attach-file path "file"))

(defun qq-chat-attach (attach-type)
  "Choose ATTACH-TYPE with completion and invoke its attachment command.

`\\[universal-argument]' remains visible to the selected command.  For
example, the clipboard command uses it to force image data to be attached as
a generic file.  See `qq-chat-attach-commands' for the extensible command
table."
  (interactive
   (list
    (completing-read
     "Attachment type: "
     (mapcar #'car qq-chat-attach-commands)
     nil t)))
  (let ((command (cadr (assoc attach-type qq-chat-attach-commands))))
    (cl-assert (commandp command))
    (call-interactively command)))

(defun qq-chat--read-base-face-id (&optional prompt)
  "Prompt for and return one QQ base face id.

PROMPT defaults to \"QQ face: \".  Completion uses the existing QQ face
panel ordering, names, and local image affixation."
  (qq-completion-read-base-face-id prompt))

(defun qq-chat-attach-face (&optional face-id)
  "Insert a small QQ system face into the chat composer.

With FACE-ID (string or number), insert that face directly.  Interactively,
prompt with completing-read over face names (`/斜眼笑') and ids, each
prefixed with the local face PNG when available.

Candidates stay in QQ face-id order (0, 1, 2, …) via completion
metadata (`display-sort-function' = identity), so Vertico/Icomplete
match QQ's base emoji panel order instead of history/length sort.

Sends as a structured small `face' segment.  Use
`qq-chat-attach-large-face' for the large rendition of a supported face.
Bound via `qq-chat-attach-emoji' (`C-c C-e'); `qq-chat-attach-transient' `E'."
  (interactive
   (list (qq-chat--read-base-face-id)))
  (let* ((id (format "%s" (or face-id
                              (user-error "qq: face id required"))))
         (segment (qq-media-face-to-segment id)))
    (qq-chat--insert-input-segment-object segment)
    (qq-chat--sync-draft-from-buffer)
    (message "qq: face %s (%s)"
             (or (qq-media-face-name id) id)
             id)))

(defun qq-chat-attach-large-face (&optional face-id)
  "Insert a large QQ system face into this chat's draft, without sending it.
With FACE-ID, select that supported face directly.  Otherwise open the
account's large-face picker; the catalog and previews load asynchronously.
Send the draft normally after selection.  Ordinary face commands stay small."
  (interactive)
  (unless (derived-mode-p 'qq-chat-mode)
    (user-error "qq: open a chat before inserting a large face"))
  (let* ((buffer (current-buffer))
         (view (qq-chat--ensure-view))
         (id (or face-id (qq-completion-read-large-face-id))))
    (unless (and (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (qq-chat--captured-view-current-p view)))
      (user-error "qq: large-face picker belongs to a closed or replaced chat"))
    (with-current-buffer buffer
      (qq-chat--insert-input-segment-object (qq-media-large-face-to-segment id))
      (qq-chat--sync-draft-from-buffer))))

(defun qq-chat--insert-custom-face (face)
  "Insert favorite FACE into the composer by durable native identity."
  (let ((segment (qq-media-custom-face-to-segment face)))
    (qq-chat--insert-input-segment-object
     segment (qq-media-custom-face-display-string face))
    (qq-chat--sync-draft-from-buffer)
    (message "qq: favorite %s" (qq-media-custom-face-label face))))

(defun qq-chat-attach-custom-face (&optional force-refresh)
  "Insert a favorite custom face (收藏表情) into the chat composer.

The native catalog contributes only a durable favorite identity to the draft.
At send time Gateway materializes verified bytes and the existing image
attachment pipeline prepares them with image subtype 1.

Open the picker immediately; the shared `/fav' catalog and image previews
populate it asynchronously.  `C-g' cancels selection without opening another
picker when a pending request completes.
With prefix FORCE-REFRESH, bypass Gateway's catalog cache.
Bound via `C-u C-c C-e' or `qq-chat-attach-transient' `F'."
  (interactive "P")
  (let* ((buffer (current-buffer))
         (view (qq-chat--ensure-view))
         (face (qq-completion-read-custom-face force-refresh)))
    (unless (and (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (qq-chat--captured-view-current-p view)))
      (user-error "qq: favorite picker belongs to a closed or replaced chat"))
    (with-current-buffer buffer
      (qq-chat--insert-custom-face face))))

(defun qq-chat--favorite-view ()
  "Capture the live chat owning an interactive favorite operation."
  (unless (derived-mode-p 'qq-chat-mode)
    (user-error "qq: open a chat before managing favorite emojis"))
  (qq-favorite-emoji--current-owner)
  (qq-chat--ensure-view))

(defun qq-chat--call-with-favorite-view (view function)
  "Run FUNCTION in VIEW's account after a favorite picker or prompt."
  (let ((buffer (appkit-surface-buffer view)))
    (unless (and (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (qq-chat--captured-view-current-p view)))
      (user-error "qq: favorite operation belongs to a closed or replaced chat"))
    (with-current-buffer buffer
      (qq-runtime-with-account (appkit-app-identity (appkit-surface-app view))
        (funcall function)))))

(defun qq-chat-add-favorite-emoji (&optional path)
  "Add local image PATH to this account's QQ favorites, without sending it."
  (interactive)
  (let* ((view (qq-chat--favorite-view))
         (path (or path (appkit-media-read-file-name "Add favorite image: "))))
    (qq-chat--call-with-favorite-view
     view
     (lambda ()
       (qq-favorite-emoji-add-file
        path
        (lambda (receipt)
          (message "qq: added %s"
                   (qq-media-custom-face-label (alist-get 'entry receipt))))
        #'qq-chat--default-error)))))

(defun qq-chat-delete-favorite-emoji ()
  "Choose and delete one favorite emoji from QQ, after confirmation."
  (interactive)
  (let* ((view (qq-chat--favorite-view))
         (face (qq-completion-read-custom-face))
         (id (qq-media-custom-face-id face)))
    (when (yes-or-no-p
           (format "Delete %s from QQ favorites? "
                   (qq-media-custom-face-label face)))
      (qq-chat--call-with-favorite-view
       view
       (lambda ()
         (qq-favorite-emoji-delete
          (list id)
          (lambda (receipt)
            (let ((entry (car (alist-get 'entries receipt))))
              (cond
               ((member id (alist-get 'unreported_favorite_emoji_ids receipt))
                (message "qq: QQ did not confirm deletion; refresh favorites to check"))
               ((zerop (alist-get 'status entry))
                (message "qq: favorite emoji deleted"))
               (t
                (message "qq: QQ rejected favorite deletion (status %s)"
                         (alist-get 'status entry))))))
          #'qq-chat--default-error))))))

(defun qq-chat-describe-favorite-emoji ()
  "Choose a favorite emoji and edit its QQ annotation.
An empty annotation clears the existing description."
  (interactive)
  (let* ((view (qq-chat--favorite-view))
         (face (qq-completion-read-custom-face))
         (description (read-string "Favorite annotation (empty clears): "
                                   (alist-get 'description face))))
    (qq-chat--call-with-favorite-view
     view
     (lambda ()
       (qq-favorite-emoji-set-description
        (qq-media-custom-face-id face) description
        (lambda (_receipt) (message "qq: favorite annotation updated"))
        #'qq-chat--default-error)))))

(defun qq-chat-move-favorite-emoji ()
  "Choose a favorite emoji and move it to a one-based catalog position."
  (interactive)
  (let* ((view (qq-chat--favorite-view))
         (face (qq-completion-read-custom-face))
         (position (read-number "Favorite position (1 = first): " 1)))
    (qq-chat--call-with-favorite-view
     view
     (lambda ()
       (qq-favorite-emoji-move
        (qq-media-custom-face-id face) position
        (lambda (_catalog) (message "qq: favorite emoji order updated"))
        #'qq-chat--default-error)))))

(defun qq-chat-refresh-favorite-emojis ()
  "Refresh QQ favorites from the network, including annotations and order."
  (interactive)
  (qq-chat--favorite-view)
  (qq-runtime-request '(favorite-emojis) t))

(defun qq-chat-save-image-as-favorite (&optional context)
  "Collect the exact image card at point, or captured media CONTEXT."
  (interactive)
  (let ((segment (plist-get (or context (appkit-media-card-context-at-point))
                            :payload)))
    (unless (qq-media-favorite-image-p segment)
      (user-error "qq: put point on a native message image first"))
    (qq-media-save-image-as-favorite
     segment
     (lambda (_receipt) (message "qq: message image added to QQ favorites"))
     #'qq-chat--default-error)))

(defun qq-chat-attach-emoji (&optional custom-p)
  "Attach a QQ emoji into the composer.

Without prefix: a small system face.
Use `qq-chat-attach-large-face' for a large face.
With prefix CUSTOM-P (`C-u'): favorite custom face (收藏表情).

Keys: `C-c C-e' / `C-u C-c C-e'."
  (interactive "P")
  (if custom-p
      (qq-chat-attach-custom-face (equal custom-p '(16)))
    (call-interactively #'qq-chat-attach-face)))

(defvar-local qq-chat--clipboard-files nil
  "Temporary input sources created by this buffer, never user attachments.")

(defvar-local qq-chat--clipboard-send-owners nil
  "Exact pending send tokens retaining clipboard sources during staging.")

(defun qq-chat--clipboard-draft-files (draft)
  "Return file paths referenced by structured objects in DRAFT."
  (let ((position 0)
        files)
    (while (< position (length draft))
      (when-let* ((object (get-text-property
                          position appkit-chatbuf-input-object-property draft))
                  (segment (plist-get object :segment))
                  (data (alist-get 'data segment))
                  (path (or (alist-get 'file data) (alist-get 'path data))))
        (cl-pushnew path files :test #'equal))
      (setq position
            (next-single-property-change
             position appkit-chatbuf-input-object-property draft (length draft))))
    files))

(defun qq-chat--prune-clipboard-files ()
  "Delete only owned sources absent from both the draft and pending sends."
  (when qq-chat--clipboard-files
    (let ((retained
           (append (qq-chat--clipboard-draft-files (appkit-chatbuf-input-state))
                   (apply #'append (mapcar #'car qq-chat--clipboard-send-owners))))
          (files qq-chat--clipboard-files))
      (setq qq-chat--clipboard-files
            (seq-filter (lambda (path) (member path retained)) files))
      (dolist (path files)
        (unless (member path qq-chat--clipboard-files)
          (ignore-errors (delete-file path)))))))

(defun qq-chat--dispose-clipboard-files ()
  "Revoke this buffer's input ownership and delete its clipboard sources."
  (let ((files qq-chat--clipboard-files))
    (setq qq-chat--clipboard-files nil
          qq-chat--clipboard-send-owners nil)
    (dolist (path files)
      (ignore-errors (delete-file path)))))

(defun qq-chat--retain-clipboard-send (draft)
  "Retain owned sources in DRAFT until its send settles; return a token."
  (when-let* ((files (seq-intersection
                     qq-chat--clipboard-files
                     (qq-chat--clipboard-draft-files draft) #'equal)))
    (let ((owner (list files)))
      (push owner qq-chat--clipboard-send-owners)
      owner)))

(defun qq-chat--clipboard-send-current-p (buffer owner)
  "Return whether BUFFER still retains the exact clipboard send OWNER."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (memq owner qq-chat--clipboard-send-owners))))

(defun qq-chat--release-clipboard-send (buffer owner)
  "Release clipboard send OWNER without disturbing a newer BUFFER draft."
  (when (qq-chat--clipboard-send-current-p buffer owner)
    (with-current-buffer buffer
      (setq qq-chat--clipboard-send-owners
            (delq owner qq-chat--clipboard-send-owners))
      (qq-chat--prune-clipboard-files))))

(defun qq-chat--attach-clipboard-file (path type)
  "Adopt freshly created clipboard PATH as a TYPE input source.
Ordinary attachments never enter this ownership registry."
  (let (adopted-p)
    (unwind-protect
        (progn
          (qq-chat-attach-file path type)
          (push path qq-chat--clipboard-files)
          (add-hook 'kill-buffer-hook #'qq-chat--dispose-clipboard-files nil t)
          (add-hook 'change-major-mode-hook
                    #'qq-chat--dispose-clipboard-files nil t)
          (setq adopted-p t)
          path)
      (unless adopted-p
        (ignore-errors (delete-file path))))))

(defun qq-chat--clipboard-temp-file (extension)
  "Create a transient input source with EXTENSION, including its leading dot."
  (make-temp-file "qq-clip-" nil extension))

(defun qq-chat--write-binary-temp-file (data extension)
  "Write binary DATA to a clipboard temp file with EXTENSION and return path."
  (let ((path (qq-chat--clipboard-temp-file extension))
        (coding-system-for-write 'binary)
        written-p)
    (unwind-protect
        (progn
          (with-temp-file path
            (set-buffer-multibyte nil)
            (insert data))
          (setq written-p t)
          path)
      (unless written-p
        (ignore-errors (delete-file path))))))

(defun qq-chat--yank-media (mime-type data &optional as-file-p)
  "Attach clipboard/yank media DATA of MIME-TYPE into the chat input.

When AS-FILE-P is non-nil, force segment type `file' (telega C-u clipboard)."
  (unless (and (stringp data) (> (length data) 0))
    (user-error "qq: empty clipboard media"))
  (let* ((ext (pcase mime-type
                ((or 'image/png "image/png") ".png")
                ((or 'image/jpeg "image/jpeg") ".jpg")
                ((or 'image/bmp "image/bmp") ".bmp")
                (_ ".bin")))
         (path (qq-chat--write-binary-temp-file data ext))
         (type (if as-file-p "file" (qq-chat--guess-file-segment-type path))))
    (qq-chat--attach-clipboard-file path type)))

(defun qq-chat-attach-clipboard (&optional as-file-p)
  "Stage clipboard files or image bytes in the current composer.
With a universal prefix AS-FILE-P, stage generic files instead of media.
Ordinary source files remain user-owned; image-byte sources retain the
existing clipboard ownership through captured sends."
  (interactive "P")
  (let ((source (appkit-media-clipboard-source)))
    (if-let* ((paths (plist-get source :files)))
        (dolist (path paths)
          (qq-chat-attach-file path (when as-file-p "file")))
      (qq-chat--yank-media (plist-get source :mime-type)
                          (plist-get source :data) as-file-p))))

(defun qq-chat--current-input-segments ()
  "Parse current input region into outbound QQ message segments.

Follow telega's `telega-chatbuf--input-imcs' shape:

1. Split the input string by `appkit-chatbuf-input-object-property'
   (telega splits on `telega-attach' via `telega--split-by-text-prop').
2. Chunks with no object property → plain `text' segments (CJK included).
3. Chunks with an object → the structured segment (image/face/file/…).

With telega-style insert (object body + trailing spacer with
`rear-nonsticky t'), typed text after an attachment is a separate chunk and
is not swallowed into the image segment."
  (let* ((input (or (appkit-chatbuf-input-string) ""))
         (object-prop appkit-chatbuf-input-object-property)
         (chunks (appkit-chatbuf-split-by-text-property input object-prop))
         segments)
    (dolist (chunk chunks)
      (let ((object (and (not (string-empty-p chunk))
                         (get-text-property 0 object-prop chunk))))
        (if object
            (when-let* ((segment (qq-chat--input-object-segment object)))
              (push segment segments))
          (let ((text (substring-no-properties chunk)))
            ;; telega skips blank text chunks; keep pure whitespace only if
            ;; it is the sole content (otherwise trim attachment spacers).
            (unless (string-empty-p text)
              (push `((type . "text")
                      (data . ((text . ,text))))
                    segments))))))
    ;; Drop whitespace-only text segments that are only the telega-style
    ;; attachment spacer leftovers when mixed with real content.
    (setq segments (nreverse segments))
    (if (seq-find (lambda (s)
                    (or (not (equal (alist-get 'type s) "text"))
                        (not (string-blank-p
                              (or (alist-get 'text (alist-get 'data s)) "")))))
                  segments)
        (seq-filter
         (lambda (s)
           (or (not (equal (alist-get 'type s) "text"))
               (not (string-blank-p
                     (or (alist-get 'text (alist-get 'data s)) "")))))
         segments)
      segments)))

(defun qq-chat--reply-context-text ()
  "Return the unified reply context card shown above the composer.

Never dump a legacy wire token or `raw-message' here.  The bounded preview
comes from `qq-state-message-preview', which is segment-first."
  (let ((message (qq-chat--reply-message)))
    (if-let* ((reply-target
               (and message
                    (qq-message-reply-target qq-chat--session-key message)))
              (name (or (car (qq-chat--message-sender-display-parts message))
                        "message"))
              (title-face (qq-chat--message-title-face message))
              (preview (string-trim
                        (or (qq-state-message-preview message) ""))))
        (appkit-chatbuf-aux-render
         :title (concat "Reply to " (propertize name 'face title-face))
         :preview
         (qq-media-message-one-line-preview
          message
          (if (string-empty-p preview)
              "Message preview unavailable"
            preview))
         :cancel-action #'qq-chat-cancel-reply
         :cancel-help "Cancel reply"
         :accent-face title-face
         :width (qq-chat--line-fill-column))
      "")))

(defun qq-chat--insert-date-separator-row (day-label)
  "Insert a disco-style centered date-break row for DAY-LABEL."
  (let* ((body (format "──(%s)──" day-label))
         (padding
          (max 0 (/ (- (qq-chat--line-fill-column) (string-width body)) 2)))
         (bars (make-string padding ?─)))
    (appkit-presentation-insert-note-line
     (concat bars body bars)
     :face 'qq-msg-date-separator)))

(defun qq-chat--message-by-server-id (server-id)
  "Return the projected message with SERVER-ID in the current session."
  (when (and server-id qq-chat--session-key)
    (seq-find
     (lambda (message)
       (equal (format "%s" (or (alist-get 'server-id message) ""))
              (format "%s" server-id)))
     (qq-state-session-messages qq-chat--session-key))))

(defun qq-chat--message-position (anchor)
  "Return buffer position of timeline ANCHOR's projected row, or nil."
  (and anchor
       (appkit-chat-timeline-live-p)
       (appkit-chat-timeline-key-position (format "%s" anchor))))

(defun qq-chat--message-end-position (start)
  "Return end position of the message block starting at START."
  (or (and start
           (next-single-property-change
            start 'qq-chat-message-anchor nil (point-max)))
      start))

(defun qq-chat--highlight-region (start end)
  "Pulse region START..END (telega uses `pulse-momentary-highlight-region')."
  (when (and (number-or-marker-p start)
             (number-or-marker-p end)
             (< start end)
             (fboundp 'pulse-momentary-highlight-region))
    (with-no-warnings
      (pulse-momentary-highlight-region start end))))

(defun qq-chat--messages-pop-ring-last-p (center)
  "Return non-nil when CENTER is already the top of the pop ring."
  (and (ring-p qq-chat--messages-pop-ring)
       (not (ring-empty-p qq-chat--messages-pop-ring))
       (equal (ring-ref qq-chat--messages-pop-ring 0) center)))

(defun qq-chat--messages-pop-ring-push (center)
  "Push a copy of exact CENTER onto the jump-back ring."
  (when (and center (ring-p qq-chat--messages-pop-ring)
             (not (qq-chat--messages-pop-ring-last-p center)))
    (ring-insert qq-chat--messages-pop-ring (copy-tree center))
    (message "qq: %s to jump back"
             (or (key-description
                  (where-is-internal #'qq-chat-goto-pop-message
                                     qq-chat-timeline-mode-map t))
                 "x"))))

(defun qq-chat--goto-loaded-message (anchor &optional highlight window)
  "Goto timeline ANCHOR only if already displayed.
When HIGHLIGHT is non-nil, pulse the block.  WINDOW, when non-nil,
is the exact window to position, for a context preview."
  (when-let* ((id (and anchor (format "%s" anchor)))
              (pos (qq-chat--message-position id)))
    (goto-char pos)
    (when-let* ((win (or window (get-buffer-window (current-buffer) t))))
      (when (and (window-live-p win)
                 (eq (window-buffer win) (current-buffer)))
        (set-window-point win pos)
        (with-selected-window win
          (goto-char pos)
          (recenter))))
    (when highlight
      (qq-chat--highlight-region pos (qq-chat--message-end-position pos)))
    t))

(defun qq-chat--jump-history-count ()
  "Return history page size used when seeking a jump target."
  (max 1 (or qq-chat-jump-history-count qq-history-fetch-count)))

(defun qq-chat--finish-jump-if-loaded (center)
  "Jump to CENTER only when exactly one matching row is rendered."
  (let* ((window (nth 3 qq-chat--jump-owner))
         (sources
          (and (or (null window)
                   (and (window-live-p window)
                        (eq (window-buffer window) (current-buffer))))
               (seq-filter
                (lambda (message)
                  (qq-message-history-locator-matches-p center message))
                (qq-state-session-messages qq-chat--session-key))))
         (anchor (and (= (length sources) 1)
                      (qq-state-message-anchor (car sources)))))
    (when (and anchor (qq-chat--goto-loaded-message anchor t window))
      (setq qq-chat--pending-jump-center nil)
      (run-hook-with-args 'qq-chat-message-jump-hook center)
      t)))

(defun qq-chat--jump-fail (target &optional reason)
  "Clear pending jump for TARGET and report not found."
  (when (equal qq-chat--pending-jump-center target)
    (setq qq-chat--pending-jump-center nil))
  (message "qq: message not found%s"
           (if (and reason (not (string-empty-p reason)))
               (format " (%s)" reason)
             "")))

(defun qq-chat--cancel-history-request ()
  "Stop waiting for this chat's open or page; the App still merges its rows."
  (when-let* ((view (qq-chat--live-current-view)))
    (appkit-surface-send view '(qq-timeline-cancel))))

(defun qq-chat--apply-timeline-ops (rows ops)
  "Return ROWS, a list of (ROW-KEY . ANCHOR), after Gateway diff OPS."
  (dolist (op ops rows)
    (setq rows (cl-remove (nth 1 op) rows :key #'car :test #'equal))
    (when (eq (car op) 'upsert)
      (let ((row (cons (nth 1 op) (nth 2 op)))
            (index (and (nth 3 op)
                        (cl-position (nth 3 op) rows :key #'car :test #'equal))))
        (setq rows (if index
                       (append (seq-take rows (1+ index)) (list row) (nthcdr (1+ index) rows))
                     (cons row rows)))))))

(defun qq-chat--history-present (presentation)
  "Apply the position request PRESENTATION after projection."
  (pcase (plist-get presentation :kind)
    ('latest (qq-chat--goto-latest-window-end))
    ('around
     (let ((center (plist-get presentation :center))
           (anchor (plist-get presentation :anchor)))
       (cond
        ((plist-get presentation :error)
         (qq-chat--jump-fail center (plist-get presentation :error)))
        ((not (equal qq-chat--pending-jump-center center)))
        ((and anchor (qq-chat--goto-loaded-message anchor t (nth 3 qq-chat--jump-owner)))
         (setq qq-chat--pending-jump-center nil)
         (run-hook-with-args 'qq-chat-message-jump-hook center))
        (t (qq-chat--jump-fail center "around window omitted target")))))))

(defun qq-chat--page-count (focus)
  "Return the page size for opening FOCUS or paging."
  (if (eq (car-safe focus) 'around)
      (qq-chat--jump-history-count)
    (min 100 (max 1 qq-history-fetch-count))))

(defun qq-chat--dataline-update (context model message)
  "Load a DataLine chat by history pages; it is not a Gateway timeline."
  (let ((post (lambda (target message)
                (appkit-command-post-message :target target :message message
                                             :delivery 'report))))
    (pcase message
      (`(qq-timeline-start ,_focus)
       (let ((input (list :kind 'initial :session-key qq-chat--session-key
                          :view (qq-chat--view-id) :limit (qq-chat--page-count nil))))
         (appkit-next
          :model (qq-chat--with model :opening input :history-presentation nil)
          :render (appkit-projection-change-create :frame-p t)
          :commands (list (appkit-command-post-message
                           :target (appkit-transition-context-parent-address context)
                           :message (list 'qq-history-fetch input)
                           :reply-correlation 'qq-timeline :delivery 'report)))))
      (`(qq-timeline-page older)
       (let ((input (list :kind 'older :cursor (plist-get model :older-cursor)
                          :session-key qq-chat--session-key :view (qq-chat--view-id)
                          :limit (qq-chat--page-count nil))))
         (appkit-next
          :model (qq-chat--with model :paging input)
          :render (appkit-projection-change-create :frame-p t)
          :commands (list (appkit-command-post-message
                           :target (appkit-transition-context-parent-address context)
                           :message (list 'qq-history-fetch input)
                           :reply-correlation 'qq-timeline :delivery 'report)))))
      (`(qq-history-result ,input ,meta ,response ,reason)
       (if (not (memq input (list (plist-get model :opening) (plist-get model :paging))))
           (appkit-next :model model :render appkit-render-none)
         (when reason (qq-chat--default-error response reason))
         (let* ((initial (eq (plist-get input :kind) 'initial))
                (cursor (if reason (plist-get model :older-cursor)
                          (copy-tree (plist-get meta :history-older-cursor))))
                (presentation (and initial (not reason) (list :kind 'latest))))
           (appkit-next
            :model (qq-chat--with model :timeline (or (plist-get model :timeline)
                                                     (and (not reason) 'dataline))
                                  :older-cursor cursor
                                  :has-older (and cursor (plist-get meta :history-has-older-p) t)
                                  :has-newer nil :opening nil :paging nil
                                  :history-presentation presentation)
            :render (appkit-projection-change-create :full-p t :frame-p t :position 'preserve)
            :commands (when presentation
                        (list (funcall post (appkit-transition-context-owner-address context)
                                       (list 'qq-history-presented presentation))))))))
      (_ (appkit-next :model model :render appkit-render-none)))))

(defun qq-chat-timeline-update (context model message)
  "Reduce timeline MESSAGE in the chat Surface's update.
The account App opens, pages and merges; this Surface owns its rows."
  (let ((parent 'parent) (self 'self))
    (cl-flet ((post (target message &optional correlation)
                (appkit-command-post-message
                 :target (if (eq target 'parent)
                             (appkit-transition-context-parent-address context)
                           (appkit-transition-context-owner-address context))
                 :message message :reply-correlation correlation :delivery 'report))
              (next (model &optional render commands)
                (appkit-next :model model :render (or render appkit-render-none)
                             :commands commands)))
      (if (and qq-chat--session-key
               (eq (qq-state-session-key-type qq-chat--session-key) 'dataline)
               (memq (car-safe message) '(qq-timeline-start qq-timeline-page qq-history-result)))
          (qq-chat--dataline-update context model message)
        (pcase message
          (`(qq-timeline-start ,focus)
           (let ((focus (copy-sequence focus)))
             (next (qq-chat--with model :opening focus :paging nil :history-presentation nil)
                   (appkit-projection-change-create :frame-p t)
                   (append
                    (when-let* ((old (plist-get model :timeline)))
                      (list (post parent (list 'qq-timeline-close old))))
                    (list (post parent (list 'qq-timeline-open qq-chat--session-key focus
                                             (qq-chat--page-count focus))
                                'qq-timeline))))))
          (`(qq-timeline-loaded ,id ,focus ,rows ,has-older ,has-newer ,center-anchor)
           (if (not (eq focus (plist-get model :opening)))
               (next model nil (list (post parent (list 'qq-timeline-close id))))
             (let ((presentation
                    (if (eq (car focus) 'around)
                        (list :kind 'around :center (cadr focus) :anchor center-anchor)
                      (list :kind 'latest))))
               (next (qq-chat--with model :timeline id :rows rows
                                    :has-older has-older :has-newer has-newer
                                    :opening nil :history-presentation presentation)
                     (appkit-projection-change-create :full-p t :frame-p t :position 'preserve)
                     (list (post self (list 'qq-history-presented presentation)))))))
          (`(qq-timeline-failed ,focus ,reason)
           (if (not (eq focus (plist-get model :opening)))
               (next model)
             (let ((presentation (and (eq (car focus) 'around)
                                      (list :kind 'around :center (cadr focus) :error reason))))
               (unless presentation (message "qq: %s" reason))
               (next (qq-chat--with model :opening nil :history-presentation presentation)
                     (appkit-projection-change-create :frame-p t)
                     (when presentation
                       (list (post self (list 'qq-history-presented presentation))))))))
          (`(qq-timeline-changed ,id ,ops ,has-older ,has-newer)
           (if (not (equal id (plist-get model :timeline)))
               (next model)
             (next (qq-chat--with model
                                  :rows (qq-chat--apply-timeline-ops (plist-get model :rows) ops)
                                  :has-older has-older :has-newer has-newer)
                   (appkit-projection-change-create :full-p t :frame-p t :position 'preserve))))
          (`(qq-timeline-page ,direction)
           (let ((id (plist-get model :timeline)))
             (if (or (not (stringp id)) (plist-get model :opening) (plist-get model :paging)
                     (not (plist-get model (if (eq direction 'older) :has-older :has-newer))))
                 (next model)
               (next (qq-chat--with model :paging direction)
                     (appkit-projection-change-create :frame-p t)
                     (list (post parent (list 'qq-timeline-paginate id
                                              direction (qq-chat--page-count nil))
                                 'qq-timeline))))))
          (`(qq-timeline-paged ,id ,direction ,outcome ,reason)
           (if (not (and (equal id (plist-get model :timeline))
                         (eq direction (plist-get model :paging))))
               (next model)
             (when (eq outcome 'failed) (message "qq: %s" reason))
             (let ((edge (if (eq direction 'older) :has-older :has-newer)))
               (next (qq-chat--with model :paging nil
                                    edge (and (not (eq outcome 'edge)) (plist-get model edge)))
                     (appkit-projection-change-create :frame-p t)))))
          ('(qq-timeline-cancel)
           (next (qq-chat--with model :opening nil :paging nil :history-presentation nil)
                 (appkit-projection-change-create :frame-p t)))
          (`(qq-history-presented ,presentation)
           (next (if (eq presentation (plist-get model :history-presentation))
                     (qq-chat--with model :history-presentation nil)
                   model)))
          (_ (next model)))))))

(defun qq-chat-goto-message (center &optional no-pop window)
  "Goto typed history CENTER, opening the timeline around it if needed.

Unless NO-POP, retain the current row's full locator for jump-back.
WINDOW, when non-nil, receives the context without selecting it.
Return a unique jump owner usable with `qq-chat--cancel-jump'."
  (interactive
   (list (or (get-text-property (point) 'qq-chat-reply-center)
             (qq-chat--message-reply-locator (qq-chat--message-at-point))
             `((kind . "message") (message_id . ,(read-string "Message id: "))))))
  (unless qq-chat--session-key
    (user-error "qq: this buffer is not bound to a session"))
  (unless center
    (user-error "qq: reply lacks a usable native message locator"))
  (setq center
        (qq-message--history-center
         center (qq-message--history-conversation-params qq-chat--session-key)
         "Jump target"))
  (unless no-pop
    (when-let* ((at-point (qq-chat--message-at-point))
                (origin (qq-message-history-locator at-point)))
      (unless (qq-message-history-locator-matches-p center at-point)
        (qq-chat--messages-pop-ring-push origin))))
  (when qq-search--inline-p (qq-search-cancel-filter t))
  (unless window
    (when-let* ((visible (get-buffer-window (current-buffer) t)))
      (set-window-parameter visible 'qq-chat-preview nil)))
  (qq-chat--cancel-jump qq-chat--jump-owner)
  (let ((owner (list 'jump center nil window)))
    (setq qq-chat--jump-owner owner)
    (unless (qq-chat--finish-jump-if-loaded center)
      (setq qq-chat--pending-jump-center center)
      (message "qq: loading…")
      (qq-chat--open-around center)
      (setf (nth 2 owner) (qq-chat--loading)))
    owner))

(defun qq-chat--open-around (center)
  "Open this chat's timeline around typed history CENTER."
  (appkit-surface-send (qq-chat--ensure-view)
                       (list 'qq-timeline-start (list 'around center))))

(defun qq-chat--cancel-jump (owner)
  "Cancel only the still-pending message jump belonging to OWNER."
  (when (and owner (eq owner qq-chat--jump-owner))
    (setq qq-chat--jump-owner nil qq-chat--pending-jump-center nil)
    (when (and (nth 2 owner)
               (eq (nth 2 owner) (qq-chat--loading)))
      (qq-chat--cancel-history-request))))

(defun qq-chat-goto-reply (&optional message)
  "Goto the message that MESSAGE replies to.

MESSAGE defaults to the message at point.  Bound to timeline `g'.
The inline reply preview also calls this path, corresponding to
telega's `telega-msg-goto-reply-to-message'."
  (interactive)
  (let* ((msg (or message
                  (qq-chat--message-at-point)
                  (user-error "qq: no message at point")))
         (locator (qq-chat--message-reply-locator msg)))
    (unless locator
      (user-error "qq: reply lacks source metadata for an exact history lookup"))
    (qq-chat-goto-message locator)))

(defun qq-chat-goto-pop-message ()
  "Pop a message from the jump ring and goto it.

Bound to timeline `x', corresponding to telega's
`telega-chatbuf-goto-pop-message'."
  (interactive)
  (unless (and (ring-p qq-chat--messages-pop-ring)
               (not (ring-empty-p qq-chat--messages-pop-ring)))
    (user-error "qq: no messages to pop to"))
  (let ((id (ring-remove qq-chat--messages-pop-ring 0)))
    (message "qq: %d messages left in ring"
             (ring-length qq-chat--messages-pop-ring))
    ;; Avoid re-pushing current while popping (telega binds ring to nil).
    (let ((qq-chat--messages-pop-ring nil))
      (qq-chat-goto-message id 'no-pop))))

(defun qq-chat--reply-preview-text (message)
  "Return MESSAGE's one-line content, retaining semantic mention colors.
Preview entities have no independent actions: the quote opens its original."
  (let ((segments (alist-get 'segments message)))
    (if (or (qq-state-message-recalled-p message) (null segments))
        (qq-chat--message-body message)
      (appkit-ui-one-line-text
       (mapconcat
        (lambda (segment)
          (pcase (alist-get 'type segment)
            ("text" (or (alist-get 'text (alist-get 'data segment)) ""))
            ("at" (qq-chat--mention-preview-string (alist-get 'data segment)))
            (_ (qq-state-message-preview-from-segments (list segment)))))
        segments "")))))

(defun qq-chat--insert-reply-preview-line (reply-data properties prefix-state
                                                    &optional context)
  "Insert a shared inline reply preview and its exact native jump target.
CONTEXT carries the quote snapshot captured by the timeline projection."
  (let* ((model (or (plist-get context :reply-preview)
                    (qq-reply-preview-model
                     reply-data (qq-reply-source qq-chat--session-key reply-data))))
         (target (if (plist-member context :reply-center)
                     (plist-get context :reply-center)
                   (qq-chat--reply-data-locator reply-data))))
    (appkit-chat-ins-insert-reply-preview
     (appkit-ui-one-line-preview-create
      :label (plist-get model :sender)
      :label-face (appkit-name-color-face (plist-get model :sender-key))
      :text (plist-get model :body))
     (max 12 (- (qq-chat--line-fill-column)
                (string-width (appkit-ui-prefix-string prefix-state nil ""))))
     :prefix prefix-state
     :action (and target (lambda () (qq-chat-goto-message target)))
     :help-echo (and target "Jump to replied message (G / RET)")
     :properties (append properties
                         (list 'qq-chat-reply-center target
                               'qq-chat-reply-button (and target t))))))

(defun qq-chat--face-segment-id (segment)
  "Return QQ base face id from face SEGMENT, or nil."
  (let* ((data (alist-get 'data segment))
         (raw (alist-get 'raw data)))
    (or (alist-get 'id data)
        (alist-get 'faceIndex data)
        (and (listp raw) (alist-get 'faceIndex raw)))))

(defun qq-chat--animated-face-segment-p (segment)
  "Return non-nil when face SEGMENT identifies an animated base face."
  (when (equal (alist-get 'type segment) "face")
    (let* ((data (alist-get 'data segment))
           (raw (alist-get 'raw data))
           (face-type (or (alist-get 'face_type data)
                          (alist-get 'faceType data)
                          (and (listp raw) (alist-get 'faceType raw))))
           (sticker-id (or (alist-get 'sticker_id data)
                           (alist-get 'stickerId data)
                           (and (listp raw) (alist-get 'stickerId raw)))))
      (or (equal face-type "animated")
          (equal face-type 3)
          (and sticker-id
               (not (equal (format "%s" sticker-id) "0")))))))

(defun qq-chat--face-segment-description (segment)
  "Return the native display description from face SEGMENT, or nil."
  (let* ((data (alist-get 'data segment))
         (raw (alist-get 'raw data)))
    (or (alist-get 'description data)
        (alist-get 'faceText data)
        (alist-get 'face_text data)
        (and (listp raw) (alist-get 'faceText raw))
        (and (listp raw) (alist-get 'face_text raw)))))

(defun qq-chat--open-mention-user (user-id)
  "Open the profile for mentioned USER-ID."
  (unless (and (qq-protocol-user-uin-p user-id)
               (not (equal user-id "0")))
    (user-error "qq: mention has no user profile"))
  (qq-user-open user-id))

(defun qq-chat--mention-display-string (data)
  "Return a telega-style interactive mention string from segment DATA."
  (let* ((target (and (alist-get 'qq data)
                      (format "%s" (alist-get 'qq data))))
         (kind (cond
                ((equal target "all") 'at-all)
                ((and target
                      (equal target (qq-state-self-user-id))) 'at-me)
                (t 'ordinary)))
         (label (or (alist-get 'name data)
                    (and (eq kind 'at-all) "全体成员")
                    target
                    "mention"))
         (text (concat "@" label))
         (profile-p (and (qq-protocol-user-uin-p target)
                         (not (equal target "0"))))
         (display
          (if profile-p
              (buttonize
               text #'qq-chat--open-mention-user target
               (format "Open %s's profile (QQ %s)" label target))
            text))
         (color-face
          (and (eq kind 'ordinary)
               profile-p
               (appkit-name-color-face target)))
         (mention-face
          (cond
           ((memq kind '(at-me at-all)) 'qq-msg-mention-self)
           (color-face (list color-face 'qq-msg-mention))
           (t 'qq-msg-mention))))
    (when profile-p
      (put-text-property 0 (length display) 'keymap appkit-ui-button-map display))
    (add-text-properties
     0 (length display)
     (list 'face mention-face
           'qq-chat-mention-kind kind
           'qq-chat-mention-user-id (and profile-p target)
           'rear-nonsticky '(qq-chat-mention-kind
                             qq-chat-mention-user-id))
     display)
    display))

(defun qq-chat--mention-preview-string (data)
  "Return mention segment DATA as preview text keeping only its face.
Previews such as reply quotes and root rows have no per-entity actions."
  (let ((mention (qq-chat--mention-display-string data)))
    (propertize (substring-no-properties mention)
                'face (get-text-property 0 'face mention))))

(defun qq-chat--segment-inline-string (segment)
  "Return inline display string for SEGMENT, or nil for block-like segments.

Face segments render as inline images from Linux QQ's default emoji catalog,
never as legacy wire text."
  (let ((type (alist-get 'type segment))
        (data (alist-get 'data segment)))
    (pcase type
      ("text" (or (alist-get 'text data) ""))
      ("at"
       (qq-chat--mention-display-string data))
      ("__unsupported"
       (qq-state-message-preview-from-segments (list segment)))
      ("face"
       (qq-media-face-display-string
        (or (qq-chat--face-segment-id segment) "?")
        (qq-chat--face-segment-description segment)))
      ("favorite_emoji" "[收藏表情]")
      (_ nil))))

(defun qq-chat--mail-segment-p (segment)
  "Return non-nil when SEGMENT is a structured QQ Mail notification."
  (equal (alist-get 'type segment) "mail"))

(defun qq-chat--wallet-segment-p (segment)
  "Return non-nil when SEGMENT is a native QQ wallet message."
  (equal (alist-get 'type segment) "wallet"))

(defun qq-chat--poke-segment-p (segment)
  "Return non-nil when SEGMENT is a typed Poke GrayTip."
  (and (equal (alist-get 'type segment) "gray-tip")
       (equal (alist-get 'kind (alist-get 'data segment)) "poke")))

(defun qq-chat--gray-tip-segment-p (segment)
  "Return non-nil when SEGMENT is a QQ gray-tip notice."
  (equal (alist-get 'type segment) "gray-tip"))

(defun qq-chat--insert-gray-tip-user (part)
  "Insert the typed user identity represented by GrayTip PART.

Only a real UIN is profile-addressable.  UID-only producer semantics remain
plain text instead of inventing a UIN alias or an avatar capability."
  (let ((user-id (alist-get 'user-id part))
        (name (alist-get 'name part)))
    (if (null user-id)
        (insert (propertize name 'face 'qq-msg-user-title))
      (let ((label (concat (qq-media-avatar-display-string user-id) " " name))
            (action (lambda () (qq-user-open user-id))))
        (appkit-ui-insert-action-button
         label action
         :face 'qq-msg-user-title
         :help-echo (format "Open %s's profile" name)
         :properties
         (list 'read-only t
               'front-sticky t
               'rear-nonsticky '(read-only)
               'qq-chat-gray-tip-user-id user-id))))))

(defun qq-chat--gray-tip-label (message)
  "Return MESSAGE's structured, interactive gray-tip label."
  (let ((parts (alist-get 'parts (qq-state-gray-tip-message-data message))))
    (if (null parts)
        (qq-chat--message-body message)
      (with-temp-buffer
        (let ((inhibit-read-only t))
          (dolist (part parts)
            (pcase (alist-get 'type part)
              ("text" (insert (alist-get 'text part)))
              ("user" (qq-chat--insert-gray-tip-user part))))
          (buffer-string))))))

(defun qq-chat--insert-gray-tip-message (message properties)
  "Insert centered gray-tip MESSAGE using PROPERTIES."
  (let ((start (point)))
    (appkit-chat-ins-insert-divider-row
     (qq-chat--gray-tip-label message)
     'qq-msg-poke
     (qq-chat--line-fill-column))
    (add-text-properties start (point) properties)))

(defun qq-chat--insert-poke-message (message properties)
  "Insert decorative gray-tip MESSAGE using PROPERTIES.

Pokes deliberately bypass the ordinary message header/body layout.  Following
telega's special-message inserter, render one centered `( action )' row with
horizontal bars and keep the time at the far right.  QQ's native descriptive
tail stays on that same row; truncate the action before allowing it to collide
with the timestamp."
  (let* ((data (or (qq-state-poke-message-data message) '()))
         (names (qq-state--poke-display-names data))
         (actor (or (car names)
                    (qq-chat--present-string (alist-get 'sender-name message))
                    "某人"))
         (target (cdr names))
         (action (or (qq-chat--present-string (alist-get 'action data))
                     "戳了戳"))
         (detail (qq-chat--present-string (alist-get 'detail data)))
         (image-url (qq-chat--present-string (alist-get 'image-url data)))
         (image
          (if image-url
              (qq-media-url-one-line-preview-display-string
               (qq-media-poke-image-cache-key image-url) image-url "✦")
            "✦"))
         (time (qq-chat--format-time-short (alist-get 'time message)))
         (time-width (string-width time))
         (max-content-width
          (max 1 (- (qq-chat--line-fill-column) time-width 12)))
         (content
          (truncate-string-to-width
           (concat actor " " image " "
                   (string-join
                    (delq nil (list action target detail))
                    " "))
           max-content-width nil nil "…"))
         )
    (appkit-chat-ins-insert-tip-row
     content 'qq-msg-poke (qq-chat--line-fill-column)
     :time time :time-face 'qq-msg-status :properties properties)))

(defun qq-chat--card-segment-p (segment)
  "Return non-nil when SEGMENT is a normalized Ark rich card."
  (equal (alist-get 'type segment) "card"))

(defun qq-chat--link-preview-segment-p (segment)
  "Return non-nil when text SEGMENT carries native URL-preview metadata."
  (and (equal (alist-get 'type segment) "text")
       (alist-get 'link_preview (alist-get 'data segment))))

(defun qq-chat--insert-link-preview-segment (segment prefix-state properties)
  "Insert SEGMENT as an Appkit link card without duplicating its URL body."
  (let* ((data (alist-get 'data segment))
         (preview (alist-get 'link_preview data))
         (text (or (alist-get 'text data) ""))
         (url (string-trim text))
         (title (qq-chat--present-string (alist-get 'title preview)))
         (description
          (qq-chat--present-string (alist-get 'description preview)))
         (open-action
          (and (qq-media-http-url-p url)
               (lambda (_button) (browse-url url t))))
         (card-properties
          (append properties
                  (when open-action
                    (list 'mouse-face 'highlight
                          'help-echo (format "Open %s" url)
                          'follow-link t
                          'keymap appkit-ui-button-map
                          'button t 'category 'default-button
                          'action open-action))))
         (appkit-ui-card-indent-prefix-state prefix-state)
         (card-prefix-state (appkit-ui-card-prefix-state)))
    (when (and title (not (equal title url)))
      (appkit-ui-insert-prefixed-lines
       card-prefix-state title :face 'bold :properties card-properties))
    (when (and description (not (equal description title))
               (not (equal description url)))
      (appkit-ui-insert-prefixed-lines
       card-prefix-state description :face 'shadow
       :properties card-properties))
    (when-let* ((icon-url (qq-media-link-preview-icon-url segment))
                (key (qq-media-link-preview-cache-key segment))
                (image (qq-media-url-preview-image
                        key icon-url qq-media-preview-image-height)))
      (let ((start (point)))
        (appkit-media-insert-image-slices image nil nil " ")
        (insert "\n")
        (appkit-ui-apply-line-prefix start (point) card-prefix-state)
        (add-text-properties start (point) (append '(face default) card-properties))))
    ;; Keep the exact authored body visible and available to copy.  Missing
    ;; title/description never substitutes a fabricated label for the URL.
    (appkit-ui-insert-prefixed-lines
     card-prefix-state text :face (if open-action 'link 'default)
     :properties card-properties)))

(defun qq-chat--insert-mail-segment (segment prefix-state properties)
  "Insert structured QQ Mail SEGMENT as a compact card."
  (let* ((data (alist-get 'data segment))
         (sender (alist-get 'sender data))
         (subject (alist-get 'subject data))
         (content (alist-get 'content data))
         (prompt (alist-get 'prompt data))
         (detail (or (alist-get 'detail data) "Open Mail"))
         (url (alist-get 'url data))
         (appkit-ui-card-indent-prefix-state prefix-state)
         (card-prefix-state (appkit-ui-card-prefix-state)))
    (appkit-ui-insert-prefixed-lines
     card-prefix-state
     (if (and (stringp sender) (not (string-empty-p sender)))
         (format "Mail · %s" sender)
       "Mail")
     :face 'bold
     :properties properties)
    (when (and (stringp subject) (not (string-empty-p subject)))
      (appkit-ui-insert-prefixed-lines
       card-prefix-state subject
       :properties properties))
    (when-let* ((body (cond
                       ((and (stringp content) (not (string-empty-p content))) content)
                       ((and (stringp prompt) (not (string-empty-p prompt))) prompt))))
      (appkit-ui-insert-prefixed-lines
       card-prefix-state body
       :face 'shadow
       :properties properties))
    (when (and (stringp url) (not (string-empty-p url)))
      (let ((start (point)))
        (appkit-ui-insert-action-button
         (format "[%s]" detail)
         (lambda () (browse-url url t))
         :help-echo "Open this message in QQ Mail")
        (insert "\n")
        (appkit-ui-apply-line-prefix start (point) card-prefix-state)
        (add-text-properties start (point) properties)))))

(defun qq-chat--card-kind-label (kind)
  "Return a short display label for normalized card KIND."
  (pcase kind
    ("miniapp" "Mini App")
    ("app" "App")
    ("share" "Share")
    ("forward" "Chat History")
    ("forum" "Channel Post")
    ("guild" "频道帖子")
    ("announcement" "Group Announcement")
    ("contact" "Contact")
    (_ "Card")))

(defun qq-chat--insert-card-image (segment card-prefix)
  "Insert SEGMENT's cover using Appkit's media grid inside CARD-PREFIX."
  (let* ((surface (appkit-current-surface))
         (url (alist-get 'image (alist-get 'data segment)))
         (width (min appkit-media-preview-max-width
                     (* (max 1 (- (or (and surface
                                          (appkit-surface-responsive-width surface))
                                     fill-column)
                                  (string-width (appkit-ui-prefix-string card-prefix))))
                        (appkit-media--base-char-pixel-width))))
         (resource (qq-media-card-image-resource surface segment))
         (file (alist-get 'file resource))
         (context
          (appkit-media-card-context-create
           :payload resource :kind 'image :title "图片"
           :open-action (lambda () (qq-media-open-resource resource 'image nil :owner surface))
           :save-as-action
           (lambda ()
             (let ((name (appkit-media-resource-name resource)))
               (appkit-media-copy-or-download-resource-async
                resource (read-file-name "Save image as: " nil name nil name)
                (lambda (path) (message "qq: saved image -> %s" path))
                (lambda (reason) (message "qq: failed to save image: %s" reason)))))
           :copy-url-action (lambda () (kill-new url)))))
    (appkit-media-insert-grid
     (list (appkit-media-grid-cell-create
            :image (and file (appkit-media-preview-image-from-file file width))
            :fallback (propertize "[图片]" 'face 'shadow)
            :action (plist-get context :open-action)
            :help-echo "查看图片"
            :properties (list appkit-media-card-context-property context)))
     :prefix card-prefix)
    (insert "\n")))

(defun qq-chat--insert-card-segment (segment prefix-state properties)
  "Insert normalized Ark rich-card SEGMENT using shared Appkit card semantics."
  (let* ((data (alist-get 'data segment))
         (label (qq-chat--card-kind-label (alist-get 'kind data)))
         (title (qq-chat--present-string (alist-get 'title data)))
         (content (qq-chat--present-string (alist-get 'content data)))
         (prompt (qq-chat--present-string (alist-get 'prompt data)))
         (source (qq-chat--present-string (alist-get 'source data)))
         (summary (qq-chat--present-string (alist-get 'summary data)))
         (url (qq-chat--present-string (alist-get 'url data)))
         (image-url (qq-chat--present-string (alist-get 'image data)))
         (body (or content (and (not title) prompt)))
         (guild-share (alist-get 'guild-share data))
         (account (qq-runtime-current-account-id))
         (open-action
          (if guild-share
              (lambda () (qq-guild-share-open guild-share account))
            (and url (lambda () (browse-url url t))))))
    (appkit-ui-insert-card
     (lambda (card-prefix)
       (appkit-ui-insert-prefixed-lines
        card-prefix (if source (format "%s · %s" label source) label) :face 'bold)
       (when title
         (appkit-ui-insert-prefixed-lines card-prefix title))
       (when body
         (appkit-ui-insert-prefixed-lines card-prefix body :face 'shadow))
       (when (and summary (not (equal summary body)))
         (appkit-ui-insert-prefixed-lines card-prefix summary :face 'shadow))
       (when image-url
         (qq-chat--insert-card-image segment card-prefix)))
     :prefix prefix-state :properties properties
     :action open-action
     :help-echo (if guild-share "打开频道帖子" (format "Open this %s" (downcase label))))))

(defun qq-chat--insert-wallet-segment
    (segment _message prefix-state properties)
  "Insert wallet SEGMENT as a read-only summary card."
  (let* ((data (alist-get 'data segment))
         (receiver (alist-get 'receiver data))
         (sender (alist-get 'sender data))
         (presentation
          (if (seq-some (lambda (key)
                          (qq-chat--present-string (alist-get key receiver)))
                        '(title sub_title content notice))
              receiver
            sender))
         (title (qq-chat--present-string (alist-get 'title presentation)))
         (subtitle
          (qq-chat--present-string (alist-get 'sub_title presentation)))
         (content (qq-chat--present-string (alist-get 'content presentation)))
         (wallet-kind (alist-get 'wallet_kind data))
         (kind-label
          (pcase wallet-kind
            ("red-packet" "🧧 QQ 红包")
            ("password-red-packet" "🧧 口令红包")
            ("transfer" (if content (format "💳 %s" content) "💳 转账"))
            (_ (if content (format "💳 %s" content) "QQ 钱包"))))
         (appkit-ui-card-indent-prefix-state prefix-state)
         (card-prefix-state (appkit-ui-card-prefix-state)))
    (appkit-ui-insert-prefixed-lines
     card-prefix-state kind-label
     :face 'bold
     :properties properties)
    (when title
      (appkit-ui-insert-prefixed-lines
       card-prefix-state title
       :properties properties))
    (when subtitle
      (appkit-ui-insert-prefixed-lines
       card-prefix-state subtitle
       :face 'shadow
       :properties properties))
    (when (and content (not (equal content title)))
      (appkit-ui-insert-prefixed-lines
       card-prefix-state content
       :face 'shadow
       :properties properties))))

(defun qq-chat--media-segment-p (segment)
  "Return non-nil when SEGMENT should render as a media block."
  (member (alist-get 'type segment)
          '("image" "file" "group_file" "record" "video" "mface")))

(defun qq-chat--segment-media-kind-label (segment)
  "Return short kind label for media SEGMENT."
  (let ((type (alist-get 'type segment))
        (data (alist-get 'data segment)))
    (pcase type
      ("image" (if (alist-get 'emoji_id data) "Sticker" "Image"))
      ("file"
       (pcase (qq-media-segment-kind segment)
         ('image "Image")
         ('video "Video")
         (_ "File")))
      ("group_file" "Group File")
      ("record" "Voice")
      ("video" "Video")
      ("mface" "Sticker")
      (_ "Media"))))

(defun qq-chat--segment-media-summary (segment)
  "Return one-line human summary for media SEGMENT.

Prefer a short name; never dump full URLs or CQ blobs into the timeline."
  (let* ((data (alist-get 'data segment))
         (type (alist-get 'type segment))
         (fallback (pcase type
                     ("image" "image")
                     ("mface" "sticker")
                     ("record" "voice")
                     ("video" "video")
                     ((or "file" "group_file") "file")
                     (_ "media")))
         (summary (or (alist-get 'summary data)
                      (qq-media-segment-display-name segment))))
    (qq-state--short-media-label summary fallback)))

(defun qq-chat--format-byte-size (value)
  "Return human-readable string for byte VALUE, or nil."
  (let ((bytes (cond
                ((integerp value) value)
                ((stringp value) (truncate (string-to-number value)))
                (t nil))))
    (when (and bytes (> bytes 0))
      (cond
       ((>= bytes (* 1024 1024 1024))
        (format "%.1f GB" (/ bytes 1073741824.0)))
       ((>= bytes (* 1024 1024))
        (format "%.1f MB" (/ bytes 1048576.0)))
       ((>= bytes 1024)
        (format "%.1f KB" (/ bytes 1024.0)))
       (t (format "%d B" bytes))))))

(defun qq-chat--segment-media-meta-line (segment)
  "Return one quiet meta line for media SEGMENT.

Keep this short — size is useful; internal sub_type / emoji ids are not."
  (let* ((data (alist-get 'data segment))
         (size (qq-chat--format-byte-size (or (alist-get 'file_size data)
                                              (alist-get 'file-size data))))
         (duration (alist-get 'duration_seconds data))
         (duration-text
          (when (and (integerp duration) (> duration 0))
            (format "%d:%02d" (/ duration 60) (% duration 60)))))
    (string-join (delq nil (list duration-text size)) " · ")))

(defun qq-chat--segment-media-card-kind (segment)
  "Return shared media card kind for timeline SEGMENT."
  (pcase (alist-get 'type segment)
    ("record" 'audio)
    ("mface" 'sticker)
    (_ (qq-media-segment-kind segment))))

(defun qq-chat--segment-media-card-context (segment &optional capabilities preview-position)
  "Adapt media SEGMENT to the shared card action protocol.

CAPABILITIES defaults to the centralized `qq-media' action/status model.
PREVIEW-POSITION, when present, marks this card's inline media occurrence.
The exact Surface is captured while rendering; later actions never resolve
a replacement owner."
  (let* ((owner (appkit-current-surface))
         (capabilities (or capabilities
                           (qq-media-segment-capabilities segment)))
         (url (plist-get capabilities :remote-url))
         (transfer (qq-media-segment-transfer segment)))
    (appkit-media-card-context-create
     :payload segment
     :kind (qq-chat--segment-media-card-kind segment)
     :title (qq-chat--segment-media-summary segment)
     :open-action (when (plist-get capabilities :open)
                    (lambda ()
                      (if-let* ((buffer (and preview-position
                                            (marker-buffer preview-position)))
                                (host
                                 (with-current-buffer buffer
                                   (appkit-media-inline-host-at-point
                                    preview-position))))
                          (appkit-media-inline-host-activate host 'dedicated)
                        (qq-media-segment-open segment :owner owner))))
     :download-action (when (plist-get capabilities :download)
                        (lambda ()
                          (qq-media-segment-start-download
                           segment nil
                           :owner owner)))
     :cancel-action (plist-get transfer :action)
     :save-as-action (when (plist-get capabilities :save)
                       (lambda ()
                         (qq-media-segment-save-as segment)))
     :copy-url-action (when (plist-get capabilities :copy-url)
                        (lambda ()
                          (kill-new url)
                          (message "qq: copied media URL"))))))

(defun qq-chat--media-card-fallback-context ()
  "Return primary media context for the message at point."
  (when-let* ((message (ignore-errors (qq-chat--message-at-point)))
              (segment (qq-media-message-primary-segment message)))
    (qq-chat--segment-media-card-context segment)))

(defun qq-chat--native-record-control-state (segment capabilities)
  "Adapt native record SEGMENT and CAPABILITIES to Appkit voice-note state."
  (let* ((playback (qq-media-native-record-playback-state segment))
         (data (alist-get 'data segment))
         (remote-status (plist-get capabilities :remote-status))
         (state
          (or (plist-get playback :status)
              (pcase remote-status
                ("materializing" 'preparing)
                ("failed" 'failed)
                (_ 'idle)))))
    (list :state state
          :duration-seconds
          (or (plist-get playback :duration-seconds)
              (alist-get 'duration_seconds data))
          :played-seconds (or (plist-get playback :played-seconds) 0)
          :status-text (plist-get capabilities :status))))

(defun qq-chat--inline-media-resolver (segment owner)
  "Return a generation-bound local-source resolver for SEGMENT and OWNER."
  (let ((qq-media--consumer (or owner (qq-media--current-consumer))))
    (qq-media--owned-callback
     qq-media--consumer qq-media--generation
     (lambda (resolve reject)
       (let ((active t))
         (qq-media-resolve-segment-resource
          segment
          (lambda (resource)
            (when active
              ;; Never hand Appkit a locator: URL acquisition belongs solely
              ;; to Gateway, including when this resolver starts after render.
              (if-let* ((file (alist-get 'file resource))
                          ((appkit-media-file-present-p file)))
                  (funcall resolve
                           (appkit-media-resource-create
                            :file file
                            :mime-type (alist-get 'mime-type resource)))
                (funcall reject "Gateway media has no granted local source"))))
          (lambda (_response reason)
            (when active (funcall reject reason))))
         ;; The Surface retains ready sources.  Cancelling this host only
         ;; fences its delivery, not another consumer's shared materialization.
         (lambda () (setq active nil)))))))

(defun qq-chat--insert-segment-media-line (segment prefix-state properties)
  "Insert one rich media card for SEGMENT using PREFIX-STATE and PROPERTIES."
  (let* ((kind-label (qq-chat--segment-media-kind-label segment))
         (meta (qq-chat--segment-media-meta-line segment))
         (capabilities (qq-media-segment-capabilities segment))
         (transfer (qq-media-segment-transfer segment))
         (preview-position (make-marker))
         (context (qq-chat--segment-media-card-context
                   segment capabilities preview-position))
         (native-record-p (qq-media--native-record-media-id segment))
         (prefix-state (let ((appkit-ui-card-indent-prefix-state prefix-state))
                         (appkit-ui-card-prefix-state))))
    (appkit-chat-ins-insert-media-card
     :kind (qq-chat--segment-media-card-kind segment)
     :title (qq-chat--segment-media-summary segment)
     :details (unless (or native-record-p (string-empty-p meta))
                (list meta))
     :status (unless (or native-record-p transfer)
               (plist-get capabilities :status))
     :transfer (unless native-record-p transfer)
     :prefix prefix-state
     :title-face 'bold
     :meta-face 'shadow
     :properties properties
     :context context
     :open-help-echo (format "Open %s in Emacs or the configured player"
                             (downcase kind-label))
     :body-inserter
     (lambda (card-prefix-state)
       (cond
        (native-record-p
         (let ((control (qq-chat--native-record-control-state
                         segment capabilities)))
           (appkit-chat-ins-insert-voice-note
            :state (plist-get control :state)
            :duration-seconds (plist-get control :duration-seconds)
            :played-seconds (plist-get control :played-seconds)
            :status-text (unless transfer
                           (plist-get control :status-text))
            :prefix card-prefix-state
            :face 'shadow
            :action (plist-get context :open-action)
            :help-echo "Play, pause, or replay this voice note")
           (when transfer
             (apply #'appkit-chat-ins-insert-transfer
                    :prefix card-prefix-state
                    :face 'shadow
                    transfer))))
        ((qq-media-segment-preview-capable-p segment)
         (let* ((preview-start (point))
                (preview
                 (appkit-media-image-with-resource
                  (qq-media-segment-preview-image segment)
                  (qq-media-segment-inline-resource segment)
                  :kind (if (qq-media-videoish-segment-p segment) 'video 'image)
                  :label "qq"
                  :interaction 'media
                  :cache-key (qq-media--segment-resource-key segment)
                  :resolve-function
                  (qq-chat--inline-media-resolver
                   segment (appkit-current-surface))))
                (loading (qq-media-segment-preview-fetching-p segment))
                preview-end)
           (cond
            (preview
             (set-marker preview-position (point))
             (appkit-media-insert-image-slices
              preview nil nil
              (cond
               ((equal (alist-get 'type segment) "mface") "[sticker]")
               ((qq-media-videoish-segment-p segment) "[video]")
               (t "[image]")))
             (setq preview-end (point))
             (insert "\n"))
            (loading
             (insert "[loading preview]\n"))
            (t
             (insert "[preview unavailable]\n")))
           (appkit-ui-apply-line-prefix preview-start (point) card-prefix-state)
           (appkit-ui-append-face preview-start (point) 'shadow)
           (when preview-end
             (appkit-media-add-action-properties
              preview-start preview-end
              (lambda ()
                (if-let* ((host (appkit-media-inline-host-at-point
                                 preview-position)))
                    (appkit-media-inline-host-activate host)
                  (appkit-media-card-call-action 'open context)))
              (format "Open %s" (downcase kind-label)))))))))))

(defun qq-chat--insert-animated-face-segment (segment prefix-state properties)
  "Insert animated face SEGMENT below the avatar's two-line header."
  ;; Keep the tall animation below the avatar's two-line header, not inside it.
  (let ((avatar-tail-start (point)))
    (insert " \n")
    (appkit-ui-apply-line-prefix avatar-tail-start (point) prefix-state)
    (add-text-properties avatar-tail-start (point) properties))
  (let* ((id (or (qq-chat--face-segment-id segment) "?"))
         (description (qq-chat--face-segment-description segment))
         (image (qq-media-face-image id t))
         (animation-start (point)))
    (insert
     (appkit-media-image-display-string
      image (or description (qq-media-face-text-fallback id))))
    (insert "\n")
    (appkit-ui-apply-line-prefix animation-start (point) prefix-state)
    (add-text-properties animation-start (point) properties)))

(defun qq-chat--insert-message-body (message prefix-state properties)
  "Insert MESSAGE content body using PREFIX-STATE and PROPERTIES."
  (let ((segments (alist-get 'segments message))
        (inline-parts nil))
    (cl-labels ((flush-inline ()
                  (when inline-parts
                    (appkit-ui-insert-prefixed-lines
                     prefix-state
                     (mapconcat #'identity (nreverse inline-parts) "")
                     :properties properties)
                    (setq inline-parts nil))))
      (if (or (qq-state-message-recalled-p message)
              (null segments))
          (appkit-ui-insert-prefixed-lines
           prefix-state (qq-chat--message-body message)
           :properties properties)
        (dolist (segment segments)
          (let ((type (alist-get 'type segment)))
            (unless (equal type "reply")
              (cond
               ((qq-chat--forward-segment-p segment)
                (flush-inline)
                (qq-forward-insert-segment
                 (qq-chat--canonical-forward-segment segment)
                 prefix-state properties))
               ((qq-chat--mail-segment-p segment)
                (flush-inline)
                (qq-chat--insert-mail-segment segment prefix-state properties))
               ((qq-chat--wallet-segment-p segment)
                (flush-inline)
                (qq-chat--insert-wallet-segment
                 segment message prefix-state properties))
               ((qq-chat--link-preview-segment-p segment)
                (flush-inline)
                (qq-chat--insert-link-preview-segment
                 segment prefix-state properties))
               ((qq-chat--card-segment-p segment)
                (flush-inline)
                (qq-chat--insert-card-segment segment prefix-state properties))
               ((qq-chat--media-segment-p segment)
                (flush-inline)
                (qq-chat--insert-segment-media-line segment prefix-state properties))
               ((qq-chat--animated-face-segment-p segment)
                (flush-inline)
                (qq-chat--insert-animated-face-segment
                 segment prefix-state properties))
               ((qq-chat--segment-inline-string segment)
                (push (qq-chat--segment-inline-string segment) inline-parts))
               (t
                (push (format "[%s]" (or type "segment")) inline-parts))))))
        (flush-inline)))))

(defun qq-chat--reaction-by-emoji-id (message emoji-id)
  "Return MESSAGE reaction matching EMOJI-ID, or nil."
  (seq-find
   (lambda (reaction)
     (equal (alist-get 'emoji-id reaction) (format "%s" emoji-id)))
   (qq-state-message-reactions message)))

(defun qq-chat--unicode-reaction-string (emoji-id)
  "Return Unicode character represented by decimal EMOJI-ID, or nil."
  (when (and (stringp emoji-id)
             (string-match-p "\\`[0-9]+\\'" emoji-id))
    (let* ((codepoint (string-to-number emoji-id))
           (character (and (<= 0 codepoint #x10ffff)
                           (not (<= #xd800 codepoint #xdfff))
                           (decode-char 'ucs codepoint))))
      (and character (char-to-string character)))))

(defun qq-chat--reaction-display-string (reaction)
  "Return inline display string for normalized REACTION."
  (let ((emoji-id (alist-get 'emoji-id reaction))
        (emoji-type (alist-get 'emoji-type reaction)))
    (if (equal emoji-type "2")
        (or (qq-chat--unicode-reaction-string emoji-id)
            (format "[emoji:%s]" emoji-id))
      (qq-media-face-display-string emoji-id))))

(defun qq-chat--message-reactable-p (message)
  "Return non-nil when MESSAGE can receive a group reaction."
  (and (listp message)
       (not (qq-state-service-message-p message))
       (eq (alist-get 'type (qq-chat--session)) 'group)
       (qq-protocol-message-id-p (alist-get 'server-id message))
       (not (qq-state-message-recalled-p message))
       (qq-chat--message-current-account-p message)))

(defun qq-chat--message-current-account-p (message)
  "Return non-nil when MESSAGE belongs to this chat buffer's account."
  (let ((account-id (and (listp message)
                         (alist-get 'gateway-account-id message))))
    (and (stringp account-id)
         (not (string-empty-p account-id))
         (equal account-id (qq-runtime-current-account-id)))))

(defun qq-chat--message-essence-capable-p (message)
  "Return non-nil when MESSAGE supports an essence mutation."
  (and (listp message)
       (not (qq-state-service-message-p message))
       (eq (alist-get 'type (qq-chat--session)) 'group)
       (qq-protocol-message-id-p (alist-get 'server-id message))
       (not (qq-state-message-recalled-p message))
       (qq-chat--message-current-account-p message)))

(defun qq-chat--pin-capable-p ()
  "Return non-nil when the current conversation supports pinning."
  (qq-directory-conversation-pin-capable-p qq-chat--session-key))

(defun qq-chat-toggle-pin (session-key)
  "Toggle SESSION-KEY's pin state from a chat buffer or the root row."
  (interactive
   (list (or (and (derived-mode-p 'qq-chat-mode) qq-chat--session-key)
             (and (derived-mode-p 'qq-root-mode) (qq-root--session-key-at-point))
             (user-error "qq: select a conversation to toggle its pin state"))))
  (qq-directory-set-conversation-pinned
   session-key (not (eq (alist-get 'pinned (qq-state-session session-key)) t))))

(defun qq-chat-pin ()
  "Pin the current conversation."
  (interactive)
  (qq-directory-set-conversation-pinned qq-chat--session-key t))

(defun qq-chat-unpin ()
  "Unpin the current conversation."
  (interactive)
  (qq-directory-set-conversation-pinned qq-chat--session-key nil))

(defalias 'qq-chat-pin-friend #'qq-chat-pin)
(defalias 'qq-chat-unpin-friend #'qq-chat-unpin)

(defun qq-chat--message-todo-capable-p (message)
  "Return non-nil when MESSAGE supports a group todo mutation."
  (and (listp message)
       (not (qq-state-service-message-p message))
       (eq (alist-get 'type (qq-chat--session)) 'group)
       (qq-protocol-message-id-p (alist-get 'server-id message))
       (not (qq-state-message-recalled-p message))
       (qq-chat--message-current-account-p message)))

(defun qq-chat-toggle-message-reaction (message-id reaction)
  "Toggle normalized REACTION on cached MESSAGE-ID."
  (interactive
   (let* ((message (or (qq-chat--message-at-point)
                       (user-error "qq: no message at point")))
          (reaction (or (car (qq-state-message-reactions message))
                        (user-error "qq: message has no reaction to toggle"))))
     (list (alist-get 'server-id message) reaction)))
  (let* ((message (or (qq-chat--message-by-server-id message-id)
                      (user-error "qq: reaction message is not loaded")))
         (_reactable (or (qq-chat--message-reactable-p message)
                         (user-error "qq: reactions require a live group message")))
         (emoji-id (alist-get 'emoji-id reaction))
         (current (qq-chat--reaction-by-emoji-id message emoji-id))
         (set (not (and current (alist-get 'chosen-p current)))))
    (qq-core-set-message-reaction
     message reaction set
     (lambda (_response)
       (message "qq: reaction %s (%s)"
                (if set "added" "removed") emoji-id)))))

(defun qq-chat-toggle-message-essence (&optional message)
  "Toggle essence state for MESSAGE or the sole operation target."
  (interactive)
  (let* ((message (or message (qq-chat--message-for-interactive)))
         (_capable
          (or (qq-chat--message-essence-capable-p message)
              (user-error "qq: essence requires a live group message")))
         (set (not (eq (alist-get 'essence-p message) t))))
    (qq-core-set-message-essence
     message set
     (lambda (_response)
       (message "qq: essence message %s"
                (if set "set" "removed"))))))

(defun qq-chat--mutate-message-todo (operation &optional message)
  "Apply todo OPERATION to MESSAGE or the sole operation target."
  (let* ((message (or message (qq-chat--message-for-interactive)))
         (_capable
          (or (qq-chat--message-todo-capable-p message)
              (user-error "qq: todo requires a live group message"))))
    (qq-core-set-message-todo
     message operation
     (lambda (_receipt)
       (message "qq: message todo %s" operation)))))

(defun qq-chat-set-message-todo (&optional message)
  "Set MESSAGE or the sole operation target as a group todo."
  (interactive)
  (qq-chat--mutate-message-todo 'set message))

(defun qq-chat-complete-message-todo (&optional message)
  "Complete the group todo for MESSAGE or the sole operation target."
  (interactive)
  (qq-chat--mutate-message-todo 'complete message))

(defun qq-chat-cancel-message-todo (&optional message)
  "Cancel the group todo for MESSAGE or the sole operation target."
  (interactive)
  (qq-chat--mutate-message-todo 'cancel message))

(defun qq-chat--insert-essence-line (message prefix-state properties)
  "Insert an essence badge for MESSAGE using PREFIX-STATE and PROPERTIES."
  (when (eq (alist-get 'essence-p message) t)
    (appkit-ui-insert-prefixed-lines
     prefix-state "★ 精华消息"
     :face 'font-lock-keyword-face
     :properties properties)))

(defun qq-chat--insert-reaction-line (message prefix-state properties)
  "Insert shared appkit reaction chips adapted for QQ MESSAGE."
  (let ((reactions (qq-state-message-reactions message))
        (message-id (alist-get 'server-id message)))
    (when reactions
      (when-let* ((span
                   (appkit-chat-ins-insert-reaction-line
                    reactions
                    :prefix prefix-state
                    :selected-face 'qq-msg-reaction-chosen
                    :unselected-face 'qq-msg-reaction
                    :label-function
                    (lambda (reaction)
                      (concat " "
                              (qq-chat--reaction-display-string reaction)
                              " "
                              (number-to-string
                               (or (alist-get 'count reaction) 0))
                              " "))
                    :selected-p-function
                    (lambda (reaction) (alist-get 'chosen-p reaction))
                    :action-function
                    (lambda (reaction)
                      (qq-chat-toggle-message-reaction
                       message-id reaction))
                    :help-echo-function
                    (lambda (reaction)
                      (let ((emoji-id (alist-get 'emoji-id reaction)))
                        (format "%s %s"
                                (if (alist-get 'chosen-p reaction)
                                    "Remove reaction"
                                  "Add reaction")
                                (or (qq-media-face-name emoji-id)
                                    emoji-id)))))))
        (add-text-properties (car span) (cdr span) properties)))))

(defun qq-chat--insert-compact-message-body (message prefix-state properties short-time)
  "Insert a same-sender continuation body for MESSAGE.

Uses structured segments (so faces render as images).  The row's own status and
SHORT-TIME are appended on the first inline line when the body is pure inline
content."
  (let* ((segments (alist-get 'segments message))
         (status-suffix (qq-chat--status-suffix message))
         (status-time
          (cond
           ((and (not (string-empty-p status-suffix))
                 (stringp short-time)
                 (not (string-empty-p short-time)))
            (concat status-suffix " " short-time))
           ((not (string-empty-p status-suffix)) status-suffix)
           (t short-time)))
         (inline-parts nil)
         (saw-block nil))
    (cl-labels
        ((flush-inline (&optional with-time)
           (when inline-parts
             (let* ((text (mapconcat #'identity (nreverse inline-parts) ""))
                    (start (point)))
               (insert (if (string-empty-p text) "(empty message)" text))
               (when (and with-time
                          (stringp status-time)
                          (not (string-empty-p status-time)))
                 (qq-chat--insert-right-aligned-time
                  status-time
                  (string-width
                   (or (appkit-ui-prefix-state-current prefix-state) ""))
                  t))
               (insert "\n")
               (appkit-ui-apply-line-prefix start (point) prefix-state)
               (add-text-properties start (point) properties)
               (setq inline-parts nil)))))
      (cond
       ((or (qq-state-message-recalled-p message) (null segments))
        (let* ((text (qq-chat--message-body message))
               (start (point)))
          (insert (if (string-empty-p text) "(empty message)" text))
          (when (and (stringp status-time) (not (string-empty-p status-time)))
            (qq-chat--insert-right-aligned-time
             status-time
             (string-width
              (or (appkit-ui-prefix-state-current prefix-state) ""))
             t))
          (insert "\n")
          (appkit-ui-apply-line-prefix start (point) prefix-state)
          (add-text-properties start (point) properties)))
       (t
        (dolist (segment segments)
          (let ((type (alist-get 'type segment)))
            (unless (equal type "reply")
              (cond
               ((qq-chat--forward-segment-p segment)
                (flush-inline nil)
                (setq saw-block t)
                (qq-forward-insert-segment
                 (qq-chat--canonical-forward-segment segment)
                 prefix-state properties))
               ((qq-chat--mail-segment-p segment)
                (flush-inline nil)
                (setq saw-block t)
                (qq-chat--insert-mail-segment
                 segment prefix-state properties))
               ((qq-chat--card-segment-p segment)
                (flush-inline nil)
                (setq saw-block t)
                (qq-chat--insert-card-segment
                 segment prefix-state properties))
               ((qq-chat--media-segment-p segment)
                (flush-inline nil)
                (setq saw-block t)
                (qq-chat--insert-segment-media-line
                 segment prefix-state properties))
               ((qq-chat--segment-inline-string segment)
                (push (qq-chat--segment-inline-string segment) inline-parts))
               (t
                (push (format "[%s]" (or type "segment")) inline-parts))))))
        ;; Time rides on the first (only) inline flush when there is no media card.
        (flush-inline (not saw-block))
        (when (and saw-block
                   (stringp status-time)
                   (not (string-empty-p status-time))
                   (null inline-parts))
          (let ((start (point)))
            (qq-chat--insert-right-aligned-time
             status-time
             (string-width
              (or (appkit-ui-prefix-state-current prefix-state) ""))
             t)
            (insert "\n")
            (appkit-ui-apply-line-prefix start (point) prefix-state)
            (add-text-properties
             start (point)
             (append properties (list 'face 'qq-msg-status))))))))))

(defun qq-chat--set-pending-reply (message)
  "Set MESSAGE as the pending reply target in current chat buffer."
  (when (qq-state-message-recalled-p message)
    (user-error "qq: cannot reply to a recalled message"))
  (qq-chat--ensure-composer-visible)
  (let* ((target (qq-message-reply-target qq-chat--session-key message))
         (label (or (alist-get 'message_id target)
                    (alist-get 'sequence target))))
    (unless target
      (user-error "qq: selected message has no native reply target"))
    (qq-chat--set-reply-message message)
    (qq-chat--update-frame)
    (appkit-chatbuf-focus-input)
    (message "qq: next message will reply to %s" label)))

(defun qq-chat--recall-message-internal (message)
  "Recall MESSAGE from QQ after confirmation."
  (let* ((view (qq-chat--live-current-view))
         (anchor (qq-chat--message-anchor message))
         (message-id (alist-get 'server-id message))
         (poke-p (qq-state-poke-message-p message))
         (target
          (unless poke-p
            (qq-message-recall-target
             (alist-get 'session-key message) message)))
         (target-label (or message-id target)))
    (unless (if poke-p
                (qq-message-poke-recall-capable-p message)
              (and target (qq-message-recall-capable-p message)))
      (user-error
       "qq: selected message is not currently eligible for recall"))
    (when (y-or-n-p (format "Recall message %s? " target-label))
      (setq message (qq-chat--message-in-view view anchor)
            poke-p (qq-state-poke-message-p message))
      (unless (if poke-p
                  (qq-message-poke-recall-capable-p message)
                (qq-message-recall-capable-p message))
        (user-error "qq: selected message is no longer eligible for recall"))
      (if poke-p
          (qq-core-recall-poke message)
        (qq-core-recall-message message)))))

(defun qq-chat--message-sender-color-key (message)
  "Return MESSAGE's stable sender key for shared name coloring."
  ;; Mentions identify users by UIN, so headings use that same immutable key
  ;; whenever it is available.  Both surfaces then choose one AppKit color.
  (or (qq-chat--present-string (alist-get 'sender-id message))
      (qq-chat--present-string (alist-get 'sender-native-id message))
      (qq-chat--message-sender-name message)))

(defun qq-chat--message-title-face (message)
  "Return the identity-colored sender title face for MESSAGE.

Every ordinary sender uses Appkit's deterministic identity color.  The current
account additionally keeps QQ's self-title emphasis."
  (let ((color-face
         (appkit-name-color-face
          (qq-chat--message-sender-color-key message))))
    (cond
     ((alist-get 'self-p message)
      (if color-face
          (list color-face 'qq-msg-self-title)
        'qq-msg-self-title))
     (color-face
      (list color-face 'qq-msg-user-title))
     (t 'qq-msg-user-title))))

(defun qq-chat--message-avatar-prefixes (message)
  "Return shared telega-style two-line avatar prefixes for MESSAGE."
  (let* ((sender-id (alist-get 'sender-id message))
         (image (qq-media-message-avatar-image message))
         (prefixes
          (appkit-chat-avatar-prefixes
           image "@"
           :pixel-size (appkit-chat-avatar-two-line-pixel-size)
           :resize t))
         (avatar-properties
          (list 'mouse-face 'highlight
                'help-echo "Open sender avatar"
                'qq-chat-avatar-sender-id sender-id)))
    (dolist (key '(:header :first-body))
      (let ((prefix (copy-sequence (plist-get prefixes key))))
        (when (and (stringp prefix) (> (length prefix) 0))
          (add-text-properties 0 (length prefix) avatar-properties prefix))
        (setq prefixes (plist-put prefixes key prefix))))
    prefixes))

(cl-defun qq-chat-message-layout
    (message &key compact (avatar-p t))
  "Return shared presentation layout for MESSAGE.

COMPACT makes the first body line use the ordinary continuation prefix.
AVATAR-P controls whether the shared two-line avatar occupies the heading and
first body line.  The returned plist owns a fresh mutable body prefix state."
  (let* ((avatar-prefixes
          (and avatar-p (qq-chat--message-avatar-prefixes message)))
         (header-prefix (or (plist-get avatar-prefixes :header) ""))
         (body-first-prefix (or (plist-get avatar-prefixes :first-body) "  "))
         (body-rest-prefix (or (plist-get avatar-prefixes :rest-body) "  ")))
    (list :header-prefix header-prefix
          :body-rest-prefix body-rest-prefix
          :body-prefix-state
          (if compact
              (appkit-ui-make-prefix-state
               body-rest-prefix body-rest-prefix)
            (appkit-ui-make-prefix-state
             body-first-prefix body-rest-prefix)))))

(defun qq-chat--message-group-member (message)
  "Return current membership for MESSAGE's exact group and sender.
Never infer a forwarded message's origin from the containing chat."
  (when (and (equal (alist-get 'message-type message) "group")
             (not (qq-state-service-message-p message))
             (qq-protocol-group-uin-p (alist-get 'group-id message))
             (or (null (alist-get 'gateway-account-id message))
                 (equal (alist-get 'gateway-account-id message)
                        (qq-runtime-current-account-id))))
    (qq-directory-group-member
     (alist-get 'group-id message)
     (alist-get 'sender-id message)
     (alist-get 'sender-native-id message))))

(defun qq-chat--insert-sender-badges (message)
  "Insert current group membership badges independently of MESSAGE's names."
  (when-let* ((member (qq-chat--message-group-member message)))
    (let* ((role (alist-get 'role member))
           (title (qq-directory-member-active-title member))
           (tag (or title (pcase role
                            ("owner" "群主")
                            ("admin" "管理员"))))
           (level (alist-get 'level member)))
      (when tag
        (insert " "
                (propertize
                 tag 'face (pcase role
                             ("owner" 'qq-msg-owner-sender-tag)
                             ("admin" 'qq-msg-admin-sender-tag)
                             (_ 'qq-msg-sender-tag))
                 'help-echo "Current group membership (not historical)")))
      (when (and (integerp level) (> level 0))
        (insert " " (propertize (format "Lv.%d" level)
                                'face 'qq-msg-sender-level))))))

(cl-defun qq-chat-insert-message-heading
    (message properties layout
             &key (title-face nil title-face-p)
             (status-suffix nil status-suffix-p))
  "Insert the shared heading for MESSAGE and return its body prefix state.

PROPERTIES cover the generated heading.  LAYOUT must come from
`qq-chat-message-layout'.  TITLE-FACE and STATUS-SUFFIX default to the normal
QQ message presentation when omitted."
  (let* ((header-prefix (plist-get layout :header-prefix))
         (body-rest-prefix (plist-get layout :body-rest-prefix))
         (header-start (point)))
    (qq-chat--insert-message-sender
     message (if title-face-p
                 title-face
               (qq-chat--message-title-face message)))
    (qq-chat--insert-sender-badges message)
    (insert (if status-suffix-p
                (or status-suffix "")
              (qq-chat--status-suffix message)))
    (qq-chat--insert-right-aligned-time
     (qq-chat--format-time (alist-get 'time message))
     (string-width header-prefix)
     t)
    (insert "\n")
    (appkit-ui-apply-line-prefix
     header-start (point)
     (appkit-ui-make-prefix-state header-prefix body-rest-prefix))
    (appkit-ui-append-face header-start (point) 'appkit-chat-heading)
    (add-text-properties header-start (point) properties)
    (plist-get layout :body-prefix-state)))

(defun qq-chat--render-message (message context)
  "Insert one formatted MESSAGE block using projected CONTEXT.

Visual model (using shared Appkit presentation):
- optional date / unread bars above the node
- heading row: avatar + sender + status + time (`appkit-chat-heading')
- optional reply preview (`appkit-chat-inline-reply')
- body with light indent (no `>>'/`|' gutters)
- no per-message action button row (use the message operate menu on `o')"
  (let* ((anchor (qq-chat--message-anchor message))
         (start (point))
         (insert-date (plist-get context :insert-date))
         (insert-unread (plist-get context :insert-unread))
         (title-face (qq-chat--message-title-face message))
         (reply-data (qq-chat--message-reply-data message))
         (properties (qq-chat--message-line-properties message anchor))
         (status-suffix (qq-chat--status-suffix message))
         (compact (plist-get context :compact))
         (selected (qq-chat-message-selected-p message))
         (ordinary-message-p
          (not (qq-state-service-message-p message)))
         (layout
          (qq-chat-message-layout
           message
           :compact compact
           :avatar-p ordinary-message-p))
         (header-prefix (plist-get layout :header-prefix))
         (body-rest-prefix (plist-get layout :body-rest-prefix))
         (body-prefix-state (plist-get layout :body-prefix-state))
         (short-time (qq-chat--format-time-short (alist-get 'time message)))
         content-start)
    (when (and (stringp insert-date) (not (string-empty-p insert-date)))
      (qq-chat--insert-date-separator-row (qq-chat--message-day-label insert-date)))
    (when insert-unread
      (appkit-chat-read-insert-divider (qq-chat--line-fill-column)))
    (setq content-start (point))
    (cond
     ((qq-state-message-recalled-p message)
      ;; Stub path for `qq-chat-show-recalled-messages' (telega deleted style).
      (let ((header-start (point)))
        (qq-chat--insert-message-sender message title-face)
        (insert status-suffix)
        (qq-chat--insert-right-aligned-time
         (qq-chat--format-time (alist-get 'time message))
         (string-width header-prefix)
         t)
        (insert "\n")
        (appkit-ui-apply-line-prefix
         header-start (point)
         (appkit-ui-make-prefix-state header-prefix body-rest-prefix))
        (add-text-properties
         header-start (point)
         (append properties (list 'face 'qq-msg-deleted)))
        (appkit-ui-insert-prefixed-lines
         body-prefix-state
         (qq-chat--message-body message)
         :properties (append properties (list 'face 'qq-msg-deleted)))))
     ((qq-state-poke-message-p message)
      (qq-chat--insert-poke-message message properties))
     ((qq-state-gray-tip-message-p message)
      (qq-chat--insert-gray-tip-message message properties))
     (compact
      ;; Same-sender continuations still need segment-rich bodies (faces,
      ;; images, …).  Never dump plain `preview' or legacy wire text here;
      ;; doing so would show "[face:178]" while the image path already worked.
      (when reply-data
        (qq-chat--insert-reply-preview-line
         reply-data properties body-prefix-state context))
      (qq-chat--insert-compact-message-body
       message body-prefix-state properties short-time))
     (t
      (qq-chat-insert-message-heading
       message properties layout
       :title-face title-face
       :status-suffix status-suffix)
      (when reply-data
        (qq-chat--insert-reply-preview-line
         reply-data properties body-prefix-state context))
      (qq-chat--insert-message-body message body-prefix-state properties)))
    (unless (or (qq-state-message-recalled-p message)
                (qq-state-service-message-p message))
      (qq-chat--insert-translation message body-prefix-state)
      (qq-chat--insert-essence-line message body-prefix-state properties)
      (qq-chat--insert-reaction-line message body-prefix-state properties))
    (when selected
      (appkit-chat-ins-apply-message-selection content-start (point)))
    (add-text-properties start (point) properties)))

(defun qq-chat-render ()
  "Synchronize current chat frame and projected timeline from local state."
  (interactive)
  (unless qq-chat--session-key
    (user-error "qq: this buffer is not bound to a session"))
  (qq-chat--ensure-view)
  (qq-chat--header-line-update)
  ;; Establish the EWOC rows before the trailing composer.  On an empty EWOC
  ;; the footer's tail boundary is not stable until first reconciliation;
  ;; binding input first can leave the prompt inside the message region.
  (qq-chat--sync-timeline)
  (qq-chat--update-frame)
  (qq-chat--update-group-timeline-mode)
  ;; A detached/replacement EWOC can temporarily own its footer boundary at
  ;; the old tail.  Materialize only after timeline and frame reconciliation;
  ;; generated composer replacement remains undo-free and property-preserving.
  (when qq-chat--send-sync-request
    (qq-chat--materialize-pending-send-restoration)))

(defun qq-chat-refresh ()
  "Rebuild the authoritative latest history window."
  (interactive)
  (unless qq-chat--session-key
    (user-error "qq: this buffer is not bound to a session"))
  (if qq-search--inline-p (qq-search-refresh)
    (qq-chat-return-to-latest t)))

(defun qq-chat-load-newer-messages (&optional quiet)
  "Extend this chat's timeline by one newer page."
  (interactive)
  (unless qq-chat--session-key
    (user-error "qq: this buffer is not bound to a session"))
  (cond
   (qq-search--inline-p
    (unless quiet (message "qq: newest search matches are already loaded")))
   ((not (qq-chat--history-known-p))
    (if (qq-chat--loading)
        (unless quiet (message "qq: initial history is still loading"))
      (qq-chat-return-to-latest)))
   ((qq-chat--loading)
    (unless quiet (message "qq: history load already in progress")))
   ((not (plist-get (qq-chat--model) :has-newer))
    (unless quiet (message "qq: latest history is already loaded")))
   (t (appkit-surface-send (qq-chat--ensure-view) '(qq-timeline-page newer)))))

(defun qq-chat--goto-latest-window-end ()
  "Move to the newest loaded row, or to the composer end."
  (let ((latest (cdr (car (last (plist-get (qq-chat--model) :rows))))))
    (unless (and latest (qq-chat--goto-loaded-message latest nil))
      (goto-char (or (appkit-chatbuf-input-start-position) (point-max))))))

(defun qq-chat-return-to-latest (&optional force)
  "Open this chat's timeline at its latest rows and move to its end.

With FORCE, reopen even if the timeline already follows latest.
This command navigates without sending an explicit read-all report."
  (interactive)
  (unless qq-chat--session-key
    (user-error "qq: this buffer is not bound to a session"))
  (when qq-search--inline-p (qq-search-cancel-filter t))
  (cond
   ((plist-get (qq-chat--model) :opening)
    (message "qq: history load already in progress"))
   ((and (not force)
         (qq-chat--history-known-p)
         (not (plist-get (qq-chat--model) :has-newer)))
    (qq-chat--goto-latest-window-end)
    (message "qq: latest history"))
   (t
    (qq-chat--load-initial-history (current-buffer) qq-chat--session-key))))

(defun qq-chat-load-older-messages (&optional quiet)
  "Extend this chat's timeline by one older page."
  (interactive)
  (unless qq-chat--session-key
    (user-error "qq: this buffer is not bound to a session"))
  (cond
   (qq-search--inline-p (qq-search--load-more))
   ((not (qq-chat--history-known-p))
    (unless quiet (message "qq: history window is not initialized")))
   ((not (plist-get (qq-chat--model) :has-older))
    (unless quiet (message "qq: no older messages available")))
   ((qq-chat--loading)
    (unless quiet (message "qq: history load already in progress")))
   (t (appkit-surface-send (qq-chat--ensure-view) '(qq-timeline-page older)))))

(defun qq-chat--restore-failed-send
    (buffer session-key cleared-revision draft-state aux-state
            &optional captured-view)
  "Restore one failed send when BUFFER still has its cleared composer slot.

SESSION-KEY prevents a reused buffer from receiving another chat's draft.
CLEARED-REVISION must still match Appkit's monotonic input/aux revision.
DRAFT-STATE preserves rich input properties and AUX-STATE preserves its reply.
Non-nil CAPTURED-VIEW identifies an asynchronous callback, but presentation is
requested from the exact live view resolved at restoration time.  Return
non-nil only when restoration happened."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (equal qq-chat--session-key session-key)
                 (integerp cleared-revision)
                 (= cleared-revision
                    (appkit-chatbuf-composer-revision)))
        (appkit-chatbuf-input-state-set draft-state :reset-history-p t)
        (if aux-state
            (appkit-chatbuf-aux-set (copy-tree aux-state))
          (appkit-chatbuf-aux-reset))
        (if captured-view
            (let ((presentation-view (qq-chat--live-current-view)))
              ;; The request that failed may belong to a retired view.  Resolve
              ;; the canonical presentation target only after logical restore.
              (setq qq-chat--send-sync-request
                    (list :view presentation-view))
              (when presentation-view
                (qq-runtime-invalidate presentation-view :frame-p t :position 'preserve)))
          (qq-chat--render-canonical-input)
          (qq-chat--update-frame)
          (appkit-chatbuf-focus-input))
        (qq-chat--maybe-update-my-action-from-input)
        t))))

(defun qq-chat-send-message ()
  "Send current chat draft."
  (interactive)
  (unless qq-chat--session-key
    (user-error "qq: this buffer is not bound to a session"))
  (qq-chat--ensure-composer-visible)
  ;; Never submit a stale canonical cache or a partially edited rich object.
  (appkit-chatbuf-input-prune-broken-objects)
  (qq-chat--sync-draft-from-buffer)
  (let* ((buffer (current-buffer))
         (session-key qq-chat--session-key)
         (view (qq-chat--ensure-view))
         (draft-state (appkit-chatbuf-input-state))
         (aux-state (copy-tree (appkit-chatbuf-aux-state)))
         (text (qq-chat--current-draft-string))
         (reply-message (qq-chat--reply-message))
         (reply-target
          (and reply-message
               (or (qq-message-reply-target session-key reply-message)
                   (user-error "qq: reply draft has no native target"))))
         (content-segments (qq-chat--current-input-segments))
         (send-segments (append
                         (when reply-target
                           `(((type . "reply")
                              (data . ((target . ,reply-target))))))
                         content-segments))
         (raw-message (unless (appkit-chatbuf-input-has-objects-p)
                        text))
         cleared-revision
         clipboard-owner
         settled-p)
    (if (and (not (appkit-chatbuf-input-has-objects-p))
             (string-empty-p (string-trim text)))
        (message "qq: draft is empty")
      (qq-chat--push-input-history text)
      (condition-case err
          (progn
            (setq clipboard-owner (qq-chat--retain-clipboard-send draft-state))
            (appkit-chatbuf-input-state-clear :reset-history-p t)
            (setq cleared-revision (appkit-chatbuf-composer-revision))
            (appkit-chatbuf-aux-reset)
            (setq cleared-revision (appkit-chatbuf-composer-revision))
            ;; telega: empty input after send → chatActionCancel
            (qq-chat--set-my-action 'cancel)
            (qq-chat--render-canonical-input)
            (qq-chat--update-frame)
            (qq-core-send-message
             session-key send-segments raw-message
             (lambda (_response)
               (unless settled-p
                 (setq settled-p t)
                 (qq-chat--release-clipboard-send buffer clipboard-owner)))
             (lambda (response reason)
               (unless settled-p
                 (setq settled-p t)
                 (unwind-protect
                     (let ((restored
                            (and
                             (or (null clipboard-owner)
                                 (qq-chat--clipboard-send-current-p
                                  buffer clipboard-owner))
                             (qq-chat--restore-failed-send
                              buffer session-key cleared-revision draft-state
                              aux-state view))))
                       (qq-chat--default-error
                        response
                        (if restored
                            (format "%s (draft restored)"
                                    (or reason "send failed"))
                          reason)))
                   (qq-chat--release-clipboard-send buffer clipboard-owner))))))
        ((error quit)
         (unless settled-p
           (setq settled-p t)
           (unwind-protect
               (when (or (null clipboard-owner)
                         (qq-chat--clipboard-send-current-p buffer clipboard-owner))
                 (qq-chat--restore-failed-send
                  buffer session-key cleared-revision draft-state aux-state))
             (qq-chat--release-clipboard-send buffer clipboard-owner)))
         (signal (car err) (cdr err)))))))

(defun qq-chat--return-input (arg)
  "Complete the token, send the draft, or insert a newline with ARG."
  (interactive "P")
  (cond
   (arg
    (insert "\n"))
   ((qq-completion-token-at-point)
    (or (qq-completion-complete)
        (message "qq: no completion candidate; C-c C-c sends literally")))
   (t
    (qq-chat-send-message))))

(defun qq-chat--return-timeline ()
  "Activate inline media or open a filtered message in normal context."
  (interactive)
  (if-let* ((host (or (appkit-media-inline-host-at-point)
                     (and (> (point) (point-min))
                          (appkit-media-inline-host-at-point (1- (point)))))))
      (appkit-media-inline-host-activate host)
    (when qq-search--inline-p (qq-search-open-hit))))

(defun qq-chat-return-dwim (arg)
  "Activate the timeline or complete, send, or insert a newline with ARG.

An unresolved @member, /face, /favorite, or :emoji: token always owns RET.
This prevents a completion frontend with no preselected row from falling
through and sending the query as plain text.  `C-c C-c' remains the explicit
way to send literal token text."
  (interactive "P")
  (qq-chat--ensure-composer-visible)
  (let ((current-prefix-arg arg))
    (appkit-chatbuf-dispatch-return
     #'qq-chat--return-input #'qq-chat--return-timeline)))

(defun qq-chat-edit-draft ()
  "Focus the editable draft and enter insert state when Evil is already active."
  (interactive)
  (qq-chat--ensure-composer-visible)
  (appkit-chatbuf-enter-input))

(defun qq-chat-complete ()
  "Complete at composer point, or move to the composer from the timeline."
  (interactive)
  (if (appkit-chatbuf-point-in-input-p)
      (or (qq-completion-complete)
          (message "qq: no completion at point"))
    (qq-chat-edit-draft)))

(defun qq-chat-draft-prev ()
  "Replace draft with previous entry from input history."
  (interactive)
  (condition-case _err
      (progn
        (appkit-chatbuf-input-history-prev)
        (qq-chat--sync-draft-from-buffer)
        (appkit-chatbuf-focus-input))
    (user-error
     (user-error "qq: no previous inputs"))))

(defun qq-chat-draft-next ()
  "Move draft navigation toward more recent input history."
  (interactive)
  (condition-case _err
      (progn
        (appkit-chatbuf-input-history-next)
        (qq-chat--sync-draft-from-buffer)
        (appkit-chatbuf-focus-input))
    (user-error
     (user-error "qq: not currently browsing input history"))))

(defun qq-chat-clear-draft ()
  "Clear current draft and exit input history navigation."
  (interactive)
  (qq-chat--set-draft ""))

(defun qq-chat-reply-to-message (&optional message)
  "Reply to MESSAGE or the sole operation target."
  (interactive)
  (qq-chat--set-pending-reply
   (or message (qq-chat--message-for-interactive))))

(defun qq-chat-delete-message (&optional message)
  "Persistently delete MESSAGE or the sole operation target from local history."
  (interactive)
  (let* ((message (or message (qq-chat--message-for-interactive)))
         (view (qq-chat--live-current-view))
         (anchor (qq-chat--message-anchor message)))
    (unless (qq-message-delete-local-capable-p message)
      (user-error "qq: local deletion requires a canonical timeline row"))
    (when (y-or-n-p
           "Delete this message locally? The QQ server copy is unaffected. ")
      (setq message (qq-chat--message-in-view view anchor))
      (unless (qq-message-delete-local-capable-p message)
        (user-error "qq: selected message can no longer be deleted locally"))
      (qq-core-delete-message-local message))))

(defun qq-chat-recall-message (&optional message)
  "Recall MESSAGE or the sole operation target from QQ."
  (interactive)
  (qq-chat--recall-message-internal
   (or message (qq-chat--message-for-interactive))))

(defun qq-chat-react-to-message (&optional message reaction)
  "Add REACTION to MESSAGE, defaulting to the sole target and a chooser.

REACTION is a normalized emoji identity.  For compatibility, a decimal string
or integer is accepted as a QQ base face id.  Interactive use offers every
reaction kind supported by Gateway: QQ base faces and scalar Unicode emoji.
Clicking an existing reaction chip performs add/remove toggle instead."
  (interactive)
  (let* ((message (or message (qq-chat--message-for-interactive)))
         (_initial-reactable (or (qq-chat--message-reactable-p message)
				 (user-error "qq: reactions require a live group message")))
         (view (qq-chat--live-current-view))
         (anchor (qq-chat--message-anchor message))
         (prompt-p (null reaction))
         (reaction (or reaction (qq-completion-read-reaction)))
         (message (if prompt-p
                      (qq-chat--message-in-view view anchor)
                    message))
         (_reactable (or (qq-chat--message-reactable-p message)
                         (user-error "qq: reactions require a live group message")))
         (reaction
          (cond
           ((and (listp reaction)
                 (alist-get 'emoji-id reaction)
                 (alist-get 'emoji-type reaction))
            reaction)
           ((or (integerp reaction)
                (and (stringp reaction)
                     (string-match-p "\\`[0-9]+\\'" reaction)))
            `((emoji-id . ,(format "%s" reaction)) (emoji-type . "1")))
           ((and (stringp reaction) (= (length reaction) 1))
            `((emoji-id . ,(number-to-string (aref reaction 0)))
              (emoji-type . "2")))
           (t (user-error "qq: reaction requires a supported emoji identity"))))
         (emoji-id (alist-get 'emoji-id reaction)))
    (qq-core-set-message-reaction
     message reaction t
     (lambda (_response)
       (message "qq: reaction added (%s)" emoji-id)))))

(defun qq-chat-open-resource-at-point ()
  "Open the exact media card at point, or the message's primary media."
  (interactive)
  (appkit-media-card-open))

(defun qq-chat-open-avatar-at-point (&optional message)
  "Open MESSAGE's sender avatar, or the avatar currently under point."
  (interactive)
  (if message
      (qq-media-open-message-avatar message)
    (if-let* ((user-id (get-text-property
                       (point) 'qq-chat-gray-tip-user-id)))
        (qq-media-open-user-avatar user-id)
      (qq-media-open-message-avatar
       (or (qq-chat--message-at-point)
           (user-error "qq: no message at point"))))))

(defun qq-chat-open-user-at-point (&optional message)
  "Open MESSAGE's sender page, or the user currently under point."
  (interactive)
  (let* ((gray-tip-user-id
          (unless message
            (get-text-property (point) 'qq-chat-gray-tip-user-id)))
         (message (or message (qq-chat--message-at-point)
                      (user-error "qq: no message at point"))))
    (if gray-tip-user-id
        (progn
          (unless (and (qq-protocol-user-uin-p gray-tip-user-id)
                       (not (equal gray-tip-user-id "0")))
            (user-error "qq: service message has no user profile"))
          (qq-user-open gray-tip-user-id))
      (qq-chat--open-message-sender-profile message))))

(defun qq-chat-open-peer-user ()
  "Open the current private peer's user page."
  (interactive)
  (let* ((session (qq-state-session qq-chat--session-key))
         (type (alist-get 'type session))
         (user-id (or (alist-get 'peer-uin session)
                      (alist-get 'target-id session))))
    (unless (and (eq type 'private) (qq-protocol-user-uin-p user-id))
      (user-error "qq: current chat has no user profile"))
    (qq-user-open user-id)))

(defun qq-chat-open-peer-info ()
  "Open the current private user or group profile page."
  (interactive)
  (let* ((session (or (qq-state-session qq-chat--session-key)
                      (user-error "qq: current chat has no session")))
         (type (alist-get 'type session))
         (target-id (or (and (eq type 'private) (alist-get 'peer-uin session))
                        (alist-get 'target-id session))))
    (pcase type
      ('private
       (unless (qq-protocol-user-uin-p target-id)
         (user-error "qq: current chat has no user profile"))
       (qq-user-open target-id))
      ('group
       (unless (qq-protocol-group-uin-p target-id)
         (user-error "qq: current chat has no group profile"))
       (qq-group-open target-id))
      (_ (user-error "qq: current chat has no profile page")))))

(defun qq-chat-cancel-reply ()
  "Cancel only the reply target, preserving the draft and active search."
  (interactive)
  (qq-chat--set-reply-message nil)
  (qq-chat--update-frame)
  (message "qq: reply target cleared"))

(defun qq-chat-cancel-dwim ()
  "Cancel chat search, then reply context or draft input.

Bound to `C-c C-k' (also ESC ESC / C-M-c).  While searching,
leave the draft and reply target intact."
  (interactive)
  (cond
   (qq-search--inline-p (qq-search-cancel-filter))
   ((and (boundp 'qq-chat-search--query) qq-chat-search--query)
    (qq-chat-search-cancel)
    (message "qq: chat search cleared"))
   ((qq-chat--reply-message)
    (qq-chat-cancel-reply))
   ((or (appkit-chatbuf-input-history-active-p)
        (not (string-empty-p (string-trim (qq-chat--current-draft-string)))))
    (qq-chat-clear-draft)
    (message "qq: draft cleared"))
   (t
    (message "qq: nothing to cancel"))))

(defun qq-chat--apply-read-state-change ()
  "Synchronize unread-divider context after session read-state changes."
  (qq-chat--header-line-update)
  (qq-chat--sync-timeline)
  t)

(defun qq-chat-read-all ()
  "Mark this conversation read and present its latest window after confirmation."
  (interactive)
  (unless qq-chat--session-key (user-error "qq: this buffer is not bound to a session"))
  (qq-chat--ensure-view)
  (qq-runtime-request (list 'read-conversation qq-chat--session-key) nil 'read-all))

(defun qq-chat--poke-session (session-key)
  "Return poke-capable SESSION-KEY metadata, or signal `user-error'."
  (unless session-key
    (user-error "qq: this buffer is not bound to a session"))
  (let ((session (qq-state-session session-key)))
    (unless (memq (alist-get 'type session) '(private group))
      (user-error "qq: this conversation does not support pokes"))
    session))

(defun qq-chat--poke-target-id-p (target-id)
  "Return non-nil when TARGET-ID identifies a real QQ user."
  (and (qq-protocol-user-uin-p target-id)
       (not (equal target-id "0"))))

(defun qq-chat--validate-poke-target (session target-id)
  "Return TARGET-ID when it is valid for poke-capable SESSION.

A private conversation can only target its peer or the current user.  Group
targets are established by the message-sender or native member-selection
paths before reaching this validator."
  (unless (qq-chat--poke-target-id-p target-id)
    (user-error "qq: poke target is not a valid QQ user"))
  (when (eq (alist-get 'type session) 'private)
    (let ((peer-id (alist-get 'target-id session))
          (self-id (qq-state-self-user-id)))
      (unless (or (equal target-id peer-id)
                  (and (qq-chat--poke-target-id-p self-id)
                       (equal target-id self-id)))
        (user-error "qq: private poke target must be the peer or self"))))
  target-id)

(defun qq-chat--send-poke-to (session-key target-id)
  "Poke TARGET-ID in SESSION-KEY using the strict peer/target contract."
  (qq-chat--validate-poke-target
   (qq-chat--poke-session session-key) target-id)
  (qq-core-send-poke
   session-key target-id
   (lambda (_response)
     (message "qq: poke sent"))))

(defun qq-chat--poke-sender-at-point (&optional message)
  "Return MESSAGE's sender ID, defaulting to the message at point.

Only `sender-id' has this meaning.  In particular, a message's `target-id'
denotes its conversation peer (or a poke decoration target), so it must not
be used to infer the sender."
  (let* ((message (or message (qq-chat--message-at-point)
                      (user-error "qq: no message at point")))
         (sender-id (alist-get 'sender-id message)))
    (unless (qq-chat--poke-target-id-p sender-id)
      (user-error "qq: message sender cannot be poked"))
    sender-id))

(defun qq-chat-poke-sender (&optional message)
  "Poke MESSAGE's sender, defaulting to the message at point.

This works for both another user and the current user's own messages."
  (interactive)
  (qq-chat--send-poke-to
   qq-chat--session-key
   (qq-chat--poke-sender-at-point message)))

(defun qq-chat-send-poke (&optional target-id)
  "Choose and poke TARGET-ID in the current chat.

Interactive private chats offer the peer and the current user.  Interactive
group chats search the native member directory and then require an explicit
member choice.  A non-nil programmatic TARGET-ID bypasses the chooser but is
still validated by the strict API contract."
  (interactive)
  (let* ((session-key qq-chat--session-key)
         (session (qq-chat--poke-session session-key)))
    (if target-id
        (qq-chat--send-poke-to session-key target-id)
      (let* ((buffer (current-buffer))
             (message (ignore-errors (qq-chat--message-at-point)))
             (sender-id (alist-get 'sender-id message))
             (initial-user-id
              (and (eq (alist-get 'type session) 'group)
                   (qq-chat--poke-target-id-p sender-id)
                   sender-id)))
        (qq-completion-read-poke-target
         session-key
         (lambda (selected-user-id)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (when (equal qq-chat--session-key session-key)
                 (qq-chat--send-poke-to session-key selected-user-id)))))
         initial-user-id)))))

(defvar-keymap qq-chat-navigation-map
  :doc "QQ chat navigation actions."
  ">" #'qq-chat-read-all
  "r" #'qq-chat-read-all
  "x" #'qq-chat-goto-pop-message
  "s" #'qq-chat-search
  "p" #'qq-chat-search-previous
  "n" #'qq-chat-search-next)

(defvar-keymap qq-chat-mode-map
  :doc "Keymap for `qq-chat-mode'."
  "C-c r" #'qq-chat-read-all
  "C-c C-r" #'qq-chat-search
  "C-c C-s" #'qq-chat-search-forward
  "C-c P" #'qq-chat-send-poke
  "C-c i" #'qq-chat-open-peer-info
  "TAB" #'qq-chat-complete
  "<tab>" #'qq-chat-complete
  "C-M-i" #'qq-chat-complete
  "M-p" #'qq-chat-draft-prev
  "M-n" #'qq-chat-draft-next
  "C-c C-e" #'qq-chat-attach-emoji
  ;; telega: ESC ESC / C-M-c also cancel reply-or-edit aux.
  "ESC ESC" #'qq-chat-cancel-dwim
  "C-M-c" #'qq-chat-cancel-dwim)

(appkit-bindings-define 'qq-chat-mode-map 'input
  'return #'qq-chat-return-dwim
  'newline #'newline
  'submit #'qq-chat-send-message
  'input-options #'qq-chat-composer-transient
  'attach #'qq-chat-attach
  'attach-file #'qq-chat-attach-file
  'attach-clipboard #'qq-chat-attach-clipboard
  'cancel #'qq-chat-cancel-dwim
  'app-menu #'qq-chat-transient
  'navigation qq-chat-navigation-map
  'filter-search #'qq-search-chat
  'filter-cancel #'qq-search-cancel-filter
  'refresh #'qq-chat-refresh
  'help #'describe-mode)

(define-derived-mode qq-chat-mode appkit-chatbuf-mode "QQ-Chat"
  "Major mode for emacs-qq chat buffers.

Message actions use point + keys (`r'/`d'/`!'/`P'/`o'/`a' on the timeline) or
`qq-transient-msg-operate' (timeline `o').  `qq-chat-transient'
(timeline `?') opens timeline or input operations at point.
Open composer operations directly with `C-c C-o'.
Attach from clipboard with `C-c C-v' (telega-style)."
  ;; Keep vertically sliced two-line avatars visually contiguous.
  (setq-local line-spacing 0)
  (appkit-chatbuf-reset-state 32)
  (setq-local appkit-chatbuf-input-sync-function
              #'qq-chat--sync-draft-from-buffer)
  (qq-completion-setup)
  (setq-local qq-chat--send-sync-request nil)
  (setq-local qq-chat--message-selection nil)
  (setq-local qq-chat--forward-presentation nil)
  (setq-local qq-chat--forward-sync-request nil)
  (setq-local qq-chat--forward-plan-owner (list 'forward-plan-owner))
  (setq-local appkit-media-card-fallback-context-function
              #'qq-chat--media-card-fallback-context)
  (setq-local qq-chat--pending-jump-center nil)
  (setq-local qq-chat--messages-pop-ring
              (make-ring (max 1 qq-chat-messages-pop-ring-size)))
  ;; telega-style: M-x yank-media also drops images into the composer.
  (when (fboundp 'yank-media-handler)
    (funcall #'yank-media-handler
             '(image/png image/jpeg image/bmp)
             (lambda (mime-type data)
               (qq-chat--yank-media mime-type data nil))))
  (add-hook 'post-command-hook #'qq-chat--post-command t t)

  (add-hook 'kill-buffer-hook #'qq-chat--clear-forward-presentation nil t)
  (appkit-chatbuf-use-timeline-mode #'qq-chat-timeline-mode))

(defun qq-chat--load-initial-history (buffer session-key)
  "Open BUFFER's timeline for exact SESSION-KEY at its latest rows."
  (with-current-buffer buffer
    (when (equal qq-chat--session-key session-key)
      (appkit-surface-send (qq-chat--ensure-view) '(qq-timeline-start (latest))))))

(defun qq-chat--open-buffer (session-key &optional preview-p)
  "Create or reuse SESSION-KEY's chat without loading history.
PREVIEW-P selects a separate context view of the same conversation.
Only a newly created view focuses the composer; reuse preserves position."
  (unless session-key (user-error "qq: session key is required"))
  (let ((owner (or (qq-runtime-current-account-id)
                   (user-error "qq: Select a QQ account first")))
        created-p)
    (qq-runtime-with-account owner
      (qq-state-upsert-session session-key nil nil)
      (let* ((view
              (qq-runtime-open-account-surface
               :account-id owner
               :id (list (if preview-p 'chat-preview 'chat) session-key)
               :mode 'qq-chat-mode
               :update #'qq-chat--surface-update
               :buffer-name (if preview-p
                                (format "*QQ 上下文: %s:%s*" owner session-key)
                              (qq-chat--buffer-name session-key))
               :state session-key
               :setup (lambda (surface)
                        (qq-timeline-bind-surface surface)
                        (setq created-p t)
                        (setq-local qq-chat--session-key session-key
                                    qq-chat--preview-p preview-p))
               :render-function #'qq-chat--render
               :recover-function #'qq-chat--recover-render))
             (buffer (appkit-surface-buffer view)))
        (with-current-buffer buffer
          (qq-chat--install-scroll-observer view)
          (qq-completion-preload-members)
          (qq-chat-render)
          (when created-p (appkit-chatbuf-focus-input)))
        buffer))))

(defun qq-chat-open (session-key)
  "Open chat for SESSION-KEY, preserving an existing history position."
  (interactive)
  (let ((owner (or (qq-runtime-current-account-id)
                   (user-error "qq: Select a QQ account first"))))
    (qq-runtime-with-account owner
                             (let ((buffer (qq-chat--open-buffer session-key)))
                               (with-current-buffer buffer
                                 (unless (or (qq-chat--history-known-p)
                                             (qq-chat--loading))
                                   (qq-chat--load-initial-history buffer session-key)))
                               (pop-to-buffer buffer)
                               (with-current-buffer buffer
                                 (appkit-surface-refresh-responsive-geometry (appkit-current-surface)))))))

(defun qq-chat--composer-preview-media-key-p (media-key)
  "Return non-nil when MEDIA-KEY affects the active reply preview."
  (and (stringp media-key)
       (member media-key
               (qq-media-message-one-line-preview-keys
                (qq-chat--reply-message)))))

(defun qq-chat--rerender-open-chats (&optional media-key)
  "Refresh open chat rows and composer destinations affected by MEDIA-KEY."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when-let* ((surface (qq-chat--live-current-view)))
        (let* ((prompt-key
                (and qq-runtime--account-id
                     (qq-runtime-with-account qq-runtime--account-id
                                              (qq-chat--prompt-avatar-cache-key))))
               (resources (and media-key (list (list :media media-key))))
               (frame-p (or (equal media-key prompt-key)
                            (and media-key
                                 (qq-chat--composer-preview-media-key-p media-key)))))
          (if media-key
              (when (or frame-p
                        (and (appkit-chat-timeline-live-p)
                             (appkit-chat-timeline-dependent-keys resources)))
                (qq-runtime-invalidate surface :frame-p frame-p :resources resources))
            (when (or (appkit-chat-timeline-live-p)
                      (appkit-chatbuf-prompt-button-live-p))
              (qq-runtime-invalidate surface
                                     :frame-p (and prompt-key t)
                                     :keys (and (appkit-chat-timeline-live-p)
                                                (appkit-chat-timeline-keys))))))))))

(defun qq-chat--message-event-rekeys (event)
  "Return the row rekey described by EVENT, when it is an identity promotion.

When both the old and new keys already have timeline nodes, state has collapsed
two independently projected observations of the same native message.  That is
a deletion of the redundant old row, not a rekey: normal reconciliation must
remove it while retaining the already rendered canonical row."
  (let* ((message (plist-get event :message))
         (new-key (and (listp message) (qq-chat--message-anchor message)))
         (old-keys
          (delete-dups
           (delq nil
                 (list (plist-get event :previous-anchor)
                       (and (listp message) (alist-get 'local-id message))))))
         (old-key
          (seq-find (lambda (key)
                      (and (not (equal key new-key))
                           (appkit-chat-timeline-node key)))
                    old-keys)))
    (and old-key
         new-key
         (not (appkit-chat-timeline-node new-key))
         (list (cons old-key new-key)))))

(defun qq-chat--apply-message-state-change (event)
  "Synchronize one QQ message EVENT through the canonical projection."
  (let* ((message (plist-get event :message))
         (anchor (or (plist-get event :message-anchor)
                     (and (listp message) (qq-chat--message-anchor message))))
         (previous-anchor (plist-get event :previous-anchor))
         (resources
          (mapcar (lambda (key) (list :message key))
                  (delete-dups (delq nil (list anchor previous-anchor)))))
         (rekeys (qq-chat--message-event-rekeys event)))
    (unless (or anchor previous-anchor)
      (error "qq: message state event has no stable anchor: %S" event))
    (qq-chat--rekey-message-selection previous-anchor anchor)
    (qq-chat--header-line-update)
    (qq-chat--sync-timeline
     :changed-resources resources
     :rekeys rekeys)
    (when (or (qq-chat--message-affects-composer-context-p anchor)
              (qq-chat--message-affects-composer-context-p previous-anchor))
      (qq-chat--refresh-composer-context-for-message
       (or anchor previous-anchor)))))

(defun qq-chat--handle-state-change (event)
  "Send domain state EVENT to each matching live QQ chat Surface."
  (let ((event-session-key (plist-get event :session-key))
        (event-type (plist-get event :type))
        (event-owner (plist-get event :account-id)))
    (when (eq event-type 'reset)
      ;; The remembered destination belongs to the previous runtime/account,
      ;; not to any particular chat buffer.
      (setq qq-chat--last-forward-target-key nil
            qq-chat-forward-target-history nil))
    (when (memq event-type '(message history reset session action connection
                             sessions-refreshed friends-refreshed groups-refreshed
                             group-members-refreshed))
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (and (derived-mode-p 'qq-chat-mode)
                     (equal event-owner qq-runtime--account-id))
            ;; Reset is a hard ownership boundary even after runtime shutdown
            ;; has made the AppKit view non-live.  Rotate/clear opaque owners
            ;; before any best-effort cancellation can run callbacks.
            (when (eq event-type 'reset)
              (setq qq-chat--forward-plan-owner (list 'forward-plan-owner)
                    qq-chat--message-selection nil)
              (qq-chat--clear-forward-presentation))
            ;; Only UI projection requires a live view.  Ownership cleanup
            ;; above deliberately does not.
            (when (or (memq event-type
                            '(reset connection sessions-refreshed
                              friends-refreshed groups-refreshed))
                      (equal event-session-key qq-chat--session-key))
              (when-let* ((view (qq-chat--live-current-view)))
                (appkit-surface-send view (list 'qq-state-event event))))))))))

(add-hook 'qq-media-cache-update-hook #'qq-chat--rerender-open-chats)
(add-hook 'qq-state-change-hook #'qq-chat--handle-state-change)

(provide 'qq-chat)

;;; qq-chat.el ends here
