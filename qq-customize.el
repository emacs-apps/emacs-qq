;;; qq-customize.el --- Customization for emacs-qq -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: comm

;;; Commentary:

;; User customization for emacs-qq.

;;; Code:

(require 'subr-x)

(defgroup qq nil
  "QQ client for Emacs."
  :group 'comm)

(defcustom qq-server-websocket-url "ws://127.0.0.1:3002/"
  "QQ service WebSocket endpoint used by emacs-qq.

The service is a long-lived process which owns zero or more QQ account
runtimes.  Closing this WebSocket only disconnects this Emacs client; it does
not stop or log out any managed account."
  :type 'string
  :group 'qq)

(defcustom qq-server-auth-token-file
  (locate-user-emacs-file "qq/service-token")
  "File containing the native service authentication token.

The token is read only while opening a connection and is sent in the native
protocol handshake.  It must be a single non-whitespace value of at least 32
bytes.  On Unix the file must not be accessible by group or other users."
  :type 'file
  :group 'qq)

(defcustom qq-server-client-name "emacs-qq"
  "Client name sent in the native protocol handshake."
  :type 'string
  :group 'qq)

(defcustom qq-server-request-timeout 30
  "Seconds before an unanswered native request fails locally.

Nil disables request timeouts."
  :type '(choice (const :tag "Disabled" nil)
                 (number :tag "Seconds"))
  :group 'qq)

(defcustom qq-server-ready-timeout 10
  "Seconds to wait for the ready snapshot after a successful handshake.

The service sends this authoritative snapshot immediately after the handshake
response.  Nil disables this additional protocol timeout."
  :type '(choice (const :tag "Disabled" nil)
                 (number :tag "Seconds"))
  :group 'qq)

(defcustom qq-server-reconnect-delay 3
  "Seconds before reconnecting the native service WebSocket."
  :type 'number
  :group 'qq)

(defcustom qq-server-reconnect-max-attempts nil
  "Maximum native service reconnect attempts before stopping.

Set to nil to retry indefinitely.  The counter is reset only after an
authenticated ready snapshot, not merely when the socket opens."
  :type '(choice (const :tag "Unlimited" nil) integer)
  :group 'qq)

(defcustom qq-login-captcha-timeout 300
  "Seconds allowed for completing one interactive QQ captcha.

The isolated browser is closed when this timeout expires."
  :type '(restricted-sexp
          :match-alternatives
          ((lambda (value)
             (and (integerp value) (> value 0)))))
  :group 'qq)

(defcustom qq-login-qrencode-program "qrencode"
  "Program used to render QQ login QR codes.

The program must support the standard qrencode command-line interface.  A PNG
is used in graphical Emacs; terminal frames use its UTF-8 rendering."
  :type 'string
  :group 'qq)

(defcustom qq-login-qr-image-size 220
  "Pixel width and height of a graphical QQ login QR code."
  :type 'integer
  :group 'qq)

(defcustom qq-recent-contact-count 50
  "Default amount of recent sessions requested during refresh."
  :type 'integer
  :group 'qq)

(defcustom qq-history-fetch-count 20
  "Default amount of messages fetched per history request."
  :type 'integer
  :group 'qq)

(defcustom qq-chat-history-auto-load-threshold 2000
  "Character distance from a timeline edge that triggers history paging.

Near the top, load older messages.  Near the bottom of a partial around
window, load newer messages while the composer is idle.  Set to nil to disable
telega-style automatic edge paging."
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Characters"))
  :group 'qq)

(defcustom qq-chat-history-tail-poll-interval 5
  "Minimum seconds between group-history polls at the live bottom edge.

Live pushes normally advance an attached chat immediately.  This lightweight
poll closes gaps after reconnects or missed pushes while point remains near
the footer.  Set to nil to disable live-edge polling without disabling normal
partial-window paging."
  :type '(choice (const :tag "Disabled" nil)
                 (number :tag "Seconds"))
  :group 'qq)

(defcustom qq-chat-use-date-breaks t
  "Non-nil means render a date break at local calendar-day boundaries.

Date breaks are derived presentation between adjacent canonical messages;
they are not timeline rows or messages."
  :type 'boolean
  :group 'qq)

(defcustom qq-chat-date-break-format "%d %B %Y %a"
  "Format used for chat date-break labels.

The value is accepted by `format-time-string'.  The default matches telega's
`date-break-bar' format."
  :type 'string
  :group 'qq)

(defcustom qq-chat-buffer-name-prefix "QQ"
  "Application prefix used for human-readable chat buffer names.

The conversation kind is expressed by brackets around its title.  Account and
protocol identities are appended only when another live buffer has the same
human name."
  :type 'string
  :group 'qq)

(defcustom qq-chat-show-prompt-avatar t
  "Non-nil means show the destination avatar in the chat composer prompt.

Private chats show the peer avatar and group chats show the group avatar.
Non-avatar session kinds retain a compact textual destination icon."
  :type 'boolean
  :group 'qq)

(defcustom qq-chat-messages-pop-ring-size 50
  "Size of the chatbuf messages pop ring.

When jumping to a reply target (or any message), the previous message at
point is pushed so `qq-chat-goto-pop-message' (`x') can jump back.  This
corresponds to telega's `telega-chat-messages-pop-ring-size'."
  :type 'integer
  :group 'qq)

(defcustom qq-chat-jump-history-count nil
  "History page size when seeking a jump target (reply goto).

Nil means use `qq-history-fetch-count'.  The Gateway requests one bounded
history page around the target snowflake instead of iterating from the
buffer's oldest message."
  :type '(choice (const :tag "Same as qq-history-fetch-count" nil)
                 (integer :tag "Count"))
  :group 'qq)

(defcustom qq-chat-attach-commands
  '(("image" qq-chat-attach-image)
    ("video" qq-chat-attach-video)
    ("file" qq-chat-attach-document)
    ("media" qq-chat-attach-file)
    ("clipboard" qq-chat-attach-clipboard)
    ("qq-face" qq-chat-attach-face)
    ("favorite-face" qq-chat-attach-custom-face))
  "Attachment commands offered by `qq-chat-attach'.

Each entry has the form (NAME COMMAND).  NAME is the stable completion
candidate shown after `C-c C-a'; COMMAND must be an interactive function.
As in telega's `telega-chat-attach-commands', users and extensions may append
their own attachment kinds without replacing the dispatcher."
  :type '(alist :key-type (string :tag "Attachment name")
                :value-type (list function))
  :group 'qq)

(defcustom qq-auto-mark-read t
  "When non-nil, advance a chat's read position with its cursor.

The selected chat follows point.  Point in the composer means the newest
loaded message; point on the timeline means that exact message.  Merely
displaying a chat buffer does not mark the whole session as read."
  :type 'boolean
  :group 'qq)

(defcustom qq-input-status-ttl 6
  "Seconds to keep a friend's chat-action (typing) after the last push.

Modeled after telega's ephemeral chat actions.  The Gateway may omit an
explicit stop event, so the client auto-clears unless a newer push refreshes
the action."
  :type 'integer
  :group 'qq)

(defcustom qq-chat-show-peer-actions t
  "When non-nil, show peer chat-actions (e.g. 正在输入) in chat footer and root.

Telega equivalent: footer `telega-chatbuf-footer-ins-prompt-delim' with
actions + root `telega-ins--chat-status' preferring actions."
  :type 'boolean
  :group 'qq)

(defcustom qq-chat-send-typing t
  "When non-nil, emit `set_input_status' while composing private-chat input.

Telega equivalent: `telega-chatbuf--set-action' \"Typing\" / \"Cancel\" driven
by input non-emptiness after commands."
  :type 'boolean
  :group 'qq)

(defcustom qq-chat-action-prefix ".. "
  "Prefix shown before peer action text in footer/root (telega typing symbol)."
  :type 'string
  :group 'qq)

(defcustom qq-chat-show-recalled-messages nil
  "When non-nil, show recalled messages as stubs in the chat timeline.

Default nil matches telega (`telega-chat-show-deleted-messages-for' defaults
to nil): recalled rows stay in `qq-state' but are omitted from the EWOC.
When non-nil, render a \"[message recalled]\" placeholder (no action buttons).

Requires the Gateway to project authoritative recall state."
  :type 'boolean
  :group 'qq)

(defcustom qq-chat-show-unread-divider t
  "When non-nil, render an unread divider before the first unread message.

The divider requires an exact first-unread NT snowflake from the Gateway's
authoritative unread materialization.  It is omitted while only read/latest
cursors are known."
  :type 'boolean
  :group 'qq)

(defcustom qq-chat-auto-fill-margin-columns 1
  "Additional right margin columns used for chat timeline alignment.

This mirrors `telega-chat-auto-fill-margin-columns' and keeps right-aligned
timestamps clear of the window edge while zooming or resizing."
  :type '(choice (const :tag "No additional margin" nil)
                 (integer :tag "Additional margin columns"))
  :group 'qq)

(defcustom qq-root-auto-fill-margin-columns 1
  "Additional right margin columns used for root session rows.

This mirrors `telega-root-auto-fill-margin-columns'."
  :type '(choice (const :tag "No additional margin" nil)
                 (integer :tag "Additional margin columns"))
  :group 'qq)

(defgroup qq-notifications nil
  "Desktop notifications for emacs-qq."
  :group 'qq)

(defgroup qq-modes nil
  "Global presentation modes for emacs-qq."
  :group 'qq)

(defcustom qq-mode-line-string-format
  '("  " (:eval (qq-mode-line-icon))
    (:eval (qq-mode-line-unread-unmuted))
    (:eval (qq-mode-line-mentions)))
  "Format used by `qq-mode-line-mode' in `mode-line-misc-info'."
  :type 'sexp
  :group 'qq-modes)

(defface qq-mode-line-unread
  '((t :inherit warning :weight bold))
  "Face for the unmuted unread message count in the mode line."
  :group 'qq-modes)

(defface qq-mode-line-mention
  '((t :inherit error :weight bold))
  "Face for the unread mention count in the mode line."
  :group 'qq-modes)

(defface qq-root-unmuted-count
  '((((class color) (background dark))
     :foreground "DeepSkyBlue")
    (((class color) (background light))
     :foreground "blue"))
  "Face for unread counts from unmuted root sessions."
  :group 'qq)

(defface qq-root-muted-count
  '((t :inherit shadow))
  "Face for unread counts from muted root sessions."
  :group 'qq)

(defface qq-root-mention-count
  '((t :inherit qq-root-unmuted-count :weight bold))
  "Face for unread mention markers in root sessions."
  :group 'qq)

(defcustom qq-notifications-delay 0.5
  "Seconds to delay a message notification before rechecking visibility."
  :type 'number
  :group 'qq-notifications)

(defcustom qq-notifications-timeout 4.0
  "Seconds before emacs-qq closes its current desktop notification.

Nil leaves notification lifetime to the desktop notification server."
  :type '(choice (const :tag "Desktop default" nil) number)
  :group 'qq-notifications)

(defcustom qq-notifications-max-message-age 60
  "Maximum incoming message age in seconds eligible for notification."
  :type 'integer
  :group 'qq-notifications)

(defcustom qq-notifications-body-limit 120
  "Maximum notification body width in characters."
  :type 'integer
  :group 'qq-notifications)

(defcustom qq-notifications-show-preview t
  "When non-nil, include a compact message preview in notifications."
  :type 'boolean
  :group 'qq-notifications)

(defcustom qq-notifications-at-all-breaks-mute t
  "When non-nil, unread @全体成员 messages can notify through group mute.

Direct @self always breaks mute, matching QQ's native unread-at distinction."
  :type 'boolean
  :group 'qq-notifications)

(defcustom qq-notifications-history-ring-size 30
  "Number of recent desktop notifications retained by emacs-qq."
  :type 'integer
  :group 'qq-notifications)

(defcustom qq-notifications-extra-args nil
  "Additional keyword arguments appended to `notifications-notify'."
  :type '(repeat sexp)
  :group 'qq-notifications)

;; Faces mirror telega-msg-* defaults (not a visual clone of every palette
;; trick).  Keep them ordinary so theming stays familiar.

(defface qq-msg-heading
  '((((class color) (background light))
     :background "gray90" :extend t)
    (((class color) (background dark))
     :background "gray20" :extend t)
    (t :inherit widget-single-line-field :extend t))
  "Face for message heading rows (avatar + sender + time).

Same defaults as `telega-msg-heading'."
  :group 'qq)

(defface qq-msg-selected-marker
  '((t :inherit warning :weight bold))
  "Face for the vertical mark on selected chat messages."
  :group 'qq)

(defface qq-msg-self-title
  '((t :bold t))
  "Face for the current account's sender title.

Same defaults as `telega-msg-self-title'."
  :group 'qq)

(defface qq-msg-user-title
  '((t nil))
  "Base face for other users' sender titles.

Message headings combine this face with Appkit's identity-keyed name color.
The default otherwise matches `telega-msg-user-title'."
  :group 'qq)

(defface qq-msg-inline-reply
  '((t :inherit (qq-msg-heading shadow)))
  "Face for inline reply preview rows.

Same idea as `telega-msg-inline-reply'."
  :group 'qq)

(defface qq-msg-poke
  '((t :inherit shadow))
  "Face for QQ gray-tip poke decorations.

Pokes use telega-style centered special-message chrome instead of the ordinary
message heading/body layout."
  :group 'qq)

(defface qq-msg-deleted
  '((t :inherit custom-invalid :extend t))
  "Face used for recalled message stubs.

Same defaults as `telega-msg-deleted'."
  :group 'qq)

(defface qq-msg-date-separator
  '((t :inherit (shadow bold)))
  "Face for telega-style day separator rows in the chat timeline."
  :group 'qq)

(defface qq-msg-unread-divider
  '((t :inherit shadow))
  "Face for the unread messages bar (telega unread bar)."
  :group 'qq)

(defface qq-msg-status
  '((t :inherit shadow))
  "Face for pending/failed/recalled status suffixes and timestamps."
  :group 'qq)

(defface qq-msg-mention
  '((t :inherit font-lock-variable-name-face))
  "Face for ordinary QQ @mentions, matching telega's mention entity role."
  :group 'qq)

(defface qq-msg-mention-self
  '((t :inherit (warning bold)))
  "Face for @self and @全体成员 mentions requiring special attention."
  :group 'qq)

(defface qq-msg-reaction
  '((t :inherit button))
  "Face for a message reaction not selected by the current account."
  :group 'qq)

(defface qq-msg-reaction-chosen
  '((t :inherit (highlight bold)))
  "Face for a message reaction selected by the current account."
  :group 'qq)

(defcustom qq-media-avatar-image-height 20
  "Pixel height used when decoding avatar image resources.

Chat timelines rescale these resources to the shared two-text-line geometry."
  :type 'integer
  :group 'qq)

(defcustom qq-media-face-image-height 18
  "Pixel height used for inline QQ face images in chat buffers."
  :type 'integer
  :group 'qq)

(defcustom qq-media-animated-face-image-height 64
  "Pixel height used for animated QQ base faces in chat buffers."
  :type 'integer
  :group 'qq)

(defcustom qq-media-poke-image-height 28
  "Maximum pixel height used for decorative poke action images."
  :type 'integer
  :group 'qq)

(defcustom qq-media-default-emoji-directory
  "/opt/QQ/resources/app/resource/default-emojis"
  "Directory of LinuxQQ built-in base face PNGs (`<id>.png').

Used as the primary source for inline QQ faces so chat rendering does not
depend on a backend request.  When a file is missing, chat rendering keeps
the textual face description."
  :type 'directory
  :group 'qq)

(defcustom qq-media-system-emoji-cache-directories nil
  "Additional LinuxQQ dynamic system-emoji cache roots.

Each root contains `<id>/png', `<id>/apng', and `<id>/lottie'.  When nil,
emacs-qq discovers account caches below `~/.config/QQ/nt_qq_*'."
  :type '(repeat directory)
  :group 'qq)

(defcustom qq-media-lottie-renderer-command
  (executable-find "tgs2png")
  "Renderer used for system-face Lottie previews and playback.

The command must implement the `tgs2png' interface used by disco.el.  Nil
keeps APNG/static rendering and the textual fallback."
  :type '(choice (const :tag "Unavailable" nil) file)
  :group 'qq)

(defcustom qq-media-face-names-file
  (let* ((lib (or (locate-library "qq-customize.el")
                  (locate-library "qq-media.el")
                  load-file-name
                  buffer-file-name))
         ;; straight build often symlinks *.el into build/; resolve to the
         ;; real package dir so non-el data files (json) are found.
         (here (and lib (file-name-directory (file-truename lib)))))
    (expand-file-name "qq-face-names.json" (or here default-directory)))
  "JSON map of QQ face id → display name (e.g. \"178\" → \"/斜眼笑\").

Bundled with emacs-qq from Linux QQ's face catalog.  Used for plain-text
fallbacks and previews when the face image is not yet available."
  :type 'file
  :group 'qq)

(defcustom qq-media-preview-image-height 160
  "Maximum pixel height used for inline media previews in chat buffers."
  :type 'integer
  :group 'qq)

(defcustom qq-media-preview-image-max-width 320
  "Maximum pixel width used for inline media previews in chat buffers."
  :type 'integer
  :group 'qq)

(defcustom qq-media-one-line-preview-columns 2
  "Columns reserved for compact message media previews."
  :type 'integer
  :group 'qq)

(defcustom qq-media-download-directory
  (locate-user-emacs-file "qq-downloads/")
  "Directory used for QQ media downloads materialized by the Gateway."
  :type 'directory
  :group 'qq)

(defcustom qq-media-cache-directory
  (locate-user-emacs-file "qq-media-cache/")
  "Directory used for cached remote QQ media copies needed for inline rendering."
  :type 'directory
  :group 'qq)


(defcustom qq-self-message-dedupe-window 10
  "Seconds used to weakly dedupe self-message event echoes."
  :type 'integer
  :group 'qq)

(provide 'qq-customize)

;;; qq-customize.el ends here
