;;; qq-chat.el --- Chat buffers for emacs-qq -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; Chat buffer built on appkit's shared timeline and presentation primitives,
;; while borrowing
;; telega.el-style naming and the most familiar chat input bindings.

;;; Code:

(require 'cl-lib)
(require 'ring)
(require 'button)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-invalidation)
(require 'appkit-chat-avatar)
(require 'appkit-chatbuf)
(require 'appkit-chat-history)
(require 'appkit-chat-completion)
(require 'appkit-chat-timeline)
(require 'appkit-scroll)
(require 'appkit-chat-ins)
(require 'appkit-name-color)
(require 'appkit-media)
(require 'appkit-ui)
(require 'appkit-presentation)
(require 'qq-core)
(require 'qq-completion)
(require 'qq-customize)
(require 'qq-media)
(require 'qq-protocol)
(require 'qq-runtime)
(require 'qq-state)
(require 'qq-request)

;; `qq-forward' requires this module to reuse the message-body renderer, so
;; keep the reverse dependency lazy and avoid a load cycle.
(autoload 'qq-forward-segment-p "qq-forward")
(autoload 'qq-forward-insert-segment "qq-forward")
(autoload 'qq-forward-event-segment-to-internal "qq-forward")
(autoload 'qq-user-open "qq-user" nil t)
(autoload 'qq-group-open "qq-group" nil t)
(autoload 'qq-chat-forward-transient "qq-transient" nil t)
(autoload 'qq-chat-message-todo-transient "qq-transient" nil t)

(declare-function qq-forward-segment-p "qq-forward" (segment))
(declare-function qq-forward-insert-segment
                  "qq-forward" (segment prefix-state properties))
(declare-function qq-forward-event-segment-to-internal
                  "qq-forward" (segment session-key))
(declare-function qq-user-open "qq-user" (user-id))
(declare-function qq-group-open "qq-group" (group-id))
(declare-function qq-chat-message-transient "qq-transient" (&rest args))
(declare-function qq-chat-transient "qq-transient" (&rest args))
(declare-function qq-chat-forward-transient "qq-transient" (&rest args))
(declare-function qq-chat-message-todo-transient "qq-transient" (&rest args))
(declare-function qq-message-send-merged-forward
                  "qq-message"
                  (source-session-key target-session-key message-ids
                                      &optional callback errback))
(declare-function qq-core-send-poke
                  "qq-core" (session-key target-id &optional callback errback))
(declare-function qq-core-recall-poke
                  "qq-core" (message &optional callback errback))
(declare-function qq-message-poke-recall-capable-p
                  "qq-message" (message))

(defun qq-chat--default-error (_response reason)
  "Display a chat operation failure REASON."
  (message "qq: %s" (or reason "chat operation failed")))

(defvar-local qq-chat--session-key nil
  "Session key associated with the current chat buffer.")

(defvar-local qq-chat--my-action nil
  "Local outgoing chat-action for this buffer.

Non-nil means we currently advertise typing via `set_input_status'.  This
mirrors telega's `telega-chatbuf--my-action'.")

(defvar-local qq-chat--callback-sync-request nil
  "Accepted asynchronous state awaiting presentation by its captured view.

The value is a plist containing `:view' and an ordered `:actions' list.
Actions run only after the accepted state has been projected by that exact
live Appkit view.")

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

(defvar-local qq-chat--remote-latest-id nil
  "Newest exact server message id observed for this QQ session.

This loaded-history frontier remains client-owned.  Protocol-independent
window edges, loading ownership, exhaustion, and stalls live in AppKit's
buffer-local continuous history controller.")

(defvar-local qq-chat--guild-history-start-sequence nil
  "Lowest native Guild sequence covered by the current history window.")

(defvar-local qq-chat--guild-history-end-sequence nil
  "Highest native Guild sequence covered by the current history window.")

(defvar-local qq-chat--gateway-history-older-cursor nil
  "Opaque unified-history cursor for the next older Gateway page.")

(defvar-local qq-chat--gateway-history-newer-cursor nil
  "Opaque unified-history cursor for the next newer Gateway page or tail poll.")

(defvar-local qq-chat--last-tail-poll-at nil
  "Time of the latest automatic unified-history poll at the live edge.")
(defvar-local qq-chat--scroll-observer nil)

(defvar-local qq-chat--guild-forum-next-cursor nil
  "Opaque native cursor for the next older QQ Guild forum page.")

(defvar-local qq-chat--initial-history-owner nil
  "Appkit operation owning the active initial-history request chain.")

(defun qq-chat--reset-history-state ()
  "Reset current buffer history paging state."
  (appkit-chat-history-reset-state)
  (setq qq-chat--remote-latest-id nil
        qq-chat--guild-history-start-sequence nil
        qq-chat--guild-history-end-sequence nil
        qq-chat--gateway-history-older-cursor nil
        qq-chat--gateway-history-newer-cursor nil
        qq-chat--last-tail-poll-at nil
        qq-chat--guild-forum-next-cursor nil))


(defun qq-chat--set-history-window (first-message-id last-message-id)
  "Project one contiguous history window from FIRST-MESSAGE-ID through LAST.

Both identifiers remain opaque strings.  A nil FIRST-MESSAGE-ID leaves the
older edge unbounded.  A nil LAST-MESSAGE-ID attaches the window to the live
latest edge; otherwise it is the exact cursor used to fetch newer history."
  (appkit-chat-history-window-set first-message-id last-message-id))

(defun qq-chat--set-empty-history-window ()
  "Establish an authoritative empty window attached to live history."
  (appkit-chat-history-window-establish-empty))

(defun qq-chat--history-window-partial-p ()
  "Return non-nil when newer messages exist outside the projected window."
  (appkit-chat-history-window-partial-p))

(defun qq-chat--history-window-known-p ()
  "Return non-nil after this buffer established an exact history window."
  (appkit-chat-history-window-known-p))

(defun qq-chat--record-gateway-history-range (meta &optional direction)
  "Record unified history cursors from META for DIRECTION.

DIRECTION defaults to `replace'.  Directional pages update only the edge they
extended, preserving the opposite opaque cursor without understanding whether
it represents a sequence, roaming tuple, or DataLine Message ID."
  (unless (and (eql (plist-get meta :history-port-version)
                    qq-core-history-port-version)
               (equal (plist-get meta :history-account-id)
                      (qq-runtime-current-account-id))
               (equal (plist-get meta :history-session-key)
                      qq-chat--session-key))
    (error "qq: Gateway history metadata does not belong to this chat"))
  (pcase (or direction 'replace)
    ('replace
     (setq qq-chat--gateway-history-older-cursor
           (copy-tree (plist-get meta :history-older-cursor))
           qq-chat--gateway-history-newer-cursor
           (copy-tree (plist-get meta :history-newer-cursor))))
    ('older
     (setq qq-chat--gateway-history-older-cursor
           (copy-tree (plist-get meta :history-older-cursor))))
    ('newer
     (setq qq-chat--gateway-history-newer-cursor
           (copy-tree (plist-get meta :history-newer-cursor))))
    (value
     (error "qq: Unknown unified history direction %S" value)))
  meta)

(defun qq-chat--adopt-gateway-message-frontier ()
  "Remember the newest canonical message currently projected for this chat."
  (when-let* ((message (qq-chat--latest-server-message))
              (message-id (alist-get 'server-id message)))
    (setq qq-chat--remote-latest-id message-id)))

(defun qq-chat--prepare-around-history-window (&optional remote-latest-id)
  "Prepare an around-message window with REMOTE-LATEST-ID frontier."
  (setq qq-chat--remote-latest-id
        (or remote-latest-id qq-chat--remote-latest-id))
  (appkit-chat-history-older-loaded-set nil)
  (appkit-chat-history-newer-stalled-clear))

(defvar-local qq-chat--pending-jump-id nil
  "Server message id to jump to after history loads, or nil.

Mirrors telega's async `telega-chatbuf--goto-msg' when the target is not yet
loaded in the chatbuf.")

(defvar-local qq-chat--messages-pop-ring nil
  "Ring of message anchors for jump-back.

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

(defvar-local qq-chat--forward-request nil
  "Cancelable transport token for the active forward request.")

(defvar-local qq-chat--forward-request-owner nil
  "Opaque owner of the active forward request from this chat buffer.")

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
                           plan-owner)))
  "Immutable source snapshot passed through the forwarding transient."
  buffer
  session-key
  anchors
  messages
  memberships
  plan-owner)


(defvar-local qq-chat--last-read-target-row-key nil
  "Newest canonical row submitted from this buffer's cursor.")


(defvar qq-chat-timeline-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    ;; Message actions at point (telega-style single keys; never steal input).
    (define-key map (kbd "r") #'qq-chat-reply-to-message)
    (define-key map (kbd "n") #'qq-chat-next-message)
    (define-key map (kbd "p") #'qq-chat-previous-message)
    (define-key map (kbd "d") #'qq-chat-delete-transient)
    (define-key map (kbd "f") #'qq-chat-forward-transient)
    (define-key map (kbd "m") #'qq-chat-toggle-message-selection)
    (define-key map (kbd "U") #'qq-chat-clear-message-selection)
    (define-key map (kbd "o") #'qq-chat-open-resource-at-point)
    (define-key map (kbd "a") #'qq-chat-open-avatar-at-point)
    (define-key map (kbd "i") #'qq-chat-open-user-at-point)
    (define-key map (kbd "h") #'qq-chat-open-peer-info)
    (define-key map (kbd "g") #'qq-chat-goto-reply)
    (define-key map (kbd "x") #'qq-chat-goto-pop-message)
    (define-key map (kbd "P") #'qq-chat-poke-sender)
    (define-key map (kbd "!") #'qq-chat-react-to-message)
    (define-key map (kbd "?") #'qq-chat-transient)
    map)
  "Timeline-only keymap active when point is outside the draft region.

Single-key message actions (`r' reply, `n'/`p' message navigation, `d'
delete menu, `f' forward, `!' react, `P' poke sender, `m' select/unselect,
`U' clear selection, `o' open media, `a' avatar, `i' user, `g' goto
replied-to, `x' pop jump) and the `?' menu apply on the timeline.  They are
inactive in the composer so typing is never stolen.")

(define-minor-mode qq-chat-timeline-mode
  "Buffer-local navigation bindings active outside the draft region."
  :init-value nil
  :lighter nil
  :keymap qq-chat-timeline-mode-map)

(defun qq-chat--session ()
  "Return current chat session object."
  (and qq-chat--session-key
       (qq-state-session qq-chat--session-key)))


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
    (let ((name (qq-chat--buffer-name qq-chat--session-key)))
      (unless (equal (buffer-name) name)
        (rename-buffer name)))))


(defun qq-chat--header-line ()
  "Return the formatted header line for the active chat buffer."
  (let* ((session (qq-chat--session))
         (title (or (alist-get 'title session) qq-chat--session-key))
         (status (qq-state-connection-status))
         (unread (or (alist-get 'unread-badge-count session) 0))
         (status-part
          (if (memq status '(connected ready))
              ""
            (format "  [%s]" status)))
         (unread-part (if (> unread 0) (format "  · %d unread" unread) ""))
         (selected-count (length qq-chat--message-selection))
         (selected-part
          (if (> selected-count 0)
              (format "  · %d selected" selected-count)
            ""))
         (forward-part
          (if qq-chat--forward-request-owner "  · forwarding…" "")))
    (format " %s%s%s%s%s"
            title status-part unread-part selected-part forward-part)))


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
  (or (appkit-view-responsive-width qq-chat-auto-fill-margin-columns)
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
                  (qq-chat--media-segment-p segment))
          (setq found t))
        (setq segments (cdr segments))))
    found))

(defun qq-chat--messages-compact-group-p (previous current)
  "Return non-nil when CURRENT should be compact-grouped under PREVIOUS."
  (and qq-chat-group-messages
       (listp previous)
       (listp current)
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

(defun qq-chat--authoritative-latest-message-id ()
  "Return the best exact latest-message id known for the current session."
  (let ((session (qq-chat--session)))
    (or (alist-get 'read-latest-message-id session)
        (alist-get 'last-message-id session))))

(defun qq-chat--history-batch-bounds (meta)
  "Return (OLDEST . NEWEST) timeline anchors for history batch META.

Derive ordering from the already normalized session timeline instead of the
wire array: native batch direction is not part of the client identity
contract.  An empty batch, or one not present in canonical state, has no
bounds."
  (let ((ids (plist-get meta :batch-message-ids))
        oldest
        newest)
    (when ids
      (let ((members (make-hash-table :test #'equal)))
        (dolist (id ids)
          (when id
            (puthash (format "%s" id) t members)))
        (dolist (message (qq-state-session-messages qq-chat--session-key))
          (when-let* ((id (qq-state-message-anchor message))
                      ((gethash id members)))
            (unless oldest
              (setq oldest id))
            (setq newest id)))))
    (cons oldest newest)))

(defun qq-chat--message-reply-data (message)
  "Return normalized reply segment data from MESSAGE, or nil."
  (when-let* ((segment
               (seq-find
                (lambda (candidate)
                  (equal (alist-get 'type candidate) "reply"))
                (alist-get 'segments message))))
    (alist-get 'data segment)))

(defun qq-chat--message-by-sequence (sequence)
  "Return the current conversation message carrying exact SEQUENCE.

This is the canonical Message Sequence used by native history.  It is not a
message identity and must never be compared with a snowflake `server-id'."
  (when (and sequence qq-chat--session-key)
    (seq-find
     (lambda (message)
       (equal (alist-get 'message-seq message) sequence))
     (qq-state-session-messages qq-chat--session-key))))

(defun qq-chat--message-by-reply-sequence (sequence)
  "Resolve SourceMsg OrigSeq SEQUENCE in the current conversation.

Group OrigSeq is the canonical Message Sequence.  Private OrigSeq is the
target's Client Message Sequence and must never be used as an SsoGetC2cMsg
range cursor."
  (when (and sequence qq-chat--session-key)
    (let ((field (if (eq (qq-state-session-key-type qq-chat--session-key)
                         'private)
                     'native-client-sequence
                   'message-seq)))
      (seq-find
       (lambda (message)
         (equal (alist-get field message) sequence))
       (qq-state-session-messages qq-chat--session-key)))))

(defun qq-chat--message-reply-id (message)
  "Return MESSAGE's resolved reply target snowflake, or nil.

SourceMsg OrigSeq is group Message Sequence or private Client Message
Sequence.  Historical segment shapes without a sequence may still carry an
exact `id'; a numeric-looking SourceMsg reserve value accompanying a sequence
is deliberately not treated as a snowflake."
  (when-let* ((data (qq-chat--message-reply-data message)))
    (let ((sequence (or (alist-get 'message_seq data)
                        (alist-get 'sequence data))))
      (if sequence
          (alist-get 'server-id
                     (qq-chat--message-by-reply-sequence sequence))
        (when-let* ((reply-id (or (alist-get 'id data)
                                  (alist-get 'message_id data))))
          (format "%s" reply-id))))))

(defun qq-chat--message-reply-sequence (message)
  "Return MESSAGE's canonical history seek sequence, or nil.

Never return a private SourceMsg OrigSeq directly: that value is a Client
Message Sequence, not an SsoGetC2cMsg cursor."
  (when-let* ((data (qq-chat--message-reply-data message))
              (reply-sequence (or (alist-get 'message_seq data)
                                  (alist-get 'sequence data))))
    (let ((source (qq-chat--message-by-reply-sequence
                   (format "%s" reply-sequence))))
      (or (alist-get 'message-seq source)
          (and (not (eq (qq-state-session-key-type qq-chat--session-key)
                        'private))
               (format "%s" reply-sequence))))))

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

(defun qq-chat--latest-server-message ()
  "Return the newest loaded message carrying a canonical server id."
  (seq-find
   (lambda (message)
     (qq-protocol-message-id-p (alist-get 'server-id message)))
   (reverse (qq-state-session-messages qq-chat--session-key))))

(defun qq-chat--latest-visible-read-target ()
  "Return the newest read-capable row in this buffer's contiguous window."
  (seq-find #'qq-core-message-read-capable-p
            (reverse (qq-chat--timeline-messages))))

(defun qq-chat--message-index (message-id messages)
  "Return MESSAGE-ID's zero-based position in ordered MESSAGES, or nil."
  (seq-position
   messages message-id
   (lambda (message candidate-id)
     (equal (alist-get 'server-id message) candidate-id))))

(defun qq-chat--message-id-after-p (candidate-id reference-id messages)
  "Return non-nil when CANDIDATE-ID follows REFERENCE-ID in MESSAGES.

Return nil when either id is absent, because an unknown timeline relation
must not be guessed."
  (let ((candidate-index (qq-chat--message-index candidate-id messages))
        (reference-index (qq-chat--message-index reference-id messages)))
    (and candidate-index reference-index
         (> candidate-index reference-index))))

(defun qq-chat--canonical-row-index (row-key messages)
  "Return ROW-KEY's zero-based position in ordered MESSAGES, or nil."
  (seq-position
   messages row-key
   (lambda (message candidate-row)
     (equal (alist-get 'canonical-row-key message) candidate-row))))

(defun qq-chat--history-frontier-behind-batch-p
    (frontier-id newest-id batch-ids)
  "Return non-nil when canonical order proves FRONTIER-ID predates a batch."
  (and frontier-id newest-id
       (not (member frontier-id batch-ids))
       (qq-chat--message-id-after-p
        newest-id frontier-id
        (qq-state-session-messages qq-chat--session-key))))

(defun qq-chat--read-target-needed-p (message)
  "Return non-nil when canonical MESSAGE can advance the known read position.

First-unread is compared by native sequence because the message owning that
position may have been locally deleted and is therefore absent from the local
timeline; a server-id lookup would then fail closed forever."
  (let* ((messages (qq-state-session-messages qq-chat--session-key))
         (row-key (alist-get 'canonical-row-key message))
         (target-index (qq-chat--canonical-row-index row-key messages))
         (last-index
          (and qq-chat--last-read-target-row-key
               (qq-chat--canonical-row-index
                qq-chat--last-read-target-row-key messages)))
         (session (qq-state-session qq-chat--session-key))
         (unread-count (alist-get 'unread-message-count session))
         (first-unread (alist-get 'first-unread-message-id session))
         (read-latest (alist-get 'read-latest-message-id session)))
    (and target-index
         (or (null qq-chat--last-read-target-row-key)
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

(defun qq-chat--mark-message-viewed (message &optional force)
  "Advance native read position through MESSAGE.

With FORCE, submit even when this buffer already requested the same target."
  (when message
    (let ((row-key (alist-get 'canonical-row-key message)))
      (if (not (qq-core-message-read-capable-p message))
          (when force
            (user-error
             "qq: this message lacks a current canonical timeline row"))
        (when (or force (qq-chat--read-target-needed-p message))
          (let ((buffer (current-buffer))
                (session-key qq-chat--session-key))
            (qq-core-mark-message-read
             message
             (lambda (_receipt)
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (when (equal qq-chat--session-key session-key)
                     (setq qq-chat--last-read-target-row-key row-key))))))))))))

(defun qq-chat--manage-read-position (&optional position)
  "Advance read state to the message represented by POSITION.

Point in the composer represents the newest loaded message, matching telega's
prompt behavior.  Point on the timeline represents that exact message."
  (when (and qq-auto-mark-read qq-chat--session-key)
    (let ((position (or position (point))))
      (qq-chat--mark-message-viewed
       (if (appkit-chatbuf-point-in-input-p position)
           (qq-chat--latest-visible-read-target)
         (qq-chat--message-at-point position))))))


(defun qq-chat--message-forwardable-p (message)
  "Return non-nil when MESSAGE can be forwarded by its server ID."
  (and (listp message)
       (qq-protocol-message-id-p (alist-get 'server-id message))
       (not (qq-state-service-message-p message))
       (not (qq-state-message-recalled-p message))))

(defun qq-chat--forward-source-supported-p
    (&optional style session-key)
  "Return non-nil when SESSION-KEY may submit native merged forwarding.

STYLE is nil while discovering a plan and `merged' during dispatch."
  (condition-case nil
      (let ((type
             (qq-state-session-key-type
              (or session-key qq-chat--session-key))))
        (and (memq style '(nil merged))
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

(defun qq-chat--forward-message-candidates ()
  "Return materialized forward candidates in stable timeline order."
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
   (qq-chat--forward-message-candidates)))

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
      (dolist (message (qq-chat--forward-message-candidates))
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

(defun qq-chat--current-forward-plan ()
  "Return a stable forward plan from selected messages or point."
  (qq-chat--validate-forward-source-session nil qq-chat--session-key)
  (qq-chat--prune-message-selection)
  (let* ((selection-p (and qq-chat--message-selection t))
         (messages
          (if selection-p
              (qq-chat-selected-messages)
            (let ((message (qq-chat--message-at-point)))
              (and message (list message)))))
         (anchors (mapcar #'qq-chat--message-anchor messages))
         (memberships
          (when selection-p
            (mapcar
             (lambda (anchor)
               (let ((membership
                      (qq-chat--message-selection-find anchor)))
                 (unless membership
                   (error "qq: selected message %s lost its membership"
                          anchor))
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
     qq-chat--forward-plan-owner)))

(defun qq-chat--forward-plan-messages (plan)
  "Return PLAN's immutable message snapshots in their original order."
  (unless (qq-chat-forward-plan-p plan)
    (user-error "qq: invalid forwarding plan"))
  (let ((buffer (qq-chat-forward-plan-buffer plan))
        (session-key (qq-chat-forward-plan-session-key plan))
        (anchors (qq-chat-forward-plan-anchors plan))
        (messages (qq-chat-forward-plan-messages plan))
        (plan-owner (qq-chat-forward-plan-plan-owner plan)))
    (unless (buffer-live-p buffer)
      (user-error "qq: forwarding source buffer no longer exists"))
    (with-current-buffer buffer
      (unless (and (derived-mode-p 'qq-chat-mode)
                   (equal qq-chat--session-key session-key))
        (user-error "qq: forwarding source buffer changed sessions"))
      (unless (and plan-owner (eq plan-owner qq-chat--forward-plan-owner))
        (user-error "qq: forwarding plan belongs to a stale runtime"))
      (unless (and (consp messages)
                   (= (length anchors) (length messages)))
        (user-error "qq: forwarding plan has inconsistent messages"))
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
        (copy-tree projected)))))

(defun qq-chat--forward-request-current-p (buffer session-key owner)
  "Return non-nil when OWNER still owns BUFFER's SESSION-KEY forward request.

This is logical ownership only.  The view captured by OWNER is checked
separately before any Appkit presentation is requested."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'qq-chat-mode)
              (equal qq-chat--session-key session-key)
              (eq qq-chat--forward-request-owner owner)))))

(defun qq-chat--cancel-forward-request ()
  "Cancel and forget the active forwarding request in this buffer."
  (let ((request qq-chat--forward-request))
    (setq qq-chat--forward-request nil
          qq-chat--forward-request-owner nil
          qq-chat--forward-sync-request nil)
    (when (qq-request-p request)
      (qq-request-cancel request))))

(defun qq-chat--forward-succeeded
    (buffer session-key owner anchors memberships style target _response)
  "Settle successful forward OWNER and remove captured MEMBERSHIPS."
  (when (qq-chat--forward-request-current-p buffer session-key owner)
    (with-current-buffer buffer
      (let ((view (plist-get owner :view))
            removed)
        (dolist (snapshot memberships)
          (let* ((anchor (car snapshot))
                 (membership (qq-chat--message-selection-find anchor)))
            (when (and membership
                       (eq (qq-chat-message-selection-owner membership)
                           (cdr snapshot)))
              (setq qq-chat--message-selection
                    (delq membership qq-chat--message-selection))
              (push anchor removed))))
        (setq qq-chat--forward-request nil
              qq-chat--forward-request-owner nil)
        (when (qq-chat--captured-view-current-p view)
          (setq qq-chat--forward-sync-request
                (list :kind 'forward-settlement :owner owner :view view))
          (appkit-request-sync
           view :part 'frame :entries (nreverse removed)))))
    (setq qq-chat--last-forward-target-key target)
    (message "qq: 已%s %d 条消息到 %s"
             (qq-chat--forward-style-label style)
             (length anchors)
             target)))

(defun qq-chat--forward-failed (buffer session-key owner response reason)
  "Settle failed forward OWNER while preserving its message selection."
  (when (qq-chat--forward-request-current-p buffer session-key owner)
    (with-current-buffer buffer
      (let ((view (plist-get owner :view)))
        (setq qq-chat--forward-request nil
              qq-chat--forward-request-owner nil)
        (when (qq-chat--captured-view-current-p view)
          (setq qq-chat--forward-sync-request
                (list :kind 'forward-settlement :owner owner :view view))
          (appkit-request-sync view :part 'frame))))
    (qq-chat--default-error response reason)))

(defun qq-chat--submit-forward (style plan &optional target-session-key)
  "Submit PLAN using STYLE to TARGET-SESSION-KEY.

STYLE is `individual' or `merged'.  Cancellation and failures preserve the
selection; success removes only the immutable selection snapshot in PLAN."
  (unless (qq-chat-forward-plan-p plan)
    (user-error "qq: invalid forwarding plan"))
  (let* ((buffer (qq-chat-forward-plan-buffer plan))
         (session-key (qq-chat-forward-plan-session-key plan))
         (_validated-source
          (qq-chat--validate-forward-source-session style session-key))
         (messages (qq-chat--forward-plan-messages plan))
         (anchors (copy-sequence (qq-chat-forward-plan-anchors plan)))
         (memberships
          (copy-sequence (qq-chat-forward-plan-memberships plan)))
         ids target owner request request-installed-p)
    (with-current-buffer buffer
      (when qq-chat--forward-request-owner
        (user-error "qq: another forwarding request is already in progress"))
      (setq target
            (or target-session-key
                (qq-chat--read-forward-target style (length messages))))
      ;; The minibuffer permits process filters and recursive commands.  Treat
      ;; its return as a fresh ownership boundary: another request, a reset,
      ;; or a recall may have invalidated every pre-prompt observation.
      (when qq-chat--forward-request-owner
        (user-error "qq: another forwarding request is already in progress"))
      (qq-chat--validate-forward-source-session style session-key)
      (setq messages (qq-chat--forward-plan-messages plan)
            ids (mapcar (lambda (message) (alist-get 'server-id message))
                        messages)
            owner (list :kind 'forward
                        :style style
                        :session-key session-key
                        :target target
                        :anchors anchors
                        :view (qq-chat--ensure-view))
            qq-chat--forward-request-owner owner)
      (condition-case error-data
          (progn
            ;; Acquiring the owner and reflecting it in the header is part of
            ;; the pre-dispatch transaction.  A rendering error must not leave
            ;; an owner behind that permanently suppresses retries.
            (qq-chat--header-line-update)
            ;; Extend the transport's inhibited handoff through installation
            ;; of its returned token in this buffer.  A C-g arriving after the
            ;; socket handoff but before this assignment is deferred until the
            ;; request has a cancelable, callback-owned identity here.
            (let ((inhibit-quit t))
              (unless (eq style 'merged)
                (error "qq: unsupported forward style %S" style))
              (setq request
                    (qq-message-send-merged-forward
                     session-key target ids
                     (apply-partially
                      #'qq-chat--forward-succeeded
                      buffer session-key owner anchors memberships style target)
                     (apply-partially
                      #'qq-chat--forward-failed buffer session-key owner)))
              ;; A test adapter or transport may settle synchronously.  Never
              ;; install its already-finished request token afterward.
              (when (qq-chat--forward-request-current-p
                     buffer session-key owner)
                (setq qq-chat--forward-request request
                      request-installed-p (and request t)))
              ;; Restoring `inhibit-quit' does not guarantee an immediate
              ;; check of `quit-flag'.  Turn a quit from this outer handoff
              ;; into one synchronous signal only after token installation,
              ;; so it cannot leak to an unrelated caller safe point.
              (when quit-flag
                (setq quit-flag nil)
                (signal 'quit nil)))
            request)
        ((error quit)
         ;; Once a non-nil token is installed, the action may already have
         ;; been delivered.  Keep ownership until its response, timeout, or
         ;; explicit cancellation; clearing it here would permit a duplicate
         ;; retry with unknown first-delivery status.
         (unless request-installed-p
           (when (eq qq-chat--forward-request-owner owner)
             (setq qq-chat--forward-request nil
                   qq-chat--forward-request-owner nil)
             (ignore-errors (qq-chat--header-line-update))))
         ;; An automatically delivered deferred quit may leave `quit-flag'
         ;; set.  Consume that flag before re-signalling exactly once.
         (when (eq (car error-data) 'quit)
           (setq quit-flag nil))
         (signal (car error-data) (cdr error-data)))))))


(defun qq-chat-forward-merged (&optional plan target-session-key)
  "Forward PLAN as one merged-forward card to TARGET-SESSION-KEY."
  (interactive)
  (qq-chat--submit-forward
   'merged (or plan (qq-chat--current-forward-plan)) target-session-key))

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
    (appkit-chat-timeline-flush-deferred)))

(defun qq-chat--maybe-auto-load-older (&optional position)
  "Load an older page when POSITION approaches the timeline top."
  (let ((position (or position (point))))
    (when (and qq-chat--session-key
               (not (appkit-chatbuf-point-in-input-p))
               (appkit-chat-history-autoload-older-p
                position (point-min) qq-chat-history-auto-load-threshold))
      (when-let* ((view (qq-chat--live-current-view)))
        (qq-chat--request-callback-sync
         view (lambda () (qq-chat-load-older-messages t)))))))

(defun qq-chat--history-tail-poll-due-p (position footer composer-idle-p)
  "Return non-nil when an attached unified-history edge should be polled."
  (and qq-chat--session-key
       composer-idle-p
       (qq-chat--history-window-known-p)
       (null (appkit-chat-history-window-last-key))
       qq-chat--gateway-history-newer-cursor
       (not (appkit-chat-history-loading-p))
       (numberp qq-chat-history-auto-load-threshold)
       (> position
          (- footer (max 0 qq-chat-history-auto-load-threshold)))
       (numberp qq-chat-history-tail-poll-interval)
       (or (null qq-chat--last-tail-poll-at)
           (>= (- (float-time) qq-chat--last-tail-poll-at)
               (max 0 qq-chat-history-tail-poll-interval)))))

(defun qq-chat--maybe-auto-load-newer (&optional position)
  "Load a newer page when POSITION approaches the timeline footer."
  (let* ((position (or position (point)))
         (footer (or (appkit-chat-timeline-footer-start-position)
                     (appkit-chatbuf-input-start-position)
                     (point-max)))
         (composer-idle-p (appkit-chatbuf-composer-idle-p))
         (tail-poll-p
          (qq-chat--history-tail-poll-due-p
           position footer composer-idle-p)))
    (when (and qq-chat--session-key
               (or
                (appkit-chat-history-autoload-newer-p
                 position footer qq-chat-history-auto-load-threshold
                 composer-idle-p)
                tail-poll-p))
      (when-let* ((view (qq-chat--live-current-view)))
        (when tail-poll-p
          (setq qq-chat--last-tail-poll-at (float-time)))
        (qq-chat--request-callback-sync
         view (lambda () (qq-chat-load-newer-messages t)))))))

(defun qq-chat--install-scroll-observer (view)
  "Install VIEW's lifecycle-owned history edge observer."
  (unless (and (appkit-scroll-observer-p qq-chat--scroll-observer)
               (appkit-scroll-observer-active-p qq-chat--scroll-observer)
               (eq view
                   (appkit-scroll-observer-owner qq-chat--scroll-observer)))
    (when (appkit-scroll-observer-p qq-chat--scroll-observer)
      (appkit-scroll-observer-cancel qq-chat--scroll-observer))
    (setq-local
     qq-chat--scroll-observer
     (appkit-scroll-observer-install
      view
      :end-boundary-function #'appkit-chat-timeline-footer-start-position
      :start-function
      (lambda (_window position _start)
        (qq-chat--maybe-auto-load-older position))
      :end-function
      (lambda (window position _end)
        ;; The selected window's post-command path owns its point-based read.
        (unless (eq window (selected-window))
          (qq-chat--manage-read-position (window-point window)))
        (qq-chat--maybe-auto-load-newer position))))))

(defun qq-chat--post-command ()
  "Maintain QQ-specific timeline behavior after each command."
  (unless (appkit-chatbuf-rendering-p)
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
  (appkit-chat-history-delimiter-string
   (max 8 (qq-chat--line-fill-column))
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
     "\n"
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
  "Return the exact first-unread anchor present in MESSAGES."
  (when qq-chat-show-unread-divider
    (let* ((session (qq-chat--session))
           (count (and session (alist-get 'unread-message-count session)))
           (exact (and (integerp count)
                       (> count 0)
                       (alist-get 'first-unread-message-id session))))
      (and exact
           (seq-some (lambda (message)
                       (equal exact (qq-chat--message-anchor message)))
                     messages)
           exact))))

(defun qq-chat--compute-message-render-context (previous-message message
                                                                 first-unread-anchor)
  "Project render context for MESSAGE after PREVIOUS-MESSAGE."
  (let* ((day-key (qq-chat--message-day-key message))
         (previous-day-key
          (and previous-message (qq-chat--message-day-key previous-message)))
         (insert-date
          (and qq-chat-use-date-breaks
               previous-day-key
               day-key
               (not (equal day-key previous-day-key))
               day-key))
         (anchor (qq-chat--message-anchor message)))
    (list :compact
          (and previous-message
               (not insert-date)
               (qq-chat--messages-compact-group-p previous-message message)
               t)
          :insert-date insert-date
          :insert-unread
          (and first-unread-anchor
               anchor
               (equal anchor first-unread-anchor)
               t))))

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

(defun qq-chat--message-dependency-keys (message)
  "Return opaque resource keys that can change rendered MESSAGE."
  (let (dependencies)
    (when-let* ((reply-id (qq-chat--message-reply-id message)))
      (push (list :message reply-id) dependencies))
    (dolist (media-key (qq-chat--message-media-cache-keys message))
      (push (list :media media-key) dependencies))
    (delete-dups dependencies)))

(defun qq-chat--message-visible-in-timeline-p (message)
  "Return non-nil when MESSAGE belongs in the projected timeline."
  (or qq-chat-show-recalled-messages
      (not (qq-state-message-recalled-p message))))

(defun qq-chat--history-window-slice (messages)
  "Return the one contiguous buffer-local slice of ordered MESSAGES."
  (if (appkit-chat-history-window-empty-p)
      ;; An authoritative empty remote window may still own optimistic local
      ;; rows created after the snapshot.  They become normal window entries
      ;; when their explicit local→server rekey seeds the first exact edge.
      (seq-filter (lambda (message)
                    (not (qq-protocol-message-id-p
                          (alist-get 'server-id message))))
                  messages)
    (let ((slice
           (appkit-chat-history-window-slice
            messages #'qq-chat--message-anchor)))
      (and (plist-get slice :valid-p)
           (plist-get slice :entries)))))






(defun qq-chat--timeline-messages (&optional messages)
  "Return visible messages in the current contiguous history window."
  (let ((projected
         (when (qq-chat--history-window-known-p)
           (qq-chat--history-window-slice
            (or messages
                (and qq-chat--session-key
                     (qq-state-session-messages qq-chat--session-key))
                '())))))
    (seq-filter #'qq-chat--message-visible-in-timeline-p projected)))

(defun qq-chat--project-timeline (messages)
  "Project visible QQ MESSAGES into shared timeline rows."
  (if (null messages)
      (let* ((state 'normal)
             (placeholder (list :kind qq-chat--empty-placeholder
                                :state state)))
        (list (appkit-chat-timeline-row-create
               :key qq-chat--empty-placeholder
               :payload placeholder
               :context (list :state state))))
    (let ((first-unread (qq-chat--first-unread-anchor messages)))
      (appkit-chat-timeline-project
       messages
       #'qq-chat--message-anchor
       :context-function
       (lambda (previous message)
         (qq-chat--compute-message-render-context
          previous message first-unread))
       :dependencies-function #'qq-chat--message-dependency-keys))))

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
   :after-mutation-function #'appkit-chatbuf-update-context-mode))

(defun qq-chat--view-id ()
  "Return the opaque appkit view id for the current QQ chat."
  (unless qq-chat--session-key
    (error "qq: chat buffer has no session key"))
  (list 'chat qq-chat--session-key))

(defun qq-chat--live-current-view ()
  "Return this buffer's live canonical chat view, or nil when detached."
  (let ((view (appkit-current-view)))
    (and qq-chat--session-key
         (derived-mode-p 'qq-chat-mode)
         (appkit-view-live-p view)
         (eq (appkit-view-buffer view) (current-buffer))
         (equal (appkit-view-id view) (qq-chat--view-id))
         (equal (appkit-view-state view) qq-chat--session-key)
         view)))

(defun qq-chat--captured-view-current-p (view)
  "Return non-nil when VIEW is this buffer's live canonical chat view."
  (and view (eq view (qq-chat--live-current-view))))

(defun qq-chat--request-callback-sync (view &optional action)
  "Project accepted callback state through captured VIEW.

When ACTION is non-nil, run it inside VIEW's sync transaction after the state
projection.  A replacement or detached view is inert."
  (when (and (qq-chat--captured-view-current-p view)
             (or (null action) (functionp action)))
    (let ((request
            (if (eq view (plist-get qq-chat--callback-sync-request :view))
                qq-chat--callback-sync-request
              (list :view view :actions nil))))
      (when action
        (setf (plist-get request :actions)
              (append (plist-get request :actions) (list action))))
      (setq qq-chat--callback-sync-request request)
      (if action
          (appkit-request-sync
           view :structure t :parts '(timeline frame) :position t)
        (appkit-request-sync
         view :structure t :parts '(timeline frame))))))

(defun qq-chat--apply-state-event (event)
  "Apply queued QQ state EVENT inside an appkit sync transaction."
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

(defun qq-chat--sync-invalidations (view invalidations events)
  "Synchronize VIEW's chat from Appkit INVALIDATIONS and EVENTS."
  (let* ((parts (appkit-invalidations-parts invalidations))
         (geometry-p (memq 'geometry parts))
         (composer-p (memq 'composer parts))
         (non-composer-parts (delq 'composer (copy-sequence parts)))
         (diff
          (appkit-projection-diff-derive
           invalidations
           :existing-keys
           (and (appkit-chat-timeline-live-p)
                (appkit-chat-timeline-keys))
           :reconcile-parts '(timeline)))
         (force-keys (appkit-projection-diff-force-keys diff))
         (changed-resources
          (appkit-projection-diff-changed-dependencies diff))
         (raw-forward-sync-request qq-chat--forward-sync-request)
         (forward-sync-request
          (and (or (null (plist-get raw-forward-sync-request :view))
                   (eq view (plist-get raw-forward-sync-request :view)))
               raw-forward-sync-request))
         (raw-callback-sync-request qq-chat--callback-sync-request)
         (callback-sync-request
          (and (eq view (plist-get raw-callback-sync-request :view))
               raw-callback-sync-request))
         (callback-actions
          (copy-sequence (plist-get callback-sync-request :actions)))
         (raw-send-sync-request qq-chat--send-sync-request)
         (send-sync-request raw-send-sync-request))
    (when geometry-p
      (when-let* ((next
                   (appkit-view-responsive-width
                    qq-chat-auto-fill-margin-columns)))
        (setq-local fill-column next)))
    (dolist (event events)
      (when (appkit-view-live-p view)
        (qq-chat--apply-state-event event)))
    (when (appkit-view-live-p view)
      (let (rendered-p)
        (cond
         (callback-sync-request
          (qq-chat-render)
          (setq rendered-p t))
         (geometry-p
          (qq-chat--sync-timeline
           :force-keys force-keys
           :changed-resources changed-resources)
          (qq-chat--update-frame))
         (forward-sync-request
          (when (or force-keys changed-resources)
            (qq-chat--sync-timeline
             :force-keys force-keys
             :changed-resources changed-resources))
          (qq-chat--update-frame))
         (send-sync-request
          (qq-chat--update-frame))
         ((and (null events)
               (or (appkit-invalidations-structure-p invalidations)
                   non-composer-parts
                   (appkit-invalidations-position-p invalidations)))
          (qq-chat-render)
          (setq rendered-p t))
         ((or force-keys changed-resources)
          (qq-chat--sync-timeline
           :force-keys force-keys
           :changed-resources changed-resources)))
        (when (and rendered-p (or force-keys changed-resources))
          (qq-chat--sync-timeline
           :force-keys force-keys
           :changed-resources changed-resources))
        (when composer-p
          (qq-chat--refresh-prompt))
        (when send-sync-request
          (qq-chat--materialize-pending-send-restoration))
        (when forward-sync-request
          (qq-chat--header-line-update))
        (when (eq raw-forward-sync-request qq-chat--forward-sync-request)
          (setq qq-chat--forward-sync-request nil))
        (when (eq raw-callback-sync-request qq-chat--callback-sync-request)
          (setq qq-chat--callback-sync-request nil))
        (when (eq raw-send-sync-request qq-chat--send-sync-request)
          (setq qq-chat--send-sync-request nil))
        (when send-sync-request
          (appkit-chatbuf-focus-input))
        (dolist (action callback-actions)
          (when (qq-chat--captured-view-current-p view)
            (funcall action)))
        (when (appkit-scroll-observer-p qq-chat--scroll-observer)
          (appkit-scroll-observer-check qq-chat--scroll-observer))))))

(defun qq-chat--ensure-view ()
  "Return the live Appkit view owning the current QQ chat buffer."
  (let ((owner
         (or (qq-runtime-current-account-id)
             (user-error "qq: chat buffer has no account owner"))))
    (qq-runtime-bind-account owner)
    (let ((view
           (qq-runtime-ensure-account-view
            :id (qq-chat--view-id)
            :mode 'qq-chat-mode
            :state qq-chat--session-key
            :sync-function #'qq-chat--sync-invalidations
            :parts '(frame timeline composer geometry))))
      (let ((history-owner (appkit-chat-history-request-owner)))
        (when (and (appkit-view-operation-p history-owner)
                   (not (appkit-chat-history-request-current-p
                         history-owner)))
          (appkit-chat-history-request-cancel)
          (when (eq qq-chat--initial-history-owner history-owner)
            (setq qq-chat--initial-history-owner nil))))
      (qq-chat--install-scroll-observer view)
      (appkit-view-enable-responsive-geometry view)
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
  (appkit-chat-timeline-sync
   (qq-chat--project-timeline
    (if messages-p messages (qq-chat--timeline-messages)))
   :force-keys force-keys
   :changed-resources changed-resources
   :rekeys rekeys))

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
    (appkit-chat-timeline-invalidate
     anchors :defer-while-mark-active t)))

(defun qq-chat--render-empty-placeholder (state)
  "Insert the empty timeline placeholder row for STATE."
  (let ((start (point)))
    (appkit-presentation-insert-note-line
     (pcase state
       ('searching "Searching messages…")
       ('no-match "No matching messages.")
       ('normal "No messages loaded yet.")
       (_ (error "qq: invalid empty timeline state %S" state))))
    (insert "\n")
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
      (qq-chat--render-message message context)))))

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
         (qq-media--image-display-string image (format "[mface:%s]" summary))))
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
                            (qq-media-composer-image-preview file))))
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
           (qq-media--image-display-string
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

When SEGMENT-TYPE is nil, infer the most useful QQ segment type from PATH."
  (interactive
   (list (appkit-media-read-file-name "Attach file: ")
         nil))
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
  "Insert a QQ base face (system emoji) into the chat composer.

With FACE-ID (string or number), insert that face directly.  Interactively,
prompt with completing-read over face names (`/斜眼笑') and ids, each
prefixed with the local face PNG when available.

Candidates stay in QQ face-id order (0, 1, 2, …) via completion
metadata (`display-sort-function' = identity), so Vertico/Icomplete
match QQ's base emoji panel order instead of history/length sort.

Sends as a structured native `face' segment, not Unicode or inline text.
Bound via `qq-chat-attach-emoji' (`C-c C-e'); attach transient `e'."
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

(defun qq-chat--insert-custom-face (face)
  "Insert favorite FACE into the composer by durable native identity."
  (let ((segment (qq-media-custom-face-to-segment face)))
    (qq-chat--insert-input-segment-object
     segment (qq-media-custom-face-display-string face))
    (qq-chat--sync-draft-from-buffer)
    (message "qq: favorite %s" (qq-media-custom-face-label face))))

(defun qq-chat--pick-custom-face (faces)
  "Completing-read among FACES and insert the chosen favorite.

Uses the shared Appkit candidate layer and preserves native catalog order."
  (unless faces
    (user-error "qq: no favorite custom faces (收藏表情为空)"))
  (qq-chat--insert-custom-face (qq-completion-read-custom-face faces)))

(defun qq-chat-attach-custom-face (&optional force-refresh)
  "Insert a favorite custom face (收藏表情) into the chat composer.

The native catalog contributes only a durable favorite identity to the draft.
At send time Gateway materializes verified bytes and the existing image
attachment pipeline prepares them with image subtype 1.

With prefix FORCE-REFRESH, bypass Gateway's catalog cache.
Bound via `C-u C-c C-e' or attach transient `E'."
  (interactive "P")
  (let ((buffer (current-buffer))
        (session-key qq-chat--session-key)
        (view (qq-chat--ensure-view)))
    (message "qq: loading favorite faces…")
    (qq-media-ensure-custom-faces
     (lambda (faces)
       (when (and (buffer-live-p buffer) view)
         (with-current-buffer buffer
           (when (and (equal qq-chat--session-key session-key)
                      (qq-chat--captured-view-current-p view))
             (qq-chat--request-callback-sync
              view
              (lambda ()
                (condition-case err
                    (qq-chat--pick-custom-face faces)
                  (error
                   (message "%s" (error-message-string err))))))))))
     (lambda (_response reason)
       (message "qq: failed to load favorites: %s" reason))
     force-refresh)))

(defun qq-chat-attach-emoji (&optional custom-p)
  "Attach a QQ emoji into the composer.

Without prefix: system base face (`face' id, same as QQ 小黄脸).
With prefix CUSTOM-P (`C-u'): favorite custom face (收藏表情).

Keys: `C-c C-e' / `C-u C-c C-e'."
  (interactive "P")
  (if custom-p
      (qq-chat-attach-custom-face (equal custom-p '(16)))
    (call-interactively #'qq-chat-attach-face)))

(defun qq-chat--clipboard-temp-file (extension)
  "Return a unique temp file path with EXTENSION (including the leading dot)."
  (let ((dir (expand-file-name "clipboard" qq-media-cache-directory)))
    (make-directory dir t)
    (make-temp-file (expand-file-name "qq-clip-" dir) nil extension)))

(defun qq-chat--write-binary-temp-file (data extension)
  "Write binary DATA to a clipboard temp file with EXTENSION and return path."
  (let ((path (qq-chat--clipboard-temp-file extension))
        (coding-system-for-write 'binary))
    (with-temp-file path
      (set-buffer-multibyte nil)
      (insert data))
    path))

(defun qq-chat--uri-list-local-paths (uri-list)
  "Parse URI-LIST (text/uri-list) into absolute local file paths."
  (let (paths)
    (dolist (uri (split-string (or uri-list "") "[\r\n\0]" t))
      (setq uri (string-trim uri))
      (when (and (not (string-empty-p uri))
                 (not (string-prefix-p "#" uri)))
        (cond
         ((string-match-p "\\`file:" uri)
          (require 'url-parse)
          (let* ((parsed (url-generic-parse-url uri))
                 (raw (url-filename parsed))
                 (path (and raw (url-unhex-string raw))))
            ;; url-filename keeps leading "/" on file:///path.
            (when (appkit-media-readable-file-p path)
              (push (expand-file-name path) paths))))
         ((and (file-name-absolute-p uri)
               (appkit-media-readable-file-p uri))
          (push (expand-file-name uri) paths)))))
    (nreverse paths)))

(defun qq-chat--clipboard-selection (type)
  "Return CLIPBOARD selection for TYPE, or nil."
  (condition-case nil
      (let ((selection-coding-system
             (if (memq type '(image/png image/jpeg image/bmp))
                 'no-conversion
               selection-coding-system)))
        (gui-get-selection 'CLIPBOARD type))
    (error nil)))

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
    (qq-chat-attach-file path type)
    path))

(defun qq-chat-attach-clipboard (&optional as-file-p)
  "Attach clipboard content to the chat composer (telega `C-c C-v').

Prefer order:
1. `text/uri-list' local files from the clipboard
2. image bytes (`image/png', `image/jpeg')
3. on Darwin, `pngpaste' if available

With `\\[universal-argument]' (AS-FILE-P), force segment type `file' instead of
image/video inference."
  (interactive "P")
  (cond
   ;; macOS: pngpaste is the reliable image path (same as telega).
   ((and (eq system-type 'darwin)
         (executable-find "pngpaste"))
    (let ((tmp (qq-chat--clipboard-temp-file ".png")))
      (unless (= 0 (call-process "pngpaste" nil nil nil tmp))
        (ignore-errors (delete-file tmp))
        (user-error "qq: no image in clipboard (pngpaste failed)"))
      (qq-chat-attach-file tmp (if as-file-p "file" "image"))))
   (t
    (let* ((targets (qq-chat--clipboard-selection 'TARGETS))
           (target-list (cond
                         ((stringp targets) (split-string targets))
                         ((listp targets) targets)
                         ((vectorp targets) (append targets nil))
                         (t nil)))
           (has-uri (cl-find "text/uri-list" target-list :test #'string-equal))
           (uris (and has-uri (qq-chat--clipboard-selection 'text/uri-list)))
           (paths (and uris (qq-chat--uri-list-local-paths uris))))
      (cond
       (paths
        (dolist (path paths)
          (qq-chat-attach-file
           path
           (when as-file-p "file"))))
       (t
        (let ((attached nil))
          (dolist (mime '(image/png image/jpeg image/bmp))
            (unless attached
              (when-let* ((data (qq-chat--clipboard-selection mime)))
                (qq-chat--yank-media mime data as-file-p)
                (setq attached t))))
          (unless attached
            (user-error "qq: no file or image in clipboard")))))))))

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
         :cancel-action #'qq-chat-cancel-dwim
         :cancel-help "Cancel reply (C-c C-k)"
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

(defun qq-chat--insert-unread-divider-row ()
  "Insert the unread separator row above the first unread message.

Label matches telega's unread bar wording (\"Unread Messages\")."
  (appkit-presentation-insert-note-line
   "Unread Messages"
   :face 'qq-msg-unread-divider))

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

(defun qq-chat--messages-pop-ring-last-p (message-id)
  "Return non-nil when MESSAGE-ID is already the top of the pop ring."
  (and (ring-p qq-chat--messages-pop-ring)
       (not (ring-empty-p qq-chat--messages-pop-ring))
       (equal (format "%s" (ring-ref qq-chat--messages-pop-ring 0))
              (format "%s" message-id))))

(defun qq-chat--messages-pop-ring-push (message-id)
  "Push MESSAGE-ID onto the jump-back ring (telega chatbuf pop ring)."
  (when (and message-id (ring-p qq-chat--messages-pop-ring))
    (let ((id (format "%s" message-id)))
      (unless (qq-chat--messages-pop-ring-last-p id)
        (ring-insert qq-chat--messages-pop-ring id)
        (message "qq: %s to jump back"
                 (or (key-description
                      (where-is-internal #'qq-chat-goto-pop-message
                                         qq-chat-timeline-mode-map t))
                     "x"))))))

(defun qq-chat--goto-loaded-message (anchor &optional highlight)
  "Goto timeline ANCHOR only if already displayed (telega `--goto-loaded-msg').

Return non-nil on success.  When HIGHLIGHT is non-nil, pulse the block."
  (when-let* ((id (and anchor (format "%s" anchor)))
              (pos (qq-chat--message-position id)))
    (goto-char pos)
    (when-let* ((win (get-buffer-window (current-buffer) t)))
      (set-window-point win pos)
      (with-selected-window win
        (goto-char pos)
        ;; telega-button--make-observable equivalent: keep target in view.
        (recenter)))
    (when highlight
      (qq-chat--highlight-region pos (qq-chat--message-end-position pos)))
    t))

(defun qq-chat--jump-history-count ()
  "Return history page size used when seeking a jump target."
  (max 1 (or qq-chat-jump-history-count qq-history-fetch-count)))

(defun qq-chat--fetch-history-around
    (session-key message-id callback
                 &optional errback count sequence lifecycle-owner)
  "Fetch SESSION-KEY history around MESSAGE-ID or authored SEQUENCE.

CALLBACK and ERRBACK receive settlement; COUNT limits rows.  LIFECYCLE-OWNER
owns cancellation of the request."
  (qq-core-fetch-history-around
   session-key message-id callback errback count sequence lifecycle-owner))

(defun qq-chat--jump-target-key (target sequence)
  "Return one buffer-local pending-jump key for TARGET or SEQUENCE."
  (or (and target (format "%s" target))
      (and sequence (concat "sequence:" sequence))))

(defun qq-chat--finish-jump-if-loaded (target &optional sequence)
  "If TARGET or SEQUENCE resolves to a rendered row, jump and finish.

Return non-nil on success."
  (let* ((source
          (or (and target (qq-chat--message-by-server-id target))
              (and sequence (qq-chat--message-by-sequence sequence))))
         (resolved-anchor (qq-state-message-anchor source)))
    (when (and resolved-anchor
               (qq-chat--goto-loaded-message resolved-anchor t))
      (setq qq-chat--pending-jump-id nil)
      t)))

(defun qq-chat--jump-fail (target &optional reason)
  "Clear pending jump for TARGET and report not found."
  (when (equal qq-chat--pending-jump-id target)
    (setq qq-chat--pending-jump-id nil))
  (message "qq: message not found%s"
           (if (and reason (not (string-empty-p reason)))
               (format " (%s)" reason)
             "")))

(defun qq-chat--seek-history-for-jump
    (session-key target buffer &optional sequence)
  "Load SESSION-KEY history around exact TARGET or SEQUENCE in BUFFER."
  (let ((jump-key (qq-chat--jump-target-key target sequence)))
    (qq-chat--cancel-initial-history-request)
    (qq-chat--adopt-gateway-message-frontier)
    (let* ((view (qq-chat--ensure-view))
           (owner (appkit-chat-history-request-start view 'around)))
      (qq-chat--prepare-around-history-window)
      (appkit-request-sync view :part 'frame)
      (qq-chat--fetch-history-around
       session-key
       target
       (lambda (meta)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (when (and (equal qq-chat--session-key session-key)
                        (appkit-chat-history-request-end owner))
               (qq-chat--record-gateway-history-range meta)
               (qq-chat--note-history-window meta)
               (qq-chat--request-callback-sync
                view
                (lambda ()
                  (unless (qq-chat--finish-jump-if-loaded
                           target sequence)
                    (qq-chat--jump-fail
                     jump-key "around window omitted target"))))))))
       (lambda (_response reason)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (when (and (equal qq-chat--session-key session-key)
                        (appkit-chat-history-request-end owner))
               (qq-chat--request-callback-sync
                view (lambda () (qq-chat--jump-fail jump-key reason)))))))
       (qq-chat--jump-history-count)
       sequence
       owner))))

(defun qq-chat-goto-message (message-id &optional no-pop sequence)
  "Goto MESSAGE-ID or authored SEQUENCE in the current chatbuf.

Push the message at point onto the pop ring unless NO-POP.  Already loaded
targets jump immediately; native reply targets seek by their conversation
sequence without inventing a snowflake identity."
  (interactive
   (let* ((message (qq-chat--message-at-point))
          (sequence
           (or (get-text-property (point) 'qq-chat-reply-sequence)
               (qq-chat--message-reply-sequence message)))
          (message-id
           (or (get-text-property (point) 'qq-chat-reply-id)
               (qq-chat--message-reply-id message)
               (unless sequence (read-string "Message id: ")))))
     (list message-id nil sequence)))
  (unless qq-chat--session-key
    (user-error "qq: this buffer is not bound to a session"))
  (let ((id (and message-id (format "%s" message-id)))
        (sequence (and sequence (format "%s" sequence)))
        (session-key qq-chat--session-key)
        (buffer (current-buffer)))
    (when (and id (string-empty-p id))
      (setq id nil))
    (when (and sequence (string-empty-p sequence))
      (setq sequence nil))
    (unless (or id sequence)
      (user-error "qq: no message locator to jump to"))
    (let ((jump-key (qq-chat--jump-target-key id sequence)))
      ;; telega: put message at point into messages-pop-ring.
      (unless no-pop
        (when-let* ((at-point (qq-chat--message-at-point))
                    (cur-id (qq-chat--message-anchor at-point)))
          (unless (equal (format "%s" cur-id) jump-key)
            (qq-chat--messages-pop-ring-push cur-id))))
      (unless (qq-chat--finish-jump-if-loaded id sequence)
        (setq qq-chat--pending-jump-id jump-key)
        (message "qq: loading…")
        (qq-chat--seek-history-for-jump
         session-key id buffer sequence)))))

(defun qq-chat-goto-reply (&optional message)
  "Goto the message that MESSAGE replies to.

MESSAGE defaults to the message at point.  Bound to timeline `g'.  The ↪
reply preview line is also a button that calls this path, corresponding to
telega's `telega-msg-goto-reply-to-message'."
  (interactive)
  (let* ((msg (or message
                  (qq-chat--message-at-point)
                  (user-error "qq: no message at point")))
         (reply-id (qq-chat--message-reply-id msg))
         (sequence (qq-chat--message-reply-sequence msg)))
    (unless (or reply-id sequence)
      (user-error "qq: message is not a reply"))
    (qq-chat-goto-message reply-id nil sequence)))

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

(defun qq-chat--insert-reply-preview-line (reply-data properties prefix-state)
  "Insert one inline reply preview line for normalized REPLY-DATA.

Telega uses `telega-ins--with-props' together with its goto-reply action on the
reply header.  Here the line is a button (RET / mouse-1) that selects either
an exact Message ID center or an authored conversation-sequence center."
  (let* ((reply-id (or (alist-get 'message_id reply-data)
                       (alist-get 'id reply-data)))
         (reply-sequence (or (alist-get 'message_seq reply-data)
                             (alist-get 'sequence reply-data)))
         (source (if reply-sequence
                     (qq-chat--message-by-reply-sequence reply-sequence)
                   (qq-chat--message-by-server-id reply-id)))
         (sequence (or (alist-get 'message-seq source)
                       (and (not (eq (qq-state-session-key-type
                                      qq-chat--session-key)
                                     'private))
                            reply-sequence)))
         (sender (or (and source
                          (car (qq-chat--message-sender-display-parts source)))
                     (alist-get 'sender_name reply-data)))
         (preview (and source (string-trim (or (qq-state-message-preview source) ""))))
         (body (cond
                ((and sender preview (not (string-empty-p preview)))
                 (format "%s: %s" sender
                         (truncate-string-to-width preview 56 nil nil t)))
                ((and preview (not (string-empty-p preview)))
                 (truncate-string-to-width preview 64 nil nil t))
                (sender (format "%s's message" sender))
                ((and reply-id (not sequence))
                 (format "id %s" reply-id))
                (reply-sequence (format "message #%s" reply-sequence))
                (t "replied message")))
         (reply-start (point))
         (target (or (qq-state-message-anchor source)
                     (and reply-id (not sequence)
                          (format "%s" reply-id))))
         (map (let ((map (make-sparse-keymap)))
                (set-keymap-parent map button-map)
                (define-key map [mouse-1]
                            (lambda ()
                              (interactive)
                              (qq-chat-goto-message target nil sequence)))
                (define-key map (kbd "RET")
                            (lambda ()
                              (interactive)
                              (qq-chat-goto-message target nil sequence)))
                map)))
    (insert (format "↪ %s\n" body))
    (add-text-properties
     reply-start (point)
     (append properties
             (list 'face 'qq-msg-inline-reply
                   'mouse-face 'highlight
                   'help-echo "Jump to replied message (telega-style; g / RET)"
                   'follow-link t
                   'keymap map
                   'button t
                   'category 'default-button
                   'action (lambda (_button)
                             (qq-chat-goto-message target nil sequence))
                   'qq-chat-reply-id target
                   'qq-chat-reply-sequence sequence
                   'qq-chat-reply-button t)))
    (appkit-ui-apply-line-prefix reply-start (point) prefix-state)))

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
    (add-text-properties
     0 (length display)
     (list 'face mention-face
           'qq-chat-mention-kind kind
           'qq-chat-mention-user-id (and profile-p target)
           'rear-nonsticky '(qq-chat-mention-kind
                             qq-chat-mention-user-id))
     display)
    display))

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
         (core (concat "( " content " )"))
         (available (- (qq-chat--line-fill-column)
                       (string-width core)
                       time-width
                       8))
         (bar-count (max 0 (floor (/ (max 0 available) 2))))
         (bars (make-string bar-count ?─))
         (start (point))
         (poke-properties (append properties (list 'face 'qq-msg-poke))))
    (insert bars " " core " " bars)
    (qq-chat--insert-right-aligned-time time nil nil)
    (insert "\n")
    (add-text-properties start (point) poke-properties)
    (when (not (string-empty-p time))
      (save-excursion
        (goto-char (1- (point)))
        (let ((time-start (- (point) (length time))))
          (add-text-properties time-start (point)
                               (list 'face 'qq-msg-status)))))))

(defun qq-chat--card-segment-p (segment)
  "Return non-nil when SEGMENT is a normalized Ark rich card."
  (equal (alist-get 'type segment) "card"))

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
       card-prefix-state subject :properties properties))
    (when-let* ((body (cond
                       ((and (stringp content) (not (string-empty-p content))) content)
                       ((and (stringp prompt) (not (string-empty-p prompt))) prompt))))
      (appkit-ui-insert-prefixed-lines
       card-prefix-state body :face 'shadow :properties properties))
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
    ("announcement" "Group Announcement")
    ("contact" "Contact")
    (_ "Card")))

(defun qq-chat--insert-card-segment (segment prefix-state properties)
  "Insert normalized Ark rich-card SEGMENT."
  (let* ((data (alist-get 'data segment))
         (kind (alist-get 'kind data))
         (label (qq-chat--card-kind-label kind))
         (title (qq-chat--present-string (alist-get 'title data)))
         (content (qq-chat--present-string (alist-get 'content data)))
         (prompt (qq-chat--present-string (alist-get 'prompt data)))
         (source (qq-chat--present-string (alist-get 'source data)))
         (summary (qq-chat--present-string (alist-get 'summary data)))
         (url (qq-chat--present-string (alist-get 'url data)))
         (image-url (qq-chat--present-string (alist-get 'image data)))
         (body (or content
                   (and (not title) prompt)))
         (open-action (and url (lambda () (browse-url url t))))
         (map (when open-action
                (let ((map (make-sparse-keymap)))
                  (set-keymap-parent map button-map)
                  (define-key map (kbd "RET")
                              (lambda () (interactive) (funcall open-action)))
                  (define-key map [mouse-1]
                              (lambda () (interactive) (funcall open-action)))
                  map)))
         (card-properties
          (append
           properties
           (when open-action
             (list 'mouse-face 'highlight
                   'help-echo (format "Open this %s" (downcase label))
                   'follow-link t
                   'keymap map
                   'button t
                   'category 'default-button
                   'action (lambda (_button) (funcall open-action))))))
         (appkit-ui-card-indent-prefix-state prefix-state)
         (card-prefix-state (appkit-ui-card-prefix-state))
         (start (point)))
    (appkit-ui-insert-prefixed-lines
     card-prefix-state
     (if source (format "%s · %s" label source) label)
     :face 'bold
     :properties card-properties)
    (when title
      (appkit-ui-insert-prefixed-lines
       card-prefix-state title :properties card-properties))
    (when image-url
      (let ((image-start (point)))
        (insert
         (qq-media-url-preview-display-string
          (format "card-image-url:%s" image-url)
          image-url "" qq-media-preview-image-height))
        (insert "\n")
        (appkit-ui-apply-line-prefix image-start (point) card-prefix-state)
        (add-text-properties image-start (point) card-properties)))
    (when body
      (appkit-ui-insert-prefixed-lines
       card-prefix-state body :face 'shadow :properties card-properties))
    (when (and summary (not (equal summary body)))
      (appkit-ui-insert-prefixed-lines
       card-prefix-state summary :face 'shadow :properties card-properties))
    (when open-action
      (add-text-properties start (point) card-properties))))

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
     card-prefix-state kind-label :face 'bold :properties properties)
    (when title
      (appkit-ui-insert-prefixed-lines
       card-prefix-state title :properties properties))
    (when subtitle
      (appkit-ui-insert-prefixed-lines
       card-prefix-state subtitle :face 'shadow :properties properties))
    (when (and content (not (equal content title)))
      (appkit-ui-insert-prefixed-lines
       card-prefix-state content :face 'shadow :properties properties))))

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

(defun qq-chat--segment-media-card-context (segment &optional capabilities)
  "Adapt media SEGMENT to the shared card action protocol.

CAPABILITIES defaults to the centralized `qq-media' action/status model.
The exact account app is captured while rendering; later actions never resolve
a replacement app instance."
  (let* ((view (appkit-current-view))
         (owner (and (appkit-view-live-p view)
                     (eq (appkit-app-kind (appkit-view-app view)) 'qq)
                     (appkit-view-app view)))
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
                      (qq-media-segment-open segment :owner owner)))
     :download-action (when (plist-get capabilities :download)
                        (lambda ()
                          (qq-media-segment-start-download
                           segment nil :owner owner)))
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

(defun qq-chat--insert-segment-media-line (segment prefix-state properties)
  "Insert one rich media card for SEGMENT using PREFIX-STATE and PROPERTIES."
  (let* ((kind-label (qq-chat--segment-media-kind-label segment))
         (meta (qq-chat--segment-media-meta-line segment))
         (capabilities (qq-media-segment-capabilities segment))
         (transfer (qq-media-segment-transfer segment))
         (context (qq-chat--segment-media-card-context
                   segment capabilities))
         (native-record-p (and (qq-media--native-record-media-id segment) t))
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
         (let ((preview-start (point))
               (preview (qq-media-segment-preview-image segment))
               (loading (qq-media-segment-preview-fetching-p segment))
               preview-end)
           (cond
            (preview
             (condition-case _
                 (progn
                   (appkit-media-insert-image-slices
                    preview nil nil
                    (cond
                     ((equal (alist-get 'type segment) "mface") "[sticker]")
                     ((qq-media-videoish-segment-p segment) "[video]")
                     (t "[image]")))
                   (setq preview-end (point)))
               (error
                (insert "[preview unavailable]")))
             (when preview-end
               (appkit-media-add-action-properties
                preview-start preview-end
                (lambda (&optional _event)
                  (interactive)
                  (appkit-media-card-call-action 'open context))
                (format "Open %s" (downcase kind-label))))
             (insert "\n"))
            (loading
             (insert "[loading preview]\n"))
            (t
             (insert "[preview unavailable]\n")))
           (appkit-ui-apply-line-prefix preview-start (point) card-prefix-state)
           (appkit-ui-append-face preview-start (point) 'shadow))))))))

(defun qq-chat--insert-animated-face-segment (segment prefix-state properties)
  "Insert animated face SEGMENT below the avatar's two-line header."
  ;; Consume the avatar's normal-height first-body slice before inserting the
  ;; tall animation.  Otherwise line-prefix stretches that slice to the media
  ;; line and makes one avatar look like two vertically separated avatars.
  (let ((avatar-tail-start (point)))
    (insert " \n")
    (appkit-ui-apply-line-prefix avatar-tail-start (point) prefix-state)
    (add-text-properties avatar-tail-start (point) properties))
  (let ((animation-start (point)))
    (insert (or (qq-chat--segment-inline-string segment)
                (qq-state-message-preview-from-segments (list segment))))
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

(defun qq-chat--friend-pin-target ()
  "Return the current private friend's exact UIN, or signal `user-error'."
  (let* ((session (or (qq-chat--session)
                      (user-error "qq: this buffer has no session")))
         (user-id (or (alist-get 'peer-uin session)
                      (alist-get 'target-id session))))
    (unless (eq (alist-get 'type session) 'private)
      (user-error "qq: conversation pinning here requires a private chat"))
    (unless (qq-protocol-user-uin-p user-id)
      (user-error "qq: private chat has no exact peer UIN"))
    (unless (qq-state-friend user-id)
      (user-error "qq: conversation pinning requires a current friend"))
    user-id))

(defun qq-chat--friend-pin-capable-p ()
  "Return non-nil when the current chat targets an authoritative friend."
  (condition-case nil
      (progn (qq-chat--friend-pin-target) t)
    (user-error nil)))

(defun qq-chat--set-friend-pinned (pinned)
  "Set the current friend conversation's PINNED state explicitly."
  (let ((user-id (qq-chat--friend-pin-target)))
    (qq-core-set-friend-pinned
     user-id pinned
     (lambda (_receipt)
       (message "qq: friend conversation %s"
                (if pinned "pinned" "unpinned"))))))

(defun qq-chat-pin-friend ()
  "Pin the current friend conversation."
  (interactive)
  (qq-chat--set-friend-pinned t))

(defun qq-chat-unpin-friend ()
  "Unpin the current friend conversation."
  (interactive)
  (qq-chat--set-friend-pinned nil))

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
  "Toggle essence state for MESSAGE or the message at point."
  (interactive)
  (let* ((message (or message
                      (qq-chat--message-at-point)
                      (user-error "qq: no message at point")))
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
  "Apply todo OPERATION to MESSAGE or the message at point."
  (let* ((message (or message
                      (qq-chat--message-at-point)
                      (user-error "qq: no message at point")))
         (_capable
          (or (qq-chat--message-todo-capable-p message)
              (user-error "qq: todo requires a live group message"))))
    (qq-core-set-message-todo
     message operation
     (lambda (_receipt)
       (message "qq: message todo %s" operation)))))

(defun qq-chat-set-message-todo (&optional message)
  "Set MESSAGE or the message at point as a group todo."
  (interactive)
  (qq-chat--mutate-message-todo 'set message))

(defun qq-chat-complete-message-todo (&optional message)
  "Complete the group todo for MESSAGE or the message at point."
  (interactive)
  (qq-chat--mutate-message-todo 'complete message))

(defun qq-chat-cancel-message-todo (&optional message)
  "Cancel the group todo for MESSAGE or the message at point."
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
  (let* ((message-id (alist-get 'server-id message))
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
    (message &key compact (avatar-p t) selected-p)
  "Return shared presentation layout for MESSAGE.

COMPACT makes the first body line use the ordinary continuation prefix.
AVATAR-P controls whether the shared two-line avatar occupies the heading and
first body line.  SELECTED-P adds a stable mark to every visual line without
inserting a synthetic timeline row.  The returned plist owns a fresh mutable
body prefix state."
  (let* ((avatar-prefixes
          (and avatar-p (qq-chat--message-avatar-prefixes message)))
         (selection-prefix
          (if selected-p
              (propertize "▌ " 'face 'qq-msg-selected-marker)
            ""))
         (header-prefix
          (concat selection-prefix
                  (or (plist-get avatar-prefixes :header) "")))
         (body-first-prefix
          (concat selection-prefix
                  (or (plist-get avatar-prefixes :first-body) "  ")))
         (body-rest-prefix
          (concat selection-prefix
                  (or (plist-get avatar-prefixes :rest-body) "  "))))
    (list :header-prefix header-prefix
          :body-rest-prefix body-rest-prefix
          :body-prefix-state
          (if compact
              (appkit-ui-make-prefix-state
               body-rest-prefix body-rest-prefix)
            (appkit-ui-make-prefix-state
             body-first-prefix body-rest-prefix)))))

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
    (appkit-ui-append-face header-start (point) 'qq-msg-heading)
    (add-text-properties header-start (point) properties)
    (plist-get layout :body-prefix-state)))

(defun qq-chat--render-message (message context)
  "Insert one formatted MESSAGE block using projected CONTEXT.

Visual model (telega-inspired; later appkit):
- optional date / unread bars above the node
- heading row: avatar + sender + status + time (`qq-msg-heading')
- optional reply preview (`qq-msg-inline-reply')
- body with light indent (no `>>'/`|' gutters)
- no per-message action button row (use `C-c m r/d/o/a' at point)"
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
           message :compact compact :avatar-p ordinary-message-p
           :selected-p selected))
         (header-prefix (plist-get layout :header-prefix))
         (body-rest-prefix (plist-get layout :body-rest-prefix))
         (body-prefix-state (plist-get layout :body-prefix-state))
         (short-time (qq-chat--format-time-short (alist-get 'time message)))
         content-start)
    (when (and (stringp insert-date) (not (string-empty-p insert-date)))
      (qq-chat--insert-date-separator-row (qq-chat--message-day-label insert-date)))
    (when insert-unread
      (qq-chat--insert-unread-divider-row))
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
         reply-data properties body-prefix-state))
      (qq-chat--insert-compact-message-body
       message body-prefix-state properties short-time))
     (t
      (qq-chat-insert-message-heading
       message properties layout
       :title-face title-face
       :status-suffix status-suffix)
      (when reply-data
        (qq-chat--insert-reply-preview-line
         reply-data properties body-prefix-state))
      (qq-chat--insert-message-body message body-prefix-state properties)))
    ;; Gray tips and pokes bypass the ordinary heading/body layout.  Give their
    ;; single visual row the same stable selection stripe without manufacturing
    ;; a second timeline row.
    (when (and selected (qq-state-service-message-p message))
      (appkit-ui-apply-line-prefix
       content-start (point)
       (appkit-ui-make-prefix-state header-prefix header-prefix)))
    (unless (or (qq-state-message-recalled-p message)
                (qq-state-service-message-p message))
      (qq-chat--insert-essence-line message body-prefix-state properties)
      (qq-chat--insert-reaction-line message body-prefix-state properties))
    (insert "\n")
    (add-text-properties start (point) properties)))

(defun qq-chat-render ()
  "Synchronize current chat frame and projected timeline from local state."
  (interactive)
  (unless qq-chat--session-key
    (user-error "qq: this buffer is not bound to a session"))
  (qq-chat--ensure-view)
  ;; A replacement view may be the first presentation transaction after a
  ;; failed-send callback restored canonical state.  Never synchronize its
  ;; still-empty tail over that authoritative state.
  (unless qq-chat--send-sync-request
    (when (appkit-chatbuf-input-region-bounds)
      (appkit-chatbuf-input-state-sync :reset-history-p nil)))
  (qq-chat--header-line-update)
  ;; Establish the EWOC rows before the trailing composer.  On an empty EWOC
  ;; the footer's tail boundary is not stable until first reconciliation;
  ;; binding input first can leave the prompt inside the message region.
  (qq-chat--sync-timeline)
  (qq-chat--update-frame)
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
  (qq-chat-return-to-latest t))

(defun qq-chat--note-history-window (meta &optional remote-latest-id)
  "Record the canonical around-message slice in META.

REMOTE-LATEST-ID is the live frontier captured before the request.  The older
flag owns a validated continuation across local and native coverage; the newer
flag describes rows already materialized after this slice."
  (pcase-let* ((`(,oldest . ,newest) (qq-chat--history-batch-bounds meta))
               (has-older
                (plist-get meta :history-has-older-p))
               (has-newer
                (plist-get meta :history-has-newer-materialized-p))
               (latest (or qq-chat--remote-latest-id remote-latest-id))
               (stale-frontier
                (and has-newer
                     (qq-chat--history-frontier-behind-batch-p
                      latest newest
                      (plist-get meta :batch-message-ids)))))
    (setq qq-chat--remote-latest-id
          (cond ((not has-newer) newest)
                (stale-frontier nil)
                (t latest)))
    (appkit-chat-history-older-loaded-set (not has-older))
    (when newest
      (qq-chat--set-history-window oldest (and has-newer newest)))))

(defun qq-chat--load-newer-gateway-messages (&optional quiet)
  "Extend the current unified Gateway history window toward newer rows."
  (unless qq-chat--gateway-history-newer-cursor
    (user-error "qq: History has no newer continuation cursor"))
  (let* ((session-key qq-chat--session-key)
         (history-cursor (copy-tree qq-chat--gateway-history-newer-cursor))
         (buffer (current-buffer))
         (view (qq-chat--ensure-view))
         (window-cursor (appkit-chat-history-window-last-key))
         (owner (appkit-chat-history-request-start view 'newer))
         (point-anchor
          (and (not (appkit-chatbuf-point-in-input-p))
               (get-text-property (point) 'qq-chat-message-anchor)))
         (point-anchor-offset 0))
    (when point-anchor
      (when-let* ((anchor-pos (qq-chat--message-position point-anchor)))
        (setq point-anchor-offset (- (point) anchor-pos))))
    (when view (appkit-request-sync view :part 'frame))
    (qq-core-fetch-history-page
     session-key history-cursor 'newer
     (lambda (meta)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (when (and (equal qq-chat--session-key session-key)
                      (appkit-chat-history-request-end owner))
             (qq-chat--record-gateway-history-range meta 'newer)
             (let* ((bounds (qq-chat--history-batch-bounds meta))
                    (newest (cdr bounds))
                    (added (or (plist-get meta :added-count) 0))
                    (has-newer
                     (plist-get meta :history-has-newer-materialized-p)))
               (unless has-newer
                 (setq qq-chat--remote-latest-id
                       (or newest qq-chat--remote-latest-id)))
               (qq-chat--set-history-window
                (appkit-chat-history-window-first-key)
                (and has-newer (or newest window-cursor)))
               (qq-chat--request-callback-sync
                view
                (and point-anchor
                     (lambda ()
                       (when-let* ((anchor-pos
                                    (qq-chat--message-position point-anchor)))
                         (goto-char (+ anchor-pos point-anchor-offset))))))
               (unless quiet
                 (if (not has-newer)
                     (message "qq: reached materialized history tail")
                   (message "qq: loaded %d newer message%s"
                            added (if (= added 1) "" "s")))))))))
     (lambda (response reason)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (when (and (equal qq-chat--session-key session-key)
                      (appkit-chat-history-request-end owner))
             (qq-chat--request-callback-sync view)
             (qq-chat--default-error response reason)))))
     (min 100 (max 1 qq-history-fetch-count))
     owner)))

(defun qq-chat-load-newer-messages (&optional quiet)
  "Extend the current contiguous history window by one newer page."
  (interactive)
  (unless qq-chat--session-key
    (user-error "qq: this buffer is not bound to a session"))
  (let ((cursor (appkit-chat-history-window-last-key)))
    (cond
     ((not (qq-chat--history-window-known-p))
      (if (appkit-chat-history-loading-p)
          (unless quiet (message "qq: initial history is still loading"))
        (qq-chat-return-to-latest)))
     ((appkit-chat-history-loading-p)
      (unless quiet (message "qq: history load already in progress")))
     (qq-chat--gateway-history-newer-cursor
      (qq-chat--load-newer-gateway-messages quiet))
     ((not cursor)
      (unless quiet (message "qq: latest history is already loaded")))
     (t
      (unless quiet
        (message "qq: history has no newer continuation cursor"))))))

(defun qq-chat--goto-latest-window-end (&optional preferred-id)
  "Move to PREFERRED-ID in the latest window, or to its composer end."
  (let ((latest (or preferred-id
                    qq-chat--remote-latest-id
                    (qq-chat--authoritative-latest-message-id))))
    (unless (and latest (qq-chat--goto-loaded-message latest nil))
      (goto-char (or (appkit-chatbuf-input-start-position) (point-max))))))

(defun qq-chat--mark-latest-window-read (&optional message-id)
  "Mark MESSAGE-ID, or the newest visible canonical row, as read."
  (if-let* ((message (or (and message-id
                              (qq-chat--message-by-server-id message-id))
                         (qq-chat--latest-visible-read-target))))
      (qq-chat--mark-message-viewed message t)
    (user-error "qq: this chat has no canonical message to mark as read")))

(defun qq-chat--adopt-live-frontier-window
    (frontier-at-start observed-frontier)
  "Establish a minimal latest window from an in-flight live observation.

Return OBSERVED-FRONTIER when it differs from FRONTIER-AT-START and still
names a canonical cached server message.  A nil-cursor history response may
be empty because its snapshot preceded that websocket delivery; the single
live row is nevertheless a provably contiguous window attached to latest.
Older paging can extend its exact first edge normally."
  (when (and observed-frontier
             (not (equal observed-frontier frontier-at-start))
             (qq-chat--message-by-server-id observed-frontier))
    (setq qq-chat--remote-latest-id observed-frontier)
    (appkit-chat-history-older-loaded-set nil)
    (qq-chat--set-history-window observed-frontier nil)
    observed-frontier))

(defun qq-chat-return-to-latest (&optional force mark-read)
  "Rebuild the authoritative latest history window and move to its end.

With FORCE, refetch even when the current slice is already attached to live
history.  With MARK-READ, submit the exact latest string id only after that
window is available.  This follows telega's latest/read-all behavior rather
than jumping across an unfilled cached gap."
  (interactive)
  (unless qq-chat--session-key
    (user-error "qq: this buffer is not bound to a session"))
  (cond
   ((appkit-chat-history-loading-p)
    (message "qq: history load already in progress"))
   ((and (not force)
         (qq-chat--history-window-known-p)
         (not (qq-chat--history-window-partial-p)))
    (qq-chat--goto-latest-window-end)
    (when mark-read
      (qq-chat--mark-latest-window-read))
    (message "qq: latest history"))
   (t
    (qq-chat--load-initial-gateway-history
     (current-buffer) qq-chat--session-key t))))




(defun qq-chat--load-older-gateway-messages (&optional quiet)
  "Extend the current unified Gateway history toward older rows."
  (unless qq-chat--gateway-history-older-cursor
    (user-error "qq: History has no older continuation cursor"))
  (let* ((session-key qq-chat--session-key)
         (history-cursor (copy-tree qq-chat--gateway-history-older-cursor))
         (buffer (current-buffer))
         (view (qq-chat--ensure-view))
         (owner (appkit-chat-history-request-start view 'older)))
    (when view (appkit-request-sync view :part 'frame))
    (qq-core-fetch-history-page
     session-key history-cursor 'older
     (lambda (meta)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (when (and (equal qq-chat--session-key session-key)
                      (appkit-chat-history-request-end owner))
             (qq-chat--record-gateway-history-range meta 'older)
             (let* ((bounds (qq-chat--history-batch-bounds meta))
                    (oldest (car bounds))
                    (added (or (plist-get meta :added-count) 0))
                    (has-older
                     (plist-get meta :history-has-older-p)))
               (appkit-chat-history-older-loaded-set (not has-older))
               (qq-chat--set-history-window
                (or oldest (appkit-chat-history-window-first-key))
                (appkit-chat-history-window-last-key))
               (qq-chat--request-callback-sync view)
               (unless (or quiet qq-chat--pending-jump-id)
                 (if (not has-older)
                     (message "qq: no older messages")
                   (message "qq: loaded %d older message%s"
                            added (if (= added 1) "" "s")))))))))
     (lambda (response reason)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (when (and (equal qq-chat--session-key session-key)
                      (appkit-chat-history-request-end owner))
             (qq-chat--request-callback-sync view)
             (qq-chat--default-error response reason)))))
     (min 100 (max 1 qq-history-fetch-count))
     owner)))

(defun qq-chat-load-older-messages (&optional quiet)
  "Extend the current contiguous history window by one older page."
  (interactive)
  (unless qq-chat--session-key
    (user-error "qq: this buffer is not bound to a session"))
  (cond
   ((not (qq-chat--history-window-known-p))
    (unless quiet (message "qq: history window is not initialized")))
   ((appkit-chat-history-older-loaded-p)
    (unless quiet (message "qq: no older messages available")))
   ((appkit-chat-history-loading-p)
    (unless quiet (message "qq: history load already in progress")))
   ((null qq-chat--gateway-history-older-cursor)
    (appkit-chat-history-older-loaded-set t)
    (unless quiet (message "qq: no older messages available")))
   (t
    (qq-chat--load-older-gateway-messages quiet))))

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
                (appkit-request-sync
                 presentation-view :part 'frame :position t)))
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
         settled-p)
    (if (and (not (appkit-chatbuf-input-has-objects-p))
             (string-empty-p (string-trim text)))
        (message "qq: draft is empty")
      (qq-chat--push-input-history text)
      (condition-case err
          (progn
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
                 (setq settled-p t)))
             (lambda (response reason)
               (unless settled-p
                 (setq settled-p t)
                 (let ((restored
                        (qq-chat--restore-failed-send
                         buffer session-key cleared-revision draft-state
                         aux-state view)))
                   (qq-chat--default-error
                    response
                    (if restored
                        (format "%s (draft restored)"
                                (or reason "send failed"))
                      reason)))))))
        (error
         (unless settled-p
           (setq settled-p t)
           (qq-chat--restore-failed-send
            buffer session-key cleared-revision draft-state aux-state))
         (signal (car err) (cdr err)))))))

(defun qq-chat-return-dwim (arg)
  "Complete an unresolved token, send draft, or insert newline with ARG.

An unresolved @member, /face, /favorite, or :emoji: token always owns RET.
This prevents a completion frontend with no preselected row from falling
through and sending the query as plain text.  `C-c RET' remains the explicit
way to send literal token text."
  (interactive "P")
  (qq-chat--ensure-composer-visible)
  (if (not (appkit-chatbuf-point-in-input-p))
      (let ((face-id
             (or (get-text-property (point) 'qq-system-face-id)
                 (and (> (point) (point-min))
                      (get-text-property
                       (1- (point)) 'qq-system-face-id)))))
        (if face-id
            (qq-media-play-system-face face-id)
          (appkit-chatbuf-focus-input)))
    (cond
     (arg
      (insert "\n"))
     ((qq-completion-token-at-point)
      (or (qq-completion-complete)
          (message "qq: no completion candidate; C-c RET sends literally")))
     (t
      (qq-chat-send-message)))))

(defun qq-chat-edit-draft ()
  "Move point to the editable draft area."
  (interactive)
  (qq-chat--ensure-composer-visible)
  (appkit-chatbuf-focus-input))

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

(defun qq-chat-reply-to-message ()
  "Reply to the message currently under point."
  (interactive)
  (qq-chat--set-pending-reply
   (or (qq-chat--message-at-point)
       (user-error "qq: no message at point"))))

(defun qq-chat-delete-message ()
  "Persistently delete the message at point from local history only."
  (interactive)
  (let ((message (or (qq-chat--message-at-point)
                     (user-error "qq: no message at point"))))
    (unless (qq-message-delete-local-capable-p message)
      (user-error "qq: local deletion requires a canonical timeline row"))
    (when (y-or-n-p
           "Delete this message locally? The QQ server copy is unaffected. ")
      (qq-core-delete-message-local message))))

(defun qq-chat-recall-message ()
  "Recall the message currently under point from QQ."
  (interactive)
  (qq-chat--recall-message-internal
   (or (qq-chat--message-at-point)
       (user-error "qq: no message at point"))))

(defun qq-chat-react-to-message (&optional reaction message)
  "Add REACTION to MESSAGE at point.

REACTION is a normalized emoji identity.  For compatibility, a decimal string
or integer is accepted as a QQ base face id.  Interactive use offers every
reaction kind supported by Gateway: QQ base faces and scalar Unicode emoji.
Clicking an existing reaction chip performs add/remove toggle instead."
  (interactive (list (qq-completion-read-reaction)))
  (let* ((message (or message
                      (qq-chat--message-at-point)
                      (user-error "qq: no message at point")))
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

(defun qq-chat-open-avatar-at-point ()
  "Open sender avatar for the message at point."
  (interactive)
  (if-let* ((user-id (get-text-property
                      (point) 'qq-chat-gray-tip-user-id)))
      (qq-media-open-user-avatar user-id)
    (qq-media-open-message-avatar
     (or (qq-chat--message-at-point)
         (user-error "qq: no message at point")))))

(defun qq-chat-open-user-at-point ()
  "Open the sender user page for the message at point."
  (interactive)
  (let* ((message (or (qq-chat--message-at-point)
                      (user-error "qq: no message at point")))
         (gray-tip-user-id (get-text-property
                            (point) 'qq-chat-gray-tip-user-id)))
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

(defun qq-chat-cancel-dwim ()
  "Cancel reply context, or clear draft when no reply is pending.

Bound to `C-c C-k' (also ESC ESC / C-M-c).  Reply footer × is clickable."
  (interactive)
  (cond
   ((qq-chat--reply-message)
    (qq-chat--set-reply-message nil)
    (qq-chat--update-frame)
    (message "qq: reply target cleared"))
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
  "Return to authoritative latest history and mark it read."
  (interactive)
  (qq-chat-return-to-latest nil t))

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

(defun qq-chat--poke-sender-at-point ()
  "Return the QQ user ID of the message sender at point.

Only `sender-id' has this meaning.  In particular, a message's `target-id'
denotes its conversation peer (or a poke decoration target), so it must not
be used to infer the sender."
  (let* ((message (or (qq-chat--message-at-point)
                      (user-error "qq: no message at point")))
         (sender-id (alist-get 'sender-id message)))
    (unless (qq-chat--poke-target-id-p sender-id)
      (user-error "qq: message sender cannot be poked"))
    sender-id))

(defun qq-chat-poke-sender ()
  "Poke the sender of the message at point.

This works for both another user and the current user's own messages."
  (interactive)
  (qq-chat--send-poke-to
   qq-chat--session-key
   (qq-chat--poke-sender-at-point)))

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

(defvar qq-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-l") #'recenter-top-bottom)
    (define-key map (kbd "C-c g") #'qq-chat-refresh)
    (define-key map (kbd "C-c r") #'qq-chat-read-all)
    (define-key map (kbd "C-c P") #'qq-chat-send-poke)
    (define-key map (kbd "C-c m") #'qq-chat-message-transient)
    (define-key map (kbd "C-c f") #'qq-chat-forward-transient)
    (define-key map (kbd "C-c i") #'qq-chat-open-peer-info)
    ;; Keep M-</M-> as native Emacs beginning/end-of-buffer commands.
    ;; Telega reserves its authoritative latest/read-all action for M-g.
    (define-key map (kbd "RET") #'qq-chat-return-dwim)
    (define-key map (kbd "TAB") #'qq-chat-complete)
    (define-key map (kbd "<tab>") #'qq-chat-complete)
    (define-key map (kbd "C-M-i") #'qq-chat-complete)
    (define-key map (kbd "C-c '") #'qq-chat-edit-draft)
    (define-key map (kbd "M-p") #'qq-chat-draft-prev)
    (define-key map (kbd "M-n") #'qq-chat-draft-next)
    (define-key map (kbd "C-c C-f") #'qq-chat-attach-file)
    (define-key map (kbd "C-c C-v") #'qq-chat-attach-clipboard)
    (define-key map (kbd "C-c C-e") #'qq-chat-attach-emoji)
    (define-key map (kbd "C-c C-a") #'qq-chat-attach)
    (define-key map (kbd "M-g >") #'qq-chat-read-all)
    (define-key map (kbd "M-g r") #'qq-chat-read-all)
    (define-key map (kbd "M-g x") #'qq-chat-goto-pop-message)
    (define-key map (kbd "C-c RET") #'qq-chat-send-message)
    (define-key map (kbd "C-c C-k") #'qq-chat-cancel-dwim)
    ;; telega: ESC ESC / C-M-c also cancel reply-or-edit aux.
    (define-key map (kbd "\e\e") #'qq-chat-cancel-dwim)
    (define-key map (kbd "C-M-c") #'qq-chat-cancel-dwim)
    (define-key map (kbd "C-c ?") #'qq-chat-transient)
    map)
  "Keymap for `qq-chat-mode'.")

(define-derived-mode qq-chat-mode appkit-chatbuf-mode "QQ-Chat"
  "Major mode for emacs-qq chat buffers.

Message actions use point + keys (`r'/`d'/`!'/`P'/`o'/`a' on the timeline) or
`qq-chat-message-transient' (`C-c m' / timeline `m').  Chat-wide commands
are in `qq-chat-transient' (`C-c ?' / timeline `?').
Attach from clipboard with `C-c C-v' (telega-style)."
  ;; Keep vertically sliced two-line avatars visually contiguous.
  (setq-local line-spacing 0)
  (appkit-chatbuf-reset-state 32)
  (setq-local appkit-chatbuf-input-sync-function
              #'qq-chat--sync-draft-from-buffer)
  (qq-completion-setup)
  (setq-local qq-chat--callback-sync-request nil)
  (setq-local qq-chat--send-sync-request nil)
  (setq-local qq-chat--message-selection nil)
  (setq-local qq-chat--forward-request nil)
  (setq-local qq-chat--forward-request-owner nil)
  (setq-local qq-chat--forward-sync-request nil)
  (setq-local qq-chat--forward-plan-owner (list 'forward-plan-owner))
  (setq-local qq-chat--last-read-target-row-key nil)
  (setq-local appkit-media-card-fallback-context-function
              #'qq-chat--media-card-fallback-context)
  (qq-chat--reset-history-state)
  (setq-local qq-chat--scroll-observer nil)
  (setq-local qq-chat--pending-jump-id nil)
  (setq-local qq-chat--messages-pop-ring
              (make-ring (max 1 qq-chat-messages-pop-ring-size)))
  ;; telega-style: M-x yank-media also drops images into the composer.
  (when (fboundp 'yank-media-handler)
    (funcall #'yank-media-handler
             '(image/png image/jpeg image/bmp)
             (lambda (mime-type data)
               (qq-chat--yank-media mime-type data nil))))
  (add-hook 'post-command-hook #'qq-chat--post-command t t)
  (add-hook 'kill-buffer-hook #'qq-chat--cancel-initial-history-request nil t)
  (add-hook 'kill-buffer-hook #'qq-chat--cancel-forward-request nil t)
  (appkit-chatbuf-use-timeline-mode #'qq-chat-timeline-mode))

(defun qq-chat--initial-history-owner-p (buffer session-key owner)
  "Return non-nil when OWNER still names BUFFER and SESSION-KEY."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'qq-chat-mode)
              (equal qq-chat--session-key session-key)
              (eq qq-chat--initial-history-owner owner)))))

(defun qq-chat--cancel-initial-history-request ()
  "Cancel and forget this chat buffer's initial-history operation."
  (let ((owner qq-chat--initial-history-owner))
    (setq qq-chat--initial-history-owner nil)
    (when (appkit-chat-history-request-current-p owner)
      (appkit-chat-history-request-cancel))))

(defun qq-chat--fail-initial-history-load
    (buffer session-key owner view response reason)
  "Settle BUFFER's SESSION-KEY OWNER failure for VIEW.

RESPONSE and REASON describe the transport failure."
  (when (qq-chat--initial-history-owner-p buffer session-key owner)
    (with-current-buffer buffer
      (when (appkit-chat-history-request-end owner)
        (setq qq-chat--initial-history-owner nil)
        (qq-chat--request-callback-sync view)
        (qq-chat--default-error response reason)))))

(defun qq-chat--complete-initial-gateway-history
    (buffer session-key owner view frontier-at-start goto-latest-p meta)
  "Settle BUFFER's SESSION-KEY history OWNER for VIEW from META.

FRONTIER-AT-START fences live advancement.  GOTO-LATEST-P controls position."
  (when (qq-chat--initial-history-owner-p buffer session-key owner)
    (with-current-buffer buffer
      (when (appkit-chat-history-request-end owner)
        (setq qq-chat--initial-history-owner nil)
        (let* ((_range (qq-chat--record-gateway-history-range meta))
               (observed-frontier qq-chat--remote-latest-id)
               (live-advanced
                (not (equal observed-frontier frontier-at-start)))
               (bounds (qq-chat--history-batch-bounds meta))
               (oldest (car bounds))
               (newest (cdr bounds))
               (has-older
                (plist-get meta :history-has-older-p))
               (has-newer
                (plist-get meta :history-has-newer-materialized-p)))
          (cond
           (oldest
            (setq qq-chat--remote-latest-id
                  (if live-advanced observed-frontier newest))
            (qq-chat--set-history-window
             oldest (and has-newer (not live-advanced) newest))
            (appkit-chat-history-older-loaded-set (not has-older)))
           ((qq-chat--adopt-live-frontier-window
             frontier-at-start observed-frontier)
            (appkit-chat-history-older-loaded-set (not has-older)))
           (t
            (setq qq-chat--remote-latest-id nil)
            (qq-chat--set-empty-history-window)
            (appkit-chat-history-older-loaded-set (not has-older))))
          (qq-chat--request-callback-sync
           view
           (when goto-latest-p
             (lambda ()
               (qq-chat--goto-latest-window-end
                qq-chat--remote-latest-id)))))))))

(defun qq-chat--load-initial-gateway-history
    (buffer session-key &optional goto-latest-p)
  "Load native latest history for BUFFER and exact SESSION-KEY.

When GOTO-LATEST-P is non-nil, move to the attached live edge after the
accepted Appkit projection."
  (let* ((latest-message
          (with-current-buffer buffer
            (qq-chat--latest-server-message)))
         (remote-latest-id (alist-get 'server-id latest-message))
         (view (with-current-buffer buffer (qq-chat--ensure-view)))
         owner)
    (with-current-buffer buffer
      (qq-chat--cancel-initial-history-request)
      (appkit-chat-history-window-clear)
      (setq qq-chat--remote-latest-id remote-latest-id
            qq-chat--gateway-history-older-cursor nil
            qq-chat--gateway-history-newer-cursor nil)
      (setq owner (appkit-chat-history-request-start view 'initial)
            qq-chat--initial-history-owner owner)
      (qq-chat--sync-timeline :messages nil)
      (qq-chat--update-frame))
    (condition-case error-data
        (qq-core-fetch-history-page
         session-key nil 'older
         (lambda (meta)
           (qq-chat--complete-initial-gateway-history
            buffer session-key owner view remote-latest-id goto-latest-p meta))
         (lambda (response reason)
           (qq-chat--fail-initial-history-load
            buffer session-key owner view response reason))
         (min 100 (max 1 qq-history-fetch-count))
         owner)
      ((error quit)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (when (eq qq-chat--initial-history-owner owner)
             (setq qq-chat--initial-history-owner nil)
             (appkit-chat-history-request-cancel)
             (qq-chat--request-callback-sync view))))
       (signal (car error-data) (cdr error-data))))))






(defun qq-chat--load-initial-history (buffer session-key)
  "Load the native initial position for SESSION-KEY."
  (qq-chat--load-initial-gateway-history buffer session-key))

(defun qq-chat--open-buffer (session-key)
  "Create or reuse SESSION-KEY's chat view without loading history."
  (unless session-key
    (user-error "qq: session key is required"))
  (let ((owner (or (qq-runtime-current-account-id)
                   (user-error "qq: Select a QQ account first"))))
    (qq-runtime-with-account owner
      (qq-state-upsert-session session-key nil nil)
      (let* ((view
              (qq-runtime-open-account-view
               :account-id owner
               :id (list 'chat session-key)
               :mode 'qq-chat-mode
               :buffer-name (qq-chat--buffer-name session-key)
               :state session-key
               :sync-function #'qq-chat--sync-invalidations
               :parts '(frame timeline composer geometry)))
             (buffer (appkit-view-buffer view)))
        (with-current-buffer buffer
          (let ((fresh-p (null qq-chat--session-key)))
            (setq qq-chat--session-key session-key)
            (qq-chat--install-scroll-observer view)
            (qq-completion-preload-members)
            (if fresh-p
                (progn
                  ;; A fresh buffer has no proven contiguous window yet.  Do not
                  ;; render every cache island while the initial/around request is
                  ;; still choosing its exact slice.
                  (appkit-chat-history-window-clear)
                  (qq-chat--ensure-view)
                  (qq-chat--header-line-update)
                  (qq-chat--sync-timeline :messages nil)
                  (qq-chat--update-frame))
              (qq-chat-render)))
          (appkit-chatbuf-focus-input))
        buffer))))

(defun qq-chat-open (session-key)
  "Open chat for SESSION-KEY and load its official initial position."
  (interactive)
  (let ((owner (or (qq-runtime-current-account-id)
                   (user-error "qq: Select a QQ account first"))))
    (qq-runtime-with-account owner
      (let ((buffer (qq-chat--open-buffer session-key)))
        (with-current-buffer buffer
          (qq-chat--load-initial-history buffer session-key))
        (pop-to-buffer buffer)
        (with-current-buffer buffer
          (appkit-view-refresh-responsive-geometry))))))





(defun qq-chat--composer-preview-media-key-p (media-key)
  "Return non-nil when MEDIA-KEY affects the active reply preview."
  (and (stringp media-key)
       (member media-key
               (qq-media-message-one-line-preview-keys
                (qq-chat--reply-message)))))

(defun qq-chat--rerender-open-chats (&optional media-key)
  "Invalidate open chat rows and composer destinations affected by MEDIA-KEY."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when-let* ((view (qq-chat--live-current-view)))
        (let ((prompt-key
               (and qq-runtime--account-id
                    (qq-runtime-with-account qq-runtime--account-id
                      (qq-chat--prompt-avatar-cache-key)))))
          (if media-key
              (appkit-request-sync
               view
               :part
               (and (or (equal media-key prompt-key)
                        (qq-chat--composer-preview-media-key-p media-key))
                    'composer)
               :resource (list :media media-key))
            (when (or (appkit-chat-timeline-live-p)
                      (appkit-chatbuf-prompt-button-live-p))
              (appkit-request-sync
               view
               :part (and prompt-key 'composer)
               :entries
               (and (appkit-chat-timeline-live-p)
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

(defun qq-chat--message-event-advances-frontier-p (event)
  "Return non-nil when EVENT introduces a canonical server message.

Ordinary updates such as recall and emoji-like notices must never move the
remote frontier.  A create is a new remote row once it has a server id; an
update only qualifies when it explicitly promotes the message's own pending
local anchor to that server id."
  (let* ((message (plist-get event :message))
         (mutation (plist-get event :mutation))
         (source (plist-get event :source))
         (event-anchor (plist-get event :message-anchor))
         (previous-anchor (plist-get event :previous-anchor))
         (local-id (and (listp message) (alist-get 'local-id message)))
         (server-id (and (listp message) (alist-get 'server-id message))))
    (and (eq (plist-get event :type) 'message)
         (equal (plist-get event :session-key) qq-chat--session-key)
         (qq-protocol-message-id-p server-id)
         (equal event-anchor server-id)
         (or (and (eq mutation 'create)
                  (memq source '(event notice)))
             (and (eq mutation 'update)
                  (memq source '(event notice response))
                  previous-anchor
                  local-id
                  (equal previous-anchor local-id)
                  (not (equal previous-anchor server-id)))))))

(defun qq-chat--observe-message-frontier (event)
  "Advance this buffer's remote frontier from canonical message EVENT.

This is deliberately state-only and idempotent.  The state-change hook calls
it before scheduling AppKit invalidations, so an HTTP history callback in the
same event-loop turn cannot overwrite a websocket message merely because
redisplay has not processed the queued event yet."
  (let* ((message (plist-get event :message))
         (anchor (or (plist-get event :message-anchor)
                     (and (listp message)
                          (qq-chat--message-anchor message)))))
    (when (and (qq-chat--message-event-advances-frontier-p event)
               (qq-protocol-message-id-p anchor)
               (equal anchor
                      (alist-get 'server-id
                                 (qq-chat--latest-server-message))))
      (appkit-chat-history-window-seed-live anchor)
      (unless (equal anchor qq-chat--remote-latest-id)
        (appkit-chat-history-newer-stalled-clear))
      (setq qq-chat--remote-latest-id anchor))))

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
    ;; Idempotent with the eager observation in `qq-chat--handle-state-change'.
    (qq-chat--observe-message-frontier event)
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
  "Queue state EVENT invalidations for open QQ chats."
  (let ((event-session-key (plist-get event :session-key))
        (event-type (plist-get event :type))
        (event-owner (plist-get event :account-id)))
    (when (eq event-type 'reset)
      ;; The remembered destination belongs to the previous runtime/account,
      ;; not to any particular chat buffer.
      (setq qq-chat--last-forward-target-key nil
            qq-chat-forward-target-history nil))
    (when (memq event-type '(message history reset session action connection
                             sessions-refreshed friends-refreshed groups-refreshed))
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
              (qq-chat--cancel-forward-request))
            ;; Only UI projection requires a live view.  Ownership cleanup
            ;; above deliberately does not.
            (when (or (memq event-type
                            '(reset connection sessions-refreshed
                              friends-refreshed groups-refreshed))
                      (equal event-session-key qq-chat--session-key))
              (when-let* ((view (qq-chat--live-current-view)))
                (when (eq event-type 'message)
                  (qq-chat--observe-message-frontier event))
                (appkit-view-enqueue-event view event)
                (appkit-request-sync
                 view :part (if (memq event-type
                                      '(session action connection
                                        sessions-refreshed friends-refreshed
                                        groups-refreshed))
                                'frame
                              'timeline))))))))))

(add-hook 'qq-media-cache-update-hook #'qq-chat--rerender-open-chats)
(add-hook 'qq-state-change-hook #'qq-chat--handle-state-change)

(provide 'qq-chat)

;;; qq-chat.el ends here
