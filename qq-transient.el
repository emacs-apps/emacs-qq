;;; qq-transient.el --- Transient menus for emacs-qq -*- lexical-binding: t; -*-

;; Author: 0WD0 <me@0wd0.com>

;;; Commentary:

;; Object and operation menus for QQ sessions, messages, media and input.
;; `qq-chat-transient' routes input point to the composer menu; the timeline,
;; root and account menus lead to focused operation prefixes.
;;
;; These replace discoverability that used to live in always-visible
;; action-button rows.  Single-key timeline bindings remain for power use.

;;; Code:

(require 'transient)
(require 'appkit-media)
(require 'qq-core)
(require 'qq-chat)
(require 'qq-protocol)
(require 'qq-presence)
(require 'qq-root)
(require 'qq-search)
(require 'qq-search-navigation)
(require 'qq-state)

(declare-function qq-connect "qq")
(declare-function qq-disconnect "qq")
(declare-function qq-reset-session-state "qq")


(defvar qq-transient--presentation-message :inactive
  "Message cached only inside one native prefix presentation pass.")

(defun qq-transient--message-environment (function)
  "Call FUNCTION with a fresh, lazy message cache for prefix presentation.
Resolve only after Transient has initialized the new prefix's scope."
  (let ((qq-transient--presentation-message :unresolved))
    (funcall function)))

(defclass qq-transient-message-prefix (transient-prefix)
  ((environment :initform #'qq-transient--message-environment))
  "Prefixes operating on captured QQ message targets, not a later point.")

(defun qq-transient--capture-selection (&optional allow-empty)
  "Inherit captured targets, or snapshot the message region, marks, or point.
ALLOW-EMPTY is for the chat menu, whose navigation needs no message target."
  (or (transient-scope nil 'qq-transient-message-prefix)
      (progn
        (qq-transient--require-chat)
        (let* ((view (qq-chat--live-current-view))
               (messages (qq-chat--operation-messages))
               (anchors (mapcar #'qq-chat--message-anchor messages)))
          (unless (and view (or messages allow-empty))
            (user-error "qq: select messages in a live chat first"))
          (list
           :surface view :anchors anchors
           :forward-plan
           (when (and messages
                      (qq-chat--forward-source-supported-p)
                      (seq-every-p #'qq-chat--message-forwardable-p messages))
             (qq-chat--current-forward-plan messages))
           :media
           (when (and (= (length anchors) 1)
                      (equal (car anchors)
                             (qq-chat--message-anchor
                              (qq-chat--message-at-point))))
             (appkit-media-card-context-at-point)))))))

(defun qq-transient--call-with-message (target function)
  "Call FUNCTION with TARGET's current canonical message in its own account."
  (let* ((view (plist-get target :surface))
         (anchors (plist-get target :anchors))
         (_single (unless (= (length anchors) 1)
                    (user-error "qq: this action requires exactly one message")))
         (message (qq-chat--message-in-view view (car anchors))))
    (with-current-buffer (appkit-surface-buffer view)
      (qq-runtime-with-account qq-runtime--account-id
        (funcall function message)))))

(defun qq-transient--media-context (target message)
  "Resolve TARGET's exact media in current MESSAGE, without selecting another."
  (let* ((context (plist-get target :media))
         (segment (plist-get context :payload)))
    (when (and segment
               (not (qq-state-message-recalled-p message))
               (member segment (alist-get 'segments message)))
      (qq-chat--segment-media-card-context segment))))

(defun qq-transient--message-command (function &rest arguments)
  "Invoke FUNCTION with the prefix's canonical message before ARGUMENTS."
  (qq-transient--call-with-message
   (transient-scope nil 'qq-transient-message-prefix)
   (lambda (message) (apply function message arguments))))

(defun qq-transient--forward-plan-for-interactive ()
  "Use the menu's captured forward plan, or capture direct-command targets."
  (if-let* ((target (transient-scope nil 'qq-transient-message-prefix)))
      (or (plist-get target :forward-plan)
          (user-error "qq: the captured messages cannot be forwarded"))
    (qq-chat--current-forward-plan)))

(defun qq-transient--media-command (function &rest arguments)
  "Invoke FUNCTION on the captured media card, never another card at point."
  (let ((target (transient-scope nil 'qq-transient-message-prefix)))
    (qq-transient--call-with-message
     target
     (lambda (message)
       (let ((context (qq-transient--media-context target message)))
         (unless context
           (user-error "qq: the captured media is no longer available"))
         (apply function context arguments))))))

(transient-define-suffix qq-transient-media-save (target)
  "Save the captured media card."
  (interactive (list (transient-scope nil 'qq-transient-message-prefix)))
  (qq-transient--call-with-message
   target
   (lambda (message)
     (let* ((context (qq-transient--media-context target message))
            (_ (unless (and context
                            (null (appkit-media-card-action-inapt-reason
                                   'save-as context)))
                 (user-error "qq: the captured media cannot be saved")))
            (name (qq-media-segment-default-save-name
                   (plist-get context :payload)))
            (path (read-file-name "Save media as: " nil name nil name)))
       (qq-transient--call-with-message
        target
        (lambda (current)
          (let ((context (qq-transient--media-context target current)))
            (unless context
              (user-error "qq: the captured media is no longer available"))
            (qq-media-segment-save-as (plist-get context :payload) path))))))))

;;; Availability helpers

(defun qq-transient--require-chat ()
  "Require a QQ chat buffer with a conversation."
  (unless (and (derived-mode-p 'qq-chat-mode) qq-chat--session-key)
    (user-error "qq: open a chat first")))

(defun qq-transient--require-root ()
  "Require the QQ session directory."
  (unless (derived-mode-p 'qq-root-mode)
    (user-error "qq: open the session directory first")))

(defun qq-transient--message ()
  "Resolve the captured message, sharing work only during menu presentation."
  (if (memq qq-transient--presentation-message '(:inactive :unresolved))
      (let ((message
             (if-let* ((target (transient-scope))
                       ((listp target))
                       ((plist-get target :surface)))
                 (when (= (length (plist-get target :anchors)) 1)
                   (condition-case nil
                       (qq-chat--message-in-view
                        (plist-get target :surface)
                        (car (plist-get target :anchors)))
                     (user-error nil)))
               (ignore-errors (qq-chat--message-at-point)))))
        (when (eq qq-transient--presentation-message :unresolved)
          (setq qq-transient--presentation-message message))
        message)
    qq-transient--presentation-message))

(defun qq-transient--no-message-at-point-p ()
  "Return non-nil when there is no message under point."
  (null (qq-transient--message)))

(defun qq-transient--no-operation-targets-p ()
  "Return non-nil when the captured or current operation has no messages."
  (if-let* ((target (transient-scope nil 'qq-transient-message-prefix)))
      (null (plist-get target :anchors))
    (null (qq-chat--operation-target-keys))))

(defun qq-transient--poke-session-inapt-p ()
  "Return non-nil when the current conversation cannot send pokes."
  (condition-case nil
      (progn
        (qq-chat--poke-session qq-chat--session-key)
        nil)
    (user-error t)))

(defun qq-transient--poke-sender-inapt-p ()
  "Return non-nil when the message sender at point cannot be poked."
  (or (qq-transient--poke-session-inapt-p)
      (condition-case nil
          (progn
            (qq-chat--poke-sender-at-point (qq-transient--message))
            nil)
        (user-error t))))

(defun qq-transient--reply-inapt-p ()
  "Return non-nil when reply is unavailable for the message at point."
  (let ((message (qq-transient--message)))
    (or (null message)
        (null (qq-message-reply-target qq-chat--session-key message))
        (qq-state-message-recalled-p message))))

(defun qq-transient--goto-reply-inapt-p ()
  "Return non-nil when the message at point has no reply target to jump to."
  (let ((message (qq-transient--message)))
    (or (null message)
        (null (qq-chat--message-reply-locator message)))))

(defun qq-transient--pop-ring-empty-p ()
  "Return non-nil when the messages pop ring is empty."
  (or (not (boundp 'qq-chat--messages-pop-ring))
      (null qq-chat--messages-pop-ring)
      (not (ring-p qq-chat--messages-pop-ring))
      (ring-empty-p qq-chat--messages-pop-ring)))

(defun qq-transient--delete-local-inapt-p ()
  "Return non-nil when local deletion is unavailable at point."
  (let ((message (qq-transient--message)))
    (or (null message)
        (not (qq-message-delete-local-capable-p message)))))

(defun qq-transient--recall-inapt-p ()
  "Return non-nil when recall is unavailable for the message at point."
  (let* ((message (qq-transient--message))
         (poke-p (and message (qq-state-poke-message-p message))))
    (or (null message)
        (if poke-p
            (not (qq-message-poke-recall-capable-p message))
          (not (qq-message-recall-capable-p message))))))

(defun qq-transient--forward-inapt-p ()
  "Return non-nil when forwarding the message at point is unavailable."
  (or (not (qq-chat--forward-source-supported-p))
      (not (qq-chat--message-forwardable-p
            (qq-chat--message-at-point)))))

(defun qq-transient--reaction-inapt-p ()
  "Return non-nil when reacting to the message at point is unavailable."
  (not (qq-chat--message-reactable-p
        (qq-transient--message))))

(defun qq-transient--translation-inapt-p ()
  "Return non-nil when the message at point has no loaded prose."
  (not (qq-chat--message-translatable-p
        (qq-transient--message))))

(defun qq-transient--essence-inapt-p ()
  "Return non-nil when toggling essence at point is unavailable."
  (not (qq-chat--message-essence-capable-p
        (qq-transient--message))))

(defun qq-transient--todo-inapt-p ()
  "Return non-nil when mutating todo at point is unavailable."
  (not (qq-chat--message-todo-capable-p
        (qq-transient--message))))

(defun qq-transient--no-message-selection-p ()
  "Return non-nil when there are no selected message memberships to clear."
  (null qq-chat--message-selection))

(defun qq-transient--forward-selection-inapt-p ()
  "Return non-nil when the entire captured or current selection cannot forward."
  (or qq-chat--forward-request-owner
      (not (qq-chat--forward-source-supported-p))
      (condition-case nil
          (if-let* ((target (transient-scope nil 'qq-transient-message-prefix)))
              (let ((plan (plist-get target :forward-plan)))
                (or (null plan)
                    (not (qq-chat--forward-plan-messages plan))))
            (let ((messages (qq-chat--operation-messages)))
              (not (and messages
                        (seq-every-p #'qq-chat--message-forwardable-p messages)))))
        (user-error t))))

(defun qq-transient--media-action-inapt-p (action)
  "Whether ACTION is unavailable for the exact scoped media card."
  (let ((context (qq-transient--media-context
                  (transient-scope) (qq-transient--message))))
    (or (null context)
        (appkit-media-card-action-inapt-reason action context))))

(defun qq-transient--favorite-image-inapt-p ()
  "Whether the captured media lacks an original image that QQ can collect."
  (let ((context (qq-transient--media-context
                  (transient-scope) (qq-transient--message))))
    (or (not (qq-rpc-method-available-p "favorite_emoji.add"))
        (not (qq-rpc-method-available-p "media.materialize"))
        (not (qq-media-favorite-image-p (plist-get context :payload))))))

(defun qq-transient--avatar-inapt-p ()
  "Return non-nil when avatar open is unavailable at point."
  (let ((message (qq-transient--message)))
    (or (null message)
        (null (alist-get 'sender-id message)))))

(defun qq-transient--user-inapt-p ()
  "Return non-nil when the message sender has no user page."
  (let* ((message (qq-transient--message))
         (user-id (and message (alist-get 'sender-id message))))
    (not (and (qq-protocol-user-uin-p user-id) (not (equal user-id "0"))))))

(defun qq-transient--peer-user-inapt-p ()
  "Return non-nil when the current chat has no private peer user page."
  (let* ((session (and (boundp 'qq-chat--session-key)
                       (qq-state-session qq-chat--session-key)))
         (user-id (and session
                       (or (alist-get 'peer-uin session)
                           (alist-get 'target-id session)))))
    (not (and (eq (alist-get 'type session) 'private)
              (qq-protocol-user-uin-p user-id)))))

(defun qq-transient--friend-pin-inapt-p ()
  "Return non-nil when the current chat is not an authoritative friend chat."
  (not (qq-chat--friend-pin-capable-p)))

(defun qq-transient--presence-inapt-p ()
  "Return non-nil when account presence control is unavailable."
  (not (qq-core-presence-capable-p)))

(defun qq-transient--chat-info-inapt-p ()
  "Return non-nil when the current chat has no profile page."
  (let* ((session (and (boundp 'qq-chat--session-key)
                       (qq-state-session qq-chat--session-key)))
         (type (and session (alist-get 'type session)))
         (target-id (and session
                         (or (and (eq type 'private)
                                  (alist-get 'peer-uin session))
                             (alist-get 'target-id session)))))
    (pcase type
      ('private (not (qq-protocol-user-uin-p target-id)))
      ('group (not (qq-protocol-group-uin-p target-id)))
      (_ t))))

(defun qq-transient--no-reply-context-p ()
  "Return non-nil when composer has no pending reply."
  (null (qq-chat--reply-message)))

(defun qq-transient--cancel-inapt-p ()
  "Return non-nil when cancel-dwim has nothing to clear."
  (and (qq-transient--no-reply-context-p)
       (let ((draft (ignore-errors (qq-chat--current-draft-string))))
         (or (null draft) (string-empty-p (string-trim draft))))))

(defun qq-transient--no-session-at-point-p ()
  "Return non-nil when root point is not on a session row."
  (null (ignore-errors (qq-root--session-key-at-point))))

(defun qq-transient--root-info-inapt-p ()
  "Return non-nil when the root session has no profile page."
  (let* ((session (ignore-errors (qq-root--session-at-point)))
         (type (and session (alist-get 'type session)))
         (target-id (and session
                         (or (and (eq type 'private)
                                  (alist-get 'peer-uin session))
                             (alist-get 'target-id session)))))
    (pcase type
      ('private (not (qq-protocol-user-uin-p target-id)))
      ('group (not (qq-protocol-group-uin-p target-id)))
      (_ t))))


;;; Chat / message transients
;;
;; Magit-style autoloads: do NOT put bare `;;;###autoload' above
;; `transient-define-prefix'.  loaddefs would copy the whole form into
;; *-autoloads.el, where `transient-define-prefix' is still undefined.
;; Use an explicit (autoload SYMBOL FILE nil t) cookie instead; the real
;; definition runs only after this file loads and (require 'transient).

;;;###autoload(autoload 'qq-chat-message-todo-transient "qq" nil t)
(transient-define-prefix qq-chat-message-todo-transient (&optional target)
  "Todo actions for one captured QQ group message."
  :class qq-transient-message-prefix
  [[:description "Todo"
    :advice qq-transient--message-command
    :inapt-if qq-transient--todo-inapt-p
    ("s" "Set" qq-chat-set-message-todo)
    ("c" "Complete" qq-chat-complete-message-todo)
    ("x" "Cancel" qq-chat-cancel-message-todo)]]
  (interactive)
  (transient-setup 'qq-chat-message-todo-transient nil nil
                   :scope (or target (qq-transient--capture-selection))))

;;;###autoload(autoload 'qq-chat-friend-pin-transient "qq" nil t)
(transient-define-prefix qq-chat-friend-pin-transient ()
  "Explicit pin actions for the current QQ friend conversation."
  [["Friend conversation"
    ("p" "Pin" qq-chat-pin-friend
     :inapt-if qq-transient--friend-pin-inapt-p)
    ("u" "Unpin" qq-chat-unpin-friend
     :inapt-if qq-transient--friend-pin-inapt-p)]]
  (interactive)
  (qq-transient--require-chat)
  (transient-setup 'qq-chat-friend-pin-transient))

;;;###autoload(autoload 'qq-chat-delete-transient "qq" nil t)
(transient-define-prefix qq-chat-delete-transient (&optional target)
  "Choose local deletion or QQ recall for one captured message."
  :class qq-transient-message-prefix
  [[:description "Delete"
    :advice qq-transient--message-command
    ("d" "Delete locally" qq-chat-delete-message
     :inapt-if qq-transient--delete-local-inapt-p)
    ("r" "Recall from QQ" qq-chat-recall-message
     :inapt-if qq-transient--recall-inapt-p)]]
  (interactive)
  (transient-setup 'qq-chat-delete-transient nil nil
                   :scope (or target (qq-transient--capture-selection))))

;;;###autoload(autoload 'qq-transient-msg-operate "qq" nil t)
(transient-define-prefix qq-transient-msg-operate (&optional target)
  "Operate on captured message targets, using the selection for batch actions."
  :class qq-transient-message-prefix
  [[:description
    (lambda ()
      (let ((count (length (plist-get (transient-scope) :anchors))))
        (if (= count 1) "Message" (format "Selection (%d messages)" count))))
    ("f" "Forward…" qq-chat-forward-transient
     :inapt-if qq-transient--forward-selection-inapt-p)
    ("r" "Reply" qq-chat-reply-to-message
     :advice qq-transient--message-command
     :inapt-if qq-transient--reply-inapt-p)
    ("t" "Translate" qq-chat-translate-message
     :advice qq-transient--message-command
     :inapt-if qq-transient--translation-inapt-p)]
   ["Group message"
    :if (lambda ()
          (not (and (qq-transient--reaction-inapt-p)
                    (qq-transient--essence-inapt-p)
                    (qq-transient--todo-inapt-p))))
    ("!" "React…" qq-chat-react-to-message
     :advice qq-transient--message-command
     :inapt-if qq-transient--reaction-inapt-p)
    ("e" "Toggle essence" qq-chat-toggle-message-essence
     :advice qq-transient--message-command
     :inapt-if qq-transient--essence-inapt-p)
    ("T" "Group todo…" qq-chat-message-todo-transient
     :inapt-if qq-transient--todo-inapt-p)]
   ["Inspect"
    ("g" "Goto reply target" qq-chat-goto-reply
     :advice qq-transient--message-command
     :inapt-if qq-transient--goto-reply-inapt-p)
    ("i" "Sender…" qq-chat-sender-transient
     :inapt-if qq-transient--user-inapt-p)
    ("o" "Media…" qq-chat-media-transient
     :if (lambda ()
           (qq-transient--media-context
            (transient-scope) (qq-transient--message))))]
   [:description "Remove"
    :advice qq-transient--message-command
    ("d" "Delete locally" qq-chat-delete-message
     :inapt-if qq-transient--delete-local-inapt-p)
    ("D" "Recall from QQ" qq-chat-recall-message
     :inapt-if qq-transient--recall-inapt-p)]]
  (interactive)
  (transient-setup 'qq-transient-msg-operate nil nil
                   :scope (or target (qq-transient--capture-selection))))

;;;###autoload(autoload 'qq-chat-sender-transient "qq" nil t)
(transient-define-prefix qq-chat-sender-transient (&optional target)
  "Actions on the captured QQ message's sender."
  :class qq-transient-message-prefix
  [[:description "Sender"
    :advice qq-transient--message-command
    ("P" "Poke" qq-chat-poke-sender
     :inapt-if qq-transient--poke-sender-inapt-p)
    ("a" "Open avatar" qq-chat-open-avatar-at-point
     :inapt-if qq-transient--avatar-inapt-p)
    ("i" "User page" qq-chat-open-user-at-point
     :inapt-if qq-transient--user-inapt-p)]]
  (interactive)
  (transient-setup 'qq-chat-sender-transient nil nil
                   :scope (or target (qq-transient--capture-selection))))

;;;###autoload(autoload 'qq-chat-media-transient "qq" nil t)
(transient-define-prefix qq-chat-media-transient (&optional target)
  "Operate on the exact media card captured in one message."
  :class qq-transient-message-prefix
  [["Media"
    ("o" "Open / play" appkit-media-card-open
     :advice qq-transient--media-command
     :inapt-if (lambda () (qq-transient--media-action-inapt-p 'open)))
    ("D" "Download / retry" appkit-media-card-download
     :advice qq-transient--media-command
     :inapt-if (lambda () (qq-transient--media-action-inapt-p 'download)))
    ("s" "Save as" qq-transient-media-save
     :inapt-if (lambda () (qq-transient--media-action-inapt-p 'save-as)))
    ("f" "Add to QQ favorites" qq-chat-save-image-as-favorite
     :advice qq-transient--media-command
     :inapt-if qq-transient--favorite-image-inapt-p)
    ("y" "Copy media URL" appkit-media-card-copy-url
     :advice qq-transient--media-command
     :inapt-if (lambda () (qq-transient--media-action-inapt-p 'copy-url)))]]
  (interactive)
  (setq target (or target (qq-transient--capture-selection)))
  (qq-transient--call-with-message
   target
   (lambda (message)
     (unless (qq-transient--media-context target message)
       (user-error "qq: the captured message has no media card"))))
  (transient-setup 'qq-chat-media-transient nil nil :scope target))

(defun qq-transient--forward-plan-scope ()
  "Return the immutable plan exported by the forwarding prefix."
  (let ((plan (transient-scope 'qq-chat-forward-transient)))
    (unless (qq-chat-forward-plan-p plan)
      (user-error "qq: forwarding action requires an active forwarding menu"))
    plan))

(transient-define-suffix qq-transient-forward-individually (plan)
  "Forward PLAN as separate ordinary messages."
  :transient nil
  (interactive (list (qq-transient--forward-plan-scope)))
  (qq-chat-forward-individually plan))

(transient-define-suffix qq-transient-forward-merged (plan)
  "Forward PLAN as one native merged-forward card."
  :transient nil
  (interactive (list (qq-transient--forward-plan-scope)))
  (qq-chat-forward-merged plan))

;;;###autoload(autoload 'qq-chat-forward-transient "qq" nil t)
(transient-define-prefix qq-chat-forward-transient (plan)
  "Choose explicit QQ forwarding semantics for one immutable PLAN."
  [[:description
    (lambda ()
      (format "转发 %d 条消息"
              (length
               (qq-chat-forward-plan-anchors
                (oref (transient-prefix-object) scope)))))
    ("i" "逐条转发…" qq-transient-forward-individually)
    ("m" "合并转发…" qq-transient-forward-merged)]]
  (interactive (list (qq-transient--forward-plan-for-interactive)))
  (unless (qq-chat-forward-plan-p plan)
    (user-error "qq: invalid forwarding plan"))
  (unless (qq-chat--forward-source-supported-p
           nil (qq-chat-forward-plan-session-key plan))
    (user-error "qq: forwarding is unavailable from this session"))
  (qq-chat--forward-plan-messages plan)
  (transient-setup 'qq-chat-forward-transient nil nil :scope plan))

;;;###autoload(autoload 'qq-chat-transient "qq" nil t)
(transient-define-prefix qq-chat-transient ()
  "Open timeline operations, or composer operations when point is in input."
  :class qq-transient-message-prefix
  [["Timeline"
    ("g" "Refresh" qq-chat-refresh)
    (">" "Latest / mark read" qq-chat-read-all)
    ("o" "Message / selection…" qq-transient-msg-operate
     :inapt-if qq-transient--no-operation-targets-p)
    ("n" "Next message" qq-chat-next-message)
    ("p" "Previous message" qq-chat-previous-message)
    ("x" "Pop jump" qq-chat-goto-pop-message
     :inapt-if qq-transient--pop-ring-empty-p)]
   ["Search"
    ("C-c C-r" "Search older messages…" qq-chat-search)
    ("C-c C-s" "Search newer messages…" qq-chat-search-forward)
    ("M-g p" "Previous match" qq-chat-search-previous)
    ("M-g n" "Next match" qq-chat-search-next)
    ("C-c C-k" "Cancel search / reply / draft" qq-chat-cancel-dwim)
    ("C-c /" "Search results…" qq-search-chat)]
   ["Selection"
    ("m" "Select / unselect at point" qq-chat-toggle-message-selection
     :inapt-if qq-transient--forward-inapt-p)
    ("U" "Clear message selection" qq-chat-clear-message-selection
     :inapt-if qq-transient--no-message-selection-p)
    ("f" "Forward…" qq-chat-forward-transient
     :inapt-if qq-transient--forward-selection-inapt-p)]
   ["Conversation"
    ("c" "Compose…" qq-chat-composer-transient)
    ("F" "Favorite emojis…" qq-chat-favorite-emoji-transient)
    ("s" "Conversation details…" qq-chat-session-transient)
    ("q" "Quit window" quit-window)
    ("?" "Describe mode" describe-mode)]]
  (interactive)
  (qq-transient--require-chat)
  (if (appkit-chatbuf-point-in-input-p)
      (qq-chat-composer-transient)
    (transient-setup 'qq-chat-transient nil nil
                     :scope (qq-transient--capture-selection t))))

;;;###autoload(autoload 'qq-chat-composer-transient "qq" nil t)
(transient-define-prefix qq-chat-composer-transient ()
  "Compose a message in the current QQ conversation."
  [["Composer"
    ("c" "Send" qq-chat-send-message)
    ("a" "Attach…" qq-chat-attach-transient)
    ("k" "Cancel reply/draft" qq-chat-cancel-dwim
     :inapt-if qq-transient--cancel-inapt-p)
    ("e" "Focus draft" qq-chat-edit-draft)]]
  (interactive)
  (qq-transient--require-chat)
  (qq-chat-edit-draft)
  (transient-setup 'qq-chat-composer-transient))

;;;###autoload(autoload 'qq-chat-attach-transient "qq" nil t)
(transient-define-prefix qq-chat-attach-transient ()
  "Choose an attachment for the QQ composer."
  [["Attach"
    ("a" "File…" qq-chat-attach)
    ("E" "QQ small face" qq-chat-attach-face)
    ("L" "QQ large face" qq-chat-attach-large-face)
    ("F" "Favorite face" qq-chat-attach-custom-face)
    ("m" "Manage favorites…" qq-chat-favorite-emoji-transient)]]
  (interactive)
  (qq-transient--require-chat)
  (qq-chat-edit-draft)
  (transient-setup 'qq-chat-attach-transient))

;;;###autoload(autoload 'qq-chat-favorite-emoji-transient "qq" nil t)
(transient-define-prefix qq-chat-favorite-emoji-transient ()
  "Manage this account's QQ favorite emoji catalog."
  [["Favorites"
    ("f" "Insert favorite" qq-chat-attach-custom-face)
    ("a" "Add local image…" qq-chat-add-favorite-emoji
     :inapt-if (lambda ()
                 (not (and (qq-rpc-method-available-p "favorite_emoji.add")
                           (qq-rpc-method-available-p "resource.stage_local")))))
    ("d" "Delete…" qq-chat-delete-favorite-emoji
     :inapt-if (lambda ()
                 (not (qq-rpc-method-available-p "favorite_emoji.delete"))))]
   ["Organize"
    ("e" "Edit annotation…" qq-chat-describe-favorite-emoji
     :inapt-if (lambda ()
                 (not (qq-rpc-method-available-p "favorite_emoji.set_description"))))
    ("m" "Move to position…" qq-chat-move-favorite-emoji
     :inapt-if (lambda ()
                 (not (qq-rpc-method-available-p "favorite_emoji.move"))))
    ("g" "Refresh from QQ" qq-chat-refresh-favorite-emojis)]]
  (interactive)
  (qq-transient--require-chat)
  (transient-setup 'qq-chat-favorite-emoji-transient))

;;;###autoload(autoload 'qq-chat-session-transient "qq" nil t)
(transient-define-prefix qq-chat-session-transient ()
  "Inspect and manage the current QQ conversation."
  [["Conversation"
    ("P" "Poke user…" qq-chat-send-poke
     :inapt-if qq-transient--poke-session-inapt-p)
    ("t" "Friend pin…" qq-chat-friend-pin-transient
     :inapt-if qq-transient--friend-pin-inapt-p)
    ("h" "Chat info" qq-chat-open-peer-info
     :inapt-if qq-transient--chat-info-inapt-p)
    ("i" "User page" qq-chat-open-peer-user
     :inapt-if qq-transient--peer-user-inapt-p)]]
  (interactive)
  (qq-transient--require-chat)
  (transient-setup 'qq-chat-session-transient))


;;; Root transient

;;;###autoload(autoload 'qq-presence-transient "qq" nil t)
(transient-define-prefix qq-presence-transient ()
  "Set presence for the selected QQ account."
  [["Standard"
    ("o" "Online" qq-presence-online
     :inapt-if qq-transient--presence-inapt-p)
    ("q" "Q me" qq-presence-q-me
     :inapt-if qq-transient--presence-inapt-p)
    ("a" "Away" qq-presence-away
     :inapt-if qq-transient--presence-inapt-p)
    ("b" "Busy" qq-presence-busy
     :inapt-if qq-transient--presence-inapt-p)]
   ["Privacy / custom"
    ("d" "Do not disturb" qq-presence-do-not-disturb
     :inapt-if qq-transient--presence-inapt-p)
    ("i" "Invisible" qq-presence-invisible
     :inapt-if qq-transient--presence-inapt-p)
    ("c" "Custom…" qq-presence-custom
     :inapt-if qq-transient--presence-inapt-p)]])

;;;###autoload(autoload 'qq-root-session-transient "qq" nil t)
(transient-define-prefix qq-root-session-transient ()
  "Operate on the session row at point in the QQ directory."
  [["Session"
    ("RET" "Open" qq-root-open-at-point)
    ("a" "Open avatar" qq-root-open-avatar-at-point)
    ("i" "Session info" qq-root-open-info-at-point
     :inapt-if qq-transient--root-info-inapt-p)]]
  (interactive)
  (qq-transient--require-root)
  (when (qq-transient--no-session-at-point-p)
    (user-error "qq: put point on a session first"))
  (transient-setup 'qq-root-session-transient))

;;;###autoload(autoload 'qq-account-transient "qq" nil t)
(transient-define-prefix qq-account-transient ()
  "Manage QQ accounts and the selected account's identity."
  [["Account"
    ("I" "My profile" qq-root-open-self-user)
    ("A" "Open account…" qq-root-switch-account
     :if (lambda () qq-multi-account))
    ("M" "Account manager" qq-root-open-gateway
     :if (lambda () qq-multi-account))
    ("F" "Read-only FileBridge" qq-account-filebridge)
    ("l" "Login / continue" qq-login)
    ("a" "Change login account" qq-login-switch-account
     :if (lambda () (not qq-multi-account)))
    ("n" "Login new account" qq-login-new-account
     :if (lambda () qq-multi-account))
    ("p" "Presence…" qq-presence-transient
     :inapt-if qq-transient--presence-inapt-p)
    ("c" "Connection…" qq-connection-transient)]]
  (interactive)
  (qq-transient--require-root)
  (transient-setup 'qq-account-transient))

;;;###autoload(autoload 'qq-connection-transient "qq" nil t)
(transient-define-prefix qq-connection-transient ()
  "Manage the selected QQ account's connection and cached state."
  [["Connection"
    ("c" "Connect" qq-connect)
    ("C" "Disconnect" qq-disconnect)
    ("x" "Reset state" qq-reset-session-state)]]
  (interactive)
  (qq-transient--require-root)
  (transient-setup 'qq-connection-transient))

;;;###autoload(autoload 'qq-root-transient "qq" nil t)
(transient-define-prefix qq-root-transient ()
  "Browse QQ conversations and open focused management menus."
  [["Sessions"
    ("g" "Refresh" qq-root-refresh)
    ("RET" "Session at point…" qq-root-session-transient
     :inapt-if qq-transient--no-session-at-point-p)
    ("/" "Search saved records…" qq-search-messages)
    ("s" "Find session…" qq-root-open-session)
    ("u" "Next unread" qq-root-next-unread)]
   ["Directories"
    ("d" "Contacts" qq-contacts-open)
    ("r" "Group requests" qq-group-requests-open)
    ("A" (lambda () (if qq-multi-account "Accounts…" "Account…"))
     qq-account-transient)]
   ["Window"
    ("q" "Quit window" quit-window)
    ("?" "Describe mode" describe-mode)]]
  (interactive)
  (qq-transient--require-root)
  (transient-setup 'qq-root-transient))

(provide 'qq-transient)

;;; qq-transient.el ends here
