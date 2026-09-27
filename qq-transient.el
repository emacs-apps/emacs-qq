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
(require 'qq-state)

(declare-function qq-connect "qq")
(declare-function qq-disconnect "qq")
(declare-function qq-reset-session-state "qq")


;;; Availability helpers

(defun qq-transient--require-chat ()
  "Require a QQ chat buffer with a conversation."
  (unless (and (derived-mode-p 'qq-chat-mode) qq-chat--session-key)
    (user-error "qq: open a chat first")))

(defun qq-transient--require-message ()
  "Require a message in the current QQ chat timeline."
  (qq-transient--require-chat)
  (when (or (appkit-chatbuf-point-in-input-p)
            (qq-transient--no-message-at-point-p))
    (user-error "qq: put point on a message first")))

(defun qq-transient--require-root ()
  "Require the QQ session directory."
  (unless (derived-mode-p 'qq-root-mode)
    (user-error "qq: open the session directory first")))

(defun qq-transient--message-at-point ()
  "Return message at point, or nil without signaling."
  (ignore-errors (qq-chat--message-at-point)))

(defun qq-transient--no-message-at-point-p ()
  "Return non-nil when there is no message under point."
  (null (qq-transient--message-at-point)))

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
            (qq-chat--poke-sender-at-point)
            nil)
        (user-error t))))

(defun qq-transient--reply-inapt-p ()
  "Return non-nil when reply is unavailable for the message at point."
  (let ((message (qq-transient--message-at-point)))
    (or (null message)
        (null (qq-message-reply-target qq-chat--session-key message))
        (qq-state-message-recalled-p message))))

(defun qq-transient--goto-reply-inapt-p ()
  "Return non-nil when the message at point has no reply target to jump to."
  (let ((message (qq-transient--message-at-point)))
    (or (null message)
        (null (qq-chat--message-reply-id message)))))

(defun qq-transient--pop-ring-empty-p ()
  "Return non-nil when the messages pop ring is empty."
  (or (not (boundp 'qq-chat--messages-pop-ring))
      (null qq-chat--messages-pop-ring)
      (not (ring-p qq-chat--messages-pop-ring))
      (ring-empty-p qq-chat--messages-pop-ring)))

(defun qq-transient--delete-local-inapt-p ()
  "Return non-nil when local deletion is unavailable at point."
  (let ((message (qq-transient--message-at-point)))
    (or (null message)
        (not (qq-message-delete-local-capable-p message)))))

(defun qq-transient--recall-inapt-p ()
  "Return non-nil when recall is unavailable for the message at point."
  (let* ((message (qq-transient--message-at-point))
         (poke-p (and message (qq-state-poke-message-p message))))
    (or (null message)
        (if poke-p
            (not (qq-message-poke-recall-capable-p message))
          (not (qq-message-recall-capable-p message))))))

(defun qq-transient--forward-inapt-p ()
  "Return non-nil when forwarding the message at point is unavailable."
  (or (not (qq-chat--forward-source-supported-p))
      (not (qq-chat--message-forwardable-p
            (qq-transient--message-at-point)))))

(defun qq-transient--reaction-inapt-p ()
  "Return non-nil when reacting to the message at point is unavailable."
  (not (qq-chat--message-reactable-p
        (qq-transient--message-at-point))))

(defun qq-transient--essence-inapt-p ()
  "Return non-nil when toggling essence at point is unavailable."
  (not (qq-chat--message-essence-capable-p
        (qq-transient--message-at-point))))

(defun qq-transient--todo-inapt-p ()
  "Return non-nil when mutating todo at point is unavailable."
  (not (qq-chat--message-todo-capable-p
        (qq-transient--message-at-point))))

(defun qq-transient--no-message-selection-p ()
  "Return non-nil when there are no selected message memberships to clear."
  (null qq-chat--message-selection))

(defun qq-transient--forward-selection-inapt-p ()
  "Return non-nil when neither selection nor point can be forwarded."
  (or qq-chat--forward-request-owner
      (not (qq-chat--forward-source-supported-p))
      (not (or (and qq-chat--message-selection
                    (qq-chat-selected-messages))
               (qq-chat--message-forwardable-p
                (qq-transient--message-at-point))))))

(defun qq-transient--resource-inapt-p ()
  "Return non-nil when open-resource is unavailable at point."
  (appkit-media-card-action-inapt-reason 'open))

(defun qq-transient--no-media-at-point-p ()
  "Return non-nil when there is no media object at point."
  (null (appkit-media-card-context-at-point)))

(defun qq-transient--avatar-inapt-p ()
  "Return non-nil when avatar open is unavailable at point."
  (let ((message (qq-transient--message-at-point)))
    (or (null message)
        (null (alist-get 'sender-id message)))))

(defun qq-transient--user-inapt-p ()
  "Return non-nil when the message sender has no user page."
  (let* ((message (qq-transient--message-at-point))
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
(transient-define-prefix qq-chat-message-todo-transient ()
  "Todo actions for the QQ group message at point."
  [["Todo"
    ("s" "Set" qq-chat-set-message-todo
     :inapt-if qq-transient--todo-inapt-p)
    ("c" "Complete" qq-chat-complete-message-todo
     :inapt-if qq-transient--todo-inapt-p)
    ("x" "Cancel" qq-chat-cancel-message-todo
     :inapt-if qq-transient--todo-inapt-p)]]
  (interactive)
  (qq-transient--require-message)
  (transient-setup 'qq-chat-message-todo-transient))

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
(transient-define-prefix qq-chat-delete-transient ()
  "Choose local deletion or QQ ordinary recall for the message at point."
  [["Delete"
    ("d" "Delete locally" qq-chat-delete-message
     :inapt-if qq-transient--delete-local-inapt-p)
    ("r" "Recall from QQ" qq-chat-recall-message
     :inapt-if qq-transient--recall-inapt-p)]]
  (interactive)
  (qq-transient--require-message)
  (transient-setup 'qq-chat-delete-transient))

;;;###autoload(autoload 'qq-transient-msg-operate "qq" nil t)
(transient-define-prefix qq-transient-msg-operate ()
  "Message actions for the QQ chat message at point.

Prefer this over inline button rows."
  [["Message"
    ("r" "Reply" qq-chat-reply-to-message
     :inapt-if qq-transient--reply-inapt-p)
    ("f" "Forward…" qq-chat-forward-transient
     :inapt-if qq-transient--forward-inapt-p)
    ("m" "Select / unselect" qq-chat-toggle-message-selection
     :inapt-if qq-transient--forward-inapt-p)
    ("d" "Delete…" qq-chat-delete-transient)
    ("!" "React…" qq-chat-react-to-message
     :inapt-if qq-transient--reaction-inapt-p)
    ("e" "Toggle essence" qq-chat-toggle-message-essence
     :inapt-if qq-transient--essence-inapt-p)
    ("t" "Todo…" qq-chat-message-todo-transient
     :inapt-if qq-transient--todo-inapt-p)
    ("i" "Sender…" qq-chat-sender-transient)
    ("o" "Media…" qq-chat-media-transient
     :inapt-if qq-transient--no-media-at-point-p)
    ("g" "Goto reply target" qq-chat-goto-reply
     :inapt-if qq-transient--goto-reply-inapt-p)]]
  (interactive)
  (qq-transient--require-message)
  (transient-setup 'qq-transient-msg-operate))

;;;###autoload(autoload 'qq-chat-sender-transient "qq" nil t)
(transient-define-prefix qq-chat-sender-transient ()
  "Actions on the sender of the QQ message at point."
  [["Sender"
    ("P" "Poke" qq-chat-poke-sender
     :inapt-if qq-transient--poke-sender-inapt-p)
    ("a" "Open avatar" qq-chat-open-avatar-at-point
     :inapt-if qq-transient--avatar-inapt-p)
    ("i" "User page" qq-chat-open-user-at-point
     :inapt-if qq-transient--user-inapt-p)]]
  (interactive)
  (qq-transient--require-message)
  (transient-setup 'qq-chat-sender-transient))

;;;###autoload(autoload 'qq-chat-media-transient "qq" nil t)
(transient-define-prefix qq-chat-media-transient ()
  "Operate on the QQ media card at point."
  [["Media"
    ("o" "Open / play" appkit-media-card-open
     :inapt-if qq-transient--resource-inapt-p)
    ("D" "Download / retry" appkit-media-card-download
     :inapt-if (lambda ()
                 (appkit-media-card-action-inapt-reason 'download)))
    ("s" "Save as" appkit-media-card-save-as
     :inapt-if (lambda ()
                 (appkit-media-card-action-inapt-reason 'save-as)))
    ("y" "Copy media URL" appkit-media-card-copy-url
     :inapt-if (lambda ()
                 (appkit-media-card-action-inapt-reason 'copy-url)))]]
  (interactive)
  (qq-transient--require-chat)
  (when (or (appkit-chatbuf-point-in-input-p)
            (qq-transient--no-media-at-point-p))
    (user-error "qq: put point on a media card first"))
  (transient-setup 'qq-chat-media-transient))

(defun qq-transient--forward-plan-scope ()
  "Return the immutable plan exported by the forwarding prefix."
  (let ((plan (transient-scope 'qq-chat-forward-transient)))
    (unless (qq-chat-forward-plan-p plan)
      (user-error "qq: forwarding action requires an active forwarding menu"))
    plan))

(transient-define-suffix qq-transient-forward-merged (plan)
  "Forward PLAN as one native merged-forward card."
  :transient nil
  (interactive (list (qq-transient--forward-plan-scope)))
  (qq-chat-forward-merged plan))

;;;###autoload(autoload 'qq-chat-forward-transient "qq" nil t)
(transient-define-prefix qq-chat-forward-transient (plan)
  "Choose explicit QQ forwarding semantics for one immutable PLAN."
  [:description
   (lambda ()
     (format "转发 %d 条消息"
             (length
              (qq-chat-forward-plan-anchors
               (oref (transient-prefix-object) scope)))))]
  [["发送方式"
    ("m" "合并转发…" qq-transient-forward-merged)]]
  (interactive (list (qq-chat--current-forward-plan)))
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
  [["Timeline"
    ("g" "Refresh" qq-chat-refresh)
    (">" "Latest / mark read" qq-chat-read-all)
    ("f" "Forward selected / at point…" qq-chat-forward-transient
     :inapt-if qq-transient--forward-selection-inapt-p)
    ("U" "Clear message selection" qq-chat-clear-message-selection
     :inapt-if qq-transient--no-message-selection-p)
    ("m" "Message at point…" qq-transient-msg-operate
     :inapt-if qq-transient--no-message-at-point-p)
    ("n" "Next message" qq-chat-next-message)
    ("p" "Previous message" qq-chat-previous-message)
    ("x" "Pop jump" qq-chat-goto-pop-message
     :inapt-if qq-transient--pop-ring-empty-p)]
   ["Conversation"
    ("c" "Compose…" qq-chat-composer-transient)
    ("s" "Conversation details…" qq-chat-session-transient)
    ("q" "Quit window" quit-window)
    ("?" "Describe mode" describe-mode)]]
  (interactive)
  (qq-transient--require-chat)
  (if (appkit-chatbuf-point-in-input-p)
      (qq-chat-composer-transient)
    (transient-setup 'qq-chat-transient)))

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
    ("E" "QQ face" qq-chat-attach-face)
    ("F" "Favorite face" qq-chat-attach-custom-face)]]
  (interactive)
  (qq-transient--require-chat)
  (qq-chat-edit-draft)
  (transient-setup 'qq-chat-attach-transient))

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
    ("A" "Open account…" qq-root-switch-account)
    ("M" "Account manager" qq-root-open-gateway)
    ("l" "Login / continue" qq-login)
    ("n" "Login new account" qq-login-new-account)
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
    ("/" "Find session…" qq-root-open-session)
    ("u" "Next unread" qq-root-next-unread)]
   ["Directories"
    ("d" "Contacts" qq-contacts-open)
    ("r" "Group requests" qq-group-requests-open)
    ("A" "Accounts…" qq-account-transient)]
   ["Window"
    ("q" "Quit window" quit-window)
    ("?" "Describe mode" describe-mode)]]
  (interactive)
  (qq-transient--require-root)
  (transient-setup 'qq-root-transient))

(provide 'qq-transient)

;;; qq-transient.el ends here
