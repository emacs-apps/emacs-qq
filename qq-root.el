;;; qq-root.el --- Root buffer for emacs-qq -*- lexical-binding: t; -*-

;; Author: 0WD0 <me@0wd0.com>

;;; Commentary:

;; Persistent keyed-EWOC root view.  Session rows use appkit's shared list
;; reconciliation and one-line presentation infrastructure.

;;; Code:

(require 'cl-lib)

(require 'seq)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-bindings)
(require 'appkit-directory)
(require 'appkit-projection)
(require 'appkit-transaction)
(require 'appkit-presentation)

(require 'qq-core)
(require 'qq-chat)
(require 'qq-search)
(require 'qq-account)
(require 'qq-media)
(require 'qq-login)
(require 'qq-runtime)
(require 'qq-state)
(require 'qq-protocol)

(autoload 'qq-user-open "qq-user" nil t)
(autoload 'qq-group-open "qq-group" nil t)
(autoload 'qq-guild-open "qq-guild" nil t)
(autoload 'qq-contacts-open "qq-contacts" nil t)
(autoload 'qq-group-requests-open "qq-group-requests" nil t)

(declare-function qq-root-transient "qq-transient" ())

(defconst qq-root-buffer-name "*qq-root*"
  "Name of the emacs-qq root buffer.")

(defvar-local qq-root--fill-column nil
  "Last root width measured from a window that actually displayed it.")

(defvar-local qq-root--scope nil
  "Stable account ID shown by this root, or `gateway' for account management.")

(defun qq-root--stable-fill-column ()
  "Return the root width from its Surface-owned responsive geometry."
  (max 60
       (or (appkit-surface-responsive-width
            (appkit-current-surface) qq-root-auto-fill-margin-columns)
           qq-root--fill-column
           80)))

(defun qq-root--buffer-width ()
  "Return current root row width in columns."
  (max 60 (or qq-root--fill-column
              (setq-local qq-root--fill-column
                          (qq-root--stable-fill-column)))))

(defun qq-root--format-time (timestamp)
  "Return display string for TIMESTAMP."
  (if (and timestamp (> timestamp 0))
      (format-time-string "%m-%d %H:%M" (seconds-to-time timestamp))
    ""))

(defun qq-root--account-title (account self-info)
  "Return the user-facing title for selected ACCOUNT and SELF-INFO.

SELF-INFO may lag behind a Gateway account switch.  Its nickname is therefore
used only when its QQ number agrees with ACCOUNT."
  (let* ((account-uin (alist-get 'uin account))
         (self-uin (alist-get 'user_id self-info))
         (self-current-p
          (and self-info
               account-uin
               (equal account-uin self-uin)))
         (nickname (and self-current-p
                        (alist-get 'nickname self-info)))
         (label (alist-get 'label account))
         (uin (or account-uin
                  (and self-current-p self-uin)))
         (title (or nickname label uin "Unbound account")))
    (concat title
            (if (and uin (not (equal title uin)))
                (format " (%s)" uin)
              ""))))

(defun qq-root--header-line ()
  "Return dynamic header line for the root buffer."
  (let* ((gateway-p (eq qq-root--scope 'gateway))
         (self-info (and (not gateway-p) (qq-state-self-info (qq-runtime-state))))
         (status
          (if gateway-p
              (qq-server-state)
            (qq-state-connection-status (qq-runtime-state))))
         (owner (and (not gateway-p) qq-root--scope))
         (account (qq-account-get owner))
         (accounts (and qq-multi-account (qq-account-list)))
         (online-count
          (cl-count-if
           (lambda (snapshot)
             (equal (alist-get 'phase snapshot) "online"))
           accounts))
         (account-count (length accounts))
         (account-summary
          (cond
           (gateway-p (if qq-multi-account "account manager" "login"))
           (account
            (format "%s — %s"
                    (qq-root--account-title account self-info)
                    (alist-get 'phase account)))
           (t "account no longer managed")))
         (registry-summary
          (and (> account-count 1)
               (format " · %d/%d online" online-count account-count))))
    (format " emacs-qq  [%s]  %s%s"
            status account-summary (or registry-summary ""))))

(defun qq-root--recent-sessions ()
  "Return the ordered sessions in the authoritative recent projection."
  (mapcar
   (lambda (session-key)
     (or (qq-state-session (qq-runtime-state) session-key)
         (error "qq: recent projection references missing session %s"
                session-key)))
   (qq-state-recent-session-keys (qq-runtime-state))))

(defun qq-root--activity-metrics (&optional sessions)
  "Return root activity metrics for SESSIONS or current recent projection."
  (let* ((sessions (or sessions (qq-root--recent-sessions)))
         (badges-complete-p
          (cl-every (lambda (session)
                      (let ((badge (alist-get 'unread-badge-count session)))
                        (and (integerp badge) (>= badge 0))))
                    sessions))
         (unread (and badges-complete-p
                      (cl-count-if
                       (lambda (session)
                         (> (alist-get 'unread-badge-count session) 0))
                       sessions)))
         (important (and badges-complete-p
                         (cl-count-if #'qq-root--session-important-unread-p
                                      sessions)))
         (muted (and badges-complete-p
                     (cl-count-if
                      (lambda (session)
                        (and (qq-root--session-muted-p session)
                             (> (alist-get 'unread-badge-count session) 0)))
                      sessions)))
         (dms (cl-count-if (lambda (session)
                             (not (eq (alist-get 'type session) 'group)))
                           sessions)))
    (list :all (length sessions)
          :unread unread
          :important important
          :muted muted
          :dms dms)))

(defun qq-root--filter-chip (label count &optional active)
  "Return one root filter chip for LABEL and exact COUNT.

Render `?' instead of a partial number when COUNT is nil."
  (format "[%s%s:%s]"
          (if active "*" "")
          label
          (if (integerp count) (number-to-string count) "?")))

(defun qq-root--filters-line (&optional sessions)
  "Return filter-chip line for SESSIONS or current state."
  (let ((metrics (qq-root--activity-metrics sessions)))
    (string-join
     (list (qq-root--filter-chip "Main" (plist-get metrics :all) t)
           (qq-root--filter-chip "Important" (plist-get metrics :important))
           (qq-root--filter-chip "Muted" (plist-get metrics :muted))
           (qq-root--filter-chip "DMs" (plist-get metrics :dms))
           "[activity sort:recent]")
     "  ")))

(defun qq-root--mode-divider-line ()
  "Return a divider line carrying the active root mode marker."
  (let* ((label "(activity/all)")
         (width (max (qq-root--buffer-width) (+ 8 (string-width label))))
         (filler (max 0 (- width (string-width label) 2)))
         (left (/ filler 2))
         (right (- filler left)))
    (concat "_/"
            (make-string left ?-)
            label
            (make-string right ?-))))

(defun qq-root--session-context-label (session)
  "Return one-line context label for SESSION."
  (let ((label (or (alist-get 'title session)
                   (alist-get 'key session)
                   "session")))
    (if (eq (alist-get 'pinned session) t)
        (concat "\N{PUSHPIN} " label)
      label)))

(defun qq-root--session-muted-p (session)
  "Return non-nil when SESSION has QQ message notifications muted."
  (eq (alist-get 'muted-p session) t))

(defun qq-root--session-mention-kinds (session)
  "Return unread native mention kinds represented by SESSION."
  (delq nil
        (list (and (or (alist-get 'unread-at-me-message-id session)
                       (alist-get 'unread-at-me-message-seq session))
                   'at-me)
              (and (or (alist-get 'unread-at-all-message-id session)
                       (alist-get 'unread-at-all-message-seq session))
                   'at-all))))

(defun qq-root--session-important-unread-p (session)
  "Return non-nil when SESSION has unmuted unread or an unread mention."
  (or (and (> (or (alist-get 'unread-badge-count session) 0) 0)
           (not (qq-root--session-muted-p session)))
      (qq-root--session-mention-kinds session)))

(defun qq-root--format-unread-count (count)
  "Format positive badge COUNT using QQ's visible 99+ cap."
  (if (> count 99) "99+" (number-to-string count)))

(defun qq-root--session-unread-trail (session)
  "Return SESSION's propertized unread trail for the title brackets.

Like telega's chat unread trail, the active adapter's complete named Badge
Count follows the title and uses a muted or unmuted face.  Mention kinds
remain independently prominent even when the badge is unavailable or the
session muted."
  (let* ((badge (alist-get 'unread-badge-count session))
         (unread (and (integerp badge) (> badge 0) badge))
         (mentions (qq-root--session-mention-kinds session))
         (count-face (if (qq-root--session-muted-p session)
                         'qq-root-muted-count
                       'qq-root-unmuted-count)))
    (string-join
     (delq nil
           (list (and unread
                      (propertize (qq-root--format-unread-count unread)
                                  'face count-face))
                 (and (memq 'at-me mentions)
                      (propertize "@" 'face 'qq-root-mention-count))
                 (and (memq 'at-all mentions)
                      (propertize "@all" 'face 'qq-root-mention-count))))
     " ")))

(defun qq-root--session-icon-face (session)
  "Return the icon face for SESSION."
  (cond
   ((qq-root--session-muted-p session) 'shadow)
   ((eq (alist-get 'type session) 'group) 'font-lock-keyword-face)
   (t 'font-lock-variable-name-face)))

(defun qq-root--insert-session-icon (session)
  "Insert SESSION's avatar/icon at the current text-line height."
  (let ((start (point)))
    (insert (qq-media-session-avatar-display-string session))
    (add-text-properties
     start
     (point)
     (list 'face (qq-root--session-icon-face session)
           'help-echo (format "Open avatar for %s"
                              (or (alist-get 'title session)
                                  (alist-get 'key session)
                                  "session"))))))

(defun qq-root--session-last-message (session)
  "Return SESSION's cached latest message object, or nil."
  (qq-state-session-message (qq-runtime-state)
                            (alist-get 'key session)
                            (alist-get 'last-message-id session)
                            (alist-get 'last-message-local-id session)))

(defun qq-root--session-preview-label-face (session identity message)
  "Return the sender-label face for SESSION's latest MESSAGE.

Prefer MESSAGE's stable sender identity so the root and timeline agree.  A
session-only preview uses summary IDENTITY; its visible SENDER is a last-resort
fallback when the protocol supplied no identity."
  (cond
   ((qq-chat--present-string (alist-get 'sender-name message))
    (qq-chat--message-title-face message))
   ((eq (alist-get 'last-message-self-p session) t)
    (if-let* ((color-face (appkit-name-color-face identity)))
        (list color-face 'qq-msg-self-title)
      'qq-msg-self-title))
   ((when-let* ((color-face (appkit-name-color-face identity)))
      (list color-face 'qq-msg-user-title)))
   (t 'qq-msg-user-title)))

(defun qq-root--session-preview-model (session)
  "Return the Appkit one-line preview for SESSION.

Peer chat-actions take priority over the last message.  Group previews identify
an ordinary message's sender; private previews do so only for outgoing
messages, since the session title already identifies an incoming peer."
  (let* ((session-key (alist-get 'key session))
         (action-text
          (and qq-chat-show-peer-actions
               session-key
               (qq-state-preview-one-line
                (qq-state-action-text (qq-runtime-state) session-key))))
         (preview
          (qq-state-preview-one-line
           (alist-get 'last-message-preview session)))
         (sender
          (qq-state-preview-one-line
           (alist-get 'last-message-sender-name session)))
         (identity
          (or (qq-chat--present-string
               (alist-get 'last-message-sender-id session))
              sender))
         (message (qq-root--session-last-message session))
         (show-sender-p
          (and (not (string-empty-p sender))
               (pcase (alist-get 'type session)
                 ('group t)
                 ('private (eq (alist-get 'last-message-self-p session) t))
                 (_ nil)))))
    (if (and (stringp action-text) (not (string-empty-p action-text)))
        (appkit-ui-one-line-preview-create
         :text (concat (or qq-chat-action-prefix ".. ") action-text))
      (let ((label
             (and show-sender-p
                  (not (string-empty-p preview))
                  sender)))
        (qq-media-message-one-line-preview
         message preview
         :label label
         :separator (and label appkit-ui-sender-text-delimiter)
         :mention-function #'qq-chat--mention-preview-string
         :label-face
         (and label
              (qq-root--session-preview-label-face
               session identity message)))))))

(defun qq-root--session-one-line-row (session)
  "Return one-line row model for SESSION."
  (let* ((session-key (alist-get 'key session))
         (unread (or (alist-get 'unread-badge-count session) 0))
         (muted (qq-root--session-muted-p session))
         (important (qq-root--session-important-unread-p session))
         (preview (qq-root--session-preview-model session)))
    (appkit-presentation-one-line-row-create
     :icon-inserter (lambda ()
                      (qq-root--insert-session-icon session))
     :context (qq-root--session-context-label session)
     :context-trail (qq-root--session-unread-trail session)
     :preview preview
     ;; Unread activity lives in the title trail.  Keep a dedicated time-tail
     ;; face (meant for a trailing status glyph) off the plain timestamp.
     :time (qq-root--format-time (alist-get 'last-message-time session))
     :time-face 'shadow
     :time-tail-face nil
     :line-properties
     (list 'qq-root-row-type 'session
           'qq-root-session-key session-key
           'qq-root-badge-count unread
           'qq-root-has-unread (and (> unread 0) t)
           'qq-root-muted-p muted
           'qq-root-has-important-unread (and important t))
     :help-echo (format "Open %s%s"
                        session-key
                        (if muted " (message notifications muted)" "")))))

(defun qq-root--insert-session-line (session)
  "Insert one session row for SESSION."
  (appkit-presentation-insert-one-line-row
   (qq-root--session-one-line-row session)
   :width (qq-root--buffer-width)
   :icon-slot-width 'auto
   :context-width-spec '(0.32 16 30)))

(defun qq-root--session-entry-key (session-key)
  "Return the stable root entry key for SESSION-KEY."
  (cons 'session session-key))

(defun qq-root--insert-entry (_surface entry)
  "Insert application-owned presentation for directory ENTRY."
  (pcase (car-safe (appkit-directory-entry-key entry))
    ('session
     (qq-root--insert-session-line (appkit-directory-entry-payload entry))
     t)
    ('account
     (let* ((account (appkit-directory-entry-payload entry))
            (account-id (alist-get 'account_id account))
            (label (or (alist-get 'label account)
                       (alist-get 'uin account)
                       account-id))
            (uin (alist-get 'uin account))
            (phase (alist-get 'phase account)))
       (insert (format "  %-16s  %s%s\n"
                       phase label
                       (if (and uin (not (equal label uin)))
                           (format " (%s)" uin)
                         ""))))
     t)))

(defun qq-root--login-entries ()
  "Project foreground login into physical directory rows without losing properties."
  (when-let* ((login (qq-login-view-model))
              (visible-p
               (or (eq qq-root--scope 'gateway)
                   (equal qq-root--scope (plist-get login :account-id)))))
    (cons
     (appkit-directory-entry-create
      :key 'login-gap
      :role 'spacer)
     (with-temp-buffer
       (qq-login-insert-view login)
       (goto-char (point-min))
       (let ((index 0)
             entries)
         (while (not (eobp))
           (let ((text (buffer-substring (point) (line-end-position))))
             (push
              (appkit-directory-entry-create
               :key (if (= index 0) 'login (cons 'login index))
               :role 'note
               :label text
               :stamp (list login (object-intervals text)))
              entries))
           (cl-incf index)
           (forward-line 1))
         (nreverse entries))))))

(defun qq-root--project-gateway-entries ()
  "Project the startup view, or the explicitly enabled account manager."
  (if (not qq-multi-account)
      (or (qq-root--login-entries)
          (list
           (appkit-directory-entry-create
            :key 'startup
            :role 'note
            :label "Start QQ login from the account menu, or run M-x qq-login."
            :face 'shadow)))
    (let ((accounts (qq-account-list)))
      (append
       (list
        (appkit-directory-entry-create
         :key 'accounts-heading
         :role 'section
         :label (format "Managed accounts: %d" (length accounts))
         :face 'font-lock-doc-face)
        (appkit-directory-entry-create
         :key 'accounts-gap
         :role 'spacer))
       (mapcar
        (lambda (account)
          (let ((account-id (alist-get 'account_id account)))
            (appkit-directory-entry-create
             :key (cons 'account account-id)
             :role 'item
             :section-key 'accounts-heading
             :item-p t
             :primary-action 'item
             :payload account
             :help-echo "Open this account's persistent QQ view"
             :mouse-face 'highlight
             :properties (list 'qq-root-row-type 'account
                               'qq-root-account-id account-id))))
        accounts)
       (unless accounts
         (list
          (appkit-directory-entry-create
           :key 'no-accounts
           :role 'note
           :label "No managed QQ accounts.  Start login to create one."
           :face 'shadow)))
       (qq-root--login-entries)))))

(defun qq-root--project-account-entries ()
  "Project the current account state into stable-keyed directory entries."
  (let* ((sessions (qq-root--recent-sessions))
         (width (qq-root--buffer-width))
         (metadata
          (list
           (appkit-directory-entry-create
            :key 'filters
            :role 'note
            :label (qq-root--filters-line sessions)
            :face 'font-lock-doc-face)
           (appkit-directory-entry-create
            :key 'divider
            :role 'section
            :label (qq-root--mode-divider-line)
            :face 'shadow)
           (appkit-directory-entry-create
            :key 'metadata-gap
            :role 'spacer))))
    (append
     metadata
     (qq-root--login-entries)
     (mapcar
      (lambda (session)
        (let ((session-key (alist-get 'key session)))
          (appkit-directory-entry-create
           :key (qq-root--session-entry-key session-key)
           :role 'item
           :section-key 'divider
           :item-p t
           :unread-p (and (or (> (or (alist-get 'unread-badge-count session) 0) 0)
                              (qq-root--session-mention-kinds session))
                          t)
           :primary-action 'item
           :payload session
           :stamp width
           :help-echo (format "Open %s%s" session-key
                              (if (qq-root--session-muted-p session)
                                  " (message notifications muted)"
                                "")))))
      sessions)
     (unless sessions
       (list
        (appkit-directory-entry-create
         :key 'empty
         :role 'note
         :label "No recent conversations available yet."
         :face 'shadow))))))

(defun qq-root--project-entries ()
  "Project this root buffer's explicit account or Gateway scope."
  (if (eq qq-root--scope 'gateway)
      (qq-root--project-gateway-entries)
    (unless (and (stringp qq-root--scope)
                 (equal qq-root--scope qq-runtime--account-id))
      (error "qq: account root has no stable account scope"))
    (qq-root--project-account-entries)))

(defun qq-root--session-entry-keys ()
  "Return stable entry keys for all current sessions."
  (unless (eq qq-root--scope 'gateway)
    (mapcar (lambda (session)
              (qq-root--session-entry-key (alist-get 'key session)))
            (qq-root--recent-sessions))))

(defun qq-root--render (surface _app-view _model change)
  "Render CHANGE through the shared directory reconciliation owner."
  (let* ((directory (appkit-directory-surface))
         (keys (appkit-projection-change-keys change))
         (geometry-p (appkit-projection-change-geometry-p change))
         (position (appkit-projection-change-position change))
         (reconcile-p (or (appkit-projection-change-full-p change) geometry-p)))
    (when (or reconcile-p keys position)
      (appkit-with-content-update surface
                                  (when geometry-p
                                    (setq-local qq-root--fill-column (qq-root--stable-fill-column)))
                                  (appkit-directory-reconcile
                                   directory (qq-root--project-entries)
                                   :force-keys (if reconcile-p (qq-root--session-entry-keys) keys))))
    (when (appkit-projection-change-frame-p change)
      (force-mode-line-update)))
  nil)

(defun qq-root--session-key-at-point (&optional pos)
  "Return the directory session key at POS, or nil."
  (when-let* ((entry (appkit-directory-entry-at-point pos)))
    (pcase (appkit-directory-entry-key entry)
      (`(session . ,session-key) session-key))))

(defun qq-root--account-id-at-point (&optional pos)
  "Return the directory managed account ID at POS, or nil."
  (when-let* ((entry (appkit-directory-entry-at-point pos)))
    (pcase (appkit-directory-entry-key entry)
      (`(account . ,account-id) account-id))))

(defun qq-root--session-at-point ()
  "Return root session object at point, or nil."
  (when-let* ((session-key (qq-root--session-key-at-point)))
    (qq-state-session (qq-runtime-state) session-key)))

(defun qq-root--activate-entry (_surface entry)
  "Open the session or managed account represented by directory ENTRY."
  (pcase (appkit-directory-entry-key entry)
    (`(session . ,session-key) (qq-chat-open session-key))
    (`(account . ,account-id) (qq-root-open-account account-id))
    (_ (user-error "qq: no session or account at point"))))

(defun qq-root--run-operation (_surface operation targets)
  "Apply supported directory OPERATION to captured TARGETS."
  (unless (eq operation 'mark-read)
    (user-error "qq: unsupported directory operation %s" operation))
  (pcase targets
    (`((session . ,session-key))
     (let ((owner qq-root--scope)
           (session (qq-state-session (qq-runtime-state) session-key)))
       (unless (and (stringp owner)
                    session
                    (memq (alist-get 'type session) '(private group)))
         (user-error "qq: this row has no supported conversation read operation"))
       (qq-runtime-request (qq-runtime-request-create 'read-conversation :session-key session-key))))
    (_ (user-error "qq: mark read requires the current conversation row"))))

(defun qq-root-open-at-point ()
  "Open the session or managed account at point."
  (interactive)
  (appkit-directory-activate))

(defun qq-root-open-avatar-at-point ()
  "Open avatar/icon for the session at point."
  (interactive)
  (qq-media-open-session-avatar
   (or (qq-root--session-at-point)
       (user-error "qq: no session at point"))))

(defun qq-root-open-user-at-point ()
  "Open the private session user's profile at point."
  (interactive)
  (let* ((session (or (qq-root--session-at-point)
                      (user-error "qq: no session at point")))
         (user-id (or (alist-get 'peer-uin session)
                      (alist-get 'target-id session))))
    (unless (and (eq (alist-get 'type session) 'private)
                 (qq-protocol-user-uin-p user-id))
      (user-error "qq: session has no user profile"))
    (qq-user-open user-id)))

(defun qq-root-open-info-at-point ()
  "Open the user or group profile for the session at point."
  (interactive)
  (let* ((session (or (qq-root--session-at-point)
                      (user-error "qq: no session at point")))
         (type (alist-get 'type session))
         (target-id (or (and (eq type 'private) (alist-get 'peer-uin session))
                        (alist-get 'target-id session))))
    (pcase type
      ('private
       (unless (qq-protocol-user-uin-p target-id)
         (user-error "qq: session has no user profile"))
       (qq-user-open target-id))
      ('group
       (unless (qq-protocol-group-uin-p target-id)
         (user-error "qq: session has no group profile"))
       (qq-group-open target-id))
      (_ (user-error "qq: session has no profile page")))))

(defun qq-root-open-self-user ()
  "Open the logged-in user's profile."
  (interactive)
  (let ((user-id (alist-get 'user_id (qq-state-self-info (qq-runtime-state)))))
    (unless (qq-protocol-user-uin-p user-id)
      (user-error "qq: self user profile is unavailable"))
    (qq-user-open user-id)))

(defun qq-root-mouse-open-at-point (event)
  "Open the session clicked by mouse EVENT."
  (interactive "e")
  (mouse-set-point event)
  (qq-root-open-at-point))

(defun qq-root-button-forward ()
  "Move to the next directory item, wrapping once."
  (interactive)
  (appkit-directory-move
   (appkit-directory-surface) #'appkit-directory-entry-item-p 1 t))

(defun qq-root-button-backward ()
  "Move to the previous directory item, wrapping once."
  (interactive)
  (appkit-directory-move
   (appkit-directory-surface) #'appkit-directory-entry-item-p -1 t))

(defun qq-root-tab-dwim ()
  "Move to the next session row."
  (interactive)
  (qq-root-button-forward))

(defun qq-root-next-unread ()
  "Move to the next unread session, wrapping once."
  (interactive)
  (appkit-directory-next-unread))

(defun qq-root--read-session-key (prompt &optional predicate)
  "Read and return a session key using PROMPT.

When PREDICATE is non-nil, only sessions for which it returns non-nil are
offered."
  (let* ((sessions (if predicate
                       (seq-filter predicate (qq-state-sessions (qq-runtime-state)))
                     (qq-state-sessions (qq-runtime-state))))
         (choices
          (mapcar (lambda (session)
                    (cons (format "%s  [%s]"
                                  (or (alist-get 'title session)
                                      (alist-get 'key session))
                                  (alist-get 'key session))
                          (alist-get 'key session)))
                  sessions)))
    (unless choices
      (user-error "qq: no sessions available"))
    (cdr (assoc (completing-read prompt choices nil t) choices))))

(defun qq-root-open-session ()
  "Prompt for a session and open its chat buffer."
  (interactive)
  (qq-chat-open (qq-root--read-session-key "Open session: ")))

(defun qq-root-switch-account ()
  "Change the QQ account used by this client.

Single-account mode offers saved login identities and a new-login action.
Multi-account mode opens a managed account's persistent root.  Other accounts'
views and Gateway runtimes remain unchanged."
  (interactive)
  (if qq-multi-account
      (progn
        (call-interactively #'qq-account-select)
        (qq-root-open (qq-account-current-id)))
    (qq-login-switch-account)))

(defun qq-root-open-account (account-id)
  "Open stable ACCOUNT-ID's persistent root without closing other accounts."
  (interactive (list (qq-account--read-account-id "Open QQ account: ")))
  (unless (qq-account-get account-id)
    (user-error "qq: QQ account does not exist: %s" account-id))
  (qq-root-open account-id))

(defun qq-root-refresh ()
  "Request fresh native recent and directory snapshots."
  (interactive)
  (qq-core-refresh))

(defvar qq-root-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'qq-contacts-open)
    (define-key map (kbd "C") #'qq-guild-open)
    (define-key map (kbd "P") #'qq-chat-toggle-pin)
    (define-key map (kbd "^") #'qq-chat-toggle-pin)
    (define-key map (kbd "N") #'qq-group-requests-open)
    (define-key map (kbd "a") #'qq-root-open-avatar-at-point)
    (define-key map (kbd "i") #'qq-root-open-info-at-point)
    (define-key map (kbd "I") #'qq-root-open-self-user)
    (define-key map [mouse-1] #'qq-root-mouse-open-at-point)
    (define-key map (kbd "u") #'qq-root-next-unread)
    map)
  "Keymap for `qq-root-mode'.")

(appkit-bindings-define 'qq-root-mode-map 'read-only
                        'activate #'qq-root-open-at-point
                        'app-menu #'qq-root-transient
                        'refresh #'qq-root-refresh
                        'mark-read #'appkit-directory-mark-read
                        'search #'qq-search-messages
                        'next-item #'qq-root-button-forward
                        'previous-item #'qq-root-button-backward
                        'next-button #'qq-root-tab-dwim
                        'previous-button #'qq-root-button-backward
                        'help #'describe-mode
                        'quit #'quit-window
                        'quit-cancel #'evil-quit)

(define-derived-mode qq-root-mode appkit-directory-mode "QQ-Root"
  "Major mode for the emacs-qq root buffer.

`?' opens `qq-root-transient' (discoverable command menu)."
  (appkit-directory-configure
   (appkit-directory-surface)
   :entry-inserter #'qq-root--insert-entry
   :activate-function #'qq-root--activate-entry
   :operation-function #'qq-root--run-operation)
  (setq-local switch-to-buffer-preserve-window-point nil)
  (setq-local qq-root--fill-column nil)
  (setq-local header-line-format '(:eval (qq-root--header-line))))

(defun qq-root--live-view (&optional account-id)
  "Return ACCOUNT-ID's existing live Appkit root view, or nil.

This lookup reads the current runtime registry directly.  It deliberately
does not call `qq-runtime-app', because state and media hooks must not create
an application session merely to discover that no root is open."
  (let* ((gateway-p (eq account-id 'gateway))
         (owner (and (not gateway-p)
                     (or account-id (qq-runtime-current-account-id))))
         (runtime (and owner (qq-runtime-account owner)))
         (app (if gateway-p
                  (and (appkit-app-live-p qq-runtime--app) qq-runtime--app)
                (and runtime (qq-runtime-account-app runtime)))))
    (when app
      (when-let* ((view (appkit-app-surface app 'root)))
        (and (with-current-buffer (appkit-surface-buffer view)
               (derived-mode-p 'qq-root-mode))
             view)))))

(defun qq-root--setup-scope (scope view)
  "Bind the current root buffer and its responsive geometry to SCOPE and VIEW."
  (setq-local qq-root--scope scope)
  (appkit-surface-enable-responsive-geometry
   view
   (lambda (surface _width)
     (qq-runtime-invalidate surface :geometry-p t))))

(defun qq-root-open (&optional account-id)
  "Open ACCOUNT-ID's root, or the account selected for the current UI mode.

Single-account mode uses the globally selected login account.  Multi-account
mode uses the current execution context's account.
ACCOUNT-ID may be `gateway' to open startup or the multi-account manager."
  (interactive)
  (let ((owner (and (not (eq account-id 'gateway))
                   (or account-id
                       (if qq-multi-account
                           (qq-runtime-current-account-id)
                         (qq-account-current-id))))))
    (cl-flet
        ((open-root ()
           (let* ((scope (or owner 'gateway))
                  (app (if owner (qq-runtime-app owner) (qq-runtime-gateway-app)))
                  (existing (appkit-app-surface app 'root))
                  (host (seq-find
                         (lambda (buffer)
                           (with-current-buffer buffer
                             (and (derived-mode-p 'qq-root-mode)
                                  (equal qq-root--scope scope)
                                  (not (appkit-current-surface)))))
                         (buffer-list)))
                  (view (qq-runtime-open-surface
                         :app app :account-id owner
                         :id 'root :mode 'qq-root-mode
                         :buffer (and (not existing) host)
                         :buffer-name (if owner
                                          (format "*qq-root:%s*" (qq-runtime-account-display-name owner))
                                        qq-root-buffer-name)
                         :renderer-factory (qq-runtime-renderer-factory #'qq-root--render)
                         :setup (apply-partially #'qq-root--setup-scope scope)
                         :select t))
                  (buffer (appkit-surface-buffer view)))
             (with-current-buffer buffer
               (qq-runtime-invalidate view :full-p t :frame-p t)
               (appkit-surface-refresh-responsive-geometry view)
               (unless (qq-root--session-key-at-point)
                 (goto-char (point-min))
                 (qq-root-button-forward)))
             buffer)))
      (if owner
          (qq-runtime-call-with-account owner #'open-root)
        (open-root)))))

(defun qq-root-open-gateway ()
  "Open the Gateway-wide account manager root."
  (interactive)
  (unless qq-multi-account
    (user-error "qq: enable the account manager with (setq qq-multi-account t)"))
  (qq-root-open 'gateway))

(defun qq-root--session-avatar-media-key (session)
  "Return the exact avatar cache key used by SESSION, or nil."
  (qq-media-session-avatar-cache-key session))

(defun qq-root--handle-media-cache-update (media-key)
  "Invalidate rows identified by MEDIA-KEY, or all session rows when nil."
  (when (or (null media-key) (stringp media-key))
    (dolist (runtime (qq-runtime-accounts))
      (let ((owner (qq-runtime-account-id runtime)))
        (when-let* ((surface (qq-root--live-view owner)))
          (qq-runtime-with-account owner
            (let (keys)
              (dolist (session (qq-state-sessions (qq-runtime-state)))
                (when (or (null media-key)
                          (equal media-key
                                 (qq-root--session-avatar-media-key session))
                          (member media-key
                                  (qq-media-message-one-line-preview-keys
                                   (qq-root--session-last-message session))))
                  (push (qq-root--session-entry-key (alist-get 'key session))
                        keys)))
              (when keys
                (qq-runtime-invalidate surface
                                       :keys keys
                                       :resources (and media-key (list media-key)))))))))))

(defun qq-root--handle-state-change (event)
  "Render the account root affected by state EVENT."
  (when-let* ((owner (plist-get event :account-id))
              (surface (qq-root--live-view owner)))
    (let* ((type (plist-get event :type))
           (session-key (plist-get event :session-key))
           (keys (and session-key
                      (list (qq-root--session-entry-key session-key))))
           (change
            (pcase type
              ((or 'connection 'self-info)
               (appkit-projection-change-create
                :frame-p t))
              ('action
               (and keys (appkit-projection-change-create
                          :keys keys)))
              ((or 'session 'message 'history 'reset 'sessions-refreshed
                   'friends-refreshed 'groups-refreshed 'recent-order)
               (appkit-projection-change-create
                :full-p t
                :keys keys)))))
      (when change
        (appkit-surface-post surface (list 'qq-render change))))))

(defun qq-root--handle-login-change ()
  "Reconcile visible login blocks, including removal after finish or cancel."
  (when-let* ((surface (qq-root--live-view 'gateway)))
    (qq-runtime-invalidate surface :full-p t))
  (let ((owner (plist-get (qq-login-view-model) :account-id)))
    (dolist (runtime (qq-runtime-accounts))
      (when-let* ((account-id (qq-runtime-account-id runtime))
                  (surface (qq-root--live-view account-id)))
        (when (or (equal owner account-id)
                  (with-current-buffer (appkit-surface-buffer surface)
                    (appkit-directory-entry-for-key
                     (appkit-directory-surface) 'login)))
          (qq-runtime-invalidate surface :full-p t))))))

(defun qq-root--handle-gateway-account-change (&rest _arguments)
  "Refresh account manager rows and every account-root header."
  (when-let* ((surface (qq-root--live-view 'gateway)))
    (qq-runtime-invalidate surface :full-p t :frame-p t))
  (dolist (runtime (qq-runtime-accounts))
    (when-let* ((surface (qq-root--live-view (qq-runtime-account-id runtime))))
      (qq-runtime-invalidate surface :frame-p t))))

(add-hook 'qq-media-cache-update-hook #'qq-root--handle-media-cache-update)
(add-hook 'qq-runtime-change-hook #'qq-root--handle-state-change)
(add-hook 'qq-login-change-hook #'qq-root--handle-login-change)
(add-hook 'qq-account-registry-changed-hook
          #'qq-root--handle-gateway-account-change)
(add-hook 'qq-account-selection-changed-hook
          #'qq-root--handle-gateway-account-change)

(provide 'qq-root)

;;; qq-root.el ends here
