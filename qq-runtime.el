;;; qq-runtime.el --- Appkit session ownership for emacs-qq -*- lexical-binding: t; -*-

;; Author: 0WD0 <me@0wd0.com>

;;; Commentary:

;; Own one Gateway application plus one account-scoped Appkit application for
;; every QQ account projected by emacs-qq.  Appkit owns lifecycle and views;
;; `qq-state' owns the account-partitioned protocol state.

;;; Code:

(require 'cl-lib)
(require 'appkit-core)
(require 'qq-state)
(require 'seq)
(require 'appkit-app)
(require 'appkit-surface)
(require 'appkit-projection)
(require 'appkit-effect)
(require 'appkit-operation)

(declare-function qq-rpc-call "qq-rpc")
(declare-function qq-server-gateway-instance-id "qq-server")
(declare-function qq-server-cancel "qq-server")
(declare-function qq-core-fetch-history-page "qq-core")
(declare-function qq-request-p "qq-request")
(declare-function qq-request-cancel "qq-request")
(declare-function qq-message--commit-history-result "qq-message")
(declare-function qq-message--event-update "qq-message")
(declare-function qq-media-update "qq-media")
(declare-function qq-timeline-app-update "qq-timeline")
(declare-function qq-core-disconnect "qq-core")
(declare-function qq-account-get "qq-account" (account-id))
(declare-function qq-account-current-id "qq-account")

(defvar qq-media--consumer)

(cl-defstruct (qq-runtime-account
               (:constructor qq-runtime-account--create))
  "One stable Gateway account's Appkit and canonical-state ownership."
  id
  app
  input-status-expiry)

(defvar qq-runtime--accounts (make-hash-table :test #'equal)
  "Account runtimes keyed by stable Gateway account ID.")

(defvar qq-runtime--snapshots (make-hash-table :test #'equal)
  "Retained immutable account data, independent of executable runtimes.")

(defvar qq-runtime-change-hook nil
  "Listeners receiving committed account changes and their immutable :state.")

(defvar qq-runtime--context-account 'unscoped
  "Explicit transport/UI owner, independent of the current buffer.")

(defvar-local qq-runtime--account-id nil
  "Stable Gateway account owning the current account-scoped buffer.")

(defvar qq-runtime--app nil
  "Gateway-wide Appkit application used by global emacs-qq views.")

(defun qq-runtime-call-with-account (account-id function)
  "Call FUNCTION with explicit ACCOUNT-ID ownership across buffer changes."
  (let ((qq-runtime--context-account account-id)) (funcall function)))

(defmacro qq-runtime-with-account (account-id &rest body)
  "Evaluate BODY in ACCOUNT-ID's canonical state context."
  (declare (indent 1) (debug t))
  `(qq-runtime-call-with-account ,account-id (lambda () ,@body)))

(defconst qq-runtime--gateway-type
  (appkit-app-type-create
   :name 'qq
   :init (lambda (_context input) (appkit-next :model input :render appkit-render-none))
   :update (lambda (_context model _message)
             (appkit-next :model model :render appkit-render-none))
   :shutdown #'qq-runtime--gateway-shutdown))

(defun qq-runtime--history-effect (account-id input route)
  "Fetch ACCOUNT-ID's DataLine history page for Surface INPUT, replying on ROUTE.
Other chats are Gateway timelines.  Keyed by the requesting Surface, so its
next request replaces this one."
  (appkit-effect-create
   :key (list 'qq-history (plist-get input :view))
   :input (list :input input :route route)
   :start
   (lambda (_context effect-input _observe resolve reject)
     (let* ((input (plist-get effect-input :input))
            (request
             (qq-runtime-call-with-account
              account-id
              (lambda ()
                (condition-case condition
                    (let ((session-key (plist-get input :session-key))
                          (cursor (plist-get input :cursor))
                          (limit (plist-get input :limit))
                          (failure (lambda (response reason)
                                     (funcall reject response reason))))
                      (qq-core-fetch-history-page
                       session-key cursor 'older resolve failure limit))
                  (error (funcall reject nil (error-message-string condition)) nil))))))
       (appkit-cancellation-create
        :kind 'logical
        :cancel (lambda ()
                  (when (qq-request-p request) (qq-request-cancel request))))))
   :success (lambda (effect-input raw)
              (list 'qq-history-fetched effect-input raw nil nil))
   :failure (lambda (effect-input response reason)
              (list 'qq-history-fetched effect-input nil response reason))))

(defun qq-runtime--account-update (context model message)
  "Commit account data, operation state and their notifications together."
  (let* ((next (qq-runtime--account-reduce context model message))
         (after (and (appkit-next-p next) (appkit-next-model next))))
    (when after
      (let* ((source (if (eq (car-safe message) 'qq-state-apply)
                         (if (eq (cadr message) #'qq-message--event-update) 'event 'local)
                       'response))
             (events (qq-state-changes (qq-state-partition-store model)
                                       (qq-state-partition-store after) source)))
        (when (and (listp (appkit-next-value next))
                   (plist-member (appkit-next-value next) :batch-message-ids))
          (setq events (seq-remove (lambda (event) (eq (plist-get event :type) 'message)) events))
          (push (append (list :type 'history :source 'response :mutation 'history
                              :state (qq-state-partition-store after))
                        (appkit-next-value next)) events))
        (dolist (change (appkit-data-map-diff (qq-state-partition-operations model)
                                              (qq-state-partition-operations after)))
          (when (and (eq (caar change) 'group-members)
                     (not (equal (plist-get (unless (eq (nth 1 change) appkit-data-absent) (nth 1 change)) :value)
                                 (plist-get (unless (eq (nth 2 change) appkit-data-absent) (nth 2 change)) :value))))
            (push (list :type 'group-members-refreshed
                        :account-id (qq-state-partition-account-id after)
                        :session-key (qq-state-session-key 'group (cadar change))
                        :group-id (cadar change) :state (qq-state-partition-store after)) events)))
        (when events
          (setq next (appkit-next
                      :model after :render (appkit-next-render next) :value (appkit-next-value next)
                      :commands (append (appkit-next-commands next)
                                        (list (appkit-command-start-effect
                                               (qq-runtime--notify-effect events)))))))))
    next))

(defconst qq-runtime--account-type
  (appkit-app-type-create
   :name 'qq-account
   :init (lambda (_context input) (appkit-next :model input :render appkit-render-none))
   :update #'qq-runtime--account-update
   :shutdown #'qq-runtime--account-shutdown))

(defun qq-runtime--surface-update (context model message)
  "Handle shared QQ presentation MESSAGE for this Surface."
  (or
   (appkit-operation-surface-update context model message)
   (pcase message
     (`(qq-data ,key)
      (run-hook-with-args 'qq-runtime-data-hook key)
      (appkit-next :model model
                   :render (appkit-projection-change-create :full-p t :frame-p t)))
     (`(qq-history-changed ,_)
      (appkit-next :model model
                   :render (appkit-projection-change-create :full-p t :frame-p t)))
     (`(qq-render ,change)
      (appkit-next :model model :render change))
     (`(qq-media . ,_) (qq-media-update context model message))
     (_ (appkit-next :model model :render appkit-render-none)))))

(defun qq-runtime-post-invalidation (surface &rest change)
  "Queue projection CHANGE for SURFACE, merging renders within the next pass.
Use from external callbacks.  Drain a full mailbox instead of losing updates."
  (let ((message (list 'qq-render (apply #'appkit-projection-change-create change))))
    (when (eq (appkit-surface-post surface message) 'full)
      (appkit-surface-send surface message))))

(defun qq-runtime-invalidate (surface &rest change)
  "Request a render of SURFACE using projection CHANGE keyword arguments."
  (appkit-surface-send surface
                       (list 'qq-render
                             (apply #'appkit-projection-change-create change))))

(defvar-local qq-runtime--surface-owner nil
  "Exact Surface whose teardown still owns this buffer's client work.")

(defun qq-runtime--gateway-shutdown (_app)
  "Stop transport resources owned by the Gateway Appkit application."
  (when (fboundp 'qq-core-disconnect)
    (qq-core-disconnect)))

(defun qq-runtime-model (&optional account-id)
  "Read ACCOUNT-ID's current or retained model without starting an App."
  (let ((owner (or account-id (qq-runtime-current-account-id))))
    (or (when-let* ((runtime (qq-runtime-account owner)))
          (appkit-app-model (qq-runtime-account-app runtime)))
        (gethash owner qq-runtime--snapshots)
        (qq-state-partition--create :store (qq-state-store-create :account-id owner)))))

(defun qq-runtime-state (&optional account-id)
  "Read ACCOUNT-ID's committed domain snapshot."
  (qq-state-partition-store (qq-runtime-model account-id)))

(defun qq-runtime-account-ids ()
  "Return all live or retained account identities."
  (delete-dups (append (hash-table-keys qq-runtime--accounts)
                       (hash-table-keys qq-runtime--snapshots))))

(defun qq-runtime-transact (reducer &rest arguments)
  "Apply pure REDUCER to the account snapshot and return its committed value."
  (let ((ticket (appkit-app-send (qq-runtime-app)
                                 (list 'qq-state-apply reducer arguments))))
    (unless (and (appkit-loop-ticket-p ticket)
                 (eq (appkit-loop-ticket-state ticket) 'accepted))
      (let ((reason (and (appkit-loop-ticket-p ticket) (appkit-loop-ticket-outcome ticket))))
        (if (and (consp reason) (symbolp (car reason)))
            (signal (car reason) (cdr reason))
          (error "qq: account transaction was not accepted: %S" ticket))))
    (appkit-loop-ticket-outcome ticket)))

(defun qq-runtime--notify-effect (events)
  "Deliver EVENTS to host listeners after their account commits."
  (appkit-effect-create
   :key (make-symbol "qq-state-change") :input events
   :start
   (lambda (_context changes _observe resolve _reject)
     (dolist (event changes)
       (qq-runtime-with-account (plist-get event :account-id)
         (run-hook-wrapped
          'qq-runtime-change-hook
          (lambda (function value)
            (condition-case condition
                (funcall function value)
              (error
               (message "qq: listener %S failed: %s"
                        function (error-message-string condition))))
            nil)
          event)))
     (funcall resolve nil)
     (appkit-cancellation-create :kind 'logical :cancel #'ignore))
   :success (lambda (_input _value) '(qq-state-notified))
   :failure (lambda (_input _body _reason) '(qq-state-notified))))

(defun qq-runtime-gateway-app ()
  "Return the live Gateway App."
  (unless (appkit-app-live-p qq-runtime--app)
    (setq qq-runtime--app
          (appkit-app-start qq-runtime--gateway-type :identity 'default)))
  qq-runtime--app)

(defun qq-runtime-current-account-id ()
  "Return explicit context ownership, then buffer ownership or selection."
  (if (eq qq-runtime--context-account 'unscoped)
      (or qq-runtime--account-id
          (and (fboundp 'qq-account-current-id) (qq-account-current-id)))
    qq-runtime--context-account))

(defun qq-runtime-require-account-id (&optional operation)
  "Return the account owning the current context, or reject OPERATION.

OPERATION is a short human-readable description used in the error message."
  (or (qq-runtime-current-account-id)
      (user-error "qq: %s requires an explicit QQ account"
                  (or operation "this operation"))))

(defun qq-runtime-account-display-name (account-id)
  "Return a concise display name for stable ACCOUNT-ID."
  (let* ((account
          (and (fboundp 'qq-account-get)
               (qq-account-get account-id)))
         (label (alist-get 'label account))
         (uin (alist-get 'uin account)))
    (cond
     ((and label uin) (format "%s/%s" label uin))
     (uin uin)
     (label (format "%s/%s" label account-id))
     (t account-id))))

(defun qq-runtime-account-buffer-name
    (kind &optional detail account-id)
  "Return an account-qualified buffer name for KIND and optional DETAIL.

ACCOUNT-ID defaults to the exact current account context."
  (let ((owner (or account-id
                   (qq-runtime-require-account-id "buffer creation"))))
    (format "*qq-%s:%s%s*"
            kind
            (qq-runtime-account-display-name owner)
            (if detail (format ":%s" detail) ""))))

(defun qq-runtime-account (account-id)
  "Return live account runtime for stable ACCOUNT-ID, or nil."
  (let ((runtime (and account-id
                      (gethash account-id qq-runtime--accounts))))
    (and (qq-runtime-account-p runtime)
         (appkit-app-live-p (qq-runtime-account-app runtime))
         runtime)))

(defun qq-runtime-accounts ()
  "Return every live account runtime."
  (seq-filter (lambda (runtime)
                (and (qq-runtime-account-p runtime)
                     (appkit-app-live-p (qq-runtime-account-app runtime))))
              (nreverse (hash-table-values qq-runtime--accounts))))

(defun qq-runtime--account-shutdown (app)
  "Retain APP's last immutable data snapshot and retire its execution."
  (let* ((id (appkit-app-identity app)) (runtime (gethash id qq-runtime--accounts)))
    (when (and runtime (eq app (qq-runtime-account-app runtime)))
      (puthash id
               (appkit-data-with (appkit-app-model app)
                 ((:store) :actions appkit-data-map-empty
                           :input-status-revisions appkit-data-map-empty
                           :pending-input-status appkit-data-map-empty))
               qq-runtime--snapshots)
      (remhash id qq-runtime--accounts))))

(defun qq-runtime-ensure-account (account-id)
  "Return ACCOUNT-ID's live account App, reopening retained data when needed."
  (or (qq-runtime-account account-id)
      (let* ((saved (gethash account-id qq-runtime--snapshots))
             (model (qq-state-partition--create
                     :store (if saved (qq-state-partition-store saved)
                              (qq-state-store-create :account-id account-id))))
             (app (appkit-app-start qq-runtime--account-type :identity account-id :input model))
             (runtime (qq-runtime-account--create :id account-id :app app)))
        (puthash account-id runtime qq-runtime--accounts)
        (remhash account-id qq-runtime--snapshots)
        runtime)))

(defun qq-runtime-app (&optional account-id)
  "Return the account Appkit application for ACCOUNT-ID or current context."
  (let ((owner
         (or account-id
             (qq-runtime-require-account-id "account application access"))))
    (qq-runtime-account-app (qq-runtime-ensure-account owner))))

(defun qq-runtime-bind-account (account-id)
  "Bind this buffer's UI ownership to ACCOUNT-ID."
  (setq-local qq-runtime--account-id account-id)
  (qq-runtime-ensure-account account-id))

(defun qq-runtime--renderer-call (surface account-id function &rest arguments)
  "Call renderer FUNCTION for SURFACE in its owning ACCOUNT-ID context."
  (with-current-buffer (appkit-surface-buffer surface)
    (let ((qq-media--consumer surface))
      (if account-id
          (qq-runtime-with-account account-id (apply function arguments))
        (apply function arguments)))))

(defun qq-runtime--renderer-create
    (surface account-id renderer-factory render-function recover-function)
  "Wrap a standard renderer with QQ account and media ownership."
  (let* ((renderer
          (if renderer-factory
              (funcall renderer-factory surface)
            (appkit-generated-renderer-create
             :merge #'appkit-projection-change-merge
             :resource-request (lambda (keys)
                                 (appkit-projection-change-create :resources keys))
             :render (lambda (surface _app-view model change)
                       (funcall render-function surface model change))
             :recover (and recover-function
                           (lambda (surface _app-view model condition)
                             (funcall recover-function surface model condition))))))
         (mount (appkit-generated-renderer-mount renderer))
         (render (appkit-generated-renderer-render renderer))
         (recover (appkit-generated-renderer-recover renderer))
         (unmount (appkit-generated-renderer-unmount renderer)))
    (appkit-generated-renderer-create
     :mount
     (lambda (surface app-view model)
       (when account-id (qq-runtime-bind-account account-id))
       (when mount
         (qq-runtime--renderer-call surface account-id mount surface app-view model)))
     :merge (appkit-generated-renderer-merge renderer)
     :resource-request (appkit-generated-renderer-resource-request renderer)
     :render
     (lambda (surface app-view model change)
       (when (appkit-surface-live-p surface)
         (qq-runtime--renderer-call surface account-id render surface app-view model change)))
     :recover
     (and recover
          (lambda (surface app-view model condition)
            (qq-runtime--renderer-call surface account-id recover surface app-view model condition)))
     :unmount
     (lambda (surface)
       (unwind-protect
           (when unmount
             (qq-runtime--renderer-call surface account-id unmount surface))
         (when (eq qq-runtime--surface-owner surface)
           (setq-local qq-runtime--surface-owner nil)))))))

(cl-defun qq-runtime-open-surface
    (&key app id mode state (update #'qq-runtime--surface-update) render-function recover-function renderer-factory setup buffer buffer-name select account-id)
  "Open or select the exact canonical QQ Surface identified by APP and ID.
UPDATE handles committed Surface messages, defaulting to shared presentation.
RECOVER-FUNCTION, when supplied, rebuilds presentation from canonical state;
it receives the Surface, committed model, and rendering condition.
RENDERER-FACTORY, when supplied, builds a standard Appkit renderer instead
of the legacy RENDER-FUNCTION and RECOVER-FUNCTION callbacks."
  (let ((existing (appkit-app-surface app id)))
    (if (appkit-surface-live-p existing)
        (progn
          (when (and buffer (not (eq buffer (appkit-surface-buffer existing))))
            (error "qq: Surface identity already belongs to another buffer"))
          (when select (pop-to-buffer (appkit-surface-buffer existing)))
          existing)
      (let ((surface
             (appkit-open-generated-surface
              (appkit-surface-type-create
               :name mode
               :mode (if buffer #'ignore mode)
               :init (lambda (_context _input)
                       (appkit-next :model (list :state state) :render appkit-render-none))
               :update update
               :subscriptions
               (lambda (_model)
                 (when account-id
                   (list
                    (appkit-subscription-create
                     :topic 'qq-data :delivery 'level
                     :notify (lambda (key) (list 'qq-data key)))
                    (appkit-subscription-create
                     :topic 'qq-history-state :delivery 'lossless :pending-limit 32
                     :notify (lambda (session-key) (list 'qq-history-changed session-key))
                     :resync (lambda ()
                               (list 'qq-render
                                     (appkit-projection-change-create
                                      :full-p t :frame-p t)))))))
               :renderer-factory
               (lambda (surface)
                 (qq-runtime--renderer-create
                  surface account-id renderer-factory render-function recover-function)))
              :app app
              :identity id
              :buffer buffer
              :buffer-name buffer-name)))
        (condition-case error-data
            (progn
              (with-current-buffer (appkit-surface-buffer surface)
                (setq-local qq-runtime--surface-owner surface)
                (when setup
                  (if account-id
                      (qq-runtime-with-account account-id (funcall setup surface))
                    (funcall setup surface))))
              (qq-runtime-invalidate surface :full-p t)
              (when select (pop-to-buffer (appkit-surface-buffer surface)))
              surface)
          ((error quit)
           (appkit-surface-stop surface)
           (signal (car error-data) (cdr error-data))))))))

(defun qq-runtime-open-account-surface (&rest options)
  "Open an account Surface using `qq-runtime-open-surface' OPTIONS."
  (let ((owner (or (plist-get options :account-id)
                   (qq-runtime-require-account-id "opening a Surface"))))
    (apply #'qq-runtime-open-surface
           :app (qq-runtime-app owner) :account-id owner options)))

(defun qq-runtime-ensure-account-surface (&rest options)
  "Attach this buffer to its account's canonical Surface using OPTIONS."
  (let* ((owner (or qq-runtime--account-id
                    (user-error "qq: buffer has no account owner")))
         (app (qq-runtime-app owner))
         (surface (appkit-current-surface)))
    (cond
     ((and (appkit-surface-live-p surface)
           (eq (appkit-surface-app surface) app)
           (equal (appkit-surface-identity surface) (plist-get options :id))) surface)
     ((appkit-surface-live-p surface)
      (error "qq: buffer belongs to another Surface"))
     (t (apply #'qq-runtime-open-surface
               :app app :account-id owner :buffer (current-buffer) options)))))

(defun qq-runtime-stop-account (account-id &optional drop-state)
  "Stop ACCOUNT-ID's App; DROP-STATE also forgets its retained snapshot."
  (when-let* ((runtime (gethash account-id qq-runtime--accounts)))
    (appkit-app-close (qq-runtime-account-app runtime)))
  (remhash account-id qq-runtime--accounts)
  (when drop-state
    (remhash account-id qq-runtime--snapshots))
  t)

(defun qq-runtime-stop ()
  "Stop every account UI and the Gateway-wide Appkit application."
  (dolist (runtime (qq-runtime-accounts))
    (appkit-app-close (qq-runtime-account-app runtime)))
  (clrhash qq-runtime--accounts)
  (when (appkit-app-p qq-runtime--app)
    (appkit-app-close qq-runtime--app))
  (setq qq-runtime--app nil))

(defvar qq-runtime--operations (make-hash-table :test #'eq)
  "Account operations, run by `appkit-operation-update' in each account App.")

(defmacro qq-runtime-define-operation (name arguments &rest options)
  "Define NAME over explicit account snapshots.
Bodies read STATE and OWNER.  Reducers thread the local STATE; Appkit commits
it together with operation status.  Gateway instance changes reject settlement."
  (declare (indent 2))
  (cl-labels
      ((scope (body)
         `(let ((state (qq-state-partition-store model))
                (owner (qq-state-partition-account-id model)))
            (ignore state owner)
            ,body))
       (reduce (body gateway-p)
         (scope
          `(let ((value ,body))
             (cons (appkit-data-with model (() :store state))
                   ,(if gateway-p `(cons (qq-server-gateway-instance-id) value) 'value))))))
    `(appkit-define-operation qq-runtime--operations ,name ,arguments
       ,@(cl-loop for (option body) on options by #'cddr
                  unless (memq option '(:method :params :prepare :commit))
                  append (list option (if (memq option '(:steps :invalidate :cancel :next))
                                          (scope body) body)))
       :fetch ,(scope
                `(qq-runtime--rpc-fetch owner ,(plist-get options :method)
                                        ,(plist-get options :params) resolve reject))
       :prepare ,(reduce (plist-get options :prepare) t)
       :commit ,(reduce
                 `(let ((observation (qq-runtime--current-observation observation)))
                    (ignore observation)
                    ,(or (plist-get options :commit) 'result)) nil))))

(defun qq-runtime--rpc-fetch (owner method params resolve reject)
  "Call METHOD for explicit OWNER and return its transport canceller."
  (let ((transport (qq-rpc-call method (cons (cons 'account_id owner) params)
                                :callback resolve :errback reject)))
    (lambda () (qq-server-cancel transport))))

(defun qq-runtime--current-observation (observation)
  "Return OBSERVATION's own part, or signal if the Gateway instance changed."
  (unless (equal (car observation) (qq-server-gateway-instance-id))
    (error "QQ Gateway changed during request"))
  (cdr observation))

(defun qq-runtime--operations-of (owner)
  "Return OWNER's committed operation entries."
  (if-let* ((runtime (qq-runtime-account owner)))
      (qq-state-partition-operations (appkit-app-model (qq-runtime-account-app runtime)))
    appkit-data-map-empty))

(defun qq-runtime-operation (key &optional account-id)
  "Read KEY from ACCOUNT-ID's committed operation snapshot."
  (appkit-data-map-get
   (qq-state-partition-operations (qq-runtime-model account-id)) key))

(defun qq-runtime-request-create (name &rest input)
  "Create operation NAME's immutable request from named INPUT fields."
  (apply #'appkit-operation-request-create qq-runtime--operations name input))

(defun qq-runtime-request (request &optional refresh reply-token)
  "Submit REQUEST to the account App; see `appkit-operation-submit'.
REPLY-TOKEN routes a completion message back to the current Surface."
  (appkit-operation-submit
   qq-runtime--operations (qq-runtime-model (qq-runtime-require-account-id))
   (if reply-token
       (or (appkit-current-surface) (user-error "qq: reply requires a live Surface"))
     (qq-runtime-app))
   request refresh reply-token))

(defun qq-runtime--operation-update (context model message)
  "Reduce operations and domain state as one account model."
  (appkit-operation-update qq-runtime--operations model context message :topic 'qq-data))

(defun qq-runtime-forget (prefix &optional account-id)
  "Revoke cached and pending operation keys starting with PREFIX for ACCOUNT-ID."
  (when-let* ((runtime (qq-runtime-account (or account-id (qq-runtime-current-account-id)))))
    (appkit-app-send (qq-runtime-account-app runtime) (list 'appkit-operation-forget prefix))))

(defvar-local qq-runtime-data-hook nil
  "Surface-local presentation listeners called with a changed App data key.")

(defun qq-runtime--account-reduce (context model message)
  "Reduce one command or transport settlement against the account MODEL."
  (or
   (and (fboundp 'qq-guild-app-update) (qq-guild-app-update context model message))
   (qq-runtime--operation-update context model message)
   (qq-timeline-app-update context model message)
   (pcase message
     (`(qq-state-apply ,reducer ,arguments)
      (condition-case condition
          (let ((result (apply reducer (qq-state-partition-store model) arguments)))
            (appkit-next :model (appkit-data-with model (() :store (car result)))
                         :render appkit-render-none :value (cdr result)))
        (error (appkit-next-reject condition))))
     (`(qq-history-fetch ,input)
      (appkit-next
       :model model :render appkit-render-none
       :commands (list (appkit-command-start-effect
                        (qq-runtime--history-effect
                         (qq-state-partition-account-id model) input
                         (appkit-transition-context-reply-route context))))))
     (`(qq-history-fetched ,effect-input ,raw ,response ,reason)
      (let ((input (plist-get effect-input :input)) meta)
        (unless reason
          (condition-case condition
              (let ((result (qq-message--commit-history-result
                             (qq-state-partition-store model) raw
                             (qq-state-partition-account-id model))))
                (setq model (appkit-data-with model (() :store (car result))) meta (cdr result)))
            (error (setq reason (error-message-string condition)))))
        (appkit-next
         :model model :render appkit-render-none :value meta
         :commands
         (append
          (when meta (list (appkit-command-publish 'qq-history-state (plist-get input :session-key))))
          (list (appkit-command-post-message
                 :target (plist-get effect-input :route) :delivery 'report
                 :message (list 'qq-history-result input meta response reason)))))))
     (_ (appkit-next :model model :render appkit-render-none)))))

(defun qq-runtime-reset-state ()
  "Forget all account snapshots after revoking their Apps."
  (dolist (owner (qq-runtime-account-ids))
    (qq-runtime-stop-account owner t)
    (run-hook-with-args 'qq-runtime-change-hook
                        (list :type 'reset :account-id owner :state (qq-state-store-create :account-id owner)))))

(provide 'qq-runtime)

;;; qq-runtime.el ends here
