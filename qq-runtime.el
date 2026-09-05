;;; qq-runtime.el --- Appkit session ownership for emacs-qq -*- lexical-binding: t; -*-

;; Author: 0WD0 <me@0wd0.com>

;;; Commentary:

;; Own one Gateway application plus one account-scoped Appkit application for
;; every QQ account projected by emacs-qq.  Appkit owns lifecycle and views;
;; `qq-state' owns the account-partitioned protocol state.

;;; Code:

(require 'seq)
(require 'appkit-app)
(require 'appkit-surface)
(require 'appkit-projection)

(defconst qq-runtime--gateway-type
  (appkit-app-type-create
   :name 'qq
   :init (lambda (_context input) (appkit-next :model input :render appkit-render-none))
   :update (lambda (_context model _message)
             (appkit-next :model model :render appkit-render-none))
   :shutdown #'qq-runtime--gateway-shutdown))

(defconst qq-runtime--account-type
  (appkit-app-type-create
   :name 'qq-account
   :init (lambda (_context input) (appkit-next :model input :render appkit-render-none))
   :update (lambda (_context model _message)
             (appkit-next :model model :render appkit-render-none))
   :shutdown #'qq-runtime--account-shutdown))

(defun qq-runtime--surface-update (context model message)
  "Commit a QQ Surface MESSAGE, then request presentation or Effects."
  (pcase message
    (`(qq-render ,change)
     (appkit-next :model model :render change))
    (`(qq-state-event ,event)
     (appkit-next :model (plist-put (copy-sequence model) :events
                                    (append (plist-get model :events) (list event)))
                  :render (appkit-projection-change-create :frame-p t)
                  :commands
                  (list (appkit-command-post-message
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
        :render appkit-render-none)))
    (`(qq-media . ,_)
     (qq-media-update context model message))
    (_ (appkit-next :model model :render appkit-render-none))))

(defvar-local qq-runtime--surface-owner nil
  "Exact Surface whose teardown still owns this buffer's client work.")

(cl-defun qq-runtime-open-surface
    (&key app id mode state render-function setup buffer buffer-name select account-id)
  "Open or select the exact canonical QQ Surface identified by APP and ID."
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
               :update #'qq-runtime--surface-update
               :renderer-factory
               (lambda (_surface)
                 (appkit-generated-renderer-create
                  :mount (lambda (_surface _app-view _model)
                           (when account-id (qq-runtime-bind-account account-id)))
                  :merge #'appkit-projection-change-merge
                  :render (lambda (surface _app-view model change)
                            (when (appkit-surface-live-p surface)
                              (if account-id
                                  (qq-runtime-with-account account-id
                                    (funcall render-function surface model change))
                                (funcall render-function surface model change)))
                            nil)
                  :recover (lambda (surface _app-view model _request)
                             (if account-id
                                 (qq-runtime-with-account account-id
                                   (funcall render-function surface model
                                            (appkit-projection-change-create :full-p t)))
                               (funcall render-function surface model
                                        (appkit-projection-change-create :full-p t)))
                             nil)
                  :unmount (lambda (surface)
                             (when (eq qq-runtime--surface-owner surface)
                               (setq-local qq-runtime--surface-owner nil))))))
              :app app :identity id :buffer buffer :buffer-name buffer-name)))
        (condition-case error-data
            (progn
              (with-current-buffer (appkit-surface-buffer surface)
                (setq-local qq-runtime--surface-owner surface)
                (when setup
                  (if account-id
                      (qq-runtime-with-account account-id (funcall setup surface))
                    (funcall setup surface))))
              (appkit-surface-send surface
                                   (list 'qq-render
                                         (appkit-projection-change-create :full-p t)))
              (when select (pop-to-buffer (appkit-surface-buffer surface)))
              surface)
          ((error quit)
           (appkit-surface-stop surface)
           (signal (car error-data) (cdr error-data))))))))

(cl-defun qq-runtime-open-account-surface
    (&key account-id id mode state render-function setup buffer buffer-name select)
  "Open a generated Surface owned by the exact ACCOUNT-ID App."
  (let ((owner (or account-id (qq-runtime-require-account-id "opening a Surface"))))
    (qq-runtime-open-surface
     :app (qq-runtime-app owner) :account-id owner :id id :mode mode
     :state state :render-function render-function :setup setup
     :buffer buffer :buffer-name buffer-name :select select)))

(cl-defun qq-runtime-ensure-account-surface
    (&key id mode state render-function setup)
  "Attach this buffer to its account's canonical Surface, preserving buffer state."
  (let* ((owner (or qq-runtime--account-id
                    (user-error "qq: buffer has no account owner")))
         (app (qq-runtime-app owner))
         (surface (appkit-current-surface)))
    (cond
     ((and (appkit-surface-live-p surface)
           (eq (appkit-surface-app surface) app)
           (equal (appkit-surface-identity surface) id)) surface)
     ((appkit-surface-live-p surface)
      (error "qq: buffer belongs to another Surface"))
     (t (qq-runtime-open-surface
         :app app :account-id owner :id id :mode mode :state state
         :render-function render-function :setup setup :buffer (current-buffer))))))

(require 'cl-lib)
(require 'appkit-core)
(require 'qq-state)

(declare-function qq-core-disconnect "qq-core")
(declare-function qq-account-get "qq-account" (account-id))
(declare-function qq-account-current-id "qq-account")

(defun qq-runtime--gateway-shutdown (_app)
  "Stop transport resources owned by the Gateway Appkit application."
  (when (fboundp 'qq-core-disconnect)
    (qq-core-disconnect)))

(cl-defstruct (qq-runtime-account
               (:constructor qq-runtime-account--create))
  "One stable Gateway account's Appkit and canonical-state ownership."
  id
  app)

(defvar qq-runtime--accounts (make-hash-table :test #'equal)
  "Account runtimes keyed by stable Gateway account ID.")

(defvar-local qq-runtime--account-id nil
  "Stable Gateway account owning the current account-scoped buffer.")

(defvar qq-runtime--context-account-id nil
  "Dynamically scoped account for non-buffer account work.")

(defvar qq-runtime--app nil
  "Gateway-wide Appkit application used by global emacs-qq views.")

(defun qq-runtime-gateway-app ()
  "Return the live Gateway App."
  (unless (appkit-app-live-p qq-runtime--app)
    (setq qq-runtime--app
          (appkit-app-start qq-runtime--gateway-type :identity 'default)))
  qq-runtime--app)

(defun qq-runtime-current-account-id ()
  "Return the account owning the current execution context.

An explicit account operation wins over the current buffer's owner.  The
Gateway selection is only the final interactive fallback."
  (or (and qq-runtime--context-account-id
           (copy-sequence qq-runtime--context-account-id))
      (and qq-runtime--account-id
           (copy-sequence qq-runtime--account-id))
      (and (fboundp 'qq-account-current-id)
           (qq-account-current-id))))

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
  (let (runtimes)
    (maphash
     (lambda (_account-id runtime)
       (when (and (qq-runtime-account-p runtime)
                  (appkit-app-live-p (qq-runtime-account-app runtime)))
         (push runtime runtimes)))
     qq-runtime--accounts)
    (nreverse runtimes)))

(defun qq-runtime--account-shutdown (app)
  "Retire the exact account APP without stopping the Gateway."
  (let* ((id (appkit-app-identity app))
         (runtime (gethash id qq-runtime--accounts)))
    (when (and runtime (eq app (qq-runtime-account-app runtime)))
      (remhash id qq-runtime--accounts))))

(defun qq-runtime-ensure-account (account-id)
  "Return ACCOUNT-ID's exact live canonical App runtime."
  (or (qq-runtime-account account-id)
      (let* ((app (appkit-app-start qq-runtime--account-type
                                    :identity (copy-sequence account-id)
                                    :input (qq-state-partition account-id)))
             (runtime (qq-runtime-account--create
                       :id (copy-sequence account-id) :app app)))
        (puthash (copy-sequence account-id) runtime qq-runtime--accounts)
        runtime)))

(defun qq-runtime-app (&optional account-id)
  "Return the account Appkit application for ACCOUNT-ID or current context."
  (let ((owner
         (or account-id
             (qq-runtime-require-account-id "account application access"))))
    (qq-runtime-account-app (qq-runtime-ensure-account owner))))

(defun qq-runtime-bind-account (account-id)
  "Bind the current buffer to stable ACCOUNT-ID and return its runtime."
  (let ((runtime (qq-runtime-ensure-account account-id)))
    (setq-local qq-runtime--account-id (copy-sequence account-id))
    (add-hook 'pre-command-hook
              #'qq-runtime-enter-buffer-account-context nil t)
    (qq-state-select-account account-id)
    runtime))

(defun qq-runtime-enter-buffer-account-context ()
  "Install the state partition owned by the current account buffer."
  (when qq-runtime--account-id
    (qq-state-select-account qq-runtime--account-id)))

(defun qq-runtime-call-with-account (account-id function)
  "Call FUNCTION in ACCOUNT-ID's canonical state context."
  (qq-runtime-ensure-account account-id)
  (let ((qq-runtime--context-account-id (copy-sequence account-id)))
    (qq-state-call-with-account account-id function)))

(defmacro qq-runtime-with-account (account-id &rest body)
  "Evaluate BODY in ACCOUNT-ID's canonical state context."
  (declare (indent 1) (debug t))
  `(qq-runtime-call-with-account ,account-id (lambda () ,@body)))

(defun qq-runtime-stop-account (account-id &optional drop-state)
  "Stop ACCOUNT-ID's UI runtime.

When DROP-STATE is non-nil, also forget its canonical state partition.  This
does not stop or log out the Gateway-managed QQ runtime."
  (when-let* ((runtime (gethash account-id qq-runtime--accounts))
              (app (qq-runtime-account-app runtime)))
    (appkit-app-close app))
  (remhash account-id qq-runtime--accounts)
  (when drop-state
    (qq-state-drop-partition account-id))
  t)

(defun qq-runtime-stop ()
  "Stop every account UI and the Gateway-wide Appkit application."
  (dolist (runtime (qq-runtime-accounts))
    (appkit-app-close (qq-runtime-account-app runtime)))
  (clrhash qq-runtime--accounts)
  (when (appkit-app-p qq-runtime--app)
    (appkit-app-close qq-runtime--app))
  (setq qq-runtime--app nil))

(provide 'qq-runtime)

;;; qq-runtime.el ends here
