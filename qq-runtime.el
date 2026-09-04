;;; qq-runtime.el --- Appkit session ownership for emacs-qq -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; Own one Gateway application plus one account-scoped Appkit application for
;; every QQ account projected by emacs-qq.  Appkit owns lifecycle and views;
;; `qq-state' owns the account-partitioned protocol state.

;;; Code:

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

(appkit-define-app-kind qq
  :shutdown #'qq-runtime--gateway-shutdown)

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
  "Return emacs-qq's live Gateway-wide Appkit application."
  (unless (appkit-app-live-p qq-runtime--app)
    (setq qq-runtime--app
          (appkit-app-start 'qq :id 'default)))
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
  "Remove the account runtime owned by APP without stopping the Gateway."
  (let ((account-id (appkit-app-id app)))
    (when-let* ((runtime (gethash account-id qq-runtime--accounts)))
      (when (eq app (qq-runtime-account-app runtime))
        (remhash account-id qq-runtime--accounts)))))

(appkit-define-app-kind qq-account
  :shutdown #'qq-runtime--account-shutdown)

(defun qq-runtime-ensure-account (account-id)
  "Return ACCOUNT-ID's live account runtime, creating it when needed."
  (or (qq-runtime-account account-id)
      (let* ((partition (qq-state-partition account-id))
             (app
              (appkit-app-start
               'qq-account
               :id (copy-sequence account-id)
               :state partition))
             (runtime
              (qq-runtime-account--create
               :id (copy-sequence account-id)
               :app app)))
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

(defun qq-runtime--account-sync (account-id function view invalidations events)
  "Run account sync FUNCTION for VIEW, INVALIDATIONS, and EVENTS under ACCOUNT-ID."
  (qq-runtime-with-account account-id
    (funcall function view invalidations events)))

(defun qq-runtime-account-sync-function (account-id function)
  "Return an Appkit sync wrapper for ACCOUNT-ID and FUNCTION."
  (apply-partially #'qq-runtime--account-sync account-id function))

(cl-defun qq-runtime-ensure-account-view
    (&key id mode state sync-function parts setup)
  "Return the account Appkit view owning the current buffer.

ID, MODE, STATE, SYNC-FUNCTION, and PARTS describe the required view.  Reuse
and refresh a matching live view, reject a foreign live view, or attach a new
one.  SETUP runs only after a new view is attached."
  (let* ((account-id
          (or qq-runtime--account-id
              (user-error "qq: buffer has no account owner")))
         (app (qq-runtime-app account-id))
         (wrapped-sync
          (qq-runtime-account-sync-function account-id sync-function))
         (current (appkit-current-view)))
    (cond
     ((and (appkit-view-live-p current)
           (eq app (appkit-view-app current))
           (equal id (appkit-view-id current)))
      (setf (appkit-view-state current) state
            (appkit-view-sync-function current) wrapped-sync
            (appkit-view-parts current) parts)
      current)
     ((appkit-view-live-p current)
      (error "QQ: buffer belongs to another Appkit view"))
     (t
      (let ((view
             (appkit-attach-view
              :app app
              :id id
              :mode mode
              :state state
              :sync-function wrapped-sync
              :parts parts)))
        (qq-runtime-bind-account account-id)
        (when setup
          (funcall setup view))
        view)))))

(cl-defun qq-runtime-open-account-view
    (&key account-id id mode buffer-name state sync-function parts
          position-policy setup select)
  "Open one ACCOUNT-ID-scoped Appkit view.

The buffer receives stable account context before application SETUP runs, and
SYNC-FUNCTION always observes that account's canonical state partition."
  (let* ((account-id (or account-id (qq-runtime-current-account-id)))
         (_ (unless account-id
              (user-error "qq: select a QQ account first")))
         (runtime (qq-runtime-ensure-account account-id))
         (app (qq-runtime-account-app runtime))
         (wrapped-setup
          (lambda (view)
            (qq-runtime-bind-account account-id)
            (when setup
              (funcall setup view))))
         (view
          (appkit-open-view
           :app app
           :id id
           :mode mode
           :buffer-name buffer-name
           :state state
           :sync-function
           (qq-runtime-account-sync-function account-id sync-function)
           :parts parts
           :position-policy position-policy
           :setup wrapped-setup
           :select select)))
    (with-current-buffer (appkit-view-buffer view)
      (qq-runtime-bind-account account-id))
    view))

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
