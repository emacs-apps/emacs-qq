;;; qq-login.el --- Native QQ authorization interaction -*- lexical-binding: t; -*-

;; Author: 0WD0 <me@0wd0.com>
;; Keywords: comm

;;; Commentary:

;; Foreground authorization controller for one managed QQ account.  Gateway
;; owns every account runtime; this file only turns the selected account's
;; projected phase/challenge into a serialized Emacs interaction.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'url-parse)
(require 'url-util)
(require 'browser-session)
(require 'qq-customize)
(require 'qq-account)
(require 'qq-rpc)
(require 'qq-server)
(require 'qq-core)

(cl-defstruct (qq-login--captcha-capture
               (:constructor qq-login--captcha-capture-create))
  "One browser-owned QQ captcha interaction."
  challenge-id
  account-id
  url
  sid
  directory
  output-file
  request)

(cl-defstruct (qq-login--session
               (:constructor qq-login--session-create))
  "One foreground authorization interaction."
  active-p
  account-id
  create-p
  label
  label-read-p
  managed-accounts-loaded-p
  login-accounts-loaded-p
  login-accounts
  quick-login-uin
  quick-login-suppressed-p
  in-flight-p
  prompting-p
  retry-failed-p
  handled-challenge-id
  captcha-capture
  status
  error
  qr-url
  qr-display
  qr-file
  timer)

(defvar qq-login--current nil
  "Current foreground authorization interaction, or nil.")

(defvar qq-login-change-hook nil
  "Hook run when the foreground login presentation changes.")

(defun qq-login-active-p ()
  "Return non-nil when an interactive login flow is active."
  (and (qq-login--session-p qq-login--current)
       (qq-login--session-active-p qq-login--current)))

(defun qq-login--current-p (session)
  "Return non-nil when SESSION still owns foreground authorization."
  (and (eq session qq-login--current)
       (qq-login--session-active-p session)))

(defun qq-login--delete-captcha-directory (capture)
  "Delete private files owned by CAPTCHA CAPTURE."
  (when-let* ((directory (qq-login--captcha-capture-directory capture)))
    (setf (qq-login--captcha-capture-directory capture) nil)
    (when (file-directory-p directory)
      (ignore-errors (delete-directory directory t)))))

(defun qq-login--cancel-captcha-capture (session)
  "Cancel and detach SESSION's active browser captcha capture."
  (when-let* ((capture (qq-login--session-captcha-capture session)))
    (setf (qq-login--session-captcha-capture session) nil)
    (if-let* ((request (qq-login--captcha-capture-request capture)))
        (condition-case nil
            (browser-session-cancel request)
          (error
           (message "qq: could not cancel the captcha browser")))
      (qq-login--delete-captcha-directory capture))))

(defun qq-login--cancel-timer (session)
  "Cancel SESSION's pending progression timer."
  (when-let* ((timer (qq-login--session-timer session)))
    (setf (qq-login--session-timer session) nil)
    (cancel-timer timer)))

(defun qq-login--delete-qr-file (session)
  "Delete SESSION's generated QR image, if any."
  (when-let* ((file (qq-login--session-qr-file session)))
    (setf (qq-login--session-qr-file session) nil)
    (when (file-exists-p file)
      (ignore-errors (delete-file file)))))

(defun qq-login--clear-qr (session)
  "Clear SESSION's QR presentation."
  (qq-login--delete-qr-file session)
  (setf (qq-login--session-qr-url session) nil
        (qq-login--session-qr-display session) nil))

(defun qq-login--changed ()
  "Publish a foreground login presentation change."
  (run-hooks 'qq-login-change-hook))

(defun qq-login--present (session status error-text)
  "Present STATUS and ERROR-TEXT for current SESSION."
  (when (qq-login--current-p session)
    (unless (and (equal status (qq-login--session-status session))
                 (equal error-text (qq-login--session-error session)))
      (setf (qq-login--session-status session) status
            (qq-login--session-error session) error-text)
      (qq-login--changed))))

(defun qq-login--finish (session &optional message-text)
  "Finish SESSION and optionally report MESSAGE-TEXT."
  (when (qq-login--current-p session)
    (qq-login--cancel-timer session)
    (qq-login--cancel-captcha-capture session)
    (qq-login--clear-qr session)
    (setf (qq-login--session-active-p session) nil
          (qq-login--session-in-flight-p session) nil
          (qq-login--session-prompting-p session) nil)
    (setq qq-login--current nil)
    (qq-login--changed)
    (when message-text
      (message "%s" message-text)))
  nil)

;;;###autoload
(defun qq-login-cancel ()
  "Cancel the foreground login interaction without stopping its QQ runtime."
  (interactive)
  (if (qq-login-active-p)
      (qq-login--finish
       qq-login--current
       "qq: login interaction cancelled; account runtime left unchanged")
    (when (called-interactively-p 'interactive)
      (message "qq: no login interaction is active"))))

(defun qq-login--timer-fire (session)
  "Progress SESSION outside the Gateway event callback."
  (when (qq-login--session-p session)
    (setf (qq-login--session-timer session) nil))
  (when (qq-login--current-p session)
    (qq-login--drive session)))

(defun qq-login--schedule (session)
  "Schedule one progression pass for current SESSION."
  (when (and (qq-login--current-p session)
             (not (timerp (qq-login--session-timer session))))
    (setf (qq-login--session-timer session)
          (run-at-time 0 nil #'qq-login--timer-fire session))))

(defun qq-login--request-error (session body reason)
  "Pause SESSION after request failure BODY described by REASON."
  (when (qq-login--current-p session)
    (setf (qq-login--session-in-flight-p session) nil)
    (let* ((code (alist-get 'code body))
           (error-text
            (format "Login failed%s: %s"
                    (if (qq-protocol-non-empty-string-p code)
                        (format " [%s]" code)
                      "")
                    (or reason "native request failed"))))
      (qq-login--present session "Login paused." error-text)
      (message "qq: %s" error-text))))

(defun qq-login--request-success (session _snapshot)
  "Continue SESSION after a lifecycle request returned a snapshot."
  (when (qq-login--current-p session)
    (setf (qq-login--session-in-flight-p session) nil)
    (qq-login--present session "Processing QQ login state…" nil)
    (qq-login--schedule session)))

(defun qq-login--create-success (session snapshot)
  "Bind SESSION to newly created account SNAPSHOT and continue."
  (when (qq-login--current-p session)
    (let ((account-id (alist-get 'account_id snapshot)))
      (setf (qq-login--session-account-id session)
            (copy-sequence account-id)
            (qq-login--session-create-p session) nil
            (qq-login--session-in-flight-p session) nil)
      (qq-account--set-current-account account-id)
      (qq-login--present session "Managed account created." nil)
      (qq-login--schedule session))))

(defun qq-login--request (session starter &optional success failure)
  "Run asynchronous lifecycle STARTER for SESSION.

SUCCESS defaults to `qq-login--request-success'.  FAILURE defaults to
`qq-login--request-error'."
  (setf (qq-login--session-in-flight-p session) t)
  (qq-login--changed)
  (condition-case error-data
      (funcall starter
               (apply-partially
                (or success #'qq-login--request-success) session)
               (apply-partially
                (or failure #'qq-login--request-error) session))
    (error
     (setf (qq-login--session-in-flight-p session) nil)
     (funcall
      (or failure #'qq-login--request-error)
      session nil
      (format "could not start native request: %s"
              (error-message-string error-data))))))

(defun qq-login--read-label ()
  "Read an optional label for a newly managed account."
  (let ((label (string-trim (read-string "Account label (optional): "))))
    (and (not (string-empty-p label)) label)))

(defun qq-login--quick-login-available-p ()
  "Return non-nil when the Gateway exposes the complete EasyLogin API."
  (and (qq-rpc-method-available-p "account.login.list")
       (qq-rpc-method-available-p "account.login.quick")))

(defun qq-login--managed-account-list-success (session _accounts)
  "Continue SESSION after its authoritative managed-account refresh."
  (when (qq-login--current-p session)
    (setf (qq-login--session-in-flight-p session) nil
          (qq-login--session-managed-accounts-loaded-p session) t)
    (qq-login--present session "Preparing account choices…" nil)
    (qq-login--schedule session)))

(defun qq-login--request-managed-accounts (session)
  "Refresh the authoritative managed-account catalog for SESSION."
  (qq-login--present session "Loading managed QQ accounts…" nil)
  (qq-login--request
   session
   (lambda (success failure)
     (qq-account-refresh-accounts success failure 'login))
   #'qq-login--managed-account-list-success))

(defun qq-login--active-runtime-p (account)
  "Return non-nil when managed ACCOUNT already has an active runtime."
  (member (alist-get 'phase account)
          '("online" "starting" "logging_in" "stopping")))

(defun qq-login--quick-account-list-success (session accounts)
  "Install EasyLogin ACCOUNTS for current SESSION and schedule its chooser."
  (when (qq-login--current-p session)
    (setf (qq-login--session-in-flight-p session) nil
          (qq-login--session-login-accounts-loaded-p session) t
          (qq-login--session-login-accounts session)
          (copy-tree accounts))
    (qq-login--select-quick-login-for-bound-account session)
    (qq-login--present session "Choose a QQ account." nil)
    (qq-login--schedule session)))

(defun qq-login--quick-account-list-error (session body reason)
  "Continue SESSION without EasyLogin candidates after BODY and REASON.

The managed-account selector is the login entry point, not an optional
credential-store feature.  A failed EasyLogin catalog therefore removes only
that candidate source; managed accounts and new-account login remain usable."
  (when (qq-login--current-p session)
    (let ((code (alist-get 'code body)))
      (setf (qq-login--session-in-flight-p session) nil
            (qq-login--session-login-accounts-loaded-p session) t
            (qq-login--session-login-accounts session) nil)
      (message "qq: EasyLogin accounts unavailable%s: %s"
               (if (qq-protocol-non-empty-string-p code)
                   (format " [%s]" code)
                 "")
               (or reason "native request failed"))
      (qq-login--present session "Choose a QQ account." nil)
      (qq-login--schedule session))))

(defun qq-login--request-quick-accounts (session)
  "Request completion-safe EasyLogin identities for SESSION."
  (qq-login--present session "Loading quick-login accounts…" nil)
  (qq-login--request
   session
   (lambda (success failure)
     (qq-account-login-list success failure))
   #'qq-login--quick-account-list-success
   #'qq-login--quick-account-list-error))

(defun qq-login--managed-account-title (account)
  "Return the compact user-facing identity for managed ACCOUNT."
  (let ((label (alist-get 'label account))
        (uin (alist-get 'uin account)))
    (format "%s%s"
            (or label uin "Unbound account")
            (if (and label uin) (format " (%s)" uin) ""))))

(defun qq-login--phase-label (phase)
  "Return a human-readable completion label for account PHASE."
  (capitalize (string-replace "_" " " phase)))

(defun qq-login--quick-account-label (quick-account &optional managed-account)
  "Return the completion label for QUICK-ACCOUNT.

When MANAGED-ACCOUNT already represents the same QQ identity, include its
user-facing label."
  (format "%s — Quick login"
          (if managed-account
              (qq-login--managed-account-title managed-account)
            (alist-get 'uin quick-account))))

(defun qq-login--managed-account-label (account)
  "Return a lifecycle-aware completion label for managed ACCOUNT."
  (format "%s — %s"
          (qq-login--managed-account-title account)
          (qq-login--phase-label (alist-get 'phase account))))

(defun qq-login--quick-account-for-managed (account quick-accounts)
  "Return the EasyLogin identity matching managed ACCOUNT."
  (when-let* ((uin (alist-get 'uin account)))
    (seq-find
     (lambda (quick-account)
       (equal uin (alist-get 'uin quick-account)))
     quick-accounts)))

(defun qq-login--select-quick-login-for-bound-account (session)
  "Prefer SESSION's Device-Profile-bound Login Record when available.

This handles callers which selected a stable Account ID directly and therefore
did not pass through the unified account chooser.  A rejected EasyLogin round
suppresses automatic reselection so the same session can fall back to Password."
  (when (and (qq-login--session-account-id session)
             (not (qq-login--session-quick-login-suppressed-p session))
             (null (qq-login--session-quick-login-uin session)))
    (when-let* ((account
                 (qq-account-get (qq-login--session-account-id session)))
                (quick-account
                 (qq-login--quick-account-for-managed
                  account (qq-login--session-login-accounts session))))
      (setf (qq-login--session-quick-login-uin session)
            (copy-sequence (alist-get 'uin quick-account))))))

(defun qq-login--uniquify-account-choices (choices)
  "Return CHOICES with exact identities appended only to duplicate labels."
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (choice choices)
      (puthash (car choice)
               (1+ (gethash (car choice) counts 0))
               counts))
    (mapcar
     (lambda (choice)
       (if (= (gethash (car choice) counts) 1)
           choice
         (let ((identity
                (pcase (cdr choice)
                  (`(managed . ,account) (alist-get 'account_id account))
                  (`(quick . ,account) (alist-get 'uin account))
                  (`(new) "new"))))
           (cons (format "%s [%s]" (car choice) identity)
                 (cdr choice)))))
     choices)))

(defun qq-login--account-choices (session)
  "Return unified managed-account and EasyLogin choices for SESSION."
  (let ((quick-accounts (qq-login--session-login-accounts session))
        managed-choices
        quick-choices
        represented-uins)
    (dolist (account (qq-account-list))
      (let ((quick-account
             (qq-login--quick-account-for-managed account quick-accounts)))
        (when quick-account
          (push (alist-get 'uin quick-account) represented-uins))
        (if (and quick-account
                 (not (qq-login--active-runtime-p account)))
            (push
             (cons (qq-login--quick-account-label quick-account account)
                   (cons 'quick quick-account))
             managed-choices)
          (push
           (cons (qq-login--managed-account-label account)
                 (cons 'managed account))
           managed-choices))))
    (dolist (quick-account quick-accounts)
      (unless (member (alist-get 'uin quick-account) represented-uins)
        (push
         (cons (qq-login--quick-account-label quick-account)
               (cons 'quick quick-account))
         quick-choices)))
    (qq-login--uniquify-account-choices
     (append (nreverse managed-choices)
             (nreverse quick-choices)
             '(("Add QQ account" . (new)))))))

(defun qq-login--matching-managed-account (quick-account)
  "Return a managed slot already bound to QUICK-ACCOUNT, or nil."
  (let* ((uin (alist-get 'uin quick-account))
         (uid (alist-get 'uid quick-account))
         (current (qq-account-current))
         (accounts (qq-account-list))
         (bound
          (cl-remove-if-not
           (lambda (account)
             (equal (alist-get 'uin account) uin))
           accounts)))
    (dolist (account bound)
      (when (and (alist-get 'uid account)
                 (not (equal (alist-get 'uid account) uid)))
        (error "qq: stored quick-login identity contradicts managed UIN %s"
               uin)))
    (or
     (and current
          (equal (alist-get 'uin current) uin)
          current)
     (car bound)
     ;; An explicitly selected unbound slot is safe to bind.  This matters
     ;; after `account.start' has already brought it to `login_required'.
     (and current
          (null (alist-get 'uin current))
          (null (alist-get 'uid current))
          current))))

(defun qq-login--read-account-choice (session)
  "Read and install one managed, EasyLogin, or new-account choice for SESSION."
  (let* ((choices (qq-login--account-choices session))
         choice)
    (setf (qq-login--session-prompting-p session) t)
    (unwind-protect
        (setq choice
              (cdr
               (assoc
                (completing-read "QQ login: " choices nil t)
                choices)))
      (when (qq-login--session-p session)
        (setf (qq-login--session-prompting-p session) nil)))
    (pcase choice
      (`(managed . ,account)
       (let ((account-id (alist-get 'account_id account)))
         (setf (qq-login--session-account-id session)
               (copy-sequence account-id)
               (qq-login--session-create-p session) nil
               (qq-login--session-quick-login-uin session) nil)
         (qq-account--set-current-account account-id)))
      (`(quick . ,account)
       (let ((managed (qq-login--matching-managed-account account)))
         (setf (qq-login--session-quick-login-uin session)
               (copy-sequence (alist-get 'uin account))
               (qq-login--session-account-id session)
               (and managed
                    (copy-sequence (alist-get 'account_id managed)))
               (qq-login--session-create-p session) (null managed)
               (qq-login--session-quick-login-suppressed-p session) nil
               ;; A quick-login identity already provides the useful label.
               (qq-login--session-label session) nil
               (qq-login--session-label-read-p session) t)
         (when managed
           (qq-account--set-current-account
            (alist-get 'account_id managed)))))
      (`(new)
       (setf (qq-login--session-account-id session) nil
             (qq-login--session-create-p session) t
             (qq-login--session-label session) nil
             (qq-login--session-label-read-p session) nil
             (qq-login--session-quick-login-uin session) nil))
      (_ (error "qq: invalid login account choice")))
    t))

(defun qq-login--resolve-account-choice (session)
  "Resolve SESSION's account source before driving its lifecycle.

Return non-nil once SESSION names a managed slot or requests creation.  A nil
return means an asynchronous account-catalog request is still pending."
  (cond
   ((and (qq-login--session-account-id session)
         (not (qq-login--session-login-accounts-loaded-p session))
         (qq-login--quick-login-available-p))
    (qq-login--request-quick-accounts session)
    nil)
   ((or (qq-login--session-account-id session)
        (qq-login--session-create-p session))
    t)
   ((not (qq-login--session-managed-accounts-loaded-p session))
    ;; `gateway.ready' seeds the projection and account events maintain it,
    ;; but an interactive selector is a consistency boundary of its own.
    ;; Refresh once so a client that reconnected late cannot hide accounts
    ;; created or changed by another Gateway client.
    (qq-login--request-managed-accounts session)
    nil)
   ((not (qq-login--session-login-accounts-loaded-p session))
    (if (qq-login--quick-login-available-p)
        (progn
          (qq-login--request-quick-accounts session)
          nil)
      ;; Credential storage contributes EasyLogin identities, but it never
      ;; owns the selector itself.  An empty optional source is now a known
      ;; result, so the same pass can present managed accounts plus the
      ;; new-account action.
      (setf (qq-login--session-login-accounts-loaded-p session) t
            (qq-login--session-login-accounts session) nil)
      (qq-login--read-account-choice session)))
   (t
    (qq-login--read-account-choice session))))

(defun qq-login--ensure-account (session)
  "Resolve or create SESSION's account.

Return its current snapshot, or nil while account creation is in flight."
  (let ((account-id (qq-login--session-account-id session)))
    (cond
     (account-id
      (or (qq-account-get account-id)
          (user-error "qq: QQ account does not exist: %s" account-id)))
     ((or (qq-login--session-create-p session)
          (null (qq-account-list)))
      (unless (qq-login--session-in-flight-p session)
        (unless (qq-login--session-label-read-p session)
          (setf (qq-login--session-label session) (qq-login--read-label)
                (qq-login--session-label-read-p session) t))
        (qq-login--present session "Creating managed QQ account…" nil)
        (qq-login--request
         session
         (lambda (success failure)
           (qq-account-create
            (qq-login--session-label session) success failure))
         #'qq-login--create-success))
      nil)
     (t
      (let ((selected
             (or (qq-account-current-id)
                 (and (= (length (qq-account-list)) 1)
                      (alist-get 'account_id (car (qq-account-list))))
                 (qq-account--read-account-id "Login account: "))))
        (setf (qq-login--session-account-id session)
              (copy-sequence selected))
        (qq-account--set-current-account selected)
        (qq-account-get selected))))))

(defconst qq-login--captcha-path
  "/safe/tools/captcha/sms-verify-login"
  "Only evidenced QQ ProofWater page path accepted by the browser adapter.")

(defconst qq-login--captcha-script
  (mapconcat
   #'identity
   '("(() => {"
     "  'use strict';"
     "  const stateKey = '__EMACS_QQ_CAPTCHA_CAPTURE_V1__';"
     "  let state = window[stateKey];"
     "  if (!state) {"
     "    state = { installed: false, proof: null };"
     "    Object.defineProperty(window, stateKey, {"
     "      value: state,"
     "      enumerable: false,"
     "      configurable: false,"
     "      writable: false"
     "    });"
     "  }"
     "  if (state.proof !== null) return state.proof;"
     "  if (state.installed) return null;"
     "  const original = window.captchaCallback;"
     "  if (typeof original !== 'function') return null;"
     "  window.captchaCallback = function (result) {"
     "    if (result !== null && typeof result === 'object' &&"
     "        result.ret === 0 &&"
     "        typeof result.ticket === 'string' &&"
     "        result.ticket.trim().length > 0 &&"
     "        !result.ticket.startsWith('terror_') &&"
     "        typeof result.randstr === 'string' &&"
     "        result.randstr.trim().length > 0) {"
     "      const sid = new URL(window.location.href).searchParams.get('sid');"
     "      if (sid) {"
     "        state.proof = {"
     "          ticket: result.ticket,"
     "          randstr: result.randstr,"
     "          sid: sid"
     "        };"
     "      }"
     "    }"
     "    return Reflect.apply(original, this, arguments);"
     "  };"
     "  state.installed = true;"
     "  return null;"
     "})();")
   "\n")
  "Exact provider expression for the evidenced QQ ProofWater callback.")

(defun qq-login--captcha-url-sid (url)
  "Validate QQ captcha URL and return its non-empty sid query value."
  (let ((sid
         (condition-case nil
             (when (qq-protocol-non-empty-string-p url)
               (let* ((parsed (url-generic-parse-url url))
                      (filename (url-filename parsed))
                      (query-index (and filename (string-match "\\?" filename)))
                      (path (and filename
                                 (if query-index
                                     (substring filename 0 query-index)
                                   filename)))
                      (query (and query-index
                                  (substring filename (1+ query-index)))))
                 (when (and (equal (url-type parsed) "https")
                            (equal (downcase (or (url-host parsed) ""))
                                   "ti.qq.com")
                            (null (url-user parsed))
                            (memq (url-port parsed) '(nil 443))
                            (equal path qq-login--captcha-path)
                            query)
                   (cadr (assoc "sid" (url-parse-query-string query))))))
           (error nil))))
    (unless (qq-protocol-non-empty-string-p sid)
      (user-error "qq: unsupported or incomplete captcha URL"))
    sid))

(defun qq-login--captcha-capture-current-p (session capture)
  "Return non-nil when SESSION still owns CAPTCHA CAPTURE."
  (and (qq-login--current-p session)
       (eq capture (qq-login--session-captcha-capture session))))

(defun qq-login--captcha-proof (capture)
  "Read and validate the private proof owned by CAPTCHA CAPTURE."
  (let* ((document
          (browser-session-read
           (qq-login--captcha-capture-output-file capture)))
         (source (alist-get 'source document))
         (proof (browser-session-page document))
         (ticket (alist-get 'ticket proof))
         (rand-str (alist-get 'randstr proof))
         (sid (alist-get 'sid proof)))
    (unless (and (equal (alist-get 'url source)
                        (qq-login--captcha-capture-url capture))
                 (null (browser-session-cookies document))
                 (qq-server-wire-exact-object-keys-p
                  proof '(ticket randstr sid))
                 (qq-protocol-non-empty-string-p ticket)
                 (not (string-prefix-p "terror_" ticket))
                 (qq-protocol-non-empty-string-p rand-str)
                 (qq-protocol-non-empty-string-p sid)
                 (equal sid (qq-login--captcha-capture-sid capture)))
      (user-error "qq: browser returned an invalid captcha proof"))
    (list ticket rand-str sid)))

(defun qq-login--captcha-error (session capture error)
  "Pause SESSION after CAPTCHA CAPTURE failed with structured ERROR."
  (unwind-protect
      (when (qq-login--captcha-capture-current-p session capture)
        (setf (qq-login--session-captcha-capture session) nil)
        (let ((reason (browser-session-error-message error)))
          (qq-login--present session "CAPTCHA verification paused." reason)
          (message "qq: CAPTCHA verification paused: %s" reason)))
    (qq-login--delete-captcha-directory capture)))

(defun qq-login--captcha-success (session capture _metadata)
  "Submit the private proof produced for SESSION CAPTCHA CAPTURE."
  (if (not (qq-login--captcha-capture-current-p session capture))
      (qq-login--delete-captcha-directory capture)
    (condition-case error-data
        (pcase-let ((`(,ticket ,rand-str ,sid)
                     (qq-login--captcha-proof capture)))
          (setf (qq-login--session-captcha-capture session) nil
                (qq-login--session-retry-failed-p session) nil)
          (qq-login--delete-captcha-directory capture)
          (qq-login--present session "Submitting CAPTCHA proof…" nil)
          (unwind-protect
              (condition-case request-error
                  (qq-login--request
                   session
                   (lambda (success failure)
                     (qq-account-login-captcha
                      (qq-login--captcha-capture-account-id capture)
                      (qq-login--captcha-capture-challenge-id capture)
                      ticket rand-str sid success failure)))
                (error
                 (qq-login--request-error
                  session nil (error-message-string request-error))))
            (clear-string ticket)))
      (error
       (qq-login--captcha-error
        session capture
        `((message . ,(error-message-string error-data))))))))

(defun qq-login--password (session account)
  "Read credentials and begin password login for SESSION ACCOUNT."
  (setf (qq-login--session-prompting-p session) t)
  (unwind-protect
      (let* ((account-id (alist-get 'account_id account))
             (uin (read-string "QQ UIN: " (alist-get 'uin account)))
             (password (read-passwd "QQ password: ")))
        (unwind-protect
            (progn
              (setf (qq-login--session-retry-failed-p session) nil)
              (qq-login--present session "Signing in to QQ…" nil)
              (qq-login--request
               session
               (lambda (success failure)
                 (qq-account-login-password
                  account-id uin password nil success failure))))
          (clear-string password)))
    (when (qq-login--session-p session)
      (setf (qq-login--session-prompting-p session) nil))))

(defun qq-login--quick-error (session body reason)
  "Pause SESSION after EasyLogin failure BODY described by REASON."
  (when (qq-login--session-p session)
    ;; Continuing the same session retries this stable managed slot through
    ;; password login instead of repeatedly using a rejected stored record.
    (setf (qq-login--session-quick-login-uin session) nil
          (qq-login--session-quick-login-suppressed-p session) t))
  (qq-login--request-error session body reason))

(defun qq-login--quick (session account)
  "Begin EasyLogin for SESSION ACCOUNT using its selected stored identity."
  (let ((uin (qq-login--session-quick-login-uin session)))
    (unless uin
      (error "qq: quick login has no selected UIN"))
    (setf (qq-login--session-retry-failed-p session) nil)
    (qq-login--present session (format "Quick logging in QQ %s…" uin) nil)
    (qq-login--request
     session
     (lambda (success failure)
       (qq-account-login-quick
        (alist-get 'account_id account) uin nil success failure))
     nil
     #'qq-login--quick-error)))

(defun qq-login--captcha (session account challenge)
  "Start isolated browser verification for SESSION ACCOUNT and CHALLENGE."
  (let* ((account-id (alist-get 'account_id account))
         (challenge-id (alist-get 'challenge_id challenge))
         (url (alist-get 'url challenge))
         (sid (alist-get 'sid challenge))
         (url-sid (qq-login--captcha-url-sid url)))
    (dolist (value (list account-id challenge-id sid))
      (unless (qq-protocol-non-empty-string-p value)
        (user-error "qq: captcha challenge is incomplete")))
    (unless (equal sid url-sid)
      (user-error "qq: captcha challenge sid conflicts with its URL"))
    (qq-login--cancel-captcha-capture session)
    (let* ((directory (make-temp-file "emacs-qq-captcha-" t))
           (output-file (expand-file-name "capture.json" directory))
           (profile-directory (expand-file-name "profile" directory))
           (capture
            (qq-login--captcha-capture-create
             :challenge-id (copy-sequence challenge-id)
             :account-id (copy-sequence account-id)
             :url (copy-sequence url)
             :sid (copy-sequence sid)
             :directory directory
             :output-file output-file)))
      (setf (qq-login--session-retry-failed-p session) nil
            (qq-login--session-handled-challenge-id session)
            (copy-sequence challenge-id)
            (qq-login--session-captcha-capture session) capture)
      (qq-login--present
       session "Complete the QQ verification in the opened browser…" nil)
      (condition-case error-data
          (setf
           (qq-login--captcha-capture-request capture)
           (let ((browser-session-timeout qq-login-captcha-timeout))
             (browser-session-capture
              :url url
              :output-file output-file
              :profile-directory profile-directory
              :script qq-login--captcha-script
              :callback
              (lambda (metadata)
                (qq-login--captcha-success session capture metadata))
              :errorback
              (lambda (error)
                (qq-login--captcha-error session capture error)))))
        (error
         (when (eq capture (qq-login--session-captcha-capture session))
           (setf (qq-login--session-captcha-capture session) nil))
         (qq-login--delete-captcha-directory capture)
         (signal (car error-data) (cdr error-data)))))))

(defun qq-login--qrencode ()
  "Return the executable used for QR rendering, or signal a user error."
  (or (and (stringp qq-login-qrencode-program)
           (executable-find qq-login-qrencode-program))
      (user-error
       "qq: install qrencode or customize `qq-login-qrencode-program'")))

(defun qq-login--render-qr (url)
  "Return (DISPLAY . FILE) for the scannable QQ URL."
  (let ((program (qq-login--qrencode)))
    (if (and (display-graphic-p)
             (image-type-available-p 'png))
        (let ((file (make-temp-file "emacs-qq-login-qr-" nil ".png")))
          (unless (eq 0
                      (call-process
                       program nil nil nil "-m" "1" "-s" "8" "-t" "PNG"
                       "-o" file url))
            (delete-file file)
            (user-error "qq: qrencode could not render the login QR code"))
          (cons
           (propertize
            " "
            'display
            (create-image
             file 'png nil :ascent 'center
             :width qq-login-qr-image-size
             :height qq-login-qr-image-size))
           file))
      (with-temp-buffer
        (unless (eq 0
                    (call-process
                     program nil t nil "-m" "1" "-t" "UTF8" url))
          (user-error "qq: qrencode could not render the login QR code"))
        (cons (buffer-string) nil)))))

(defun qq-login--prepare-qr (session url)
  "Render URL and install its presentation into SESSION."
  (unless (equal url (qq-login--session-qr-url session))
    (pcase-let ((`(,display . ,file) (qq-login--render-qr url)))
      (qq-login--clear-qr session)
      (setf (qq-login--session-qr-url session) (copy-sequence url)
            (qq-login--session-qr-display session) display
            (qq-login--session-qr-file session) file)
      (qq-login--changed))))

(defun qq-login-view-model ()
  "Return the current foreground login presentation, or nil."
  (when (qq-login-active-p)
    (list
     :account-id (copy-sequence
                  (qq-login--session-account-id qq-login--current))
     :status (copy-sequence
              (or (qq-login--session-status qq-login--current)
                  "Preparing QQ login…"))
     :error (and (qq-login--session-error qq-login--current)
                 (copy-sequence
                  (qq-login--session-error qq-login--current)))
     :display (and (qq-login--session-qr-display qq-login--current)
                   (copy-sequence
                    (qq-login--session-qr-display qq-login--current))))))

(defun qq-login-insert-view (model)
  "Insert foreground login presentation MODEL into the current buffer."
  (insert
   (propertize
    (concat (or (plist-get model :status) "QQ login…") "\n")
    'face 'bold))
  (when-let* ((error-text (plist-get model :error)))
    (insert (propertize (concat error-text "\n") 'face 'error)))
  (when-let* ((display (plist-get model :display)))
    (insert display)
    (unless (bolp)
      (insert "\n"))
    (insert
     (propertize "Scan with mobile QQ when a QR is shown.\n"
                 'face 'bold)
     "Confirm the login on your phone when prompted.  "
     "The native service continues automatically.\n"))
  (when-let* ((account-id (plist-get model :account-id)))
    (insert (format "Managed account: %s\n" account-id))))

(defun qq-login--new-device (session account challenge)
  "Display Rust-owned new-device verification for SESSION ACCOUNT CHALLENGE."
  (ignore account)
  (let ((qr-url (alist-get 'qr_url challenge)))
    (unless (qq-protocol-non-empty-string-p qr-url)
      (error "qq: new-device challenge has no scannable qr_url"))
    (qq-login--prepare-qr session qr-url)
    (setf (qq-login--session-retry-failed-p session) nil)
    (qq-login--present session "Waiting for mobile QQ confirmation…" nil)))

(defun qq-login--unusual-device (session account challenge)
  "Display Rust-owned unusual-device confirmation for SESSION ACCOUNT CHALLENGE.

Gateway harvests checkSig, runs TransEmp31/12, and continues EasyLogin or
PasswordLogin UnusualDevice automatically.  Emacs only shows wait status and
an optional public QR URL."
  (ignore account)
  (let ((qr-url (alist-get 'qr_url challenge)))
    (when (qq-protocol-non-empty-string-p qr-url)
      (qq-login--prepare-qr session qr-url))
    (setf (qq-login--session-retry-failed-p session) nil)
    (qq-login--present
     session
     "Confirm this login on your phone QQ…"
     nil)))

(defun qq-login--challenge (session account challenge)
  "Continue SESSION ACCOUNT using projected CHALLENGE."
  (pcase (alist-get 'kind challenge)
    ("new_device" (qq-login--new-device session account challenge))
    ("unusual_device" (qq-login--unusual-device session account challenge))
    ("captcha"
     (if (equal (alist-get 'challenge_id challenge)
                (qq-login--session-handled-challenge-id session))
         (unless (qq-login--session-error session)
           (qq-login--present
            session
            (if (qq-login--session-captcha-capture session)
                "Complete the QQ verification in the opened browser…"
              "Waiting for QQ login to continue…")
            nil))
       (qq-login--captcha session account challenge)))
    (kind (error "qq: unsupported login challenge kind %S" kind))))

(defun qq-login--reconcile-captcha-capture (session phase challenge)
  "Cancel SESSION's capture unless PHASE and CHALLENGE still own it."
  (when-let* ((capture (qq-login--session-captcha-capture session)))
    (unless (and (equal phase "logging_in")
                 (equal (alist-get 'kind challenge) "captcha")
                 (equal (alist-get 'challenge_id challenge)
                        (qq-login--captcha-capture-challenge-id capture)))
      (qq-login--cancel-captcha-capture session))))

(defun qq-login--start-account (session account)
  "Start native runtime for SESSION ACCOUNT."
  (setf (qq-login--session-retry-failed-p session) nil)
  (qq-login--present session "Starting native QQ account…" nil)
  (qq-login--request
   session
   (lambda (success failure)
     (qq-account-start
      (alist-get 'account_id account) success failure))))

(defun qq-login--drive-ready (session)
  "Progress SESSION while the Gateway is ready."
  (when (qq-login--resolve-account-choice session)
    (when-let* ((account (qq-login--ensure-account session)))
      (let ((phase (alist-get 'phase account))
            (challenge (alist-get 'challenge account)))
        (qq-login--reconcile-captcha-capture session phase challenge)
        (when (and (qq-login--session-qr-display session)
                   (not (and (equal phase "logging_in")
                             (member (alist-get 'kind challenge)
                                     '("new_device" "unusual_device")))))
          (qq-login--clear-qr session)
          (qq-login--changed))
        (pcase phase
          ((or "stopped" "logged_out" "failed")
           (if (qq-login--session-retry-failed-p session)
               (qq-login--start-account session account)
             (let* ((problem (alist-get 'problem account))
                    (code (alist-get 'code problem))
                    (message-text (alist-get 'message problem)))
               (qq-login--present
                session
                "QQ login is not running."
                (cond
                 ((and code message-text)
                  (format "[%s] %s" code message-text))
                 (message-text message-text)
                 (t "Run `M-x qq` again to retry."))))))
          ("login_required"
           (if (qq-login--session-quick-login-uin session)
               (qq-login--quick session account)
             (qq-login--password session account)))
          ("logging_in"
           (if challenge
               (qq-login--challenge session account challenge)
             (qq-login--present
              session
              (if (qq-login--session-qr-display session)
                  "Waiting for mobile QQ confirmation…"
                "QQ login is continuing in the native service…")
              nil)))
          ("online"
           (qq-login--finish
            session
            (format "qq: account %s is online"
                    (or (alist-get 'uin account)
                        (alist-get 'account_id account)))))
          ("starting"
           (qq-login--present session "Starting native QQ account…" nil))
          ("stopping"
           (qq-login--present session "Stopping native QQ account…" nil))
          (_ (error "qq: unsupported account phase %S" phase)))))))

(defun qq-login--drive (session)
  "Progress current foreground authorization SESSION by one state."
  (when (and (qq-login--current-p session)
             (not (qq-login--session-in-flight-p session))
             (not (qq-login--session-prompting-p session))
             (qq-server-ready-p))
    (condition-case error-data
        (qq-login--drive-ready session)
      (quit
       (qq-login--finish
        session
        (format "qq: login interaction cancelled: %s"
                (error-message-string error-data))))
      (error
       (qq-login--request-error
        session nil (error-message-string error-data))))))

(defun qq-login--projection-changed (&rest _arguments)
  "Continue the current login after account projection changes."
  (when (qq-login-active-p)
    (qq-login--schedule qq-login--current)))

(defun qq-login--start (account-id create-p label label-read-p)
  "Start foreground login for ACCOUNT-ID or a new account.

CREATE-P requests a new managed slot with optional LABEL.  LABEL-READ-P means
the caller has already made the optional label choice, including choosing nil."
  (if (and (qq-login-active-p)
           (not create-p)
           (or (null account-id)
               (equal account-id
                      (qq-login--session-account-id qq-login--current))))
      (let ((session qq-login--current))
        (if (or (qq-login--session-in-flight-p session)
                (qq-login--session-captcha-capture session))
            (message "qq: %s"
                     (or (qq-login--session-status session)
                         "login request is still running"))
          (setf (qq-login--session-retry-failed-p session) t
                (qq-login--session-handled-challenge-id session) nil)
          (qq-login--present session "Continuing QQ login…" nil)
          (qq-login--schedule session))
        session)
    (when (qq-login-active-p)
      (qq-login-cancel))
    (let ((online-account
           (and account-id
                (not create-p)
                (qq-server-ready-p)
                (qq-account-get account-id))))
      (if (and online-account
               (equal (alist-get 'phase online-account) "online"))
          (progn
            (when account-id
              (qq-account--set-current-account account-id))
            (message "qq: account %s is already online"
                     (or (alist-get 'uin online-account)
                         (alist-get 'account_id online-account)))
            nil)
        (let ((session
               (qq-login--session-create
                :active-p t
                :account-id (and account-id (copy-sequence account-id))
                :create-p create-p
                :label label
                :label-read-p label-read-p
                :managed-accounts-loaded-p nil
                :login-accounts-loaded-p nil
                :login-accounts nil
                :quick-login-uin nil
                :quick-login-suppressed-p nil
                :retry-failed-p t
                :status "Preparing QQ login…")))
          (setq qq-login--current session)
          (qq-login--changed)
          (condition-case error-data
              (progn
                (unless (qq-core-running-p)
                  (qq-login--present
                   session "Connecting to native QQ service…" nil)
                  (qq-core-connect))
                (qq-login--schedule session)
                session)
            (error
             (qq-login--request-error
              session nil (error-message-string error-data))
             session)))))))

;;;###autoload
(defun qq-login (&optional account-id)
  "Log in or continue authorization for managed ACCOUNT-ID.

When ACCOUNT-ID is nil, continue the selected active runtime or offer managed
accounts, every Gateway EasyLogin identity, and `Add QQ account'.  An online
managed account is selected without logging in again.  An EasyLogin identity
reuses a matching managed slot or creates one as needed.  The command then
follows projected account phases until the account is online."
  (interactive)
  (qq-login--start account-id nil nil nil))

;;;###autoload
(defun qq-login-new-account (label)
  "Create a managed account with optional LABEL and run its login flow."
  (interactive (list (qq-login--read-label)))
  (qq-login--start nil t label t))

(add-hook 'qq-account-registry-ready-hook #'qq-login--projection-changed t)
(add-hook 'qq-account-registry-changed-hook #'qq-login--projection-changed t)

(provide 'qq-login)

;;; qq-login.el ends here
