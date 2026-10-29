;;; qq-operation-test.el --- Account App operation regressions -*- lexical-binding: t; -*-
(require 'ert)
(require 'qq-runtime-test)
(require 'qq-core)

(defun qq-operation-test--drain ()
  "Drain all account loops, including queued Effect settlements."
  (let ((turns 0) pending)
    (while
        (progn
          (setq pending nil)
          (dolist (runtime (qq-runtime-accounts))
            (let ((app (qq-runtime-account-app runtime)))
              (dolist (loop
                       (cons (appkit-app-loop app)
                             (mapcar (lambda (entry) (appkit-surface-loop (cdr entry)))
                                     (hash-table-values (appkit-app-surfaces app)))))
                (when (> (appkit-loop-pending-count loop) 0)
                  (setq pending t)
                  (appkit-loop-run-pass loop)))))
          (and pending (< (cl-incf turns) 100))))
    (should (< turns 100))))

(defmacro qq-operation-test-with-app (&rest body)
  "Run BODY with real Apps and a manually settled transport."
  (declare (indent 0))
  `(qq-runtime-test-with-reset
     (let ((qq-runtime--operations (copy-hash-table qq-runtime--operations))
           calls cancelled commits)
       (cl-letf (((symbol-function 'qq-account-get)
                  (lambda (id) `((account_id . ,id) (uin . "10001") (phase . "online"))))
                 ((symbol-function 'qq-server-gateway-instance-id) (lambda () "gateway-a"))
                 ((symbol-function 'qq-rpc-call)
                  (lambda (method params &rest options)
                    (let ((token (make-symbol "transport")))
                      (push (list :method method :params params :options options :token token) calls)
                      token)))
                 ((symbol-function 'qq-server-cancel) (lambda (token) (push token cancelled))))
         (qq-runtime-define-operation probe (id)
           :method "probe" :params `((id . ,id))
           :commit (progn (push (list owner id result) commits) result))
         (qq-runtime-with-account "slot-a" ,@body)))))

(defun qq-operation-test--resolve (call value)
  (funcall (plist-get (plist-get call :options) :callback) value))

(ert-deftest qq-operation-raw-results-commit-only-in-app-and-dedupe ()
  (qq-operation-test-with-app
    (qq-runtime-request '(probe "x"))
    (qq-runtime-request '(probe "x"))
    (qq-operation-test--drain)
    (should (= (length calls) 1))
    (qq-operation-test--resolve (car calls) 'first)
    (should-not commits)
    (qq-operation-test--drain)
    (should (equal commits '(("slot-a" "x" first))))
    (should (eq (plist-get (qq-runtime-operation '(probe "x")) :value) 'first))
    (qq-runtime-request '(probe "x"))
    (qq-operation-test--drain)
    (should (= (length calls) 1))))

(ert-deftest qq-operation-superseded-response-never-projects ()
  (qq-operation-test-with-app
    (qq-runtime-request '(probe "x"))
    (qq-operation-test--drain)
    (let ((old (car calls)))
      (qq-runtime-request '(probe "x") t)
      (qq-operation-test--drain)
      (should (memq (plist-get old :token) cancelled))
      (qq-operation-test--resolve old 'stale)
      (qq-operation-test--resolve (car calls) 'fresh)
      (qq-operation-test--drain)
      (should (equal commits '(("slot-a" "x" fresh)))))))

(ert-deftest qq-operation-accounts-and-closed-surfaces-have-independent-lifetimes ()
  (qq-operation-test-with-app
    (let* (replies
           (surface (qq-runtime-open-account-surface
                     :account-id "slot-a" :id 'operation-test :mode #'fundamental-mode
                     :render-function #'ignore))
           (buffer (appkit-surface-buffer surface)))
      (unwind-protect
          (progn
            (with-current-buffer buffer (qq-runtime-request '(probe "x") nil 'old))
            (qq-runtime-with-account "slot-b" (qq-runtime-request '(probe "x")))
            (qq-operation-test--drain)
            (appkit-surface-stop surface)
            ;; Reusing the identity and host must not revive the old reply route.
            (qq-runtime-open-account-surface
             :account-id "slot-a" :id 'operation-test :buffer buffer
             :mode #'fundamental-mode :render-function #'ignore
             :update (lambda (context model message)
                       (when (eq (car-safe message) 'appkit-operation-completed)
                         (push message replies))
                       (qq-runtime--surface-update context model message)))
            (dolist (call calls)
              (qq-operation-test--resolve call (alist-get 'account_id (plist-get call :params))))
            (qq-operation-test--drain)
            (should-not replies)
            (should (equal (plist-get (qq-runtime-operation '(probe "x") "slot-a") :value) "slot-a"))
            (should (equal (plist-get (qq-runtime-operation '(probe "x") "slot-b") :value) "slot-b")))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest qq-operation-mutation-revokes-old-read-and-runs-dependent-refresh ()
  (qq-operation-test-with-app
    (qq-runtime-define-operation mutate ()
      :write t :method "mutate" :cancel '((probe)) :next (unless reason '((probe "x"))))
    (qq-runtime-request '(probe "x"))
    (qq-operation-test--drain)
    (let ((old (car calls)))
      (qq-runtime-request '(mutate))
      (qq-operation-test--drain)
      (qq-operation-test--resolve (car calls) 'accepted)
      (qq-operation-test--drain)
      (should (= (length calls) 3))
      (qq-operation-test--resolve old 'stale)
      (qq-operation-test--resolve (car calls) 'new)
      (qq-operation-test--drain)
      (should (equal commits '(("slot-a" "x" new)))))))

(defun qq-operation-test--seed (key &rest fields)
  "Install canonical FIELDS for KEY in an isolated test App."
  (puthash key (append fields '(:ready t))
           (qq-state-partition-operations (appkit-app-model (qq-runtime-app)))))

(defun qq-operation-test--entry (key)
  "Return mutable canonical test data for KEY."
  (or (qq-runtime-operation key)
      (qq-operation-test--seed key :value nil :pending nil :error nil :date nil)))

(defun qq-operation-test--group-profile (id)
  (qq-core--group-profile-from-state id))

(gv-define-setter qq-operation-test--group-profile (value _id)
  `(let ((profile ,value))
     (qq-state-apply-groups
      (list (mapcar (lambda (cell)
                      (cons (pcase (car cell)
                              ('name 'group_name) ('remark 'group_remark)
                              (key key))
                            (cdr cell)))
                    (assq-delete-all 'pinned (copy-tree profile)))))
     (when (assq 'pinned profile)
       (qq-state-upsert-session
        (qq-state-session-key 'group (alist-get 'group_id profile))
        (list (cons 'pinned (alist-get 'pinned profile))) nil))))

(ert-deftest qq-operation-and-timeline-transitions-retain-the-canonical-store ()
  (qq-operation-test-with-app
    (let* ((app (qq-runtime-app))
           (initial (appkit-app-model app))
           (store (qq-state-current)))
      (qq-state-set-self-info '((user_id . "10001")))
      (qq-runtime-request '(probe "x"))
      (qq-operation-test--drain)
      (should-not (eq initial (appkit-app-model app)))
      (should (eq store (qq-state-partition-store (appkit-app-model app))))
      (qq-operation-test--resolve (car calls) 'result)
      (qq-operation-test--drain)
      (let ((settled (appkit-app-model app)))
        (appkit-app-send app '(qq-timeline-forget))
        (qq-operation-test--drain)
        (should-not (eq settled (appkit-app-model app))))
      (should (eq store (qq-state-partition-store (appkit-app-model app))))
      (should (eq store (qq-state-ensure-store "slot-a")))
      (should (equal (alist-get 'user_id (qq-state-self-info)) "10001"))
      (qq-runtime-with-account "slot-b"
        (should-not (qq-state-self-info))))))

(provide 'qq-operation-test)
