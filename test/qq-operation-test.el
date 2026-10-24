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
          (and pending (< (cl-incf turns) 30))))
    (should (< turns 30))))

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
    (let* ((surface (qq-runtime-open-account-surface
                     :account-id "slot-a" :id 'operation-test :mode #'fundamental-mode :render-function #'ignore))
           (buffer (appkit-surface-buffer surface)))
      (unwind-protect
          (progn
            (with-current-buffer buffer (qq-runtime-request '(probe "x")))
            (qq-runtime-with-account "slot-b" (qq-runtime-request '(probe "x")))
            (qq-operation-test--drain)
            (appkit-surface-stop surface)
            (dolist (call calls)
              (qq-operation-test--resolve call (alist-get 'account_id (plist-get call :params))))
            (qq-operation-test--drain)
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

(provide 'qq-operation-test)
