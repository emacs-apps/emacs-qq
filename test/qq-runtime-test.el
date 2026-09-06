;;; qq-runtime-test.el --- Tests for account-scoped QQ Appkit runtime -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'qq-runtime)

(defmacro qq-runtime-test-with-reset (&rest body)
  "Run BODY with isolated Gateway and account Appkit applications."
  (declare (indent 0) (debug t))
  `(let ((qq-runtime--app nil)
         (qq-runtime--accounts (make-hash-table :test #'equal))
         (qq-state--partitions (make-hash-table :test #'equal))
         (qq-state--active-account-id nil))
     (unwind-protect
         (progn ,@body)
       (qq-runtime-stop))))

(ert-deftest qq-runtime-owns-one-live-app-per-stable-account ()
  (qq-runtime-test-with-reset
    (let* ((first (qq-runtime-ensure-account "slot-a"))
           (again (qq-runtime-ensure-account "slot-a"))
           (other (qq-runtime-ensure-account "slot-b")))
      (should (eq first again))
      (should-not (eq first other))
      (should
       (equal (appkit-app-identity (qq-runtime-account-app first)) "slot-a"))
      (should
       (qq-state-partition-p
        (appkit-app-model (qq-runtime-account-app first))))
      (should (= (length (qq-runtime-accounts)) 2)))))

(ert-deftest qq-runtime-account-stop-does-not-stop-gateway-app ()
  (qq-runtime-test-with-reset
    (let ((gateway (qq-runtime-gateway-app)))
      (qq-runtime-ensure-account "slot-a")
      (qq-runtime-stop-account "slot-a")
      (should (appkit-app-live-p gateway))
      (should-not (qq-runtime-account "slot-a")))))

(ert-deftest qq-runtime-buffer-context-selects-account-app-and-state ()
  (qq-runtime-test-with-reset
    (qq-runtime-with-account "slot-a"
      (qq-state-set-self-info '((user_id . "10001"))))
    (qq-runtime-with-account "slot-b"
      (qq-state-set-self-info '((user_id . "10002"))))
    (with-temp-buffer
      (let ((runtime (qq-runtime-bind-account "slot-b")))
        (should
         (eq (qq-runtime-app) (qq-runtime-account-app runtime)))
        (qq-runtime-with-account (qq-runtime-current-account-id)
          (should
           (equal (alist-get 'user_id (qq-state-self-info)) "10002")))))))

(ert-deftest qq-runtime-explicit-account-context-wins-over-current-buffer ()
  (qq-runtime-test-with-reset
    (with-temp-buffer
      (qq-runtime-bind-account "slot-a")
      (should (equal (qq-runtime-current-account-id) "slot-a"))
      (qq-runtime-with-account "slot-b"
        (should (equal (qq-runtime-current-account-id) "slot-b"))
        (should
         (eq (qq-runtime-app)
             (qq-runtime-account-app
              (qq-runtime-ensure-account "slot-b")))))
      (should (equal (qq-runtime-current-account-id) "slot-a")))))

(ert-deftest qq-runtime-account-surface-runs-setup-and-render-in-owner-context ()
  (qq-runtime-test-with-reset
    (let (surfaces setups)
      (dolist (owner '("slot-a" "slot-b"))
        (qq-runtime-with-account owner
          (qq-state-set-self-info `((user_id . ,owner)))))
      (unwind-protect
          (with-temp-buffer
            (qq-runtime-bind-account "slot-b")
            (dolist (owner '("slot-a" "slot-b"))
              (let ((surface
                     (qq-runtime-open-account-surface
                      :account-id owner :id 'runtime-test :mode #'fundamental-mode
                      :setup
                      (lambda (_surface)
                        (push (qq-runtime-current-account-id) setups))
                      :render-function
                      (lambda (_surface _model _change)
                        (erase-buffer)
                        (insert (qq-runtime-current-account-id) ":"
                                (alist-get 'user_id (qq-state-self-info)))))))
                (push surface surfaces)
                (should (appkit-surface-live-p surface))
                (with-current-buffer (appkit-surface-buffer surface)
                  (should (equal (buffer-string) (concat owner ":" owner))))))
            (should (equal (nreverse setups) '("slot-a" "slot-b")))
            (should (equal (qq-runtime-current-account-id) "slot-b")))
        (dolist (surface surfaces)
          (let ((buffer (appkit-surface-buffer surface)))
            (appkit-surface-stop surface)
            (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(provide 'qq-runtime-test)

;;; qq-runtime-test.el ends here
