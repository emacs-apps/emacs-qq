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
         (qq-state--active-account-id nil)
         (qq-state--self-info nil))
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
                      :account-id owner
                      :id 'runtime-test
                      :mode #'fundamental-mode
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

(ert-deftest qq-runtime-standard-renderer-keeps-owner-through-recovery-and-stop ()
  (qq-runtime-test-with-reset
    (let (surface events
          (resource-request (lambda (_keys) appkit-render-none)))
      (qq-runtime-with-account "slot-a"
        (qq-state-set-self-info '((user_id . "10001"))))
      (unwind-protect
          (with-temp-buffer
            (qq-runtime-bind-account "slot-b")
            (setq surface
                  (qq-runtime-open-account-surface
                   :account-id "slot-a"
                   :id 'standard-renderer-test
                   :mode #'fundamental-mode
                   :renderer-factory
                   (lambda (_surface)
                     (let ((record
                            (lambda (phase host)
                              (should (eq (current-buffer)
                                          (appkit-surface-buffer host)))
                              (should (equal (qq-runtime-current-account-id) "slot-a"))
                              (should (equal (alist-get 'user_id (qq-state-self-info))
                                             "10001"))
                              (should (eq qq-media--consumer host))
                              (push phase events))))
                       (appkit-generated-renderer-create
                        :mount (lambda (host _view _model) (funcall record 'mount host))
                        :merge #'appkit-projection-change-merge
                        :resource-request resource-request
                        :render (lambda (host _view _model _change)
                                  (funcall record 'render host)
                                  (error "Recover this presentation"))
                        :recover (lambda (host _view _model _condition)
                                   (funcall record 'recover host)
                                   (insert "recovered")
                                   nil)
                        :unmount (lambda (host) (funcall record 'unmount host)))))))
            (should (equal (qq-runtime-current-account-id) "slot-b"))
            (should (appkit-surface-live-p surface))
            (should (eq (appkit-generated-renderer-resource-request
                         (appkit-surface-renderer surface))
                        resource-request))
            (with-current-buffer (appkit-surface-buffer surface)
              (should (equal (buffer-string) "recovered")))
            (appkit-surface-stop surface)
            (with-current-buffer (appkit-surface-buffer surface)
              (should-not qq-runtime--surface-owner))
            (should (equal (nreverse events) '(mount render recover unmount))))
        (when surface
          (let ((buffer (appkit-surface-buffer surface)))
            (appkit-surface-stop surface)
            (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(ert-deftest qq-runtime-history-ignores-stale-delivery-and-presentation-reports ()
  (qq-runtime-test-with-reset
    (with-temp-buffer
      (qq-runtime-bind-account "slot-a")
      (let* ((old (list :kind 'initial))
             (current (list :kind 'around))
             (presentation (list :kind 'latest))
             (model (list :loading current :history-presentation presentation))
             (next
              (qq-runtime--surface-update
               nil model
               (appkit-delivery-report--create
                :correlation old :outcome 'unavailable))))
        (should (eq model (appkit-next-model next)))
        (setq next (qq-runtime--surface-update
                    nil model (list 'qq-history-presented (list :kind 'latest))))
        (should (eq model (appkit-next-model next)))
        (setq next (qq-runtime--surface-update
                    nil model (list 'qq-history-presented presentation)))
        (should-not (plist-get (appkit-next-model next) :history-presentation))
        (should (eq current (plist-get (appkit-next-model next) :loading)))))))


(provide 'qq-runtime-test)

;;; qq-runtime-test.el ends here
