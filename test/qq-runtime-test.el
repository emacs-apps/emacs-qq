;;; qq-runtime-test.el --- Tests for account-scoped QQ Appkit runtime -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'qq-runtime)
(require 'qq-chat)

(defmacro qq-runtime-test-with-reset (&rest body)
  "Run BODY with isolated Gateway and account Appkit applications."
  (declare (indent 0) (debug t))
  `(let ((qq-runtime--account-id
          (or (qq-state-store-account-id (qq-state-store-create :self-info nil))
              (qq-runtime-current-account-id) "slot-a"))
         (qq-runtime--snapshots (qq-test--snapshots (qq-state-store-create :self-info nil)))
         (qq-runtime--app nil) (qq-runtime--accounts (make-hash-table :test #'equal)))
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
    (let* ((gateway (qq-runtime-gateway-app))
           (first (qq-runtime-app "slot-a"))
           (original (appkit-app-model first)))
      (qq-runtime-with-account "slot-a"
        (qq-runtime-transact #'qq-state-set-self-info '((user_id . "10001"))))
      (let* ((data (appkit-app-model first))
             (store (qq-state-partition-store data))
             (pending (appkit-data-with data
                        ((:operations) 'old-operation t) ((:timelines) 'old-timeline t))))
        (setf (appkit-loop--model (appkit-app-loop first)) pending)
        (qq-runtime-stop-account "slot-a")
        (should (appkit-app-live-p gateway))
        (should-not (qq-runtime-account "slot-a"))
        (should-not (qq-state-store-self-info (qq-state-partition-store original)))
        (let* ((reopened (qq-runtime-app "slot-a")) (current (appkit-app-model reopened)))
          (should-not (eq first reopened))
          (should (eq store (qq-state-partition-store current)))
          (should (= 0 (appkit-data-map-count (qq-state-partition-operations current))))
          (should (= 0 (appkit-data-map-count (qq-state-partition-timelines current))))
          (should (equal (qq-state-self-info (qq-runtime-state "slot-a")) '((user_id . "10001")))))
        (qq-runtime-stop-account "slot-a" t)
        (should-not (qq-state-self-info (qq-runtime-state "slot-a")))
        (should (equal (qq-state-store-self-info store) '((user_id . "10001"))))
        (should (appkit-app-live-p gateway))))))

(ert-deftest qq-runtime-bulk-forget-keeps-account-and-chat-surface-live ()
  "Reconnect invalidation may cancel more cached operations than active tasks."
  (qq-runtime-test-with-reset
    (let* ((app (qq-runtime-app "slot-a"))
           (surface
            (qq-runtime-open-account-surface
             :account-id "slot-a" :id 'bulk-forget-test
             :mode #'fundamental-mode
             :renderer-factory
             (qq-runtime-renderer-factory (lambda (&rest _) nil)))))
      (unwind-protect
          (progn
            (let ((model (appkit-app-model app)))
              (dotimes (index 96)
                (setq model
                      (appkit-data-with model
                        ((:operations) (list 'avatar (number-to-string index))
                         (list :ready t :value index)))))
              (setf (appkit-loop--model (appkit-app-loop app)) model))
            (qq-runtime-forget nil "slot-a")
            (should (appkit-app-live-p app))
            (should (eq (qq-runtime-app "slot-a") app))
            (should (appkit-surface-live-p surface))
            (should (zerop
                     (appkit-data-map-count
                      (qq-state-partition-operations (appkit-app-model app)))))
            (qq-runtime-with-account "slot-a"
              (qq-runtime-transact
               #'qq-state-set-self-info '((user_id . "10001"))))
            (qq-test-drain app)
            (should (appkit-surface-live-p surface))
            (should (equal (qq-state-self-info (qq-runtime-state "slot-a"))
                           '((user_id . "10001")))))
        (let ((buffer (appkit-surface-buffer surface)))
          (appkit-surface-stop surface)
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest qq-runtime-buffer-context-selects-account-app-and-state ()
  (qq-runtime-test-with-reset
    (qq-runtime-with-account "slot-a"
                             (qq-runtime-transact #'qq-state-set-self-info '((user_id . "10001"))))
    (qq-runtime-with-account "slot-b"
                             (qq-runtime-transact #'qq-state-set-self-info '((user_id . "10002"))))
    (with-temp-buffer
      (let ((runtime (qq-runtime-bind-account "slot-b")))
        (should
         (eq (qq-runtime-app) (qq-runtime-account-app runtime)))
        (qq-runtime-with-account (qq-runtime-current-account-id)
                                 (should
                                  (equal (alist-get 'user_id (qq-state-self-info (qq-runtime-state))) "10002")))))))

(ert-deftest qq-runtime-explicit-account-context-wins-over-current-buffer ()
  (qq-runtime-test-with-reset
    (qq-runtime-with-account "slot-a"
      (qq-runtime-transact #'qq-state-set-self-info '((user_id . "10001"))))
    (qq-runtime-with-account "slot-b"
      (qq-runtime-transact #'qq-state-set-self-info '((user_id . "10002"))))
    (with-temp-buffer
      (qq-runtime-bind-account "slot-a")
      (should (equal (qq-runtime-current-account-id) "slot-a"))
      (qq-runtime-with-account "slot-b"
        (qq-runtime-bind-account "slot-a")
        (should (equal (qq-runtime-current-account-id) "slot-b"))
        (should (equal (qq-state-active-account-id (qq-runtime-state)) "slot-b"))
        (should (equal (alist-get 'user_id (qq-state-self-info (qq-runtime-state))) "10002"))
        (should (eq (qq-runtime-state) (qq-runtime-state "slot-b")))
        (should (eq (qq-runtime-app)
                    (qq-runtime-account-app (qq-runtime-ensure-account "slot-b")))))
      (cl-letf (((symbol-function 'qq-account-current-id) (lambda () "slot-b")))
        (qq-runtime-with-account nil
          (should-not (qq-runtime-current-account-id))
          (should-not (qq-state-active-account-id (qq-runtime-state)))))
      (should (equal (qq-runtime-current-account-id) "slot-a"))
      (should (equal (qq-state-active-account-id (qq-runtime-state)) "slot-a"))
      (should (equal (alist-get 'user_id (qq-state-self-info (qq-runtime-state))) "10001")))))

(ert-deftest qq-runtime-media-burst-merges-renders-without-dropping-overflow ()
  (qq-runtime-test-with-reset
    (let (surface renders)
      (unwind-protect
          (progn
            (setq surface
                  (qq-runtime-open-account-surface
                   :account-id "slot-a" :id 'media-burst-test :mode #'fundamental-mode
                   :renderer-factory (qq-runtime-renderer-factory (lambda (_surface _app-view _model change) (push change renders) nil))))
            (setq renders nil)
            (qq-runtime-post-invalidation surface :keys '(one) :resources '((:media "one")))
            (qq-runtime-post-invalidation surface :keys '(two) :resources '((:media "two")) :frame-p t)
            (should-not renders)
            (appkit-loop-run-pass (appkit-surface-loop surface))
            (should (= (length renders) 1))
            (should (equal (appkit-projection-change-keys (car renders)) '(one two)))
            (should (equal (appkit-projection-change-resources (car renders))
                           '((:media "one") (:media "two"))))
            (should (appkit-projection-change-frame-p (car renders)))
            (setq renders nil)
            ;; Exceed the mailbox's ordinary capacity before any scheduled pass.
            (dotimes (n 70)
              (qq-runtime-post-invalidation surface :resources (list (list :media n))))
            (while (> (appkit-loop-pending-count (appkit-surface-loop surface)) 0)
              (appkit-loop-run-pass (appkit-surface-loop surface)))
            (should (< (length renders) 10))
            (should (equal (sort (mapcar #'cadr
                                        (apply #'append
                                               (mapcar #'appkit-projection-change-resources renders)))
                                 #'<)
                           (number-sequence 0 69))))
        (when surface
          (let ((buffer (appkit-surface-buffer surface)))
            (appkit-surface-stop surface)
            (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(ert-deftest qq-runtime-account-surface-runs-setup-and-render-in-owner-context ()
  (qq-runtime-test-with-reset
    (let (surfaces setups updates)
      (dolist (owner '("slot-a" "slot-b"))
        (qq-runtime-with-account owner
                                 (qq-runtime-transact #'qq-state-set-self-info `((user_id . ,owner)))))
      (unwind-protect
          (with-temp-buffer
            (qq-runtime-bind-account "slot-b")
            (dolist (owner '("slot-a" "slot-b"))
              (let ((surface
                     (qq-runtime-open-account-surface
                      :account-id owner
                      :id 'runtime-test
                      :mode #'fundamental-mode
                      :update
                      (lambda (context model message)
                        (push (qq-runtime-current-account-id) updates)
                        (qq-runtime--surface-update context model message))
                      :setup
                      (lambda (_surface)
                        (push (qq-runtime-current-account-id) setups))
                      :renderer-factory (qq-runtime-renderer-factory (lambda (_surface _app-view _model _change) (erase-buffer) (insert (qq-runtime-current-account-id) ":" (alist-get 'user_id (qq-state-self-info (qq-runtime-state)))))))))
                (push surface surfaces)
                (should (appkit-surface-live-p surface))
                (with-current-buffer (appkit-surface-buffer surface)
                  (should (equal (buffer-string) (concat owner ":" owner))))))
            (should (equal (nreverse setups) '("slot-a" "slot-b")))
            (should (equal (nreverse updates) '("slot-a" "slot-b")))
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
        (qq-runtime-transact #'qq-state-set-self-info '((user_id . "10001"))))
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
                              (should (equal (alist-get 'user_id (qq-state-self-info (qq-runtime-state)))
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
              (qq-chat--surface-update
               nil model
               (appkit-delivery-report--create
                :correlation old :outcome 'unavailable))))
        (should (eq model (appkit-next-model next)))
        (setq next (qq-chat--surface-update
                    nil model (list 'qq-history-presented (list :kind 'latest))))
        (should (eq model (appkit-next-model next)))
        (setq next (qq-chat--surface-update
                    nil model (list 'qq-history-presented presentation)))
        (should-not (plist-get (appkit-next-model next) :history-presentation))
        (should (eq current (plist-get (appkit-next-model next) :loading)))))))


(ert-deftest qq-runtime-nested-context-restores-after-error-and-drop ()
  (qq-runtime-test-with-reset
    (qq-runtime-transact #'qq-state-set-self-info '((user_id . "10001")))
    (let ((selected (qq-runtime-state)))
      (qq-runtime-with-account "slot-b"
        (qq-runtime-transact #'qq-state-set-self-info '((user_id . "10002")))
        (let ((outer (qq-runtime-state)))
          (should-error
           (qq-runtime-with-account "slot-c"
             (qq-runtime-transact #'qq-state-set-self-info '((user_id . "10003")))
             (error "Abort the nested context")))
          (should (eq outer (qq-runtime-state)))
          (should (equal (qq-runtime-current-account-id) "slot-b"))))
      (should (eq selected (qq-runtime-state)))
      (qq-runtime-with-account "slot-c"
        (should (equal (alist-get 'user_id (qq-state-self-info (qq-runtime-state))) "10003")))
      (qq-runtime-stop-account "slot-a" t)
      (should-not (qq-state-self-info (qq-runtime-state)))
      (should (equal (alist-get 'user_id (qq-state-self-info selected)) "10001"))
      (should (equal (qq-runtime-current-account-id) "slot-a"))
      (should-not (gethash "slot-a" qq-runtime--snapshots)))))

(ert-deftest qq-runtime-notifications-survive-cold-byte-compilation ()
  (let* ((directory (make-temp-file "qq-runtime-compile-" t))
         (source (expand-file-name "qq-runtime.el" directory))
         (qq-runtime--context-account "outer")
         observed resolved
         (qq-runtime-change-hook
          (list (lambda (_event)
                  (setq observed (qq-runtime-current-account-id))))))
    (unwind-protect
        (progn
          (require 'bytecomp)
          (copy-file (locate-library "qq-runtime.el") source)
          ;; Loading source first must not hide a forward macro reference.
          (cl-letf (((symbol-function 'qq-runtime-with-account) nil))
            (let ((byte-compile-dest-file-function nil)
                  (byte-compile-warnings nil))
              (should (byte-compile-file source))))
          (let* ((notify
                  (with-temp-buffer
                    (insert-file-contents (concat source "c"))
                    (let ((read-circle t))
                      (catch 'notify
                        (while t
                          (let ((form (read (current-buffer))))
                            (when (and (eq (car-safe form) 'defalias)
                                       (equal (nth 1 form)
                                              ''qq-runtime--notify-effect))
                              (throw 'notify (eval (nth 2 form) t)))))))))
                 (spec (funcall notify '((:account-id "compiled-account")))))
            (should
             (appkit-cancellation-p
              (funcall (appkit-effect-spec-start spec)
                       nil (appkit-effect-spec-input spec)
                       #'ignore (lambda (_value) (setq resolved t)) #'ignore))))
          (should (equal observed "compiled-account"))
          (should (equal qq-runtime--context-account "outer"))
          (should resolved))
      (delete-directory directory t))))

(ert-deftest qq-runtime-change-listeners-post-from-committed-account ()
  (qq-runtime-test-with-reset
    (let (surface events (render-count 0))
      (unwind-protect
          (progn
            (setq surface
                  (qq-runtime-open-account-surface
                   :account-id "slot-a" :id 'change-listener-test
                   :mode #'fundamental-mode
                   :renderer-factory (qq-runtime-renderer-factory (lambda (&rest _app-view _) (cl-incf render-count) nil))))
            (setq render-count 0)
            (let ((qq-runtime-change-hook
                   (list
                    (lambda (event)
                      (push event events)
                      (should (eq appkit-loop--active-loop
                                  (appkit-app-loop (qq-runtime-app "slot-a"))))
                      (should-not appkit-loop--current-envelope)
                      (should (eq (plist-get event :state) (qq-runtime-state)))
                      (should
                       (eq (appkit-surface-post
                            surface
                            (list 'qq-render
                                  (appkit-projection-change-create :full-p t)))
                           'enqueued))))))
              (qq-runtime-with-account "slot-a"
                (qq-runtime-transact
                 #'qq-state-set-self-info '((user_id . "10001"))))
              (should (= (length events) 1))
              (should (zerop render-count))
              (should (= (appkit-loop-pending-count
                          (appkit-surface-loop surface)) 1))
              (should
               (equal (qq-state-self-info (plist-get (car events) :state))
                      '((user_id . "10001"))))
              (qq-test-drain (qq-runtime-app "slot-a"))
              (should (= render-count 1))))
        (when surface
          (let ((buffer (appkit-surface-buffer surface)))
            (appkit-surface-stop surface)
            (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(provide 'qq-runtime-test)

;;; qq-runtime-test.el ends here
