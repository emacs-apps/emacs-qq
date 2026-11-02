;;; qq-search-preview-test.el --- Search context preview tests -*- lexical-binding: t; -*-

(require 'qq-test-state)

(require 'ert)
(require 'cl-lib)
(require 'qq-search-preview)

(defmacro qq-search-preview-test-with-ui (&rest body)
  "Run BODY with a live search surface and detached chat host."
  (declare (indent 0) (debug t))
  `(save-window-excursion
     (let ((qq-runtime--account-id
            (or (qq-state-store-account-id (qq-state-store-create)) (qq-runtime-current-account-id) "slot-a"))
           (qq-runtime--snapshots (qq-test--snapshots (qq-state-store-create))) (qq-runtime--app nil)
           (qq-runtime--accounts (make-hash-table :test #'equal))
           (source (generate-new-buffer " *qq-preview-search*"))
           (chat (generate-new-buffer " *qq-preview-chat*")) (instance "gateway-a") (account-present t)
           (hit '(:session-key "group:123" :center ((kind . "row") (row_key . "987")))) (calls nil) view)
       (unwind-protect
           (cl-letf (((symbol-function 'qq-account-get)
                      (lambda (id) (and account-present (equal id "slot-a") t)))
                     ((symbol-function 'qq-server-gateway-instance-id)
                      (lambda () instance))
                     ((symbol-function 'qq-chat--open-buffer)
                      (lambda (session)
                        (should (equal session "group:123"))
                        (should (equal (qq-runtime-current-account-id) "slot-a"))
                        chat))
                     ((symbol-function 'qq-chat-goto-message)
                      (lambda (center &optional no-pop window)
                        (should (equal (qq-runtime-current-account-id) "slot-a"))
                        (push (list center no-pop window) calls)
                        (setq qq-chat--jump-owner (list 'jump center nil window)))))
             (setq view
                   (qq-runtime-open-account-surface
                    :account-id "slot-a" :id 'preview-test
                    :mode #'fundamental-mode :buffer source
                    :render-function #'ignore
                    :setup (lambda (surface)
                             (setq-local qq-search--view surface)
                             (setq-local qq-search--account-id "slot-a")
                             (setq-local qq-search--instance-id instance))))
             (switch-to-buffer source)
             (insert (make-string 80 ?x))
             (goto-char 32)
             ,@body)
         (qq-runtime-stop)
         (when (buffer-live-p source) (kill-buffer source))
         (when (buffer-live-p chat) (kill-buffer chat))))))

(ert-deftest qq-search-preview-retains-list-focus-and-uses-exact-target-window ()
  (qq-search-preview-test-with-ui
   (let ((window (selected-window))
         (start (window-start))
         (position (point)))
     (qq-search-preview-hit hit)
     (should (eq (selected-window) window))
     (should (eq (current-buffer) source))
     (should (= (point) position))
     (should (= (window-start window) start))
     (pcase-let ((`(,center ,no-pop ,target) (car calls)))
       (should (equal center (plist-get hit :center)))
       (should no-pop)
       (should (window-live-p target))
       (should-not (eq target window))
       (should (eq (window-buffer target) chat))
       (should (eq (car (window-parameter target 'qq-chat-preview)) chat))))))

(ert-deftest qq-search-preview-surface-stop-and-new-preview-cancel-only-owned-jumps ()
  (qq-search-preview-test-with-ui
   (qq-search-preview-hit hit)
   (let ((first qq-search-preview--state))
     (qq-search-preview-hit hit)
     (should-not (appkit-handle-alive-p
                  (qq-search-preview--state-handle first)))
     (should (eq (buffer-local-value 'qq-chat--jump-owner chat)
                 (qq-search-preview--state-jump qq-search-preview--state)))
     (appkit-surface-stop view)
     (should-not qq-search-preview--state)
     (should-not (buffer-local-value 'qq-chat--jump-owner chat)))))

(ert-deftest qq-search-preview-cancel-preserves-a-newer-user-jump ()
  (qq-search-preview-test-with-ui
   (qq-search-preview-hit hit)
   (let* ((window (qq-search-preview--state-window qq-search-preview--state))
          (user-jump (list 'jump '((kind . "row") (row_key . "999")) nil)))
     (with-current-buffer chat (setq qq-chat--jump-owner user-jump))
     (qq-search-preview-cancel t)
     (should (eq (buffer-local-value 'qq-chat--jump-owner chat) user-jump))
     (should-not (window-parameter window 'qq-chat-preview)))))

(ert-deftest qq-search-preview-rejects-stale-account-gateway-and-view ()
  (qq-search-preview-test-with-ui
   (setq instance "gateway-b")
   (should-error (qq-search-preview-hit hit) :type 'user-error)
   (setq instance "gateway-a" account-present nil)
   (should-error (qq-search-preview-hit hit) :type 'user-error)
   (setq account-present t)
   (appkit-surface-stop view)
   (should-error (qq-search-preview-hit hit) :type 'user-error)
   (should-not calls)))

(ert-deftest qq-search-preview-passive-window-does-not-mark-read-until-selected ()
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (let ((qq-auto-mark-read t)
            (qq-chat--session-key "group:123")
            (window (selected-window))
            (reads 0))
        (set-window-parameter window 'qq-chat-preview (cons (current-buffer) 'preview))
        (cl-letf (((symbol-function 'appkit-chat-read-target)
                   (lambda (&rest _)
                     (appkit-chat-timeline-row-create :key 'row :payload 'message)))
                  ((symbol-function 'qq-chat--mark-message-viewed)
                   (lambda (_) (cl-incf reads)))
                  ((symbol-function 'appkit-chatbuf-rendering-p) (lambda () nil))
                  ((symbol-function 'qq-chat--flush-deferred-node-redisplay) #'ignore))
          (qq-chat--manage-read-position 1 window)
          (should (= reads 0))
          (qq-chat--post-command)
          (should (= reads 1))
          (should-not (window-parameter window 'qq-chat-preview)))))))

(ert-deftest qq-search-preview-late-jump-never-positions-a-repurposed-window ()
  (save-window-excursion
    (with-temp-buffer
      (let* ((chat (current-buffer))
             (window (selected-window))
             (qq-chat--jump-owner (list 'jump nil nil window))
             (qq-chat--session-key "group:123")
             (positioned nil))
        (should-not (eq (window-buffer window) chat))
        (cl-letf (((symbol-function 'qq-state-session-messages)
                   (lambda (state _) (error "Repurposed preview must not inspect history")))
                  ((symbol-function 'qq-chat--goto-loaded-message)
                   (lambda (&rest _) (setq positioned t))))
          (should-not
           (qq-chat--finish-jump-if-loaded '((kind . "row") (row_key . "987"))))
          (should-not positioned))))))

(ert-deftest qq-search-preview-positions-only-its-explicit-chat-window ()
  (save-window-excursion
    (with-temp-buffer
      (let ((chat (current-buffer))
            (source-window (selected-window)))
        (insert (make-string 200 ?x))
        (let ((first (split-window source-window nil 'below)))
          (set-window-buffer first chat)
          (let ((target (split-window first nil 'right)))
            (set-window-buffer target chat)
            (set-window-point first 2)
            (set-window-point target 3)
            (cl-letf (((symbol-function 'qq-chat--message-position) (lambda (_) 100))
                      ((symbol-function 'qq-chat--message-end-position) (lambda (_) 101))
                      ((symbol-function 'qq-chat--highlight-region) #'ignore))
              (should (qq-chat--goto-loaded-message "987" t target))
              (should (eq (selected-window) source-window))
              (should (= (window-point target) 100))
              (should (= (window-point first) 2)))))))))

(provide 'qq-search-preview-test)
;;; qq-search-preview-test.el ends here
