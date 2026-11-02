;;; qq-contacts-test.el --- Tests for native QQ directory -*- lexical-binding: t; -*-

(require 'qq-test-state)

;;; Code:

(require 'ert)
(require 'qq-operation-test)
(require 'cl-lib)
(require 'qq-contacts)
(require 'qq-root)

(defmacro qq-contacts-test-with-state (&rest body)
  "Run BODY with deterministic native directory fixtures."
  `(let ((qq-runtime--account-id
          (or (qq-state-store-account-id (qq-state-store-create)) (qq-runtime-current-account-id) "slot-a"))
         (qq-runtime--snapshots (qq-test--snapshots (qq-state-store-create)))
         (qq-contacts-buffer-name (generate-new-buffer-name " *qq-contacts-state-test*"))
         (qq-runtime-change-hook nil) (qq-media-cache-update-hook nil)
         (qq-runtime--accounts (make-hash-table :test #'equal)))
     (when-let* ((buffer (get-buffer qq-contacts-buffer-name)))
       (kill-buffer buffer))
     (unwind-protect
         (qq-runtime-with-account "slot-a"
           (qq-test-reset)
           (qq-runtime-transact #'qq-state-apply-friend-categories
                                '(((category_id . 7) (sort_id . 1) (name . "工作")
                                   (online_count . 1)
                                   (friends
                                    . (((user_id . "10002") (nickname . "Bob") (remark))
                                       ((user_id . "10001") (nickname . "Alice")
                                        (remark . "A姐")))))
                                  ((category_id . 3) (sort_id . 2) (name . "空分组")
                                   (online_count . 0) (friends))))
           (qq-runtime-transact #'qq-state-apply-groups
                                '(((group_id . "20002") (group_name . "Dormant Group")
                                   (group_remark) (member_count . 20) (max_member_count . 500)
                                   (pinned . :false) (self_permission . "member"))
                                  ((group_id . "20001") (group_name . "Recent Group")
                                   (group_remark . "常用群") (member_count . 40)
                                   (max_member_count . 500) (pinned . t)
                                   (self_permission . "admin"))))
           (qq-runtime-transact #'qq-state-upsert-session
                                "group:20001"
                                '((type . group) (target-id . "20001") (title . "Recent Group"))
                                )
           (progn (qq-test-set-field (qq-runtime-state) :recent-session-keys '("group:20001")))
           (qq-test-put (qq-runtime-state) :recent-session-key-set "group:20001" t)
           ,@body)
       (when-let* ((buffer (get-buffer qq-contacts-buffer-name)))
         (kill-buffer buffer))
       (dolist (runtime (qq-runtime-accounts))
         (dolist (entry (hash-table-values
                         (appkit-app-surfaces (qq-runtime-account-app runtime))))
           (let* ((surface (cdr entry))
                  (buffer (appkit-surface-buffer surface)))
             (appkit-surface-stop surface)
             (when (buffer-live-p buffer)
               (kill-buffer buffer)))))
       (qq-runtime-stop-account "slot-a" t)
       (qq-test-reset))))

(defmacro qq-contacts-test-with-runtime-view (&rest body)
  "Run BODY in an isolated live contacts view.

BODY may refer to the lexical variables `app', `buffer', and `view'."
  (declare (indent 0) (debug t))
  `(let* ((qq-runtime--account-id
           (or (qq-state-store-account-id (qq-state-store-create)) (qq-runtime-current-account-id) "slot-a"))
          (qq-runtime--snapshots (qq-test--snapshots (qq-state-store-create)))
          (qq-contacts-buffer-name (generate-new-buffer-name " *qq-contacts-test*"))
          (qq-runtime--accounts (make-hash-table :test #'equal))
          (runtime (qq-runtime-ensure-account "slot-a")) (app (qq-runtime-account-app runtime))
          (buffer (get-buffer-create qq-contacts-buffer-name)) view)
     (unwind-protect
         (with-current-buffer buffer
           (qq-contacts-test-mode)
           (setq view (qq-contacts--ensure-view))
           ,@body)
       (qq-runtime-stop-account "slot-a" t)
       (when (buffer-live-p buffer)
         (kill-buffer buffer)))))

(defun qq-contacts-test-mode ()
  "Enter `qq-contacts-mode' with an explicit synthetic account owner."
  (qq-contacts-mode)
  (setq-local qq-runtime--account-id "slot-a"))

(defun qq-contacts-test--entry-keys (entries)
  "Return stable keys from ENTRIES."
  (mapcar #'appkit-directory-entry-key entries))

(defun qq-contacts-test--item-position (item-id)
  "Return position of directory ITEM-ID in current buffer."
  (let ((position (point-min)) found)
    (while (and (< position (point-max)) (not found))
      (if (equal (get-text-property position 'qq-contacts-item-id) item-id)
          (setq found position)
        (setq position
              (next-single-property-change
               position 'qq-contacts-item-id nil (point-max)))))
    found))

(ert-deftest qq-contacts-friend-projection-preserves-category-and-friend-order ()
  (qq-contacts-test-with-state
   (with-temp-buffer
     (qq-contacts-test-mode)
     (should
      (equal (qq-contacts-test--entry-keys
              (qq-contacts--project-friends))
             '(navigation
               (category . 7)
               (friend . "10002")
               (friend . "10001")
               (category . 3)))))))

(ert-deftest qq-contacts-category-collapse-removes-only-its-friends ()
  (qq-contacts-test-with-state
   (with-temp-buffer
     (qq-contacts-test-mode)
     (appkit-directory-set-fold-expanded
      (appkit-directory-surface) '(category . 7) nil)
     (should
      (equal (qq-contacts-test--entry-keys
              (qq-contacts--project-friends))
             '(navigation (category . 7) (category . 3)))))))

(ert-deftest qq-contacts-all-groups-include-peers-without-recent-session ()
  (qq-contacts-test-with-state
   (with-temp-buffer
     (qq-contacts-test-mode)
     (should
      (equal (qq-contacts-test--entry-keys
              (qq-contacts--project-groups))
             '(navigation (group . "20002") (group . "20001"))))
     (should
      (equal (qq-contacts-test--entry-keys
              (qq-contacts--project-groups t))
             '(navigation (group . "20002"))))
     ;; Opening/hydrating a chat does not rewrite the authoritative recent
     ;; snapshot membership used by this view.
     (qq-runtime-transact #'qq-state-upsert-session
                          "group:20002" '((type . group) (target-id . "20002")) )
     (should
      (equal (qq-contacts-test--entry-keys
              (qq-contacts--project-groups t))
             '(navigation (group . "20002")))))))

(ert-deftest qq-contacts-navigation-buttons-are-real-and-switch-view ()
  (qq-contacts-test-with-state
   (with-temp-buffer
     (qq-contacts-test-mode)
     (qq-contacts--ensure-view)
     (cl-letf (((symbol-function 'qq-media-avatar-cached-display-string)
                (lambda (_id) "@"))
               ((symbol-function 'qq-media-group-avatar-cached-display-string)
                (lambda (_id) "#")))
       (qq-contacts--reconcile))
     (goto-char (point-min))
     (search-forward "全部群")
     (let ((button (button-at (1- (point)))))
       (should button)
       (push-button button))
     (should (eq qq-contacts--view 'groups)))))

(ert-deftest qq-contacts-action-row-opens-exact-canonical-chat ()
  (qq-contacts-test-with-state
   (with-temp-buffer
     (qq-contacts-test-mode)
     (let (opened)
       (cl-letf (((symbol-function 'qq-media-avatar-cached-display-string)
                  (lambda (_id) "@"))
                 ((symbol-function 'qq-media-group-avatar-cached-display-string)
                  (lambda (_id) "#"))
                 ((symbol-function 'qq-chat-open)
                  (lambda (session-key) (setq opened session-key))))
         (qq-contacts--reconcile)
         (goto-char (qq-contacts-test--item-position "10001"))
         (qq-contacts-open-at-point)
         (should (equal opened "private:10001")))))))

(ert-deftest qq-contacts-info-dispatches-to-native-user-and-group-pages ()
  (qq-contacts-test-with-state
   (with-temp-buffer
     (qq-contacts-test-mode)
     (let (user group)
       (cl-letf (((symbol-function 'qq-media-avatar-cached-display-string)
                  (lambda (_id) "@"))
                 ((symbol-function 'qq-media-group-avatar-cached-display-string)
                  (lambda (_id) "#"))
                 ((symbol-function 'qq-user-open)
                  (lambda (id) (setq user id)))
                 ((symbol-function 'qq-group-open)
                  (lambda (id) (setq group id))))
         (qq-contacts--reconcile)
         (goto-char (qq-contacts-test--item-position "10002"))
         (qq-contacts-open-info-at-point)
         (should (equal user "10002"))
         (setq qq-contacts--view 'groups)
         (qq-contacts--reconcile)
         (goto-char (qq-contacts-test--item-position "20002"))
         (qq-contacts-open-info-at-point)
         (should (equal group "20002")))))))

(ert-deftest qq-contacts-renamed-buffer-hooks-use-the-registered-view ()
  (qq-contacts-test-with-runtime-view
    (let ((renamed (generate-new-buffer-name " *qq-contacts-renamed*")))
      (rename-buffer renamed)
      (setq qq-contacts--dirty nil qq-contacts--pending-force-keys nil)
      (cl-letf (((symbol-function 'qq-runtime-app)
                 (lambda (&rest _arguments)
                   (ert-fail "contacts hook started a runtime app")))
                ((symbol-function 'qq-contacts--displayed-p) (lambda () nil)))
        (qq-contacts--handle-state-change
         '(:type friends-refreshed :account-id "slot-a"))
        (qq-test-drain (appkit-surface-app view))
        (should qq-contacts--dirty)
        (qq-contacts--handle-media-cache-update "avatar:10001"))
      (should (equal qq-contacts--pending-force-keys
                     '((member . "10001") (friend . "10001"))))
      (should (equal renamed (buffer-name buffer)))
      (should (eq view (qq-contacts--live-view "slot-a"))))))

(ert-deftest qq-contacts-hooks-do-not-create-a-runtime-app-when-closed ()
  (let ((qq-runtime--app nil))
    (cl-letf (((symbol-function 'qq-runtime-app)
               (lambda ()
                 (ert-fail "closed contacts hook created a runtime app")))
              ((symbol-function 'appkit-surface-send)
               (lambda (&rest _arguments)
                 (ert-fail "closed contacts hook requested a sync"))))
      (qq-contacts--handle-state-change '(:type friends-refreshed))
      (qq-contacts--handle-media-cache-update "avatar:10001"))
    (should-not qq-runtime--app)))

(ert-deftest qq-contacts-visible-avatar-update-invalidates-only-target-rows ()
  (qq-contacts-test-with-state
   (with-temp-buffer
     (qq-contacts-test-mode)
     (let ((view (qq-contacts--ensure-view)) (avatar "old-avatar"))
       (cl-letf (((symbol-function 'qq-contacts--displayed-p) (lambda () t))
                 ((symbol-function 'qq-contacts--usable-width) (lambda () 80))
                 ((symbol-function 'qq-contacts--ensure-window-avatars) #'ignore)
                 ((symbol-function 'qq-media-avatar-cached-display-string)
                  (lambda (user-id)
                    (if (equal user-id "10001") avatar "other-avatar"))))
         (appkit-surface-send
          view (list 'qq-render (appkit-projection-change-create :full-p t)))
         (should (string-match-p "old-avatar" (buffer-string)))
         (setq avatar "new-avatar")
         (qq-contacts--handle-media-cache-update "avatar:10001")
         (should (string-match-p "new-avatar" (buffer-string)))
         (should-not (string-match-p "old-avatar" (buffer-string)))
         (should (string-match-p "other-avatar" (buffer-string))))))))

(ert-deftest qq-contacts-hidden-avatar-update-retains-forced-row-keys ()
  (qq-contacts-test-with-state
   (with-temp-buffer
     (qq-contacts-test-mode)
     (let ((view (qq-contacts--ensure-view))
           (visible t) (avatar "old-avatar"))
       (cl-letf (((symbol-function 'qq-contacts--displayed-p) (lambda () visible))
                 ((symbol-function 'qq-contacts--usable-width) (lambda () 80))
                 ((symbol-function 'qq-contacts--ensure-window-avatars) #'ignore)
                 ((symbol-function 'qq-media-avatar-cached-display-string)
                  (lambda (_user-id) avatar)))
         (appkit-surface-send
          view (list 'qq-render (appkit-projection-change-create :full-p t)))
         (setq visible nil avatar "new-avatar")
         (qq-contacts--handle-media-cache-update "avatar:10001")
         (should (string-match-p "old-avatar" (buffer-string)))
         (should-not (string-match-p "new-avatar" (buffer-string)))
         (setq visible t)
         (appkit-surface-send
          view (list 'qq-render (appkit-projection-change-create :full-p t)))
         (should (string-match-p "new-avatar" (buffer-string)))
         (should-not qq-contacts--dirty)
         (should-not qq-contacts--pending-force-keys))))))

(ert-deftest qq-contacts-dead-view-makes-window-callbacks-inert ()
  (qq-contacts-test-with-runtime-view
    (let (queued avatar-started)
      (let ((inhibit-read-only t))
        (insert "Synthetic friend\n")
        (add-text-properties
         (point-min) (point-max)
         '(qq-contacts-row-type friend
           qq-contacts-object ((user_id . "10001")))))
      (setq qq-contacts--dirty t
            qq-contacts--fill-column 80)
      (appkit-surface-stop view)
      (cl-letf (((symbol-function 'window-live-p) (lambda (_window) t))
                ((symbol-function 'window-buffer)
                 (lambda (_window) (current-buffer)))
                ((symbol-function 'window-start)
                 (lambda (_window) (point-min)))
                ((symbol-function 'window-end)
                 (lambda (&rest _args) (point-max)))
                ((symbol-function 'qq-contacts--displayed-p) (lambda () t))
                ((symbol-function 'qq-contacts--usable-width) (lambda () 100))
                ((symbol-function 'qq-contacts--ensure-view)
                 (lambda () (ert-fail "window callback reattached view")))
                ((symbol-function 'appkit-surface-send)
                 (lambda (&rest _args) (setq queued t)))
                ((symbol-function 'qq-media-avatar-image)
                 (lambda (&rest _args) (setq avatar-started t))))
        (qq-contacts--window-buffer-change 'synthetic-window)
        (appkit-surface-refresh-responsive-geometry view)
        (qq-contacts--window-scroll 'synthetic-window nil))
      (should-not queued)
      (should-not avatar-started)
      (should-not (appkit-current-surface))
      (should-not (appkit-app-surface app qq-contacts--view-id)))))

(ert-deftest qq-contacts-reconcile-preserves-force-keys-queued-during-render ()
  (qq-contacts-test-with-state
   (with-temp-buffer
     (qq-contacts-test-mode)
     (let (forced-pages)
       (cl-letf (((symbol-function 'appkit-ewoc-reconcile)
                  (lambda (_ewoc _entries _key-function &rest args)
                    (push (plist-get args :force-keys) forced-pages)
                    (when (= (length forced-pages) 1)
                      (qq-contacts--reconcile '((group . "20001"))))
                    (make-hash-table :test #'equal))))
         (qq-contacts--reconcile '((friend . "10001"))))
       (should (equal (nreverse forced-pages)
                      '(((friend . "10001")) ((group . "20001")))))
       (should-not qq-contacts--pending-force-keys)))))

(ert-deftest qq-contacts-reconcile-failure-restores-position-and-retries-keys ()
  (with-temp-buffer
    (qq-contacts-test-mode)
    (let (restored)
      (cl-letf (((symbol-function 'appkit-position-capture)
                 (lambda (&rest _args) 'synthetic-position))
                ((symbol-function 'appkit-position-restore)
                 (lambda (snapshot) (setq restored snapshot)))
                ((symbol-function 'appkit-ewoc-reconcile)
                 (lambda (&rest _args)
                   (error "synthetic reconciliation failure"))))
        (should-error
         (qq-contacts--reconcile '((friend . "synthetic-user")))))
      (should (eq restored 'synthetic-position))
      (should qq-contacts--dirty)
      (should (equal qq-contacts--pending-force-keys
                     '((friend . "synthetic-user"))))
      (should-not qq-contacts--rendering))))

(ert-deftest qq-contacts-targeted-invalidation-retains-key-after-printer-error ()
  (qq-contacts-test-with-state
   (with-temp-buffer
     (qq-contacts-test-mode)
     (qq-contacts--reconcile)
     (let* ((directory (appkit-directory-surface))
            (node (gethash '(friend . "10001")
                           (appkit-directory-surface-node-table directory))))
       (goto-char (ewoc-location node))
       (cl-letf (((symbol-function 'qq-media-avatar-cached-display-string)
                  (lambda (&rest _args) (error "synthetic printer failure"))))
         (should-error
          (qq-contacts--reconcile '((friend . "10001")))))
       (should qq-contacts--dirty)
       (should (equal qq-contacts--pending-force-keys
                      '((friend . "10001"))))))))

(ert-deftest qq-contacts-layout-uses-canonical-visible-window ()
  (qq-contacts-test-with-runtime-view
    (let* ((frame (selected-frame))
           (original-width (frame-width frame)))
      (unwind-protect
          (save-window-excursion
            (set-frame-width frame 180)
            (delete-other-windows)
            (switch-to-buffer buffer)
            (let* ((narrow (selected-window))
                   (wide (split-window-right 60)))
              (set-window-buffer wide buffer)
              (run-hook-with-args 'window-state-change-functions frame)
              (let ((narrow-width qq-contacts--fill-column))
                (should (= narrow-width (appkit-geometry-window-width
                                         narrow qq-contacts-margin-columns)))
                (select-window wide)
                (run-hook-with-args 'window-state-change-functions frame)
                (should (= qq-contacts--fill-column
                           (appkit-geometry-window-width
                            wide qq-contacts-margin-columns)))
                (should (> qq-contacts--fill-column narrow-width)))))
        (set-frame-width frame original-width)))))

(ert-deftest qq-contacts-group-member-search-reuses-a-renamed-owning-buffer ()
  (qq-contacts-test-with-runtime-view
    (let ((renamed (generate-new-buffer-name " *qq-contacts-members*"))
          call
          selected)
      (rename-buffer renamed)
      (cl-letf
          (((symbol-function 'qq-runtime-app)
            (lambda ()
              (ert-fail "member entrypoint ignored its registered view")))
           ((symbol-function 'qq-contacts-open)
            (lambda ()
              (ert-fail "member entrypoint opened a duplicate view")))
           ((symbol-function 'pop-to-buffer)
            (lambda (candidate &rest _arguments)
              (setq selected candidate)
              candidate))
           ((symbol-function 'appkit-surface-send) #'ignore)
           ((symbol-function 'qq-runtime-request)
            (lambda (request &rest _) (setq call request))))
        ;; Invoke the public entrypoint away from the owning buffer.  Its
        ;; Appkit view id, not the default buffer name, selects the target.
        (with-temp-buffer
          (setq-local qq-runtime--account-id "slot-a")
          (qq-contacts-search-group-members "20002" " Carol ")))
      (should (eq selected buffer))
      (should (eq buffer (appkit-surface-buffer view)))
      (should (equal renamed (buffer-name buffer)))
      (should-not (get-buffer qq-contacts-buffer-name))
      (should (equal call (qq-runtime-request-create 'group-members :group-id "20002")))
      (should (eq qq-contacts--view 'members))
      (should (equal qq-contacts--member-group-id "20002"))
      (should (equal qq-contacts--query "Carol")))))

(ert-deftest qq-contacts-group-member-search-uses-canonical-open-without-view ()
  (let ((qq-runtime--account-id
         (or (qq-state-store-account-id (qq-state-store-create)) (qq-runtime-current-account-id) "slot-a"))
        (qq-runtime--snapshots (qq-test--snapshots (qq-state-store-create)))
        (qq-runtime--accounts (make-hash-table :test #'equal)) (qq-runtime-change-hook nil)
        (qq-media-cache-update-hook nil) (buffer (generate-new-buffer " *qq-contacts-canonical-open*"))
        opened opened-view issued selected)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (qq-contacts-test-mode))
          (cl-letf (((symbol-function 'qq-contacts-open)
                     (lambda ()
                       (setq opened t)
                       (setq opened-view
                             (with-current-buffer buffer
                               (qq-contacts--ensure-view)))
                       buffer))
                    ((symbol-function 'get-buffer-create)
                     (lambda (&rest _arguments)
                       (ert-fail
                        "member entrypoint bypassed canonical open")))
                    ((symbol-function 'appkit-surface-send) #'ignore)
                    ((symbol-function 'qq-runtime-request)
                     (lambda (request &rest _)
                       (setq issued (list request (appkit-current-surface) (current-buffer)))))
                    ((symbol-function 'pop-to-buffer)
                     (lambda (candidate &rest _arguments)
                       (setq selected candidate)
                       candidate)))
            (qq-contacts-search-group-members "20002" "Carol"))
          (should opened)
          (should (equal (car issued) (qq-runtime-request-create 'group-members :group-id "20002")))
          (should (eq (cadr issued) opened-view))
          (should (eq (nth 2 issued) buffer))
          (should (eq selected buffer))
          (with-current-buffer buffer
            (should (eq qq-contacts--view 'members))
            (should (equal qq-contacts--member-group-id "20002"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (qq-runtime-stop-account "slot-a" t))))

(ert-deftest qq-contacts-bindings-follow-directory-and-root-conventions ()
  (should (eq (lookup-key qq-root-mode-map (kbd "c")) #'qq-contacts-open))
  (should (eq (lookup-key qq-contacts-mode-map (kbd "g"))
              #'qq-contacts-refresh))
  (should (eq (lookup-key qq-contacts-mode-map (kbd "RET"))
              #'qq-contacts-open-at-point))
  (should (eq (lookup-key qq-contacts-mode-map (kbd "i"))
              #'qq-contacts-open-info-at-point))
  (should (eq (lookup-key qq-contacts-mode-map (kbd "C"))
              #'qq-contacts-set-member-card-at-point))
  (should (eq (lookup-key qq-contacts-mode-map (kbd "T"))
              #'qq-contacts-set-member-special-title-at-point))
  (should (eq (lookup-key qq-contacts-mode-map (kbd "K"))
              #'qq-contacts-kick-member-at-point)))

(ert-deftest qq-contacts-keeps-two-account-directories-live-together ()
  (let ((qq-runtime--account-id
         (or (qq-state-store-account-id (qq-state-store-create)) (qq-runtime-current-account-id) "slot-a"))
        (qq-runtime--snapshots (qq-test--snapshots (qq-state-store-create)))
        (qq-runtime--accounts (make-hash-table :test #'equal))
        (qq-account--accounts (make-hash-table :test #'equal)) (qq-account--account-order nil)
        (qq-account--current-account-id nil) (qq-account-registry-changed-hook nil)
        (qq-account-selection-changed-hook nil) (qq-runtime-change-hook nil) (qq-media-cache-update-hook nil)
        buffer-a buffer-b)
    (unwind-protect
        (progn
          (qq-account--replace-accounts
           '(((account_id . "slot-a") (label . "Work")
              (phase . "online") (uin . "10001"))
             ((account_id . "slot-b") (label . "Personal")
              (phase . "online") (uin . "20002")))
           'ready "gateway-test")
          (qq-runtime-with-account "slot-a"
                                   (qq-runtime-transact #'qq-state-apply-friend-categories
                                 '(((category_id . 1) (name . "A")
                                    (friends . (((user_id . "11001")
                                                 (nickname . "Work Alice")))))))
                                   (qq-runtime-transact #'qq-state-apply-groups nil))
          (qq-runtime-with-account "slot-b"
                                   (qq-runtime-transact #'qq-state-apply-friend-categories
                                 '(((category_id . 1) (name . "B")
                                    (friends . (((user_id . "21001")
                                                 (nickname . "Personal Bob")))))))
                                   (qq-runtime-transact #'qq-state-apply-groups nil))
          (save-window-excursion
            (qq-runtime-with-account "slot-a"
                                     (setq buffer-a (qq-contacts-open)))
            (qq-runtime-with-account "slot-b"
                                     (setq buffer-b (qq-contacts-open))))
          (should (buffer-live-p buffer-a))
          (should (buffer-live-p buffer-b))
          (should-not (eq buffer-a buffer-b))
          (with-current-buffer buffer-a
            (should (equal qq-runtime--account-id "slot-a"))
            (should (string-match-p "Work Alice" (buffer-string)))
            (should-not (string-match-p "Personal Bob" (buffer-string))))
          (with-current-buffer buffer-b
            (should (equal qq-runtime--account-id "slot-b"))
            (should (string-match-p "Personal Bob" (buffer-string)))
            (should-not (string-match-p "Work Alice" (buffer-string)))))
      (qq-runtime-stop-account "slot-a" t)
      (qq-runtime-stop-account "slot-b" t)
      (dolist (buffer (list buffer-a buffer-b))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest qq-contacts-directory-fold-navigation-and-refresh-share-stable-entries ()
  (qq-contacts-test-with-state
   (with-temp-buffer
     (qq-contacts-test-mode)
     (save-window-excursion
       (switch-to-buffer (current-buffer))
       (qq-contacts--ensure-view)
       (let* ((directory (appkit-directory-surface))
              (category-key '(category . 7))
              (category-node
               (gethash category-key
                        (appkit-directory-surface-node-table directory))))
         (goto-char (ewoc-location category-node))
         (qq-contacts-open-at-point)
         (should-not (appkit-directory-fold-expanded-p directory category-key t))
         (should-not (appkit-directory-entry-for-key directory '(friend . "10001")))
         (qq-contacts-open-at-point)
         (should (appkit-directory-fold-expanded-p directory category-key t))
         (qq-contacts-next-item)
         (should (equal (appkit-directory-key-at-point) '(friend . "10002")))
         (forward-char 5)
         (let ((column (current-column)))
           (qq-contacts--reconcile '((friend . "10002")))
           (should (equal (appkit-directory-key-at-point) '(friend . "10002")))
           (should (= (current-column) column)))
         (qq-contacts-next-item)
         (should (equal (appkit-directory-key-at-point) '(friend . "10001")))
         (qq-contacts-previous-item)
         (should (equal (appkit-directory-key-at-point) '(friend . "10002"))))))))

(ert-deftest qq-contacts-category-position-survives-preceding-fold ()
  "Keep the current category and column when earlier rows disappear."
  (qq-contacts-test-with-state
   (with-temp-buffer
     (qq-contacts-test-mode)
     (save-window-excursion
       (switch-to-buffer (current-buffer))
       (qq-contacts--ensure-view)
       (let* ((directory (appkit-directory-surface))
              (key '(category . 3))
              (node (gethash key (appkit-directory-surface-node-table directory))))
         (goto-char (ewoc-location node))
         (forward-char 2)
         (let ((column (current-column)))
           (qq-contacts-toggle-category 7)
           (should (equal (appkit-directory-key-at-point) key))
           (should (= (current-column) column))
           (qq-contacts-toggle-category 7)
           (should (equal (appkit-directory-key-at-point) key))
           (should (= (current-column) column))))))))

(ert-deftest qq-contacts-member-search-filters-shared-page-and-opens-canonical-user ()
  (qq-contacts-test-with-runtime-view
    (let* ((member '((group_id . "20002") (group_name . "Emacs Group")
                     (user_id . "10003") (uid . "uid-c") (nickname . "Carol") (card . "C酱")))
           opened)
      (qq-operation-test--seed '(group-members "20002") :value (list (cons 'members (list member))))
      (cl-letf (((symbol-function 'pop-to-buffer) (lambda (&rest _) buffer))
                ((symbol-function 'qq-chat-open) (lambda (key) (setq opened key))))
        (qq-contacts-search-group-members "20002" " Carol ")
        (qq-operation-test--drain)
        (should (equal qq-contacts--query "Carol"))
        (should (equal (qq-contacts-test--entry-keys (qq-contacts--project-members))
                       '(navigation (section . members) (member . "10003"))))
        (appkit-directory-activate-entry (appkit-directory-surface) (qq-contacts--member-entry member))
        (should (equal opened "private:10003"))))))

(ert-deftest qq-contacts-member-actions-submit-data-without-patching-view-copies ()
  (with-temp-buffer
    (qq-contacts-test-mode)
    (let ((member '((group_id . "8209413637") (user_id . "9007199254741001")
                    (nickname . "Crab") (card . "Ferris") (title . "Maintainer")))
          intents)
      (let ((inhibit-read-only t))
        (insert "member")
        (add-text-properties (point-min) (point-max)
                             (list 'qq-contacts-row-type 'member 'qq-contacts-object member)))
      (goto-char (point-min))
      (cl-letf (((symbol-function 'qq-runtime-request) (lambda (request &rest _) (push request intents)))
                ((symbol-function 'yes-or-no-p) (lambda (_) t))
                ((symbol-function 'y-or-n-p) (lambda (_) t)))
        (qq-contacts-set-member-card-at-point "")
        (qq-contacts-set-member-special-title-at-point "Lead")
        (call-interactively #'qq-contacts-kick-member-at-point))
      (should (equal (alist-get 'card member) "Ferris"))
      (should (equal (nreverse intents) (list (qq-runtime-request-create 'member-setting :group-id "8209413637" :user-id "9007199254741001" :field 'card :new-value "") (qq-runtime-request-create 'member-setting :group-id "8209413637" :user-id "9007199254741001" :field 'title :new-value "Lead") (qq-runtime-request-create 'member-kick :group-id "8209413637" :user-id "9007199254741001" :reject-add-request t)))))))

(ert-deftest qq-contacts-refresh-outlives-surface-and-replacement-projects-result ()
  (qq-operation-test-with-app
    (with-temp-buffer
      (qq-contacts-test-mode)
      (qq-runtime-bind-account "slot-a")
      (qq-contacts-refresh) (qq-operation-test--drain)
      (let ((surface (appkit-current-surface)))
        (appkit-surface-stop surface)
        (dolist (call calls)
          (qq-operation-test--resolve call (if (equal (plist-get call :method) "contact.list_friends")
                                               '((account_id . "slot-a") (friends) (categories))
                                             '((account_id . "slot-a") (groups)))))
        (qq-operation-test--drain)
        (should (qq-state-friend-categories-loaded-p (qq-runtime-state)))
        (should (qq-state-groups-loaded-p (qq-runtime-state)))
        (should-not cancelled)
        (should-not (eq surface (qq-contacts--ensure-view)))))))

(provide 'qq-contacts-test)

;;; qq-contacts-test.el ends here
