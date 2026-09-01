;;; qq-contacts-test.el --- Tests for native QQ directory -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-contacts)
(require 'qq-root)

(defmacro qq-contacts-test-with-state (&rest body)
  "Run BODY with deterministic native directory fixtures."
  `(let ((qq-state-change-hook nil)
         (qq-media-cache-update-hook nil)
         (qq-runtime--accounts (make-hash-table :test #'equal))
         (qq-state--partitions (make-hash-table :test #'equal))
         (qq-state--active-account-id nil))
     (when-let* ((buffer (get-buffer qq-contacts-buffer-name)))
       (kill-buffer buffer))
     (unwind-protect
         (qq-runtime-with-account "slot-a"
           (qq-state-reset)
           (qq-state-apply-friend-categories
            '(((category_id . 7) (sort_id . 1) (name . "工作")
               (online_count . 1)
               (friends
                . (((user_id . "10002") (nickname . "Bob") (remark))
                   ((user_id . "10001") (nickname . "Alice")
                    (remark . "A姐")))))
              ((category_id . 3) (sort_id . 2) (name . "空分组")
               (online_count . 0) (friends))))
           (qq-state-apply-groups
            '(((group_id . "20002") (group_name . "Dormant Group")
               (group_remark) (member_count . 20) (max_member_count . 500)
               (pinned . :false) (self_permission . "member"))
              ((group_id . "20001") (group_name . "Recent Group")
               (group_remark . "常用群") (member_count . 40)
               (max_member_count . 500) (pinned . t)
               (self_permission . "admin"))))
           (qq-state-upsert-session
            "group:20001"
            '((type . group) (target-id . "20001") (title . "Recent Group"))
            nil)
           (setq qq-state--recent-session-keys '("group:20001"))
           (puthash "group:20001" t qq-state--recent-session-key-set)
           ,@body)
       (when-let* ((buffer (get-buffer qq-contacts-buffer-name)))
         (kill-buffer buffer))
       (qq-runtime-stop-account "slot-a" t)
       (qq-state-reset))))

(defmacro qq-contacts-test-with-runtime-view (&rest body)
  "Run BODY in an isolated live contacts view.

BODY may refer to the lexical variables `app', `buffer', and `view'."
  (declare (indent 0) (debug t))
  `(let* ((qq-contacts-buffer-name
           (generate-new-buffer-name " *qq-contacts-test*"))
          (qq-runtime--accounts (make-hash-table :test #'equal))
          (qq-state--partitions (make-hash-table :test #'equal))
          (qq-state--active-account-id nil)
          (runtime (qq-runtime-ensure-account "slot-a"))
          (app (qq-runtime-account-app runtime))
          (buffer (get-buffer-create qq-contacts-buffer-name))
          view)
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
  (mapcar #'qq-contacts--entry-key entries))

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
     (puthash 7 t qq-contacts--collapsed-categories)
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
     (qq-state-upsert-session
      "group:20002" '((type . group) (target-id . "20002")) nil)
     (should
      (equal (qq-contacts-test--entry-keys
              (qq-contacts--project-groups t))
             '(navigation (group . "20002")))))))








(ert-deftest qq-contacts-navigation-buttons-are-real-and-switch-view ()
  (qq-contacts-test-with-state
   (with-temp-buffer
     (qq-contacts-test-mode)
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

(ert-deftest qq-contacts-synchronous-refresh-does-not-retain-stale-tokens ()
  (with-temp-buffer
    (qq-contacts-test-mode)
    (cl-letf (((symbol-function 'qq-contacts--queue-view-sync) #'ignore)
              ((symbol-function 'qq-core-refresh-friend-categories)
               (lambda (callback &optional _errback)
                 (funcall callback nil)
                 'already-finished-friends))
              ((symbol-function 'qq-core-refresh-joined-groups)
               (lambda (callback &optional _errback)
                 (funcall callback nil)
                 'already-finished-groups)))
      (qq-contacts-refresh)
      (should-not qq-contacts--loading)
      (should-not qq-contacts--refresh-owner)
      (should-not qq-contacts--refresh-parts)
      (should-not qq-contacts--friend-request)
      (should-not qq-contacts--group-request))))

(ert-deftest qq-contacts-media-update-targets-only-exact-row-key ()
  (let ((buffer (get-buffer-create qq-contacts-buffer-name)) forced)
    (unwind-protect
        (with-current-buffer buffer
          (qq-contacts-test-mode)
          (qq-contacts--ensure-view)
          (cl-letf (((symbol-function 'qq-contacts--queue-view-sync)
                     (lambda (_view &optional keys) (setq forced keys))))
            (qq-contacts--handle-media-cache-update "avatar:10001")
            (should (equal forced '((friend . "10001")
                                    (member . "10001"))))
            (setq forced nil)
            (qq-contacts--handle-media-cache-update "forward-image:x")
            (should-not forced)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest qq-contacts-renamed-buffer-hooks-use-the-registered-view ()
  (qq-contacts-test-with-runtime-view
    (let ((renamed (generate-new-buffer-name " *qq-contacts-renamed*"))
          calls)
      (rename-buffer renamed)
      (cl-letf (((symbol-function 'qq-runtime-app)
                 (lambda ()
                   (ert-fail "contacts hook started a runtime app")))
                ((symbol-function 'qq-contacts--queue-view-sync)
                 (lambda (candidate &optional keys)
                   (push (list candidate keys (current-buffer)) calls))))
        (qq-contacts--handle-state-change
         '(:type friends-refreshed :account-id "slot-a"))
        (qq-contacts--handle-media-cache-update "avatar:10001"))
      (setq calls (nreverse calls))
      (should (= 2 (length calls)))
      (dolist (call calls)
        (should (eq view (nth 0 call)))
        (should (eq buffer (nth 2 call))))
      (should-not (nth 1 (nth 0 calls)))
      (should
       (equal '((friend . "10001")
                (member . "10001"))
              (nth 1 (nth 1 calls))))
      (should (equal renamed (buffer-name buffer)))
      (should-not (get-buffer qq-contacts-buffer-name)))))

(ert-deftest qq-contacts-hooks-do-not-create-a-runtime-app-when-closed ()
  (let ((qq-runtime--app nil))
    (cl-letf (((symbol-function 'qq-runtime-app)
               (lambda ()
                 (ert-fail "closed contacts hook created a runtime app")))
              ((symbol-function 'qq-contacts--queue-view-sync)
               (lambda (&rest _arguments)
                 (ert-fail "closed contacts hook requested a sync"))))
      (qq-contacts--handle-state-change '(:type friends-refreshed))
      (qq-contacts--handle-media-cache-update "avatar:10001"))
    (should-not qq-runtime--app)))

(ert-deftest qq-contacts-queue-view-sync-uses-atomic-appkit-request ()
  (qq-contacts-test-with-runtime-view
    (let ((forced '((friend . "10001") (member . "10001")))
          calls)
      (cl-letf (((symbol-function 'appkit-request-sync)
                 (lambda (candidate &rest arguments)
                   (push (cons candidate arguments) calls)
                   'owned-timer))
                ((symbol-function 'appkit-invalidate)
                 (lambda (&rest _arguments)
                   (ert-fail "contacts queue used bare invalidation")))
                ((symbol-function 'appkit-schedule-sync)
                 (lambda (&rest _arguments)
                   (ert-fail "contacts queue used bare scheduling")))
                ((symbol-function 'appkit-sync-invalidations)
                 (lambda (&rest _arguments)
                   (ert-fail "contacts queue synchronized inline"))))
        (should (eq 'owned-timer
                    (qq-contacts--queue-view-sync view forced)))
        (should (eq 'owned-timer
                    (qq-contacts--queue-view-sync view)))
        (appkit-kill-view view)
        (should-not (qq-contacts--queue-view-sync view forced)))
      (setq calls (nreverse calls))
      (should
       (equal calls
              (list (list view :entries forced)
                    (list view :structure t :part 'directory)))))))


(ert-deftest qq-contacts-refresh-callback-rejects-replacement-view ()
  (qq-contacts-test-with-runtime-view
    (let (friend-success friend-failure group-success group-failure syncs)
      (cl-letf (((symbol-function 'qq-core-refresh-friend-categories)
                 (lambda (callback &optional errback)
                   (setq friend-success callback
                         friend-failure errback)
                   'friend-refresh-request))
                ((symbol-function 'qq-core-refresh-joined-groups)
                 (lambda (callback &optional errback)
                   (setq group-success callback
                         group-failure errback)
                   'group-refresh-request))
                ((symbol-function 'qq-request-cancel) #'ignore)
                ((symbol-function 'appkit-request-sync)
                 (lambda (candidate &rest arguments)
                   (push (cons candidate arguments) syncs)
                   'owned-timer)))
        (qq-contacts-refresh)
        (should (cl-every (lambda (call) (eq (car call) view)) syncs))
        (appkit-kill-view view)
        (let ((replacement (qq-contacts--ensure-view))
              (replacement-owner (list 'replacement-refresh)))
          (should-not (eq replacement view))
          (setq qq-contacts--refresh-owner replacement-owner
                qq-contacts--refresh-parts '(friends groups)
                qq-contacts--refresh-pending 2
                qq-contacts--loading t
                qq-contacts--error "replacement state"
                syncs nil)
          (funcall friend-success nil)
          (funcall friend-failure nil "stale friend failure")
          (funcall group-success nil)
          (funcall group-failure nil "stale group failure")
          (should (eq qq-contacts--refresh-owner replacement-owner))
          (should (equal qq-contacts--refresh-parts '(friends groups)))
          (should (= qq-contacts--refresh-pending 2))
          (should qq-contacts--loading)
          (should (equal qq-contacts--error "replacement state"))
          (should-not syncs))))))

(ert-deftest qq-contacts-position-only-does-not-reconcile ()
  (with-temp-buffer
    (qq-contacts-test-mode)
    (let ((view (qq-contacts--ensure-view))
          (invalidations (appkit-invalidations-create)))
      (setf (appkit-invalidations-position-p invalidations) t)
      (cl-letf (((symbol-function 'qq-contacts--project-entries)
                 (lambda ()
                   (ert-fail "position-only sync rebuilt contacts"))))
        (qq-contacts--sync-invalidations view invalidations nil)))))

(ert-deftest qq-contacts-visible-avatar-update-invalidates-only-target-rows ()
  (with-temp-buffer
    (qq-contacts-test-mode)
    (let ((view (qq-contacts--ensure-view)))
      (setq qq-contacts--fill-column 80)
      (let (invalidated)
        (cl-letf (((symbol-function 'qq-contacts--displayed-p) (lambda () t))
                  ((symbol-function 'qq-contacts--usable-width) (lambda () 80))
                  ((symbol-function 'qq-contacts--project-entries)
                   (lambda ()
                     (ert-fail "avatar update rebuilt full projection")))
                  ((symbol-function 'qq-contacts--invalidate-keys)
                   (lambda (keys) (setq invalidated keys))))
          (qq-contacts--request-reconcile
           '((friend . "10001") (member . "10001")))
          (appkit-sync-invalidations view)
          (should (equal invalidated
                         '((friend . "10001")
                           (member . "10001")))))))))

(ert-deftest qq-contacts-hidden-avatar-update-retains-forced-row-keys ()
  (qq-contacts-test-with-state
   (with-temp-buffer
     (qq-contacts-test-mode)
     (let ((view (qq-contacts--ensure-view)))
       (cl-letf (((symbol-function 'qq-contacts--displayed-p)
                  (lambda () nil)))
         (qq-contacts--request-reconcile '((group . "20002"))))
       (appkit-sync-invalidations view)
       (should qq-contacts--dirty)
       (should (equal qq-contacts--pending-force-keys
                      '((group . "20002"))))
       (let (forced)
         (cl-letf (((symbol-function 'appkit-ewoc-reconcile)
                    (lambda (_ewoc _entries _key-function &rest args)
                      (setq forced (plist-get args :force-keys))
                      (make-hash-table :test #'equal))))
           (qq-contacts--reconcile))
         (should (equal forced '((group . "20002"))))
         (should-not qq-contacts--pending-force-keys)
         (should-not qq-contacts--dirty))))))

(ert-deftest qq-contacts-state-and-completion-coalesce-one-appkit-sync ()
  (let ((buffer (get-buffer-create qq-contacts-buffer-name))
        sync-count)
    (unwind-protect
        (with-current-buffer buffer
          (qq-contacts-test-mode)
          (let* ((view (qq-contacts--ensure-view))
                 (owner (list 'synthetic-refresh-owner)))
            (setq qq-contacts--refresh-owner owner
                  qq-contacts--refresh-parts '(friends)
                  qq-contacts--refresh-pending 1
                  qq-contacts--loading t)
            (cl-letf (((symbol-function 'qq-contacts--displayed-p)
                       (lambda () t))
                      ((symbol-function 'qq-contacts--reconcile)
                       (lambda (&optional _keys)
                         (setq sync-count (1+ (or sync-count 0))))))
              ;; One native response first publishes authoritative state and
              ;; then settles the per-request loading owner.  Repeated state
              ;; notifications before the timer fires still form one snapshot.
              (qq-contacts--handle-state-change '(:type friends-refreshed))
              (qq-contacts--handle-state-change '(:type friends-refreshed))
              (qq-contacts--finish-refresh-part view buffer owner 'friends)
              (should-not sync-count)
              (appkit-sync-invalidations view)
              (should (= sync-count 1))
              (should-not qq-contacts--loading)
              (should-not qq-contacts--refresh-owner))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))


(ert-deftest qq-contacts-dead-view-makes-window-callbacks-inert ()
  (with-temp-buffer
    (qq-contacts-test-mode)
    (let* ((view (qq-contacts--ensure-view))
           (app (appkit-view-app view))
           queued
           avatar-started)
      (let ((inhibit-read-only t))
        (insert "Synthetic friend\n")
        (add-text-properties
         (point-min) (point-max)
         '(qq-contacts-row-type friend
           qq-contacts-object ((user_id . "10001")))))
      (setq qq-contacts--dirty t
            qq-contacts--fill-column 80)
      (appkit-kill-view view)
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
                ((symbol-function 'qq-contacts--queue-view-sync)
                 (lambda (&rest _args) (setq queued t)))
                ((symbol-function 'qq-media-avatar-image)
                 (lambda (&rest _args) (setq avatar-started t))))
        (qq-contacts--window-buffer-change 'synthetic-window)
        (qq-contacts--window-size-change)
        (qq-contacts--window-scroll 'synthetic-window nil))
      (should-not queued)
      (should-not avatar-started)
      (should-not (appkit-current-view))
      (should-not (appkit-view-for-id app qq-contacts--view-id)))))

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
  (with-temp-buffer
    (qq-contacts-test-mode)
    (cl-letf (((symbol-function 'appkit-ewoc-invalidate-key)
               (lambda (&rest _args) (error "synthetic printer failure"))))
      (should-error
       (qq-contacts--invalidate-keys '((friend . "10001")))))
    (should qq-contacts--dirty)
    (should (equal qq-contacts--pending-force-keys
                   '((friend . "10001"))))))





(ert-deftest qq-contacts-layout-uses-narrowest-visible-window ()
  (with-temp-buffer
    (qq-contacts-test-mode)
    (cl-letf (((symbol-function 'get-buffer-window-list)
               (lambda (&rest _args) '(wide narrow)))
              ((symbol-function 'appkit-geometry-window-width)
               (lambda (window _margin)
                 (if (eq window 'wide) 120 72))))
      (should (= (qq-contacts--usable-width) 72)))))

(ert-deftest qq-contacts-ignores-ordinary-session-state-events ()
  (let ((buffer (get-buffer-create qq-contacts-buffer-name)) reconciles)
    (unwind-protect
        (with-current-buffer buffer
          (qq-contacts-test-mode)
          (qq-contacts--ensure-view)
          (cl-letf (((symbol-function 'qq-contacts--queue-view-sync)
                     (lambda (&rest _args)
                       (setq reconciles (1+ (or reconciles 0))))))
            (qq-contacts--handle-state-change
             '(:type session :account-id "slot-a"))
            (should-not reconciles)
            (qq-contacts--handle-state-change
             '(:type sessions-refreshed :account-id "slot-a"))
            (should (= reconciles 1))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest qq-contacts-mode-cancels-native-work-before-major-mode-change ()
  (with-temp-buffer
    (qq-contacts-test-mode)
    (should (memq #'qq-contacts--cancel-refresh change-major-mode-hook))
    (should (memq #'qq-contacts--cancel-search change-major-mode-hook))))


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
           ((symbol-function 'qq-contacts--queue-view-sync) #'ignore)
           ((symbol-function 'qq-core-search-group-members)
            (lambda (group-id query callback &optional _errback _limit)
              (setq call (list group-id query))
              (funcall callback nil)
              'finished)))
        ;; Invoke the public entrypoint away from the owning buffer.  Its
        ;; Appkit view id, not the default buffer name, selects the target.
        (with-temp-buffer
          (setq-local qq-runtime--account-id "slot-a")
          (qq-contacts-search-group-members "20002" " Carol ")))
      (should (eq selected buffer))
      (should (eq buffer (appkit-view-buffer view)))
      (should (equal renamed (buffer-name buffer)))
      (should-not (get-buffer qq-contacts-buffer-name))
      (should (equal call '("20002" "Carol")))
      (should (eq qq-contacts--view 'members))
      (should (equal qq-contacts--member-group-id "20002"))
      (should-not qq-contacts--search-pending))))

(ert-deftest qq-contacts-group-member-search-uses-canonical-open-without-view ()
  (let ((qq-runtime--app nil)
        (buffer (generate-new-buffer " *qq-contacts-canonical-open*"))
        opened
        opened-view
        issued
        selected)
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
                    ((symbol-function 'qq-contacts--queue-view-sync) #'ignore)
                    ((symbol-function 'qq-contacts--issue-search-request)
                     (lambda (candidate section &rest _arguments)
                       (setq issued
                             (list candidate section (current-buffer)))))
                    ((symbol-function 'pop-to-buffer)
                     (lambda (candidate &rest _arguments)
                       (setq selected candidate)
                       candidate)))
            (qq-contacts-search-group-members "20002" "Carol"))
          (should opened)
          (should (eq (car issued) opened-view))
          (should (equal (cdr issued) (list 'members buffer)))
          (should (eq selected buffer))
          (with-current-buffer buffer
            (should (eq qq-contacts--view 'members))
            (should (equal qq-contacts--member-group-id "20002"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (qq-runtime-stop))))

(ert-deftest qq-contacts-group-member-search-is-native-and-actionable ()
  (let ((buffer (get-buffer-create qq-contacts-buffer-name)) call opened)
    (unwind-protect
        (cl-letf
            (((symbol-function 'qq-contacts-open)
              (lambda () buffer))
             ((symbol-function 'pop-to-buffer) (lambda (&rest _args) buffer))
             ((symbol-function 'qq-contacts--queue-view-sync) #'ignore)
             ((symbol-function 'qq-core-search-group-members)
              (lambda (group-id query callback &optional _errback _limit)
                (setq call (list group-id query))
                (funcall callback
                         '(((group_id . "20002")
                            (group_name . "Emacs Group")
                            (user_id . "10003") (uid . "uid-c")
                            (nickname . "Carol") (remark . "")
                            (card . "C酱") (is_friend . :false))))
                'finished))
             ((symbol-function 'qq-chat-open)
              (lambda (key) (setq opened key))))
          (with-current-buffer buffer
            (qq-contacts-test-mode)
            (qq-contacts-search-group-members "20002" " Carol "))
          (with-current-buffer buffer
            (should (equal call '("20002" "Carol")))
            (should (eq qq-contacts--view 'members))
            (should-not qq-contacts--search-pending)
            (should
             (equal (qq-contacts-test--entry-keys
                     (qq-contacts--project-members))
                    '(navigation (section . members) (member . "10003"))))
            (qq-contacts--activate-entry
             (qq-contacts--member-entry (car qq-contacts--search-members)))
            (should (equal opened "private:10003"))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest qq-contacts-member-setting-commands-update-row-after-receipt ()
  (with-temp-buffer
    (qq-contacts-test-mode)
    (let* ((member
            '((group_id . "8209413637") (group_name . "Protocol Lab")
              (user_id . "9007199254741001") (uid . "u_member")
              (nickname . "Crab") (card . "Ferris")
              (title . "Maintainer")))
           (qq-contacts--search-members (list member))
           calls)
      (let ((inhibit-read-only t))
        (insert "member\n")
        (add-text-properties
         (point-min) (point-max)
         (list 'qq-contacts-row-type 'member
               'qq-contacts-object member)))
      (goto-char (point-min))
      (cl-letf
          (((symbol-function 'qq-core-set-group-member-card)
            (lambda (group-id user-id value callback &optional _errback)
              (push (list 'card group-id user-id value) calls)
              (funcall callback '((status . "ok")))))
           ((symbol-function 'qq-core-set-group-member-special-title)
            (lambda (group-id user-id value callback &optional _errback)
              (push (list 'title group-id user-id value) calls)
              (funcall callback '((status . "ok")))))
           ((symbol-function 'qq-contacts--queue-view-sync) #'ignore)
           ((symbol-function 'message) #'ignore))
        (qq-contacts-set-member-card-at-point "")
        (qq-contacts-set-member-special-title-at-point "Lead"))
      (should-not (alist-get 'card member))
      (should (equal (alist-get 'title member) "Lead"))
      (should
       (equal
        (nreverse calls)
        '((card "8209413637" "9007199254741001" "")
          (title "8209413637" "9007199254741001" "Lead")))))))

(ert-deftest qq-contacts-member-kick-confirms-before-dispatch-and-removes-row ()
  (with-temp-buffer
    (qq-contacts-test-mode)
    (let* ((member
            '((group_id . "8209413637") (group_name . "Protocol Lab")
              (user_id . "9007199254741001") (uid . "u_member")
              (nickname . "Crab") (card . "Ferris")))
           (qq-contacts--search-members (list member))
           calls prompts)
      (let ((inhibit-read-only t))
        (insert "member\n")
        (add-text-properties
         (point-min) (point-max)
         (list 'qq-contacts-row-type 'member
               'qq-contacts-object member)))
      (goto-char (point-min))
      (cl-letf
          (((symbol-function 'qq-core-kick-group-member)
            (lambda (&rest arguments) (push arguments calls)))
           ((symbol-function 'yes-or-no-p)
            (lambda (prompt) (push prompt prompts) nil)))
        (should-error (call-interactively
                       #'qq-contacts-kick-member-at-point)
                      :type 'user-error))
      (should-not calls)
      (should (equal qq-contacts--search-members (list member)))
      (cl-letf
          (((symbol-function 'qq-core-kick-group-member)
            (lambda (group-id user-id reject callback &optional _errback)
              (push (list group-id user-id reject) calls)
              (funcall callback '((reject_add_request . t)))))
           ((symbol-function 'yes-or-no-p)
            (lambda (prompt) (push prompt prompts) t))
           ((symbol-function 'y-or-n-p)
            (lambda (prompt) (push prompt prompts) t))
           ((symbol-function 'qq-contacts--queue-view-sync) #'ignore)
           ((symbol-function 'message) #'ignore))
        (call-interactively #'qq-contacts-kick-member-at-point))
      (should (equal calls
                     '(("8209413637" "9007199254741001" t))))
      (should-not qq-contacts--search-members)
      (should (seq-some
               (lambda (prompt)
                 (string-match-p "QQ 9007199254741001" prompt))
               prompts)))))

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
  (let ((qq-runtime--accounts (make-hash-table :test #'equal))
        (qq-state--partitions (make-hash-table :test #'equal))
        (qq-state--active-account-id nil)
        (qq-account--accounts (make-hash-table :test #'equal))
        (qq-account--account-order nil)
        (qq-account--current-account-id nil)
        (qq-account-registry-changed-hook nil)
        (qq-account-selection-changed-hook nil)
        (qq-state-change-hook nil)
        (qq-media-cache-update-hook nil)
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
            (qq-state-apply-friend-categories
             '(((category_id . 1) (name . "A")
                (friends . (((user_id . "11001")
                             (nickname . "Work Alice")))))))
            (qq-state-apply-groups nil))
          (qq-runtime-with-account "slot-b"
            (qq-state-apply-friend-categories
             '(((category_id . 1) (name . "B")
                (friends . (((user_id . "21001")
                             (nickname . "Personal Bob")))))))
            (qq-state-apply-groups nil))
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

(provide 'qq-contacts-test)

;;; qq-contacts-test.el ends here
