;;; qq-favorite-emoji-test.el --- Tests for native favorites -*- lexical-binding: t; -*-

(require 'qq-test-state)

;;; Code:

(require 'ert)
(require 'qq-operation-test)
(require 'cl-lib)
(require 'qq-favorite-emoji)

(defconst qq-favorite-emoji-test-id
  "10001_0_0_1_01F97FC1C8118A7D09DE81124B346F67_195359_1d5176b6464998bd07f94c17d42012e5")

(defconst qq-favorite-emoji-test-entry
  `((favorite_emoji_id . ,qq-favorite-emoji-test-id)
    (md5 . "01f97fc1c8118a7d09de81124b346f67")
    (url . ,(concat "https://p.qpic.cn/qq_expression/10001/"
                    qq-favorite-emoji-test-id "/0"))))

(defmacro qq-favorite-emoji-test-with-account (&rest body)
  "Run BODY with one isolated selected managed account."
  (declare (indent 0) (debug t))
  `(let ((qq-runtime--snapshots (qq-test--snapshots (qq-state-store-create)))
         (qq-account--accounts (make-hash-table :test #'equal)) (qq-account--account-order nil)
         (qq-account--current-account-id nil) (qq-account--gateway-instance-id nil)
         (qq-account-registry-changed-hook nil) (qq-account-selection-changed-hook nil)
         (qq-resource--resources (make-hash-table :test #'equal)) (qq-resource--order nil)
         (qq-resource-changed-hook nil) (qq-favorite-emoji-changed-hook nil)
         (qq-request--active (make-hash-table :test #'eq))
         (qq-runtime--account-id nil) (qq-runtime--app nil)
         (qq-runtime--accounts (make-hash-table :test #'equal)) (qq-server--state 'ready))
     (unwind-protect
         (progn
           (qq-account--replace-accounts
            '(((account_id . "slot-a")
               (label . "Primary")
               (phase . "online")
               (uin . "10001")
               (uid . "u_self")
               (challenge)
               (problem)))
            'ready "gateway-test")
           (setq qq-runtime--account-id "slot-a")
           ,@body)
       (qq-runtime-stop)
       (qq-test-reset))))

(defun qq-favorite-emoji-test-catalog (&optional entries)
  "Return one exact native catalog containing ENTRIES."
  `((account_id . "slot-a")
    (owner . "10001")
    (max_entries . 500)
    (entries . ,(or entries (list (copy-tree qq-favorite-emoji-test-entry))))
    (source . "network")
    (refreshed_at . 1786300000)))

(defun qq-favorite-emoji-test-resource ()
  "Return one verified ready image Resource snapshot."
  (copy-tree
   '((resource_id . "res-11111111-1111-4111-8111-111111111111")
     (phase . "ready")
     (suggested_name . "favorite.png")
     (size . 11084)
     (media_type . "image/png")
     (digests
      . ((sha256 . "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
         (sha1 . "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")
         (md5 . "01f97fc1c8118a7d09de81124b346f67")))
     (created_at . 1786300000)
     (updated_at . 1786300000)
     (expires_at)
     (error))))

(defmacro qq-favorite-emoji-test-with-transport (&rest body)
  "Run BODY with deferred transport PENDING and RELEASED resource identities."
  (declare (indent 0) (debug t))
  `(let (pending released)
     (cl-letf (((symbol-function 'qq-server-ready-p) (lambda () t))
               ((symbol-function 'qq-server-capabilities)
                (lambda ()
                  '("favorite_emoji.list" "favorite_emoji.add"
                    "favorite_emoji.delete" "favorite_emoji.set_description"
                    "favorite_emoji.move" "resource.stage_local"
                    "resource.release")))
               ((symbol-function 'qq-server-send)
                (lambda (method params callback errback &optional _early)
                  (if (equal method "resource.release")
                      (progn
                        (push (alist-get 'resource_id params) released)
                        (funcall callback nil))
                    (push (list method params callback errback) pending))
                  "deferred-request")))
       ,@body)))

(defun qq-favorite-emoji-test-select-other-account ()
  "Select a second managed account without removing the original owner."
  (qq-account--upsert-account
   '((account_id . "slot-b") (label . "Secondary") (phase . "online")
     (uin . "10002") (uid . "u_other") (challenge) (problem))
   'test)
  (qq-account--set-current-account "slot-b")
  (setq qq-runtime--account-id "slot-b"))

(ert-deftest qq-favorite-emoji-list-retains-owner-across-account-switch ()
  (qq-favorite-emoji-test-with-account
    (qq-favorite-emoji-test-with-transport
      (qq-runtime-request (qq-runtime-request-create 'favorite-emojis) t)
      (qq-operation-test--drain)
      (qq-favorite-emoji-test-select-other-account)
      (funcall (nth 2 (assoc "favorite_emoji.list" pending)) (qq-favorite-emoji-test-catalog))
      (qq-operation-test--drain)
      (should (plist-get (qq-runtime-operation '(favorite-emojis) "slot-a") :ready))
      (should-not (qq-runtime-operation '(favorite-emojis) "slot-b"))
      (should (equal (qq-runtime-current-account-id) "slot-b")))))

(ert-deftest qq-favorite-emoji-catalog-rejects-schema-owner-and-duplicates ()
  (qq-favorite-emoji-test-with-account
    (let ((valid (qq-favorite-emoji-test-catalog)))
      (dolist
          (invalid
           (list
            (append valid '((unexpected . t)))
            (cons '(account_id . "slot-b") (cdr valid))
            (let ((copy (copy-tree valid)))
              (setf (alist-get 'owner copy) "10002")
              copy)
            (qq-favorite-emoji-test-catalog
             (list (copy-tree qq-favorite-emoji-test-entry)
                   (copy-tree qq-favorite-emoji-test-entry)))))
        (should-error
         (qq-favorite-emoji--project-catalog invalid "slot-a"))))))

(ert-deftest qq-favorite-emoji-materialize-projects-existing-resource-store ()
  (qq-favorite-emoji-test-with-account
    (let (sent-params materialized)
      (cl-letf (((symbol-function 'qq-server-ready-p) (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () '("favorite_emoji.materialize")))
                ((symbol-function 'qq-server-send)
                 (lambda (_method params callback _errback &optional _early)
                   (setq sent-params params)
                   (funcall
                    callback
                    `((account_id . "slot-a")
                      (entry . ,(copy-tree qq-favorite-emoji-test-entry))
                      (resource . ,(qq-favorite-emoji-test-resource))))
                   "favorite-materialize-request")))
        (qq-favorite-emoji-materialize
         qq-favorite-emoji-test-id
         (lambda (value) (setq materialized value)))
        (should
         (equal sent-params
                `((account_id . "slot-a")
                  (favorite_emoji_id . ,qq-favorite-emoji-test-id))))
        (let ((resource (alist-get 'resource materialized)))
          (should (equal (alist-get 'phase resource) "ready"))
          (should (equal resource (qq-resource (alist-get 'resource_id resource)))))))))

(ert-deftest qq-favorite-emoji-materialize-rejects-digest-contradiction-before-projection ()
  (qq-favorite-emoji-test-with-account
    (let ((resource (qq-favorite-emoji-test-resource)))
      (setf (alist-get 'md5 (alist-get 'digests resource))
            "ffffffffffffffffffffffffffffffff")
      (should-error
       (qq-favorite-emoji--project-materialized
        `((account_id . "slot-a")
          (entry . ,(copy-tree qq-favorite-emoji-test-entry))
          (resource . ,resource))
        "slot-a" qq-favorite-emoji-test-id))
      (should (= 0 (hash-table-count qq-resource--resources))))))

(ert-deftest qq-favorite-emoji-optional-description-distinguishes-unknown-and-cleared ()
  (qq-favorite-emoji-test-with-account
    (dolist (description '(unknown "" "Happy cat"))
      (let* ((entry (copy-tree qq-favorite-emoji-test-entry))
             (_ (unless (eq description 'unknown)
                  (setf (alist-get 'description entry) description)))
             (projected
              (car (alist-get
                    'entries
                    (qq-favorite-emoji--project-catalog
                     (qq-favorite-emoji-test-catalog (list entry)) "slot-a")))))
        (if (eq description 'unknown)
            (should-not (assq 'description projected))
          (should (equal (alist-get 'description projected) description)))))
    (dolist (extra '(((description)) ((description . 7)) ((note . "local"))))
      (should-error
       (qq-favorite-emoji--project-catalog
        (qq-favorite-emoji-test-catalog
         (list (append (copy-tree qq-favorite-emoji-test-entry) extra)))
        "slot-a")))))

(ert-deftest qq-favorite-emoji-add-rejects-account-and-resource-contradictions ()
  (qq-favorite-emoji-test-with-account
    (qq-favorite-emoji-test-with-transport
      (let* ((resource (qq-favorite-emoji-test-resource))
             (resource-id (alist-get 'resource_id resource))
             delivered changes failure)
        (qq-resource--upsert resource 'test)
        (add-hook 'qq-favorite-emoji-changed-hook
                  (lambda (&rest args) (push args changes)))
        (dolist (mismatch '(account digest schema))
          (let ((receipt `((account_id . "slot-a")
                           (entry . ,(copy-tree qq-favorite-emoji-test-entry)))))
            (pcase mismatch
              ('account (setf (alist-get 'account_id receipt) "slot-b"))
              ('digest (setf (alist-get 'md5 (alist-get 'entry receipt))
                             "ffffffffffffffffffffffffffffffff"))
              ('schema (push '(unexpected . t) receipt)))
            (qq-favorite-emoji-add
             resource-id (lambda (value) (push value delivered))
             (lambda (body _reason) (setq failure body)))
            (funcall (nth 2 (assoc "favorite_emoji.add" pending)) receipt)
            (should (equal (alist-get 'code failure) "invalid_gateway_result"))
            (should-not delivered)
            (should-not changes)
            (should-not released)))))))

(ert-deftest qq-favorite-emoji-delete-preserves-partial-and-unreported-results ()
  (qq-favorite-emoji-test-with-account
    (qq-favorite-emoji-test-with-transport
      (let ((receipt
             '((account_id . "slot-a")
               (entries . (((favorite_emoji_id . "opaque-success") (status . 0))
                           ((favorite_emoji_id . "opaque-failure") (status . 19))))
               (unreported_favorite_emoji_ids . ("opaque-unreported"))))
            delivered changes)
        (add-hook 'qq-favorite-emoji-changed-hook
                  (lambda (owner reason value)
                    (push (list owner reason value) changes)))
        (qq-favorite-emoji-delete
         '("opaque-success" "opaque-failure" "opaque-unreported")
         (lambda (value) (setq delivered value)))
        (funcall (nth 2 (assoc "favorite_emoji.delete" pending)) receipt)
        (should (equal delivered receipt))
        (should (equal changes (list (list "slot-a" 'delete receipt))))
        (should (equal (mapcar (lambda (entry) (alist-get 'status entry))
                              (alist-get 'entries delivered))
                       '(0 19)))
        (should (equal (alist-get 'unreported_favorite_emoji_ids delivered)
                       '("opaque-unreported")))))))

(ert-deftest qq-favorite-emoji-delete-rejects-unaccounted-and-contradictory-ids ()
  (qq-favorite-emoji-test-with-account
    (qq-favorite-emoji-test-with-transport
      (let (changes delivered failure)
        (add-hook 'qq-favorite-emoji-changed-hook
                  (lambda (&rest args) (push args changes)))
        (dolist (mismatch '(owner omitted duplicate unexpected overlap status))
          (let ((receipt
                 '((account_id . "slot-a")
                   (entries . (((favorite_emoji_id . "first") (status . 0))))
                   (unreported_favorite_emoji_ids . ("second")))))
            (setq receipt (copy-tree receipt))
            (pcase mismatch
              ('owner (setf (alist-get 'account_id receipt) "slot-b"))
              ('omitted (setf (alist-get 'unreported_favorite_emoji_ids receipt) nil))
              ('duplicate
               (setf (alist-get 'entries receipt)
                     (append (alist-get 'entries receipt)
                             (copy-tree (alist-get 'entries receipt)))))
              ('unexpected
               (setf (alist-get 'unreported_favorite_emoji_ids receipt) '("third")))
              ('overlap
               (setf (alist-get 'unreported_favorite_emoji_ids receipt) '("first")))
              ('status
               (setf (alist-get 'status (car (alist-get 'entries receipt))) -1)))
            (qq-favorite-emoji-delete
             '("first" "second") (lambda (value) (push value delivered))
             (lambda (body _reason) (setq failure body)))
            (funcall (nth 2 (assoc "favorite_emoji.delete" pending)) receipt)
            (should (equal (alist-get 'code failure) "invalid_gateway_result"))
            (should-not delivered)
            (should-not changes)))))))

(ert-deftest qq-favorite-emoji-description-rejects-mismatched-receipts ()
  (qq-favorite-emoji-test-with-account
    (qq-favorite-emoji-test-with-transport
      (let (changes delivered failure)
        (add-hook 'qq-favorite-emoji-changed-hook
                  (lambda (&rest args) (push args changes)))
        (dolist (mismatch '(account id text))
          (let ((receipt (copy-tree
                          '((account_id . "slot-a")
                            (favorite_emoji_id . "opaque-id") (description . "")))))
            (pcase mismatch
              ('account (setf (alist-get 'account_id receipt) "slot-b"))
              ('id (setf (alist-get 'favorite_emoji_id receipt) "another-id"))
              ('text (setf (alist-get 'description receipt) "not cleared")))
            (qq-favorite-emoji-set-description
             "opaque-id" "" (lambda (value) (push value delivered))
             (lambda (body _reason) (setq failure body)))
            (funcall (nth 2 (assoc "favorite_emoji.set_description" pending)) receipt)
            (should (equal (alist-get 'code failure) "invalid_gateway_result"))
            (should-not delivered)
            (should-not changes)))))))

(ert-deftest qq-favorite-emoji-move-rejects-contradictory-order ()
  (qq-favorite-emoji-test-with-account
    (qq-favorite-emoji-test-with-transport
      (let* ((other (copy-tree qq-favorite-emoji-test-entry))
             (_ (setf (alist-get 'favorite_emoji_id other) "other-opaque-id"))
             (wrong (qq-favorite-emoji-test-catalog
                     (list other (copy-tree qq-favorite-emoji-test-entry))))
             changes delivered failure)
        (add-hook 'qq-favorite-emoji-changed-hook
                  (lambda (&rest args) (push args changes)))
        (qq-favorite-emoji-move
         qq-favorite-emoji-test-id 1 (lambda (value) (setq delivered value))
         (lambda (body _reason) (setq failure body)))
        (funcall (nth 2 (assoc "favorite_emoji.move" pending)) wrong)
        (should (equal (alist-get 'code failure) "invalid_gateway_result"))
        (should-not delivered)
        (should-not changes)
        (qq-favorite-emoji-move
         qq-favorite-emoji-test-id 2 (lambda (value) (setq delivered value)))
        (funcall (nth 2 (assoc "favorite_emoji.move" pending)) wrong)
        (should (equal (alist-get 'entries delivered)
                       (alist-get 'entries wrong)))
        (should (eq (cadar changes) 'move))))))

(ert-deftest qq-favorite-emoji-description-hooks-and-callbacks-keep-original-owner ()
  (qq-favorite-emoji-test-with-account
    (qq-favorite-emoji-test-with-transport
      (let (observed callback-owner failure)
        (add-hook 'qq-favorite-emoji-changed-hook
                  (lambda (owner reason receipt)
                    (push (list owner reason receipt (qq-runtime-current-account-id))
                          observed)))
        (add-hook 'qq-favorite-emoji-changed-hook
                  (lambda (&rest _args) (error "consumer hook failed")))
        (qq-favorite-emoji-set-description
         "opaque-id" ""
         (lambda (_receipt)
           (setq callback-owner (qq-runtime-current-account-id))
           (error "consumer callback failed"))
         (lambda (body _reason) (setq failure body)))
        (qq-favorite-emoji-test-select-other-account)
        (funcall (nth 2 (assoc "favorite_emoji.set_description" pending))
                 '((account_id . "slot-a")
                   (favorite_emoji_id . "opaque-id") (description . "")))
        (should (equal callback-owner "slot-a"))
        (should (equal (caar observed) "slot-a"))
        (should (equal (nth 3 (car observed)) "slot-a"))
        (should (equal (alist-get 'description (nth 2 (car observed))) ""))
        (should-not failure)
        (should (equal (qq-runtime-current-account-id) "slot-b"))))))

(ert-deftest qq-favorite-emoji-add-file-keeps-owner-and-releases-after-settlement ()
  (qq-favorite-emoji-test-with-account
    (qq-favorite-emoji-test-with-transport
      (let ((path (make-temp-file "qq-favorite-test-" nil ".png" "fixture"))
            (resource (qq-favorite-emoji-test-resource))
            callback-owner changes request)
        (unwind-protect
            (progn
              (add-hook 'qq-favorite-emoji-changed-hook
                        (lambda (owner reason _receipt)
                          (push (list owner reason) changes)))
              (setq request
                    (qq-favorite-emoji-add-file
                     path (lambda (_receipt)
                            (setq callback-owner (qq-runtime-current-account-id))
                            (error "consumer failed after release"))))
              (qq-favorite-emoji-test-select-other-account)
              (let ((staging (copy-tree resource)))
                (setf (alist-get 'phase staging) "staging")
                (funcall (nth 2 (assoc "resource.stage_local" pending))
                         `((resource . ,staging))))
              (should-not (assoc "favorite_emoji.add" pending))
              (qq-resource--upsert resource 'ready)
              (should
               (equal (alist-get 'account_id
                                 (nth 1 (assoc "favorite_emoji.add" pending)))
                      "slot-a"))
              (should-not released)
              (funcall (nth 2 (assoc "favorite_emoji.add" pending))
                       `((account_id . "slot-a")
                         (entry . ,(copy-tree qq-favorite-emoji-test-entry))))
              (should (eq (qq-request-state request) 'settled))
              (should (equal released (list (alist-get 'resource_id resource))))
              (should (equal callback-owner "slot-a"))
              (should (equal changes '(("slot-a" add))))
              (should-not qq-resource-changed-hook)
              (should-not qq-account-registry-changed-hook)
              (should (= (hash-table-count qq-request--active) 0)))
          (delete-file path))))))

(ert-deftest qq-favorite-emoji-add-file-removal-cleans-each-asynchronous-phase ()
  (dolist (phase '(stage ready add))
    (qq-favorite-emoji-test-with-account
      (qq-favorite-emoji-test-with-transport
        (let* ((path (make-temp-file "qq-favorite-test-" nil ".png" "fixture"))
               (resource (qq-favorite-emoji-test-resource))
               (resource-id (alist-get 'resource_id resource))
               request delivered changes)
          (unwind-protect
              (progn
                (add-hook 'qq-favorite-emoji-changed-hook
                          (lambda (&rest args) (push args changes)))
                (setq request
                      (qq-favorite-emoji-add-file
                       path (lambda (value) (push value delivered))
                       (lambda (&rest value) (push value delivered))))
                (unless (eq phase 'stage)
                  (let ((snapshot (copy-tree resource)))
                    (when (eq phase 'ready)
                      (setf (alist-get 'phase snapshot) "staging"))
                    (funcall (nth 2 (assoc "resource.stage_local" pending))
                             `((resource . ,snapshot)))))
                (qq-account--remove-account "slot-a" 'removed)
                (should (eq (qq-request-state request) 'cancelled))
                (pcase phase
                  ('stage
                   (should-not released)
                   (funcall (nth 2 (assoc "resource.stage_local" pending))
                            `((resource . ,resource))))
                  ('ready (qq-resource--upsert resource 'late-ready))
                  ('add
                   (should-not released)
                   (funcall (nth 2 (assoc "favorite_emoji.add" pending))
                            `((account_id . "slot-a")
                              (entry . ,(copy-tree qq-favorite-emoji-test-entry))))))
                (should (equal released (list resource-id)))
                (unless (eq phase 'add)
                  (should-not (assoc "favorite_emoji.add" pending)))
                (should-not delivered)
                (should-not changes)
                (should-not qq-resource-changed-hook)
                (should-not qq-account-registry-changed-hook)
                (should (= (hash-table-count qq-request--active) 0)))
            (delete-file path)))))))

(ert-deftest qq-favorite-emoji-add-file-failure-releases-owned-resource ()
  (dolist (failure-phase '(ready add))
    (qq-favorite-emoji-test-with-account
      (qq-favorite-emoji-test-with-transport
        (let ((path (make-temp-file "qq-favorite-test-" nil ".png" "fixture"))
              (resource (qq-favorite-emoji-test-resource))
              request failure delivered)
          (unwind-protect
              (progn
                (setq request
                      (qq-favorite-emoji-add-file
                       path (lambda (value) (setq delivered value))
                       (lambda (body _reason) (setq failure body))))
                (when (eq failure-phase 'ready)
                  (setf (alist-get 'phase resource) "failed"
                        (alist-get 'error resource)
                        '((code . "stage_failed") (message . "Failed to stage"))))
                (funcall (nth 2 (assoc "resource.stage_local" pending))
                         `((resource . ,resource)))
                (when (eq failure-phase 'add)
                  (funcall (nth 3 (assoc "favorite_emoji.add" pending))
                           '((code . "upload_failed")) "Failed to upload"))
                (should (equal (alist-get 'code failure)
                               (if (eq failure-phase 'ready)
                                   "stage_failed" "upload_failed")))
                (should-not delivered)
                (should (eq (qq-request-state request) 'failed))
                (should (equal released (list (alist-get 'resource_id resource))))
                (should-not qq-resource-changed-hook)
                (should-not qq-account-registry-changed-hook))
            (delete-file path)))))))

(ert-deftest qq-favorite-emoji-removed-owner-rejects-late-mutation-without-recreation ()
  (qq-favorite-emoji-test-with-account
    (qq-favorite-emoji-test-with-transport
      (let (changes delivered failure failure-owner)
        (add-hook 'qq-favorite-emoji-changed-hook
                  (lambda (&rest args) (push args changes)))
        (qq-favorite-emoji-set-description
         "opaque-id" "cat" (lambda (value) (setq delivered value))
         (lambda (body _reason)
           (setq failure body failure-owner (qq-runtime-current-account-id))))
        (qq-account--remove-account "slot-a" 'removed)
        (qq-runtime-stop-account "slot-a" t)
        (should-not (gethash "slot-a" qq-runtime--snapshots))
        (funcall (nth 2 (assoc "favorite_emoji.set_description" pending))
                 '((account_id . "slot-a")
                   (favorite_emoji_id . "opaque-id") (description . "cat")))
        (should (equal (alist-get 'code failure) "account_removed"))
        (should (equal failure-owner "slot-a"))
        (should-not delivered)
        (should-not changes)
        (should-not (gethash "slot-a" qq-runtime--snapshots))
        (should-not (qq-runtime-account "slot-a"))))))

(provide 'qq-favorite-emoji-test)

;;; qq-favorite-emoji-test.el ends here
