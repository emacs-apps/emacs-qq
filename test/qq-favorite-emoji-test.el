;;; qq-favorite-emoji-test.el --- Tests for native favorites -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
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
  `(let ((qq-account--accounts (make-hash-table :test #'equal))
         (qq-account--account-order nil)
         (qq-account--current-account-id nil)
         (qq-account--gateway-instance-id nil)
         (qq-account-registry-changed-hook nil)
         (qq-account-selection-changed-hook nil)
         (qq-resource--resources (make-hash-table :test #'equal))
         (qq-resource--order nil)
         (qq-resource-changed-hook nil)
         (qq-runtime--app nil)
         (qq-runtime--accounts (make-hash-table :test #'equal))
         (qq-state--partitions (make-hash-table :test #'equal))
         (qq-state--active-account-id nil)
         (qq-server--state 'ready))
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
           (qq-state-select-account "slot-a")
           ,@body)
       (qq-runtime-stop)
       (qq-state-reset))))

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
    (error)))

(ert-deftest qq-favorite-emoji-list-validates-exact-account-catalog ()
  (qq-favorite-emoji-test-with-account
    (let (sent-method sent-params catalog)
      (cl-letf (((symbol-function 'qq-server-ready-p) (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () '("favorite_emoji.list")))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (setq sent-method method sent-params params)
                   (funcall callback (qq-favorite-emoji-test-catalog))
                   "favorite-list-request")))
        (should
         (equal
          (qq-favorite-emoji-list
           t (lambda (value) (setq catalog value)))
          "favorite-list-request"))
        (should (equal sent-method "favorite_emoji.list"))
        (should (equal sent-params '((account_id . "slot-a") (refresh . t))))
        (should (equal (alist-get 'entries catalog)
                       (list qq-favorite-emoji-test-entry)))))))

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

(provide 'qq-favorite-emoji-test)

;;; qq-favorite-emoji-test.el ends here
