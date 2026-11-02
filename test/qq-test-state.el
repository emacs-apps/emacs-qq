;;; qq-test-state.el --- Account value fixtures -*- lexical-binding: t; -*-
(require 'qq-runtime)

(defun qq-test--snapshots (initial)
  "Build an isolated retained model from INITIAL."
  (let* ((owner (or (qq-state-store-account-id initial)
                    (qq-runtime-current-account-id) "slot-a"))
         (table (make-hash-table :test #'equal)))
    (dolist (field '(:sessions :friends-by-id :groups-by-id :recent-session-key-set :actions))
      (setq initial (appkit-data-with initial (() field (qq-test-value (appkit-data-get initial field))))))
    (puthash owner (qq-state-partition--create
                    :store (appkit-data-with initial (() :account-id owner))) table)
    table))

(defmacro qq-test-with-state (initial &rest body)
  "Run BODY against INITIAL using the real account commit path."
  (declare (indent 1))
  `(let* ((initial-state ,initial)
          (qq-runtime--account-id (or (qq-state-store-account-id initial-state) "slot-a"))
          (qq-runtime--accounts (make-hash-table :test #'equal))
          (qq-runtime--snapshots (qq-test--snapshots initial-state)))
     (unwind-protect (progn ,@body) (qq-runtime-stop))))

(defun qq-test-set-field (state field value)
  "Seed a domain FIELD with VALUE in STATE's owning runtime."
  (let ((qq-runtime-change-hook nil))
    (qq-runtime-with-account (or (qq-state-store-account-id state) "slot-a")
      (qq-runtime-transact
       (lambda (current)
         (cons (appkit-data-with current (() field (qq-test-value value))) value))))))

(defun qq-test-put (state field key value)
  "Seed an indexed fixture without exposing mutable tables in production."
  (let ((qq-runtime-change-hook nil))
    (qq-runtime-with-account (or (qq-state-store-account-id state) "slot-a")
      (qq-runtime-transact
       (lambda (current)
         (if (eq field :messages-by-session)
             (progn
               (setq current (appkit-data-with current
                               ((:messages-by-session) key appkit-data-delete)))
               (dolist (message value)
                 (let ((row (qq-state--merge-alists message (list (cons 'session-key key)))))
                   (unless (alist-get 'order row)
                     (setq row (qq-state--merge-alists
                                row (list (cons 'order (qq-state--step current
                                                         (qq-state--next-message-order current)))))))
                   (qq-state--step current (qq-state--put-message current key row))))
               (cons current value))
           (cons (appkit-data-with current
                   (() field (appkit-data-map-set (appkit-data-get current field) key value)))
                 value)))))))

(defun qq-test-reset ()
  "Reset the fixture account, including stopped snapshots, without reopening it."
  (let ((owner (or (qq-runtime-current-account-id) "slot-a")))
    (if (qq-runtime-account owner)
        (qq-runtime-with-account owner (qq-runtime-transact #'qq-state-reset))
      (puthash owner (qq-state-partition--create :store (qq-state-store-create :account-id owner))
               qq-runtime--snapshots))))

(defun qq-test-value (value)
  "Convert mutable fixture table builders to persistent values."
  (if (hash-table-p value)
      (let (entries)
        (maphash (lambda (key item) (push (cons key item) entries)) value)
        (appkit-data-map-edit appkit-data-map-empty entries))
    value))

(defun qq-test-drain (&optional app)
  "Deliver queued account and surface transitions without starting an App."
  (when-let* ((app (or app (when-let* ((runtime (qq-runtime-account (qq-runtime-current-account-id))))
                            (qq-runtime-account-app runtime)))))
    (let ((turns 0) pending)
      (while
          (progn
            (setq pending nil)
            (dolist (loop (cons (appkit-app-loop app)
                               (mapcar (lambda (entry) (appkit-surface-loop (cdr entry)))
                                       (hash-table-values (appkit-app-surfaces app)))))
              (when (> (appkit-loop-pending-count loop) 0)
                (setq pending t)
                (appkit-loop-run-pass loop)))
            (and pending (< (cl-incf turns) 20))))
      (should (< turns 20)))))

(provide 'qq-test-state)
