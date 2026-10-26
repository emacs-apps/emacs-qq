;;; qq-guild.el --- Account-owned QQ Guild state and operations -*- lexical-binding: t; -*-

;;; Commentary:
;; The account App owns Guild protocol objects and finite RPC Effects.
;; Directory and chat Surfaces subscribe to committed state; messages retain
;; their native guild/channel/tiny-ID/sequence identities throughout.

;;; Code:
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'appkit)
(require 'qq-runtime)
(require 'qq-rpc)
(require 'qq-account)
(require 'qq-protocol)

(defun qq-guild--channel-p (value)
  "Return non-nil for an exact guild/channel VALUE."
  (and (qq-server-wire-exact-object-keys-p value '(guild_id channel_id))
       (qq-protocol-uint64-decimal-p (alist-get 'guild_id value))
       (qq-protocol-uint64-decimal-p (alist-get 'channel_id value))))

(defun qq-guild--same-channel-p (left right)
  "Compare native routes LEFT and RIGHT independently of JSON field order."
  (and (qq-guild--channel-p left) (qq-guild--channel-p right)
       (equal (alist-get 'guild_id left) (alist-get 'guild_id right))
       (equal (alist-get 'channel_id left) (alist-get 'channel_id right))))

(defun qq-guild--key (channel)
  "Return the stable native identity of CHANNEL."
  (list (alist-get 'guild_id channel) (alist-get 'channel_id channel)))

(defun qq-guild--message-p (value)
  "Validate one text-channel message VALUE."
  (and (qq-guild--channel-p (alist-get 'channel value))
       (qq-protocol-uint64-decimal-p (alist-get 'sequence value))
       (qq-protocol-uint64-decimal-p (alist-get 'count_sequence value) t)
       (qq-protocol-uint64-decimal-p (alist-get 'sender_tiny_id value))
       (natnump (alist-get 'timestamp value))
       (stringp (alist-get 'sender_name value))
       (stringp (alist-get 'text value))
       (natnump (alist-get 'unsupported_elements value))))

(defun qq-guild--state-p (value)
  "Validate authoritative channel cursor VALUE."
  (and (qq-guild--channel-p (alist-get 'channel value))
       (stringp (alist-get 'name value))
       (natnump (alist-get 'channel_type value))
       (cl-every (lambda (key)
                   (qq-protocol-uint64-decimal-p (alist-get key value) t))
                 '(latest_sequence latest_count_sequence read_sequence read_count_sequence))))

(defun qq-guild--get (state table key)
  "Read KEY in immutable STATE's TABLE."
  (when-let* ((values (plist-get state table))) (gethash key values)))

(defun qq-guild--put (state table key value)
  "Return STATE with KEY set to VALUE in a copied TABLE."
  (let ((values (if (plist-get state table)
                    (copy-hash-table (plist-get state table))
                  (make-hash-table :test #'equal))))
    (puthash key value values)
    (plist-put (copy-sequence state) table values)))

(defun qq-guild--from-view (view)
  "Read Guild state from this pass's account read VIEW."
  (qq-state-partition-guild (appkit-app-read-view-model view)))

(defun qq-guild--operation-key (kind target)
  "Return the canonical operation key for KIND and TARGET."
  (list kind (if (listp target) (qq-guild--key target) target)))

(defun qq-guild--merge-state (state incoming)
  "Merge validated INCOMING channel cursors without regressing confirmed reads."
  (let* ((key (qq-guild--key (alist-get 'channel incoming)))
         (old (qq-guild--get state :states key))
         (next (copy-tree incoming)))
    (dolist (pair '((latest_sequence latest_count_sequence)
                    (read_sequence read_count_sequence)))
      (when (and old (> (string-to-number (alist-get (car pair) old))
                       (string-to-number (alist-get (car pair) next))))
        (dolist (field pair) (setf (alist-get field next) (alist-get field old)))))
    (qq-guild--put state :states key next)))

(defun qq-guild--merge-messages (state messages)
  "Merge validated MESSAGES by native channel and sequence into STATE."
  (let ((groups (seq-group-by (lambda (msg) (qq-guild--key (alist-get 'channel msg)))
                              messages)))
    (dolist (group groups state)
      (let ((index (make-hash-table :test #'equal)))
        (dolist (msg (qq-guild--get state :messages (car group)))
          (puthash (alist-get 'sequence msg) msg index))
        (dolist (msg (cdr group)) (puthash (alist-get 'sequence msg) (copy-tree msg) index))
        (setq state
              (qq-guild--put
               state :messages (car group)
               (sort (hash-table-values index)
                     (lambda (a b) (< (string-to-number (alist-get 'sequence a))
                                      (string-to-number (alist-get 'sequence b)))))))))))

(defun qq-guild--validate-result (kind target input result owner)
  "Validate RESULT for KIND, TARGET, INPUT and exact account OWNER."
  (unless (equal owner (alist-get 'account_id result))
    (error "频道响应属于其他账号"))
  (pcase kind
    ('list
     (let ((items (alist-get 'guilds result)))
       (unless (and (listp items)
                    (cl-every (lambda (item)
                                (and (qq-protocol-uint64-decimal-p (alist-get 'guild_id item))
                                     (stringp (alist-get 'name item)))) items))
         (error "无效的频道目录"))
       items))
    ('channels
     (let ((value (alist-get 'list result)))
       (unless (and (equal target (alist-get 'guild_id value))
                    (listp (alist-get 'channels value))
                    (cl-every
                     (lambda (item)
                       (and (qq-guild--channel-p (alist-get 'channel item))
                            (equal target (alist-get 'guild_id (alist-get 'channel item)))
                            (stringp (alist-get 'name item))
                            (natnump (alist-get 'channel_type item))
                            (or (null (alist-get 'state item))
                                (and (qq-guild--state-p (alist-get 'state item))
                                     (qq-guild--same-channel-p
                                      (alist-get 'channel item)
                                      (alist-get 'channel (alist-get 'state item)))))))
                     (alist-get 'channels value)))
         (error "无效的子频道目录"))
       value))
    ('history
     (let* ((value (alist-get 'history result))
            (begin (alist-get 'begin_sequence value))
            (end (alist-get 'end_sequence value)))
       (unless (and (qq-guild--same-channel-p target (alist-get 'channel value))
                    (qq-protocol-uint64-decimal-p begin t)
                    (qq-protocol-uint64-decimal-p end t)
                    (<= (string-to-number begin) (string-to-number end))
                    (or (null (plist-get input :end))
                        (<= (string-to-number end) (string-to-number (plist-get input :end))))
                    (natnump (alist-get 'unsupported_messages value))
                    (listp (alist-get 'messages value))
                    (cl-every
                     (lambda (msg)
                       (and (qq-guild--message-p msg)
                            (qq-guild--same-channel-p target (alist-get 'channel msg))
                            (<= (string-to-number begin) (string-to-number (alist-get 'sequence msg))
                                (string-to-number end))))
                     (alist-get 'messages value)))
         (error "无效或错路由的频道历史"))
       value))
    ((or 'send 'read)
     (let ((value (alist-get 'receipt result)))
       (unless (and (qq-guild--same-channel-p target (alist-get 'channel value))
                    (qq-protocol-uint64-decimal-p (alist-get 'sequence value))
                    (or (eq kind 'send)
                        (>= (string-to-number (alist-get 'sequence value))
                            (string-to-number (plist-get input :sequence)))))
         (error "无效的频道操作回执"))
       value))
    (_ (error "未知频道操作"))))

(defun qq-guild--commit (state kind target value)
  "Return STATE after accepting validated VALUE for KIND and TARGET."
  (pcase kind
    ('list (plist-put (plist-put (copy-sequence state) :guilds (copy-tree value)) :loaded t))
    ('channels
     (setq state (qq-guild--put state :directories target (copy-tree value)))
     (dolist (item (alist-get 'channels value) state)
       (when (alist-get 'state item)
         (setq state (qq-guild--merge-state state (alist-get 'state item))))))
    ('history (qq-guild--merge-messages state (alist-get 'messages value)))
    ('read
     (let* ((key (qq-guild--key target))
            (old (qq-guild--get state :reads key))
            (sequence (alist-get 'sequence value)))
       (qq-guild--put state :reads key
                      (if (> (string-to-number (or old "0")) (string-to-number sequence))
                          old sequence))))
    (_ state)))

(defun qq-guild--rpc-effect (owner kind target input token route epoch)
  "Build an account-owned Effect with exact request and reply authority."
  (let* ((method (cdr (assq kind '((list . "guild.list") (channels . "guild.channels")
                                  (history . "guild.history") (send . "guild.send_text")
                                  (read . "guild.mark_read")))))
         (params
          (pcase kind
            ('list nil)
            ('channels (list (cons 'guild_id target)))
            ('history (append (list (cons 'channel target) '(limit . 50))
                              (when (plist-get input :end)
                                (list (cons 'end_sequence (plist-get input :end))))))
            ('send (list (cons 'channel target) (cons 'text (plist-get input :text))))
            ('read (list (cons 'channel target) (cons 'sequence (plist-get input :sequence))))))
         (request (list :kind kind :target target :input input :token token :route route :epoch epoch)))
    (appkit-effect-create
     :key (cons 'qq-guild (qq-guild--operation-key kind target))
     :input request
     :start
     (lambda (_context _input _observe resolve reject)
       (condition-case err
           (let ((cancel (qq-runtime-call-with-account
                          owner (lambda () (qq-runtime--rpc-fetch method params resolve reject)))))
             (appkit-cancellation-create :kind 'logical :cancel cancel))
         (error (funcall reject nil (error-message-string err)) nil)))
     :success (lambda (request result) (list 'qq-guild-result request result nil))
     :failure (lambda (request _body reason) (list 'qq-guild-result request nil reason)))))

(defun qq-guild--publish (target)
  "Return change notifications for TARGET's canonical state."
  (append (list (appkit-command-publish 'qq-guild-directory 'changed))
          (when (listp target)
            (list (appkit-command-publish (cons 'qq-guild-channel (qq-guild--key target)) 'changed)))))

(defun qq-guild-app-update (context model message)
  "Handle Guild MESSAGE in the existing account App, or return nil."
  (when (memq (car-safe message) '(qq-guild-request qq-guild-result qq-guild-observed qq-guild-reset))
    (let* ((state (qq-state-partition-guild model))
           (owner (qq-state-partition-account-id model))
           commands)
      (pcase message
        (`(qq-guild-request ,kind ,target ,input ,token)
         (let* ((key (qq-guild--operation-key kind target))
                (old (qq-guild--get state :operations key))
                (route (appkit-transition-context-reply-route context)))
           (if (and (memq kind '(send read)) (plist-get old :pending))
               (setq commands
                     (list (appkit-command-post-message
                            :target route :delivery 'report
                            :message (list 'qq-guild-reply kind token nil "同一频道的操作正在进行"))))
             (setq state (qq-guild--put state :operations key
                                        (list :pending t :token token :input input))
                   commands
                   (append
                    (list (appkit-command-start-effect
                           (qq-guild--rpc-effect owner kind target input token route
                                                 (plist-get state :epoch))))
                    (qq-guild--publish target))))))
        (`(qq-guild-result ,request ,result ,reason)
         (let* ((kind (plist-get request :kind)) (target (plist-get request :target))
                (key (qq-guild--operation-key kind target))
                (token (plist-get request :token))
                (old (qq-guild--get state :operations key))
                value)
           (when (and (equal token (plist-get old :token))
                      (equal (plist-get state :epoch) (plist-get request :epoch)))
             (unless reason
               (condition-case err
                   (setq value (qq-guild--validate-result
                                kind target (plist-get request :input) result owner)
                         state (qq-guild--commit state kind target value))
                 (error (setq reason (error-message-string err)))))
             (setq state (qq-guild--put state :operations key
                                        (list :token token :error reason :input (plist-get request :input)
                                              :value (unless (eq kind 'history) value))))
             (setq commands
                   (append
                    (when (plist-get request :route)
                      (list (appkit-command-post-message
                             :target (plist-get request :route) :delivery 'report
                             :message
                             (list 'qq-guild-reply kind token
                                   (if (eq kind 'history)
                                       (assq-delete-all 'messages (copy-tree value)) value)
                                   reason))))
                    (qq-guild--publish target))))))
        (`(qq-guild-observed ,data)
         (when (and (equal owner (alist-get 'account_id data))
                    (listp (alist-get 'messages data)) (listp (alist-get 'channels data))
                    (cl-every #'qq-guild--message-p (alist-get 'messages data))
                    (cl-every #'qq-guild--state-p (alist-get 'channels data)))
           (setq state (qq-guild--merge-messages state (alist-get 'messages data)))
           (dolist (item (alist-get 'channels data))
             (setq state (qq-guild--merge-state state item)))
           (setq commands
                 (cons (appkit-command-publish 'qq-guild-directory 'changed)
                       (mapcar
                        (lambda (key)
                          (appkit-command-publish (cons 'qq-guild-channel key) 'changed))
                        (delete-dups
                         (mapcar (lambda (item) (qq-guild--key (alist-get 'channel item)))
                                 (append (alist-get 'messages data) (alist-get 'channels data)))))))))
        (`(qq-guild-reset ,phase ,instance)
         (unless (and (equal phase (plist-get state :phase))
                      (equal instance (plist-get state :instance)))
           (when-let* ((operations (plist-get state :operations)))
             (maphash
              (lambda (key op)
                (when (plist-get op :pending)
                  (push (appkit-command-cancel-effect (cons 'qq-guild key)) commands)))
              operations))
           (setq state (list :phase phase :instance instance
                             :epoch (1+ (or (plist-get state :epoch) 0)))
                 commands (append commands
                                  (list (appkit-command-publish 'qq-guild-session phase)
                                        (appkit-command-publish 'qq-guild-directory 'changed)))))))
      (let ((next (copy-qq-state-partition model)))
        (setf (qq-state-partition-guild next) state)
        (appkit-next :model next :render appkit-render-none :commands commands)))))

(defun qq-guild--event (_event data)
  "Deliver a Gateway observation once to its account App."
  (when-let* ((runtime (qq-runtime-account (alist-get 'account_id data))))
    (appkit-app-send (qq-runtime-account-app runtime) (list 'qq-guild-observed data))))

(defun qq-guild--account-changed (_reason account-id)
  "Invalidate Guild work when its owning session changes."
  (dolist (runtime (qq-runtime-accounts))
    (when (or (null account-id) (equal account-id (qq-runtime-account-id runtime)))
      (appkit-app-send
       (qq-runtime-account-app runtime)
       (list 'qq-guild-reset (alist-get 'phase (qq-account-get (qq-runtime-account-id runtime)))
             (qq-server-gateway-instance-id))))))

(defun qq-guild--request-command (context kind target input token)
  "Ask the parent account App to perform KIND using plain request data."
  (appkit-command-post-message
   :target (appkit-transition-context-parent-address context)
   :message (list 'qq-guild-request kind target input token)
   :reply-correlation token :delivery 'report))

(defun qq-guild--open-surface (type owner identity input title)
  "Open or reuse TYPE for OWNER's IDENTITY with INPUT and TITLE."
  (unless (qq-rpc-method-available-p "guild.list")
    (user-error "qq: 当前网关未提供 QQ 频道功能"))
  (let* ((app (qq-runtime-app owner))
         (existing (appkit-app-surface app identity)))
    (qq-guild--account-changed 'open owner)
    (if (appkit-surface-live-p existing)
        (progn (pop-to-buffer (appkit-surface-buffer existing)) existing)
      (appkit-open-generated-surface
       type :app app :identity identity :input (append (list :account owner) input)
       :buffer-name (qq-runtime-account-buffer-name "guild" title owner) :select t))))

;;;###autoload
(defun qq-guild-open ()
  "Open the account's hierarchical Guild directory."
  (interactive)
  (require 'qq-guilds)
  (qq-guilds-open))

(defun qq-guild-open-channel (channel title &optional owner)
  "Open exact text CHANNEL in its Appkit timeline."
  (require 'qq-guild-chat)
  (qq-guild-chat-open channel title owner))

(qq-rpc-register-event "guild.observed" #'qq-guild--event)
(add-hook 'qq-account-registry-changed-hook #'qq-guild--account-changed)
(provide 'qq-guild)
;;; qq-guild.el ends here
