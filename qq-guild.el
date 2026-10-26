;;; qq-guild.el --- Account-owned QQ Guild state and operations -*- lexical-binding: t; -*-

;;; Commentary:
;; The account App owns Guild protocol objects and finite RPC Effects.
;; Directory and chat Surfaces subscribe to committed state; messages retain
;; their native guild/channel/tiny-ID/sequence identities throughout.

;;; Code:
(require 'cl-lib)
(require 'url-parse)
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
  (list kind
        (pcase kind
          ('feed (list (alist-get 'guild_id target) (alist-get 'view target)))
          ('replies (list (qq-guild--operation-key 'comments (alist-get 'post target))
                          (alist-get 'comment_id target)))
          ('comments (append (qq-guild--key (alist-get 'channel target))
                             (list (alist-get 'post_id target) (alist-get 'author_tiny_id target))))
          (_ (if (qq-guild--channel-p target) (qq-guild--key target) target)))))

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
    ((or 'feed 'forum 'comments 'replies) (qq-guild--forum-page kind target input result))
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
    ((or 'feed 'forum 'comments 'replies) (qq-guild--merge-forum-page state kind target value))
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
                                   (read . "guild.mark_read") (forum . "guild.forum") (feed . "guild.feed")
                                   (comments . "guild.comments") (replies . "guild.replies")))))
         (params
          (pcase kind
            ((or 'feed 'forum 'comments 'replies)
             (list (cons 'request
                         (append (pcase kind
                                   ('feed target)
                                   ('forum (list (cons 'channel target)))
                                   ('comments (append (list (cons 'post target))
                                                      (when (plist-get input :include-post)
                                                        (list '(include_post . t)))))
                                   ('replies target))
                                 (list (cons 'cursor (plist-get input :cursor)) '(limit . 20))))))
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
             (appkit-cancellation-create
              :kind 'logical
              :cancel cancel))
         (error (funcall reject nil (error-message-string err)) nil)))
     :success (lambda (request result) (list 'qq-guild-result request result nil))
     :failure (lambda (request _body reason) (list 'qq-guild-result request nil reason)))))

(defun qq-guild--publish (target)
  "Return change notifications for TARGET's canonical state."
  (let* ((channel (cond ((qq-guild--channel-p target) target)
                        ((qq-guild--post-p target) (alist-get 'channel target))
                        ((and (listp target) (qq-guild--post-p (alist-get 'post target)))
                         (alist-get 'channel (alist-get 'post target)))))
         (guild (or (alist-get 'guild_id channel)
                    (and (listp target) (alist-get 'guild_id target))
                    (and (stringp target) target))))
    (append (list (appkit-command-publish 'qq-guild-directory 'changed))
            (when guild (list (appkit-command-publish (cons 'qq-guild-home guild) 'changed)))
            (when channel
              (list (appkit-command-publish
                     (cons 'qq-guild-channel (qq-guild--key channel)) 'changed))))))

(defun qq-guild--reset (state phase instance)
  "Return fresh session state, retaining uncertain sends and cancellation commands."
  (let (commands uncertain)
    (when-let* ((operations (plist-get state :operations)))
      (maphash
       (lambda (key op)
         (when (or (plist-get op :pending) (plist-get op :unknown))
           (push (appkit-command-cancel-effect (cons 'qq-guild key)) commands)
           (when (eq (car key) 'send)
             (setq uncertain
                   (qq-guild--put uncertain :operations key
                                  (list :input (plist-get op :input) :unknown t
                                        :error "会话已变更，发送结果未知；请确认后再发送"))))))
       operations))
    (cons (append (list :phase phase :instance instance
                        :epoch (1+ (or (plist-get state :epoch) 0)))
                  (if (plist-member state :epoch) uncertain state))
          (append commands (list (appkit-command-publish 'qq-guild-session phase)
                                 (appkit-command-publish 'qq-guild-directory 'changed))))))

(defun qq-guild-app-update (context model message)
  "Handle Guild MESSAGE in the existing account App, or return nil."
  (when (memq (car-safe message) '(qq-guild-request qq-guild-result qq-guild-observed qq-guild-reset qq-guild-publication))
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
                            :target route
                            :delivery 'report
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
                             :target (plist-get request :route)
                             :delivery 'report
                             :message
                             (list 'qq-guild-reply kind token
                                   (if (eq kind 'history)
                                       (assq-delete-all 'messages (copy-tree value)) value)
                                   reason))))
                    (qq-guild--publish target))))))
        (`(qq-guild-publication ,target)
         (let* ((channel (or (alist-get 'channel target) (alist-get 'channel (alist-get 'post target))))
                (guild (alist-get 'guild_id channel)))
           (setq state (qq-guild--put state :publications guild
                                     (1+ (or (qq-guild--get state :publications guild) 0))))
           (when (equal (alist-get 'kind target) "reply")
             (let* ((reply-target (list (cons 'post (alist-get 'post target))
                                        (cons 'comment_id (alist-get 'comment_id target))))
                    (key (qq-guild--operation-key 'replies reply-target)))
               (setq state (qq-guild--put state :pages key nil)
                     state (qq-guild--put state :operations key nil))
               (push (appkit-command-cancel-effect (cons 'qq-guild key)) commands)))
           (setq commands (append commands (qq-guild--publish channel)))))
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
        (`(qq-guild-reset ,phase ,instance . ,reset-options)
         (unless (and (not (car reset-options)) (equal phase (plist-get state :phase))
                      (equal instance (plist-get state :instance)))
           (let ((reset (qq-guild--reset state phase instance)))
             (setq state (car reset) commands (cdr reset))))))
      (let ((next (copy-qq-state-partition model)))
        (setf (qq-state-partition-guild next) state)
        (appkit-next
         :model next
         :render appkit-render-none
         :commands commands)))))

(defun qq-guild--event (_event data)
  "Deliver a Gateway observation once to its account App."
  (when-let* ((runtime (qq-runtime-account (alist-get 'account_id data))))
    (appkit-app-send (qq-runtime-account-app runtime) (list 'qq-guild-observed data))))

(defun qq-guild--account-changed (reason account-id)
  "Invalidate Guild work when its owning session changes."
  (dolist (runtime (qq-runtime-accounts))
    (when (or (null account-id) (equal account-id (qq-runtime-account-id runtime)))
      (appkit-app-send
       (qq-runtime-account-app runtime)
       (list 'qq-guild-reset (alist-get 'phase (qq-account-get (qq-runtime-account-id runtime)))
             (qq-server-gateway-instance-id) (memq reason '(ready resync)))))))

(defun qq-guild--request-command (context kind target input token)
  "Ask the parent account App to perform KIND using plain request data."
  (appkit-command-post-message
   :target (appkit-transition-context-parent-address context)
   :message (list 'qq-guild-request kind target input token)
   :reply-correlation token
   :delivery 'report))

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
       type
       :app app
       :identity identity
       :input (append (list :account owner) input)
       :buffer-name (qq-runtime-account-buffer-name "guild" title owner)
       :select t))))

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



(defun qq-guild--post-p (post)
  "Validate the native forum POST identity."
  (and (listp post) (qq-guild--channel-p (alist-get 'channel post))
       (qq-protocol-non-empty-string-p (alist-get 'post_id post))
       (qq-protocol-uint64-decimal-p (alist-get 'author_tiny_id post))))

(defun qq-guild--same-post-p (left right)
  "Compare native post identities regardless of JSON object order."
  (and (qq-guild--post-p left) (qq-guild--post-p right)
       (qq-guild--same-channel-p (alist-get 'channel left) (alist-get 'channel right))
       (equal (alist-get 'post_id left) (alist-get 'post_id right))
       (equal (alist-get 'author_tiny_id left) (alist-get 'author_tiny_id right))))

(defun qq-guild--forum-author-p (value)
  "Validate a native forum author VALUE."
  (and (qq-protocol-uint64-decimal-p (alist-get 'tiny_id value))
       (stringp (alist-get 'name value))))

(defun qq-guild--web-url-p (value &optional https-only)
  "Validate a credential-free HTTP link or HTTPS-only media VALUE."
  (and (stringp value)
       (condition-case nil
           (let ((url (url-generic-parse-url value)))
             (and (member (url-type url) (if https-only '("https") '("http" "https")))
                  (qq-protocol-non-empty-string-p (url-host url))
                  (null (url-user url)) (null (url-password url))
                  (not (string-match-p "[[:space:]]" value))))
         (error nil))))

(defun qq-guild--forum-body-p (value)
  "Validate optional ordered content and media of canonical VALUE."
  (and
   (listp (alist-get 'content value))
   (cl-every
    (lambda (entry)
      (pcase (alist-get 'kind entry)
        ("text" (stringp (alist-get 'text entry)))
        ("mention" (and (stringp (alist-get 'text entry))
                        (qq-protocol-uint64-decimal-p (alist-get 'tiny_id entry))))
        ("link" (and (stringp (alist-get 'text entry))
                     (qq-guild--web-url-p (alist-get 'url entry))))
        ("emoji" (and (stringp (alist-get 'id entry)) (stringp (alist-get 'name entry))
                      (qq-guild--web-url-p (alist-get 'url entry) t)))))
    (alist-get 'content value))
   (listp (alist-get 'media value))
   (cl-every
    (lambda (entry)
      (and (member (alist-get 'kind entry) '("image" "video"))
           (stringp (alist-get 'id entry))
           (qq-guild--web-url-p (alist-get 'url entry) t)
           (or (null (alist-get 'preview_url entry))
               (qq-guild--web-url-p (alist-get 'preview_url entry) t))
           (natnump (alist-get 'width entry)) (natnump (alist-get 'height entry))
           (natnump (alist-get 'duration entry))))
    (alist-get 'media value))))

(defun qq-guild--forum-comment-p (value &optional reply)
  "Validate a comment VALUE and its bounded embedded replies."
  (and (qq-protocol-non-empty-string-p (alist-get 'comment_id value))
       (qq-guild--forum-author-p (alist-get 'author value))
       (stringp (alist-get 'text value))
       (qq-guild--forum-body-p value)
       (natnump (alist-get 'timestamp value))
       (natnump (alist-get 'reply_count value))
       (natnump (alist-get 'unsupported_elements value))
       (listp (alist-get 'replies value))
       (if reply (null (alist-get 'replies value))
         (cl-every (lambda (item) (qq-guild--forum-comment-p item t)) (alist-get 'replies value)))))

(defun qq-guild--forum-page (kind target input result)
  "Validate one KIND page against TARGET and requested opaque cursor INPUT."
  (let* ((page (alist-get 'page result))
         (cursor (alist-get 'cursor page))
         (next (alist-get 'next_cursor page))
         (posts-p (memq kind '(feed forum)))
         (field (pcase kind ((or 'feed 'forum) 'posts) ('comments 'comments) ('replies 'replies)))
         (items (alist-get field page))
         (ids (mapcar (lambda (item) (if posts-p
                                        (qq-guild--operation-key 'comments (alist-get 'identity item))
                                      (alist-get 'comment_id item))) items)))
    (unless
        (and (stringp cursor) (equal cursor (plist-get input :cursor))
             (or (null next) (and (qq-protocol-non-empty-string-p next) (not (equal next cursor))))
             (listp items) (= (length ids) (length (delete-dups (copy-sequence ids))))
             (pcase kind
               ((or 'feed 'forum)
                (and (if (eq kind 'feed)
                         (and (qq-protocol-uint64-decimal-p (alist-get 'guild_id target))
                              (member (alist-get 'view target) '("all" "hot" "essence"))
                              (equal (alist-get 'guild_id target) (alist-get 'guild_id page))
                              (equal (alist-get 'view target) (alist-get 'view page)))
                       (qq-guild--same-channel-p target (alist-get 'channel page)))
                     (cl-every
                      (lambda (post)
                        (let* ((id (alist-get 'identity post))
                               (channel (alist-get 'channel id)))
                          (and (qq-guild--post-p id)
                               (if (eq kind 'feed)
                                   (equal (alist-get 'guild_id target) (alist-get 'guild_id channel))
                                 (qq-guild--same-channel-p target channel))
                               (qq-guild--forum-author-p (alist-get 'author post))
                               (equal (alist-get 'author_tiny_id id)
                                      (alist-get 'tiny_id (alist-get 'author post)))
                               (stringp (alist-get 'title post)) (stringp (alist-get 'text post))
                               (qq-guild--forum-body-p post)
                               (natnump (alist-get 'timestamp post))
                               (natnump (alist-get 'comment_count post))
                               (natnump (alist-get 'unsupported_elements post))))) items)))
               ('comments
                (and (qq-guild--same-post-p target (alist-get 'post page))
                     (natnump (alist-get 'total_count page))
                     (or (null (alist-get 'entry page))
                         (let* ((entry (alist-get 'entry page))
                                (identity (alist-get 'identity entry)))
                           (and (qq-guild--same-post-p identity target)
                                (qq-guild--forum-author-p (alist-get 'author entry))
                                (equal (alist-get 'author_tiny_id target)
                                       (alist-get 'tiny_id (alist-get 'author entry)))
                                (stringp (alist-get 'title entry))
                                (stringp (alist-get 'text entry))
                                (qq-guild--forum-body-p entry)
                                (natnump (alist-get 'timestamp entry))
                                (natnump (alist-get 'comment_count entry))
                                (natnump (alist-get 'unsupported_elements entry)))))
                     (cl-every #'qq-guild--forum-comment-p items)))
               ('replies
                (and (qq-guild--same-post-p (alist-get 'post target) (alist-get 'post page))
                     (equal (alist-get 'comment_id target) (alist-get 'comment_id page))
                     (natnump (alist-get 'total_count page))
                     (cl-every (lambda (item) (qq-guild--forum-comment-p item t)) items)))))
      (error "无效或错路由的论坛分页"))
    page))

(defun qq-guild--merge-forum-page (state kind target page)
  "Commit a validated PAGE while preserving stable post/comment order."
  (let* ((key (qq-guild--operation-key kind target))
         (old (qq-guild--get state :pages key))
         (field (pcase kind ((or 'feed 'forum) 'posts) ('comments 'comments) ('replies 'replies)))
         (cursor (alist-get 'cursor page))
         (id (lambda (item) (if (memq kind '(feed forum))
                               (qq-guild--operation-key 'comments (alist-get 'identity item))
                             (alist-get 'comment_id item))))
         (items (unless (string-empty-p cursor) (copy-tree (alist-get field old)))))
    (when (and (not (string-empty-p cursor))
               (not (equal cursor (alist-get 'next_cursor old))))
      (error "论坛分页游标不属于当前结果"))
    (dolist (item (alist-get field page))
      (if-let* ((position (cl-position (funcall id item) items :key id :test #'equal)))
          (setf (nth position items) (copy-tree item))
        (setq items (append items (list (copy-tree item))))))
    (let ((copy (copy-tree page)))
      (setf (alist-get field copy) items)
      (when (and (eq kind 'comments) (null (alist-get 'entry copy)))
        (setf (alist-get 'entry copy) (copy-tree (alist-get 'entry old))))
      (qq-guild--put state :pages key copy))))

(defun qq-guild--find-post (state identity)
  "Find IDENTITY in the account's loaded home and forum pages."
  (catch 'found
    (when-let* ((entry (alist-get 'entry (qq-guild--get state :pages
                                        (qq-guild--operation-key 'comments identity)))))
      (throw 'found entry))
    (when-let* ((pages (plist-get state :pages)))
      (maphash
       (lambda (key page)
         (when (memq (car key) '(feed forum))
           (when-let* ((post (seq-find
                             (lambda (post) (qq-guild--same-post-p identity (alist-get 'identity post)))
                             (alist-get 'posts page))))
             (throw 'found post))))
       pages))))

(provide 'qq-guild)
;;; qq-guild.el ends here
