;;; qq-message-test.el --- Tests for service messages -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-attachment)
(require 'qq-message)

(defconst qq-message-test-capabilities
  '("message.send" "dataline.send_text" "dataline.send_file"
    "file.send" "message.poke"
    "message.send_merged_forward"
    "message.recall_poke" "message.recall" "message.set_reaction"
    "message.set_essence" "message.set_todo" "message.get_history"
    "message.get_history_page" "message.get_history_around"
    "message.get_forward"
    "message.mark_read" "message.delete_local")
  "Native Gateway capabilities exercised by message tests.")

(defun qq-message-test-account (&optional account-id uin uid)
  "Return an online account for ACCOUNT-ID, UIN, and UID."
  `((account_id . ,(or account-id "slot-a"))
    (label . "Primary")
    (phase . "online")
    (uin . ,(or uin "10002"))
    (uid . ,(or uid "u_self"))
    (challenge)
    (problem)))

(defun qq-message-test-text-segment (text)
  "Return a native text segment containing TEXT."
  `((kind . "text") (payload . ((text . ,text)))))

(defun qq-message-test-reply-segment (message-id)
  "Return a UI reply segment targeting exact MESSAGE-ID."
  `((type . "reply")
    (data . ((target . ((kind . "message")
                        (message_id . ,message-id)))))))

(defun qq-message-test-wire-segment (segment)
  "Return an owned domain copy of readable SEGMENT."
  (copy-tree segment))

(defun qq-message-test-ready-image (&optional attachment-id)
  "Return one ready group-image attachment fixture."
  `((attachment_id
     . ,(or attachment-id "att-aaaaaaaa-bbbb-4ccc-8ddd-eeeeeeeeeeee"))
    (resource_id . "res-image-wire")
    (account_id . "slot-a")
    (conversation . ((kind . "group") (group_uin . "8209413637")))
    (use . ((kind . "image") (summary . "[图片]") (sub_type . 0)))
    (phase . "ready")
    (bytes_done . "0")
    (bytes_total . "3")
    (fast_path . t)
    (created_at . 1784700000)
    (updated_at . 1784700001)
    (error)))

(defun qq-message-test-ready-record (&optional attachment-id)
  "Return one ready group-record attachment fixture."
  `((attachment_id
     . ,(or attachment-id "att-aaaaaaaa-bbbb-4ccc-8ddd-eeeeeeeeeeef"))
    (resource_id . "res-record-wire")
    (account_id . "slot-a")
    (conversation . ((kind . "group") (group_uin . "8209413637")))
    (use . ((kind . "record")))
    (phase . "ready")
    (bytes_done . "0")
    (bytes_total . "3")
    (fast_path . t)
    (created_at . 1784700000)
    (updated_at . 1784700001)
    (error)))

(defun qq-message-test-ready-video (&optional attachment-id)
  "Return one ready group-video attachment fixture."
  `((attachment_id
     . ,(or attachment-id "att-aaaaaaaa-bbbb-4ccc-8ddd-eeeeeeeeeee0"))
    (resource_id . "res-video-wire")
    (account_id . "slot-a")
    (conversation . ((kind . "group") (group_uin . "8209413637")))
    (use . ((kind . "video")
            (thumbnail_resource_id . "res-video-thumbnail-wire")))
    (phase . "ready")
    (bytes_done . "0")
    (bytes_total . "6")
    (fast_path . t)
    (created_at . 1784700000)
    (updated_at . 1784700001)
    (error)))

(cl-defun qq-message-test-event
    (&key
     (account-id "slot-a")
     row-key
     (message-id "7348923749823749823")
     (sent-at 1784700000)
     (sender '((uin . "10001") (uid . "u_peer")))
     (recipient '((uin . "10002") (uid . "u_self")))
     (conversation '((kind . "private")))
     (sender-presentation '((nickname . "Peer")))
     (sequence "9007199254740999")
     (client-sequence "9007199254741001")
     (random 7) (message-type 166) (sub-type 0)
     (segments '(((kind . "text") (payload . ((text . "hello")))))))
  "Return one closed native Gateway message event payload."
  `((account_id . ,account-id)
    ,@(when row-key `((row_key . ,row-key)))
    (message
     . ((message_id . ,message-id)
        (sent_at . ,sent-at)
        (sender . ,(copy-tree sender))
        (recipient . ,(copy-tree recipient))
        (conversation . ,(copy-tree conversation))
        (sender_presentation . ,(copy-tree sender-presentation))
        (sequence . ,sequence)
        (client_sequence . ,client-sequence)
        (random . ,random)
        (message_type . ,message-type)
        (sub_type . ,sub-type)
        (segments
         . ,(mapcar #'qq-message-test-wire-segment
                    (append segments nil)))))))

(defun qq-message-test-native-row (event row-key)
  "Return one canonical native history row for EVENT and ROW-KEY."
  `((kind . "native")
    (row_key . ,row-key)
    (timeline_class . "authored")
    (recalled . :false)
    (message . ,(copy-tree (alist-get 'message event)))))

(cl-defun qq-message-test-recall
    (&key
     (account-id "slot-a")
     (conversation '((kind . "group") (group_uin . "8209413637")))
     (target '((kind . "sequence") (sequence . "9007199254740999")))
     (author-uid "u_peer") (operator-uid "u_admin")
     (tip "message recalled"))
  "Return one closed native Gateway recall event payload."
  `((account_id . ,account-id)
    (recall
     . ((conversation . ,(copy-tree conversation))
        (target . ,(copy-tree target))
        (author_uid . ,author-uid)
        (operator_uid . ,operator-uid)
        (tip . ,tip)))))

(cl-defun qq-message-test-poke
    (&key
     (account-id "slot-a")
     (row-key "42")
     (message-id "7348923749823749823") (sent-at 1784700000)
     (sequence "9007199254740999") (group-uin "8209413637")
     (actor-uin "10002") (target-uin "9007199254741001"))
  "Return one authoritative group Poke GrayTip message payload."
  (qq-message-test-event
   :account-id account-id
   :row-key row-key
   :message-id message-id
   :sent-at sent-at
   :sender `((uin . ,actor-uin))
   :recipient nil
   :conversation `((kind . "group") (group_uin . ,group-uin))
   :sender-presentation nil
   :sequence sequence
   :client-sequence "0"
   :random nil
   :message-type 732
   :sub-type 20
   :segments
   `(((kind . "gray_tip")
      (payload . ((kind . "poke")
                  (actor_uin . ,actor-uin)
                  (target_uin . ,target-uin)
                  (actor_name . "Me")
                  (target_name . "Peer")
                  (action . "戳了戳")
                  (action_image_url . "https://example.invalid/poke.png")
                  (suffix . "的肩膀")))))))

(cl-defun qq-message-test-private-poke
    (&key
     (account-id "slot-a")
     (message-id "144115189030781277") (sent-at 1784700000)
     (sequence "30737") (random 954925405)
     (actor-uin "10002") (target-uin "10001"))
  "Return one authoritative private Poke sharing the private cursor space."
  (qq-message-test-event
   :account-id account-id
   :message-id message-id
   :sent-at sent-at
   ;; Observed 528/290 records route from the semantic target to the actor;
   ;; actor/target remain GrayTip semantics rather than route aliases.
   :sender `((uin . ,target-uin) (uid . "u_peer"))
   :recipient `((uin . ,actor-uin) (uid . "u_self"))
   :conversation '((kind . "private"))
   :sender-presentation nil
   :sequence sequence
   :client-sequence "3327"
   :random random
   :message-type 528
   :sub-type 290
   :segments
   `(((kind . "gray_tip")
      (payload . ((kind . "poke")
                  (actor_uin . ,actor-uin)
                  (target_uin . ,target-uin)
                  (actor_name . "Me")
                  (target_name . "Peer")
                  (action . "戳了戳")))))))

(cl-defun qq-message-test-reaction
    (&key
     (account-id "slot-a")
     (group-uin "8209413637") (sequence "9007199254740999")
     (operator-uid "u_member") (operator-uin "10001")
     (emoji-id "178") (emoji-type "1") (is-add t) (count 3))
  "Return one authoritative native Gateway group reaction event payload."
  (let ((emoji
         (pcase emoji-type
           ("1" `((kind . "qq_face") (id . ,(string-to-number emoji-id))))
           ("2" `((kind . "unicode")
                  (value . ,(char-to-string (string-to-number emoji-id)))))
           (_ (error "invalid test reaction emoji type")))))
    `((account_id . ,account-id)
      (reaction
       . ((conversation . ((kind . "group") (group_uin . ,group-uin)))
          (sequence . ,sequence)
          (operator_uid . ,operator-uid)
          ,@(when operator-uin `((operator_uin . ,operator-uin)))
          (emoji . ,emoji)
          (is_add . ,is-add)
          (count . ,count))))))

(cl-defun qq-message-test-essence
    (&key
     (account-id "slot-a")
     (group-uin "8209413637") (sequence "9007199254740999")
     (random 7) (is-set t) (sender-uin "10001")
     (operator-uin "10002") (changed-at 1784700000)
     (operator-nickname "Moderator") (sender-nickname "Alice"))
  "Return one authoritative native Gateway group essence event payload."
  `((account_id . ,account-id)
    (essence
     . ((conversation . ((kind . "group") (group_uin . ,group-uin)))
        (sequence . ,sequence)
        (random . ,random)
        (is_set . ,is-set)
        (sender_uin . ,sender-uin)
        (operator_uin . ,operator-uin)
        (changed_at . ,changed-at)
        ,@(when operator-nickname
            `((operator_nickname . ,operator-nickname)))
        ,@(when sender-nickname
            `((sender_nickname . ,sender-nickname)))))))

(cl-defun qq-message-test-history-result
    (messages start-sequence end-sequence
              &key (response-start start-sequence)
              (response-end end-sequence) (unsupported-count 0))
  "Return a closed history result containing MESSAGES.

START-SEQUENCE and END-SEQUENCE are echoed as the requested range."
  `((account_id . "slot-a")
    (requested_start_sequence . ,start-sequence)
    (requested_end_sequence . ,end-sequence)
    (response_start_sequence . ,response-start)
    (response_end_sequence . ,response-end)
    (unsupported_message_count . ,unsupported-count)
    (messages . ,(vconcat (mapcar #'copy-tree (append messages nil))))))

(cl-defun qq-message-test-dataline-message
    (&key (variant "desktop")
     (peer-uid "u_Wcc5rknRRqRO8y5gxMD6sA")
     (message-id "7348923749823749823")
     (direction "received") (sent-at 1784700000)
     (client-sequence "0") (message-sequence "0")
     (random 0) (text "durable DataLine text"))
  "Return one closed DataLine timeline message fixture."
  `((message_id . ,message-id)
    (chat . ((peer_uid . ,peer-uid) (variant . ,variant)))
    (direction . ,direction)
    (sent_at . ,sent-at)
    (client_sequence . ,client-sequence)
    (message_sequence . ,message-sequence)
    (random . ,random)
    (segments . (((kind . "text")
                  (payload . ((text . ,text))))))))

(cl-defun qq-message-test-dataline-page
    (messages &key (variant "desktop") requested-cursor
              older-cursor newer-cursor (direction "older")
              has-older has-newer)
  "Return a closed unified DataLine page containing MESSAGES."
  `((history_version . ,qq-message-history-port-version)
    (account_id . "slot-a")
    (conversation
     . ((kind . "dataline")
        (peer_uid . "u_Wcc5rknRRqRO8y5gxMD6sA")
        (variant . ,variant)))
    (direction . ,direction)
    (requested_cursor . ,(copy-tree requested-cursor))
    (messages
     . ,(vconcat
         (mapcar (lambda (message)
                   `((kind . "dataline")
                     (message . ,(copy-tree message))))
                 (append messages nil))))
    (unsupported_message_count . 0)
    (older_cursor . ,(copy-tree older-cursor))
    (newer_cursor . ,(copy-tree newer-cursor))
    (has_older . ,(if has-older t :false))
    (has_newer_materialized . ,(if has-newer t :false))))

(defun qq-message-test-history-cursor
    (session-key position direction &optional account-id)
  "Return a direction-scoped unified history cursor fixture."
  `((version . ,qq-message-history-port-version)
    (account_id . ,(or account-id "slot-a"))
    (conversation
     . ,(qq-message--history-conversation-params session-key))
    (direction . ,(symbol-name direction))
    (position . ,(copy-tree position))))

(defmacro qq-message-test-with-state (&rest body)
  "Run BODY with one selected account and isolated message projection state."
  (declare (indent 0) (debug t))
  `(let ((qq-account--accounts (make-hash-table :test #'equal))
         (qq-account--account-order nil)
         (qq-account--current-account-id nil)
         (qq-account--gateway-instance-id nil)
         (qq-account--resync-request-id nil)
         (qq-account-registry-changed-hook nil)
         (qq-account-selection-changed-hook nil)
         (qq-account-desync-hook nil)
         (qq-account-projection-resync-hook nil)
         (qq-attachment--attachments
          (make-hash-table :test #'equal))
         (qq-attachment--order nil)
         (qq-attachment-changed-hook nil)
         (qq-message--peer-uin-by-uid
          (make-hash-table :test #'equal))
         (qq-message--pending-recalls
          (make-hash-table :test #'equal))
         (qq-message--pending-reactions
          (make-hash-table :test #'equal))
         (qq-message--pending-essences
          (make-hash-table :test #'equal))
         (qq-message--pending-sends
          (make-hash-table :test #'equal))
         (qq-message--live-frontiers
          (make-hash-table :test #'equal))
         (qq-runtime--app nil)
         (qq-runtime--accounts (make-hash-table :test #'equal))
         (qq-state--partitions (make-hash-table :test #'equal))
         (qq-state--active-account-id nil)
         (qq-message-event-hook nil)
         (qq-message-projection-error-hook nil)
         (qq-server--state 'ready)
         (qq-state-change-hook nil))
     (unwind-protect
         (progn
           (qq-state-reset)
           (qq-account--replace-accounts
            (list (qq-message-test-account)) 'ready "gateway-test")
           (qq-runtime-with-account "slot-a"
             (qq-state-reset)
             ,@body))
       (qq-runtime-stop)
       (qq-state-reset))))

(ert-deftest qq-message-send-file-publishes-staged-resource-to-native-chats ()
  (qq-message-test-with-state
    (let (method params response-callback receipt)
      (cl-letf (((symbol-function 'qq-server-ready-p) (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (sent-method sent-params callback _errback
                                      &optional _early)
                   (setq method sent-method
                         params sent-params
                         response-callback callback)
                   "file-send-request")))
        (should
         (equal
          (qq-message-send-file
           "group:8209413637" "res-group-file")
          "file-send-request"))
        (should (equal method "file.send"))
        (should
         (equal
          params
          '((account_id . "slot-a")
            (conversation . ((kind . "group")
                             (group_uin . "8209413637")))
            (resource_id . "res-group-file"))))
        (should
         (equal
          (qq-message-send-file
           "private:10001" "res-private-file"
           (lambda (value) (setq receipt value)))
          "file-send-request"))
        (should
         (equal
          params
          '((account_id . "slot-a")
            (conversation . ((kind . "private") (peer_uin . "10001")))
            (resource_id . "res-private-file"))))
        (funcall
         response-callback
         '((account_id . "slot-a")
           (peer_uin . "10001")
           (sent_at . 1784700000)
           (server_sequence . "42")
           (client_sequence . "42001")
           (random . 7)
           (resource_id . "res-private-file")
           (file_name . "notes.txt")
           (file_size . "4096")
           (fast_path . :false)))
        (should (equal (alist-get 'file_name receipt) "notes.txt"))))))

(ert-deftest qq-message-private-file-receipt-correlates-peer-and-resource ()
  (qq-message-test-with-state
    (dolist (mismatch '((peer_uin . "10002")
                        (resource_id . "res-other-file")))
      (let (failure-body failure-reason)
        (cl-letf
            (((symbol-function 'qq-server-ready-p) (lambda () t))
             ((symbol-function 'qq-server-capabilities)
              (lambda () qq-message-test-capabilities))
             ((symbol-function 'qq-server-send)
              (lambda (_method _params callback _errback &optional _early)
                (let ((receipt
                       (copy-tree
                        '((account_id . "slot-a")
                          (peer_uin . "10001")
                          (sent_at . 1784700000)
                          (server_sequence . "42")
                          (client_sequence . "42001")
                          (random . 7)
                          (resource_id . "res-private-file")
                          (file_name . "notes.txt")
                          (file_size . "4096")
                          (fast_path . t)))))
                  (setcdr (assq (car mismatch) receipt) (cdr mismatch))
                  (funcall callback receipt))
                "file-send-request")))
          (qq-message-send-file
           "private:10001" "res-private-file" nil
           (lambda (body reason)
             (setq failure-body body
                   failure-reason reason))))
        (should (equal (alist-get 'code failure-body)
                       "invalid_gateway_result"))
        (should (string-match-p "invalid private-file receipt"
                                failure-reason))))))

(ert-deftest qq-message-dataline-text-uses-the-pinned-class-operation ()
  (qq-message-test-with-state
    (let (method params receipt)
      (cl-letf (((symbol-function 'qq-server-ready-p) (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (sent-method sent-params callback _errback
                                      &optional _early)
                   (setq method sent-method params sent-params)
                   (funcall callback
                            '((account_id . "slot-a")
                              (chat
                               (peer_uid . "u_Wcc5rknRRqRO8y5gxMD6sA")
                               (variant . "desktop"))
                              (message_id . "7348923749823749823")
                              (sent_at . 1784700000)
                              (server_sequence . "0")
                              (client_sequence . "42001")
                              (random . 0)
                              (batch_id . "99")))
                   "dataline-send")))
        (should
         (equal
          (qq-message-send
           "dataline:desktop:u_Wcc5rknRRqRO8y5gxMD6sA"
           '(((type . "text") (data . ((text . "hello phone")))))
           nil (lambda (value) (setq receipt value)))
          "dataline-send")))
      (should (equal method "dataline.send_text"))
      (should
       (equal params
              '((account_id . "slot-a")
                (chat (peer_uid . "u_Wcc5rknRRqRO8y5gxMD6sA")
                      (variant . "desktop"))
                (text . "hello phone"))))
      (should (equal (alist-get 'batch_id receipt) "99"))
      (let ((message
             (car
              (qq-state-session-messages
               "dataline:desktop:u_Wcc5rknRRqRO8y5gxMD6sA"))))
        (should (equal (alist-get 'server-id message)
                       "7348923749823749823"))
        (should (equal (alist-get 'sender-name message) "Primary"))
        (should (eq (alist-get 'status message) 'sent)))
      (should (= (hash-table-count qq-message--pending-sends) 0)))))

(ert-deftest qq-message-dataline-file-promotes-from-durable-receipt ()
  (qq-message-test-with-state
    (let ((session-key "dataline:mobile:u_Wcc5rknRRqRO8y5gxMD6sA")
          method params receipt)
      (cl-letf (((symbol-function 'qq-server-ready-p) (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (sent-method sent-params callback _errback
                                      &optional _early)
                   (setq method sent-method params sent-params)
                   (funcall
                    callback
                    '((account_id . "slot-a")
                      (chat
                       (peer_uid . "u_Wcc5rknRRqRO8y5gxMD6sA")
                       (variant . "mobile"))
                      (message_id . "7348923749823749825")
                      (sent_at . 1784700001)
                      (server_sequence . "0")
                      (client_sequence . "42003")
                      (random . 2)
                      (file_name . "photo.png")
                      (file_size . "12345")
                      (file_kind . "image")
                      (fast_path . :false)))
                   "dataline-file-send")))
        (should
         (equal
          (qq-message-send-file
           session-key "res-dataline-file"
           (lambda (value) (setq receipt value)) nil
           '((type . "image")
             (data . ((file . "/tmp/photo.png") (name . "photo.png")))))
          "dataline-file-send")))
      (should (equal method "dataline.send_file"))
      (should
       (equal params
              '((account_id . "slot-a")
                (chat (peer_uid . "u_Wcc5rknRRqRO8y5gxMD6sA")
                      (variant . "mobile"))
                (resource_id . "res-dataline-file"))))
      (should (equal (alist-get 'message_id receipt)
                     "7348923749823749825"))
      (should-not (assq 'transfer_session_id receipt))
      (should-not (assq 'resource_id receipt))
      ;; The durable FILE observation replaces wire metadata but must retain
      ;; the correlated optimistic row's client-local presentation path.
      (qq-message--handle-event
       "dataline.message_received"
       '((account_id . "slot-a")
         (message
          (message_id . "7348923749823749825")
          (chat
           (peer_uid . "u_Wcc5rknRRqRO8y5gxMD6sA")
           (variant . "mobile"))
          (direction . "sent")
          (sent_at . 1784700001)
          (client_sequence . "42003")
          (message_sequence . "0")
          (random . 2)
          (segments
           . (((kind . "file")
               (payload . ((file_name . "photo.png")
                           (file_size . "12345")
                           (file_kind . "image")))))))))
      (let* ((message (car (qq-state-session-messages session-key)))
             (segment (car (alist-get 'segments message)))
             (data (alist-get 'data segment)))
        (should (equal (alist-get 'server-id message)
                       "7348923749823749825"))
        (should (eq (alist-get 'status message) 'sent))
        (should (equal (alist-get 'type segment) "file"))
        (should (equal (alist-get 'file data) "/tmp/photo.png"))
        (should (equal (alist-get 'file_name data) "photo.png"))
        (should (equal (alist-get 'file_size data) "12345"))
        (should (equal (alist-get 'file_kind data) "image"))
        (should-not (assq 'transfer_session_id data)))
      (should (= (hash-table-count qq-message--pending-sends) 0)))))

(ert-deftest qq-message-dataline-file-event-before-receipt-keeps-local-presentation ()
  (qq-message-test-with-state
    (let ((session-key "dataline:desktop:u_Wcc5rknRRqRO8y5gxMD6sA")
          response-callback)
      (cl-letf (((symbol-function 'float-time) (lambda (&optional _time) 1784700002.0))
                ((symbol-function 'qq-server-ready-p) (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (setq response-callback callback)
                   "dataline-file-send")))
        (qq-message-send-file
         session-key "res-dataline-file" nil nil
         '((type . "image")
           (data . ((file . "/tmp/before.png") (name . "before.png")))))
        (qq-message--handle-event
         "dataline.message_received"
         '((account_id . "slot-a")
           (message
            (message_id . "7348923749823749827")
            (chat
             (peer_uid . "u_Wcc5rknRRqRO8y5gxMD6sA")
             (variant . "desktop"))
            (direction . "sent")
            (sent_at . 1784700002)
            (client_sequence . "42005")
            (message_sequence . "0")
            (random . 4)
            (segments
             . (((kind . "file")
                 (payload . ((file_name . "before.png")
                             (file_size . "12345")
                             (file_kind . "image")))))))))
        (funcall
         response-callback
         '((account_id . "slot-a")
           (chat
            (peer_uid . "u_Wcc5rknRRqRO8y5gxMD6sA")
            (variant . "desktop"))
           (message_id . "7348923749823749827")
           (sent_at . 1784700002)
           (server_sequence . "0")
           (client_sequence . "42005")
           (random . 4)
           (file_name . "before.png")
           (file_size . "12345")
           (file_kind . "image")
           (fast_path . :false))))
      (let* ((messages (qq-state-session-messages session-key))
             (message (car messages))
             (data (alist-get 'data (car (alist-get 'segments message)))))
        (should (= (length messages) 1))
        (should (equal (alist-get 'server-id message)
                       "7348923749823749827"))
        (should (equal (alist-get 'file data) "/tmp/before.png"))
        (should (eq (alist-get 'status message) 'sent))))))

(ert-deftest qq-message-dataline-file-event-is-closed-and-projects-metadata ()
  (qq-message-test-with-state
    (qq-message--handle-event
     "dataline.message_received"
     '((account_id . "slot-a")
       (message
        (message_id . "7348923749823749826")
        (chat
         (peer_uid . "u_Wcc5rknRRqRO8y5gxMD6sA")
         (variant . "desktop"))
        (direction . "sent")
        (sent_at . 1784700002)
        (client_sequence . "42004")
        (message_sequence . "0")
        (random . 3)
        (segments
         . (((kind . "file")
             (payload
              . ((file_name . "photo.png")
                 (file_size . "12345")
                 (file_kind . "image")
                 (media_id
                  . "media-00000000-0000-4000-8000-000000000001")))))))))
    (let* ((message
            (car
             (qq-state-session-messages
              "dataline:desktop:u_Wcc5rknRRqRO8y5gxMD6sA")))
           (segment (car (alist-get 'segments message))))
      (should (equal (alist-get 'server-id message)
                     "7348923749823749826"))
      (should (equal (alist-get 'preview message) "[file:photo.png]"))
      (should (equal (alist-get 'type segment) "file"))
      (should (equal (alist-get 'file_name (alist-get 'data segment))
                     "photo.png"))
      (should (equal (alist-get 'media_id (alist-get 'data segment))
                     "media-00000000-0000-4000-8000-000000000001"))
      (should (equal (alist-get 'file_kind (alist-get 'data segment))
                     "image"))
      (should-not (assq 'transfer_session_id (alist-get 'data segment)))
      (should-not (assq 'dataline-transfer-session-id message)))))

(ert-deftest qq-message-dataline-ordered-file-segments-stay-one-timeline-row ()
  (qq-message-test-with-state
    (let ((session-key "dataline:desktop:u_Wcc5rknRRqRO8y5gxMD6sA"))
      (qq-message--handle-event
       "dataline.message_received"
       '((account_id . "slot-a")
         (message
          (message_id . "7348923749823749828")
          (chat
           (peer_uid . "u_Wcc5rknRRqRO8y5gxMD6sA")
           (variant . "desktop"))
          (direction . "received")
          (sent_at . 1784700003)
          (message_sequence . "84")
          (random . 5)
          (segments
           . (((kind . "file")
               (payload
                . ((file_name . "first.png")
                   (file_size . "4294967418")
                   (file_kind . "image")
                   (media_id
                    . "media-11111111-1111-4111-8111-111111111111"))))
              ((kind . "file")
               (payload
                . ((file_name . "second.mp4")
                   (file_size . "98765")
                   (file_kind . "video")
                   (media_id
                    . "media-22222222-2222-4222-8222-222222222222")))))))))
      (let* ((messages (qq-state-session-messages session-key))
             (message (car messages))
             (segments (alist-get 'segments message)))
        (should (= (length messages) 1))
        (should (equal (alist-get 'server-id message)
                       "7348923749823749828"))
        (should (equal (mapcar (lambda (segment)
                                (alist-get 'file_name
                                           (alist-get 'data segment)))
                              segments)
                       '("first.png" "second.mp4")))
        (should (equal (mapcar (lambda (segment)
                                (alist-get 'file_size
                                           (alist-get 'data segment)))
                              segments)
                       '("4294967418" "98765")))
        (should (equal (mapcar (lambda (segment)
                                (alist-get 'media_id
                                           (alist-get 'data segment)))
                              segments)
                       '("media-11111111-1111-4111-8111-111111111111"
                         "media-22222222-2222-4222-8222-222222222222")))
        (should (equal (alist-get 'preview message)
                       "[file:first.png] [file:second.mp4]"))))))

(ert-deftest qq-message-dataline-durable-event-deduplicates-the-receipt-row ()
  (qq-message-test-with-state
    (let ((session-key "dataline:desktop:u_Wcc5rknRRqRO8y5gxMD6sA"))
      (cl-letf (((symbol-function 'qq-server-ready-p) (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (funcall callback
                            '((account_id . "slot-a")
                              (chat
                               (peer_uid . "u_Wcc5rknRRqRO8y5gxMD6sA")
                               (variant . "desktop"))
                              (message_id . "7348923749823749823")
                              (sent_at . 1784700000)
                              (server_sequence . "0")
                              (client_sequence . "42001")
                              (random . 0)
                              (batch_id . "99")))
                   "dataline-send")))
        (qq-message-send
         session-key
         '(((type . "text") (data . ((text . "hello phone")))))))
      (qq-message--handle-event
       "dataline.message_received"
       '((account_id . "slot-a")
         (message
          (message_id . "7348923749823749823")
          (chat
           (peer_uid . "u_Wcc5rknRRqRO8y5gxMD6sA")
           (variant . "desktop"))
          (direction . "sent")
          (sent_at . 1784700000)
          (client_sequence . "42001")
          (message_sequence . "0")
          (random . 0)
          (segments
           . (((kind . "text")
               (payload . ((text . "hello phone")))))))))
      (let ((messages (qq-state-session-messages session-key)))
        (should (= (length messages) 1))
        (should (equal (alist-get 'server-id (car messages))
                       "7348923749823749823"))
        (should-not (alist-get 'message-seq (car messages)))
        (should (eq (alist-get 'status (car messages)) 'sent)))
      (should (= (hash-table-count qq-message--pending-sends) 0)))))

(ert-deftest qq-message-dataline-mobile-retains-its-local-send-locator ()
  (qq-message-test-with-state
    (let ((session-key "dataline:mobile:u_Wcc5rknRRqRO8y5gxMD6sA")
          sent-params)
      (cl-letf (((symbol-function 'qq-server-ready-p) (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (_method params callback _errback &optional _early)
                   (setq sent-params params)
                   (funcall callback
                            '((account_id . "slot-a")
                              (chat
                               (peer_uid . "u_Wcc5rknRRqRO8y5gxMD6sA")
                               (variant . "mobile"))
                              (message_id . "7348923749823749824")
                              (sent_at . 1784700000)
                              (server_sequence . "0")
                              (client_sequence . "42002")
                              (random . 1)
                              (batch_id . "100")))
                   "dataline-mobile-send")))
        (qq-message-send
         session-key
         '(((type . "text") (data . ((text . "hello mobile")))))))
      (should
       (equal
        (alist-get 'chat sent-params)
        '((peer_uid . "u_Wcc5rknRRqRO8y5gxMD6sA")
          (variant . "mobile"))))
      (let ((message (car (qq-state-session-messages session-key))))
        (should (equal (alist-get 'server-id message)
                       "7348923749823749824"))
        (should (eq (alist-get 'status message) 'sent)))
      (should (= (hash-table-count qq-message--pending-sends) 0)))))

(ert-deftest qq-message-unified-history-projects-mobile-dataline-page ()
  (qq-message-test-with-state
    (let* ((session-key
            "dataline:mobile:u_Wcc5rknRRqRO8y5gxMD6sA")
           (text (make-string 200 ?x))
           (message
            (qq-message-test-dataline-message
             :variant "mobile" :text text))
           (tail-cursor
            (qq-message-test-history-cursor
             session-key
             '((kind . "dataline")
               (message_id . "7348923749823749823"))
             'newer))
           method params meta)
      (cl-letf (((symbol-function 'qq-server-ready-p) (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (candidate candidate-params callback _errback
                          &optional _early)
                   (setq method candidate params candidate-params)
                   (funcall
                    callback
                    (qq-message-test-dataline-page
                     (list message) :variant "mobile"
                     :newer-cursor tail-cursor))
                   "history-page")))
        (should
         (equal
          (qq-message--request-history-page
           session-key nil 'older
           (lambda (value) (setq meta value)) nil 20)
          "history-page")))
      (should (equal method "message.get_history_page"))
      (should
       (equal
        params
        '((account_id . "slot-a")
          (conversation
           (kind . "dataline")
           (peer_uid . "u_Wcc5rknRRqRO8y5gxMD6sA")
           (variant . "mobile"))
          (direction . "older")
          (count . 20))))
      (should (= (plist-get meta :history-port-version)
                 qq-message-history-port-version))
      (should-not (plist-get meta :history-has-older-p))
      (should-not (plist-get meta :history-has-newer-materialized-p))
      (should (equal (plist-get meta :history-newer-cursor)
                     tail-cursor))
      (let ((projected (car (qq-state-session-messages session-key))))
        (should (equal (alist-get 'server-id projected)
                       "7348923749823749823"))
        (should (equal (alist-get 'chat-type projected) "134"))
        (should (equal (alist-get 'sender-name projected) "My phone"))
        (should-not (alist-get 'message-seq projected))
        (should (equal (alist-get 'raw-message projected) text))))))

(ert-deftest qq-message-unified-around-keeps-dataline-message-id-locator ()
  (qq-message-test-with-state
    (let* ((session-key
            "dataline:desktop:u_Wcc5rknRRqRO8y5gxMD6sA")
           (message-id "7348923749823749823")
           (center `((kind . "message") (message_id . ,message-id)))
           (tail-cursor
            (qq-message-test-history-cursor
             session-key
             `((kind . "dataline") (message_id . ,message-id))
             'newer))
           method params meta)
      (cl-letf (((symbol-function 'qq-server-ready-p) (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (candidate candidate-params callback _errback
                          &optional _early)
                   (setq method candidate params candidate-params)
                   (funcall
                   callback
                    `((history_version . ,qq-message-history-port-version)
                      (account_id . "slot-a")
                      (conversation
                       . ((kind . "dataline")
                          (peer_uid . "u_Wcc5rknRRqRO8y5gxMD6sA")
                          (variant . "desktop")))
                      (center . ,center)
                      (messages
                       . [((kind . "dataline")
                           (message
                            . ,(qq-message-test-dataline-message
                                :message-id message-id)))])
                      (unsupported_message_count . 0)
                      (older_cursor)
                      (newer_cursor . ,tail-cursor)
                      (has_older . :false)
                      (has_newer_materialized . :false)))
                   "history-around")))
        (qq-message--request-history-around
         session-key center (lambda (value) (setq meta value)) nil 21))
      (should (equal method "message.get_history_around"))
      (should
       (equal
        params
        `((account_id . "slot-a")
          (conversation
           (kind . "dataline")
           (peer_uid . "u_Wcc5rknRRqRO8y5gxMD6sA")
           (variant . "desktop"))
          (center . ,center)
          (count . 21))))
      (should-not (plist-get meta :history-has-older-p))
      (should-not (plist-get meta :history-has-newer-materialized-p))
      (should (equal (plist-get meta :history-newer-cursor)
                     tail-cursor)))))

(ert-deftest qq-message-unified-history-projects-native-private-page ()
  (qq-message-test-with-state
    (let* ((session-key "private:10001")
           (message
            (let ((message
                   (copy-tree
                    (alist-get 'message (qq-message-test-event)))))
              (setf (alist-get 'segments message) nil)
              message))
           (older-cursor
            (qq-message-test-history-cursor
             session-key
             '((kind . "timeline_row") (row_key . "7"))
             'older))
           method params meta)
      (cl-letf (((symbol-function 'qq-server-ready-p) (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (candidate candidate-params callback _errback
                          &optional _early)
                   (setq method candidate params candidate-params)
                   (funcall
                    callback
                    `((history_version . ,qq-message-history-port-version)
                      (account_id . "slot-a")
                      (conversation
                       . ((kind . "private") (peer_uin . "10001")))
                      (direction . "older")
                      (requested_cursor)
                      (messages
                       . [((kind . "native")
                           (row_key . "8")
                           (timeline_class . "authored")
                           (recalled . t)
                           (message . ,message))])
                      (unsupported_message_count . 0)
                      (older_cursor . ,older-cursor)
                      (newer_cursor)
                      (has_older . t)
                      (has_newer_materialized . :false)))
                   "history-page")))
        (qq-message--request-history-page
         session-key nil 'older
         (lambda (value) (setq meta value)) nil 20))
      (should (equal method "message.get_history_page"))
      (should
       (equal params
              '((account_id . "slot-a")
                (conversation (kind . "private") (peer_uin . "10001"))
                (direction . "older")
                (count . 20))))
      (should (equal (plist-get meta :history-older-cursor)
                     older-cursor))
      (let ((projected (car (qq-state-session-messages session-key))))
        (should (qq-state-message-recalled-p projected))
        (should-not (alist-get 'segments projected))
        (should (equal (alist-get 'preview projected)
                       "[message recalled]"))))))

(ert-deftest qq-message-unified-group-history-ignores-json-object-key-order ()
  (qq-message-test-with-state
    (let* ((session-key "group:20001")
           (request-cursor
            (qq-message-test-history-cursor
             session-key
             '((kind . "timeline_row") (row_key . "100"))
             'older))
           (response-cursor
            `((position (row_key . "100") (kind . "timeline_row"))
              (direction . "older")
              (conversation (group_uin . "20001") (kind . "group"))
              (account_id . "slot-a")
              (version . ,qq-message-history-port-version)))
           (newer-cursor
            `((position (row_key . "120") (kind . "timeline_row"))
              (direction . "newer")
              (conversation (group_uin . "20001") (kind . "group"))
              (account_id . "slot-a")
              (version . ,qq-message-history-port-version)))
           meta)
      (cl-letf (((symbol-function 'qq-server-ready-p) (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (funcall
                    callback
                    `((history_version . ,qq-message-history-port-version)
                      (account_id . "slot-a")
                      (conversation (group_uin . "20001") (kind . "group"))
                      (direction . "older")
                      (requested_cursor . ,response-cursor)
                      (messages . [])
                      (unsupported_message_count . 0)
                      (older_cursor)
                      (newer_cursor . ,newer-cursor)
                      (has_older . :false)
                      (has_newer_materialized . t)))
                   "history-page")))
        (qq-message--request-history-page
         session-key request-cursor 'older
         (lambda (value) (setq meta value)) nil 20))
      (should-not (plist-get meta :history-has-older-p))
      (should (plist-get meta :history-has-newer-materialized-p))
      (should
       (qq-message--history-cursor-equal-p
        (plist-get meta :history-newer-cursor) newer-cursor)))))

(ert-deftest qq-message-unified-history-rejects-cross-scope-cursors-locally ()
  (qq-message-test-with-state
    (let* ((group-cursor
            (qq-message-test-history-cursor
             "group:20001"
             '((kind . "timeline_row") (row_key . "100"))
             'older))
           (other-account-cursor
            (qq-message-test-history-cursor
             "private:10001"
             '((kind . "timeline_row") (row_key . "101"))
             'older "slot-b"))
           transport-called)
      (cl-letf (((symbol-function 'qq-server-send)
                 (lambda (&rest _args) (setq transport-called t))))
        (should-error
         (qq-message--request-history-page
          "private:10001" group-cursor 'older)
         :type 'error)
        (should-error
         (qq-message--request-history-page
          "private:10001" other-account-cursor 'older)
         :type 'error)
        (should-not transport-called)))))

(ert-deftest qq-message-unified-native-around-sends-one-closed-center ()
  (qq-message-test-with-state
    (let* ((message-id "7348923749823749823")
           (center `((kind . "message") (message_id . ,message-id)))
           method params meta)
      (cl-letf (((symbol-function 'qq-server-ready-p) (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (candidate candidate-params callback _errback
                          &optional _early)
                   (setq method candidate params candidate-params)
                   (funcall
                    callback
                    `((history_version . ,qq-message-history-port-version)
                      (account_id . "slot-a")
                      (conversation
                       . ((kind . "private") (peer_uin . "10001")))
                      (center . ,center)
                      (messages . [])
                      (unsupported_message_count . 1)
                      (older_cursor)
                      (newer_cursor)
                      (has_older . :false)
                      (has_newer_materialized . :false)))
                   "history-around")))
        (qq-message--request-history-around
         "private:10001" center
         (lambda (value) (setq meta value)) nil 21))
      (should (equal method "message.get_history_around"))
      (should
       (equal
        params
        `((account_id . "slot-a")
          (conversation (kind . "private") (peer_uin . "10001"))
          (center . ,center)
          (count . 21))))
      (should (= (plist-get meta :message-count) 0))
      (should-not (plist-get meta :history-has-newer-materialized-p)))))

(ert-deftest qq-message-dataline-send-rejects-endpoint-substitution-and-rich-content ()
  (qq-message-test-with-state
    (let (transport-called)
      (cl-letf (((symbol-function 'qq-server-send)
                 (lambda (&rest _args) (setq transport-called t))))
        (should-error
         (qq-message-send
          "dataline:desktop:u_account_uid"
          '(((type . "text") (data . ((text . "hello"))))))
         :type 'user-error)
        (should-error
         (qq-message-send
          "dataline:desktop:u_Wcc5rknRRqRO8y5gxMD6sA"
          '(((type . "face") (data . ((id . "1"))))))
         :type 'user-error)
        (should-not transport-called)))))

(ert-deftest qq-message-send-merged-forward-preserves-source-order ()
  (qq-message-test-with-state
    (let (sent-method sent-params callback-result)
      (cl-letf (((symbol-function 'qq-server-ready-p) (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (setq sent-method method
                         sent-params params)
                   (funcall
                    callback
                    '((account_id . "slot-a")
                      (resource_id . "native-long-message-resource")
                      (sent_at . 1785100000)
                      (server_sequence . "8765432109")
                      (client_sequence . "42001")
                      (random . 123)))
                   "request-forward")))
        (should
         (equal
          (qq-message-send-merged-forward
           "private:10001" "group:8209413637"
           '("9007199254742007001"
             "9007199254742007001"
             "9007199254742007002")
           (lambda (receipt) (setq callback-result receipt)))
          "request-forward"))
        (should (equal sent-method "message.send_merged_forward"))
        (should
         (equal
          sent-params
          '((account_id . "slot-a")
            (destination . ((kind . "group")
                            (group_uin . "8209413637")))
            (source . ((kind . "private")
                       (peer_uin . "10001")))
            (message_ids
             . ("9007199254742007001"
                "9007199254742007001"
                "9007199254742007002")))))
        (should
         (equal (alist-get 'resource_id callback-result)
                "native-long-message-resource"))))))

(ert-deftest qq-message-send-merged-forward-rejects-invalid-input-locally ()
  (qq-message-test-with-state
    (let (transport-called)
      (cl-letf (((symbol-function 'qq-server-send)
                 (lambda (&rest _arguments)
                   (setq transport-called t))))
        (should-error
         (qq-message-send-merged-forward
          "private:10001" "group:8209413637" nil)
         :type 'user-error)
        (should-error
         (qq-message-send-merged-forward
          "private:10001" "group:8209413637" '("local-pending"))
         :type 'user-error)
        (should-error
         (qq-message-send-merged-forward
          "service:u:mail:x" "group:8209413637"
          '("9007199254742007001"))
         :type 'user-error))
      (should-not transport-called))))

(ert-deftest qq-message-conversation-params-use-session-identity ()
  (should
   (equal
    (qq-message--conversation-params
     "private:18446744073709551615")
    '((kind . "private") (peer_uin . "18446744073709551615"))))
  (should
   (equal
    (qq-message--conversation-params "group:8209413637")
    '((kind . "group") (group_uin . "8209413637")))))

(ert-deftest qq-message-event-boundary-domainizes-and-owns-wire-data ()
  (qq-message-test-with-state
    (let* ((source-text (copy-sequence "mutable"))
           (event
            (qq-message-test-event
             :segments
             (list
              (qq-message-test-text-segment source-text)
              '((kind . "unsupported")
                (payload . ((native_keys . ("ark" "xml"))
                            (summary . "unsupported")))))))
           observed)
      (let* ((message (alist-get 'message event))
             (segments (alist-get 'segments message)))
        (setf (alist-get 'native_keys
                         (alist-get 'payload (cadr segments)))
              ["ark" "xml"]
              (alist-get 'segments message)
              (vconcat segments)))
      (let ((qq-message-event-hook
             (list (lambda (_event data) (setq observed data)))))
        (qq-rpc--handle-transport-event
         "message.received" event))
      (let* ((message (alist-get 'message observed))
             (segments (alist-get 'segments message))
             (text (alist-get 'text (alist-get 'payload (car segments))))
             (native-keys
              (alist-get 'native_keys
                         (alist-get 'payload (cadr segments)))))
        (should (listp segments))
        (should (listp native-keys))
        (should (equal (alist-get 'message_id message)
                       "7348923749823749823"))
        (should (equal text source-text))
        (should-not (eq text source-text))
        (aset text 0 ?X)
        (should (equal source-text "mutable"))))))

(ert-deftest qq-message-mark-read-sends-canonical-row-targets ()
  (qq-message-test-with-state
    (let* ((private
            '((session-key . "private:10001")
              (server-id . "7348923749823749823")
              (canonical-row-key . "17")
              (gateway-account-id . "slot-a")))
           (idless-group
            '((session-key . "group:8209413637")
              (canonical-row-key . "18")
              (gateway-account-id . "slot-a")))
           calls receipts)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (push (list method (copy-tree params)) calls)
                   (funcall
                    callback
                    `((account_id . "slot-a")
                      (row_key . ,(alist-get 'row_key params))))
                   (format "read-%d" (length calls)))))
        (should (qq-message-read-capable-p private))
        (should (qq-message-read-capable-p idless-group))
        (qq-message-mark-read
         private (lambda (receipt) (push receipt receipts)))
        (qq-message-mark-read
         idless-group (lambda (receipt) (push receipt receipts))))
      (setq calls (nreverse calls)
            receipts (nreverse receipts))
      (should
       (equal
        calls
        '(("message.mark_read"
           ((account_id . "slot-a")
            (conversation . ((kind . "private") (peer_uin . "10001")))
            (row_key . "17")))
          ("message.mark_read"
           ((account_id . "slot-a")
            (conversation . ((kind . "group")
                             (group_uin . "8209413637")))
            (row_key . "18"))))))
      (should
       (equal receipts
              '(((account_id . "slot-a") (row_key . "17"))
                ((account_id . "slot-a") (row_key . "18"))))))))

(ert-deftest qq-message-projects-group-segments-and-metadata ()
  (qq-message-test-with-state
    (let ((segments
           (list
            (qq-message-test-text-segment "hello")
            '((kind . "at")
              (payload . ((qq . "10002") (name . "Primary"))))
            '((kind . "reply")
              (payload
               . ((target
                   . ((kind . "native")
                      (sequence . "4000000001"))))))
            '((kind . "face")
              (payload . ((type . "basic") (id . "178"))))
            '((kind . "record") (payload . ((duration_seconds . 17))))
            '((kind . "unsupported")
              (payload
               . ((native_keys . ("text.pb_reserve"))
                  (summary . "text.pb_reserve")
                  (raw . ((fallback_text . "@Alice")))))))))
      (qq-message--handle-event
       "message.received"
       (qq-message-test-event
        :conversation
        '((kind . "group")
          (group_uin . "8209413637")
          (group_name . "Protocol Lab"))
        :sender-presentation
        '((member_name . "Alice") (nickname . "Alice Nick"))
        :segments segments))
      (let* ((session-key "group:8209413637")
             (messages (qq-state-session-messages session-key))
             (message (car messages))
             (internal (alist-get 'segments message)))
        (should (= (length messages) 1))
        (should (equal (alist-get 'server-id message)
                       "7348923749823749823"))
        (should (equal (alist-get 'message-seq message)
                       "9007199254740999"))
        (should (equal (alist-get 'native-client-sequence message)
                       "9007199254741001"))
        (should (= (alist-get 'native-random message) 7))
        (should-not (assq 'native-sent-at message))
        (should (equal (alist-get 'gateway-account-id message) "slot-a"))
        (should (equal (alist-get 'sender-name message) "Alice"))
        (should (equal (alist-get 'sender-secondary-name message)
                       "Alice Nick"))
        (should (equal (alist-get 'mention-kinds message) '(at-me)))
        (should (eq (alist-get 'status message) 'received))
        (should (equal (mapcar (lambda (it) (alist-get 'type it)) internal)
                       '("text" "at" "reply" "face" "record"
                         "__unsupported")))
        (should
         (equal
          (alist-get 'message_seq (alist-get 'data (nth 2 internal)))
          "4000000001"))
        (should (equal (alist-get 'id (alist-get 'data (nth 3 internal)))
                       "178"))
        (should (equal (alist-get 'face_type
                                  (alist-get 'data (nth 3 internal)))
                       "basic"))
        (should (= (alist-get 'duration_seconds
                              (alist-get 'data (nth 4 internal)))
                   17))
        (should (equal (alist-get 'title (qq-state-session session-key))
                       "Protocol Lab"))))))

(ert-deftest qq-message-record-projects-opaque-media-handle ()
  (let* ((media-id "media-aaaaaaaa-bbbb-4ccc-8ddd-eeeeeeeeeeee")
         (record `((kind . "record")
                   (payload . ((duration_seconds . 17)
                               (media_id . ,media-id)))))
         (internal (qq-message--segment-to-internal record)))
    (should (equal (alist-get 'media_id (alist-get 'data internal)) media-id))))

(ert-deftest qq-message-projects-legacy-image-as-url-image ()
  (let* ((record `((kind . "legacy_image")
                   (payload . ((md5 . "0123456789abcdeffedcba9876543210")
                               (url . "https://gchat.qpic.cn/gchatpic_new/group/0/abc/0")
                               (width . 320)
                               (height . 240)
                               (summary . "[动画表情]")))))
         (internal (qq-message--segment-to-internal record))
         (data (alist-get 'data internal)))
    (should (equal (alist-get 'type internal) "image"))
    (should (equal (alist-get 'url data)
                   "https://gchat.qpic.cn/gchatpic_new/group/0/abc/0"))
    (should (equal (alist-get 'summary data) "[动画表情]"))
    (should (equal (alist-get 'md5 data)
                   "0123456789abcdeffedcba9876543210"))
    (should (= (alist-get 'width data) 320))
    (should (= (alist-get 'height data) 240))))

(ert-deftest qq-message-projects-private-peer-by-self-endpoint ()
  (qq-message-test-with-state
    (qq-message--handle-event
     "message.received"
     (qq-message-test-event
      :sender-presentation
      '((nickname . "Alice Nick") (remark . "Alice Remark"))))
    (let* ((session-key "private:10001")
           (message (car (qq-state-session-messages session-key)))
           (session (qq-state-session session-key)))
      (should-not (alist-get 'self-p message))
      (should (equal (alist-get 'sender-name message) "Alice Remark"))
      (should (equal (alist-get 'sender-secondary-name message)
                     "Alice Nick"))
      (should (equal (alist-get 'peer-uin message) "10001"))
      (should (equal (alist-get 'peer-uid message) "u_peer"))
      (should (equal (alist-get 'peer-uid session) "u_peer"))
      (should (equal (gethash '("slot-a" "u_peer")
                              qq-message--peer-uin-by-uid)
                     "10001")))))

(ert-deftest qq-message-projects-nonselected-managed-account ()
  (qq-message-test-with-state
    (let (observed)
      (add-hook 'qq-message-event-hook
                (lambda (event data) (setq observed (list event data))))
      (qq-account--upsert-account
       (qq-message-test-account
        "slot-b" "10003" "u_other_self")
       'changed)
      (qq-message--handle-event
       "message.received"
       (qq-message-test-event
        :account-id "slot-b"
        :recipient '((uin . "10003") (uid . "u_other_self"))))
      (should (equal (car observed) "message.received"))
      (should (equal (alist-get 'account_id (cadr observed)) "slot-b"))
      (should-not (qq-state-sessions))
      (qq-runtime-with-account "slot-b"
        (should (qq-state-session "private:10001"))
        (should
         (equal (alist-get 'gateway-account-id
                           (car (qq-state-session-messages "private:10001")))
                "slot-b"))))))

(ert-deftest qq-message-temp-projects-on-the-private-product-session ()
  (qq-message-test-with-state
    (let (reason)
      (add-hook 'qq-message-projection-error-hook
                (lambda (_event _data failure) (setq reason failure)))
      (qq-message--handle-event
       "message.received"
       (qq-message-test-event
        :conversation
        '((kind . "temp")
          (name . "Temporary")
          (from_tiny_id . "1")
          (to_tiny_id . "2"))))
      (should-not reason)
      (let ((message
             (car (qq-state-session-messages "private:10001"))))
        (should (equal (alist-get 'message-type message) "temp"))
        (should (equal (alist-get 'peer-uid message) "u_peer"))))))

(ert-deftest qq-message-sequence-recall-waits-for-group-message ()
  (qq-message-test-with-state
    (qq-message--handle-event
     "message.recalled" (qq-message-test-recall))
    (should (= (hash-table-count qq-message--pending-recalls) 1))
    (qq-message--handle-event
     "message.received"
     (qq-message-test-event
      :conversation
      '((kind . "group")
        (group_uin . "8209413637")
        (group_name . "Protocol Lab")
        (sender_card . "Alice"))))
    (let ((message
           (car (qq-state-session-messages "group:8209413637"))))
      (should (qq-state-message-recalled-p message))
      (should (= (hash-table-count qq-message--pending-recalls) 0)))))

(ert-deftest qq-message-reaction-applies-authoritative-count ()
  (qq-message-test-with-state
    (qq-message--handle-event
     "message.received"
     (qq-message-test-event
      :conversation
      '((kind . "group") (group_uin . "8209413637")
        (group_name . "Protocol Lab") (sender_card . "Alice"))))
    (qq-message--handle-event
     "message.reaction_changed"
     (qq-message-test-reaction :count 7))
    (let* ((message
            (car (qq-state-session-messages "group:8209413637")))
           (reaction (car (qq-state-message-reactions message))))
      (should (equal (alist-get 'emoji-id reaction) "178"))
      (should (equal (alist-get 'emoji-type reaction) "1"))
      (should (= (alist-get 'count reaction) 7))
      (should-not (alist-get 'chosen-p reaction)))))

(ert-deftest qq-message-projects-structured-reaction-snapshot ()
  (qq-message-test-with-state
    (let* ((event
            (qq-message-test-event
             :conversation
             '((kind . "group") (group_uin . "8209413637")
               (group_name . "Protocol Lab") (sender_card . "Alice"))))
           (message (alist-get 'message event)))
      (setf
       (alist-get 'message event)
       (append
        message
        '((emoji_likes_list
           . (((emoji . ((kind . "qq_face") (id . 424)))
               (count . 2)
               (is_clicked . :false))
              ((emoji . ((kind . "unicode") (value . "👍")))
               (count . 1)
               (is_clicked . t)))))))
      (qq-message--handle-event "message.received" event)
      (should
       (equal
        (qq-state-message-reactions
         (car (qq-state-session-messages "group:8209413637")))
        '(((emoji-id . "424") (emoji-type . "1")
           (count . 2) (chosen-p))
          ((emoji-id . "128077") (emoji-type . "2")
           (count . 1) (chosen-p . t))))))))

(ert-deftest qq-message-sequence-reaction-waits-for-message ()
  (qq-message-test-with-state
    (qq-message--handle-event
     "message.reaction_changed"
     (qq-message-test-reaction
      :operator-uid "u_self" :operator-uin "10002" :count 4))
    (should (= (hash-table-count qq-message--pending-reactions) 1))
    (qq-message--handle-event
     "message.received"
     (qq-message-test-event
      :conversation
      '((kind . "group") (group_uin . "8209413637")
        (group_name . "Protocol Lab") (sender_card . "Alice"))))
    (let* ((message
            (car (qq-state-session-messages "group:8209413637")))
           (reaction (car (qq-state-message-reactions message))))
      (should (= (alist-get 'count reaction) 4))
      (should (alist-get 'chosen-p reaction))
      (should (= (hash-table-count
                  qq-message--pending-reactions)
                 0)))))

(ert-deftest qq-message-essence-applies-authoritative-metadata ()
  (qq-message-test-with-state
    (qq-message--handle-event
     "message.received"
     (qq-message-test-event
      :conversation
      '((kind . "group") (group_uin . "8209413637")
        (group_name . "Protocol Lab") (sender_card . "Alice"))))
    (qq-message--handle-event
     "message.essence_changed"
     (qq-message-test-essence))
    (let ((message
           (car (qq-state-session-messages "group:8209413637"))))
      (should (eq (alist-get 'essence-p message) t))
      (should (equal (alist-get 'essence-sender-id message) "10001"))
      (should (equal (alist-get 'essence-operator-id message) "10002"))
      (should (= (alist-get 'essence-changed-at message) 1784700000))
      (should (equal (alist-get 'essence-operator-nickname message)
                     "Moderator")))))

(ert-deftest qq-message-native-target-essence-waits-for-message ()
  (qq-message-test-with-state
    (qq-message--handle-event
     "message.essence_changed"
     (qq-message-test-essence :is-set :false))
    (should (= (hash-table-count qq-message--pending-essences) 1))
    (qq-message--handle-event
     "message.received"
     (qq-message-test-event
      :conversation
      '((kind . "group") (group_uin . "8209413637")
        (group_name . "Protocol Lab") (sender_card . "Alice"))))
    (let ((message
           (car (qq-state-session-messages "group:8209413637"))))
      (should-not (alist-get 'essence-p message))
      (should (equal (alist-get 'essence-operator-id message) "10002"))
      (should (= (hash-table-count qq-message--pending-essences) 0)))))

(ert-deftest qq-message-received-event-bumps-active-session-in-recent ()
  (qq-message-test-with-state
   (let ((key-set (make-hash-table :test #'equal)))
     (dolist (key '("group:20001" "private:10001"))
       (puthash key t key-set))
     (setq qq-state--recent-session-keys '("group:20001" "private:10001")
           qq-state--recent-session-key-set key-set))
   (qq-message--handle-event
    "message.received"
    (qq-message-test-event :message-id "7348923749823749825"))
   ;; Live activity reorders the recent projection immediately.
   (should (equal (qq-state-recent-session-keys)
                  '("private:10001" "group:20001")))))

(ert-deftest qq-message-direct-private-recall-marks-exact-message ()
  (qq-message-test-with-state
    (qq-message--handle-event
     "message.received" (qq-message-test-event))
    (qq-message--handle-event
     "message.recalled"
     (qq-message-test-recall
      :conversation '((kind . "private") (peer_uid . "u_peer"))
      :target
      '((kind . "message")
        (message_id . "7348923749823749823")
        (sequence . "9007199254740999"))))
    (should
     (qq-state-message-recalled-p
      (car (qq-state-session-messages "private:10001"))))))

(ert-deftest qq-message-send-receipt-rekeys-on-exact-self-event ()
  (qq-message-test-with-state
    (let ((now (floor (float-time))) sent-method sent-params callback-result
          local-id)
      (let ((key-set (make-hash-table :test #'equal)))
        (dolist (key '("group:20001" "private:10001"))
          (puthash key t key-set))
        (setq qq-state--recent-session-keys
              '("group:20001" "private:10001")
              qq-state--recent-session-key-set key-set))
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback errback &optional _early)
                   (pcase method
                     ("message.send"
                      (setq sent-method method sent-params params)
                      (funcall callback
                               `((account_id . "slot-a")
                                 (sent_at . ,now)
                                 (server_sequence . "8765432109")
                                 (client_sequence . "42001")
                                 (random . 123)))
                      "request-send")
                     ;; Private send immediately tries C2C history recovery.
                     ("message.get_history"
                      (funcall errback nil "history not mocked in this test")
                      "request-history-skip")
                     (_ (error "unexpected method %S" method))))))
        (should
         (equal
          (qq-message-send
           "private:10001"
           '(((type . "text") (data . ((text . "hello")))))
           nil
           (lambda (receipt) (setq callback-result receipt)))
          "request-send"))
        (setq local-id
              (alist-get 'local-id
                         (car (qq-state-session-messages "private:10001"))))
        (should (equal sent-method "message.send"))
        (should
         (equal sent-params
                '((account_id . "slot-a")
                  (conversation . ((kind . "private")
                                   (peer_uin . "10001")))
                  (segments
                   . (((kind . "text")
                       (payload . ((text . "hello")))))))))
        (should (equal (alist-get 'client_sequence callback-result) "42001"))
        (should (= (hash-table-count qq-message--pending-sends) 1))
        ;; A confirmed local send promotes its session before any self-echo.
        (should (equal (qq-state-recent-session-keys)
                       '("private:10001" "group:20001")))
        (qq-message--handle-event
         "message.received"
         (qq-message-test-event
          :sent-at now
          :sender '((uin . "10002") (uid . "u_self"))
          :recipient '((uin . "10001") (uid . "u_peer"))
          :sequence "8765432109"
          :client-sequence "42001"
          :random 123))
        (let* ((messages (qq-state-session-messages "private:10001"))
               (message (car messages)))
          (should (= (length messages) 1))
          (should (equal (alist-get 'local-id message) local-id))
          (should (equal (alist-get 'server-id message)
                         "7348923749823749823"))
          (should (eq (alist-get 'status message) 'sent))
          (should (= (hash-table-count qq-message--pending-sends) 0)))))))
(ert-deftest qq-message-private-rekeys-when-receipt-sequence-is-client-echo ()
  "C2C PbSendMsgResp field 14 may echo client_sequence, not ContentHead.Sequence.

client_sequence + session must still rekey even when conversation sequences differ."
  (qq-message-test-with-state
    (let ((now (floor (float-time))) local-id)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method _params callback errback &optional _early)
                   (pcase method
                     ("message.send"
                      (funcall callback
                               `((account_id . "slot-a")
                                 (sent_at . ,now)
                                 ;; field-14 style client echo, not conversation seq
                                 (server_sequence . "42001")
                                 (client_sequence . "42001")
                                 (random . 99)))
                      "request-private-c2c")
                     ("message.get_history"
                      (funcall errback nil "history not mocked in this test")
                      "request-history-skip")
                     (_ (error "unexpected method %S" method))))))
        (should
         (equal
          (qq-message-send
           "private:10001"
           '(((type . "text") (data . ((text . "c2c-hello"))))))
          "request-private-c2c"))
        (setq local-id
              (alist-get 'local-id
                         (car (qq-state-session-messages "private:10001"))))
        (should (= (hash-table-count qq-message--pending-sends) 1))
        ;; Live private self-echo: conversation sequence differs from receipt.
        (qq-message--handle-event
         "message.received"
         (qq-message-test-event
          :sent-at now
          :sender '((uin . "10002") (uid . "u_self"))
          :recipient '((uin . "10001") (uid . "u_peer"))
          :sequence "9007199254740999"
          :client-sequence "42001"
          :random 99
          :segments (list (qq-message-test-text-segment "c2c-hello"))))
        (let* ((messages (qq-state-session-messages "private:10001"))
               (message
                (or (seq-find
                     (lambda (it) (equal (alist-get 'local-id it) local-id))
                     messages)
                    (car messages))))
          (should (= (length messages) 1))
          (should (equal (alist-get 'local-id message) local-id))
          (should (equal (alist-get 'server-id message)
                         "7348923749823749823"))
          (should (eq (alist-get 'status message) 'sent))
          (should (= (hash-table-count qq-message--pending-sends) 0)))))))

(ert-deftest qq-message-private-rekeys-when-push-sequence-is-outbound-client-seq ()
  "Live C2C CommonMessage stores outbound client_sequence on ContentHead.Sequence.

receipt.client_sequence=40909 and receipt.server_sequence=30202, while the
push carries sequence=40909 and client_sequence=30202."
  (qq-message-test-with-state
    (let ((now (floor (float-time))) local-id)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method _params callback errback &optional _early)
                   (pcase method
                     ("message.send"
                      (funcall callback
                               `((account_id . "slot-a")
                                 (sent_at . ,now)
                                 (server_sequence . "30202")
                                 (client_sequence . "40909")
                                 (random . 694993522)))
                      "request-private-inverted")
                     ("message.get_history"
                      (funcall errback nil "history not mocked in this test")
                      "request-history-skip")
                     (_ (error "unexpected method %S" method))))))
        (should
         (equal
          (qq-message-send
           "private:10001"
           '(((type . "text") (data . ((text . "test"))))))
          "request-private-inverted"))
        (setq local-id
              (alist-get 'local-id
                         (car (qq-state-session-messages "private:10001"))))
        (should (= (hash-table-count qq-message--pending-sends) 1))
        (qq-message--handle-event
         "message.received"
         (qq-message-test-event
          :sent-at now
          :sender '((uin . "10002") (uid . "u_self"))
          :recipient '((uin . "10001") (uid . "u_peer"))
          :sequence "40909"
          :client-sequence "30202"
          :random 694993522
          :segments (list (qq-message-test-text-segment "test"))))
        (let* ((messages (qq-state-session-messages "private:10001"))
               (message
                (or (seq-find
                     (lambda (it) (equal (alist-get 'local-id it) local-id))
                     messages)
                    (car messages))))
          (should (= (length messages) 1))
          (should (equal (alist-get 'local-id message) local-id))
          (should (equal (alist-get 'server-id message)
                         "7348923749823749823"))
          (should (eq (alist-get 'status message) 'sent))
          (should (= (hash-table-count qq-message--pending-sends) 0)))))))

(ert-deftest qq-message-private-send-recovers-snowflake-via-c2c-history ()
  "When OlPush self-echo is missing, fetch SsoGetC2cMsg by server_sequence."
  (qq-message-test-with-state
    (let ((now (floor (float-time))) local-id methods)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (push method methods)
                   (pcase method
                     ("message.send"
                      (funcall callback
                               `((account_id . "slot-a")
                                 (sent_at . ,now)
                                 (server_sequence . "30203")
                                 (client_sequence . "40910")
                                 (random . 1689609174)))
                      "request-private-recover")
                     ("message.get_history"
                      (should
                       (equal params
                              '((account_id . "slot-a")
                                (conversation
                                 . ((kind . "private")
                                    (peer_uin . "10001")))
                                (start_sequence . "30203")
                                (end_sequence . "30203"))))
                      (funcall callback
                               `((account_id . "slot-a")
                                 (requested_start_sequence . "30203")
                                 (requested_end_sequence . "30203")
                                 (response_start_sequence . "30203")
                                 (response_end_sequence . "30203")
                                 (unsupported_message_count . 0)
                                 (messages
                                  .
                                  (,(alist-get
                                     'message
                                     (qq-message-test-event
                                      :sent-at now
                                      :message-id "72057595727537110"
                                      :sender '((uin . "10002")
                                                (uid . "u_self"))
                                      :recipient '((uin . "10001")
                                                   (uid . "u_peer"))
                                      :sequence "40910"
                                      :client-sequence "30203"
                                      :random 1689609174
                                      :segments
                                      (list
                                       (qq-message-test-text-segment
                                        "rekey-probe"))))))))
                      "request-private-history")
                     (_ (error "unexpected method %S" method))))))
        (should
         (equal
          (qq-message-send
           "private:10001"
           '(((type . "text") (data . ((text . "rekey-probe"))))))
          "request-private-recover"))
        (setq local-id
              (alist-get 'local-id
                         (car (qq-state-session-messages "private:10001"))))
        (let* ((messages (qq-state-session-messages "private:10001"))
               (message
                (or (seq-find
                     (lambda (it) (equal (alist-get 'local-id it) local-id))
                     messages)
                    (car messages))))
          (should (equal (nreverse methods)
                         '("message.send" "message.get_history")))
          (should (= (length messages) 1))
          (should (equal (alist-get 'local-id message) local-id))
          (should (equal (alist-get 'server-id message)
                         "72057595727537110"))
          (should (eq (alist-get 'status message) 'sent))
          (should (= (hash-table-count qq-message--pending-sends) 0)))))))

(ert-deftest qq-message-group-send-recovers-snowflake-via-group-history ()
  "When the group self-echo push never arrives, fetch SsoGetGroupMsg by sequence."
  (qq-message-test-with-state
    (let ((now (floor (float-time))) local-id methods)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (push method methods)
                   (pcase method
                     ("message.send"
                      (funcall callback
                               `((account_id . "slot-a")
                                 (sent_at . ,now)
                                 (server_sequence . "8765432112")
                                 (client_sequence . "42005")
                                 (random . 7777)))
                      "request-group-recover")
                     ("message.get_history"
                      (should
                       (equal params
                              '((account_id . "slot-a")
                                (conversation
                                 . ((kind . "group")
                                    (group_uin . "8209413637")))
                                (start_sequence . "8765432112")
                                (end_sequence . "8765432112"))))
                      (funcall callback
                               `((account_id . "slot-a")
                                 (requested_start_sequence . "8765432112")
                                 (requested_end_sequence . "8765432112")
                                 (response_start_sequence . "8765432112")
                                 (response_end_sequence . "8765432112")
                                 (unsupported_message_count . 0)
                                 (messages
                                  .
                                  (,(alist-get
                                     'message
                                     (qq-message-test-event
                                      :sent-at now
                                      :message-id "72057595727537120"
                                      :sender '((uin . "10002")
                                                (uid . "u_self"))
                                      :conversation
                                      '((kind . "group")
                                        (group_uin . "8209413637"))
                                      :sequence "8765432112"
                                      :random 7777
                                      :message-type 82
                                      :segments
                                      (list
                                       (qq-message-test-text-segment
                                        "group-rekey-probe"))))))))
                      "request-group-history")
                     (_ (error "unexpected method %S" method))))))
        (should
         (equal
          (qq-message-send
           "group:8209413637"
           '(((type . "text") (data . ((text . "group-rekey-probe"))))))
          "request-group-recover"))
        (setq local-id
              (alist-get 'local-id
                         (car (qq-state-session-messages "group:8209413637"))))
        (let* ((messages (qq-state-session-messages "group:8209413637"))
               (message
                (or (seq-find
                     (lambda (it) (equal (alist-get 'local-id it) local-id))
                     messages)
                    (car messages))))
          (should (equal (nreverse methods)
                         '("message.send" "message.get_history")))
          (should (= (length messages) 1))
          (should (equal (alist-get 'local-id message) local-id))
          (should (equal (alist-get 'server-id message)
                         "72057595727537120"))
          (should (eq (alist-get 'status message) 'sent))
          (should (= (hash-table-count qq-message--pending-sends) 0)))))))

(ert-deftest qq-message-send-lifts-reference-out-of-rich-content ()
  (qq-message-test-with-state
    (let ((now (floor (float-time))) sent-method sent-params)
      (let ((segments
             (list
              (qq-message-test-reply-segment "7348923749823749823")
              '((type . "at")
                (data . ((qq . "10001") (name . "Alice"))))
              '((type . "face")
                (data . ((id . "178") (face_type . "basic"))))
              '((type . "text") (data . ((text . " hello")))))))
        (cl-letf (((symbol-function 'qq-server-ready-p)
                   (lambda () t))
                  ((symbol-function 'qq-server-capabilities)
                   (lambda () qq-message-test-capabilities))
                  ((symbol-function 'qq-server-send)
                   (lambda (method params callback errback &optional _early)
                     (pcase method
                       ("message.send"
                        (setq sent-method method sent-params params)
                        (funcall callback
                                 `((account_id . "slot-a")
                                   (sent_at . ,now)
                                   (server_sequence . "8765432110")
                                   (client_sequence . "42002")
                                   (random . 124)))
                        "request-rich-send")
                       ;; Send recovery fetches the snowflake from history.
                       ("message.get_history"
                        (funcall errback nil "history not mocked in this test")
                        "request-history-skip")
                       (_ (error "unexpected method %S" method))))))
          (should
           (equal
            (qq-message-send "group:8209413637" segments)
            "request-rich-send"))
          (should (equal sent-method "message.send"))
          (should
           (equal
            sent-params
            `((account_id . "slot-a")
              (conversation
               . ((kind . "group") (group_uin . "8209413637")))
              (reply_to
               . ((kind . "message")
                  (message_id . "7348923749823749823")))
              (segments
               . (((kind . "mention")
                   (payload
                    . ((target . ((kind . "user") (uin . "10001")))
                       (display . "Alice"))))
                  ((kind . "face")
                   (payload . ((type . "basic") (id . "178"))))
                  ((kind . "text")
                   (payload . ((text . " hello")))))))))
          (let ((pending
                 (seq-find
                  (lambda (message)
                    (eq (alist-get 'status message) 'pending))
                  (qq-state-session-messages "group:8209413637"))))
            (should pending)
            (should (equal (alist-get 'segments pending) segments)))
          (should (= (hash-table-count qq-message--pending-sends) 1)))))))

(ert-deftest qq-message-send-image-keeps-local-path-off-wire ()
  (qq-message-test-with-state
    (let* ((now (floor (float-time)))
           (attachment-id "att-aaaaaaaa-bbbb-4ccc-8ddd-eeeeeeeeeeee")
           (wire-segments
            `(((type . "image")
               (data . ((attachment_id . ,attachment-id))))))
           (optimistic-segments
            '(((type . "image")
               (data . ((file . "/tmp/private-source.png")
                        (summary . "[图片]")
                        (sub_type . 0))))))
           sent-params)
      (qq-attachment--upsert
       (qq-message-test-ready-image attachment-id) 'test)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback errback &optional _early)
                   (pcase method
                     ("message.send"
                      (setq sent-params params)
                      (funcall callback
                               `((account_id . "slot-a")
                                 (sent_at . ,now)
                                 (server_sequence . "8765432111")
                                 (client_sequence . "42003")
                                 (random . 125)))
                      "request-image-send")
                     ("message.get_history"
                      (funcall errback nil "history not mocked in this test")
                      "request-history-skip")
                     (_ (error "unexpected method %S" method))))))
        (should
         (equal
          (qq-message-send
           "group:8209413637" wire-segments nil nil nil optimistic-segments)
          "request-image-send"))
        (should
         (equal
          (alist-get 'segments sent-params)
          `(((kind . "image")
             (payload . ((attachment_id . ,attachment-id)))))))
        (should-not
         (string-match-p "/tmp/private-source\\.png"
                         (prin1-to-string sent-params)))
        (let ((pending
               (seq-find
                (lambda (message)
                  (eq (alist-get 'status message) 'pending))
                (qq-state-session-messages "group:8209413637"))))
          (should pending)
          (should (equal (alist-get 'segments pending)
                         optimistic-segments)))))))

(ert-deftest qq-message-serializes-record-attachment-id-only ()
  (qq-message-test-with-state
    (let ((attachment-id "att-aaaaaaaa-bbbb-4ccc-8ddd-eeeeeeeeeeef"))
      (qq-attachment--upsert
       (qq-message-test-ready-record attachment-id) 'test)
      (should
       (equal
        (qq-message--prepare-outbound
         "group:8209413637"
         `(((type . "record")
            (data . ((attachment_id . ,attachment-id)))))
         "slot-a")
        `(:reply-to nil
          :segments
          (((kind . "record")
            (payload . ((attachment_id . ,attachment-id))))))))
      (should-error
       (qq-message--prepare-outbound
        "group:8209413637"
        `(((type . "image")
           (data . ((attachment_id . ,attachment-id)))))
        "slot-a")
       :type 'user-error))))

(ert-deftest qq-message-serializes-video-attachment-id-only ()
  (qq-message-test-with-state
    (let ((attachment-id "att-aaaaaaaa-bbbb-4ccc-8ddd-eeeeeeeeeee0"))
      (qq-attachment--upsert
       (qq-message-test-ready-video attachment-id) 'test)
      (should
       (equal
        (qq-message--prepare-outbound
         "group:8209413637"
         `(((type . "video")
            (data . ((attachment_id . ,attachment-id)
                     (file . "/tmp/must-not-cross-wire.mp4")))))
         "slot-a")
        `(:reply-to nil
          :segments
          (((kind . "video")
            (payload . ((attachment_id . ,attachment-id))))))))
      (should-error
       (qq-message--prepare-outbound
        "group:8209413637"
        `(((type . "image")
           (data . ((attachment_id . ,attachment-id)))))
        "slot-a")
       :type 'user-error))))

(ert-deftest qq-message-send-allows-unloaded-reply-reference ()
  (qq-message-test-with-state
    (let (sent-method sent-params)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params _callback _errback &optional _early)
                   (setq sent-method method
                         sent-params params)
                   "request-unloaded-reply")))
        (should
         (equal
          (qq-message-send
           "private:10001"
           (list
            (qq-message-test-reply-segment "7348923749823749823")
            '((type . "text") (data . ((text . "hello"))))))
          "request-unloaded-reply"))
        (should (equal sent-method "message.send"))
        (should
         (equal
          sent-params
          '((account_id . "slot-a")
            (conversation . ((kind . "private") (peer_uin . "10001")))
            (reply_to
             . ((kind . "message")
                (message_id . "7348923749823749823")))
            (segments
             . (((kind . "text")
                 (payload . ((text . "hello")))))))))
        (let ((pending
               (car (qq-state-session-messages "private:10001"))))
          (should pending)
          (should
           (equal
            (alist-get 'segments pending)
            (list
             (qq-message-test-reply-segment "7348923749823749823")
             '((type . "text") (data . ((text . "hello")))))))
          (should (eq (alist-get 'status pending) 'pending)))))))

(ert-deftest
    qq-message-send-rejects-malformed-or-duplicate-reply-before-pending
    ()
  (qq-message-test-with-state
    (let ((sent nil))
      (cl-letf (((symbol-function 'qq-server-send)
                 (lambda (&rest _arguments) (setq sent t))))
        (dolist (message-id
                 '(42 "0" "07348923749823749823"
                   "18446744073709551616"))
          (should-error
           (qq-message-send
            "private:10001"
            (list
             (qq-message-test-reply-segment message-id)
             '((type . "text") (data . ((text . "hello"))))))
           :type 'user-error))
        (should-error
         (qq-message-send
          "private:10001"
          (list
           (qq-message-test-reply-segment "7348923749823749823")
           '((type . "text") (data . ((text . "hello"))))
           (qq-message-test-reply-segment "7348923749823749824")))
         :type 'user-error)
        (should-not sent)
        (should-not (qq-state-session-messages "private:10001"))))))

(ert-deftest qq-message-send-requires-non-reply-content ()
  (qq-message-test-with-state
    (let ((sent nil))
      (cl-letf (((symbol-function 'qq-server-send)
                 (lambda (&rest _arguments) (setq sent t))))
        (dolist
            (segments
             (list nil
                   (list
                    (qq-message-test-reply-segment
                     "7348923749823749823"))))
          (should-error
           (qq-message-send "private:10001" segments)
           :type 'user-error))
        (should-not sent)
        (should-not (qq-state-session-messages "private:10001"))))))

(ert-deftest qq-message-reply-does-not-count-toward-content-limit ()
  (qq-message-test-with-state
    (let* ((reply
             (qq-message-test-reply-segment "18446744073709551615"))
           (contents
            (cl-loop repeat 128
                     collect
                     '((type . "text") (data . ((text . "hello"))))))
           (outbound
            (qq-message--prepare-outbound
             "private:10001" (cons reply contents) "slot-a")))
      (should (= (length (plist-get outbound :segments)) 128))
      (should
       (equal
        (plist-get outbound :reply-to)
        '((kind . "message")
          (message_id . "18446744073709551615")))))))

(ert-deftest qq-message-poke-uses-exact-uin-and-local-gray-tip ()
  (qq-message-test-with-state
    (qq-state-upsert-session
     "group:8209413637"
     '((type . group) (target-id . "8209413637") (title . "Protocol Lab")))
    (let (sent-method sent-params callback-result)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (setq sent-method method sent-params params)
                   (funcall callback
                            '((account_id . "slot-a")
                              (target_uin . "9007199254741001")))
                   "request-poke")))
        (should
         (equal
          (qq-message-send-poke
           "group:8209413637" "9007199254741001"
           (lambda (result) (setq callback-result result)))
          "request-poke"))
        (should (equal sent-method "message.poke"))
        (should
         (equal sent-params
                '((account_id . "slot-a")
                  (conversation
                   . ((kind . "group") (group_uin . "8209413637")))
                  (target_uin . "9007199254741001"))))
        (let* ((message
                (car (qq-state-session-messages "group:8209413637")))
               (segment (car (alist-get 'segments message)))
               (data (alist-get 'data segment)))
          (should (eq (alist-get 'timeline-class message) 'service))
          (should (alist-get 'local-poke-p message))
          (should (equal (alist-get 'type segment) "gray-tip"))
          (should (equal (alist-get 'kind data) "poke"))
          (should (equal (alist-get 'actor-id data) "10002"))
          (should (equal (alist-get 'target-id data)
                         "9007199254741001")))
        (should (equal (alist-get 'target_uin callback-result)
                       "9007199254741001"))))))

(ert-deftest qq-message-authoritative-poke-promotes-local-gray-tip ()
  (qq-message-test-with-state
    (qq-state-upsert-session
     "group:8209413637"
     '((type . group) (target-id . "8209413637") (title . "Protocol Lab")))
    (cl-letf (((symbol-function 'float-time)
               (lambda (&optional _time) 1784700000))
              ((symbol-function 'qq-server-ready-p)
               (lambda () t))
              ((symbol-function 'qq-server-capabilities)
               (lambda () qq-message-test-capabilities))
              ((symbol-function 'qq-server-send)
               (lambda (_method _params callback _errback &optional _early)
                 (funcall callback
                          '((account_id . "slot-a")
                            (target_uin . "9007199254741001")))
                 "request-poke")))
      (qq-message-send-poke
       "group:8209413637" "9007199254741001")
      (qq-message--handle-event
       "message.received" (qq-message-test-poke))
      (let* ((messages
              (qq-state-session-messages "group:8209413637"))
             (message (car messages)))
        (should (= (length messages) 1))
        (should (equal (alist-get 'server-id message)
                       "7348923749823749823"))
        (should-not (assq 'poke-recall-reference message))
        (should-not (assq 'poke-gateway-recall message))
        (should (equal (alist-get 'image-url
                                  (qq-state-poke-message-data message))
                       "https://example.invalid/poke.png"))))))

(ert-deftest qq-message-private-poke-coexists-with-authored-row-at-shared-sequence ()
  (qq-message-test-with-state
    (let (projection-errors)
      (add-hook
       'qq-message-projection-error-hook
       (lambda (&rest error) (push error projection-errors)))
      (qq-message--handle-event
       "message.received"
       (qq-message-test-event
        :message-id "72057595097851166"
        :sequence "42413"
        :random 1059923230
        :sent-at 1785667319))
      (qq-message--handle-event
       "message.received"
       (qq-message-test-private-poke
        :message-id "144115189780958658"
        :sequence "42413"
        :random 1704086978
        :sent-at 1785667494))
      (should-not projection-errors)
      (let ((messages (qq-state-session-messages "private:10001")))
        (should (= (length messages) 2))
        (should
         (equal (mapcar (lambda (message)
                          (alist-get 'server-id message))
                        messages)
                '("72057595097851166" "144115189780958658")))
        (should (= (seq-count #'qq-state-service-message-p messages) 1)))
      (should
       (equal (qq-message-live-frontier "private:10001")
              '((sequence . "42413")))))))

(ert-deftest qq-message-service-siblings-share-group-sequence-with-distinct-identities ()
  (qq-message-test-with-state
    (let (projection-errors)
      (add-hook
       'qq-message-projection-error-hook
       (lambda (&rest error) (push error projection-errors)))
      (qq-message--handle-event
       "message.received"
       (qq-message-test-poke
        :message-id "7348923749823749823"
        :sequence "70001"
        :target-uin "10001"))
      (qq-message--handle-event
       "message.received"
       (qq-message-test-poke
        :message-id "7348923749823749824"
        :sequence "70001"
        :target-uin "10003"))
      (should-not projection-errors)
      (let ((messages (qq-state-session-messages "group:8209413637")))
        (should (= (length messages) 2))
        (should (seq-every-p #'qq-state-service-message-p messages))
        (should
         (equal (mapcar (lambda (message)
                          (alist-get 'server-id message))
                        messages)
                '("7348923749823749823" "7348923749823749824")))))))

(ert-deftest qq-message-sequence-effects-wait-for-authored-row-not-service-sibling ()
  (qq-message-test-with-state
    (let ((conversation
           '((kind . "group")
             (group_uin . "8209413637")
             (group_name . "Protocol Lab")
             (sender_card . "Alice"))))
      (qq-message--handle-event
       "message.recalled"
       (qq-message-test-recall
        :target '((kind . "sequence") (sequence . "70001"))))
      (qq-message--handle-event
       "message.reaction_changed"
       (qq-message-test-reaction :sequence "70001"))
      (qq-message--handle-event
       "message.received"
       (qq-message-test-poke
        :message-id "7348923749823749824"
        :sequence "70001"))
      (let ((service
             (car (qq-state-session-messages "group:8209413637"))))
        (should (qq-state-service-message-p service))
        (should-not (qq-state-message-recalled-p service))
        (should-not (qq-state-message-reactions service)))
      (should (= (hash-table-count qq-message--pending-recalls) 1))
      (should (= (hash-table-count qq-message--pending-reactions) 1))

      (qq-message--handle-event
       "message.received"
       (qq-message-test-event
        :message-id "7348923749823749823"
        :sequence "70001"
        :conversation conversation))
      (let* ((messages (qq-state-session-messages "group:8209413637"))
             (authored
              (seq-find
               (lambda (message)
                 (not (qq-state-service-message-p message)))
               messages))
             (service
              (seq-find #'qq-state-service-message-p messages)))
        (should (= (length messages) 2))
        (should (qq-state-message-recalled-p authored))
        (should (= (alist-get 'count
                              (car (qq-state-message-reactions authored)))
                   3))
        (should-not (qq-state-message-recalled-p service))
        (should-not (qq-state-message-reactions service)))
      (should (= (hash-table-count qq-message--pending-recalls) 0))
      (should (= (hash-table-count qq-message--pending-reactions) 0)))))

(ert-deftest qq-message-poke-has-only-read-and-dedicated-recall-capabilities ()
  (qq-message-test-with-state
    (qq-message--handle-event
     "message.received" (qq-message-test-poke))
    (let* ((session-key "group:8209413637")
           (message (car (qq-state-session-messages session-key))))
      (should (qq-message-read-capable-p message))
      (should-not (qq-message-reply-target session-key message))
      (should-not (qq-message-recall-target session-key message))
      (should-error (qq-message-set-reaction message "14" t)
                    :type 'user-error)
      (should-error (qq-message-set-essence message t)
                    :type 'user-error)
      (should-error (qq-message-set-todo message 'set)
                    :type 'user-error))))

(ert-deftest qq-message-projects-group-membership-as-typed-gray-tip ()
  (qq-message-test-with-state
    (qq-message--handle-event
     "message.received"
     (qq-message-test-event
      :sender '((uin . "99999"))
      :recipient nil
      :conversation
      '((kind . "group") (group_uin . "8209413637"))
      :sender-presentation nil
      :message-type 732
      :sub-type 20
      :segments
      '(((kind . "gray_tip")
         (payload
          . ((kind . "group_members_joined")
             (members
              . (((kind . "uin") (uin . "9007199254740998"))
                 ((kind . "uin") (uin . "10003"))))
             (cause
              . ((kind . "invited")
                 (inviter
                  . ((kind . "uin") (uin . "9007199254740997")))))
             (attached_message_count . 3)))))))
    (let* ((session-key "group:8209413637")
           (message (car (qq-state-session-messages session-key)))
           (data (qq-state-gray-tip-message-data message))
           (parts (alist-get 'parts data)))
      (should (qq-state-service-message-p message))
      (should (qq-state-gray-tip-message-p message))
      (should-not (qq-state-poke-message-p message))
      (should (equal (alist-get 'sender-name message) "QQ"))
      (should-not (alist-get 'sender-id message))
      (should (equal (alist-get 'kind data) "group_members_joined"))
      (should (equal (alist-get 'members data)
                     '(((kind . "uin") (uin . "9007199254740998"))
                       ((kind . "uin") (uin . "10003")))))
      (should (equal (alist-get 'attached-message-count data) 3))
      (should (equal (mapcar (lambda (part)
                               (and (equal (alist-get 'type part) "user")
                                    (alist-get 'user-id part)))
                             parts)
                     '("9007199254740997" nil
                       "9007199254740998" nil "10003" nil nil)))
      (should (string-match-p "3 条聊天记录"
                              (qq-state-message-preview message)))
      (should-not (qq-message-reply-target session-key message))
      (should-not (qq-message-recall-target session-key message)))))

(ert-deftest qq-message-projects-group-change-and-mute-gray-tips ()
  (qq-message-test-with-state
    (dolist (fixture
             '(("7348923749823749823" "9007199254740999" 33 0
                ((kind . "group_members_joined")
                 (members . (((kind . "uid") (uid . "u_member"))))
                 (cause
                  . ((kind . "qr_code")
                     (shared_by . ((kind . "uid") (uid . "u_sharer")))))))
               ("7348923749823749824" "9007199254741000" 34 0
                ((kind . "group_member_removed")
                 (member . ((kind . "uid") (uid . "u_member")))
                 (operator . ((kind . "uid") (uid . "u_operator")))))
               ("7348923749823749825" "9007199254741001" 34 0
                ((kind . "group_disbanded")))
               ("7348923749823749826" "9007199254741002" 732 12
                ((kind . "group_mute_changed")
                 (target . ((kind . "whole_group")))
                 (operator . ((kind . "uid") (uid . "u_operator")))
                 (duration_seconds . 0)))))
      (pcase-let ((`(,message-id ,sequence ,message-type ,sub-type ,payload)
                   fixture))
        (qq-message--handle-event
         "message.received"
         (qq-message-test-event
          :message-id message-id
          :sender '((uid . "u_system_envelope"))
          :recipient nil
          :conversation
          '((kind . "group") (group_uin . "8209413637"))
          :sender-presentation nil
          :sequence sequence
          :client-sequence "0"
          :random nil
          :message-type message-type
          :sub-type sub-type
          :segments `(((kind . "gray_tip") (payload . ,payload)))))))
    (let ((messages (qq-state-session-messages "group:8209413637")))
      (cl-labels
          ((data (kind)
             (qq-state-gray-tip-message-data
              (seq-find
               (lambda (message)
                 (equal kind
                        (alist-get
                         'kind (qq-state-gray-tip-message-data message))))
               messages))))
        (let* ((joined (data "group_members_joined"))
               (removed (data "group_member_removed"))
               (removed-users
                (seq-filter
                 (lambda (part) (equal (alist-get 'type part) "user"))
                 (alist-get 'parts removed)))
               (disbanded (data "group_disbanded"))
               (mute (data "group_mute_changed")))
          (should (= (length messages) 4))
          (should (equal (alist-get 'text joined)
                         "u_sharer 分享二维码，u_member 加入群聊"))
          (should (equal (alist-get 'text removed)
                         "u_operator 将 u_member 移出群聊"))
          (should (equal (mapcar (lambda (part)
                                  (alist-get 'user-uid part))
                                removed-users)
                         '("u_operator" "u_member")))
          (should-not (seq-some (lambda (part) (alist-get 'user-id part))
                                removed-users))
          (should (equal (alist-get 'text disbanded) "群聊已解散"))
          (should (equal (alist-get 'text mute)
                         "u_operator 解除了全员禁言"))
          (should (equal (alist-get 'duration-seconds mute) 0)))))))

(ert-deftest qq-message-projects-unknown-general-gray-tip-with-safe-summary ()
  (qq-message-test-with-state
    (qq-message--handle-event
     "message.received"
     (qq-message-test-event
      :sender '((uin . "99999"))
      :recipient nil
      :conversation
      '((kind . "group") (group_uin . "8209413637"))
      :sender-presentation nil
      :message-type 732
      :sub-type 20
      :segments
      '(((kind . "gray_tip")
         (payload
          . ((kind . "general")
             (summary . "未知服务消息")
             (business_type . "9007199254740999")
             (business_id . "9007199254740998")
             (template_id . "9007199254740997")))))))
    (let* ((message
            (car (qq-state-session-messages "group:8209413637")))
           (data (qq-state-gray-tip-message-data message)))
      (should (qq-state-service-message-p message))
      (should (equal (alist-get 'kind data) "general"))
      (should (equal (alist-get 'text data) "未知服务消息"))
      (should (equal (alist-get 'business-type data)
                     "9007199254740999"))
      (should (equal (qq-state-message-preview message)
                     "未知服务消息"))
      (should-not (assq 'tips-sequence data)))))

(ert-deftest qq-message-projects-aio-group-name-change-gray-tip ()
  (qq-message-test-with-state
    (qq-message--handle-event
     "message.received"
     (qq-message-test-event
      :sender '((uid . "u_system_envelope"))
      :recipient nil
      :conversation
      '((kind . "group") (group_uin . "8209413637"))
      :sender-presentation nil
      :message-type 732
      :sub-type 20
      :segments
      '(((kind . "gray_tip")
         (payload
          . ((kind . "group_name_changed")
             (name . "新群名")))))))
    (let* ((message
            (car (qq-state-session-messages "group:8209413637")))
           (data (qq-state-gray-tip-message-data message)))
      (should (qq-state-service-message-p message))
      (should (equal (alist-get 'kind data) "group_name_changed"))
      (should (equal (alist-get 'name data) "新群名"))
      (should (equal (qq-state-message-preview message)
                     "群名称已修改为「新群名」"))
      (should (equal (alist-get 'sender-name message) "QQ"))
      (should-not (alist-get 'sender-id message)))))

(ert-deftest qq-message-rejects-gray-tip-mixed-with-authored-content ()
  (qq-message-test-with-state
    (let ((message
           (alist-get
            'message
            (qq-message-test-event
             :sender '((uin . "10001") (uid . "u_peer"))
             :recipient '((uin . "10002") (uid . "u_self"))
             :conversation '((kind . "private"))
             :sender-presentation nil
             :message-type 528
             :sub-type 290
             :segments
             '(((kind . "gray_tip")
                (payload
                 . ((kind . "general")
                    (summary . "private service row")
                    (business_type . "1")
                    (business_id . "2"))))
               ((kind . "text") (payload . ((text . "authored"))))))))))
      (should-error
       (qq-message-normalize-snapshot
        message "slot-a" (qq-account-get "slot-a")))))

(ert-deftest qq-message-reaction-sends-only-the-message-reference ()
  (qq-message-test-with-state
    (qq-message--handle-event
     "message.received"
     (qq-message-test-event
      :conversation
      '((kind . "group") (group_uin . "8209413637")
        (group_name . "Protocol Lab") (sender_card . "Alice"))))
    (let* ((session-key "group:8209413637")
           (message (car (qq-state-session-messages session-key)))
           sent-method sent-params callback-result)
      (dolist (key '(message-seq native-random))
        (setq message (assq-delete-all key message)))
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (setq sent-method method sent-params params)
                   (funcall callback
                            '((account_id . "slot-a")
                              (message_id . "7348923749823749823")
                              (sequence . "9007199254740999")
                              (emoji . ((kind . "unicode") (value . "👍")))
                              (set . t)))
                   "request-reaction")))
        (should
         (equal
          (qq-message-set-reaction
           message '((emoji-id . "128077") (emoji-type . "2")) t
           (lambda (result) (setq callback-result result)))
          "request-reaction"))
        (should (equal sent-method "message.set_reaction"))
        (should
         (equal
          sent-params
          '((account_id . "slot-a")
            (conversation . ((kind . "group")
                             (group_uin . "8209413637")))
            (message . ((message_id . "7348923749823749823")))
            (emoji . ((kind . "unicode") (value . "👍")))
            (set . t))))
        (should (equal (alist-get 'emoji callback-result)
                       '((kind . "unicode") (value . "👍"))))
        (should-not
         (qq-state-message-reactions
          (car (qq-state-session-messages session-key))))
        (qq-message--handle-event
         "message.reaction_changed"
         (qq-message-test-reaction
          :operator-uin "10002"
          :emoji-id "128077"
          :emoji-type "2"
          :count 1))
        (let ((reaction
               (car (qq-state-message-reactions
                     (car (qq-state-session-messages session-key))))))
          (should (equal (alist-get 'emoji-id reaction) "128077"))
          (should (equal (alist-get 'emoji-type reaction) "2"))
          (should (= (alist-get 'count reaction) 1))
          (should (alist-get 'chosen-p reaction)))))))

(ert-deftest qq-message-essence-sends-only-the-message-reference ()
  (qq-message-test-with-state
    (qq-message--handle-event
     "message.received"
     (qq-message-test-event
      :random 4277998232
      :conversation
      '((kind . "group") (group_uin . "8209413637")
        (group_name . "Protocol Lab") (sender_card . "Alice"))))
    (let* ((session-key "group:8209413637")
           (message (car (qq-state-session-messages session-key)))
           sent-method sent-params callback-result)
      (dolist (key '(message-seq native-random))
        (setq message (assq-delete-all key message)))
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (setq sent-method method sent-params params)
                   (funcall callback
                            '((account_id . "slot-a")
                              (message_id . "7348923749823749823")
                              (sequence . "9007199254740999")
                              (random . 4277998232)
                              (set . t)))
                   "request-essence")))
        (should
         (equal
          (qq-message-set-essence
           message t (lambda (result) (setq callback-result result)))
          "request-essence"))
        (should (equal sent-method "message.set_essence"))
        (should
         (equal
          sent-params
          '((account_id . "slot-a")
            (conversation . ((kind . "group")
                             (group_uin . "8209413637")))
            (message . ((message_id . "7348923749823749823")))
            (set . t))))
        (should (equal (alist-get 'message_id callback-result)
                       "7348923749823749823"))
        (should-not
         (alist-get
          'essence-p
          (car (qq-state-session-messages session-key))))
        (qq-message--handle-event
         "message.essence_changed"
         (qq-message-test-essence :random 4277998232))
        (should
         (eq (alist-get
              'essence-p
              (car (qq-state-session-messages session-key)))
             t))))))

(ert-deftest qq-message-authoritative-essence-beats-racing-receipt ()
  (qq-message-test-with-state
    (qq-message--handle-event
     "message.received"
     (qq-message-test-event
      :conversation
      '((kind . "group") (group_uin . "8209413637")
        (group_name . "Protocol Lab") (sender_card . "Alice"))))
    (let* ((session-key "group:8209413637")
           (message (car (qq-state-session-messages session-key)))
           response-callback)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (setq response-callback callback)
                   "request-essence")))
        (qq-message-set-essence message t)
        (qq-message--handle-event
         "message.essence_changed"
         (qq-message-test-essence :is-set :false))
        (funcall response-callback
                 '((account_id . "slot-a")
                   (message_id . "7348923749823749823")
                   (sequence . "9007199254740999")
                   (random . 7)
                   (set . t)))
        (let ((projected (car (qq-state-session-messages session-key))))
          (should-not (alist-get 'essence-p projected))
          (should (= (alist-get 'essence-changed-at projected)
                     1784700000)))))))

(ert-deftest qq-message-todo-keeps-exact-target-and-operation ()
  (qq-message-test-with-state
    (qq-message--handle-event
     "message.received"
     (qq-message-test-event
      :conversation
      '((kind . "group") (group_uin . "8209413637")
        (group_name . "Protocol Lab") (sender_card . "Alice"))))
    (let* ((message
            (car (qq-state-session-messages "group:8209413637")))
           calls receipts)
      (dolist (key '(message-seq native-random))
        (setq message (assq-delete-all key message)))
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (push (list method (copy-tree params)) calls)
                   (funcall
                   callback
                    `((account_id . "slot-a")
                      (group_uin . "8209413637")
                      (message_id . "7348923749823749823")
                      (sequence . "9007199254740999")
                      (operation . ,(alist-get 'operation params))))
                   (format "todo-%s" (alist-get 'operation params)))))
        (dolist (operation '(set complete cancel))
          (should
           (equal
            (qq-message-set-todo
             message operation
             (lambda (receipt) (push receipt receipts)))
            (format "todo-%s" operation)))))
      (should
       (equal
        (nreverse calls)
        '(("message.set_todo"
           ((account_id . "slot-a")
            (conversation . ((kind . "group")
                             (group_uin . "8209413637")))
            (message . ((message_id . "7348923749823749823")))
            (operation . "set")))
          ("message.set_todo"
           ((account_id . "slot-a")
            (conversation . ((kind . "group")
                             (group_uin . "8209413637")))
            (message . ((message_id . "7348923749823749823")))
            (operation . "complete")))
          ("message.set_todo"
           ((account_id . "slot-a")
            (conversation . ((kind . "group")
                             (group_uin . "8209413637")))
            (message . ((message_id . "7348923749823749823")))
            (operation . "cancel"))))))
      (should
       (equal (mapcar (lambda (receipt) (alist-get 'operation receipt))
                      (nreverse receipts))
              '("set" "complete" "cancel"))))))

(ert-deftest qq-message-recall-poke-sends-only-public-message-reference ()
  (qq-message-test-with-state
    (qq-message--handle-event
     "message.received" (qq-message-test-poke))
    (let* ((session-key "group:8209413637")
           (message (car (qq-state-session-messages session-key)))
           sent-method sent-params)
      (cl-letf (((symbol-function 'float-time)
                 (lambda (&optional _time) 1784700001))
                ((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (setq sent-method method sent-params params)
                   (funcall callback
                            '((account_id . "slot-a")
                              (message_id . "7348923749823749823")
                              (sequence . "9007199254740999")))
                   "request-poke-recall")))
        (should (equal (qq-message-recall-poke message)
                       "request-poke-recall"))
        (should (equal sent-method "message.recall_poke"))
        (should
         (equal
          sent-params
          '((account_id . "slot-a")
            (conversation . ((kind . "group")
                             (group_uin . "8209413637")))
            (message . ((message_id . "7348923749823749823"))))))
        (should
         (qq-state-message-recalled-p
          (car (qq-state-session-messages session-key))))))))

(ert-deftest qq-message-self-event-before-receipt-still-rekeys ()
  (qq-message-test-with-state
    (let ((now (floor (float-time))) response-callback local-id)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (setq response-callback callback)
                   "request-send")))
        (qq-message-send
         "private:10001"
         '(((type . "text") (data . ((text . "hello"))))))
        (setq local-id
              (alist-get 'local-id
                         (car (qq-state-session-messages "private:10001"))))
        (qq-message--handle-event
         "message.received"
         (qq-message-test-event
          :sent-at now
          :sender '((uin . "10002") (uid . "u_self"))
          :recipient '((uin . "10001") (uid . "u_peer"))
          :sequence "8765432109"
          :client-sequence "42001"
          :random 123))
        (funcall response-callback
                 `((account_id . "slot-a")
                   (sent_at . ,now)
                   (server_sequence . "8765432109")
                   (client_sequence . "42001")
                   (random . 123)))
        (let* ((messages (qq-state-session-messages "private:10001"))
               (message (car messages)))
          (should (= (length messages) 1))
          (should (equal (alist-get 'local-id message) local-id))
          (should (alist-get 'server-id message))
          (should (= (hash-table-count qq-message--pending-sends) 0)))))))

(ert-deftest qq-message-send-failure-marks-only-pending-row ()
  (qq-message-test-with-state
    (let (failure)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params _callback errback &optional _early)
                   (funcall errback
                            '((code . "send_failed") (message . "boom"))
                            "boom")
                   nil)))
        (qq-message-send
         "private:10001"
         '(((type . "text") (data . ((text . "hello")))))
         nil nil
         (lambda (_body reason) (setq failure reason)))
        (let ((message
               (car (qq-state-session-messages "private:10001"))))
          (should (equal failure "boom"))
          (should (eq (alist-get 'status message) 'failed))
          (should (equal (alist-get 'error message) "boom")))))))

(ert-deftest qq-message-local-deletion-is-row-scoped-and-event-driven ()
  (qq-message-test-with-state
    (qq-message--handle-event
     "message.received"
     (qq-message-test-event :row-key "42"))
    (let* ((session-key "private:10001")
           (message (car (qq-state-session-messages session-key)))
           method params response-callback)
      (cl-letf (((symbol-function 'qq-server-ready-p) (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (sent-method sent-params callback _errback
                                      &optional _early)
                   (setq method sent-method
                         params sent-params
                         response-callback callback)
                   "local-delete-request")))
        (should (equal (qq-message-delete-local message)
                       "local-delete-request"))
        (should (equal method "message.delete_local"))
        (should
         (equal params
                '((account_id . "slot-a")
                  (conversation . ((kind . "private")
                                   (peer_uin . "10001")))
                  (row_key . "42"))))
        (funcall
         response-callback
         '((account_id . "slot-a")
           (conversation . ((kind . "private") (peer_uin . "10001")))
           (row_key . "42")
           (newly_deleted . t)
           (conversation_head . ((state . "cleared")))))
        ;; The message-mutation fence delivers the durable event before the
        ;; response; the response validator does not duplicate projection.
        (should (= (length (qq-state-session-messages session-key)) 1))
        (qq-message--handle-event
         "message.locally_deleted"
         '((account_id . "slot-a")
           (conversation . ((kind . "private") (peer_uin . "10001")))
           (row_key . "42")
           (newly_deleted . t)
           (conversation_head . ((state . "cleared")))))
        (should-not (qq-state-session-messages session-key))
        (should-not
         (alist-get 'last-message-id (qq-state-session session-key)))))))

(ert-deftest qq-message-local-deletion-accepts-same-group-replacement-dto ()
  (qq-message-test-with-state
    (let* ((conversation
            '((kind . "group") (group_uin . "8209413637")))
           (replacement-event
            (qq-message-test-event
             :conversation
             '((kind . "group")
               (group_uin . "8209413637")
               (group_name . "Protocol Lab"))))
           (deletion
            `((account_id . "slot-a")
              (conversation . ,conversation)
              (row_key . "42")
              (newly_deleted . t)
              (conversation_head
               . ((state . "replaced")
                  (head . ,(qq-message-test-native-row
                            replacement-event "41")))))))
      (should
       (equal
        (plist-get
         (qq-message--check-local-deletion
          deletion "slot-a" "group replacement" conversation "42")
         :session-key)
        "group:8209413637"))
      (setf (alist-get 'group_uin
                       (alist-get 'conversation
                                  (alist-get 'message
                                             (alist-get 'head
                                                        (alist-get 'conversation_head
                                                                   deletion)))))
            "8209413638")
      (should-error
       (qq-message--check-local-deletion
        deletion "slot-a" "group replacement" conversation "42")
       :type 'error))))

(ert-deftest qq-message-local-deletion-derives-private-replacement-from-endpoints ()
  (qq-message-test-with-state
    (let* ((conversation '((kind . "private") (peer_uin . "10001")))
           (replacement-event (qq-message-test-event))
           (deletion
            `((account_id . "slot-a")
              (conversation . ,conversation)
              (row_key . "42")
              (newly_deleted . t)
              (conversation_head
               . ((state . "replaced")
                  (head . ,(qq-message-test-native-row
                            replacement-event "41")))))))
      (should
       (equal
        (plist-get
         (qq-message--check-local-deletion
          deletion "slot-a" "private replacement" conversation "42")
         :session-key)
        "private:10001")))))

(ert-deftest qq-message-ordinary-recall-aptitude-is-exact-reference-only ()
  (let ((message '((server-id . "7348923749823749823")
                   (session-key . "group:8209413637")
                   (gateway-account-id . "slot-a")
                   (self-p)
                   (time . 1)
                   (status . received)
                   (segments . (((type . "text")))))))
    (cl-letf (((symbol-function 'qq-runtime-current-account-id)
               (lambda () "slot-a")))
      (should (qq-message-recall-capable-p message))
      (should
       (qq-message-recall-capable-p (assq-delete-all 'time message)))
      (should-not
       (qq-message-recall-capable-p
        (assq-delete-all 'server-id message))))
    (cl-letf (((symbol-function 'qq-runtime-current-account-id)
               (lambda () "slot-b")))
      (should-not (qq-message-recall-capable-p message)))))

(ert-deftest qq-message-group-recall-projects-typed-acknowledgement ()
  (qq-message-test-with-state
    (qq-message--handle-event
     "message.received"
     (qq-message-test-event
      :sender '((uin . "10002") (uid . "u_self"))
      :recipient '((uin . "10001") (uid . "u_peer"))
      :conversation
      '((kind . "group")
        (group_uin . "8209413637")
        (group_name . "Protocol Lab")
        (sender_card . "Alice"))))
    (let* ((session-key "group:8209413637")
           (message (car (qq-state-session-messages session-key)))
           sent-params)
      (setq message (assq-delete-all 'message-seq message))
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (_method params callback _errback &optional _early)
                   (setq sent-params params)
                   (funcall callback
                            '((account_id . "slot-a")
                              (message_id . "7348923749823749823")))
                   "request-recall")))
        (should (equal (qq-message-recall session-key message)
                       "request-recall"))
        (should
         (equal
          sent-params
          '((account_id . "slot-a")
            (conversation . ((kind . "group")
                             (group_uin . "8209413637")))
            (message_id . "7348923749823749823"))))
        (should
         (qq-state-message-recalled-p
          (car (qq-state-session-messages session-key))))
        (qq-message--handle-event
         "message.recalled" (qq-message-test-recall))
        (should
         (qq-state-message-recalled-p
          (car (qq-state-session-messages session-key))))))))

(ert-deftest qq-message-private-recall-sends-only-the-message-reference ()
  (qq-message-test-with-state
    (qq-message--handle-event
     "message.received"
     (qq-message-test-event
      :sender '((uin . "10002") (uid . "u_self"))
      :recipient '((uin . "10001") (uid . "u_peer"))))
    (let* ((session-key "private:10001")
           (message (car (qq-state-session-messages session-key)))
           sent-params)
      (dolist (key '(message-seq native-client-sequence native-random))
        (setq message (assq-delete-all key message)))
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (_method params callback _errback &optional _early)
                   (setq sent-params params)
                   (funcall callback
                            '((account_id . "slot-a")
                              (message_id . "7348923749823749823")))
                   "request-recall")))
        (qq-message-recall session-key message)
        (should
         (equal (alist-get 'message_id sent-params)
                "7348923749823749823"))
        (should
         (equal (alist-get 'conversation sent-params)
                '((kind . "private") (peer_uin . "10001"))))
        (should
         (qq-state-message-recalled-p
          (car (qq-state-session-messages session-key))))))))

(ert-deftest qq-message-group-recall-without-message-id-remains-unavailable ()
  (qq-message-test-with-state
    (let* ((session-key "group:8209413637")
           (wire-message
            (alist-get
             'message
             (qq-message-test-event
              :message-id nil
              :sequence "105544"
              :conversation
              '((kind . "group")
                (group_uin . "8209413637")
                (group_name . "Protocol Lab")
                (sender_card . "Alice")))))
           (message
            (qq-message-normalize-snapshot
             wire-message "slot-a" (qq-account-get "slot-a") nil t))
           called)
      (qq-message--merge-normalized message 'history)
      (cl-letf (((symbol-function 'qq-server-ready-p) (lambda () t))
                ((symbol-function 'qq-server-send)
                 (lambda (&rest _args) (setq called t))))
        (should-error (qq-message-recall session-key message)
                      :type 'user-error)
        (should-not called)
        (should-not (qq-message-recall-target session-key message))))))

(ert-deftest qq-message-group-reply-uses-sequence-without-a-snowflake ()
  (qq-message-test-with-state
    (let* ((session-key "group:8209413637")
           (wire-message
            (alist-get
             'message
             (qq-message-test-event
              :message-id nil
              :sequence "105544"
              :conversation
              '((kind . "group")
                (group_uin . "8209413637")
                (group_name . "Protocol Lab")
                (sender_card . "Alice")))))
           (message
            (qq-message-normalize-snapshot
             wire-message "slot-a" (qq-account-get "slot-a") nil t))
           (target (qq-message-reply-target session-key message))
           (outbound
            (qq-message--prepare-outbound
             session-key
             (list
              `((type . "reply") (data . ((target . ,target))))
              '((type . "text") (data . ((text . "ack")))))
             "slot-a")))
      (should
       (equal target
              '((kind . "sequence") (sequence . "105544"))))
      (should (equal (plist-get outbound :reply-to) target))
      (should
       (equal
        (plist-get outbound :segments)
        '(((kind . "text") (payload . ((text . "ack"))))))))))

(ert-deftest qq-message-history-range-stays-exact-decimal ()
  (should
   (equal
    (qq-message--decimal-add-small "18446744073709551516" 99)
    "18446744073709551615"))
  (should
   (equal
    (qq-message--validate-history-range
     "18446744073709551516" "18446744073709551615")
    '("18446744073709551516" . "18446744073709551615")))
  (should (equal (qq-message--validate-history-range "0" "99")
                 '("0" . "99")))
  (dolist (range '((0 "1") ("00" "1") ("2" "1")
                   ("0" "100")
                   ("18446744073709551616" "18446744073709551616")))
    (should-error
     (qq-message--validate-history-range (car range) (cadr range))
     :type 'user-error)))

(ert-deftest qq-message-live-frontier-uses-events-not-history ()
  (qq-message-test-with-state
    (qq-message--handle-event
     "message.received"
     (qq-message-test-event
      :message-id "7348923749823749823" :sequence "100"))
    (should
     (equal (qq-message-live-frontier "private:10001")
            '((sequence . "100"))))
    (qq-message--handle-event
     "message.received"
     (qq-message-test-event
      :message-id "7348923749823749822" :sequence "99"
      :client-sequence "9007199254741000"))
    (should
     (equal (alist-get 'sequence
                       (qq-message-live-frontier "private:10001"))
            "100"))
    (qq-message-reset-correlations)
    (should-not (qq-message-live-frontier "private:10001"))))

(ert-deftest qq-message-revoke-clears-essence-correlation ()
  (qq-message-test-with-state
    (puthash '(owner target) t qq-message--pending-essences)
    (qq-message-reset-correlations)
    (should (= (hash-table-count qq-message--pending-essences) 0))))

(ert-deftest qq-message-history-request-preserves-large-sequences ()
  (qq-message-test-with-state
    (let (sent-method sent-params callback-meta)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (setq sent-method method sent-params params)
                   (funcall
                    callback
                    (qq-message-test-history-result
                     nil "18446744073709551516" "18446744073709551615"))
                   "request-history")))
        (should
         (equal
          (qq-message--request-native-history-range
           "group:8209413637"
           "18446744073709551516" "18446744073709551615"
           (lambda (meta) (setq callback-meta meta)))
          "request-history"))
        (should (equal sent-method "message.get_history"))
        (should
         (equal
          sent-params
          '((account_id . "slot-a")
            (conversation . ((kind . "group")
                             (group_uin . "8209413637")))
            (start_sequence . "18446744073709551516")
            (end_sequence . "18446744073709551615"))))
        (should (equal (plist-get callback-meta :message-count) 0))
        (should
         (equal (plist-get callback-meta :requested-start-sequence)
                "18446744073709551516"))))))

(ert-deftest qq-message-forward-request-keeps-resource-scene-and-transient-body ()
  (qq-message-test-with-state
    (let (sent-method sent-params projected)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (setq sent-method method
                         sent-params params)
                   (funcall
                    callback
                    `((account_id . "slot-a")
                      (unsupported_message_count . 2)
                      (messages
                       . ,(list
                           '((entry_id . "1")
                             (state . "live")
                             (sent_at . 1710000000)
                             (sender . ((kind . "anonymous")
                                        (name . "Alice")))
                             (origin . ((kind . "unknown")))
                             (segments . (((kind . "text")
                                           (payload
                                            . ((text . "inside")))))))))))
                   "request-forward")))
        (should
         (equal
          (qq-message-get-forward
           "resid-private" "private"
           (lambda (page) (setq projected page)))
          "request-forward"))
        (should (equal sent-method "message.get_forward"))
        (should
         (equal sent-params
                '((account_id . "slot-a")
                  (resource_id . "resid-private")
                  (scene . "private"))))
        (should (= (plist-get projected :unsupported-message-count) 2))
        (should
         (equal
          (alist-get 'entry_id (car (plist-get projected :messages)))
          "1"))
        ;; The result is viewer-local data, not a timeline/history merge.
        (should-not (qq-state-session-messages "private:10001"))))))

(ert-deftest qq-message-history-merges-once-and-deduplicates-live-row ()
  (qq-message-test-with-state
    (let* ((conversation
            '((kind . "group")
              (group_uin . "8209413637")
              (group_name . "Protocol Lab")
              (sender_card . "Alice")))
           (newer-event
            (qq-message-test-event
             :message-id "7348923749823749824"
             :sequence "101" :conversation conversation))
           (older
            (alist-get
             'message
             (qq-message-test-event
              :message-id "7348923749823749823"
              :sequence "100" :sent-at 1784699999
              :conversation conversation)))
           (newer (alist-get 'message newer-event))
           history-events callback-meta)
      (qq-message--handle-event "message.received" newer-event)
      (add-hook 'qq-state-change-hook
                (lambda (event)
                  (when (eq (plist-get event :type) 'history)
                    (push event history-events))))
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (funcall
                    callback
                    (qq-message-test-history-result
                     (list older newer) "100" "101"
                     :unsupported-count 2))
                   "request-history")))
        (qq-message--request-native-history-range
         "group:8209413637" "100" "101"
         (lambda (meta) (setq callback-meta meta)))
        (should (= (length (qq-state-session-messages
                            "group:8209413637"))
                   2))
        (should (= (plist-get callback-meta :message-count) 2))
        (should (= (plist-get callback-meta :added-count) 1))
        (should (= (plist-get callback-meta :unsupported-message-count) 2))
        (should (= (length history-events) 1))
        (should
         (equal (plist-get (car history-events) :batch-message-ids)
                '("7348923749823749823" "7348923749823749824")))))))

(ert-deftest qq-message-group-history-keeps-sequence-only-row-and-promotes-live-id ()
  (qq-message-test-with-state
    (let* ((conversation
            '((kind . "group")
              (group_uin . "8209413637")
              (group_name . "Protocol Lab")
              (sender_card . "Alice")))
           (history-message
            (alist-get
             'message
             (qq-message-test-event
              :message-id nil
              :sequence "105525"
              :random nil
              :conversation conversation)))
           (result
            (qq-message-test-history-result
             (list history-message) "105525" "105525"))
           (meta
            (qq-message--merge-history
             "group:8209413637"
             (qq-server-wire-domain-copy result)
             "slot-a"
             '(:group-history-window-p t)))
           (history-anchor
            (car (plist-get meta :batch-message-ids))))
      (should (= (plist-get meta :added-count) 1))
      (should (string-prefix-p "history:slot-a:group:8209413637:105525:"
                               history-anchor))
      (let ((messages (qq-state-session-messages "group:8209413637")))
        (should (= (length messages) 1))
        (should-not (alist-get 'server-id (car messages)))
        (should (equal (qq-state-message-anchor (car messages))
                       history-anchor)))

      ;; The live push may add random as well as the authoritative snowflake.
      ;; Group sequence is the native locator, so this promotes rather than
      ;; duplicating the already rendered history observation.
      (qq-message--handle-event
       "message.received"
       (qq-message-test-event
        :message-id "2083983882109904220"
        :sequence "105525"
        :random 995353755
        :conversation conversation))
      (let ((messages (qq-state-session-messages "group:8209413637")))
        (should (= (length messages) 1))
        (should
         (equal (alist-get 'server-id (car messages))
                "2083983882109904220"))
        (should
         (equal (qq-state-message-anchor (car messages))
                "2083983882109904220"))))))

(ert-deftest qq-message-history-applies-earlier-sequence-recall ()
  (qq-message-test-with-state
    (qq-message--handle-event
     "message.recalled"
     (qq-message-test-recall
      :target '((kind . "sequence") (sequence . "100"))))
    (let* ((message
            (alist-get
             'message
             (qq-message-test-event
              :message-id nil
              :sequence "100"
              :conversation
              '((kind . "group")
                (group_uin . "8209413637")
                (group_name . "Protocol Lab")
                (sender_card . "Alice")))))
           (result (qq-message-test-history-result
                    (list message) "100" "100")))
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (funcall callback result)
                   "request-history")))
        (qq-message--request-native-history-range
         "group:8209413637" "100" "100")
        (should
         (qq-state-message-recalled-p
          (car (qq-state-session-messages "group:8209413637"))))
        (should (= (hash-table-count qq-message--pending-recalls) 0))))))

(ert-deftest qq-message-history-applies-earlier-sequence-reaction ()
  (qq-message-test-with-state
    (qq-message--handle-event
     "message.reaction_changed"
     (qq-message-test-reaction
      :sequence "100" :operator-uin nil :count 6))
    (let* ((message
            (alist-get
             'message
             (qq-message-test-event
              :message-id nil
              :sequence "100"
              :conversation
              '((kind . "group")
                (group_uin . "8209413637")
                (group_name . "Protocol Lab")
                (sender_card . "Alice")))))
           (result (qq-message-test-history-result
                    (list message) "100" "100")))
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (funcall callback result)
                   "request-history")))
        (qq-message--request-native-history-range
         "group:8209413637" "100" "100")
        (let* ((projected
                (car (qq-state-session-messages "group:8209413637")))
               (reaction (car (qq-state-message-reactions projected))))
          (should (= (alist-get 'count reaction) 6)))
        (should (= (hash-table-count
                    qq-message--pending-reactions)
                   0))))))

(ert-deftest qq-message-history-applies-earlier-native-essence ()
  (qq-message-test-with-state
    (qq-message--handle-event
     "message.essence_changed"
     (qq-message-test-essence
      :sequence "100" :random 4294967295 :sender-nickname nil))
    (let* ((message
            (alist-get
             'message
             (qq-message-test-event
              :sequence "100" :random 4294967295
              :conversation
              '((kind . "group")
                (group_uin . "8209413637")
                (group_name . "Protocol Lab")
                (sender_card . "Alice")))))
           (result (qq-message-test-history-result
                    (list message) "100" "100")))
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (funcall callback result)
                   "request-history")))
        (qq-message--request-native-history-range
         "group:8209413637" "100" "100")
        (let ((projected
               (car (qq-state-session-messages "group:8209413637"))))
          (should (eq (alist-get 'essence-p projected) t))
          (should (= (alist-get 'native-random projected) 4294967295)))
        (should (= (hash-table-count
                    qq-message--pending-essences)
                   0))))))

(ert-deftest qq-message-history-malformed-page-is-not-partially-merged ()
  (qq-message-test-with-state
    (let* ((conversation
            '((kind . "group")
              (group_uin . "8209413637")
              (group_name . "Protocol Lab")
              (sender_card . "Alice")))
           (valid
            (alist-get
             'message
             (qq-message-test-event
              :message-id "7348923749823749823"
              :sequence "100" :conversation conversation)))
           (invalid
            (alist-get
             'message
             (qq-message-test-event
              :message-id 7348923749823749824
              :sequence "101" :conversation conversation)))
           failure)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (funcall
                    callback
                    (qq-message-test-history-result
                     (list valid invalid) "100" "101"))
                   "request-history")))
        (qq-message--request-native-history-range
         "group:8209413637" "100" "101" nil
         (lambda (_body reason) (setq failure reason)))
        (should (string-match-p "message_id" failure))
        (should-not (qq-state-sessions))))))

(ert-deftest qq-message-history-rejects-cross-conversation-page ()
  (qq-message-test-with-state
    (let* ((group-message
            (alist-get
             'message
             (qq-message-test-event
              :sequence "100"
              :conversation
              '((kind . "group")
                (group_uin . "8209413637")
                (group_name . "Protocol Lab")
                (sender_card . "Alice")))))
           (initial-order qq-state--message-order-counter)
           failure)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (funcall
                    callback
                    (qq-message-test-history-result
                     (list group-message) "100" "100"))
                   "request-history")))
        (qq-message--request-native-history-range
         "private:10001" "100" "100" nil
         (lambda (_body reason) (setq failure reason)))
        (should (string-match-p "requested conversation" failure))
        (should-not (qq-state-sessions))
        (should (= qq-state--message-order-counter initial-order))))))

(ert-deftest qq-message-history-survives-ui-account-selection ()
  (qq-message-test-with-state
    (let (response-callback failure delivered)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (setq response-callback callback)
                   "request-history")))
        (qq-message--request-native-history-range
         "group:8209413637" "100" "100"
         (lambda (_metadata) (setq delivered t))
         (lambda (_body reason) (setq failure reason)))
        (qq-account--upsert-account
         (qq-message-test-account
          "slot-b" "10003" "u_other_self")
         'changed)
        (qq-account-select "slot-b")
        (funcall
         response-callback
         (qq-message-test-history-result nil "100" "100"))
        (should delivered)
        (should-not failure)
        (should-not (qq-state-sessions))))))

(ert-deftest qq-message-history-recovers-lost-self-event ()
  (qq-message-test-with-state
    (let ((now (floor (float-time))) local-id)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method _params callback _errback &optional _early)
                   (pcase method
                     ("message.send"
                      (funcall callback
                               `((account_id . "slot-a")
                                 (sent_at . ,now)
                                 (server_sequence . "8765432109")
                                 (client_sequence . "42001")
                                 (random . 123))))
                     ("message.get_history"
                      (let ((message
                             (alist-get
                              'message
                              (qq-message-test-event
                               :sent-at now
                               :sender '((uin . "10002") (uid . "u_self"))
                               :recipient '((uin . "10001") (uid . "u_peer"))
                               :sequence "8765432109"
                               :client-sequence "42001"
                               :random 123))))
                        (funcall
                         callback
                         (qq-message-test-history-result
                          (list message) "8765432109" "8765432109")))))
                   (concat "request-" method))))
        (qq-message-send
         "private:10001"
         '(((type . "text") (data . ((text . "hello"))))))
        (setq local-id
              (alist-get 'local-id
                         (car (qq-state-session-messages "private:10001"))))
        (qq-message--request-native-history-range
         "private:10001" "8765432109" "8765432109")
        (let* ((messages (qq-state-session-messages "private:10001"))
               (message (car messages)))
          (should (= (length messages) 1))
          (should (equal (alist-get 'local-id message) local-id))
          (should (equal (alist-get 'server-id message)
                         "7348923749823749823"))
          (should (= (hash-table-count qq-message--pending-sends) 0)))))))

(ert-deftest qq-message-selection-change-preserves-account-partitions ()
  (qq-message-test-with-state
    (qq-message--handle-event
    "message.received" (qq-message-test-event))
    (should (qq-state-sessions))
    (qq-account--upsert-account
     (qq-message-test-account
     "slot-b" "10003" "u_other_self")
     'changed)
    (qq-message--handle-account-change 'changed "slot-b")
    (qq-account-select "slot-b")
    (should (qq-state-sessions))
    (qq-state-with-account "slot-b"
      (should-not (qq-state-sessions)))
    (qq-message--handle-event
     "message.received"
     (qq-message-test-event
      :account-id "slot-b"
      :recipient '((uin . "10003") (uid . "u_other_self"))
      :message-id "7348923749823749824"))
    (qq-state-with-account "slot-a"
      (should (= (length (qq-state-sessions)) 1)))
    (qq-state-with-account "slot-b"
      (should (= (length (qq-state-sessions)) 1)))))

(ert-deftest qq-message-same-slot-restart-preserves-state-and-pending ()
  (qq-message-test-with-state
    (let* ((session-key "group:8209413637")
           (conversation
            '((kind . "group")
              (group_uin . "8209413637")
              (group_name . "Protocol Lab")
              (sender_card . "Alice"))))
      (qq-message--handle-event
       "message.received"
       (qq-message-test-event
        :sender '((uin . "10002") (uid . "u_self"))
        :recipient '((uin . "10001") (uid . "u_peer"))
        :conversation conversation))
      (let* ((old-message (car (qq-state-session-messages session-key)))
             (pending
              (qq-state-insert-pending-message
               session-key
               '(((type . "text") (data . ((text . "in flight")))))))
             (local-id (alist-get 'local-id pending))
             sent-params delivered)
        (puthash '(old-runtime) t qq-message--pending-recalls)
        (puthash (qq-message--frontier-key "slot-a" session-key)
                 '((sequence . "9007199254740999"))
                 qq-message--live-frontiers)
        (let ((stopped (qq-message-test-account)))
          (setf (alist-get 'phase stopped) "stopped")
          (qq-account--upsert-account stopped 'changed))
        (qq-message--handle-account-change 'changed "slot-a")
        (qq-account--upsert-account
         (qq-message-test-account) 'changed)
        (qq-message--handle-account-change 'changed "slot-a")
        (should (= (hash-table-count qq-message--pending-recalls) 1))
        (should (= (hash-table-count qq-message--live-frontiers) 1))
        (let ((retained
               (seq-find
                (lambda (message)
                  (equal (alist-get 'server-id message)
                         "7348923749823749823"))
                (qq-state-session-messages session-key)))
              (in-flight
               (seq-find
                (lambda (message)
                  (equal (alist-get 'local-id message) local-id))
                (qq-state-session-messages session-key))))
          (should retained)
          (should (eq (alist-get 'status in-flight) 'pending))
          (should-not (alist-get 'error in-flight)))
        (cl-letf (((symbol-function 'float-time)
                   (lambda (&optional _) (+ (alist-get 'time old-message) 100)))
                  ((symbol-function 'qq-server-ready-p)
                   (lambda () t))
                  ((symbol-function 'qq-server-capabilities)
                   (lambda () qq-message-test-capabilities))
                  ((symbol-function 'qq-server-send)
                   (lambda (_method params callback _errback &optional _early)
                     (setq sent-params params)
                     (funcall
                      callback
                      '((account_id . "slot-a")
                        (message_id . "7348923749823749823")))
                     "restart-recall")))
          (should
           (equal
            (qq-message-recall
             session-key old-message
             (lambda (receipt) (setq delivered receipt)))
            "restart-recall")))
        (should delivered)
        (should (equal (alist-get 'account_id sent-params) "slot-a"))
        (should
         (equal (alist-get 'message_id sent-params)
                "7348923749823749823"))
        (should-not (assq 'generation sent-params))))))

(ert-deftest qq-message-account-ready-synchronizes-every-owner ()
  (qq-message-test-with-state
    (qq-message--handle-account-change 'ready nil)
    (should (equal (alist-get 'user_id (qq-state-self-info)) "10002"))
    (should (eq (qq-state-connection-status) 'ready))))

(provide 'qq-message-test)

;;; qq-message-test.el ends here
