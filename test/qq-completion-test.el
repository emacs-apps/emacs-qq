;;; qq-completion-test.el --- Tests for QQ composer completion -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'qq-chat)
(require 'qq-completion)
(require 'qq-operation-test)

(defmacro qq-completion-test-with-group (&rest body)
  "Evaluate BODY in a temporary writable QQ group composer."
  (declare (indent 0) (debug t))
  `(let ((qq-runtime--accounts (make-hash-table :test #'equal))
         (qq-state--stores (make-hash-table :test #'equal))
         (qq-state--current (qq-state-store-create)))
     (unwind-protect
         (qq-runtime-with-account "slot-a"
                                  (qq-state-reset)
                                  (qq-state-upsert-session
                                   "group:20001"
                                   '((type . group) (target-id . "20001") (title . "Group")) nil)
                                  (with-temp-buffer
                                    (qq-chat-mode)
                                    (qq-runtime-bind-account "slot-a")
                                    (setq-local qq-chat--session-key "group:20001")
                                    (qq-chat--ensure-view)
                                    (qq-chat-render)
                                    (appkit-chatbuf-focus-input)
                                    (should (appkit-chatbuf-point-in-input-p))
                                    ,@body))
       (qq-runtime-stop-account "slot-a" t)
       (qq-state-reset))))

(defmacro qq-completion-test-with-private (&rest body)
  "Evaluate BODY in a temporary private chat with peer and self identity."
  (declare (indent 0) (debug t))
  `(let ((qq-runtime--accounts (make-hash-table :test #'equal))
         (qq-state--stores (make-hash-table :test #'equal))
         (qq-state--current (qq-state-store-create)))
     (unwind-protect
         (qq-runtime-with-account "slot-a"
                                  (qq-state-reset)
                                  (qq-state-set-self-info
                                   '((user_id . "90001") (nickname . "Myself")))
                                  (qq-state-upsert-session
                                   "private:10001"
                                   '((type . private)
                                     (target-id . "10001")
                                     (peer-uin . "10001")
                                     (title . "Alice"))
                                   nil)
                                  (with-temp-buffer
                                    (qq-chat-mode)
                                    (qq-runtime-bind-account "slot-a")
                                    (setq-local qq-chat--session-key "private:10001")
                                    (qq-chat--ensure-view)
                                    ,@body))
       (qq-runtime-stop-account "slot-a" t)
       (qq-state-reset))))

(defconst qq-completion-test--member
  '((user_id . "10001")
    (uid . "uid-alice")
    (nickname . "Alice")
    (card . "Alice Card")
    (remark . nil)
    (qid . "alice")
    (title . "管理员")
    (role . "admin")
    (robot . nil)))

(defconst qq-completion-test--favorite
  '((favorite_emoji_id . "10001_0_0_0_ABCDEF1234567890ABCDEF1234567890_0_0")
    (md5 . "abcdef1234567890abcdef1234567890")
    (url . "https://example.invalid/favorite.png")))

(defun qq-completion-test--cache-members (_query members)
  "Install one canonical member page for the current test chat."
  (qq-operation-test--seed (list 'group-members (qq-completion--group-id))
                           :value (list (cons 'members members))))

(ert-deftest qq-completion-token-at-point-classifies-all-composer-syntax ()
  (qq-completion-test-with-group
    (cl-letf (((symbol-function 'qq-completion--base-face-candidates)
               (lambda ()
                 (list
                  (appkit-chat-completion-candidate-create
                   :label "/斜眼笑  (178)"
                   :search-terms '("斜眼笑" "178")))))
              ((symbol-function 'appkit-chat-emoji-candidates)
               (lambda (&optional _force)
                 (list
                  (appkit-chat-completion-candidate-create
                   :label ":rocket:"
                   :search-terms '("rocket" "🚀"))))))
      (dolist (case '(("@green" member "green")
                      ("/斜眼" face "斜眼")
                      ("/178" face "178")
                      ("/fav" favorite-face "fav")
                      (":rocket" unicode-emoji "rocket")
                      (":rocket:" unicode-emoji "rocket")))
        (appkit-chatbuf-input-set-text (car case))
        (goto-char (point-max))
        (let ((token (qq-completion-token-at-point)))
          (should (eq (plist-get token :kind) (nth 1 case)))
          (should (equal (plist-get token :query) (nth 2 case)))))
      (dolist (text '("hello@example.com" "12:30" "plain"
                      "https://example.com" "/tmp/foo" ":unknown" ":)"))
        (appkit-chatbuf-input-set-text text)
        (goto-char (point-max))
        (should-not (qq-completion-token-at-point))))))

(ert-deftest qq-completion-member-token-is-group-only ()
  (qq-completion-test-with-private
    (appkit-chatbuf-input-set-text "@alice")
    (goto-char (point-max))
    (should-not (qq-completion-token-at-point))))

(ert-deftest qq-completion-member-capf-inserts-real-at-segment ()
  (qq-completion-test-with-group
    (qq-completion-test--cache-members
     "alice" (list qq-completion-test--member))
    (insert "@alice")
    (cl-letf (((symbol-function 'qq-media-avatar-display-string)
               (lambda (_user-id) "@")))
      (let* ((capf (qq-completion-member-capf))
             (table (nth 2 capf))
             (exit (plist-get (nthcdr 3 capf) :exit-function))
             (label (car (all-completions "@alice" table))))
        (should (equal "@Alice Card" label))
        (delete-region (- (point) 6) (point))
        (insert label)
        (funcall exit label 'finished)))
    (goto-char (appkit-chatbuf-input-start-position))
    (let* ((object (appkit-chatbuf-input-object-at-point))
           (segment (plist-get object :segment)))
      (should (equal "at" (alist-get 'type segment)))
      (should (equal "10001"
                     (alist-get 'qq (alist-get 'data segment))))
      (should (equal "Alice Card"
                     (alist-get 'name (alist-get 'data segment)))))
    (should (string-suffix-p " " (appkit-chatbuf-input-string)))
    (should (equal '(((type . "at")
                      (data . ((qq . "10001") (name . "Alice Card")))))
                   (qq-chat--current-input-segments)))
    (goto-char (point-max))
    (appkit-chatbuf-input-backward-delete 1)
    (should (equal "" (appkit-chatbuf-input-string)))
    (should-not (qq-chat--current-input-segments))))

(ert-deftest qq-completion-member-candidates-search-aliases-and-disambiguate ()
  (let* ((second (copy-tree qq-completion-test--member))
         (_ (setf (alist-get 'user_id second) "10002"
                  (alist-get 'uid second) "u-second"
                  (alist-get 'nickname second) "Another"))
         (candidates
          (qq-completion--member-candidates
           (list qq-completion-test--member second))))
    (should (equal '("@Alice Card" "@Alice Card · 10002")
                   (mapcar #'appkit-chat-completion-candidate-label candidates)))
    (should (member "Alice"
                    (appkit-chat-completion-candidate-search-terms
                     (car candidates))))))

(ert-deftest qq-completion-private-poke-target-reader-is-strict-peer-or-self ()
  (qq-completion-test-with-private
    (let (chosen candidates initial-input peer-prefix)
      (cl-letf (((symbol-function 'appkit-chat-completion-read)
                 (lambda (_prompt values &rest arguments)
                   (setq candidates values
                         initial-input (plist-get arguments :initial-input))
                   (setq peer-prefix
                         (funcall
                          (appkit-chat-completion-candidate-prefix (car values))
                          (car values)))
                   (seq-find
                    (lambda (candidate)
                      (equal
                       (qq-completion--poke-candidate-user-id candidate)
                       "90001"))
                    values))))
        (qq-completion-read-poke-target
         "private:10001"
         (lambda (user-id) (setq chosen user-id))
         "90001"))
      (should (equal chosen "90001"))
      (should
       (equal '("10001" "90001")
              (mapcar #'qq-completion--poke-candidate-user-id candidates)))
      (should
       (equal initial-input
              (appkit-chat-completion-candidate-label (cadr candidates))))
      (should
       (equal (concat (qq-media-avatar-display-string "10001") " ")
              peer-prefix))
      (should
       (string-match-p
        "QQ 10001"
        (appkit-chat-completion-candidate-annotation (car candidates)))))))

(ert-deftest qq-completion-private-poke-rejects-non-peer-initial-target ()
  (qq-completion-test-with-private
    (cl-letf (((symbol-function 'appkit-chat-completion-read)
               (lambda (&rest _)
                 (ert-fail "strict private target should fail before reading"))))
      (should-error
       (qq-completion-read-poke-target
        "private:10001" #'ignore "77777")
       :type 'user-error))))

(ert-deftest qq-completion-poke-rejects-zero-user-identities ()
  (qq-completion-test-with-private
    (should-error
     (qq-completion-read-poke-target
      "private:10001" #'ignore "0")
     :type 'user-error)
    (should-error
     (qq-completion--poke-candidate-user-id
      (appkit-chat-completion-candidate-create
       :label "invalid"
       :value '(:kind poke-target :user-id "0")))
     :type 'error)))

(ert-deftest qq-completion-private-poke-does-not-fallback-to-peer-uin ()
  (qq-completion-test-with-private
    (cl-letf (((symbol-function 'qq-state-session)
               (lambda (_session-key)
                 '((type . private)
                   (target-id . nil)
                   (peer-uin . "10001")
                   (title . "Alice")))))
      (should-error
       (qq-completion-read-poke-target "private:10001" #'ignore)
       :type 'user-error))))

(ert-deftest qq-completion-group-poke-loads-members-then-picks-on-second-command ()
  (qq-completion-test-with-group
    (let (requests chosen (query-reads 0))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (_prompt &optional initial &rest _)
                   (cl-incf query-reads)
                   (should (equal initial "10001"))
                   "alice"))
                ((symbol-function 'qq-runtime-request)
                 (lambda (request &rest _) (push request requests)))
                ((symbol-function 'appkit-chat-completion-read)
                 (lambda (_prompt candidates &rest _) (car candidates))))
        ;; A cold page only loads; the command never opens a picker later.
        (qq-completion-read-poke-target
         "group:20001" (lambda (user-id) (setq chosen user-id)) "10001")
        (should (equal requests '((group-members "20001"))))
        (should-not chosen)
        ;; The next explicit command reuses the query and picks from the page.
        (qq-completion-test--cache-members "" (list qq-completion-test--member))
        (qq-completion-read-poke-target
         "group:20001" (lambda (user-id) (setq chosen user-id)))
        (should (= query-reads 1))
        (should (equal chosen "10001"))))))

(ert-deftest qq-completion-group-poke-rejects-empty-query-without-request ()
  (qq-completion-test-with-group
    (let ((requests 0))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "  "))
                ((symbol-function 'qq-core-search-group-members)
                 (lambda (&rest _)
                   (cl-incf requests))))
        (should-error
         (qq-completion-read-poke-target "group:20001" #'ignore)
         :type 'user-error)
        (should (= requests 0))))))

(ert-deftest qq-completion-group-poke-without-match-chooses-nobody ()
  (qq-completion-test-with-group
    (let (chosen notice)
      (qq-completion-test--cache-members "" (list qq-completion-test--member))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "missing"))
                ((symbol-function 'message)
                 (lambda (format-string &rest arguments)
                   (setq notice (apply #'format format-string arguments)))))
        (qq-completion-read-poke-target
         "group:20001" (lambda (user-id) (setq chosen user-id)))
        (should-not chosen)
        (should (equal notice "qq: no matching group member"))))))

(ert-deftest qq-completion-small-face-selection-never-promotes-to-large ()
  (qq-completion-test-with-group
    (let ((qq-media--face-names-table (make-hash-table :test #'equal))
          (qq-media--system-emoji-tables (make-hash-table :test #'equal)))
      (cl-letf (((symbol-function 'qq-server-capabilities) #'ignore))
        (dolist
            (case
             '(("5" "/流泪" 1 1 16
                ((type . "basic") (id . "5")))
               ("344" "/大怨种" 1 1 28
                ((type . "small") (id . "344") (preview . "/大怨种")))))
          (pcase-let ((`(,id ,name ,emoji-type ,pack-id ,sticker-id ,payload)
                       case))
            (clrhash qq-media--face-names-table)
            (clrhash qq-media--system-emoji-tables)
            (puthash id name qq-media--face-names-table)
            (appkit-chatbuf-input-set-text name)
            (goto-char (point-max))
            (let* ((capf (qq-completion-face-capf))
                   (label (car (all-completions name (nth 2 capf))))
                   (exit (plist-get (nthcdr 3 capf) :exit-function))
                   (catalog (make-hash-table :test #'equal)))
              (should label)
              ;; The catalog may arrive after CAPF has produced its candidates.
              (puthash id
                       `((id . ,id) (description . ,name)
                         (emoji_type . ,emoji-type)
                         (animated_pack_id . ,pack-id)
                         (animated_sticker_id . ,sticker-id))
                       catalog)
              (puthash "slot-a" catalog qq-media--system-emoji-tables)
              (delete-region (nth 0 capf) (nth 1 capf))
              (insert label)
              (funcall exit label 'finished))
            (goto-char (appkit-chatbuf-input-start-position))
            (let* ((object (appkit-chatbuf-input-object-at-point))
                   (segment (plist-get object :segment))
                   (segments (qq-chat--current-input-segments)))
              (should (equal (list segment) segments))
              (should (equal (alist-get 'face_type (alist-get 'data segment))
                             (alist-get 'type payload)))
              (should
               (equal
                (plist-get
                 (qq-message--prepare-outbound "group:20001" segments "slot-a")
                 :segments)
                `(((kind . "face") (payload . ,payload))))))))))))

(ert-deftest qq-completion-cold-small-face-keeps-its-form-after-catalog-arrives ()
  (qq-completion-test-with-group
    (let ((qq-media--face-names-table (make-hash-table :test #'equal))
          (qq-media--system-emoji-tables (make-hash-table :test #'equal)))
      (puthash "344" "/大怨种" qq-media--face-names-table)
      (cl-letf (((symbol-function 'qq-server-capabilities) #'ignore))
        (qq-chat--insert-input-segment-object
         '((type . "at") (data . ((qq . "10001") (name . "Alice")))))
        (insert " before /大怨种")
        (let* ((capf (qq-completion-face-capf))
               (label (car (all-completions "/大怨种" (nth 2 capf))))
               (exit (plist-get (nthcdr 3 capf) :exit-function)))
          (delete-region (nth 0 capf) (nth 1 capf))
          (insert label)
          (save-excursion (insert " after"))
          (funcall exit label 'finished)
          (let ((segments (copy-tree (qq-chat--current-input-segments)))
                (catalog (make-hash-table :test #'equal)))
            (should
             (equal segments
                    '(((type . "at") (data . ((qq . "10001") (name . "Alice"))))
                      ((type . "text") (data . ((text . " before "))))
                      ((type . "face")
                       (data . ((id . "344") (face_type . "small")
                                (description . "/大怨种"))))
                      ((type . "text") (data . ((text . " after")))))))
            (puthash "344" '((id . "344") (description . "/大怨种")
                             (emoji_type . 1)
                             (animated_pack_id . 1) (animated_sticker_id . 28))
                     catalog)
            (puthash "slot-a" catalog qq-media--system-emoji-tables)
            (qq-chat-render)
            (should (equal segments (qq-chat--current-input-segments)))))))))

(ert-deftest qq-completion-explicit-large-face-choice-survives-draft-render ()
  (qq-completion-test-with-group
    (let ((qq-media--system-emoji-tables (make-hash-table :test #'equal))
          (catalog (make-hash-table :test #'equal)))
      (puthash "5" '((id . "5") (description . "/流泪") (emoji_type . 1)
                     (animated_pack_id . 1) (animated_sticker_id . 16))
               catalog)
      (puthash "415" '((id . "415") (description . "/划龙舟") (emoji_type . 3)
                       (animated_pack_id . 1) (animated_sticker_id . 43))
               catalog)
      (puthash "slot-a" catalog qq-media--system-emoji-tables)
      (qq-chat-attach-face "5")
      (qq-chat-attach-large-face "5")
      (qq-chat-attach-large-face "415")
      (qq-chat-render)
      (clrhash qq-media--system-emoji-tables)
      (should
       (equal
        (plist-get (qq-message--prepare-outbound
                    "group:20001" (qq-chat--current-input-segments) "slot-a")
                   :segments)
        '(((kind . "face") (payload . ((type . "basic") (id . "5"))))
          ((kind . "face")
           (payload . ((type . "animated") (id . "5") (preview . "/流泪")
                       (pack_id . 1) (sticker_id . 16) (sticker_type . 1))))
          ((kind . "face")
           (payload . ((type . "animated") (id . "415") (preview . "/划龙舟")
                       (pack_id . 1) (sticker_id . 43) (sticker_type . 3)))))))
      (let ((draft (qq-chat--current-input-segments)))
        (should-error (qq-chat-attach-large-face "178") :type 'user-error)
        (should (equal draft (qq-chat--current-input-segments)))))))

(ert-deftest qq-completion-large-face-picker-cannot-insert-into-replacement-view ()
  (qq-completion-test-with-group
    (let ((view (appkit-current-surface)))
      (insert "keep draft")
      (qq-chat--sync-draft-from-buffer)
      (cl-letf (((symbol-function 'qq-completion-read-large-face-id)
                 (lambda ()
                   (appkit-surface-stop view)
                   (qq-chat--ensure-view)
                   "5")))
        (should-error (qq-chat-attach-large-face) :type 'user-error))
      (should (equal (appkit-chatbuf-input-string) "keep draft")))))

(ert-deftest qq-completion-large-face-picker-loads-only-sendable-visible-entries ()
  (qq-completion-test-with-group
    (let ((qq-media--system-emoji-tables (make-hash-table :test #'equal))
          (qq-media--system-emoji-requests (make-hash-table :test #'equal))
          (qq-media--consumers (make-hash-table :test #'eq))
          (qq-media-system-emoji-catalog-hook nil)
          (qq-media-cache-update-hook nil)
          (qq-media--consumer (make-symbol "large-picker"))
          success candidates status unsubscribe
          (publications 0))
      (cl-letf (((symbol-function 'qq-server-ready-p) (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () '("system_emoji.list")))
                ((symbol-function 'qq-account-get) (lambda (_) t))
                ((symbol-function 'qq-media--note-cache-updated) #'ignore)
                ((symbol-function 'qq-rpc-call)
                 (lambda (_method _params &rest options)
                   (setq success (plist-get options :callback)))))
        (unwind-protect
            (progn
              (setq unsubscribe
                    (qq-completion--subscribe-large-faces
                     (current-buffer) qq-chat--session-key (appkit-current-surface)
                     (lambda (items &optional message)
                       (setq candidates items status message)
                       (cl-incf publications))))
              (should-not candidates)
              (should status)
              (let ((before publications))
                (run-hook-with-args 'qq-media-system-emoji-catalog-hook
                                    "slot-b" "another account failed")
                (should (= before publications)))
              (let* ((entries
                      '(((id . "5") (description . "/流泪") (emoji_type . 1)
                         (animated_pack_id . 1) (animated_sticker_id . 16))
                        ((id . "114") (description . "/篮球") (emoji_type . 2)
                         (animated_pack_id . 1) (animated_sticker_id . 13))
                        ((id . "415") (description . "/划龙舟") (emoji_type . 3)
                         (animated_pack_id . 1) (animated_sticker_id . 43))
                        ((id . "416") (description . "/中龙舟") (emoji_type . 3)
                         (animated_pack_id . 1) (animated_sticker_id . 44)
                         (hidden . t))
                        ((id . "178") (description . "/斜眼笑") (emoji_type . 0))
                        ((id . "500") (description . "/缺少发送参数") (emoji_type . 3))))
                     (group `((emojis . ,entries)))
                     (panel `((groups . (,group)))))
                (funcall success `((panels . (,panel)))))
              (should-not status)
              (should
               (equal (mapcar
                       (lambda (candidate)
                         (plist-get (appkit-chat-completion-candidate-value candidate)
                                    :face-id))
                       candidates)
                      '("5" "114" "415"))))
          (when unsubscribe (funcall unsubscribe))
          (qq-media-release-consumer qq-media--consumer))))))

(ert-deftest qq-completion-large-face-picker-failure-and-cancel-do-not-insert ()
  (qq-completion-test-with-group
    (let ((qq-media--system-emoji-tables (make-hash-table :test #'equal))
          (qq-media--system-emoji-requests (make-hash-table :test #'equal))
          (qq-media--consumers (make-hash-table :test #'eq))
          (qq-media-system-emoji-catalog-hook nil)
          (qq-media-cache-update-hook nil)
          (qq-media--consumer (make-symbol "large-picker"))
          success failure candidates status unsubscribe
          (publications 0))
      (insert "keep this draft")
      (cl-letf (((symbol-function 'qq-server-ready-p) (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () '("system_emoji.list")))
                ((symbol-function 'qq-account-get) (lambda (_) t))
                ((symbol-function 'qq-media--note-cache-updated) #'ignore)
                ((symbol-function 'qq-rpc-call)
                 (lambda (_method _params &rest options)
                   (setq success (plist-get options :callback)
                         failure (plist-get options :errback)))))
        (cl-labels ((subscribe ()
                      (qq-completion--subscribe-large-faces
                       (current-buffer) qq-chat--session-key (appkit-current-surface)
                       (lambda (items &optional message)
                         (setq candidates items status message)
                         (cl-incf publications)))))
          (unwind-protect
              (progn
                (setq unsubscribe (subscribe))
                (funcall failure nil "account offline")
                (should-not candidates)
                (should (string-match-p "account offline" status))
                (funcall unsubscribe)
                (setq unsubscribe (subscribe))
                (funcall unsubscribe)
                (setq unsubscribe nil)
                (let ((before publications))
                  (funcall success '((panels . nil)))
                  (should (= before publications)))
                (should (equal (appkit-chatbuf-input-string) "keep this draft")))
            (when unsubscribe (funcall unsubscribe))
            (qq-media-release-consumer qq-media--consumer)))))))

(ert-deftest qq-completion-face-pickers-work-without-send-metadata ()
  (let ((qq-media--face-names-table (make-hash-table :test #'equal))
        (qq-media--system-emoji-tables (make-hash-table :test #'equal)))
    (puthash "344" "/大怨种" qq-media--face-names-table)
    (cl-letf (((symbol-function 'qq-server-capabilities) #'ignore)
              ((symbol-function 'appkit-chat-emoji-candidates) #'ignore)
              ((symbol-function 'completing-read)
               (lambda (_prompt table &rest _)
                 (car (all-completions "/大怨种" table)))))
      (should (equal (qq-completion-read-base-face-id) "344"))
      (should (equal (qq-completion-read-reaction)
                     '((emoji-id . "344") (emoji-type . "1")))))))

(ert-deftest qq-completion-custom-face-candidates-keep-identity-and-preview ()
  (let* ((faces '(((favorite_emoji_id . "favorite-one")
                   (md5 . "11111111111111111111111111111111")
                   (url . "https://example.invalid/one.png"))
                  ((favorite_emoji_id . "favorite-two")
                   (md5 . "22222222222222222222222222222222")
                   (url . "https://example.invalid/two.png"))))
         (candidates (qq-completion--custom-face-candidates faces))
         previewed)
    (should
     (equal '("favorite-one" "favorite-two")
            (mapcar
             (lambda (candidate)
               (alist-get
                'favorite_emoji_id
                (plist-get
                 (appkit-chat-completion-candidate-value candidate) :face)))
             candidates)))
    (cl-letf (((symbol-function 'qq-media--custom-face-completion-prefix)
               (lambda (face)
                 (setq previewed face)
                 "preview ")))
      (let ((first (car candidates)))
        (should
         (equal "preview "
                (funcall (appkit-chat-completion-candidate-prefix first)
                         first)))
        (should (equal previewed (car faces)))))))

(ert-deftest qq-completion-fav-capf-inserts-durable-favorite-segment ()
  (qq-completion-test-with-group
    (let* ((face qq-completion-test--favorite)
           (favorite-id (alist-get 'favorite_emoji_id face)))
      (qq-operation-test--seed '(favorite-emojis) :value (list (cons 'entries (list face))))
      (insert "/fav")
      (let* ((capf (qq-completion-face-capf))
             (table (nth 2 capf))
             (exit (plist-get (nthcdr 3 capf) :exit-function))
             (label (car (all-completions "/fav" table))))
        (should (stringp label))
        (delete-region (- (point) 4) (point))
        (insert label)
        (funcall exit label 'finished))
      (goto-char (appkit-chatbuf-input-start-position))
      (let* ((object (appkit-chatbuf-input-object-at-point))
             (segment (plist-get object :segment)))
        (should (equal (list segment) (qq-chat--current-input-segments)))
        (should
         (equal segment
                `((type . "favorite_emoji")
                  (data . ((favorite_emoji_id . ,favorite-id))))))))))

(ert-deftest qq-completion-unicode-emoji-capf-inserts-plain-text ()
  (qq-completion-test-with-group
    (insert ":rocket:")
    (let ((appkit-chat-emoji--candidates
           (list
            (appkit-chat-completion-candidate-create
             :label ":rocket:"
             :insert "🚀"))))
      (let* ((capf (qq-completion-unicode-emoji-capf))
             (exit (plist-get (nthcdr 3 capf) :exit-function)))
        (should capf)
        (funcall exit ":rocket:" 'finished)
        (should (equal "🚀" (appkit-chatbuf-input-string)))))))

(ert-deftest qq-completion-reaction-picker-supports-scalar-unicode ()
  (let* ((rocket
          (appkit-chat-completion-candidate-create
           :label ":rocket:"
           :prefix "🚀 "
           :value '(:kind unicode-emoji :emoji "🚀")))
         (family
          (appkit-chat-completion-candidate-create
           :label ":family:"
           :value '(:kind unicode-emoji :emoji "👨‍👩‍👧")))
         seen
         seen-prefix)
    (cl-letf (((symbol-function 'qq-completion--base-face-candidates)
               #'ignore)
              ((symbol-function 'appkit-chat-emoji-candidates)
               (lambda (&optional _) (list rocket family)))
              ((symbol-function 'completing-read)
               (lambda (_prompt table &rest _)
                 (let* ((metadata (completion-metadata "" table nil))
                        (affixation-function
                         (completion-metadata-get
                          metadata 'affixation-function)))
                   (setq seen (all-completions "" table)
                         seen-prefix
                         (cadr
                          (car (funcall affixation-function seen)))))
                 (car seen))))
      (should
       (equal (qq-completion-read-reaction)
              '((emoji-id . "128640") (emoji-type . "2"))))
      (should (equal seen '(":rocket:")))
      (should (equal seen-prefix "🚀 ")))))

(ert-deftest qq-completion-reaction-picker-preserves-preview-while-narrowing ()
  (let ((white-flower
         (appkit-chat-completion-candidate-create
          :label ":white_flower:"
          :prefix (concat (propertize " " 'display 'white-preview) " ")
          :value '(:kind unicode-emoji :emoji "💮")))
        (wind-chime
         (appkit-chat-completion-candidate-create
          :label ":wind_chime:"
          :prefix (concat (propertize " " 'display 'wind-preview) " ")
          :value '(:kind unicode-emoji :emoji "🎐"))))
    (cl-letf (((symbol-function 'qq-completion--base-face-candidates)
               #'ignore)
              ((symbol-function 'appkit-chat-emoji-candidates)
               (lambda (&optional _) (list white-flower wind-chime)))
              ((symbol-function 'completing-read)
               (lambda (_prompt table &rest _)
                 (let* ((metadata (completion-metadata "" table nil))
                        (affixation-function
                         (completion-metadata-get
                          metadata 'affixation-function))
                        (matches
                         (completion-all-completions "whi" table nil 3))
                        (title (car matches))
                        (row
                         (car
                          (funcall affixation-function
                                   (list
                                    (substring-no-properties title)))))
                        (prefix (cadr row)))
                   (should
                    (eq (completion-metadata-get metadata 'category)
                        'appkit-chat))
                   (should (equal (cdr matches) 0))
                   (should
                    (equal (substring-no-properties title)
                           ":white_flower:"))
                   (should (eq (get-text-property 0 'display prefix)
                               'white-preview))
                   (substring-no-properties title)))))
      (should
       (equal (qq-completion-read-reaction)
              '((emoji-id . "128174") (emoji-type . "2")))))))

(ert-deftest qq-completion-cold-fav-tab-requests-catalog-then-completes ()
  (qq-completion-test-with-group
    (insert "/fav")
    (let (requests)
      (cl-letf (((symbol-function 'qq-runtime-request)
                 (lambda (request &rest _) (push request requests))))
        (should (qq-completion-complete))
        (should (equal requests '((favorite-emojis)))))
      (qq-operation-test--seed
       '(favorite-emojis) :value (list (cons 'entries (list qq-completion-test--favorite))))
      (should (all-completions "/fav" (nth 2 (qq-completion-face-capf)))))))

(ert-deftest qq-completion-favorite-aliases-address-native-catalog ()
  (let* ((face '((favorite_emoji_id . "favorite-one")
                 (md5 . "11111111111111111111111111111111")
                 (url . "https://example.invalid/one.png")))
         (candidate (car (qq-completion--custom-face-candidates (list face)))))
    (dolist (alias qq-completion--custom-face-query-prefixes)
      (should
       (appkit-chat-completion--candidate-matches-p
        candidate (concat "/" alias))))))

(ert-deftest qq-completion-favorite-candidates-exclude-unsendable-faces ()
  (let ((candidates
         (qq-completion--custom-face-candidates
          '("malformed"
            ((md5 . "bad"))
            ((favorite_emoji_id . "favorite-good")
             (md5 . "33333333333333333333333333333333")
             (url . "https://example.invalid/good.png"))))))
    (should (= 1 (length candidates)))
    (should (equal "favorite-good"
                   (alist-get
                    'favorite_emoji_id
                    (plist-get
                     (appkit-chat-completion-candidate-value (car candidates))
                     :face))))))

(ert-deftest qq-completion-favorite-annotation-search-keeps-durable-insertion ()
  (qq-completion-test-with-group
    (qq-operation-test--seed
     '(favorite-emojis)
     :value '((entries ((favorite_emoji_id . "favorite-one")
                        (md5 . "11111111111111111111111111111111")
                        (description . "今天开心"))
                       ((favorite_emoji_id . "favorite-two")
                        (md5 . "22222222222222222222222222222222")
                        (description . "今天困了")))))
    (dolist (query '("/fav开心" "/favorite开心"))
      (appkit-chatbuf-input-set-text query)
      (goto-char (point-max))
      (let* ((capf (qq-completion-face-capf))
             (labels (all-completions query (nth 2 capf))))
        (should (= (length labels) 1))
        (should (string-match-p "今天开心" (car labels)))
        (delete-region (nth 0 capf) (nth 1 capf))
        (insert (car labels))
        (funcall (plist-get (nthcdr 3 capf) :exit-function)
                 (car labels) 'finished)
        (should
         (equal (qq-chat--current-input-segments)
                '(((type . "favorite_emoji")
                   (data . ((favorite_emoji_id . "favorite-one")))))))))))

(ert-deftest qq-completion-cold-member-tab-requests-page-then-completes ()
  (qq-completion-test-with-group
    (insert "@alice")
    (let (requests)
      (cl-letf (((symbol-function 'qq-runtime-request)
                 (lambda (request &rest _) (push request requests))))
        (should (qq-completion-complete))
        (should (equal requests '((group-members "20001")))))
      (qq-completion-test--cache-members "" (list qq-completion-test--member))
      (should (all-completions "@alice" (nth 2 (qq-completion-member-capf)))))))

(ert-deftest qq-completion-mode-binds-telega-style-completion-keys ()
  (should (eq (lookup-key qq-chat-mode-map (kbd "TAB")) #'qq-chat-complete))
  (should (eq (lookup-key qq-chat-mode-map (kbd "<tab>")) #'qq-chat-complete))
  (should (eq (lookup-key qq-chat-mode-map (kbd "C-M-i")) #'qq-chat-complete)))

(ert-deftest qq-completion-picker-media-is-account-bound-and-released-on-exit ()
  (dolist (exit '(return error quit))
    (let ((qq-media--consumers (make-hash-table :test #'eq))
          owner
          prefix
          observations
          released)
      (qq-state-with-store (qq-state-store-create :account-id "slot-a")
        (cl-letf (((symbol-function 'qq-resource-access-release)
                   (lambda (ref) (push ref released))))
          (condition-case condition
              (qq-completion--read-with-media
               (lambda (_prompt candidates &rest _options)
                 (setq owner qq-media--consumer
                       prefix (appkit-chat-completion-candidate-prefix
                               (car candidates)))
                 (should (symbolp owner))
                 (should-not (eq owner (current-buffer)))
                 (puthash "resource" 'picker-ref
                          (plist-get (gethash owner qq-media--consumers) :refs))
                 (let ((qq-media--consumer 'unrelated))
                   (qq-state-with-store (qq-state-store-create :account-id "slot-b")
                     (funcall prefix (car candidates))))
                 (pcase exit
                   ('error (error "reader failed"))
                   ('quit (signal 'quit nil))
                   (_ (car candidates))))
               "Picker: "
               (list
                (appkit-chat-completion-candidate-create
                 :label "Face"
                 :prefix (lambda (_candidate)
                           (push (list qq-media--consumer
                                       (qq-runtime-current-account-id))
                                 observations)
                           "preview"))))
            ((error quit) (should (eq (car condition) exit))))
          (should (equal observations (list (list owner "slot-a"))))
          (should (equal released '(picker-ref)))
          (should-not (gethash owner qq-media--consumers))
          ;; Even an affixation callback retained by another frontend is fenced.
          (should-not (funcall prefix nil))
          (should (= 1 (length observations))))))))

(ert-deftest qq-completion-picker-prefix-rejects-old-media-generation ()
  (let ((qq-media--consumers (make-hash-table :test #'eq))
        (qq-media--generation 10)
        (calls 0))
    (qq-state-with-store (qq-state-store-create :account-id "slot-a")
      (qq-completion--read-with-media
       (lambda (_prompt candidates &rest _options)
         (let ((prefix (appkit-chat-completion-candidate-prefix (car candidates))))
           (funcall prefix nil)
           (cl-incf qq-media--generation)
           (should-not (funcall prefix nil))))
       "Picker: "
       (list (appkit-chat-completion-candidate-create
              :label "Face"
              :prefix (lambda (_) (cl-incf calls) "preview"))))
      (should (= calls 1))
      (should (= 0 (hash-table-count qq-media--consumers))))))

(ert-deftest qq-completion-favorite-unsubscribe-releases-only-picker-media ()
  (qq-completion-test-with-group
    (let* ((qq-media--consumers (make-hash-table :test #'eq))
           (qq-media--consumer nil)
           (qq-media-cache-update-hook nil)
           (view (appkit-current-surface))
           (chat-state (qq-media--owner-state view))
           candidates picker-owner preview-account unsubscribe)
      (qq-operation-test--seed
       '(favorite-emojis) :value '((entries ((favorite_emoji_id . "favorite-one")))))
      (cl-letf (((symbol-function 'qq-media--custom-face-completion-prefix)
                 (lambda (_face)
                   (setq picker-owner qq-media--consumer
                         preview-account (qq-runtime-current-account-id))
                   "preview")))
        (unwind-protect
            (progn
              (setq unsubscribe
                    (qq-completion--subscribe-custom-faces
                     (current-buffer) qq-chat--session-key view nil
                     (lambda (replacement &optional _status)
                       (setq candidates replacement))))
              (let ()
                (qq-state-with-store (qq-state-store-create :account-id "slot-b")
                  (funcall (appkit-chat-completion-candidate-prefix
                            (car candidates)) (car candidates))))
              (should (equal preview-account "slot-a"))
              (should-not (eq picker-owner view))
              (should (gethash picker-owner qq-media--consumers))
              (funcall unsubscribe)
              (should-not (gethash picker-owner qq-media--consumers))
              (should (eq chat-state (gethash view qq-media--consumers)))
              (should-not qq-media-cache-update-hook))
          (when unsubscribe (funcall unsubscribe)))))))

(ert-deftest qq-completion-composer-preview-reacquires-under-chat-owner ()
  (qq-completion-test-with-group
    (let ((qq-media--consumer 'finished-picker)
          (view (appkit-current-surface))
          preview-owner insertion-owner)
      (cl-letf (((symbol-function 'qq-media-custom-face-display-string)
                 (lambda (_face)
                   (setq preview-owner qq-media--consumer)
                   "preview"))
                ((symbol-function 'qq-chat--insert-input-segment-object)
                 (lambda (_segment &optional _label)
                   (setq insertion-owner qq-media--consumer))))
        (qq-completion--insert-protocol-candidate
         (car (qq-completion--custom-face-candidates
               '(((favorite_emoji_id . "favorite-one"))))))
        (should (eq preview-owner view))
        (should (eq insertion-owner view))))))

(provide 'qq-completion-test)

;;; qq-completion-test.el ends here
