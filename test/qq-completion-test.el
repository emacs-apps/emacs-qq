;;; qq-completion-test.el --- Tests for QQ composer completion -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'qq-chat)
(require 'qq-completion)

(defmacro qq-completion-test-with-group (&rest body)
  "Evaluate BODY in a temporary writable QQ group composer."
  (declare (indent 0) (debug t))
  `(let ((qq-runtime--accounts (make-hash-table :test #'equal))
         (qq-state--partitions (make-hash-table :test #'equal))
         (qq-state--active-account-id nil))
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
             (appkit-chatbuf-install-prompt "qq> ")
             ,@body))
       (qq-runtime-stop-account "slot-a" t)
       (qq-state-reset))))

(defmacro qq-completion-test-with-private (&rest body)
  "Evaluate BODY in a temporary private chat with peer and self identity."
  (declare (indent 0) (debug t))
  `(let ((qq-runtime--accounts (make-hash-table :test #'equal))
         (qq-state--partitions (make-hash-table :test #'equal))
         (qq-state--active-account-id nil))
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

(defun qq-completion-test--cache-members (query members)
  "Install QUERY MEMBERS under the current test runtime generation."
  (let ((view (appkit-current-view)))
    (unless (appkit-view-live-p view)
      (ert-fail "member cache fixture requires a live Appkit view"))
    (qq-completion--activate-member-app (appkit-view-app view))
    (puthash query members qq-completion--member-cache)))

(ert-deftest qq-completion-token-at-point-classifies-all-composer-syntax ()
  (qq-completion-test-with-group
    (cl-letf (((symbol-function 'qq-completion--base-face-candidates)
               (lambda ()
                 (list
                  (appkit-chat-completion-candidate-create
                   :label "/斜眼笑  (178)" :search-terms '("斜眼笑" "178")))))
              ((symbol-function 'appkit-chat-emoji-candidates)
               (lambda (&optional _force)
                 (list
                  (appkit-chat-completion-candidate-create
                   :label ":rocket:" :search-terms '("rocket" "🚀"))))))
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
    (let (chosen candidates initial-input)
      (cl-letf (((symbol-function 'appkit-chat-completion-read)
                 (lambda (_prompt values &rest arguments)
                   (setq candidates values
                         initial-input (plist-get arguments :initial-input))
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
              (funcall
               (appkit-chat-completion-candidate-prefix (car candidates))
               (car candidates))))
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
       :label "invalid" :value '(:kind poke-target :user-id "0")))
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

(ert-deftest qq-completion-group-poke-result-waits-for-explicit-continuation ()
  (qq-completion-test-with-group
    (let (captured-group captured-query captured-limit success errback chosen
          initial-query owner
          (query-reads 0)
          (picker-reads 0))
      ;; A broad composer cache must never become a poke target source.
      (qq-completion-test--cache-members
       "" (list '((user_id . "99999"))))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (_prompt &optional initial _history &rest _)
                   (cl-incf query-reads)
                   (setq initial-query initial)
                   "green"))
                ((symbol-function 'qq-core-search-group-members)
                 (lambda (group-id query callback &optional error-callback limit)
                   (setq captured-group group-id
                         captured-query query
                         captured-limit limit
                         success callback
                         errback error-callback)
                   "poke-request-1"))
                ((symbol-function 'appkit-chat-completion-read)
                 (lambda (_prompt candidates &rest _)
                   (cl-incf picker-reads)
                   (car candidates))))
        (qq-completion-read-poke-target
         "group:20001"
         (lambda (user-id) (setq chosen user-id))
         "10001")
        (setq owner qq-completion--poke-request)
        (should (equal initial-query "10001"))
        (should (equal captured-group "20001"))
        (should (equal captured-query "green"))
        (should (= captured-limit 200))
        (should (functionp success))
        (should (functionp errback))
        (should (eq (plist-get owner :view) (appkit-current-view)))
        (should (eq (plist-get owner :status) 'pending))

        ;; The transport callback updates only the owner model.  It cannot
        ;; open the picker or invoke the poke continuation.
        (funcall success (list qq-completion-test--member))
        (should (= picker-reads 0))
        (should-not chosen)
        (should (eq (plist-get owner :status) 'ready))
        (should (equal (plist-get owner :members)
                       (list qq-completion-test--member)))

        ;; A second explicit command consumes that accepted model.
        (qq-completion-read-poke-target
         "group:20001"
         (lambda (user-id) (setq chosen user-id)))
        (should (= query-reads 1))
        (should (= picker-reads 1))
        (should (equal chosen "10001"))
        (should-not qq-completion--poke-request)))))

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

(ert-deftest qq-completion-group-poke-no-result-clears-owner-without-target ()
  (qq-completion-test-with-group
    (let (success chosen notice)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "missing"))
                ((symbol-function 'qq-core-search-group-members)
                 (lambda (_group-id _query callback &optional _errback _limit)
                   (setq success callback)
                   "poke-request"))
                ((symbol-function 'message)
                 (lambda (format-string &rest arguments)
                   (setq notice (apply #'format format-string arguments)))))
        (qq-completion-read-poke-target
         "group:20001" (lambda (user-id) (setq chosen user-id)))
        (should qq-completion--poke-request)
        (funcall success nil)
        (should-not chosen)
        (should-not notice)
        (should (eq (plist-get qq-completion--poke-request :status) 'empty))
        (qq-completion-read-poke-target
         "group:20001" (lambda (user-id) (setq chosen user-id)))
        (should (equal notice "qq: no matching group member"))
        (should-not qq-completion--poke-request)))))

(ert-deftest qq-completion-group-poke-sync-signal-clears-owner ()
  (qq-completion-test-with-group
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _) "green"))
              ((symbol-function 'qq-core-search-group-members)
               (lambda (&rest _)
                 (error "synchronous transport failure"))))
      (should-error
       (qq-completion-read-poke-target "group:20001" #'ignore)
       :type 'error)
      (should-not qq-completion--poke-request))))

(ert-deftest qq-completion-group-poke-error-clears-owner-without-target ()
  (qq-completion-test-with-group
    (let (errback chosen notice)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "green"))
                ((symbol-function 'qq-core-search-group-members)
                 (lambda (_group-id _query _callback &optional error-callback _limit)
                   (setq errback error-callback)
                   "poke-request"))
                ((symbol-function 'message)
                 (lambda (format-string &rest arguments)
                   (setq notice (apply #'format format-string arguments)))))
        (qq-completion-read-poke-target
         "group:20001" (lambda (user-id) (setq chosen user-id)))
        (should qq-completion--poke-request)
        (funcall errback nil "transport failed")
        (should-not chosen)
        (should-not notice)
        (should (eq (plist-get qq-completion--poke-request :status) 'failed))
        (qq-completion-read-poke-target
         "group:20001" (lambda (user-id) (setq chosen user-id)))
        (should (equal notice
                       "qq: failed to search group members: transport failed"))
        (should-not qq-completion--poke-request)))))

(ert-deftest qq-completion-group-poke-new-request-cancels-old-owner ()
  (qq-completion-test-with-group
    (let ((queries '("first" "second"))
          callbacks tokens cancelled)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _)
                   (prog1 (car queries) (setq queries (cdr queries)))))
                ((symbol-function 'qq-core-search-group-members)
                 (lambda (_group-id _query callback &optional _errback _limit)
                   (let ((token (format "request-%d" (1+ (length tokens)))))
                     (setq callbacks (append callbacks (list callback))
                           tokens (append tokens (list token)))
                     token)))
                ((symbol-function 'qq-request-cancel)
                 (lambda (token) (push token cancelled))))
        (qq-completion-read-poke-target "group:20001" #'ignore)
        (qq-completion-read-poke-target "group:20001" #'ignore)
        (should (equal cancelled '("request-1")))
        ;; The cancelled owner's response cannot replace the current model.
        (funcall (car callbacks) (list qq-completion-test--member))
        (should (eq (plist-get qq-completion--poke-request :status) 'pending))
        ;; The current owner can accept the model, but still cannot present it.
        (funcall (cadr callbacks) (list qq-completion-test--member))
        (should (eq (plist-get qq-completion--poke-request :status) 'ready))))))

(ert-deftest qq-completion-group-poke-ignores-stale-session-response ()
  (qq-completion-test-with-group
    (let (success chosen picker-called)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "green"))
                ((symbol-function 'qq-core-search-group-members)
                 (lambda (_group-id _query callback &optional _errback _limit)
                   (setq success callback)
                   "poke-request"))
                ((symbol-function 'appkit-chat-completion-read)
                 (lambda (&rest _args) (setq picker-called t))))
        (qq-completion-read-poke-target
         "group:20001" (lambda (user-id) (setq chosen user-id)))
        (setq-local qq-chat--session-key "group:other")
        (funcall success (list qq-completion-test--member))
        (should-not picker-called)
        (should-not chosen)
        (should-not qq-completion--poke-request)))))

(ert-deftest qq-completion-group-poke-ignores-replacement-view-response ()
  (qq-completion-test-with-group
    (let (success chosen picker-called old-view)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "green"))
                ((symbol-function 'qq-core-search-group-members)
                 (lambda (_group-id _query callback &optional _errback _limit)
                   (setq success callback)
                   "poke-request"))
                ((symbol-function 'appkit-chat-completion-read)
                 (lambda (&rest _)
                   (setq picker-called t))))
        (qq-completion-read-poke-target
         "group:20001" (lambda (user-id) (setq chosen user-id)))
        (setq old-view (plist-get qq-completion--poke-request :view))
        (appkit-kill-view old-view)
        (should-not (eq old-view (qq-chat--ensure-view)))
        (funcall success (list qq-completion-test--member))
        (should-not picker-called)
        (should-not chosen)
        (should-not qq-completion--poke-request)))))

(ert-deftest qq-completion-group-poke-picker-rechecks-exact-view-after-read ()
  (qq-completion-test-with-group
    (let (success chosen owner)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "green"))
                ((symbol-function 'qq-core-search-group-members)
                 (lambda (_group-id _query callback &optional _errback _limit)
                   (setq success callback)
                   "poke-request"))
                ((symbol-function 'appkit-chat-completion-read)
                 (lambda (_prompt candidates &rest _)
                   (appkit-kill-view (plist-get owner :view))
                   (qq-chat--ensure-view)
                   (car candidates))))
        (qq-completion-read-poke-target
         "group:20001" (lambda (user-id) (setq chosen user-id)))
        (setq owner qq-completion--poke-request)
        (funcall success (list qq-completion-test--member))
        (qq-completion-read-poke-target
         "group:20001" (lambda (user-id) (setq chosen user-id)))
        (should-not chosen)
        (should-not qq-completion--poke-request)))))

(ert-deftest qq-completion-face-capf-inserts-structured-face ()
  (qq-completion-test-with-group
    (insert "/斜")
    (let* ((capf (qq-completion-face-capf))
           (table (nth 2 capf))
           (exit (plist-get (nthcdr 3 capf) :exit-function))
           (label (seq-find (lambda (candidate)
                              (string-suffix-p "(178)" candidate))
                            (all-completions "/斜" table))))
      (should label)
      (delete-region (- (point) 2) (point))
      (insert label)
      (funcall exit label 'finished))
    (goto-char (appkit-chatbuf-input-start-position))
    (let* ((object (appkit-chatbuf-input-object-at-point))
           (segment (plist-get object :segment)))
      (should (equal "face" (alist-get 'type segment)))
      (should (equal "178" (alist-get 'id (alist-get 'data segment)))))))

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
    (let* ((favorite-id "10001_0_0_0_ABCDEF1234567890ABCDEF1234567890_0_0")
           (face `((favorite_emoji_id . ,favorite-id)
                   (md5 . "abcdef1234567890abcdef1234567890")
                   (url . "https://example.invalid/favorite.png"))))
      (setq qq-completion--custom-faces (list face))
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

(ert-deftest qq-completion-cold-fav-tab-caches-without-async-presentation ()
  (qq-completion-test-with-group
    (insert "/fav")
    (let (success owner)
      (cl-letf (((symbol-function 'qq-media-refresh-custom-faces)
                 (lambda (callback &optional _errback _count)
                   (setq success callback)))
                ((symbol-function 'completion-at-point)
                 (lambda ()
                   (ert-fail "favorite callback must not reopen completion")))
                ((symbol-function 'run-at-time)
                 (lambda (&rest _args)
                   (ert-fail "favorite callback must not schedule UI"))))
        (should (qq-completion-complete))
        (setq owner qq-completion--custom-face-pending)
        (should (eq (plist-get owner :view) (appkit-current-view)))
        (funcall success '(((favorite_emoji_id . "favorite-one")
                            (md5 . "11111111111111111111111111111111")
                            (url . "https://example.invalid/one.png"))))
        (should-not qq-completion--custom-face-pending)
        (should (equal "favorite-one"
                       (alist-get 'favorite_emoji_id
                                  (car qq-completion--custom-faces))))))))

(ert-deftest qq-completion-favorite-request-tracks-latest-query ()
  (qq-completion-test-with-group
    (let (success)
      (cl-letf (((symbol-function 'qq-media-ensure-custom-faces)
                 (lambda (callback &optional _errback)
                   (setq success callback)
                   'request-token))
                ((symbol-function 'run-at-time)
                 (lambda (&rest _args)
                   (ert-fail "favorite callback must not schedule UI")))
                ((symbol-function 'completion-at-point)
                 (lambda ()
                   (ert-fail "favorite callback must not reopen completion"))))
        (qq-completion--request-custom-faces "fav" t)
        (let ((owner qq-completion--custom-face-pending))
          (qq-completion--request-custom-faces "fav趴" t)
          (should (eq owner qq-completion--custom-face-pending))
          (should (eq (plist-get owner :view) (appkit-current-view)))
          (should (equal "fav趴" (plist-get owner :query)))
          (funcall success nil)
          (should-not qq-completion--custom-face-pending))))))

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

(ert-deftest qq-completion-cold-member-tab-needs-explicit-second-completion ()
  (qq-completion-test-with-group
    (insert "@missing")
    (let (success (frontend-calls 0))
      (cl-letf (((symbol-function 'qq-core-search-group-members)
                 (lambda (_group-id _query callback &optional _errback _limit)
                   (setq success callback)
                   'request-token))
                ((symbol-function 'appkit-chat-completion-complete)
                 (lambda ()
                   (cl-incf frontend-calls)
                   t))
                ((symbol-function 'completion-at-point)
                 (lambda ()
                   (ert-fail "member callback must not reopen completion")))
                ((symbol-function 'run-at-time)
                 (lambda (&rest _args)
                   (ert-fail "member callback must not schedule UI"))))
        (should (qq-completion-complete))
        (let ((owner (gethash "missing" qq-completion--member-pending)))
          (should (eq (plist-get owner :view) (appkit-current-view)))
          (should-not (plist-member owner :reopen)))
        (funcall success (list qq-completion-test--member))
        (should (= frontend-calls 0))
        (should-not (gethash "missing" qq-completion--member-pending))
        (should (equal (qq-completion--cached-members "missing")
                       (list qq-completion-test--member)))
        (should (qq-completion-complete))
        (should (= frontend-calls 1))))))

(ert-deftest qq-completion-replacement-view-rejects-member-response ()
  (qq-completion-test-with-group
    (let (success old-view)
      (cl-letf (((symbol-function 'qq-core-search-group-members)
                 (lambda (_group-id _query callback &optional _errback _limit)
                   (setq success callback)
                   'request-token)))
        (qq-completion--request-members "alice"))
      (setq old-view
            (plist-get (gethash "alice" qq-completion--member-pending) :view))
      (appkit-kill-view old-view)
      (should-not (eq old-view (qq-chat--ensure-view)))
      (funcall success (list qq-completion-test--member))
      (should-not (gethash "alice" qq-completion--member-pending))
      (should (eq qq-completion--cache-miss
                  (qq-completion--cached-members "alice"))))))

(ert-deftest qq-completion-replacement-view-retries-member-query-immediately ()
  (qq-completion-test-with-group
    (let (successes old-view replacement-view)
      (cl-letf (((symbol-function 'qq-core-search-group-members)
                 (lambda (_group-id _query callback &optional _errback _limit)
                   (setq successes (append successes (list callback)))
                   (intern (format "request-%d" (length successes))))))
        (qq-completion--request-members "alice")
        (setq old-view
              (plist-get (gethash "alice" qq-completion--member-pending) :view))
        (appkit-kill-view old-view)
        (setq replacement-view (qq-chat--ensure-view))
        (qq-completion--request-members "alice")
        (should (= (length successes) 2))
        (let ((replacement-owner
               (gethash "alice" qq-completion--member-pending)))
          (should (eq (plist-get replacement-owner :view) replacement-view))
          ;; The old completion cannot populate the cache or remove the newer
          ;; request entry, even when it returns first.
          (funcall (car successes)
                   (list '((user_id . "10002") (nickname . "Old"))))
          (should (eq replacement-owner
                      (gethash "alice" qq-completion--member-pending)))
          (should (eq qq-completion--cache-miss
                      (qq-completion--cached-members "alice")))
          (funcall (cadr successes) (list qq-completion-test--member))
          (should-not (gethash "alice" qq-completion--member-pending))
          (should (equal (qq-completion--cached-members "alice")
                         (list qq-completion-test--member))))))))

(ert-deftest qq-completion-runtime-replacement-invalidates-member-cache ()
  (let ((qq-runtime--accounts (make-hash-table :test #'equal))
        (qq-state--partitions (make-hash-table :test #'equal))
        (qq-state--active-account-id nil)
        (qq-state-change-hook nil)
        (old-member
         '((user_id . "10002") (nickname . "OLD_ACCOUNT_SECRET")))
        (late-old-member
         '((user_id . "10003") (nickname . "OLD_ACCOUNT_SECRET_LATE")))
        (new-member
         '((user_id . "20002") (nickname . "New Account Alice")))
        buffer app-a app-b view-a view-b fingerprint successes requests)
    (unwind-protect
        (progn
          (setq app-a
                (qq-runtime-account-app
                 (qq-runtime-ensure-account "slot-a")))
          (qq-state-select-account "slot-a")
          (qq-state-reset)
          (qq-state-upsert-session
           "group:20001"
           '((type . group) (target-id . "20001") (title . "Group")) nil)
          (setq buffer
                (generate-new-buffer " *qq-completion-runtime-cache-test*"))
          (with-current-buffer buffer
            (qq-chat-mode)
            (qq-runtime-bind-account "slot-a")
            (setq-local qq-chat--session-key "group:20001")
            (appkit-chatbuf-install-prompt "qq> ")
            (setq view-a (qq-chat--ensure-view)
                  fingerprint appkit--view-fingerprint)
            (cl-letf
                (((symbol-function 'qq-core-search-group-members)
                  (lambda (_group-id query callback
                           &optional _errback _limit)
                    (setq successes (append successes (list callback))
                          requests
                          (append requests
                                  (list
                                   (list
                                    (appkit-view-app
                                     (appkit-current-view))
                                    query))))
                    (intern (format "request-%d" (length successes))))))
              ;; Runtime A first caches an account-private member, then leaves
              ;; another same-query callback in flight across shutdown.
              (qq-completion--request-members "alice")
              (funcall (car successes) (list old-member))
              (should (eq qq-completion--member-cache-owner app-a))
              (should (equal (qq-completion--cached-members "alice")
                             (list old-member)))
              (qq-completion--request-members "alice")
              (should (= (length successes) 2))
              (appkit-app-close app-a)
              (should-not (appkit-current-view))

              ;; The replacement account Appkit has the same stable
              ;; fingerprint and reuses this detached chat buffer, but is a
              ;; distinct application incarnation.
              (setq app-b
                    (qq-runtime-account-app
                     (qq-runtime-ensure-account "slot-a"))
                    view-b (qq-chat--ensure-view))
              (should-not (eq view-a view-b))
              (should (equal appkit--view-fingerprint fingerprint))
              (appkit-chatbuf-input-set-text "@alice")
              (goto-char (point-max))
              ;; A's apparent cache hit must be invisible.  The normal CAPF
              ;; path therefore submits B's request immediately.
              (should-not (qq-completion-member-capf))
              (should (= (length successes) 3))
              (should (equal (mapcar #'cadr requests)
                             '("alice" "alice" "alice")))
              (should (eq (caar (last requests)) app-b))
              (should (eq qq-completion--member-cache-owner app-b))
              (should (eq (qq-completion--cached-members "alice")
                          qq-completion--cache-miss))
              (let ((b-owner
                     (gethash "alice" qq-completion--member-pending)))
                ;; A's late callback cannot remove B's owner or populate B's
                ;; replacement table with old-account data.
                (funcall (nth 1 successes) (list late-old-member))
                (should (eq b-owner
                            (gethash "alice"
                                     qq-completion--member-pending)))
                (should (eq (qq-completion--cached-members "alice")
                            qq-completion--cache-miss))
                (funcall (nth 2 successes) (list new-member)))
              (should-not (gethash "alice" qq-completion--member-pending))
              (should (equal (qq-completion--cached-members "alice")
                             (list new-member)))
              (let* ((capf (qq-completion-member-capf))
                     (table (nth 2 capf))
                     (labels (all-completions "@alice" table)))
                (should capf)
                (should (= (length successes) 3))
                (should-not
                 (seq-some
                  (lambda (label)
                    (string-match-p "OLD_ACCOUNT_SECRET" label))
                  labels))))))
      (when (appkit-app-live-p app-a)
        (appkit-app-close app-a))
      (when (appkit-app-live-p app-b)
        (appkit-app-close app-b))
      (qq-runtime-stop-account "slot-a" t)
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (qq-state-reset))))

(ert-deftest qq-completion-replacement-view-rejects-favorite-response ()
  (qq-completion-test-with-group
    (let (success old-view)
      (cl-letf (((symbol-function 'qq-media-ensure-custom-faces)
                 (lambda (callback &optional _errback)
                   (setq success callback)
                   'request-token)))
        (qq-completion--request-custom-faces "fav"))
      (setq old-view (plist-get qq-completion--custom-face-pending :view))
      (appkit-kill-view old-view)
      (should-not (eq old-view (qq-chat--ensure-view)))
      (funcall success nil)
      (should-not qq-completion--custom-face-pending))))

(ert-deftest qq-completion-mode-binds-telega-style-completion-keys ()
  (should (eq (lookup-key qq-chat-mode-map (kbd "TAB")) #'qq-chat-complete))
  (should (eq (lookup-key qq-chat-mode-map (kbd "<tab>")) #'qq-chat-complete))
  (should (eq (lookup-key qq-chat-mode-map (kbd "C-M-i")) #'qq-chat-complete)))

(provide 'qq-completion-test)

;;; qq-completion-test.el ends here
