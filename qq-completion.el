;;; qq-completion.el --- Composer completion for emacs-qq -*- lexical-binding: t; -*-

;; Author: 0WD0 <me@0wd0.com>

;;; Commentary:

;; QQ-specific providers and protocol insertion built on appkit's generic chat
;; completion substrate.  Group members come from `qq-core-search-group-members';
;; faces remain structured send segments.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'appkit-chat-completion)
(require 'appkit-chat-emoji)
(require 'qq-api)
(require 'qq-core)
(require 'qq-directory)
(require 'qq-media)
(require 'qq-state)

(declare-function qq-chat--insert-input-segment-object
                  "qq-chat" (segment &optional visible-label))
(declare-function qq-chat--sync-draft-from-buffer "qq-chat" ())
(declare-function qq-chat--ensure-view "qq-chat" ())
(declare-function qq-chat--captured-view-current-p "qq-chat" (view))

(defvar-local qq-chat--session-key)

(defconst qq-completion--cache-miss (make-symbol "qq-completion-cache-miss"))

(defvar qq-completion--face-history nil
  "History for shared QQ base-face completion readers.")

(defvar qq-completion--large-face-history nil
  "History for explicit QQ large-face selection.")

(defvar qq-completion--reaction-history nil
  "History for combined QQ and Unicode reaction readers.")

(defvar qq-completion--custom-face-history nil
  "History for shared QQ favorite-face completion readers.")

(defvar qq-completion--poke-target-history nil
  "History for strict poke-target candidate readers.")

(defvar qq-completion--poke-search-history nil
  "History for native group-member poke searches.")

(defun qq-completion--capture-view ()
  "Return the exact live AppKit view for the current QQ chat buffer."
  (when (and qq-chat--session-key (derived-mode-p 'qq-chat-mode))
    (let ((view (qq-chat--ensure-view)))
      (and (qq-chat--captured-view-current-p view) view))))

(defun qq-completion--captured-view-current-p
    (buffer session-key view)
  "Return non-nil when BUFFER still owns exact SESSION-KEY VIEW."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (equal qq-chat--session-key session-key)
              (qq-chat--captured-view-current-p view)))))

(defun qq-completion--group-id ()
  "Return current group id, or nil outside a group chat buffer."
  (let ((session (and qq-chat--session-key
                      (qq-state-session qq-chat--session-key))))
    (when (eq (alist-get 'type session) 'group)
      (or (alist-get 'target-id session)
          (qq-state-session-key-target-id qq-chat--session-key)))))

(defun qq-completion--member-token ()
  "Return current group @mention token, or nil."
  (and (qq-completion--group-id)
       (appkit-chat-completion-token-bounds ?@)))

(defun qq-completion-member-token-at-point ()
  "Return unresolved native group @mention token at point, or nil.

Accepted mentions are structured composer objects and therefore never match
this function.  It is intended for submit guards such as
`qq-chat-return-dwim'."
  (qq-completion--member-token))

(defun qq-completion--face-token ()
  "Return the current `/face' composer token, or nil."
  (appkit-chat-completion-token-bounds ?/))

(defun qq-completion--unicode-emoji-token ()
  "Return the current `:emoji:' composer token, or nil."
  (appkit-chat-completion-delimited-token-bounds ?:))

(defun qq-completion--member-field-values (member)
  "Return searchable non-empty values from native MEMBER."
  (seq-filter
   (lambda (value) (and (stringp value) (not (string-empty-p value))))
   (mapcar (lambda (key) (alist-get key member))
           '(card nickname remark qid user_id))))

(defun qq-completion--member-display-name (member)
  "Return preferred visible name for native MEMBER."
  (or (seq-find
       (lambda (value) (and (stringp value) (not (string-empty-p value))))
       (mapcar (lambda (key) (alist-get key member))
               '(card nickname remark qid user_id)))
      "QQ member"))

(defun qq-completion--member-role-label (role)
  "Return compact Chinese label for native member ROLE."
  (pcase role
    ("owner" "群主")
    ("admin" "管理员")
    ("stranger" "陌生人")
    (_ nil)))

(defun qq-completion--member-annotation (member)
  "Return rich completion annotation for native MEMBER."
  (let* ((display (qq-completion--member-display-name member))
         (nickname (alist-get 'nickname member))
         (user-id (alist-get 'user_id member))
         (title (alist-get 'title member))
         (role (qq-completion--member-role-label (alist-get 'role member)))
         (parts
          (delq nil
                (list (and (stringp nickname)
                           (not (string-empty-p nickname))
                           (not (equal nickname display))
                           nickname)
                      role
                      (and (stringp title) (not (string-empty-p title)) title)
                      (and (alist-get 'robot member) "机器人")
                      (and user-id (format "QQ %s" user-id))))))
    (if parts (concat "  " (string-join parts " · ")) "")))

(defun qq-completion--own-media-prefixes (candidates owner)
  "Capture OWNER, its account, and generation in CANDIDATES' image prefixes."
  (mapcar
   (lambda (candidate)
     (let ((prefix (appkit-chat-completion-candidate-prefix candidate)))
       (if (not (functionp prefix))
           candidate
         (let ((owned (copy-appkit-chat-completion-candidate candidate)))
           (setf (appkit-chat-completion-candidate-prefix owned)
                 (qq-media--owned-callback owner qq-media--generation prefix))
           owned))))
   candidates))

(defun qq-completion--read-with-media (reader prompt candidates &rest options)
  "Call READER with a media lifetime independent of the originating chat."
  (let ((qq-media--consumer (make-symbol "qq-completion-picker")))
    (qq-media--owner-state qq-media--consumer)
    (unwind-protect
        (apply reader prompt
               (qq-completion--own-media-prefixes candidates qq-media--consumer)
               options)
      (qq-media-release-consumer qq-media--consumer))))

(defun qq-completion--member-prefix (member)
  "Return avatar prefix for native MEMBER."
  (let ((user-id (alist-get 'user_id member)))
    (if user-id
        (concat (qq-media-avatar-display-string user-id) " ")
      "  ")))

(defun qq-completion--member-candidates (members)
  "Return appkit candidates for native MEMBERS, preserving server order."
  (let ((seen (make-hash-table :test #'equal))
        candidates)
    (dolist (member members)
      (let* ((display (qq-completion--member-display-name member))
             (base (concat "@" display))
             (count (1+ (gethash base seen 0)))
             (user-id (alist-get 'user_id member))
             (label (if (= count 1)
                        base
                      (format "%s · %s" base user-id)))
             (segment
              `((type . "at")
                (data . ((qq . ,user-id) (name . ,display))))))
        (puthash base count seen)
        (push
         (appkit-chat-completion-candidate-create
          :label label
          :value `(:kind member :member ,member :segment ,segment)
          :search-terms (qq-completion--member-field-values member)
          :prefix (lambda (_candidate)
                    (qq-completion--member-prefix member))
          :annotation (lambda (_candidate)
                        (qq-completion--member-annotation member)))
         candidates)))
    (nreverse candidates)))

(defun qq-completion--filter-members (members query)
  "Return MEMBERS locally matching QUERY."
  (let ((needle (downcase (string-trim (or query "")))))
    (if (string-empty-p needle)
        members
      (seq-filter
       (lambda (member)
         (seq-some
          (lambda (value)
            (string-match-p (regexp-quote needle) (downcase value)))
          (qq-completion--member-field-values member)))
       members))))

(defun qq-completion--cached-members (query)
  "Filter the canonical group page for QUERY, or return a cache miss."
  (let ((entry (qq-runtime-operation (list 'group-members (qq-completion--group-id)))))
    (if (plist-get entry :ready)
        (qq-core--filter-members (alist-get 'members (plist-get entry :value)) query 200)
      qq-completion--cache-miss)))

(defun qq-completion--member-fallback (query)
  "Return locally filtered broad member cache for QUERY."
  (let ((broad (qq-completion--cached-members "")))
    (unless (eq broad qq-completion--cache-miss)
      (qq-completion--filter-members broad query))))

(defun qq-completion-member-capf ()
  "CAPF for native QQ group @mentions."
  (when-let* ((token (qq-completion--member-token)))
    (let* ((query (plist-get token :query))
           (cached (qq-completion--cached-members query))
           (members (if (eq cached qq-completion--cache-miss)
                        (qq-completion--member-fallback query)
                      cached)))
      (when (eq cached qq-completion--cache-miss)
        (qq-runtime-request (list 'group-members (qq-completion--group-id))))
      (when members
        (appkit-chat-completion-capf
         (plist-get token :start)
         (plist-get token :end)
         (qq-completion--member-candidates members)
         :insert-function #'qq-completion--insert-protocol-candidate
         :suffix " "
         :sync-function #'qq-chat--sync-draft-from-buffer)))))

(defun qq-completion--poke-user-id-p (value)
  "Return non-nil when VALUE is a canonical nonzero poke user id."
  (and (qq-protocol-user-uin-p value)
       (not (equal value "0"))))

(defun qq-completion--poke-session-current-p
    (buffer session-key type target-id &optional view)
  "Return non-nil when BUFFER still owns canonical fields and optional VIEW."
  (and (if view
           (qq-completion--captured-view-current-p buffer session-key view)
         (buffer-live-p buffer))
       (with-current-buffer buffer
         (let ((session (qq-state-session session-key)))
           (and (derived-mode-p 'qq-chat-mode)
                (equal qq-chat--session-key session-key)
                (eq (alist-get 'type session) type)
                (equal (alist-get 'target-id session) target-id))))))

(defun qq-completion--poke-candidate-user-id (candidate)
  "Return canonical original user id carried by CANDIDATE."
  (let* ((value (appkit-chat-completion-candidate-value candidate))
         (member (plist-get value :member))
         (user-id (or (plist-get value :user-id)
                      (and (listp member) (alist-get 'user_id member)))))
    (unless (qq-completion--poke-user-id-p user-id)
      (error "qq: poke target candidate has invalid user id: %S" user-id))
    user-id))

(defun qq-completion--private-poke-candidate (user-id name relation)
  "Return a rich private poke candidate for USER-ID, NAME, and RELATION."
  (unless (qq-completion--poke-user-id-p user-id)
    (error "qq: private poke candidate has invalid user id: %S" user-id))
  (let ((user-id user-id)
        (name name)
        (relation relation))
    (appkit-chat-completion-candidate-create
     :label (format "%s · %s" relation name)
     :value `(:kind poke-target :user-id ,user-id)
     :search-terms (delete-dups (list relation name user-id))
     :prefix (lambda (_candidate)
               (concat (qq-media-avatar-display-string user-id) " "))
     :annotation (format "  QQ %s" user-id))))

(defun qq-completion--private-poke-candidates (session)
  "Return the strict peer/self poke candidates for private SESSION."
  (let* ((peer-id (alist-get 'target-id session))
         (self-info (qq-state-self-info))
         (self-id (qq-state-self-user-id))
         (peer-name (or (alist-get 'title session) peer-id))
         (self-name (or (alist-get 'nickname self-info) self-id)))
    (unless (qq-completion--poke-user-id-p peer-id)
      (user-error "qq: private poke requires a canonical peer id"))
    (unless (qq-completion--poke-user-id-p self-id)
      (user-error "qq: private poke requires loaded self identity"))
    (if (equal peer-id self-id)
        (list (qq-completion--private-poke-candidate
               self-id self-name "Me"))
      (list (qq-completion--private-poke-candidate
             peer-id peer-name "Peer")
            (qq-completion--private-poke-candidate
             self-id self-name "Me")))))

(defun qq-completion--read-private-poke-target
    (buffer session-key session callback initial-user-id view)
  "Read a strict private poke target and call CALLBACK with its user id."
  (let* ((candidates (qq-completion--private-poke-candidates session))
         (initial-candidate
          (and initial-user-id
               (seq-find
                (lambda (candidate)
                  (equal (qq-completion--poke-candidate-user-id candidate)
                         initial-user-id))
                candidates))))
    (when (and initial-user-id (null initial-candidate))
      (user-error "qq: private poke target must be the peer or self"))
    (let ((candidate
           (qq-completion--read-with-media
            #'appkit-chat-completion-read
            "Poke target: " candidates
            :history 'qq-completion--poke-target-history
            :initial-input
            (and initial-candidate
                 (appkit-chat-completion-candidate-label initial-candidate)))))
      (unless (qq-completion--poke-session-current-p
               buffer session-key 'private (alist-get 'target-id session) view)
        (user-error "qq: poke target belongs to a stale chat session"))
      (funcall callback
               (qq-completion--poke-candidate-user-id candidate)))))

(defun qq-completion--group-poke-candidates (members)
  "Return strict rich poke candidates for native group MEMBERS."
  (dolist (member members)
    (unless (qq-completion--poke-user-id-p (alist-get 'user_id member))
      (error "qq: group poke search returned invalid user id: %S"
             (alist-get 'user_id member))))
  (qq-completion--member-candidates members))

(defun qq-completion-read-poke-target
    (session-key callback &optional initial-user-id)
  "Choose a strict poke target for SESSION-KEY, then call CALLBACK.

Private chats synchronously choose between the canonical peer and self.
Group chats filter the App's shared member page.  If it is still loading,
invoke this command again to choose a target explicitly.
INITIAL-USER-ID, when non-nil, must be an original decimal string and seeds the
private selection or native group search.  No typed text is ever accepted as a
target without a matching canonical candidate."
  (unless (functionp callback)
    (error "qq: poke target callback must be a function"))
  (when (and initial-user-id
             (not (qq-completion--poke-user-id-p initial-user-id)))
    (user-error "qq: initial poke target must be an original QQ string"))
  (let* ((buffer (current-buffer))
         (session (qq-state-session session-key))
         (type (and session (alist-get 'type session)))
         (view (qq-completion--capture-view)))
    (unless (and session
                 (derived-mode-p 'qq-chat-mode)
                 (equal qq-chat--session-key session-key))
      (user-error "qq: poke target reader requires the current chat session"))
    (unless view
      (user-error "qq: poke target reader requires a live chat view"))
    (pcase type
      ('private
       (qq-completion--read-private-poke-target
        buffer session-key session callback initial-user-id view))
      ('group
       (let* ((group-id (alist-get 'target-id session))
              (entry (qq-runtime-operation (list 'group-members group-id))))
         (unless qq-completion--poke-query
           (setq qq-completion--poke-query
                 (string-trim (read-string "Search group member: " initial-user-id
                                           'qq-completion--poke-search-history))))
         (when (string-empty-p qq-completion--poke-query)
           (setq qq-completion--poke-query nil)
           (user-error "qq: group poke requires a non-empty member search"))
         (cond
          ((plist-get entry :pending) (message "qq: group members are loading"))
          ((plist-get entry :ready)
           (let ((members (qq-core--filter-members
                           (alist-get 'members (plist-get entry :value))
                           qq-completion--poke-query 200)))
             (setq qq-completion--poke-query nil)
             (if (null members) (message "qq: no matching group member")
               (let* ((candidate
                       (qq-completion--read-with-media
                        #'appkit-chat-completion-read "Poke group member: "
                        (qq-completion--group-poke-candidates members)
                        :history 'qq-completion--poke-target-history))
                      (user-id (qq-completion--poke-candidate-user-id candidate)))
                 (when (qq-completion--captured-view-current-p buffer session-key view)
                   (funcall callback user-id))))))
          (t (qq-runtime-request (list 'group-members group-id))
             (message "qq: loading group members; invoke again to choose a target")))))
      (_
       (user-error "qq: poke target selection is unsupported for %s sessions"
                   type)))))

(defun qq-completion--base-face-candidates (&optional large-p)
  "Return shared system face candidates, selecting large faces with LARGE-P."
  (mapcar
   (lambda (label)
     (let ((id (qq-media-face-id-from-completion label)))
       (appkit-chat-completion-candidate-create
        :label label
        :value `(:kind ,(if large-p 'large-face 'base-face) :face-id ,id)
        :search-terms (list id (or (qq-media-face-name id) ""))
        :prefix (lambda (_candidate)
                  (qq-media--face-completion-prefix id)))))
   (qq-media-face-completion-candidates large-p)))

(defconst qq-completion--custom-face-query-prefixes
  '("fav" "favorite" "sticker" "收藏" "表情包")
  "Explicit `/PREFIX' values that open visual favorite-face completion.")

(defun qq-completion--custom-face-token ()
  "Return the current explicit `/fav' favorite-face token, or nil."
  (when-let* ((token (qq-completion--face-token))
              (query (downcase (plist-get token :query)))
              ((seq-some (lambda (prefix)
                           (string-prefix-p prefix query))
                         qq-completion--custom-face-query-prefixes)))
    token))

(defun qq-completion--candidate-matches-token-query-p (candidate token)
  "Return non-nil when CANDIDATE can satisfy TOKEN's current query.

The match deliberately includes protocol candidate search terms, so numeric QQ
face ids and normalized Unicode names remain actionable while arbitrary paths,
URLs, colon prose, and emoticons do not capture RET."
  (let* ((query (downcase (or (plist-get token :query) "")))
         (terms (appkit-chat-completion-candidate-search-terms candidate))
         (values
          (cons (appkit-chat-completion-candidate-label candidate)
                (cond
                 ((stringp terms) (list terms))
                 ((listp terms) terms)
                 (t nil)))))
    (seq-some
     (lambda (value)
       (and (stringp value)
            (string-match-p (regexp-quote query) (downcase value))))
     values)))

(defun qq-completion-token-at-point ()
  "Return the unresolved QQ composer token at point, or nil.

The result includes a `:kind' of `member', `favorite-face', `face', or
`unicode-emoji'.  Submit dispatchers use syntax ownership rather than frontend
popup state, so a completion with no preselected candidate cannot fall through
and send literal token text accidentally."
  (let (token kind)
    (cond
     ((setq token (qq-completion--member-token))
      (setq kind 'member))
     ((setq token (qq-completion--custom-face-token))
      (setq kind 'favorite-face))
     ((and (setq token (qq-completion--face-token))
           (seq-some
            (lambda (candidate)
              (qq-completion--candidate-matches-token-query-p candidate token))
            (qq-completion--base-face-candidates)))
      (setq kind 'face))
     ((and (setq token (qq-completion--unicode-emoji-token))
           (seq-some
            (lambda (candidate)
              (qq-completion--candidate-matches-token-query-p candidate token))
            (appkit-chat-emoji-candidates)))
      (setq kind 'unicode-emoji)))
    (when (and token kind)
      (plist-put token :kind kind))))

(defun qq-completion--custom-faces-loaded-p ()
  "Return whether the account App has a favorite catalog."
  (plist-get (qq-runtime-operation '(favorite-emojis)) :ready))

(defun qq-completion--favorite-search-query (input)
  "Strip the longest favorite selector from INPUT before matching annotations."
  (let ((raw (string-remove-prefix "/" input))
        (prefix-length 0))
    (dolist (prefix qq-completion--custom-face-query-prefixes)
      (when (string-prefix-p prefix (downcase raw))
        (setq prefix-length (max prefix-length (length prefix)))))
    (if (> prefix-length 0)
        (string-trim-left (substring raw prefix-length))
      input)))

(defun qq-completion-face-capf ()
  "CAPF for `/名称' base faces and explicit `/fav' favorite faces."
  (when-let* ((token (qq-completion--face-token)))
    (let* ((custom-p (qq-completion--custom-face-token))
           (query (plist-get token :query))
           (candidates
            (if custom-p
                (when (qq-completion--custom-faces-loaded-p)
                  (qq-completion--custom-face-candidates
                   (alist-get 'entries (plist-get (qq-runtime-operation '(favorite-emojis)) :value))))
              (qq-completion--base-face-candidates))))
      (when (and custom-p (not (qq-completion--custom-faces-loaded-p)))
        (qq-runtime-request '(favorite-emojis)))
      (when candidates
        (let ((capf
               (appkit-chat-completion-capf
                (plist-get token :start)
                (plist-get token :end)
                candidates
                :insert-function #'qq-completion--insert-protocol-candidate
                :sync-function #'qq-chat--sync-draft-from-buffer)))
          (when custom-p
            (let ((table (nth 2 capf)))
              (setf (nth 2 capf)
                    (lambda (input predicate action)
                      (let ((result
                             (funcall table
                                      (qq-completion--favorite-search-query input)
                                      predicate action)))
                        ;; A presentation-label prefix is not an accepted face.
                        ;; Retain the selector so native CAPF ownership survives.
                        (if (and (null action) (stringp result)
                                 (not (test-completion result table predicate)))
                            input
                          result))))))
          capf)))))

(defun qq-completion-unicode-emoji-capf ()
  "CAPF for shared standard Unicode `:emoji_name:' completion."
  (appkit-chat-emoji-capf
   :sync-function #'qq-chat--sync-draft-from-buffer))

(defun qq-completion--custom-face-candidates (faces)
  "Return shared completion candidates for favorite FACES."
  (cl-loop for face in (seq-filter #'qq-media-custom-face-sendable-p faces)
           for index from 0
           collect
           (let ((current (copy-tree face)))
             (appkit-chat-completion-candidate-create
              :label (qq-media-custom-face-label current index)
              :value `(:kind custom-face :face ,current)
              :prefix (lambda (_candidate)
                        (qq-media--custom-face-completion-prefix current))
              :search-terms
              (append qq-completion--custom-face-query-prefixes
                      (list (alist-get 'description current)
                            (alist-get 'md5 current)
                            (alist-get 'favorite_emoji_id current)))))))

(defun qq-completion-read-base-face-id (&optional prompt)
  "Read a small QQ system face and return its string id."
  (let* ((candidate
          (qq-completion--read-with-media
           #'appkit-chat-completion-read-visual
           (or prompt "QQ face: ")
           (qq-completion--base-face-candidates)
           :history 'qq-completion--face-history))
         (value (appkit-chat-completion-candidate-value candidate)))
    (plist-get value :face-id)))

(defun qq-completion--subscribe-large-faces (buffer session-key view publish)
  "Publish BUFFER's large-face catalog and previews while its picker is open."
  (let* ((owner qq-media--consumer)
         (state (with-current-buffer buffer (qq-media--owner-state owner)))
         (account (plist-get state :account))
         (generation qq-media--generation)
         candidates)
    (cl-labels
        ((current-p ()
           (and (= generation qq-media--generation)
                (eq state (gethash owner qq-media--consumers))))
         (update (changed-account &optional reason)
           (when (and (equal changed-account account) (current-p))
             (cond
              ((not (qq-completion--captured-view-current-p buffer session-key view))
               (funcall publish nil "Chat closed or replaced"))
              (reason
               (setq candidates nil)
               (funcall publish nil (format "Failed to load large faces: %s" reason)))
              ((gethash account qq-media--system-emoji-tables)
               (let ((qq-media--consumer owner)
                     (qq-runtime--context-account-id account))
                 (setq candidates
                       (qq-completion--own-media-prefixes
                        (qq-completion--base-face-candidates t) owner)))
               (funcall publish candidates
                        (unless candidates "No large faces available")))
              (t (funcall publish nil "Loading large faces…")))))
         (preview-updated (key)
           (when (and (current-p) candidates
                      (or (null key) (string-prefix-p "face:" key))
                      (qq-completion--captured-view-current-p buffer session-key view))
             (funcall publish candidates))))
      (add-hook 'qq-media-system-emoji-catalog-hook #'update)
      (add-hook 'qq-media-cache-update-hook #'preview-updated)
      (with-current-buffer buffer
        (if (or (gethash account qq-media--system-emoji-tables)
                (and (qq-server-ready-p) (qq-account-get account)
                     (qq-rpc-method-available-p "system_emoji.list")))
            (progn
              (update account)
              (qq-media--ensure-system-emoji-catalog))
          (update account "Connect the account to load its catalog")))
      (lambda ()
        (remove-hook 'qq-media-system-emoji-catalog-hook #'update)
        (remove-hook 'qq-media-cache-update-hook #'preview-updated)))))

(defun qq-completion-read-large-face-id ()
  "Read a large face from the current account's asynchronously loaded catalog."
  (let* ((buffer (current-buffer))
         (session-key qq-chat--session-key)
         (view (or (qq-completion--capture-view)
                   (user-error "qq: large faces require a live chat view")))
         (candidate
          (qq-completion--read-with-media
           #'appkit-chat-completion-read-visual
           "QQ large face: " nil
           :history 'qq-completion--large-face-history
           :subscribe
           (lambda (publish)
             (qq-completion--subscribe-large-faces buffer session-key view publish)))))
    (plist-get (appkit-chat-completion-candidate-value candidate) :face-id)))

(defun qq-completion-read-reaction ()
  "Read and return one normalized QQ face or Unicode reaction."
  (let* ((unicode-candidates
          (seq-filter
           (lambda (candidate)
             (let* ((value
                     (appkit-chat-completion-candidate-value candidate))
                    (emoji (plist-get value :emoji)))
               (and (stringp emoji) (= (length emoji) 1))))
           (appkit-chat-emoji-candidates)))
         (candidate
          (qq-completion--read-with-media
           #'appkit-chat-completion-read-visual
           "Reaction: "
           (append (qq-completion--base-face-candidates)
                   unicode-candidates)
           :history 'qq-completion--reaction-history))
         (value (appkit-chat-completion-candidate-value candidate)))
    (pcase (plist-get value :kind)
      ('base-face
       `((emoji-id . ,(plist-get value :face-id)) (emoji-type . "1")))
      ('unicode-emoji
       (let ((emoji (plist-get value :emoji)))
         `((emoji-id . ,(number-to-string (aref emoji 0)))
           (emoji-type . "2"))))
      (_ (error "qq: reaction picker returned an unsupported candidate")))))

(defun qq-completion--subscribe-custom-faces
    (buffer session-key view force-refresh publish)
  "Publish BUFFER's favorite catalog and preview changes to the open picker."
  (let* ((owner (or qq-media--consumer
                    (make-symbol "qq-completion-picker")))
         (state (with-current-buffer buffer
                  (qq-media--owner-state owner)))
         (generation qq-media--generation)
         candidates)
    (cl-labels
        ((update (&optional _key)
           (cond
            ((or (/= generation qq-media--generation)
                 (not (eq state (gethash owner qq-media--consumers))))
             nil)
            ((not (qq-completion--captured-view-current-p
                   buffer session-key view))
             (funcall publish nil "Chat closed or replaced"))
            ((plist-get (qq-runtime-operation '(favorite-emojis) (plist-get state :account)) :error)
             (funcall publish nil
                      (format "Failed to load favorites: %s"
                              (plist-get (qq-runtime-operation '(favorite-emojis) (plist-get state :account)) :error))))
            ((with-current-buffer buffer
               (qq-completion--custom-faces-loaded-p))
             (let ((qq-media--consumer owner)
                   (qq-runtime--context-account-id (plist-get state :account)))
               (setq candidates
                     (qq-completion--own-media-prefixes
                      (qq-completion--custom-face-candidates
                       (alist-get 'entries (plist-get (qq-runtime-operation '(favorite-emojis)) :value)))
                      owner)))
             (funcall publish candidates
                      (unless candidates "No favorite faces (收藏表情为空)")))
            (t
             (setq candidates nil)
             (funcall publish nil "Loading favorite faces…"))))
         (preview-updated (key)
           (when (and (= generation qq-media--generation)
                      (eq state (gethash owner qq-media--consumers))
                      candidates
                      (or (null key) (string-prefix-p "favorite-emoji:" key)))
             (when (qq-completion--captured-view-current-p
                    buffer session-key view)
               (funcall publish candidates)))))
      (with-current-buffer buffer
        (update
         (when (or force-refresh (not (qq-completion--custom-faces-loaded-p)))
           (qq-runtime-request '(favorite-emojis) force-refresh)))
        (add-hook 'qq-runtime-data-hook #'update nil t))
      (add-hook 'qq-media-cache-update-hook #'preview-updated)
      (lambda ()
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (remove-hook 'qq-runtime-data-hook #'update t)))
        (remove-hook 'qq-media-cache-update-hook #'preview-updated)
        (qq-media-release-consumer owner)))))

(defun qq-completion-read-custom-face (&optional force-refresh)
  "Choose a favorite face while its catalog and previews load asynchronously.

With FORCE-REFRESH, replace the shared `/fav' catalog before selecting."
  (let* ((buffer (current-buffer))
         (session-key qq-chat--session-key)
         (view (or (qq-completion--capture-view)
                   (user-error "qq: favorite faces require a live chat view")))
         (candidate
          (qq-completion--read-with-media
           #'appkit-chat-completion-read-visual
           "Favorite face: " nil
           :history 'qq-completion--custom-face-history
           :subscribe
           (lambda (publish)
             (qq-completion--subscribe-custom-faces
              buffer session-key view force-refresh publish))))
         (value (appkit-chat-completion-candidate-value candidate)))
    (plist-get value :face)))

(defun qq-completion--insert-protocol-candidate (candidate)
  "Materialize and insert the protocol segment selected by appkit CANDIDATE."
  (let* ((qq-media--consumer (or (qq-completion--capture-view) (current-buffer)))
         (value (appkit-chat-completion-candidate-value candidate))
         (kind (plist-get value :kind))
         (face (and (eq kind 'custom-face) (plist-get value :face)))
         (segment (pcase kind
                    ('base-face
                     (qq-media-face-to-segment (plist-get value :face-id)))
                    ('large-face
                     (qq-media-large-face-to-segment (plist-get value :face-id)))
                    ('custom-face
                     (qq-media-custom-face-to-segment face))
                    (_ (plist-get value :segment))))
         (visible-label
          (when face (qq-media-custom-face-display-string face))))
    (unless segment
      (error "qq: completion candidate has no protocol segment"))
    (qq-chat--insert-input-segment-object segment visible-label)))

(defun qq-completion-complete ()
  "Complete the current QQ composer token.

Cold native member searches update only the model.  Invoke completion again
after the model is ready to present candidates."
  (interactive)
  (when (appkit-chatbuf-point-in-input-p)
    (cond
     ((and (qq-completion--custom-face-token)
           (not (qq-completion--custom-faces-loaded-p)))
      (qq-runtime-request '(favorite-emojis))
      (message "qq: loading favorite faces…")
      t)
     ((and (qq-completion--custom-face-token)
           (qq-completion--custom-faces-loaded-p)
           (null (alist-get 'entries (plist-get (qq-runtime-operation '(favorite-emojis)) :value))))
      (message "qq: favorite faces are empty")
      t)
     ((if-let* ((token (qq-completion--member-token))
                (query (plist-get token :query))
                ((eq (qq-completion--cached-members query)
                     qq-completion--cache-miss))
                ((null (qq-completion--member-fallback query))))
          (progn
            (qq-runtime-request (list 'group-members (qq-completion--group-id)))
            (message "qq: loading matching group members…")
            t)
        (appkit-chat-completion-complete))))))

(defun qq-completion-preload-members ()
  "Warm the shared group directory for headings and composer completion."
  (when-let* ((group-id (qq-completion--group-id)))
    (qq-directory-ensure-group-members group-id)))

(defun qq-completion-setup ()
  "Initialize QQ composer completion without per-buffer account caches."
  (setq-local qq-completion--poke-query nil)
  (appkit-chat-completion-setup
   :capf-functions '(qq-completion-member-capf qq-completion-face-capf
                     qq-completion-unicode-emoji-capf)
   :append t))

(defvar-local qq-completion--poke-query nil
  "Member search text retained until the user explicitly chooses a poke target.")

(provide 'qq-completion)

;;; qq-completion.el ends here
