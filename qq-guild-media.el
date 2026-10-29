;;; qq-guild-media.el --- Forum rich text and owned media -*- lexical-binding: t; -*-
;;; Code:
(require 'qq-guild)
(require 'qq-media)
(require 'appkit-resource)
(require 'appkit-media-effect)
(require 'appkit-media-grid)

(defun qq-guild-media--key (state model identity)
  "Scope IDENTITY to this committed account and native session."
  (list 'qq-guild-media (plist-get model :account)
        (qq-server-gateway-instance-id) (plist-get state :epoch) identity))

(defun qq-guild-media-plan (state model item)
  "Prepare ITEM's media and emoji specifications without starting work."
  (when (plist-get item :body-visible-p)
    (let* ((value (plist-get item :value))
           (scope (or (alist-get 'identity value)
                      (list (plist-get model :target) (plist-get item :key))))
           (media
            (cl-loop for entry in (alist-get 'media value) for index from 0
                     collect
                     (let* ((id (alist-get 'id entry))
                            (key (qq-guild-media--key
                                  state model
                                  (list scope (alist-get 'kind entry)
                                        (if (string-empty-p id) index id)))))
                       (list :key key :index index :media entry
                             :source (or (alist-get 'preview_url entry)
                                         (and (equal (alist-get 'kind entry) "image")
                                              (alist-get 'url entry)))))))
           (emoji
            (cl-loop for entry in (alist-get 'content value)
                     when (equal (alist-get 'kind entry) "emoji")
                     collect (list :key (qq-guild-media--key
                                         state model (list 'emoji (alist-get 'url entry)))
                                   :emoji entry :source (alist-get 'url entry)))))
      (list :media media :emoji emoji))))

(defun qq-guild-media--load (context input resolve reject)
  "Acquire INPUT's preview, falling back to the original for opaque CDN formats.
Some QPic thumbnail URLs return AVIF even when JPEG is requested.  Keep
those bytes out of the renderer and acquire the original image instead."
  (let (cancellations cancelled)
    (cl-labels
        ((fetch (acquisition fallback)
           (unless cancelled
             (let ((cancellation
                    (appkit-media-image-resource-load
                     context acquisition
                     (lambda (file)
                       (unless cancelled
                         (if (image-type-from-file-header file)
                             (funcall resolve file)
                           (if fallback (fetch fallback nil)
                             (funcall reject "图片格式暂不能预览")))))
                     (lambda (reason)
                       (unless cancelled
                         (if fallback (fetch fallback nil) (funcall reject reason)))))))
               (when cancellation (push cancellation cancellations))))))
      (fetch (plist-get input :preview) (plist-get input :original))
      (appkit-cancellation-create
       :kind 'logical
       :cancel (lambda ()
                 (setq cancelled t)
                 (dolist (cancellation cancellations)
                   (funcall (appkit-cancellation-cancel cancellation))))))))

(defun qq-guild-media-demand (spec)
  "Create a cancellable Appkit image demand for SPEC's preview."
  (when-let* ((url (plist-get spec :source)))
    (let* ((key (plist-get spec :key))
           (cache-id (list (nth 1 key) (nth 4 key)))
           (base (expand-file-name
                  (secure-hash 'sha256 (prin1-to-string cache-id))
                  (expand-file-name "qq/guild-media/" user-emacs-directory)))
           (media (plist-get spec :media))
           (original (and (equal (alist-get 'kind media) "image")
                          (not (equal url (alist-get 'url media)))
                          (alist-get 'url media)))
           (input (list :preview
                        (appkit-media-image-acquisition-create
                         (appkit-media-resource-create :url url) base)
                        :original
                        (when original
                          (appkit-media-image-acquisition-create
                           (appkit-media-resource-create :url original)
                           (concat base "-original"))))))
      (appkit-resource-demand-create
       :key key :input input :loader #'qq-guild-media--load
       :acquisition-identity (list 'supported-image key url original)
       :sharing-policy 'app-private :cache-policy 'while-interested))))


(defun qq-guild-media--file (surface key)
  "Read a prepared file for KEY without acquiring it."
  (when-let* ((state (appkit-resource-state surface key))
              ((eq (appkit-resource-state-status state) 'ready)))
    (appkit-resource-state-value state)))

(defun qq-guild-media--emoji-label (entry)
  "Return accessible text for one canonical emoji ENTRY."
  (let ((name (alist-get 'name entry)))
    (if (string-empty-p name)
        (qq-media-face-text-fallback (alist-get 'id entry))
      (format "[%s]" name))))

(defun qq-guild-media-insert-text (surface value specs)
  "Insert VALUE's ordered rich text using prepared emoji SPECS."
  (if (null (alist-get 'content value))
      (insert (alist-get 'text value))
    (dolist (entry (alist-get 'content value))
      (pcase (alist-get 'kind entry)
        ("text" (insert (alist-get 'text entry)))
        ("mention" (insert (propertize (alist-get 'text entry) 'face 'font-lock-constant-face)))
        ("link"
         (let ((start (point)) (url (alist-get 'url entry)))
           (insert (propertize (alist-get 'text entry) 'face 'link))
           (appkit-ui-add-fallback-action
            start (point) (lambda () (browse-url url)) :help-echo url)))
        ("emoji"
         (let* ((spec (cl-find (alist-get 'url entry) specs :test #'equal
                               :key (lambda (s) (plist-get s :source))))
                (file (and spec (qq-guild-media--file surface (plist-get spec :key))))
                (image (and file (appkit-media-one-line-preview-image-from-file
                                  file (appkit-media--base-char-pixel-height) 'contain)))
                (label (qq-guild-media--emoji-label entry)))
           (if image
               (insert (appkit-media-one-line-image-display-string image label))
             (insert label))))))))

(defun qq-guild-media--resource (entry)
  "Create the canonical full media resource for ENTRY."
  (appkit-media-resource-create
   :url (alist-get 'url entry)
   :name (if (equal (alist-get 'kind entry) "video") "频道视频" "频道图片")))

(defun qq-guild-media--cell (surface spec size single)
  "Prepare one media cell from SPEC at SIZE pixels for SURFACE."
  (let* ((entry (plist-get spec :media))
         (key (plist-get spec :key))
         (kind (if (equal (alist-get 'kind entry) "video") 'video 'image))
         (file (qq-guild-media--file surface key))
         (state (appkit-resource-state surface key))
         (status (and state (appkit-resource-state-status state)))
         (resource (qq-guild-media--resource entry))
         (label (if (eq kind 'video)
                    (format "▶ 视频 · %d 秒" (alist-get 'duration entry)) "图片"))
         (poster (and file
                      (if single (appkit-media-preview-image-from-file file size 260)
                        (or (appkit-media-cropped-preview-image-from-file file size size)
                            (appkit-media-preview-image-from-file file size size)))))
         (image (appkit-media-image-with-resource
                 poster resource :kind kind :label label
                 :interaction (if (eq kind 'video) 'media 'open)
                 :autoplay nil :cache-key key
                 :slot-key (list appkit-projection-printing-key key (plist-get spec :index)))))
    (appkit-media-grid-cell-create
     :image image
     :fallback
     (propertize
      (format "[%s%s]" label
              (cond ((eq status 'failed) " · 预览加载失败")
                    ((eq status 'ready) " · 预览不可用")
                    ((and (plist-get spec :source) (display-graphic-p)) " · 加载中")
                    (t "")))
      'face 'shadow)
     :action (lambda () (qq-media-open-resource resource kind key :owner surface))
     :help-echo (if (eq kind 'video) "播放视频" "查看原图")
     :properties (list 'qq-guild-media-item entry))))

(defun qq-guild-media-insert (surface specs width)
  "Insert prepared media SPECS in responsive Appkit bands within WIDTH columns."
  (when specs
    (unless (bolp) (insert "\n"))
    (let* ((count (length specs))
           (columns (min count (max 1 (min 3 (/ (max 1 width) 18)))))
           (pixels (* (max 1 (- width 2)) (appkit-media--base-char-pixel-width)))
           (size (max 24 (min (if (= count 1) 420 160)
                              (/ (- pixels (* 6 (1- columns))) columns))))
           (cells (mapcar (lambda (spec)
                            (qq-guild-media--cell surface spec size (= count 1))) specs))
           (indices (number-sequence 0 (1- count)))
           bands)
      (while indices
        (push (seq-take indices columns) bands)
        (setq indices (nthcdr (min columns (length indices)) indices)))
      (appkit-media-insert-grid cells :bands (nreverse bands) :gap 6))))

(provide 'qq-guild-media)
;;; qq-guild-media.el ends here
