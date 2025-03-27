;;; chan-mode.el --- Read 4chan boards as feeds in Emacs -*- lexical-binding: t -*-

;; Author: Blackdream <grim@samhain.su>
;; Version: 1.2
;; Package-Requires: ((emacs "30.1") (cl-lib "0.5"))
;; Keywords: comm, multimedia
;; URL: https://github.com/theesfeld/chan-mode

;;; Commentary:
;; This package provides a read-only interface to 4chan boards using the 4chan API.
;; Features: catalog view with OP images only, thread view with async image loading,
;; navigation, expandable images, and an image-only view mode. No external dependencies.

;;; Code:

(require 'url)
(require 'json)
(require 'shr)
(require 'cl-lib)

(defgroup chan-mode nil
  "Customization group for chan-mode."
  :group 'comm)

(defcustom chan-base-url "https://a.4cdn.org/"
  "Base URL for 4chan API."
  :type 'string
  :group 'chan-mode)

(defcustom chan-image-base-url "https://i.4cdn.org/"
  "Base URL for 4chan images."
  :type 'string
  :group 'chan-mode)

(defvar chan--boards-cache nil
  "Cache of available boards.")
(defvar chan--last-request-time 0
  "Timestamp of last API request to enforce rate limiting.")
(defvar chan--current-board nil
  "Current board being viewed.")
(defvar chan--image-queue nil
  "Queue of (buffer . (pos . url)) for async image fetches.")
(defvar chan--image-timer nil
  "Timer for processing image queue.")

;;; Utility Functions
(defun chan--rate-limit-wait ()
  "Ensure at least 1 second between API requests."
  (let ((elapsed (- (float-time) chan--last-request-time)))
    (when (< elapsed 1.0)
      (sleep-for (- 1.0 elapsed)))
    (setq chan--last-request-time (float-time))))

(defun chan--fetch-json (url)
  "Fetch and parse JSON from URL synchronously."
  (chan--rate-limit-wait)
  (with-temp-buffer
    (condition-case err
        (progn
          (url-insert-file-contents url)
          (let ((json-object-type 'alist)
                (json-array-type 'list)
                (json-key-type 'symbol))
            (json-read)))
      (error (message "Error fetching JSON: %s" err) nil))))

(defun chan--get-boards ()
  "Fetch list of boards, caching result."
  (or chan--boards-cache
      (setq chan--boards-cache
            (alist-get
             'boards
             (chan--fetch-json
              (concat chan-base-url "boards.json"))))))

(defun chan--image-url (board tim ext)
  "Construct image URL from BOARD, TIM, and EXT."
  (when (and tim ext)
    (concat
     chan-image-base-url board "/" (number-to-string tim) ext)))

(defun chan--fetch-image-sync (url &optional full-size)
  "Fetch image from URL synchronously with timeout."
  (when url
    (chan--rate-limit-wait)
    (let ((buffer (get-buffer-create "*chan-image-fetch*")))
      (with-current-buffer buffer
        (erase-buffer)
        (condition-case err
            (progn
              (url-retrieve-synchronously url t t 5) ; 5-second timeout
              (goto-char (point-min))
              (when (re-search-forward "\r?\n\r?\n" nil t)
                (create-image (buffer-substring-no-properties
                               (point) (point-max))
                              nil t
                              :max-width
                              (if full-size
                                  nil
                                200))))
          (error
           (message "Failed to fetch image %s: %s" url err) nil))
        (kill-buffer buffer)))))

(defun chan--process-image-queue ()
  "Process one image from the queue asynchronously."
  (when chan--image-queue
    (let* ((task (pop chan--image-queue))
           (buffer (car task))
           (pos (car (cdr task)))
           (url (cdr (cdr task))))
      (chan--rate-limit-wait)
      (url-retrieve
       url
       (lambda (status &rest args)
         (when (and (not status) (buffer-live-p buffer))
           (with-current-buffer buffer
             (goto-char (point-min))
             (when (re-search-forward "\r?\n\r?\n" nil t)
               (let ((inhibit-read-only t)
                     (image
                      (create-image (buffer-substring-no-properties
                                     (point) (point-max))
                                    nil t
                                    :max-width 200)))
                 (save-excursion
                   (goto-char pos)
                   (delete-char 1)
                   (insert-image image))))))
         (kill-buffer))
       nil t t))
    (if chan--image-queue
        (setq chan--image-timer
              (run-at-time 1 nil #'chan--process-image-queue))
      (setq chan--image-timer nil))))

;;; Catalog View
(defvar chan-catalog-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'chan-catalog-open-thread)
    (define-key map (kbd "b") #'chan-catalog)
    map)
  "Keymap for `chan-catalog-mode'.")

(defun chan-catalog (board)
  "Display catalog for BOARD with OP images only."
  (interactive (list
                (completing-read
                 "Board: "
                 (mapcar
                  (lambda (b)
                    (alist-get 'board b))
                  (chan--get-boards)))))
  (setq chan--current-board board)
  (let ((catalog
         (chan--fetch-json
          (concat chan-base-url board "/catalog.json")))
        (buffer
         (get-buffer-create (format "*chan-catalog-%s*" board))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (chan-catalog-mode)
        (insert
         (propertize (format "Catalog for /%s/\n\n" board)
                     'face
                     'bold))
        (if (null catalog)
            (error "Failed to fetch catalog data for /%s/" board)
          (dolist (page catalog)
            (let ((threads (alist-get 'threads page)))
              (dolist (thread threads)
                (let* ((no (alist-get 'no thread))
                       (sub (or (alist-get 'sub thread) "No Subject"))
                       (replies (alist-get 'replies thread))
                       (tim (alist-get 'tim thread))
                       (ext (alist-get 'ext thread))
                       (image
                        (when (and tim ext)
                          (chan--fetch-image-sync
                           (chan--image-url board tim ext)))))
                  (insert
                   (propertize (format "[%d] %s (%d replies)\n"
                                       no
                                       sub
                                       replies)
                               'thread-id
                               no
                               'face
                               'link
                               'keymap
                               chan-catalog-mode-map))
                  (when image
                    (insert-image image))
                  (insert "\n\n"))))))
        (goto-char (point-min))))
    (switch-to-buffer buffer)))

(define-derived-mode
 chan-catalog-mode
 special-mode
 "Chan-Catalog"
 "Major mode for browsing 4chan catalogs."
 :group
 'chan-mode
 (setq buffer-read-only t))

(defun chan-catalog-open-thread ()
  "Open thread under point."
  (interactive)
  (let ((thread-id (get-text-property (point) 'thread-id)))
    (when thread-id
      (chan-thread chan--current-board thread-id))))

;;; Thread View
(defvar-local chan--thread-images nil
  "Alist of (position . (thumbnail . full-size-fn)) for images in thread.")

(defun chan-thread (board thread-id)
  "Display THREAD-ID from BOARD with async image loading."
  (let ((thread
         (chan--fetch-json
          (concat
           chan-base-url
           board
           "/thread/"
           (number-to-string thread-id)
           ".json")))
        (buffer
         (get-buffer-create
          (format "*chan-thread-%s-%d*" board thread-id))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (chan-thread-mode)
        (setq chan--current-board board)
        (setq chan--thread-images nil)
        (insert
         (propertize (format "Thread /%s/%d\n\n" board thread-id)
                     'face 'bold))
        (dolist (post (alist-get 'posts thread))
          (let* ((no (alist-get 'no post))
                 (com (or (alist-get 'com post) ""))
                 (tim (alist-get 'tim post))
                 (ext (alist-get 'ext post))
                 (image-url
                  (when (and tim ext)
                    (chan--image-url board tim ext))))
            (insert
             (propertize (format "Post #%d\n" no) 'face 'italic))
            (if (fboundp 'libxml-parse-html-region)
                (shr-insert-document
                 (with-temp-buffer
                   (insert com)
                   (libxml-parse-html-region
                    (point-min) (point-max))))
              (insert com))
            (insert "\n")
            (when image-url
              (let ((pos (point)))
                (insert "□") ; Placeholder
                (push (cons
                       pos
                       (cons
                        nil
                        (lambda ()
                          (chan--fetch-image-sync image-url t))))
                      chan--thread-images)
                (push (cons (current-buffer) (cons pos image-url))
                      chan--image-queue)))
            (insert "\n\n"))))
      (goto-char (point-min))
      (unless chan--image-timer
        (setq chan--image-timer
              (run-at-time 1 nil #'chan--process-image-queue))))
    (switch-to-buffer buffer)))

(defvar chan-thread-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'chan-thread-next)
    (define-key map (kbd "p") #'chan-thread-prev)
    (define-key map (kbd "q") #'kill-this-buffer)
    (define-key map (kbd "e") #'chan-thread-toggle-image-size)
    (define-key map (kbd "i") #'chan-thread-image-view)
    map)
  "Keymap for `chan-thread-mode'.")

(define-derived-mode
 chan-thread-mode
 special-mode
 "Chan-Thread"
 "Major mode for viewing 4chan threads."
 :group
 'chan-mode
 (setq buffer-read-only t))

(defun chan-thread-next ()
  "Move to next post."
  (interactive)
  (re-search-forward "^Post #" nil t))

(defun chan-thread-prev ()
  "Move to previous post."
  (interactive)
  (re-search-backward "^Post #" nil t))

(defun chan-thread-toggle-image-size ()
  "Toggle image under point between thumbnail and full size."
  (interactive)
  (let ((pos (point)))
    (dolist (img-data chan--thread-images)
      (when (and (>= pos (car img-data)) (< pos (+ (car img-data) 1)))
        (let* ((thumbnail (car (cdr img-data)))
               (full-size-fn (cdr (cdr img-data)))
               (current-image
                (get-text-property (car img-data) 'display))
               (full-size (funcall full-size-fn)))
          (save-excursion
            (goto-char (car img-data))
            (let ((inhibit-read-only t))
              (delete-char 1)
              (insert-image
               (if (eq current-image thumbnail)
                   full-size
                 thumbnail))))
          (setcar
           (cdr img-data)
           (if (eq current-image thumbnail)
               full-size
             thumbnail))
          (message "Image %s"
                   (if (eq current-image thumbnail)
                       "expanded"
                     "shrunk"))
          (cl-return))))))

;;; Image View Mode
(defvar-local chan--image-view-index 0
  "Current image index in image view.")

(defun chan-thread-image-view ()
  "Open image-only view of current thread."
  (interactive)
  (unless chan--thread-images
    (user-error "No images in this thread"))
  (let ((buffer
         (get-buffer-create
          (format "*chan-images-%s-%d*"
                  chan--current-board
                  (string-to-number (buffer-name))))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (chan-image-view-mode)
        (setq chan--thread-images (reverse chan--thread-images))
        (chan--image-view-show-image 0)))
    (switch-to-buffer buffer)))

(defun chan--image-view-show-image (index)
  "Show image at INDEX in image view."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (let* ((img-data (nth index chan--thread-images))
           (full-size
            (or (car (cdr img-data)) (funcall (cdr (cdr img-data)))))
           (thumbnail (car (cdr img-data))))
      (insert-image (or full-size thumbnail))
      (insert
       (propertize (format "\nImage %d of %d"
                           (1+ index)
                           (length chan--thread-images))
                   'face 'italic)))
    (goto-char (point-min))
    (setq chan--image-view-index index)))

(defvar chan-image-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<right>") #'chan-image-view-next)
    (define-key map (kbd "<left>") #'chan-image-view-prev)
    (define-key map (kbd "q") #'kill-this-buffer)
    map)
  "Keymap for `chan-image-view-mode'.")

(define-derived-mode
 chan-image-view-mode
 special-mode
 "Chan-Image-View"
 "Major mode for viewing thread images."
 :group
 'chan-mode
 (setq buffer-read-only t))

(defun chan-image-view-next ()
  "Show next image."
  (interactive)
  (when (< (1+ chan--image-view-index) (length chan--thread-images))
    (chan--image-view-show-image (1+ chan--image-view-index))))

(defun chan-image-view-prev ()
  "Show previous image."
  (interactive)
  (when (> chan--image-view-index 0)
    (chan--image-view-show-image (1- chan--image-view-index))))

;;; Entry Point
;;;###autoload
(defun chan-mode-start ()
  "Start chan-mode by showing a catalog."
  (interactive)
  (chan-catalog "g"))

(provide 'chan-mode)
;;; chan-mode.el ends here
