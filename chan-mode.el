;;; chan-mode.el --- Read 4chan boards as feeds in Emacs -*- lexical-binding: t -*-

;; Author: Blackdream <grim@grim.su>
;; Version: 1.4
;; Package-Requires: ((emacs "30.1") (cl-lib "0.5"))
;; Keywords: comm, multimedia
;; URL: https://github.com/theesfeld/chan-mode

;;; Commentary:
;; A synchronous 4chan board reader for Emacs. Loads current page/thread thumbnails top-to-bottom,
;; with fixed-size placeholders, enhanced thread view, full-size image toggling, and an image gallery.

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

(defcustom chan-thumbnail-width 200
  "Width of thumbnail placeholders."
  :type 'integer
  :group 'chan-mode)

(defcustom chan-thumbnail-height 200
  "Height of thumbnail placeholders."
  :type 'integer
  :group 'chan-mode)

(defvar chan--boards-cache nil
  "Cache of available boards.")
(defvar chan--last-request-time 0
  "Timestamp of last API request for rate limiting.")
(defvar chan--current-board nil
  "Current board being viewed.")
(defvar chan--current-page 0
  "Current page of the catalog being viewed.")

;;; Utility Functions
(defun chan--rate-limit-wait ()
  "Ensure at least 1 second between API requests."
  (let ((elapsed (- (float-time) chan--last-request-time)))
    (when (< elapsed 1.0)
      (sleep-for (- 1.0 elapsed)))
    (setq chan--last-request-time (float-time))))

(defun chan--fetch-json (url)
  "Fetch and parse JSON from URL synchronously with diagnostics."
  (message "Fetching JSON from: %s" url)
  (chan--rate-limit-wait)
  (with-temp-buffer
    (let ((status (url-insert-file-contents url)))
      (if (not status)
          (progn
            (message "Network failure fetching %s" url)
            nil)
        (goto-char (point-min))
        (message "Raw response (first 500 chars): %s"
                 (buffer-substring (point-min) (min (point-max) 500)))
        (let ((http-status
               (if (looking-at "HTTP/[0-1].[0-1] \\([0-9]+\\)")
                   (string-to-number (match-string 1))
                 200)))
          (message "HTTP status: %d" http-status)
          (if (>= http-status 400)
              (progn
                (message "HTTP error %d fetching %s" http-status url)
                nil)
            (goto-char (point-min))
            (if (re-search-forward "\r?\n\r?\n" nil t)
                (progn
                  (message "Found header-body separator at: %d"
                           (point))
                  (condition-case err
                      (let ((json-object-type 'alist)
                            (json-array-type 'list)
                            (json-key-type 'symbol))
                        (let ((result (json-read)))
                          (message "JSON parsed successfully from %s"
                                   url)
                          result))
                    (error
                     (message "JSON parsing failed for %s: %s"
                              url
                              (error-message-string err))
                     nil)))
              (message "No header-body separator; assuming pure JSON")
              (goto-char (point-min))
              (condition-case err
                  (let ((json-object-type 'alist)
                        (json-array-type 'list)
                        (json-key-type 'symbol))
                    (let ((result (json-read)))
                      (message "JSON parsed successfully from %s" url)
                      result))
                (error
                 (message "JSON parsing failed for %s: %s"
                          url
                          (error-message-string err))
                 nil)))))))))

(defun chan--get-boards ()
  "Fetch list of boards, caching result."
  (or chan--boards-cache
      (setq chan--boards-cache
            (alist-get
             'boards
             (chan--fetch-json
              (concat chan-base-url "boards.json"))))))

(defun chan--image-url (board tim ext &optional thumbnail)
  "Construct image URL from BOARD, TIM, and EXT."
  (when (and tim ext)
    (if thumbnail
        (concat
         chan-image-base-url board "/" (number-to-string tim) "s.jpg")
      (concat
       chan-image-base-url board "/" (number-to-string tim) ext))))

(defun chan--fetch-image (url &optional full-size)
  "Fetch image from URL synchronously, return image object or nil."
  (when url
    (message "Fetching image from: %s" url)
    (chan--rate-limit-wait)
    (let ((buffer (url-retrieve-synchronously url t t 5)))
      (if (not buffer)
          (progn
            (message "Failed to retrieve buffer for %s" url)
            nil)
        (with-current-buffer buffer
          (unwind-protect
              (condition-case err
                  (progn
                    (goto-char (point-min))
                    (let ((http-status
                           (if (looking-at
                                "HTTP/[0-1].[0-1] \\([0-9]+\\)")
                               (string-to-number (match-string 1))
                             200)))
                      (if (>= http-status 400)
                          (progn
                            (message
                             "HTTP error %d fetching image from %s"
                             http-status url)
                            nil)
                        (goto-char (point-min))
                        (if (re-search-forward "\r?\n\r?\n" nil t)
                            (message
                             "Found image header-body separator at: %d"
                             (point))
                          (message
                           "No image header-body separator; assuming pure data"))
                        (let ((data
                               (buffer-substring-no-properties
                                (point) (point-max))))
                          (if (string-empty-p data)
                              (progn
                                (message
                                 "No image data received from %s"
                                 url)
                                nil)
                            (create-image
                             data
                             nil
                             t
                             :max-width
                             (if full-size
                                 nil
                               chan-thumbnail-width)
                             :max-height
                             (if full-size
                                 nil
                               chan-thumbnail-height)))))))
                (error
                 (message "Failed to fetch image %s: %s"
                          url
                          (error-message-string err))
                 nil))
            (kill-buffer buffer)))))))

;;; Catalog View
(defvar chan-catalog-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'chan-catalog-open-thread)
    (define-key map (kbd "n") #'chan-catalog-next-page)
    (define-key map (kbd "p") #'chan-catalog-prev-page)
    (define-key map [mouse-1] #'chan-catalog-open-thread-mouse)
    map)
  "Keymap for chan-catalog-mode.")

(defun chan-catalog (board &optional page)
  "Display catalog page PAGE (default 0) for BOARD."
  (interactive (list
                (completing-read
                 "Board: "
                 (mapcar
                  (lambda (b)
                    (alist-get 'board b))
                  (chan--get-boards)))))
  (setq chan--current-board board)
  (setq chan--current-page (or page 0))
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
         (propertize "Boards: "
                     'face
                     '(:weight bold :foreground "cyan")))
        (dolist (b (chan--get-boards))
          (let ((board-name (alist-get 'board b)))
            (insert
             (propertize (format "/%s/ " board-name)
                         'face
                         '(:foreground "blue" :underline t)
                         'keymap
                         chan-catalog-mode-map
                         'board
                         board-name
                         'follow-link
                         t
                         'mouse-face
                         'highlight
                         'action
                         (lambda (_) (chan-catalog board-name))))))
        (insert "\n\n")
        (chan-catalog--insert-page-navigation catalog)
        (insert "\n\n")
        (if (null catalog)
            (error "Failed to fetch catalog data for /%s/" board)
          (let ((threads
                 (alist-get
                  'threads (nth chan--current-page catalog))))
            (dolist (thread threads)
              (chan-catalog--insert-thread thread))))
        (insert "\n")
        (goto-char (point-min)))
      (switch-to-buffer buffer))))

(define-derived-mode
 chan-catalog-mode
 fundamental-mode
 "Chan-Catalog"
 "Major mode for browsing 4chan catalogs.")

(defun chan-catalog--insert-thread (thread)
  "Insert a single THREAD into the catalog."
  (let ((no (alist-get 'no thread))
        (sub (or (alist-get 'sub thread) "No Subject"))
        (time (alist-get 'time thread))
        (tim (alist-get 'tim thread))
        (ext (alist-get 'ext thread)))
    (let ((inhibit-read-only t)
          (image-url (chan--image-url chan--current-board tim ext t)))
      (insert
       (propertize (format "[%d] %s" no sub)
                   'thread-id
                   no
                   'face
                   '(:foreground "yellow" :underline t)
                   'keymap
                   chan-catalog-mode-map
                   'follow-link
                   t
                   'mouse-face
                   'highlight))
      (insert " ")
      (insert
       (propertize (format "Posted: %s"
                           (format-time-string "%Y-%m-%d %H:%M:%S"
                                               time))
                   'face '(:foreground "green" :height 0.8)))
      (insert " ")
      (let ((pos (point)))
        (insert-image
         (create-image (make-string 1 ?\s)
                       nil
                       t
                       :width chan-thumbnail-width
                       :height chan-thumbnail-height))
        (when image-url
          (let ((image (chan--fetch-image image-url)))
            (when image
              (goto-char pos)
              (delete-char 1)
              (insert-image image)))))
      (insert "\n"))))

(defun chan-catalog--insert-page-navigation (catalog)
  "Insert page navigation links for CATALOG."
  (let ((inhibit-read-only t))
    (insert
     (propertize "Navigation: "
                 'face
                 '(:weight bold :foreground "cyan")))
    (dotimes (i (length catalog))
      (insert
       (propertize (format "[Page %d]" (1+ i))
                   'face
                   (if (= i chan--current-page)
                       '(:weight bold :foreground "white")
                     '(:foreground "blue" :underline t))
                   'keymap
                   chan-catalog-mode-map
                   'page
                   i
                   'follow-link
                   t
                   'mouse-face
                   'highlight
                   'action
                   (lambda (_) (chan-catalog chan--current-board i))))
      (insert " "))))

(defun chan-catalog-next-page ()
  "Go to the next page of the catalog."
  (interactive)
  (let ((catalog
         (chan--fetch-json
          (concat
           chan-base-url chan--current-board "/catalog.json"))))
    (when (< (1+ chan--current-page) (length catalog))
      (chan-catalog chan--current-board (1+ chan--current-page)))))

(defun chan-catalog-prev-page ()
  "Go to the previous page of the catalog."
  (interactive)
  (when (> chan--current-page 0)
    (chan-catalog chan--current-board (1- chan--current-page))))

(defun chan-catalog-open-thread ()
  "Open thread under point."
  (interactive)
  (let ((thread-id (get-text-property (point) 'thread-id)))
    (when thread-id
      (chan-thread chan--current-board thread-id))))

(defun chan-catalog-open-thread-mouse (event)
  "Open thread under mouse click EVENT."
  (interactive "e")
  (mouse-set-point event)
  (chan-catalog-open-thread))

;;; Thread View
(defvar-local chan--thread-images nil
  "Alist of (position . (thumbnail . full-url)) for images in thread.")

(defvar chan-thread-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'chan-thread-next)
    (define-key map (kbd "p") #'chan-thread-prev)
    (define-key map (kbd "q") #'chan-thread-quit)
    (define-key map (kbd "e") #'chan-thread-toggle-image-size)
    (define-key map (kbd "i") #'chan-thread-image-view)
    (define-key map (kbd "c p") #'chan-thread-copy-post-link)
    (define-key map (kbd "c t") #'chan-thread-copy-thread-link)
    (define-key map (kbd "c c") #'chan-thread-copy-post-content)
    (define-key map [mouse-1] #'chan-thread-toggle-image-size-mouse)
    map)
  "Keymap for chan-thread-mode.")

(defun chan-thread (board thread-id)
  "Display THREAD-ID from BOARD with synchronous thumbnail loading."
  (let ((thread-json
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
        (insert
         (propertize (format "Thread /%s/%d\n\n" board thread-id)
                     'face '(:weight bold :foreground "cyan")))
        (if (null thread-json)
            (error
             "Failed to fetch thread data for /%s/%d" board thread-id)
          (mapc
           #'chan-thread--insert-post (alist-get 'posts thread-json)))
        (goto-char (point-min)))
      (setq chan--current-board board)
      (switch-to-buffer buffer))))

(define-derived-mode
 chan-thread-mode
 fundamental-mode
 "Chan-Thread"
 "Major mode for browsing 4chan threads.")

(defun chan-thread--insert-post (post)
  "Insert a single POST with a box and colored info."
  (let ((no (alist-get 'no post))
        (time (alist-get 'time post))
        (name (or (alist-get 'name post) "Anonymous"))
        (trip (alist-get 'trip post))
        (id (alist-get 'id post))
        (capcode (alist-get 'capcode post))
        (country (alist-get 'country post))
        (sub (alist-get 'sub post))
        (com (or (alist-get 'com post) ""))
        (tim (alist-get 'tim post))
        (ext (alist-get 'ext post)))
    (let ((inhibit-read-only t)
          (thumbnail-url
           (chan--image-url chan--current-board tim ext t))
          (full-url (chan--image-url chan--current-board tim ext))
          (start (point)))
      (insert
       (propertize
        "┌──────────────────────────────────────────────────┐\n"
        'face '(:foreground "gray")))
      (insert
       (propertize (format "│ Post #%d\n" no)
                   'face
                   '(:foreground "yellow")))
      (insert
       (propertize (format "│ No: %d " no)
                   'face
                   '(:foreground "magenta")))
      (insert
       (propertize (format "Time: %s "
                           (format-time-string "%Y-%m-%d %H:%M:%S"
                                               time))
                   'face '(:foreground "green")))
      (insert
       (propertize (format "Name: %s%s%s "
                           name
                           (if trip
                               (concat " " trip)
                             "")
                           (if capcode
                               (concat " ## " capcode)
                             ""))
                   'face '(:foreground "cyan")))
      (insert
       (propertize (format "ID: %s " (or id "N/A"))
                   'face
                   '(:foreground "purple")))
      (insert
       (propertize (format "Country: %s\n" (or country "N/A"))
                   'face
                   '(:foreground "blue")))
      (when sub
        (insert
         (propertize (format "│ Subject: %s\n" sub)
                     'face
                     '(:foreground "orange"))))
      (insert (propertize "│\n" 'face '(:foreground "gray")))
      (when thumbnail-url
        (let ((pos (point)))
          (insert-image
           (create-image (make-string 1 ?\s)
                         nil
                         t
                         :width chan-thumbnail-width
                         :height chan-thumbnail-height))
          (insert " ")
          (let ((thumbnail (chan--fetch-image thumbnail-url)))
            (when thumbnail
              (goto-char pos)
              (delete-char 1)
              (insert-image thumbnail)
              (push (cons pos (cons thumbnail full-url))
                    chan--thread-images)))))
      (insert (propertize "│ " 'face '(:foreground "gray")))
      (chan-thread--insert-comment com)
      (insert
       (propertize
        "└──────────────────────────────────────────────────┘\n\n"
        'face '(:foreground "gray")))
      (put-text-property
       start
       (point)
       'chan-post-data
       (list :no no :thread-id (string-to-number (buffer-name)))))))

(defun chan-thread--insert-comment (com)
  "Insert comment COM with readable text."
  (let ((inhibit-read-only t))
    (if (fboundp 'libxml-parse-html-region)
        (shr-insert-document
         (with-temp-buffer
           (insert com)
           (libxml-parse-html-region (point-min) (point-max))))
      (insert (propertize com 'face '(:foreground "white"))))
    (insert "\n")))

(defun chan-thread-next ()
  "Move to next post."
  (interactive)
  (re-search-forward "┌─" nil t)
  (forward-line 1))

(defun chan-thread-prev ()
  "Move to previous post."
  (interactive)
  (re-search-backward "┌─" nil t))

(defun chan-thread-quit ()
  "Quit thread view and return to catalog."
  (interactive)
  (kill-buffer)
  (chan-catalog chan--current-board chan--current-page))

(defun chan-thread-toggle-image-size ()
  "Toggle image under point between thumbnail and full size."
  (interactive)
  (let ((pos (point)))
    (dolist (img-data chan--thread-images)
      (when (and (>= pos (car img-data))
                 (< pos (+ (car img-data) chan-thumbnail-width)))
        (let ((thumbnail (car (cdr img-data)))
              (full-url (cdr (cdr img-data)))
              (current-image
               (get-text-property (car img-data) 'display)))
          (let ((full-image (chan--fetch-image full-url t)))
            (when full-image
              (goto-char (car img-data))
              (let ((inhibit-read-only t))
                (delete-char 1)
                (insert-image
                 (if (eq current-image thumbnail)
                     full-image
                   thumbnail)))
              (setcar
               (cdr img-data)
               (if (eq current-image thumbnail)
                   full-image
                 thumbnail))
              (message "Image %s"
                       (if (eq current-image thumbnail)
                           "expanded"
                         "shrunk")))))))))

(defun chan-thread-toggle-image-size-mouse (event)
  "Toggle image under mouse click EVENT."
  (interactive "e")
  (mouse-set-point event)
  (chan-thread-toggle-image-size))

(defun chan-thread-copy-post-link ()
  "Copy link to post under point."
  (interactive)
  (let* ((data (get-text-property (point) 'chan-post-data))
         (no (plist-get data :no))
         (thread-id (plist-get data :thread-id)))
    (when (and no thread-id)
      (let ((link
             (format "https://boards.4chan.org/%s/thread/%d#p%d"
                     chan--current-board
                     thread-id
                     no)))
        (kill-new link)
        (message "Copied post link: %s" link)))))

(defun chan-thread-copy-thread-link ()
  "Copy link to current thread."
  (interactive)
  (let ((thread-id (string-to-number (buffer-name))))
    (let ((link
           (format "https://boards.4chan.org/%s/thread/%d"
                   chan--current-board
                   thread-id)))
      (kill-new link)
      (message "Copied thread link: %s" link))))

(defun chan-thread-copy-post-content ()
  "Copy content of post under point."
  (interactive)
  (let ((start (save-excursion (re-search-backward "┌─" nil t)))
        (end (save-excursion (re-search-forward "└─" nil t))))
    (when (and start end)
      (let ((content (buffer-substring-no-properties start end)))
        (kill-new content)
        (message "Copied post content")))))

;;; Image View Mode
(defvar-local chan--image-view-index 0
  "Current image index in image view.")

(defvar chan-image-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<right>") #'chan-image-view-next)
    (define-key map (kbd "<left>") #'chan-image-view-prev)
    (define-key map (kbd "q") #'kill-this-buffer)
    map)
  "Keymap for chan-image-view-mode.")

(defun chan-thread-image-view ()
  "Open image-only view of current thread with full-size images."
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
        (chan--image-view-show-image 0))
      (switch-to-buffer buffer))))

(define-derived-mode
 chan-image-view-mode
 fundamental-mode
 "Chan-Image-View"
 "Major mode for viewing full-size thread images.")

(defun chan--image-view-show-image (index)
  "Show full-size image at INDEX in image view."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (let* ((img-data (nth index chan--thread-images))
           (full-url (cdr (cdr img-data)))
           (full-image (chan--fetch-image full-url t)))
      (when full-image
        (insert-image full-image)
        (insert
         (propertize (format "\nImage %d of %d"
                             (1+ index)
                             (length chan--thread-images))
                     'face '(:foreground "cyan" :height 0.8)))
        (goto-char (point-min))
        (setq chan--image-view-index index)))))

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
(defun chan-mode-start ()
  "Start chan-mode with a simple catalog."
  (interactive)
  (unless (image-type-available-p 'jpeg)
    (error
     "Emacs lacks JPEG support. Install a version with image support."))
  (chan-catalog "g"))

(provide 'chan-mode)
;;; chan-mode.el ends here
