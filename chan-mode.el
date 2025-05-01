;;; chan-mode.el --- Read-only 4chan viewer for Emacs -*- lexical-binding: t; -*-

;; Author: Your Name <your.email@example.com>
;; Version: 0.2
;; Package-Requires: ((emacs "30.1"))
;; Keywords: hypermedia, 4chan, viewer
;; URL: https://github.com/yourusername/chan-mode
;; License: GPL-3.0-or-later

;;; Commentary:

;; This package provides a read-only viewer for 4chan boards using the 4chan API.
;; Features:
;; - Catalog view with pagination and API-fetched thumbnails
;; - Thread view with thumbnail/full-size image toggle (RET on image)
;; - Board selection via minibuffer with completion
;; - Customizable default board, thumbnail scale, and auto-refresh
;; - System theming support with font locking
;; - Simple navigation (q to return to catalog, r to refresh)
;; Uses Emacs 30.1's native JSON parsing for performance.

;; Usage:
;; M-x chan-mode RET to open the catalog view for the default board.
;; Customize `chan-mode-board' for the default board (e.g., "g", "pol").
;; Press C-c b to select a new board in catalog or thread view.


;; (use-package chan-mode
;;   :ensure t
;;   :vc (:url "https://github.com/yourusername/chan-mode" :rev :newest)
;;   :commands (chan-mode)
;;   :custom
;;   (chan-mode-board "g" "Default board to view (e.g., 'a', 'g', 'k', 'pol')")
;;   (chan-mode-thumbnail-scale 0.5 "Scale factor for thumbnail images")
;;   (chan-mode-full-image-scale 1.0 "Scale factor for full-size images")
;;   (chan-mode-auto-refresh-interval 0 "Auto-refresh interval in seconds (0 to disable)")
;;   :bind
;;   (("C-c 4" . chan-mode)
;;    :map chan-mode-map
;;    ("C-c b" . chan-mode-select-board)
;;    ("r" . chan-mode-refresh)
;;    :map chan-mode-catalog-mode-map
;;    ("n" . chan-mode-next-page)
;;    ("p" . chan-mode-prev-page)
;;    ("RET" . chan-mode-open-thread)
;;    :map chan-mode-thread-mode-map
;;    ("q" . chan-mode-return-to-catalog)
;;    ("RET" . chan-mode-toggle-image-size))
;;   :hook
;;   ((chan-mode-catalog-mode . (lambda () (setq buffer-face-mode-face '(:family "monospace"))))
;;    (chan-mode-thread-mode . (lambda () (setq buffer-face-mode-face '(:family "monospace")))))
;;   :config
;;   (font-lock-mode 1)
;;   (set-face-attribute 'chan-mode-op-face nil :background "#2e2e2e" :foreground "#ffcc00")
;;   (set-face-attribute 'chan-mode-metadata-face nil :foreground "#888888")
;;   (set-face-attribute 'chan-mode-you-face nil :foreground "#ff5555" :weight 'bold))

;;; Code:

(require 'url)
(require 'shr) ;; For rendering HTML content in posts
(require 'json)

;; Customizable variables
(defgroup chan-mode nil
  "Read-only 4chan viewer for Emacs."
  :group 'applications
  :prefix "chan-mode-")

(defcustom chan-mode-board "g"
  "Default 4chan board to view (e.g., 'a', 'g', 'k', 'pol')."
  :type 'string
  :group 'chan-mode)

(defcustom chan-mode-catalog-page 1
  "Current page of the catalog view."
  :type 'integer
  :group 'chan-mode)

(defcustom chan-mode-api-base "https://a.4cdn.org"
  "Base URL for the 4chan API."
  :type 'string
  :group 'chan-mode)

(defcustom chan-mode-thumbnail-scale 0.5
  "Scale factor for thumbnail images."
  :type 'float
  :group 'chan-mode)

(defcustom chan-mode-full-image-scale 1.0
  "Scale factor for full-size images in thread view."
  :type 'float
  :group 'chan-mode)

(defcustom chan-mode-auto-refresh-interval 0
  "Interval (seconds) to auto-refresh catalog/thread view. 0 disables."
  :type 'integer
  :group 'chan-mode)

;; Faces for font locking and theming
(defface chan-mode-op-face
  '((t
     :inherit font-lock-function-name-face
     :weight bold
     :background "#2e2e2e"))
  "Face for highlighting OP posts in thread view."
  :group 'chan-mode)

(defface chan-mode-metadata-face
  '((t :inherit font-lock-comment-face))
  "Face for post metadata (e.g., post count, tripcodes)."
  :group 'chan-mode)

(defface chan-mode-you-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for (You) count in posts."
  :group 'chan-mode)

;; Buffer names
(defconst chan-mode-catalog-buffer "*Chan Catalog*"
  "Name of the catalog view buffer.")

(defconst chan-mode-thread-buffer "*Chan Thread*"
  "Name of the thread view buffer.")

;; Base major mode
(define-derived-mode
 chan-mode
 special-mode
 "Chan"
 "Base major mode for 4chan viewer."
 :group
 'chan-mode
 (setq buffer-read-only t)
 (chan-mode-setup-chan-keybindings))

;; Derived modes
(define-derived-mode
 chan-mode-catalog-mode
 chan-mode
 "Chan-Catalog"
 "Major mode for 4chan catalog view."
 :group
 'chan-mode
 (chan-mode-setup-catalog-keybindings)
 (chan-mode-start-auto-refresh))

(define-derived-mode
 chan-mode-thread-mode
 chan-mode
 "Chan-Thread"
 "Major mode for 4chan thread view."
 :group
 'chan-mode
 (chan-mode-setup-thread-keybindings)
 (chan-mode-start-auto-refresh))

;; Keybindings
(defun chan-mode-setup-chan-keybindings ()
  "Set up keybindings for chan-mode."
  (let ((map chan-mode-map))
    (define-key map (kbd "C-c b") #'chan-mode-select-board)
    (define-key map (kbd "r") #'chan-mode-refresh)))

(defun chan-mode-setup-catalog-keybindings ()
  "Set up keybindings for catalog mode."
  (let ((map chan-mode-catalog-mode-map))
    (define-key map (kbd "n") #'chan-mode-next-page)
    (define-key map (kbd "p") #'chan-mode-prev-page)
    (define-key map (kbd "RET") #'chan-mode-open-thread)))

(defun chan-mode-setup-thread-keybindings ()
  "Set up keybindings for thread mode."
  (let ((map chan-mode-thread-mode-map))
    (define-key map (kbd "q") #'chan-mode-return-to-catalog)
    (define-key map (kbd "RET") #'chan-mode-toggle-image-size)))

;; API request function
(defun chan-mode-fetch-json (url callback)
  "Fetch JSON from URL and call CALLBACK with parsed data."
  (url-retrieve
   url
   (lambda (status)
     (if (plist-get status :error)
         (message "Failed to fetch %s: %s"
                  url
                  (plist-get status :error))
       (goto-char (point-min))
       (when (search-forward "\n\n" nil t)
         (let ((json-data (json-parse-buffer :object-type 'alist)))
           (funcall callback json-data)))))
   nil t))

;; Board selection
(defvar chan-mode-board-list nil
  "List of valid 4chan boards.")

(defun chan-mode-fetch-boards (callback)
  "Fetch list of valid boards and call CALLBACK."
  (chan-mode-fetch-json
   (format "%s/boards.json" chan-mode-api-base)
   (lambda (data)
     (setq chan-mode-board-list
           (mapcar
            (lambda (board) (alist-get 'board board))
            (alist-get 'boards data)))
     (funcall callback))))

(defun chan-mode-select-board ()
  "Prompt for a board in the minibuffer and switch to it."
  (interactive)
  (chan-mode-fetch-boards
   (lambda ()
     (let ((board
            (completing-read "Select board: " chan-mode-board-list
                             nil t)))
       (when board
         (setq chan-mode-board board)
         (chan-mode-render-catalog))))))

;; API request function
(defun chan-mode-fetch-json (url callback)
  "Fetch JSON from URL and call CALLBACK with parsed data."
  (message "Fetching %s..." url) ;; Debugging feedback
  (url-retrieve
   url
   (lambda (status)
     (if (plist-get status :error)
         (progn
           (message "Failed to fetch %s: %s"
                    url
                    (plist-get status :error))
           (with-current-buffer (get-buffer-create
                                 chan-mode-catalog-buffer)
             (let ((inhibit-read-only t))
               (erase-buffer)
               (insert
                (format "Error: Failed to fetch catalog from %s\n"
                        url))
               (chan-mode-catalog-mode)
               (pop-to-buffer (current-buffer)))))
       (goto-char (point-min))
       (if (search-forward "\n\n" nil t)
           (let ((json-data
                  (condition-case err
                      (json-parse-buffer :object-type 'alist)
                    (error
                     (message "JSON parsing error: %s" err) nil))))
             (if json-data
                 (funcall callback json-data)
               (with-current-buffer (get-buffer-create
                                     chan-mode-catalog-buffer)
                 (let ((inhibit-read-only t))
                   (erase-buffer)
                   (insert "Error: Failed to parse JSON data\n")
                   (chan-mode-catalog-mode)
                   (pop-to-buffer (current-buffer))))))
         (message "No valid JSON data found in response")
         (with-current-buffer (get-buffer-create
                               chan-mode-catalog-buffer)
           (let ((inhibit-read-only t))
             (erase-buffer)
             (insert "Error: No valid JSON data in response\n")
             (chan-mode-catalog-mode)
             (pop-to-buffer (current-buffer)))))))
   nil t))

;; Catalog view
(defun chan-mode-render-catalog ()
  "Render the catalog view for the current board and page in a new buffer."
  (let ((buffer (get-buffer-create chan-mode-catalog-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert
         (format "Loading 4chan /%s/ Catalog (Page %d)...\n"
                 chan-mode-board
                 chan-mode-catalog-page))
        (chan-mode-catalog-mode)))
    (pop-to-buffer buffer))
  (chan-mode-fetch-json
   (format "%s/%s/catalog.json" chan-mode-api-base chan-mode-board)
   (lambda (data)
     (with-current-buffer (get-buffer-create chan-mode-catalog-buffer)
       (let ((inhibit-read-only t))
         (erase-buffer)
         (insert
          (format "4chan /%s/ Catalog (Page %d)\n\n"
                  chan-mode-board
                  chan-mode-catalog-page))
         (let ((threads
                (alist-get
                 'threads (nth (1- chan-mode-catalog-page) data))))
           (if threads
               (dolist (thread threads)
                 (chan-mode-insert-catalog-thread thread))
             (insert "No threads found on this page.\n")))
         (chan-mode-catalog-mode)
         (goto-char (point-min))
         (pop-to-buffer (current-buffer)))))))

;; Thread view
(defun chan-mode-open-thread ()
  "Open the thread under point in the catalog view."
  (interactive)
  (let ((thread-id (get-text-property (point) 'thread-id)))
    (when thread-id
      (chan-mode-fetch-json
       (format "%s/%s/thread/%d.json"
               chan-mode-api-base
               chan-mode-board
               thread-id)
       (lambda (data)
         (with-current-buffer (get-buffer-create
                               chan-mode-thread-buffer)
           (let ((inhibit-read-only t))
             (erase-buffer)
             (insert
              (propertize
               (format "4chan /%s/ Thread %d\n\n"
                       chan-mode-board
                       thread-id)
               'thread-id thread-id))
             (dolist (post (alist-get 'posts data))
               (chan-mode-insert-thread-post post thread-id))
             (chan-mode-thread-mode)
             (goto-char (point-min))
             (pop-to-buffer (current-buffer)))))))))

(defun chan-mode-insert-thread-post (post thread-id)
  "Insert a single POST into the THREAD-ID view."
  (let* ((no (alist-get 'no post))
         (name (or (alist-get 'name post) "Anonymous"))
         (trip (alist-get 'trip post))
         (time (alist-get 'time post))
         (com (alist-get 'com post))
         (tim (alist-get 'tim post))
         (ext (alist-get 'ext post))
         (thumb
          (when tim
            (format "https://t.4cdn.org/%s/%ss.jpg"
                    chan-mode-board
                    tim)))
         (full
          (when tim
            (format "https://i.4cdn.org/%s/%s%s"
                    chan-mode-board
                    tim
                    ext)))
         (is-op (eq no thread-id))
         (you-count (chan-mode-count-you com)))
    (insert
     (propertize (format "Post %d by %s%s [%s] (You: %d)\n"
                         no
                         name
                         (or trip "")
                         (chan-mode-format-time time)
                         you-count)
                 'face
                 (if is-op
                     'chan-mode-op-face
                   'chan-mode-metadata-face)))
    (when thumb
      (chan-mode-insert-image thumb chan-mode-thumbnail-scale full))
    (when com
      (insert
       (propertize (chan-mode-strip-html com)
                   'face
                   'font-lock-string-face)))
    (insert "\n\n")))

;; Image handling
(defun chan-mode-insert-image (url scale full-url)
  "Insert image from URL with SCALE, optionally with FULL-URL for toggling."
  (condition-case err
      (let ((buffer (url-retrieve-synchronously url t)))
        (when buffer
          (with-current-buffer buffer
            (goto-char (point-min))
            (when (search-forward "\n\n" nil t)
              (let ((image
                     (create-image (buffer-substring (point) (point-max))
                                   nil t
                                   :scale scale)))
                (insert
                 (propertize " "
                             'display
                             image
                             'image-url
                             url
                             'full-url
                             full-url
                             'current-scale
                             scale)))))))
    (error (insert (format "[Image loading failed: %s]" err)))))

(defun chan-mode-toggle-image-size ()
  "Toggle between thumbnail and full-size image at point."
  (interactive)
  (let ((url (get-text-property (point) 'image-url))
        (full-url (get-text-property (point) 'full-url))
        (current-scale (get-text-property (point) 'current-scale)))
    (when (and url full-url)
      (let ((inhibit-read-only t)
            (new-url
             (if (equal url full-url)
                 (replace-regexp-in-string "i\\.4cdn" "t.4cdn" url)
               full-url))
            (new-scale
             (if (equal url full-url)
                 chan-mode-thumbnail-scale
               chan-mode-full-image-scale)))
        (delete-char 1)
        (chan-mode-insert-image new-url new-scale full-url)))))

;; Utility functions
(defun chan-mode-strip-html (html)
  "Strip HTML tags from HTML string."
  (if html
      (with-temp-buffer
        (insert html)
        (shr-insert-document
         (libxml-parse-html-region (point-min) (point-max)))
        (buffer-string))
    ""))

(defun chan-mode-count-you (text)
  "Count occurrences of '(You)' in TEXT."
  (if text
      (with-temp-buffer
        (insert text)
        (how-many "(You)" (point-min) (point-max)))
    0))

(defun chan-mode-format-time (timestamp)
  "Format Unix TIMESTAMP to human-readable string."
  (format-time-string "%Y-%m-%d %H:%M:%S"
                      (seconds-to-time timestamp)))

;; Pagination
(defun chan-mode-next-page ()
  "Go to the next catalog page."
  (interactive)
  (setq chan-mode-catalog-page (1+ chan-mode-catalog-page))
  (chan-mode-render-catalog))

(defun chan-mode-prev-page ()
  "Go to the previous catalog page."
  (interactive)
  (when (> chan-mode-catalog-page 1)
    (setq chan-mode-catalog-page (1- chan-mode-catalog-page))
    (chan-mode-render-catalog)))

;; Refresh
(defun chan-mode-refresh ()
  "Refresh the current view (catalog or thread)."
  (interactive)
  (cond
   ((eq major-mode 'chan-mode-catalog-mode)
    (chan-mode-render-catalog))
   ((eq major-mode 'chan-mode-thread-mode)
    (let ((thread-id (get-text-property (point-min) 'thread-id)))
      (when thread-id
        (chan-mode-open-thread))))))

(defun chan-mode-start-auto-refresh ()
  "Start auto-refresh timer if interval is set."
  (when (> chan-mode-auto-refresh-interval 0)
    (run-at-time
     chan-mode-auto-refresh-interval
     chan-mode-auto-refresh-interval
     #'chan-mode-refresh)))

;; Navigation
(defun chan-mode-return-to-catalog ()
  "Return to the catalog view from the thread view."
  (interactive)
  (kill-this-buffer)
  (chan-mode-render-catalog))

;; Entry point
;;;###autoload
(defun chan-mode ()
  "Start the 4chan viewer."
  (interactive)
  (chan-mode-render-catalog))

(provide 'chan-mode)
;;; chan-mode.el ends here
