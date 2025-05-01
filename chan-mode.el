;;; chan-viewer.el --- Read-only 4chan viewer for Emacs -*- lexical-binding: t; -*-

;; Author: Your Name <your.email@example.com>
;; Version: 0.2
;; Package-Requires: ((emacs "30.1"))
;; Keywords: hypermedia, 4chan, viewer
;; URL: https://github.com/yourusername/chan-viewer
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
;; M-x chan-viewer RET to open the catalog view for the default board.
;; Customize `chan-viewer-board' for the default board (e.g., "g", "pol").
;; Press C-c b to select a new board in catalog or thread view.


;; (use-package chan-viewer
;;   :ensure t
;;   :vc (:url "https://github.com/yourusername/chan-viewer" :rev :newest)
;;   :commands (chan-viewer)
;;   :custom
;;   (chan-viewer-board "g" "Default board to view (e.g., 'a', 'g', 'k', 'pol')")
;;   (chan-viewer-thumbnail-scale 0.5 "Scale factor for thumbnail images")
;;   (chan-viewer-full-image-scale 1.0 "Scale factor for full-size images")
;;   (chan-viewer-auto-refresh-interval 0 "Auto-refresh interval in seconds (0 to disable)")
;;   :bind
;;   (("C-c 4" . chan-viewer)
;;    :map chan-mode-map
;;    ("C-c b" . chan-viewer-select-board)
;;    ("r" . chan-viewer-refresh)
;;    :map chan-viewer-catalog-mode-map
;;    ("n" . chan-viewer-next-page)
;;    ("p" . chan-viewer-prev-page)
;;    ("RET" . chan-viewer-open-thread)
;;    :map chan-viewer-thread-mode-map
;;    ("q" . chan-viewer-return-to-catalog)
;;    ("RET" . chan-viewer-toggle-image-size))
;;   :hook
;;   ((chan-viewer-catalog-mode . (lambda () (setq buffer-face-mode-face '(:family "monospace"))))
;;    (chan-viewer-thread-mode . (lambda () (setq buffer-face-mode-face '(:family "monospace")))))
;;   :config
;;   (font-lock-mode 1)
;;   (set-face-attribute 'chan-viewer-op-face nil :background "#2e2e2e" :foreground "#ffcc00")
;;   (set-face-attribute 'chan-viewer-metadata-face nil :foreground "#888888")
;;   (set-face-attribute 'chan-viewer-you-face nil :foreground "#ff5555" :weight 'bold))

;;; Code:

(require 'url)
(require 'shr) ;; For rendering HTML content in posts

;; Customizable variables
(defgroup chan-viewer nil
  "Read-only 4chan viewer for Emacs."
  :group 'applications
  :prefix "chan-viewer-")

(defcustom chan-viewer-board "g"
  "Default 4chan board to view (e.g., 'a', 'g', 'k', 'pol')."
  :type 'string
  :group 'chan-viewer)

(defcustom chan-viewer-catalog-page 1
  "Current page of the catalog view."
  :type 'integer
  :group 'chan-viewer)

(defcustom chan-viewer-api-base "https://a.4cdn.org"
  "Base URL for the 4chan API."
  :type 'string
  :group 'chan-viewer)

(defcustom chan-viewer-thumbnail-scale 0.5
  "Scale factor for thumbnail images."
  :type 'float
  :group 'chan-viewer)

(defcustom chan-viewer-full-image-scale 1.0
  "Scale factor for full-size images in thread view."
  :type 'float
  :group 'chan-viewer)

(defcustom chan-viewer-auto-refresh-interval 0
  "Interval (seconds) to auto-refresh catalog/thread view. 0 disables."
  :type 'integer
  :group 'chan-viewer)

;; Faces for font locking and theming
(defface chan-viewer-op-face
  '((t :inherit font-lock-function-name-face :weight bold :background "#2e2e2e"))
  "Face for highlighting OP posts in thread view."
  :group 'chan-viewer)

(defface chan-viewer-metadata-face
  '((t :inherit font-lock-comment-face))
  "Face for post metadata (e.g., post count, tripcodes)."
  :group 'chan-viewer)

(defface chan-viewer-you-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for (You) count in posts."
  :group 'chan-viewer)

;; Buffer names
(defconst chan-viewer-catalog-buffer "*Chan Catalog*"
  "Name of the catalog view buffer.")

(defconst chan-viewer-thread-buffer "*Chan Thread*"
  "Name of the thread view buffer.")

;; Base major mode
(define-derived-mode chan-mode special-mode "Chan"
  "Base major mode for 4chan viewer."
  :group 'chan-viewer
  (setq buffer-read-only t)
  (chan-viewer-setup-chan-keybindings))

;; Derived modes
(define-derived-mode chan-viewer-catalog-mode chan-mode "Chan-Catalog"
  "Major mode for 4chan catalog view."
  :group 'chan-viewer
  (chan-viewer-setup-catalog-keybindings)
  (chan-viewer-start-auto-refresh))

(define-derived-mode chan-viewer-thread-mode chan-mode "Chan-Thread"
  "Major mode for 4chan thread view."
  :group 'chan-viewer
  (chan-viewer-setup-thread-keybindings)
  (chan-viewer-start-auto-refresh))

;; Keybindings
(defun chan-viewer-setup-chan-keybindings ()
  "Set up keybindings for chan-mode."
  (let ((map chan-mode-map))
    (define-key map (kbd "C-c b") #'chan-viewer-select-board)
    (define-key map (kbd "r") #'chan-viewer-refresh)))

(defun chan-viewer-setup-catalog-keybindings ()
  "Set up keybindings for catalog mode."
  (let ((map chan-viewer-catalog-mode-map))
    (define-key map (kbd "n") #'chan-viewer-next-page)
    (define-key map (kbd "p") #'chan-viewer-prev-page)
    (define-key map (kbd "RET") #'chan-viewer-open-thread)))

(defun chan-viewer-setup-thread-keybindings ()
  "Set up keybindings for thread mode."
  (let ((map chan-viewer-thread-mode-map))
    (define-key map (kbd "q") #'chan-viewer-return-to-catalog)
    (define-key map (kbd "RET") #'chan-viewer-toggle-image-size)))

;; API request function
(defun chan-viewer-fetch-json (url callback)
  "Fetch JSON from URL and call CALLBACK with parsed data."
  (url-retrieve
   url
   (lambda (status)
     (if (plist-get status :error)
         (message "Failed to fetch %s: %s" url (plist-get status :error))
       (goto-char (point-min))
       (when (search-forward "\n\n" nil t)
         (let ((json-data (json-parse-buffer :object-type 'alist)))
           (funcall callback json-data)))))
   nil t))

;; Board selection
(defvar chan-viewer-board-list nil
  "List of valid 4chan boards.")

(defun chan-viewer-fetch-boards (callback)
  "Fetch list of valid boards and call CALLBACK."
  (chan-viewer-fetch-json
   (format "%s/boards.json" chan-viewer-api-base)
   (lambda (data)
     (setq chan-viewer-board-list
           (mapcar (lambda (board) (alist-get 'board board))
                   (alist-get 'boards data)))
     (funcall callback))))

(defun chan-viewer-select-board ()
  "Prompt for a board in the minibuffer and switch to it."
  (interactive)
  (chan-viewer-fetch-boards
   (lambda ()
     (let ((board (completing-read "Select board: " chan-viewer-board-list nil t)))
       (when board
         (setq chan-viewer-board board)
         (chan-viewer-render-catalog))))))

;; Catalog view
(defun chan-viewer-render-catalog ()
  "Render the catalog view for the current board and page."
  (chan-viewer-fetch-json
   (format "%s/%s/catalog.json" chan-viewer-api-base chan-viewer-board)
   (lambda (data)
     (with-current-buffer (get-buffer-create chan-viewer-catalog-buffer)
       (let ((inhibit-read-only t))
         (erase-buffer)
         (insert (format "4chan /%s/ Catalog (Page %d)\n\n"
                         chan-viewer-board
                         chan-viewer-catalog-page))
         (dolist (page (nth (1- chan-viewer-catalog-page) data))
           (dolist (thread (alist-get 'threads page))
             (chan-viewer-insert-catalog-thread thread)))
         (chan-viewer-catalog-mode)
         (goto-char (point-min))
         (pop-to-buffer (current-buffer)))))))

(defun chan-viewer-insert-catalog-thread (thread)
  "Insert a single thread into the catalog view."
  (let* ((no (alist-get 'no thread))
         (sub (or (alist-get 'sub thread) "No Subject"))
         (com (alist-get 'com thread))
         (replies (alist-get 'replies thread))
         (images (alist-get 'images thread))
         (tim (alist-get 'tim thread))
         (thumb (when tim (format "https://t.4cdn.org/%s/%ss.jpg"
                                  chan-viewer-board tim))))
    (insert (propertize (format "[%d] %s (%d replies, %d images)\n"
                                no sub replies images)
                        'thread-id no
                        'face 'link))
    (when thumb
      (chan-viewer-insert-image thumb chan-viewer-thumbnail-scale nil))
    (when com
      (insert (propertize (chan-viewer-strip-html com)
                          'face 'font-lock-string-face)))
    (insert "\n\n")))

;; Thread view
(defun chan-viewer-open-thread ()
  "Open the thread under point in the catalog view."
  (interactive)
  (let ((thread-id (get-text-property (point) 'thread-id)))
    (when thread-id
      (chan-viewer-fetch-json
       (format "%s/%s/thread/%d.json" chan-viewer-api-base chan-viewer-board thread-id)
       (lambda (data)
         (with-current-buffer (get-buffer-create chan-viewer-thread-buffer)
           (let ((inhibit-read-only t))
             (erase-buffer)
             (insert (format "4chan /%s/ Thread %d\n\n"
                             chan-viewer-board thread-id))
             (dolist (post (alist-get 'posts data))
               (chan-viewer-insert-thread-post post thread-id))
             (chan-viewer-thread-mode)
             (goto-char (point-min))
             (pop-to-buffer (current-buffer))))))))))

(defun chan-viewer-insert-thread-post (post thread-id)
  "Insert a single post into the thread view."
  (let* ((no (alist-get 'no post))
         (name (or (alist-get 'name post) "Anonymous"))
         (trip (alist-get 'trip post))
         (time (alist-get 'time post))
         (com (alist-get 'com post))
         (tim (alist-get 'tim post))
         (ext (alist-get 'ext post))
         (thumb (when tim (format "https://t.4cdn.org/%s/%ss.jpg" chan-viewer-board tim)))
         (full (when tim (format "https://i.4cdn.org/%s/%s%s" chan-viewer-board tim ext)))
         (is-op (eq no thread-id))
         (you-count (chan-viewer-count-you com)))
    (insert (propertize (format "Post %d by %s%s [%s] (You: %d)\n"
                                no name (or trip "") (chan-viewer-format-time time) you-count)
                        'face (if is-op 'chan-viewer-op-face 'chan-viewer-metadata-face)))
    (when thumb
      (chan-viewer-insert-image thumb chan-viewer-thumbnail-scale full))
    (when com
      (insert (propertize (chan-viewer-strip-html com)
                          'face 'font-lock-string-face)))
    (insert "\n\n")))

;; Image handling
(defun chan-viewer-insert-image (url scale full-url)
  "Insert image from URL with SCALE, optionally with FULL-URL for toggling."
  (let ((buffer (url-retrieve-synchronously url t)))
    (when buffer
      (with-current-buffer buffer
        (goto-char (point-min))
        (when (search-forward "\n\n" nil t)
          (let ((image (create-image
                        (buffer-substring (point) (point-max))
                        nil t :scale scale)))
            (insert (propertize
                     " "
                     'display image
                     'image-url url
                     'full-url full-url
                     'current-scale scale))))))))

(defun chan-viewer-toggle-image-size ()
  "Toggle between thumbnail and full-size image at point."
  (interactive)
  (let ((url (get-text-property (point) 'image-url))
        (full-url (get-text-property (point) 'full-url))
        (current-scale (get-text-property (point) 'current-scale)))
    (when (and url full-url)
      (let ((inhibit-read-only t)
            (new-url (if (equal url full-url) (replace-regexp-in-string "i\\.4cdn" "t.4cdn" url) full-url))
            (new-scale (if (equal url full-url) chan-viewer-thumbnail-scale chan-viewer-full-image-scale)))
        (delete-char 1)
        (chan-viewer-insert-image new-url new-scale full-url)))))

;; Utility functions
(defun chan-viewer-strip-html (html)
  "Strip HTML tags from HTML string."
  (with-temp-buffer
    (insert html)
    (shr-insert-document (libxml-parse-html-region (point-min) (point-max)))
    (buffer-string)))

(defun chan-viewer-count-you (text)
  "Count occurrences of '(You)' in TEXT."
  (if text
      (with-temp-buffer
        (insert text)
        (how-many "(You)" (point-min) (point-max)))
    0))

(defun chan-viewer-format-time (timestamp)
  "Format Unix TIMESTAMP to human-readable string."
  (format-time-string "%Y-%m-%d %H:%M:%S" (seconds-to-time timestamp)))

;; Pagination
(defun chan-viewer-next-page ()
  "Go to the next catalog page."
  (interactive)
  (setq chan-viewer-catalog-page (1+ chan-viewer-catalog-page))
  (chan-viewer-render-catalog))

(defun chan-viewer-prev-page ()
  "Go to the previous catalog page."
  (interactive)
  (when (> chan-viewer-catalog-page 1)
    (setq chan-viewer-catalog-page (1- chan-viewer-catalog-page))
    (chan-viewer-render-catalog)))

;; Refresh
(defun chan-viewer-refresh ()
  "Refresh the current view (catalog or thread)."
  (interactive)
  (cond
   ((eq major-mode 'chan-viewer-catalog-mode)
    (chan-viewer-render-catalog))
   ((eq major-mode 'chan-viewer-thread-mode)
    (let ((thread-id (get-text-property (point-min) 'thread-id)))
      (when thread-id
        (chan-viewer-open-thread))))))

(defun chan-viewer-start-auto-refresh ()
  "Start auto-refresh timer if interval is set."
  (when (> chan-viewer-auto-refresh-interval 0)
    (run-at-time chan-viewer-auto-refresh-interval
                 chan-viewer-auto-refresh-interval
                 #'chan-viewer-refresh)))

;; Navigation
(defun chan-viewer-return-to-catalog ()
  "Return to the catalog view from the thread view."
  (interactive)
  (kill-this-buffer)
  (chan-viewer-render-catalog))

;; Entry point
;;;###autoload
(defun chan-viewer ()
  "Start the 4chan viewer."
  (interactive)
  (chan-viewer-render-catalog))

(provide 'chan-viewer)
;;; chan-viewer.el ends here
