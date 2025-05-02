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

;;; Code:

(require 'url)
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

(defcustom chan-mode-image-base "https://i.4cdn.org"
  "Base URL for the 4chan images."
  :type 'string
  :group 'chan-mode)

(defcustom chan-mode-thumb-base "https://t.4cdn.org"
  "Base URL for the 4chan thumbnails."
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

;; Board list
(defvar chan-mode-board-list
  '("a" "b" "c" "d" "e" "f" "g" "gif" "h" "hr" "k" "m" "o" "p" "r" "s" "t" "u" "v" "vg" "vm" "vmg" "vr" "vrpg" "vst" "w" "wg" "i" "ic" "r9k" "s4s" "vip" "qa" "cm" "hm" "lgbt" "y" "3" "aco" "adv" "an" "asp" "bant" "biz" "cgl" "ck" "co" "diy" "fa" "fit" "gd" "hc" "his" "int" "jp" "lit" "mlp" "mu" "n" "news" "out" "po" "pol" "pw" "qst" "sci" "soc" "sp" "tg" "toy" "trv" "tv" "vp" "vt" "wsg" "wsr" "x" "xs")
  "List of valid 4chan boards.")

;; Refresh timer
(defvar chan-mode-refresh-timer nil
  "Timer for auto-refreshing chan-mode buffers.")

;; Mode maps
(defvar chan-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c b") #'chan-mode-select-board)
    (define-key map (kbd "r") #'chan-mode-refresh)
    map)
  "Keymap for `chan-mode'.")

(defvar chan-mode-catalog-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map chan-mode-map)
    (define-key map (kbd "n") #'chan-mode-next-page)
    (define-key map (kbd "p") #'chan-mode-prev-page)
    (define-key map (kbd "RET") #'chan-mode-open-thread)
    map)
  "Keymap for `chan-mode-catalog-mode'.")

(defvar chan-mode-thread-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map chan-mode-map)
    (define-key map (kbd "q") #'chan-mode-return-to-catalog)
    (define-key map (kbd "RET") #'chan-mode-toggle-image-size)
    map)
  "Keymap for `chan-mode-thread-mode'.")

;; Major modes
(define-derived-mode chan-mode special-mode "Chan"
  "Base major mode for 4chan viewer."
  :group 'chan-mode
  (setq buffer-read-only t))

(define-derived-mode chan-mode-catalog-mode chan-mode "Chan-Catalog"
  "Major mode for 4chan catalog view."
  :group 'chan-mode
  (chan-mode-start-auto-refresh))

(define-derived-mode chan-mode-thread-mode chan-mode "Chan-Thread"
  "Major mode for 4chan thread view."
  :group 'chan-mode
  (chan-mode-start-auto-refresh))

;; Utility functions
(defun chan-mode-strip-html (html)
  "Strip HTML tags from HTML string."
  (if (and html (not (string-empty-p html)))
      (replace-regexp-in-string "<[^>]*>" "" html)
    ""))

(defun chan-mode-count-you (text)
  "Count occurrences of '(You)' in TEXT."
  (if text
      (with-temp-buffer
        (insert text)
        (count-matches "(You)" (point-min) (point-max)))
    0))

(defun chan-mode-format-time (timestamp)
  "Format Unix TIMESTAMP to human-readable string."
  (format-time-string "%Y-%m-%d %H:%M:%S"
                      (seconds-to-time timestamp)))

;; API functions
(defun chan-mode-fetch-json (url callback)
  "Fetch JSON from URL and call CALLBACK with parsed data."
  (message "Fetching %s..." url)
  (let ((url-request-method "GET")
        (url-user-agent "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/86.0.4240.111 Safari/537.36"))
    (url-retrieve
     url
     (lambda (status)
       (message "Response received from %s" url)
       (if (plist-get status :error)
           (let ((err (plist-get status :error)))
             (message "Failed to fetch %s: %s" url err)
             (funcall callback nil))
         (goto-char (point-min))
         (if (not (search-forward "\n\n" nil t))
             (progn
               (message "Invalid response format from %s" url)
               (funcall callback nil))
           (condition-case err
               (let ((json-data (json-parse-buffer :object-type 'alist
                                                  :array-type 'list
                                                  :null-object nil
                                                  :false-object nil)))
                 (message "Successfully parsed JSON from %s" url)
                 (funcall callback json-data))
             (error
              (message "JSON parsing error: %s" err)
              (funcall callback nil))))))
     nil t t)))

(defun chan-mode-fetch-boards (callback)
  "Fetch list of valid boards and call CALLBACK."
  (if chan-mode-board-list
      (funcall callback)
    (chan-mode-fetch-json
     (format "%s/boards.json" chan-mode-api-base)
     (lambda (data)
       (when data
         (setq chan-mode-board-list
               (mapcar
                (lambda (board) (alist-get 'board board))
                (alist-get 'boards data))))
       (funcall callback)))))

;; Image handling
(defun chan-mode-insert-image (url scale full-url)
  "Insert image from URL with SCALE, optionally with FULL-URL for toggling."
  (message "Loading image from %s" url)
  (condition-case err
      (let ((buffer (url-retrieve-synchronously url t nil 5)))
        (if (not buffer)
            (insert (format "[Image loading failed: no response]"))
          (with-current-buffer buffer
            (goto-char (point-min))
            (if (not (search-forward "\n\n" nil t))
                (insert "[Image loading failed: invalid response]")
              (let ((image-data (buffer-substring (point) (point-max))))
                (if (< (length image-data) 100)
                    (insert "[Image too small or not found]")
                  (condition-case img-err
                      (let ((image (create-image image-data nil t :scale scale)))
                        (if (not image)
                            (insert "[Failed to create image]")
                          (insert (propertize " "
                                             'display image
                                             'image-url url
                                             'full-url full-url
                                             'current-scale scale
                                             'mouse-face 'highlight
                                             'help-echo "RET to toggle image size"))))
                    (error (insert (format "[Image creation failed: %s]" img-err))))))))))
    (error (insert (format "[Image request failed: %s]" err)))))

(defun chan-mode-toggle-image-size ()
  "Toggle between thumbnail and full-size image at point."
  (interactive)
  (let ((url (get-text-property (point) 'image-url))
        (full-url (get-text-property (point) 'full-url))
        (current-scale (get-text-property (point) 'current-scale)))
    (if (not (and url full-url))
        (message "No image at point")
      (message "Toggling image size...")
      (let ((inhibit-read-only t)
            (new-url (if (equal url full-url)
                        (replace-regexp-in-string "i\\.4cdn" "t.4cdn" url)
                      full-url))
            (new-scale (if (equal url full-url)
                          chan-mode-thumbnail-scale
                        chan-mode-full-image-scale)))
        (save-excursion
          (let ((start (point)))
            (if (not (get-text-property start 'display))
                (message "No image display property at point")
              (delete-char 1)
              (chan-mode-insert-image new-url new-scale full-url))))))))

;; Thread view functions
(defun chan-mode-insert-thread-post (post thread-id)
  "Insert a single POST into the THREAD-ID view."
  (let* ((no (alist-get 'no post))
         (name (or (alist-get 'name post) "Anonymous"))
         (trip (alist-get 'trip post))
         (time (alist-get 'time post))
         (com (alist-get 'com post))
         (tim (alist-get 'tim post))
         (ext (alist-get 'ext post))
         (thumb (when tim
                  (format "%s/%s/%ss.jpg"
                          chan-mode-thumb-base
                          chan-mode-board
                          tim)))
         (full (when tim
                 (format "%s/%s/%s%s"
                         chan-mode-image-base
                         chan-mode-board
                         tim
                         ext)))
         (is-op (eq no thread-id))
         (you-count (chan-mode-count-you com)))
    
    ;; Insert post header
    (insert (propertize 
             (format "Post %d by %s%s [%s] (You: %d)\n"
                     no
                     name
                     (or trip "")
                     (chan-mode-format-time time)
                     you-count)
             'face (if is-op 'chan-mode-op-face 'chan-mode-metadata-face)))
    
    ;; Insert image if available
    (when thumb
      (chan-mode-insert-image thumb chan-mode-thumbnail-scale full))
    
    ;; Insert comment if available
    (when com
      (insert (propertize (chan-mode-strip-html com)
                         'face 'font-lock-string-face)))
    
    (insert "\n\n")))

(defun chan-mode-open-thread ()
  "Open the thread under point in the catalog view."
  (interactive)
  (let ((thread-id (get-text-property (point) 'thread-id)))
    (if (not thread-id)
        (message "No thread at point. Move cursor to a thread header.")
      (message "Loading thread %d..." thread-id)
      
      ;; Create and setup buffer first
      (let ((buffer (get-buffer-create chan-mode-thread-buffer)))
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "Loading 4chan /%s/ Thread %d...\n"
                           chan-mode-board thread-id))
            (chan-mode-thread-mode)))
        
        ;; Display buffer immediately
        (switch-to-buffer buffer))
      
      ;; Then fetch the data
      (chan-mode-fetch-json
       (format "%s/%s/thread/%d.json" chan-mode-api-base chan-mode-board thread-id)
       (lambda (data)
         (with-current-buffer chan-mode-thread-buffer
           (let ((inhibit-read-only t))
             (erase-buffer)
             (if (not data)
                 (insert (format "Error: Failed to fetch thread data for /%s/%d\n" 
                                chan-mode-board thread-id))
               (insert (propertize 
                        (format "4chan /%s/ Thread %d\n\n" chan-mode-board thread-id)
                        'thread-id thread-id))
               
               (let ((posts (alist-get 'posts data)))
                 (if (not posts)
                     (insert "Error: No posts found in thread\n")
                   (dolist (post posts)
                     (chan-mode-insert-thread-post post thread-id)))))
             
             (goto-char (point-min))
             (message "Thread %d loaded" thread-id))))))))

(defun chan-mode-return-to-catalog ()
  "Return to the catalog view from the thread view."
  (interactive)
  (when (get-buffer chan-mode-thread-buffer)
    (kill-buffer chan-mode-thread-buffer))
  (if (get-buffer chan-mode-catalog-buffer)
      (switch-to-buffer chan-mode-catalog-buffer)
    (chan-mode-render-catalog)))

;; Catalog view functions
(defun chan-mode-insert-catalog-thread (thread)
  "Insert a single THREAD into the catalog view."
  (let* ((no (alist-get 'no thread))
         (replies (alist-get 'replies thread))
         (images (alist-get 'images thread))
         (sub (alist-get 'sub thread))
         (com (alist-get 'com thread))
         (tim (alist-get 'tim thread))
         (ext (alist-get 'ext thread))
         (thumb (when tim
                  (format "%s/%s/%ss.jpg"
                          chan-mode-thumb-base
                          chan-mode-board
                          tim)))
         (full (when tim
                 (format "%s/%s/%s%s"
                         chan-mode-image-base
                         chan-mode-board
                         tim
                         ext))))
    
    ;; Insert thread header with properties
    (insert (propertize 
             (format "Thread %d (%d replies, %d images)\n"
                     no
                     (or replies 0)
                     (or images 0))
             'thread-id no
             'face 'font-lock-function-name-face
             'mouse-face 'highlight
             'help-echo "RET to open thread"))
    
    ;; Insert thumbnail if available
    (when thumb
      (chan-mode-insert-image thumb chan-mode-thumbnail-scale full))
    
    ;; Insert subject if available
    (when (and sub (not (string-empty-p sub)))
      (insert (propertize 
               (format "Subject: %s\n" (chan-mode-strip-html sub))
               'face 'font-lock-keyword-face 
               'thread-id no)))
    
    ;; Insert comment if available (truncated)
    (when (and com (not (string-empty-p com)))
      (let ((stripped-com (chan-mode-strip-html com)))
        (insert (propertize 
                 (if (> (length stripped-com) 300)
                     (concat (substring stripped-com 0 300) "...")
                   stripped-com)
                 'face 'font-lock-string-face 
                 'thread-id no))))
    
    (insert "\n\n")))

(defun chan-mode-render-catalog ()
  "Render the catalog view for the current board and page in a new buffer."
  (message "Fetching catalog data for /%s/..." chan-mode-board)
  
  ;; Create and setup buffer first if it doesn't exist
  (unless (get-buffer chan-mode-catalog-buffer)
    (with-current-buffer (get-buffer-create chan-mode-catalog-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Loading 4chan /%s/ catalog...\n" chan-mode-board))
        (chan-mode-catalog-mode))
      (switch-to-buffer (current-buffer))))
  
  ;; Fetch the data
  (chan-mode-fetch-json
   (format "%s/%s/catalog.json" chan-mode-api-base chan-mode-board)
   (lambda (data)
     (with-current-buffer (get-buffer-create chan-mode-catalog-buffer)
       (let ((inhibit-read-only t))
         (erase-buffer)
         (insert (format "4chan /%s/ Catalog (Page %d)\n\n"
                        chan-mode-board
                        chan-mode-catalog-page))
         
         ;; Check if we have valid data
         (if (not data)
             (insert (format "Error: Failed to fetch catalog data for /%s/\n" 
                            chan-mode-board))
           (if (not (listp data))
               (insert "Error: Invalid data received from API\n")
             (let ((page-data (nth (min (1- chan-mode-catalog-page) 
                                       (1- (length data)))
                                  data)))
               (if (not page-data)
                   (insert "Error: Page data not found\n")
                 (let ((threads (alist-get 'threads page-data)))
                   (if (not threads)
                       (insert "No threads found on this page.\n")
                     (dolist (thread threads)
                       (chan-mode-insert-catalog-thread thread))))))))
         
         (chan-mode-catalog-mode)
         (goto-char (point-min))
         (message "Catalog for /%s/ loaded" chan-mode-board))))))

;; Navigation and refresh functions
(defun chan-mode-next-page ()
  "Go to the next catalog page."
  (interactive)
  (setq chan-mode-catalog-page (1+ chan-mode-catalog-page))
  (message "Loading page %d..." chan-mode-catalog-page)
  (chan-mode-render-catalog))

(defun chan-mode-prev-page ()
  "Go to the previous catalog page."
  (interactive)
  (when (> chan-mode-catalog-page 1)
    (setq chan-mode-catalog-page (1- chan-mode-catalog-page))
    (message "Loading page %d..." chan-mode-catalog-page)
    (chan-mode-render-catalog)))

(defun chan-mode-refresh ()
  "Refresh the current view (catalog or thread)."
  (interactive)
  (message "Refreshing...")
  (cond
   ((eq major-mode 'chan-mode-catalog-mode)
    (chan-mode-render-catalog))
   ((eq major-mode 'chan-mode-thread-mode)
    (let ((thread-id (get-text-property (point-min) 'thread-id)))
      (if thread-id
          (chan-mode-open-thread)
        (message "No thread ID found, returning to catalog")
        (chan-mode-render-catalog))))))

(defun chan-mode-start-auto-refresh ()
  "Start auto-refresh timer if interval is set."
  (when (> chan-mode-auto-refresh-interval 0)
    ;; Cancel existing timer if any
    (when chan-mode-refresh-timer
      (cancel-timer chan-mode-refresh-timer))
    ;; Start new timer
    (setq chan-mode-refresh-timer
          (run-at-time chan-mode-auto-refresh-interval
                      chan-mode-auto-refresh-interval
                      #'chan-mode-refresh))))

(defun chan-mode-select-board ()
  "Prompt for a board in the minibuffer and switch to it."
  (interactive)
  (let ((board (completing-read "Select board: " chan-mode-board-list nil t)))
    (when board
      (setq chan-mode-board board)
      (setq chan-mode-catalog-page 1)
      (message "Switching to board /%s/" board)
      (chan-mode-render-catalog))))

;; Entry point
;;;###autoload
(defun chan-mode ()
  "Start the 4chan viewer."
  (interactive)
  (setq chan-mode-catalog-page 1)
  (message "Starting chan-mode for board /%s/..." chan-mode-board)
  
  ;; Create and display buffer immediately
  (with-current-buffer (get-buffer-create chan-mode-catalog-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "Loading 4chan /%s/ catalog...\n" chan-mode-board))
      (chan-mode-catalog-mode))
    (switch-to-buffer (current-buffer)))
  
  ;; Then fetch the data
  (chan-mode-render-catalog))

(provide 'chan-mode)
;;; chan-mode.el ends here
