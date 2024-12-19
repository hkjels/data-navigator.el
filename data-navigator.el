;;; data-navigator.el --- Navigate EDN/JSON data structures -*- lexical-binding: t; -*-
;;
;; Author: Henrik Kjerringvåg <henrik@kjerringvag.no>
;; URL: https://github.com/hkjels/data-navigator.el
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (parseedn "1.1.1"))
;; Keywords: data, navigation, edn, json
;;
;;; Commentary:
;;
;; data-navigator-mode is a major mode to navigate EDN and JSON data structures interactively.
;;
;; **Features:**
;; - Load EDN or JSON data from a file or raw input string.
;; - Browse top-level items and drill down into nested structures.
;; - Apply filters to quickly find entries matching a given substring.
;; - Copy and inspect values in a dedicated buffer.
;; - Auto-refresh the display when returning to the top-level view (optional).
;; - Mouse and keyboard navigation supported.
;;
;; **Usage:**
;; - To load data from a file:
;;   (require 'data-navigator)
;;   M-x data-navigator-from-file RET path/to/file.edn
;;
;; - To load data from a string:
;;   (data-navigator-from-input "[:line-bottom {:bar \"baz\"}]")
;;   (data-navigator-from-input "{\"foo\": [1, 2, 3]}")
;;
;; - To start an empty navigator and then feed data later:
;;   M-x data-navigator
;;
;; After loading, use the following keys to navigate:
;; - <return> or <right>: Dive into the selected value if it’s not a leaf.
;; - <backspace> or <left>: Go up one level.
;; - C-f: Apply a filter term to narrow down displayed entries.
;; - X: Clear the data-navigator buffer.
;; - w: Copy the current value to the kill-ring.
;; - o/O: Open the value in a new buffer (same window or other window).
;; - F: Load data from a file (prompts for a filename).
;; - I: Load data from a string input (prompts for data).
;;
;; Click with mouse-1 on keys/values or breadcrumbs to navigate.
;;
;; **Customizations:**
;; - `data-navigator-top-level-display-order`: Whether to bottom-up or top-down the entries.
;; - `data-navigator-highlight-line`: Highlight the current line.
;; - `data-navigator-column-width`: Adjust how keys are aligned in the buffer.
;; - `data-navigator-auto-refresh-interval`: Set how often to auto-refresh at the top-level.
;; - `data-navigator-preferred-edn-mode` and `data-navigator-preferred-json-mode`: Specify preferred major modes for syntax highlighting. If left `nil`, auto-detection will be used.
;;
;; **Dependencies:**
;; - parseedn: For EDN parsing.
;; - json: For JSON parsing.
;;
;; You can install these packages from MELPA or directly from their source repositories.
;;
;;; Code:

(require 'cl-lib)
(require 'hl-line)
(require 'parseedn)
(require 'json)

(defgroup data-navigator nil
  "Navigate EDN/JSON data structures."
  :group 'applications
  :prefix "data-navigator-")

(defcustom data-navigator-root-label "overview"
  "Label used for the root breadcrumb in the header line."
  :type 'string
  :group 'data-navigator)

(defcustom data-navigator-top-level-display-order 'bottom-up
  "Determines the display order of entries at the top level of the Data Navigator.
Possible values are:
- `top-down`  : Display entries in the order they were inserted.
- `bottom-up` : Display entries in reverse of the order they were inserted."
  :type '(choice (const :tag "Display entries from top to bottom" top-down)
                 (const :tag "Display entries from bottom to top" bottom-up))
  :group 'data-navigator)

(defcustom data-navigator-preferred-edn-mode nil
  "Preferred major mode for EDN highlighting.
If nil, auto-detection is used:
 - If `edn-mode` is available, use that.
 - Else if `clojure-ts-mode` is available, use that.
 - Else if `clojure-mode` is available, use that."
  :type '(choice (const :tag "Auto-detect" nil) (function))
  :group 'data-navigator)

(defcustom data-navigator-preferred-json-mode nil
  "Preferred major mode for JSON highlighting.
If nil, auto-detection is used:
 - If `json-mode` is available, use that.
 - Else fallback to `javascript-mode`."
  :type '(choice (const :tag "Auto-detect" nil) (function))
  :group 'data-navigator)

(defcustom data-navigator-buffer-name "*data-navigator*"
  "Default buffer name for data navigator buffers."
  :type 'string
  :group 'data-navigator)

(defcustom data-navigator-value-buffer-name "*data-navigator-value*"
  "Default buffer name for data navigator value buffers."
  :type 'string
  :group 'data-navigator)

(defcustom data-navigator-highlight-line t
  "Use `hl-line-mode` in the `data-navigator` view."
  :type 'boolean
  :group 'data-navigator)

(defcustom data-navigator-column-width 20
  "Width of the column used to display keys."
  :type 'integer
  :group 'data-navigator)

(defcustom data-navigator-auto-refresh-interval 5
  "Interval in seconds to auto-refresh when top-level is entered and data changes."
  :type 'integer
  :group 'data-navigator)

(defface data-navigator-active-filter-face
  '((t :foreground "orange red" :weight bold))
  "Face used in mode line to indicate an active filter.")

(defvar-local data-navigator--current-data (make-vector 0 nil)
  "All current top-level data items.  A vector of EDN/JSON data structures.")

(defvar-local data-navigator--path nil
  "Current navigation path (list of keys/indices) into `data-navigator--current-data` for this buffer.")

(defvar-local data-navigator--current-filter nil
  "Current filter string for entries.  If non-nil, entries are filtered to those that match this string.")

(defvar-local data-navigator--source 'input
  "Indicates the source of the data in this buffer.  Can be any string.")

(defvar-local data-navigator--in-entry nil
  "Non-nil means we have entered into a structure.")

(defvar-local data-navigator--format nil
  "The data-format that was parsed.")

(defvar-local data-navigator--auto-refresh-timer nil
  "Timer for auto-refreshing the display.")

(defvar data-navigator-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "F") #'data-navigator-from-file)
    (define-key map (kbd "I") #'data-navigator-from-input)
    (define-key map (kbd "<return>") #'data-navigator-enter)
    (define-key map (kbd "<backspace>") #'data-navigator-up)
    (define-key map (kbd "<left>") #'data-navigator-up)
    (define-key map (kbd "<right>") #'data-navigator-enter)
    (define-key map (kbd "C-f") #'data-navigator-filter)
    (define-key map (kbd "X") #'data-navigator-clear-buffer)
    (define-key map (kbd "w") #'data-navigator-copy-value)
    (define-key map (kbd "o") #'data-navigator-open-value)
    (define-key map (kbd "O") #'data-navigator-open-value-other-window)
    map)
  "Keymap for `data-navigator-mode`.")

(easy-menu-define data-navigator-menu data-navigator-mode-map
  "Menu for Data Navigator Mode."
  '("Data-Navigator"
    ["Clear Filter" data-navigator-clear-filter :visible data-navigator--current-filter]
    ["Set Filter" data-navigator-filter]
    "---"
    ["Go Up" data-navigator-up]
    ["Clear Buffer" data-navigator-clear-buffer]))

(easy-menu-add data-navigator-menu data-navigator-mode-map)

;; Timer

(defun data-navigator--start-auto-refresh ()
  "Start auto-refresh if `data-navigator--in-entry` is nil."
  (when (and data-navigator-auto-refresh-interval
             (not data-navigator--in-entry)
             (not data-navigator--auto-refresh-timer))
    (setq-local data-navigator--auto-refresh-timer
                (run-with-timer data-navigator-auto-refresh-interval
                                data-navigator-auto-refresh-interval
                                (lambda (buf)
                                  (when (buffer-live-p buf)
                                    (with-current-buffer buf
                                      (when (and data-navigator--in-entry
                                                 (null data-navigator--path))
                                        (data-navigator--draw)))))
                                (current-buffer)))))

(defun data-navigator--stop-auto-refresh ()
  "Stop the auto-refresh timer."
  (when data-navigator--auto-refresh-timer
    (cancel-timer data-navigator--auto-refresh-timer)
    (setq-local data-navigator--auto-refresh-timer nil)))

;; Navigation

(defun data-navigator--go-to-path (path)
  "Go directly to PATH."
  (setq-local data-navigator--path path)
  (if (null path)
      (setq-local data-navigator--in-entry nil)
    (setq-local data-navigator--in-entry t))
  (if data-navigator--in-entry
      (data-navigator--stop-auto-refresh)
    (data-navigator--start-auto-refresh))
  (data-navigator--draw))

(defun data-navigator--leaf-node-p (node)
  "Return non-nil if NODE has no children and is therefore a leaf."
  (let ((entries (data-navigator--node-entries node)))
    (not (and entries (not (null entries))))))

(defun data-navigator-enter ()
  "Enter the value at point."
  (interactive)
  (setq-local data-navigator--in-entry t)
  (let ((val (get-text-property (point) 'data-navigator-value))
        (key (get-text-property (point) 'data-navigator-key)))
    (if val
        (if (data-navigator--leaf-node-p val)
            (user-error "This is a leaf node.  There’s nowhere deeper to navigate")
          (cond
           ((not data-navigator--in-entry)
            ;; Entering a top-level item:
            (setq-local data-navigator--path (list key))
            (data-navigator--draw))
           (t
            ;; Normal navigation: descend deeper
            (setq-local data-navigator--path (append data-navigator--path (list key)))
            (data-navigator--draw))))
      (user-error "Point must be on a node in the data-navigator"))))

(defun data-navigator-up ()
  "Go up one level."
  (interactive)
  (cond
   ((and data-navigator--in-entry
         (<= (length data-navigator--path) 1))
    ;; Going up from the first chosen top-level item back to top-level listing
    (setq-local data-navigator--path nil
                data-navigator--in-entry nil))
   ((and data-navigator--path
         (> (length data-navigator--path) 1))
    ;; Just go one level up in nested structure
    (setq-local data-navigator--path (butlast data-navigator--path)
                data-navigator--in-entry t))
   (data-navigator--in-entry
    ;; If we are at top-level of a file's single structure (entered), go back to not entered
    (setq-local data-navigator--path nil
                data-navigator--in-entry nil)))
  (data-navigator--draw))

;; Mode-line

(defun data-navigator--update-mode-lighter ()
  "Update mode lighter to reflect filter status."
  (setq mode-name
        (if data-navigator--current-filter
            (propertize (format "Data-Navigator[%s]" data-navigator--current-filter)
                        'face 'data-navigator-active-filter-face)
          "Data-Navigator"))
  (force-mode-line-update))

;; Filtering

(defun data-navigator-filter (filter-term)
  "Prompt the user for a FILTER-TERM to filter the current entries.
If FILTER-TERM is empty, clear any active filter."
  (interactive (list (read-string "Filter entries by: ")))
  (setq-local data-navigator--current-filter (if (string-empty-p filter-term) nil filter-term))
  (data-navigator--update-mode-lighter)
  (data-navigator--draw))

(defun data-navigator--filter-entries (entries)
  "Filter ENTRIES by `data-navigator--current-filter`, if any.
ENTRIES is a flat list [k1 v1 k2 v2 ...].
Return only those (k,v) pairs for which either the key or value matches the filter."
  (if (not data-navigator--current-filter)
      entries
    (cl-loop for (k v) on entries by #'cddr
             when (data-navigator--entry-matches-filter k v)
             append (list k v))))

(defun data-navigator-clear-filter ()
  "Clear the current filter and redraw."
  (interactive)
  (data-navigator-filter ""))

(defun data-navigator--entry-matches-filter (key value)
  "Return non-nil if KEY or VALUE matches `data-navigator--current-filter`."
  (let ((filter data-navigator--current-filter)
        (k-str (format "%s" key))
        (v-str (if (stringp value) value (format "%S" value))))
    (or (string-match-p (regexp-quote filter) k-str)
        (string-match-p (regexp-quote filter) v-str))))

;; Value Interaction

(defun data-navigator--value-to-string (value fmt)
  "Convert VALUE to a string representation for display according to FMT."
  (if (equal fmt 'edn)
      (parseedn-print-str value)
    (json-encode value)))

(defun data-navigator-copy-value ()
  "Copy the value at point to the kill ring."
  (interactive)
  (let ((val (get-text-property (point) 'data-navigator-value)))
    (when val
      (kill-new (data-navigator--value-to-string val data-navigator--format)))))

(defun data-navigator--value-buffer ()
  "Create a buffer for inspecting the value at point."
  (let ((val (get-text-property (point) 'data-navigator-value)))
    (when val
      (let ((buf (get-buffer-create data-navigator-value-buffer-name))
            (val (data-navigator--fontify-snippet val data-navigator--format)))
        (with-current-buffer buf
          (erase-buffer)
          (insert val)
          (goto-char (point-min)))
        buf))))

(defun data-navigator-open-value ()
  "Open the value at point in a new buffer for inspection."
  (interactive)
  (let ((buf (data-navigator--value-buffer)))
    (pop-to-buffer-same-window buf)))

(defun data-navigator-open-value-other-window ()
  "Open the value at point in a new buffer for inspection."
  (interactive)
  (let ((buf (data-navigator--value-buffer)))
    (pop-to-buffer buf)))

;; Header line

(defun data-navigator--make-crumb (label path)
  "Make a single crumb with LABEL and PATH.  Clicking it goes to that PATH."
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1]
                (lambda (_event)
                  (interactive "e")
                  (data-navigator--go-to-path path)))
    (propertize label
                'face 'font-lock-function-name-face
                'mouse-face 'highlight
                'help-echo (format "Go to %s" label)
                'keymap map)))

(defun data-navigator--generate-breadcrumbs ()
  "Generate clickable breadcrumbs from `data-navigator--path`."
  (let ((parts data-navigator--path)
        (acc '()))
    (push (data-navigator--make-crumb data-navigator-root-label '()) acc)
    (cl-loop for i from 1 to (length parts)
             do (push (data-navigator--make-crumb
                       (if (eql i 1)
                           (format "%s" data-navigator--source)
                         (format "%s" (nth (1- i) parts)))
                       (cl-subseq parts 0 i)) acc))
    (mapconcat #'identity (reverse acc) " > ")))

(defun data-navigator--set-header-line ()
  "Set the header line to show breadcrumbs."
  (let ((crumbs (data-navigator--generate-breadcrumbs)))
    (setq header-line-format crumbs)))

;; Output

(defun data-navigator--parse-string (str)
  "Parse STR as EDN or JSON, return the data and set the format."
  (let ((edn-val (condition-case nil
                     (with-temp-buffer
                       (insert str)
                       (goto-char (point-min))
                       (parseedn-read))
                   (error nil)))
        (json-val (condition-case nil
                      (let ((json-object-type 'hash-table)
                            (json-array-type 'vector)
                            (json-key-type 'string))
                        (json-read-from-string str))
                    (error nil))))
    (cond
     ((and edn-val (not json-val))
      (setq-local data-navigator--format 'edn)
      edn-val)
     (json-val
      (setq-local data-navigator--format 'json)
      (vector json-val))
     (t (error "Neither EDN nor JSON parsing succeeded")))))

(defun data-navigator--format-key (k)
  "Format a key K as a string."
  (cond
   ((stringp k) k)
   ((symbolp k) (symbol-name k))
   (t (format "%S" k))))

(defun data-navigator--insert-entries (entries)
  "Insert key-value ENTRIES as two-column lines."
  (cl-loop for (k v) on entries by 'cddr
           do (data-navigator--insert-line k v)))

(defun data-navigator--mouse-click (event)
  "Handle mouse click EVENT to enter the value at the clicked line."
  (interactive "e")
  (mouse-set-point event)
  (data-navigator-enter))

(defun data-navigator--mode-by-format (format)
  "Return the major mode symbol for highlighting based on FORMAT."
  (cond
   ((eq format 'edn)
    (or data-navigator-preferred-edn-mode
        (cond
         ((fboundp 'edn-mode) 'edn-mode)
         ((fboundp 'clojure-ts-mode) 'clojure-ts-mode)
         ((fboundp 'clojure-mode) 'clojure-mode))))
   (t
    (or data-navigator-preferred-json-mode
        (if (fboundp 'json-mode)
            'json-mode
          'javascript-mode)))))

(defun data-navigator--fontify-snippet (snippet fmt)
  "Return SNIPPET highlighted according to FMT.

FORMAT should be a symbol like `edn` or `json`.

This function creates a temporary buffer, inserts SNIPPET, turns on MODE,
font-locks the buffer, and then copies the text properties back to a string."
  (with-temp-buffer
    (insert (data-navigator--value-to-string snippet fmt))
    (funcall (data-navigator--mode-by-format fmt))
    (font-lock-ensure)
    (buffer-substring (point-min) (point-max))))

(defun data-navigator--unwrap-node (node)
  "Return NODE unwrapped if it is annotated with :data-navigator--data.
If NODE is annotated, return the :data-navigator--data part; otherwise return NODE as is."
  (if (and (listp node) (plist-get node :data-navigator--data))
      (plist-get node :data-navigator--data)
    node))

(defun data-navigator--get-node (path data)
  "Get the node at PATH in DATA and return it fully unwrapped."
  (let ((node data))
    (dolist (p path node)
      (setq node (data-navigator--get-child node p)))
    (data-navigator--unwrap-node node)))

(defun data-navigator--insert-line (key value)
  "Insert a line with KEY and VALUE, aligned in two columns."
  (let ((left-col-width data-navigator-column-width)
        (key-str (format "%s" (if (and (listp value) (plist-get value :data-navigator--source))
                                  (plist-get value :data-navigator--source)
                                (data-navigator--format-key key))))
        (real-value (data-navigator--unwrap-node value))
        (timestamp (when (and (listp value) (plist-get value :data-navigator--timestamp))
                     (plist-get value :data-navigator--timestamp))))
    (when (and (listp value) (plist-get value :data-navigator--source))
      (setq-local data-navigator--source (plist-get value :data-navigator--source)))
    (when (and (listp value) (plist-get value :data-navigator--format))
      (setq-local data-navigator--format (plist-get value :data-navigator--format)))
    (let ((val-str (data-navigator--fontify-snippet real-value data-navigator--format))
          (line-map (make-sparse-keymap)))
      (define-key line-map [mouse-1] #'data-navigator--mouse-click)
      (let* ((timestamp-text (or (when timestamp
                                   (format " (%s)"
                                           (format-time-string "%Y-%m-%d %H:%M:%S"
                                                               (seconds-to-time timestamp)))) ""))
             (help-text (concat "Click or RET to enter this value" timestamp-text)))
        (insert (propertize (truncate-string-to-width key-str left-col-width 0 ?\s)
                            'face 'font-lock-keyword-face
                            'data-navigator-key key
                            'data-navigator-value real-value
                            'mouse-face 'highlight
                            'help-echo help-text
                            'keymap line-map)
                " "
                (propertize val-str
                            'data-navigator-key key
                            'data-navigator-value real-value
                            'mouse-face 'highlight
                            'help-echo help-text
                            'keymap line-map)
                "\n")))))

(defun data-navigator--get-child (node key)
  "Get child KEY from NODE.
- If NODE is a hash-table, KEY should be a hash key.
- If NODE is a vector or a list, KEY should be an integer index."
  (data-navigator--unwrap-node
   (cond
    ((hash-table-p node)
     (gethash key node))
    ((vectorp node)
     (when (and (integerp key) (>= key 0) (< key (length node)))
       (aref node key)))
    ((consp node)
     (when (integerp key)
       (nth key node))))))

(defun data-navigator--get-node (path data)
  "Get the node at PATH in DATA."
  (let ((node data))
    (dolist (p path node)
      (setq node (data-navigator--get-child node p)))))

(defun data-navigator--node-entries (node)
  "Return entries of NODE as a flat list [k1 v1 k2 v2 ...].
Always treat lists as indexed sequences."
  (cond
   ((null node) nil)
   ((hash-table-p node)
    (let (res)
      (maphash (lambda (k v)
                 (push k res)
                 (push v res)) node)
      (nreverse res)))
   ((vectorp node)
    (cl-loop for i from 0 below (length node)
             append (list i (aref node i))))
   ((consp node)
    (cl-loop for val in node
             for i from 0
             append (list i val)))
   (t nil)))

(defun data-navigator--draw ()
  "Draw the current level of the data structure in the current buffer."
  (let ((buf (get-buffer data-navigator-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (goto-char (point-min))
        (data-navigator--set-header-line)
        (let ((node (if (null data-navigator--path)
                        (if data-navigator--in-entry
                            nil
                          data-navigator--current-data)
                      (data-navigator--get-node data-navigator--path data-navigator--current-data))))
          ;; If we have entered into a structure (data-navigator--path non-nil),
          ;; and the node is annotated, unwrap it to show just the data.
          (when (and data-navigator--path
                     (listp node)
                     (plist-get node :data-navigator--data))
            (setq node (plist-get node :data-navigator--data)))
          (if (null data-navigator--path)
              (cond
               ((not data-navigator--in-entry)
                (let* ((entries (data-navigator--node-entries data-navigator--current-data))
                       (filtered (data-navigator--filter-entries entries)))
                  ;; Convert [k1 v1 k2 v2 ...] into ((k1 v1) (k2 v2) ...)
                  (let ((pairs (cl-loop for (k v) on filtered by #'cddr collect (list k v))))
                    ;; Sort by timestamp if data-navigator-top-level-display-order is 'bottom-up
                    (when (eq data-navigator-top-level-display-order 'bottom-up)
                      (setq pairs (sort pairs
                                        (lambda (a b)
                                          (let ((ta (plist-get (cadr a) :data-navigator--timestamp))
                                                (tb (plist-get (cadr b) :data-navigator--timestamp)))
                                            (> ta tb))))))
                    ;; Flatten pairs back into [k1 v1 k2 v2 ...]
                    (setq filtered (apply #'append pairs))
                    (data-navigator--insert-entries filtered))))
               (t (data-navigator--insert-line (or data-navigator--source data-navigator-root-label) node)))
            (when node
              (let* ((entries (data-navigator--node-entries node))
                     (filtered (data-navigator--filter-entries entries)))
                ;; Inside structures, just display as-is, no sorting by timestamp.
                (data-navigator--insert-entries filtered))))
          (goto-char (point-min)))))))

(defun data-navigator-clear-buffer ()
  "Clear the `data-navigator` buffer."
  (interactive)
  (with-current-buffer (get-buffer data-navigator-buffer-name)
    (setq-local data-navigator--current-data (make-vector 0 nil)
                data-navigator--source nil
                data-navigator--format nil
                data-navigator--path nil
                data-navigator--in-entry nil)
    (data-navigator--draw)))

(defun data-navigator--annotate-data (data)
  "Annotate DATA with the current timestamp."
  (let* ((timestamp (float-time))
         (annotated (mapcar (lambda (item)
                              (list :data-navigator--data item
                                    :data-navigator--timestamp timestamp
                                    :data-navigator--source data-navigator--source
                                    :data-navigator--format data-navigator--format))
                            (append data nil)))
         (annotated-vector (vconcat annotated)))
    (vconcat data-navigator--current-data annotated-vector)))

;;;###autoload
(defun data-navigator ()
  "Create and switch to a new `data-navigator` buffer, ready for input."
  (interactive)
  (let ((buf (get-buffer-create data-navigator-buffer-name)))
    (pop-to-buffer buf)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer))
      (unless (eq major-mode 'data-navigator-mode)
        (data-navigator-mode))
      (data-navigator--draw))))

;;;###autoload
(defun data-navigator-from-input (input &optional source)
  "Start `data-navigator-mode` using INPUT (a string of EDN/JSON).
SOURCE can be any string that identifies where the data is coming from."
  (interactive "sEnter data (EDN/JSON): ")
  (data-navigator)
  (let ((buf (get-buffer data-navigator-buffer-name)))
    (with-current-buffer buf
      (let ((parsed (data-navigator--parse-string input)))
        (goto-char (point-min))
        (let ((inhibit-read-only t))
          (erase-buffer))
        (setq-local data-navigator--source (or source 'input))
        (setq-local data-navigator--current-data (data-navigator--annotate-data parsed))
        (data-navigator--draw)))))

;;;###autoload
(defun data-navigator-from-file (&optional filename)
  "Start `data-navigator-mode` using FILENAME."
  (interactive "fFile: ")
  (let ((input (with-temp-buffer (insert-file-contents filename) (buffer-string))))
    (data-navigator-from-input input (file-name-nondirectory filename))))

;;;###autoload
(define-derived-mode data-navigator-mode special-mode "Data-Navigator"
  "Major mode for navigating EDN/JSON data.
\\{data-navigator-mode-map}"
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (when data-navigator-highlight-line (hl-line-mode 1)))

(provide 'data-navigator)

;;; data-navigator.el ends here
