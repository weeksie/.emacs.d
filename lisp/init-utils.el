(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))


;;----------------------------------------------------------------------------
;; Handier way to add modes to auto-mode-alist
;;----------------------------------------------------------------------------
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))


;;----------------------------------------------------------------------------
;; String utilities missing from core emacs
;;----------------------------------------------------------------------------
(defun sanityinc/string-all-matches (regex str &optional group)
  "Find all matches for `REGEX' within `STR', returning the full match string or group `GROUP'."
  (let ((result nil)
        (pos 0)
        (group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))

(defun sanityinc/string-rtrim (str)
  "Remove trailing whitespace from `STR'."
  (replace-regexp-in-string "[ \t\n]+$" "" str))


;;----------------------------------------------------------------------------
;; Find the directory containing a given library
;;----------------------------------------------------------------------------
(autoload 'find-library-name "find-func")
(defun sanityinc/directory-of-library (library-name)
  "Return the directory in which the `LIBRARY-NAME' load file is found."
  (file-name-as-directory (file-name-directory (find-library-name library-name))))


;;----------------------------------------------------------------------------
;; Delete the current file
;;----------------------------------------------------------------------------
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))


;;----------------------------------------------------------------------------
;; Rename the current file
;;----------------------------------------------------------------------------
(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (when (file-exists-p filename)
         (rename-file filename new-name 1))
        (rename-buffer new-name)
        (set-visited-file-name new-name)))))

;;----------------------------------------------------------------------------
;; Browse current HTML file
;;----------------------------------------------------------------------------
(defun browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (tramp-tramp-file-p file-name)
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))



(defun unicode-symbol (name)
   "Translate a symbolic name for a Unicode character -- e.g., LEFT-ARROW
 or GREATER-THAN into an actual Unicode character code. "
   (decode-char 'ucs (case name
		;; arrows
                          ('space 160)
                          ('left-arrow 8592)
                          ('up-arrow 8593)
                          ('right-arrow 8594)
                          ('down-arrow 8595)
                          ;; boxes
                          ('double-vertical-bar #X2551)
                          ;; relational operators
                          ('equal #X003d)
                          ('not-equal #X2260)
                          ('identical #X2261)
                          ('not-identical #X2262)
                          ('less-than #X003c)
                          ('greater-than #X003e)
                          ('less-than-or-equal-to #X2264)
                          ('greater-than-or-equal-to #X2265)
                          ;; logical operators
                          ('logical-and #X2227)
                          ('logical-or #X2228)
                          ('logical-neg #X00AC)
                          ;; misc
                          ('nil #X2205)
                          ('horizontal-ellipsis #X2026)
                          ('double-exclamation #X203C)
                          ('prime #X2032)
                          ('double-prime #X2033)
                          ('for-all #X2200)
                          ('there-exists #X2203)
                          ('element-of #X2208)
                          ('vertical-line 45)
                          ('bullet 8226)
                          ;; mathematical operators
                          ('square-root #X221A)
                          ('squared #X00B2)
                          ('cubed #X00B3)
                          ;; letters
                          ('alpha 945)
                          ('beta 946)
                          ('gamma 947)
                          ('delta 948)
                          ('iota 953)
                          ('lambda 955)
                          ('rho 961)
                          ('omega 969)
                          ('epsilon 949)
                          ('eta 951)
                          ('sigma 963))))

(defun substitute-pattern-with-unicode (pattern symbol)
    "Add a font lock hook to replace the matched part of PATTERN with the
     Unicode symbol SYMBOL looked up with UNICODE-SYMBOL."
    (font-lock-add-keywords
    nil `((,pattern
           (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                     ,(unicode-symbol symbol)
                                     'decompose-region)
                             nil))))))
(defun substitute-patterns-with-unicode (patterns)
   "Call SUBSTITUTE-PATTERN-WITH-UNICODE repeatedly."
   (mapcar #'(lambda (x)
               (substitute-pattern-with-unicode (car x)
                                                (cdr x)))
           patterns))

(provide 'init-utils)
