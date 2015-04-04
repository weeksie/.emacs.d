(require-package 'ruby-mode)

(add-auto-mode 'ruby-mode
               "\\.r[bs]$"
               "Rakefile"
               "Guardfile"
               "Gemfile"
               "\\.rake$"
               "\\.gemspec$"
               "\\.god$"
               "\\.rabl$"
               "\\.deface$")

(defun ruby-eval-buffer () (interactive)
   "Evaluate the buffer with ruby."
   (shell-command-on-region (point-min) (point-max) "ruby -W0"))

(defun ruby-electric-space (arg)
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  (save-excursion
    (cond ((is-alone-on-line "\\(def\\|if\\|class\\|module\\)")
         (progn
           (ruby-indent-line t)
           (newline)
           (ruby-insert-end))))))

(defun is-alone-on-line (kw)
  (interactive "P")
  (let* (
      (rx1 (concat kw "[[:space:]]$"))
      (rx2 (concat "[[:space:]]*" rx1)))
      (save-excursion
        (forward-word -1)
        (and (looking-at rx1)
             (progn
             (beginning-of-line)
             (looking-at rx2))))))


(defun ruby-electric-ret (arg)
  (interactive "P")
  (newline)
  (ruby-indent-line t))

(defun ruby-auto-indent (begin end)
  "Indents region and aligns on = signs"
  (interactive "r")
  (if (and mark-active (/= begin end))
      (progn
	(indent-region begin end)
	(align-regexp begin end  "\\(\\s-*\\)=" 1 1 nil))
    (indent-according-to-mode)))


(add-hook 'ruby-mode-hook (lambda ()
			    (progn
			      (gd-add-to-mode)
			      (define-key ruby-mode-map "\C-c\C-c" 'ruby-eval-buffer)
			      (define-key ruby-mode-map "\C-xrh" 'ruby-align-hash)
			      (define-key ruby-mode-map "\t" 'ruby-auto-indent)
			      (define-key ruby-mode-map " " 'ruby-electric-space)
			      (define-key ruby-mode-map "\r" 'ruby-electric-ret))))

(provide 'init-ruby)
