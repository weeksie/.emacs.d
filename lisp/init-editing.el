(use-package eldoc
  :ensure t
  :config
  (setq eldoc-echo-area-use-multiline-p nil))

(defun nuke-all-buffers ()
  (interactive)
  (mapcar 'kill-buffer (buffer-list))
  (delete-other-windows))

(defun turn-off-delete-trailing-whitespace ()
  (interactive)
  (remove-hook 'before-save-hook 'delete-trailing-whitespace))
(defun turn-on-delete-trailing-whitespace ()
  (interactive)
  (add-hook 'before-save-hook 'delete-trailing-whitespace))


(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun kill-current-line ()
  "An abbreviated method for killing a whole line plus the newline terminal"
  (kill-region (line-beginning-position) (+ (line-end-position) 1)))

(defun copy-current-line ()
  "Similar to the above but copy the text rather than cut it"
  (copy-region-as-kill (line-beginning-position) (+ (line-end-position) 1)))

(defun kill-yank (n)
  (kill-current-line) ;grab current line of text
  (forward-line n)
  (beginning-of-line)
  (yank)
  (forward-line -1)   ;move back to the beginning of the yanked text
  (beginning-of-line))

(defun kill-yank-region (start end n)
  (let ((lines (count-lines start end)))
    (goto-char start)
    (beginning-of-line)
    (kill-line lines))
  (progn
    (forward-line n)
    (yank)
    (if (> 0 n)
        (exchange-point-and-mark))
    (setq deactivate-mark nil)))

(defun copy-yank (n)
  (copy-current-line)
  (forward-line n)
  (beginning-of-line)
  (yank)
  (forward-line -1)
  (beginning-of-line))

(defun kill-yank-up ()
  (interactive)
  (if mark-active
      (kill-yank-region (region-beginning) (region-end) -1)
    (kill-yank -1)))

(defun kill-yank-down ()
  (interactive)
  (if mark-active
      (kill-yank-region (region-beginning) (region-end) 1)
    (kill-yank 1)))

(defun copy-yank-up ()
  (interactive)
  (copy-yank 0))

(defun copy-yank-down ()
  (interactive)
  (copy-yank 1))


(defun random-sort-region (beg end)
  "Sort lines in region randomly."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (eq (random 2) 0)))))))

(defun upcase-rectangle (b e)
  "change chars in rectangle to uppercase"
  (interactive "r")
  (apply-on-rectangle 'upcase-rectangle-line b e))

(defun upcase-rectangle-line (startcol endcol)
  (when (= (move-to-column startcol) startcol)
    (upcase-region (point)
                   (progn (move-to-column endcol 'coerce)
                          (point)))))

(defun egg ()
  (interactive)
  (desktop-change-dir "~/Workspace/eggbasket"))

(defun shoggy ()
  (interactive)
  (desktop-change-dir "~/Workspace/shoggy"))

(defun salad ()
  (interactive)
  (desktop-change-dir "~/Workspace/ggf/just-salad"))

(global-set-key [C-M-up] 'copy-yank-up)
(global-set-key [C-M-down] 'copy-yank-down)
(global-set-key [M-up] 'kill-yank-up)
(global-set-key [M-down] 'kill-yank-down)

(global-set-key [C-right] 'forward-word)
(global-set-key [C-left] 'backward-word)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key "\C-x\C-k" 'kill-region)


(global-set-key "\M--" 'comment-region)
(global-set-key "\M-_" 'uncomment-region)
(global-set-key [kp-delete] 'delete-char)
(global-set-key [C-kp-delete] 'kill-word)
(global-set-key "\C-x\C-a" 'align-regexp)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
;
(global-set-key (kbd "M-`") 'other-window) ; M-` just like in the rest of OS X
(setq mac-command-modifier 'meta mac-option-modifier nil)

(global-hl-line-mode 1)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default column-number-mode 1)
(setq-default case-fold-search t)
(setq-default temp-buffer-max-height 35)

(electric-pair-mode)
(show-paren-mode)

(temp-buffer-resize-mode)

(winner-mode)

(global-set-key [C-M-left] 'winner-undo)
(global-set-key [C-M-right] 'winner-redo)

(global-subword-mode 1)


;; Quit minimising the fucking window on accident
(when window-system
  (progn
    (global-set-key "\C-z" 'save-buffer)
    (global-set-key "\C-x\C-z" 'save-buffer)))


(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(provide 'init-editing)
