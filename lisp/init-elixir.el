(require-package 'elixir-mode)
(require-package 'alchemist)
(require-package 'flycheck-credo)
(add-auto-mode 'elixir-mode "\\.ex$")
(require-package 'rainbow-mode)

(defun elixir-insert-pipe ()
  (interactive)
  (princ "\n" (current-buffer))
  (princ "|> " (current-buffer))
  (indent-according-to-mode))

(defun elixir-insert-triple-quote ()
  (interactive)
  (princ "\"\"\"\n" (current-buffer))
  (indent-according-to-mode)
  (save-excursion
    (princ "\n\"\"\"" (current-buffer))
    (indent-according-to-mode)))

(add-hook 'elixir-mode-hook (lambda ()
                              (progn
                                (define-key elixir-mode-map [M-return] 'elixir-insert-pipe)
                                (define-key elixir-mode-map (kbd "C-\"") 'elixir-insert-triple-quote))))

(eval-after-load 'flycheck
  '(flycheck-credo-setup))
(add-hook 'elixir-mode-hook 'flycheck-mode)

(add-hook 'elixir-mode-hook 'rainbow-delimiters-mode)

(provide 'init-elixir)
