(require-package 'elixir-mode)
(require-package 'alchemist)
(add-auto-mode 'elixir-mode "\\.ex$")
(require 'rainbow-mode)

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

(add-hook 'elixir-mode-hook 'rainbow-delimiters-mode)

(provide 'init-elixir)
