(require-package 'elixir-mode)
(require-package 'alchemist)

(add-auto-mode 'elixir-mode "\\.ex$")


(defun elixir-insert-pipe ()
  (interactive)
  (princ "\n" (current-buffer))
  (indent-according-to-mode)
  (princ "|> " (current-buffer)))

(add-hook 'elixir-mode-hook (lambda ()
			    (progn
			      (define-key elixir-mode-map [M-return] 'elixir-insert-pipe))))


(provide 'init-elixir)
