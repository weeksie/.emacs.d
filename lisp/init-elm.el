(require-package 'elm-mode)

(require 'elm-format)

(add-auto-mode 'elm-mode
               "\\.elm$")

(setq elm-format-on-save t)
(setq elm-format-command "elm-format-0.18")

; duped from init-elixir
(defun elm-insert-pipe ()
  (interactive)
  (princ "\n" (current-buffer))
  (indent-according-to-mode)
  (princ "|> " (current-buffer)))

(add-hook 'elm-mode-hook (lambda ()
                              (progn
                                (define-key elm-mode-map [M-return] 'elm-insert-pipe))))
(provide 'init-elm)
