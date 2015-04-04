(require-package 'json-mode)
(maybe-require-package 'js2-mode)
(maybe-require-package 'coffee-mode)
(require-package 'js-comint)

(add-auto-mode 'js2-mode
               "\\.js$"
               "\\.jsx"
               "\\.json$")

(add-auto-mode 'coffee-mode
               "\\.coffee$"
               "\\.cjsx"
               "\\.em$")


(defun custom-js2-mode-hook ()
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (c-toggle-auto-state 0)
  (c-toggle-hungry-state 1)
  (setq js2-basic-offset 2)
  (define-key js2-mode-map [(return)] 'newline-and-indent)
  (define-key js2-mode-map [(backspace)] 'c-electric-backspace)
  (define-key js2-mode-map [(control d)] 'c-electric-delete-forward)
  (define-key js2-mode-map [(control meta q)] 'my-indent-sexp)
  (set-variable 'tab-width 2))

(require-package 'rainbow-delimiters)

(dolist (hook '(js2-mode-hook js-mode-hook json-mode-hook))
  (add-hook hook 'rainbow-delimiters-mode))

(add-hook 'js2-mode-hook 'custom-js2-mode-hook)

(provide 'init-js)
