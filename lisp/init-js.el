(require-package 'json-mode)
(require-package 'js-comint)
(require-package 'web-mode)
(require-package 'flycheck)
(require-package 'rjsx-mode)
(require-package 'rainbow-delimiters)

(require 'flycheck) ; no idea why this is needed for flycheck but not the others

(maybe-require-package 'js2-mode)
(maybe-require-package 'coffee-mode)


(add-auto-mode 'js2-mode "\\.js$")
(add-auto-mode 'rjsx-mode
               "components\\/.*\\.js$"
               "containers\\/.*\\.js$"
               "pages\\/.*\\.js$")


(add-auto-mode 'web-mode
               "\\.html$"
               "\\.jsp$"
               "\\.erb$"
               "\\.eex$"
               "\\.php$")

;; (add-auto-mode 'js2-mode
;;                "\\.js$"
;;                "\\.ejs$")

(add-auto-mode 'json-mode
               "\\.eslintrc"
               "\\.ftppass"
               "\\.json$")

(add-auto-mode 'coffee-mode
               "\\.coffee$"
               "\\.cjsx"
               "\\.em$")


(defun custom-js2-mode-hook ()
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (c-toggle-auto-state 0)
  (c-toggle-hungry-state 1)
  (subword-mode 1)
  (lambda () (interactive) (column-marker-1 100))
  (setq js2-basic-offset 2)
  (setq js2-strict-trailing-comma-warning nil)
  (setq js2-global-externs '("module" "require" "jQuery" "$" "_" "buster"
                             "sinon" "assert" "refute" "setTimeout" "clearTimeout"
                             "setInterval" "clearInterval" "location" "__dirname"
                             "beforEach" "describe" "it" "expect"
                             "console" "JSON"))
  (define-key js2-mode-map [(return)] 'newline-and-indent)
  (define-key js2-mode-map [(backspace)] 'c-electric-backspace)
  (define-key js2-mode-map [(control d)] 'c-electric-delete-forward)
  (define-key js2-mode-map [(control meta q)] 'my-indent-sexp)
  (define-key js2-mode-map [(control c)(control c)] 'js-send-buffer-and-go)
  (set-variable 'tab-width 2))


(dolist (hook '(js2-mode-hook js-mode-hook json-mode-hook))
  (add-hook hook 'rainbow-delimiters-mode))

(setq inferior-js-program-command "node --harmony")
(setq inferior-js-mode-hook
      (lambda ()
        ;; We like nice colors
        (ansi-color-for-comint-mode-on)
        ;; Deal with some prompt nonsense
        (add-to-list 'comint-preoutput-filter-functions
                     (lambda (output)
                       (replace-regexp-in-string ".*1G\.\.\..*5G" "..."
                     (replace-regexp-in-string "\033\\[[0-9]+[A-Z]" "" output))))))

(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

(defun shell-region (start end)
  "execute region in an inferior shell"
  (interactive "r")
  (shell-command  (buffer-substring-no-properties start end)))

(setq js-indent-level 2)
(setq js-switch-indent-offset 2)

(flycheck-add-mode 'javascript-eslint 'js2-mode)

(setq flycheck-eslintrc ".eslintrc")

(add-hook 'js2-mode-hook 'custom-js2-mode-hook)
(add-hook 'js2-mode-hook 'flycheck-mode)

(with-eval-after-load 'flycheck
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'css-csslint 'web-mode))

(provide 'init-js)
