(require-package 'json-mode)
(require-package 'js-comint)
(require-package 'web-mode)
(require-package 'flycheck)
(require-package 'rjsx-mode)
(require-package 'rainbow-delimiters)
(require-package 'company)
(require-package 'typescript-mode)

(require 'flycheck) ; no idea why this is needed for flycheck but not the others

(maybe-require-package 'js2-mode)
(maybe-require-package 'coffee-mode)


(add-auto-mode 'js2-mode "\\.js$")
(add-auto-mode 'rjsx-mode
               "templates\\/.*\\.js$"
               "components\\/.*\\.js$"
               "containers\\/.*\\.js$"
               "pages\\/.*\\.js$")


(add-auto-mode 'typescript-mode "\\.ts$")

(add-auto-mode 'web-mode
               "\\.html$"
               "\\.jsp$"
               "\\.erb$"
               "\\.eex$"
               "\\.php$"
               "\\.tsx$")

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
                             "beforeEach" "describe" "it" "expect"
                             "console" "JSON"))
  (define-key js2-mode-map [(return)] 'newline-and-indent)
  (define-key js2-mode-map [(backspace)] 'c-electric-backspace)
  (define-key js2-mode-map [(control d)] 'c-electric-delete-forward)
  (define-key js2-mode-map [(control meta q)] 'my-indent-sexp)
  (define-key js2-mode-map [(control c)(control c)] 'js-send-buffer)
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
(setq js2-indent-level 2)
(setq js3-indent-level 2)
(setq js2-basic-offset 2)
(setq sgml-basic-offset 2)
(setq standard-indent 2)
(setq js-switch-indent-offset 2)
(setq typescript-indent-level 2)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)


(setq flycheck-eslintrc ".eslintrc")

(add-hook 'js2-mode-hook 'custom-js2-mode-hook)
(add-hook 'js2-mode-hook 'flycheck-mode)
;; (add-hook 'js2-mode-hook 'flow-minor-mode)
;; (remove-hook 'js2-mode-hook 'flow-minor-mode t)

(with-eval-after-load 'flycheck
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'css-csslint 'web-mode))


(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

;; aligns annotation to the right hand side
; (setq company-tooltip-align-annotations t)

(provide 'init-js)
