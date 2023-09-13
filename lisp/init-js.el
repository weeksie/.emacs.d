(require-package 'web-mode)
(require-package 'rainbow-delimiters)
;(add-hook 'prog-mode-hook #'lsp)

(add-auto-mode 'web-mode "\\.js$")

(use-package typescript-ts-mode
  :init
  (add-auto-mode 'typescript-ts-mode ".ts$")
  (add-auto-mode 'tsx-ts-mode ".tsx$")
  :hook ((typescript-ts . eldoc))
  :custom
  (treesit-font-lock-level 3))

(add-auto-mode 'web-mode
               "\\.html$"
               "\\.jsp$"
               "\\.erb$"
               "\\.eex$"
               "\\.php$")


(add-auto-mode 'js-json-mode
               "\\.babelrc"
               "\\.eslintrc"
               "\\.ftppass"
               "\\.json$")

(define-derived-mode tailwindcss-mode fundamental-mode "TailwindCSS"
  "A major mode for editing Tailwind CSS within className attributes in TypeScript files.")

(defun shell-region (start end)
  "execute region in an inferior shell"
  (interactive "r")
  (shell-command  (buffer-substring-no-properties start end)))

(setq js-indent-level 2)
(setq sgml-basic-offset 2)
(setq standard-indent 2)
(setq js-switch-indent-offset 2)
(setq typescript-indent-level 2)
;; (setq web-mode-markup-indent-offset 2)
;; (setq web-mode-code-indent-offset 2)
;; ;(setq lsp-enable-on-type-formatting nil)
;; ;(setq lsp-enable-indentation nil)

(setq flycheck-eslintrc ".eslintrc")
(setq company-tooltip-align-annotations t)

(provide 'init-js)
