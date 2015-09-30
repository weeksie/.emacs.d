(require-package 'json-mode)
(require-package 'js-comint)
(require-package 'web-mode)
(require-package 'flycheck)

(require 'flycheck) ; no idea why this is needed for flycheck but not the others

(maybe-require-package 'js2-mode)
(maybe-require-package 'coffee-mode)


(add-auto-mode 'web-mode
               "\\.js$"
               "\\.jsx$"
               "\\.html$"
               "\\.jsp$"
               "\\.erb$"
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

(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

(defun my-web-mode-hook ()
  "Hooks for Web mode. Adjust indents"
  ;;; http://web-mode.org/
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (add-to-list 'web-mode-comment-formats '("javascript" . "//" ))
  (define-key web-mode-map [(control c)(control c)] 'js-send-buffer-and-go)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(require-package 'rainbow-delimiters)

(dolist (hook '(js2-mode-hook js-mode-hook json-mode-hook))
  (add-hook hook 'rainbow-delimiters-mode))

(defun js-unicode ()
  (interactive)
  (substitute-patterns-with-unicode
   (list ;; (cons "[:=]\s+\\(function\\(()\\)?\\)" 'lambda)
         ;; (cons "\\(&&\\)" 'logical-and)
         ;; (cons "\\(||\\)" 'logical-or)
         (cons "\\(!==\\)" 'not-equal)
         (cons "\\(>=\\)" 'greater-than-or-equal-to)
         (cons "\\(<=\\)" 'less-than-or-equal-to))))


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


(setq web-mode-content-types-alist '(("jsx"  . "\\.js[x]?\\'")))

(add-hook 'js2-mode-hook 'custom-js2-mode-hook)
(add-hook 'js2-mode-hook 'js-unicode)
(add-hook 'js2-mode-hook 'flycheck-mode)

(add-hook 'web-mode-hook  'my-web-mode-hook)
(add-hook 'web-mode-hook  'flycheck-mode)
(add-hook 'web-mode-hook  'js-unicode)

(flycheck-add-mode 'javascript-eslint 'js2-mode)
(flycheck-add-mode 'javascript-eslint 'web-mode)

(provide 'init-js)
