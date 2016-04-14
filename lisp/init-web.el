(require-package 'web-mode)
(require-package 'flycheck)


(add-auto-mode 'web-mode
               "\\.jsx$"
               "\\.html$"
               "\\.erb$"
               "\\.ejs$"
               "\\.php$")

(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

(defun my-web-mode-hook ()
  "Hooks for Web mode. Adjust indents"
  ;;; http://web-mode.org/
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(add-hook 'web-mode-hook  'my-web-mode-hook)
(add-hook 'web-mode-hook  'flycheck-mode)

(provide 'init-web)
