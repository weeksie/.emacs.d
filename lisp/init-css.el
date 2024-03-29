;;; Embedding in html
(require-package 'mmm-mode)
(after-load 'mmm-vars
  (mmm-add-group
   'html-css
   '((css-cdata
      :submode css-mode
      :face mmm-code-submode-face
      :front "<style[^>]*>[ \t\n]*\\(//\\)?<!\\[CDATA\\[[ \t]*\n?"
      :back "[ \t]*\\(//\\)?]]>[ \t\n]*</style>"
      :insert ((?j js-tag nil @ "<style type=\"text/css\">"
                   @ "\n" _ "\n" @ "</style>" @)))
     (css
      :submode css-mode
      :face mmm-code-submode-face
      :front "<style[^>]*>[ \t]*\n?"
      :back "[ \t]*</style>"
      :insert ((?j js-tag nil @ "<style type=\"text/css\">"
                   @ "\n" _ "\n" @ "</style>" @)))
     (css-inline
      :submode css-mode
      :face mmm-code-submode-face
      :front "style=\""
      :back "\"")))
  (dolist (mode (list 'html-mode 'nxml-mode))
    (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-css)))

;;; SASS and SCSS
;; (require-package 'sass-mode)
(require-package 'scss-mode)
(setq-default scss-compile-at-save nil)


;;; LESS
(require-package 'less-css-mode)
(when (featurep 'js2-mode)
  (require-package 'skewer-less))



;;; Auto-complete CSS keywords
(after-load 'auto-complete
  (dolist (hook '(css-mode-hook )) ; sass-mode-hook scss-mode-hook
    (add-hook hook 'ac-css-mode-setup)))


;; ;;; Use eldoc for syntax hints
;; (require-package 'css-eldoc)
;; (autoload 'turn-on-css-eldoc "css-eldoc")
;; (add-hook 'css-mode-hook 'turn-on-css-eldoc)
;; (add-hook 'css-mode-hook (lambda ()
;;                            (setq css-indent-offset 2)
;;                            (setq indent-tabs-mode nil)))


(add-auto-mode 'scss-mode "\\.scss")

(provide 'init-css)
