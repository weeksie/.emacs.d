(use-package web-mode
  :straight t
  :ensure t
  :mode ("\\.liquid" "\\.mjml")
  :config

  (setq
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-style-padding 2
   web-mode-script-padding 2
   web-mode-enable-auto-closing t
   web-mode-enable-auto-opening t
   web-mode-enable-auto-pairing t
   web-mode-enable-auto-indentation t
   electric-pair-mode 0)

  (dolist (alist web-mode-engines-auto-pairs)
    (setcdr alist
            (cl-loop for pair in (cdr alist)
                     unless (string-match-p "^[a-z-]" (cdr pair))
                     collect (cons (car pair)
                                   (string-trim-right (cdr pair)
                                                      "\\(?:>\\|]\\|}\\)+\\'")))))
)

(provide 'init-web)
