(require-package 'markdown-mode)

(add-auto-mode 'markdown-mode
                  "\\.md"
                  "\\.markdown")


(add-hook 'markdown-mode-hook 'turn-off-delete-trailing-whitespace)
(provide 'init-markdown)
