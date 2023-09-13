(require 'nginx-mode)

(add-to-list 'auto-mode-alist '("/sites-available/.+\\.conf\\'" . nginx-mode))

(provide 'init-nginx)
