(require-package 'racket-mode)
(require-package 'paredit)
(require-package 'geiser-mit)

(add-auto-mode 'racket-mode
               "\\.rkt")

(add-hook 'racket-mode #'paredit-mode)

(provide 'init-racket)
