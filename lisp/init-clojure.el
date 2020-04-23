(require-package 'clojure-mode)
(require-package 'cider)
(require-package 'paredit)

(add-auto-mode 'clojure-mode
               "\\.clj")

(add-hook 'clojure-mode #'paredit-mode)

(provide 'init-clojure)
