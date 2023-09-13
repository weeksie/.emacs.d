(use-package rustic
  :straight t
  :ensure t
  :mode ("\\.rs" . rustic-mode)
  :config
  (setq rustic-lsp-client 'eglot))

(use-package toml-mode
  :straight t
  :ensure t
  :mode ("\\.toml" . toml-mode))

(provide 'init-rust)
