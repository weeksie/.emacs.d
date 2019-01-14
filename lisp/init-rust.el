(require-package 'rust-mode)
(require-package 'toml-mode)

(add-auto-mode 'rust-mode "\\.rs$")
(add-auto-mode 'toml-mode "\\.toml$")

(provide 'init-rust)
