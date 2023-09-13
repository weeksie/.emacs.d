(use-package yasnippet
  :straight t
  :ensure t
  :config
  (add-hook 'after-init-hook 'yas-global-mode))

(use-package company
  :straight t
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq
   company-idle-delay 1
   company-auto-complete t
   company-tooltip-idle-delay 1
   company-require-match nil
   company-tooltip-align-annotations t
   company-dabbrev-other-buffers t
   tab-always-indent 'complete

   company-frontends
   '(company-pseudo-tooltip-unless-just-one-frontend-with-delay
     company-preview-frontend
     company-echo-metadata-frontend)))

(defun sw/project-try-cargo-toml (dir)
  "Try to locate a Rust project."
  (let ((found (locate-dominating-file dir "Cargo.toml")))
    (if (stringp found)`(transient . ,found) nil)))

(use-package project
  :straight t
  :ensure t
  :config
  (add-hook 'project-find-functions 'sw/project-try-cargo-toml nil nil))

(defun sw/setup-eldoc ()
  ;; Show flymake diagnostics first.
  (setq eldoc-documentation-functions
        (cons #'flymake-eldoc-function
              (remove #'flymake-eldoc-function eldoc-documentation-functions)))
  ;; Show all eldoc feedback.
  (setq eldoc-documentation-strategy #'eldoc-documentation-compose)
  (eldoc))

(use-package eglot
  :straight t
  :hook
  ((c-mode
    c++-mode
    rustic-mode
    python-moden
    css-mode
    LaTeX-mode
    typescript-mode
    tsx-mode
    typescript-ts-mode
    tsx-ts-mode) . eglot-ensure)

  ((tsx-ts-mode typescript-ts-mode) . flymake-show-project-diagnostics)

  (eglot-managed-mode . sw/setup-eldoc)

  :config
  (add-to-list 'eglot-server-programs '(typescript-ts-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(tsx-ts-mode . ("typescript-language-server" "--stdio")))
  (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename))

(provide 'init-eglot)
