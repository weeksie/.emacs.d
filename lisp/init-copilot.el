(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :defer t
  ; :hook (((typescript-mode typescript-tsx-mode) . copilot-mode))
  ; :after company
  :config

  ;; (with-eval-after-load 'company
  ;;   (delq 'company-preview-if-just-one-frontend company-frontends))
  (define-key copilot-completion-map (kbd "C->") 'copilot-complete)
  (define-key copilot-completion-map (kbd "M-RET") 'copilot-accept-completion)
  ;; (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

  )

(provide 'init-copilot)
