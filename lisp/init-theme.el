(use-package solarized-theme
  :straight t
  :ensure t
  :config
  (setq solarized-use-less-bold t)
  (setq solarized-use-more-italics nil)
  (setq solarized-distinct-fringe-background t))

(when (window-system)
  (set-frame-font "Fira Code 12"))

(load-theme 'solarized-light-high-contrast t)


;; Native line numbers and fringe setup.
(setq-default display-line-numbers-width 4)

(add-hook 'after-init-hook
          (lambda nil
             (when (fboundp 'mac-auto-operator-composition-mode)
               (mac-auto-operator-composition-mode))
             (toggle-frame-maximized)))

(defun light-mode ()
  (interactive)
  (load-theme 'solarized-light-high-contrast t))

(defun dark-mode ()
  (interactive)
  (load-theme 'solarized-dark t))

(provide 'init-theme)
