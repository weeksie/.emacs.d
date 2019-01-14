;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(when (not package-archive-contents)
    (package-refresh-contents))

(let ((minver "23.3"))
  (when (version<= emacs-version "23.1")
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version<= emacs-version "24")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))



;;; Good for error states.
(defun open-preferences ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key "\C-xp" 'open-preferences)

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(setq ring-bell-function 'ignore)
(setq-default indent-tabs-mode nil)
(setq inhibit-splash-screen t)
(setq one-buffer-one-frame nil)
(setq osx-key-mode nil)
(setq mac-option-modifier nil)
(setq make-backup-files nil)
(setq backup-inhibited t)
(setq auto-save-default nil)
(setq backup-directory-alist `(("." . "~/.emacs.d/.saves")))
(setq backup-by-copying t)


(add-to-list 'custom-theme-load-path (expand-file-name "emacs-color-theme-solarized" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

(when window-system
  (progn
    (set-frame-size (selected-frame) 205 70)
    (split-window-vertically)
    (split-window-horizontally)
    (enlarge-window 20)))


(require 'init-fonts)
(require 'init-theme)
(require 'init-compat)
(require 'init-utils)
(require 'init-site-lisp)
(require 'init-elpa)
(require 'init-exec-path)
(require 'init-ido)
(require 'init-editing)
(require 'init-scratch)

(require 'init-js)
(require 'init-graphql)
(require 'init-lua)
(require 'init-ruby)
(require 'init-rust)
(require 'init-php)
(require 'init-elm)
(require 'init-nxml)
(require 'init-haskell)
(require 'init-elixir)
(require 'init-erlang)
(require 'init-ess)
(require 'init-mmm)
(require 'init-css)
(require 'init-markdown)
(require 'init-yaml)
(require 'init-git)
(require 'init-projectile)
(require 'init-sql)
(require 'init-docker)
(require 'init-octave)
(require 'init-cypher)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (toml-mode rust-mode typescript tide flow-minor-mode flycheck-credo column-marker rjsx-mode js2-mode cypher-mode cypher rainbow ob-elixer slime-js skewer prettier prettier-js prettier-mode emojify alchemist slim-mode ruby-hash-syntax dockerfile-mode avy graphql-mode graphql flx-ido projectile-rails projectile yaml-mode web-mode tidy skewer-less scss-mode sass-mode rainbow-mode rainbow-delimiters php-mode nyan-mode mmm-mode markdown-mode magit lua-mode less-css-mode json-mode js-comint haskell-mode git-timemachine fullframe flycheck exec-path-from-shell ess erlang elm-mode elixir-mode css-eldoc coffee-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#F4EDDB" :foreground "Black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Fira Code")))))
