(defun open-preferences ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(global-set-key "\C-xp" 'open-preferences)

; in response to weird error: `Symbol's value as variable is void: native-comp-deferred-compilation-deny-list`
(defvar native-comp-deferred-compilation-deny-list nil)

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

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

(require 'init-straight)
(require 'init-theme)
(require 'init-compat)
(require 'init-utils)
(require 'init-site-lisp)
(require 'init-elpa)
(require 'init-exec-path)
(require 'init-ido)
(require 'init-editing)
(require 'init-scratch)
(require 'init-skeleton)

;(require 'init-gpt-n)

(require 'init-tree-sitter)
(require 'init-eglot)
;(require 'init-copilot)
(require 'init-flymake)
(require 'init-js)
(require 'init-graphql)
(require 'init-lua)
;(require 'init-ruby)
(require 'init-rust)
;(require 'init-php)
;(require 'init-web)
;(require 'init-elm)
(require 'init-nxml)
;(require 'init-haskell)
(require 'init-elixir)
(require 'init-erlang)
;(require 'init-ess)
;(require 'init-mmm)
;(require 'init-css)
;(require 'init-markdown)
;(require 'init-yaml)
(require 'init-git)
;(require 'init-projectile)
(require 'init-sql)
;(require 'init-docker)
;(require 'init-octave)
;(require 'init-cypher)
;(require 'init-solidity)
(require 'init-clojure)
;(require 'init-zil)
;(require 'init-idris)
;(require 'init-alloy)
;(require 'init-fstar)
;(require 'init-racket)
(require 'init-nginx)
;(require 'init-typing)
;(toggle-debug-on-error)

;; HACK: repeating here because something is overwriting this.
(global-set-key "\C-xp" 'open-preferences)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yassnippet yas-snippet tree-sitter-langs tree-sitter eglot lsp-ui use-package rustic rustic-mode liquid liquid-mode lsp-treemacs treemacs lsp-mode geiser-mit racket-mode tla-mode tla-pcal-mode fstar-mode alloy-mode idris-mode solarized-theme tide tide-mode js2-mode paredit cider clojure-mode solidity-flycheck company-solidity solidity-mode cypher-mode dockerfile-mode projectile-rails projectile magit yaml-mode markdown-mode css-eldoc web-mode typescript-mode toml-mode rust-mode ruby-hash-syntax rjsx-mode rainbow-mode rainbow-delimiters php-mode nyan-mode mmm-mode lua-mode json-mode js-comint haskell-mode graphql-mode fullframe flycheck-credo flx-ido exec-path-from-shell ess erlang elm-mode coffee-mode avy alchemist))
 '(safe-local-variable-values
   '((typescript-indent-offset . 4)
     (typescript-indent-offset . 2)
     (web-mode-markup-indent-offset . 2)
     (web-mode-code-indent-offset . 2)
     (standard-indent . 2)
     (js2-basic-offset . 2)
     (js2-indent-level . 2)
     (js-indent-level . 2)
     (web-mode-markup-indent-offset . 4)
     (web-mode-code-indent-offset . 4)
     (js-indent-level . 4)
     (standard-indent . 4)
     (js2-basic-offset . 4)
     (js2-indent-level . 4))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
