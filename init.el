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
;(desktop-save-mode nil)
;(setq desktop-restore-eager t)
;(set-default-font "Inconsolata-14")
(set-default-font "Consolas-13")
(set-background-color "#fdf6e3")

(when window-system
  (progn
    (set-frame-size (selected-frame) 205 75)
    (split-window-vertically)
    (split-window-horizontally)
    (enlarge-window 20)))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))


(require 'init-compat)
(require 'init-utils)
(require 'init-site-lisp)
(require 'init-elpa)
(require 'init-exec-path)
(require 'init-ido)
(require 'init-editing)

(require 'init-js)
(require 'init-lua)
(require 'init-ruby)
(require 'init-elm)
(require 'init-nxml)
(require 'init-haskell)
(require 'init-elixir)
(require 'init-erlang)
(require 'init-ess)
(require 'init-mmm)
(require 'init-css)
(require 'init-markdown)
