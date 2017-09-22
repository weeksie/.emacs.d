(require 'ob-elixir)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (sh . t)
   (elixir . t)
   (org . t)
   (ditaa . t)))
