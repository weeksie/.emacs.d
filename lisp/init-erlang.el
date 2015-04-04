(ignore-errors
  (require-package 'erlang))

(when (package-installed-p 'erlang)
  (require 'erlang-start))

(add-auto-mode 'erlang-mode "\\.erl$")


(provide 'init-erlang)
