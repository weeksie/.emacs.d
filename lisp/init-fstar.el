(require-package 'fstar-mode)

(add-auto-mode 'fstar-mode
               "\\.fst")

(setq fstar-executable "~/bin/fstar")
(setq fstar-smt-executable "~/bin/z3")

(provide 'init-fstar)
