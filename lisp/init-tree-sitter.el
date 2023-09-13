(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;(mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

(use-package tree-sitter)

(use-package tree-sitter-langs
  :straight t
  :ensure t
  :after tree-sitter
  :config
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-ts-mode . typescript))
  (add-to-list 'tree-sitter-major-mode-language-alist '(js-json-mode . json))
  (add-to-list 'tree-sitter-major-mode-language-alist '(tsx-ts-mode . tsx)))

(global-tree-sitter-mode)



(provide 'init-tree-sitter)
