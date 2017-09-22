


(add-hook 'org-mode-hook (lambda ()
			    (progn
			      (define-key ruby-mode-map "\C-c\C\x\C\l" 'org-mode-))))


(provide 'init-math)
