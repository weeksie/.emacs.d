(require-package 'octave)

(add-auto-mode 'octave-mode "\\.m$")

(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))

(provide 'init-octave)
