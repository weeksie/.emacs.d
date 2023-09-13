;; Bootstrap the `straight.el' package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq-default straight-use-package-by-default t)
;; Defer by default. Most packages should be configured with `:mode' or similar
;; but this is a bit safer and sped up init-time.
;; For cases where `:mode' etc. don't make sense, `:defer nil' explicitly.
(setq-default use-package-always-defer t
              use-package-expand-minimally t)

;; Install diminish as required by `:diminish' with use-package.
;; Note: I don't use anything special here so no need for `:delight',
;; which is slightly heavier than diminish.
(use-package diminish)

(provide 'init-straight)
