;; ESS mode for R
;; https://github.com/emacs-ess/ESS
(add-to-list 'load-path "~/.emacs.d/packages/ESS/lisp")
(autoload 'R-mode "ess-site.el" "ESS" t)
(add-to-list 'auto-mode-alist '("\\.R$" . R-mode))
(setq inferior-R-program-name "/usr/bin/R")
;; Note: I use underscores in my variable names, so it was important to
;; remap the underscore ess-smart-S-assign-key to dash. Open this file:
;; ~/.emacs.d/path/to/ESS/lisp/ess-custom.el and change:
;; (defcustom ess-smart-S-assign-key "_"
;; to:
;; (defcustom ess-smart-S-assign-key "-"