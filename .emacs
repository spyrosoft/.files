(setf inhibit-splash-screen t)
(switch-to-buffer (get-buffer-create "emtpy"))
(delete-other-windows)
(menu-bar-mode 0)

(setq make-backup-files nil)

(setq-default word-wrap t)
(setq-default scroll-preserve-screen-position t)
(setq-default recenter-positions '(top middle))
(setq-default scroll-margin 4)
(setq-default show-trailing-whitespace t)

(add-to-list 'load-path "~/.emacs.d/extra/")
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

(add-to-list 'load-path "~/.emacs.d/extra/slime")
(require 'slime-autoloads)

(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy))

(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
