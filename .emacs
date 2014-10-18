(add-to-list 'load-path "~/.emacs.d/extra/")

(setf inhibit-splash-screen t)
(switch-to-buffer (get-buffer-create "emtpy"))
(delete-other-windows)

(setq make-backup-files nil)

(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

(add-to-list 'load-path "~/.emacs.d/extra/slime")
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy))
(require 'slime-autoloads)

(menu-bar-mode 0)
