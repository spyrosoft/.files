;; Repeat last insert command package
;; http://www.emacswiki.org/emacs/dot-mode.el
(require 'dot-mode)
(add-hook 'find-file-hooks 'dot-mode-on)