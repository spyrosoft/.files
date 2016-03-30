;; http://jblevins.org/projects/markdown-mode/
;; wget http://jblevins.org/projects/markdown-mode/markdown-mode.el
(add-to-list 'load-path "~/.emacs.d/packages/markdown-mode")
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))