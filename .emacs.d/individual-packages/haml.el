;; http://www.emacswiki.org/emacs/HamlMode
;; wget https://raw.githubusercontent.com/nex3/haml-mode/master/haml-mode.el
(add-to-list 'load-path "~/.emacs.d/packages/haml-mode")
(require 'haml-mode)
(add-hook 'haml-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (define-key haml-mode-map "\C-m" 'newline-and-indent)))
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))
