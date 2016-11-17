;; https://github.com/dominikh/go-mode.el
(add-to-list 'load-path "~/.emacs.d/packages/go-mode.el")
(require 'go-mode-autoloads)

;; https://godoc.org/golang.org/x/tools/cmd/goimports
(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)