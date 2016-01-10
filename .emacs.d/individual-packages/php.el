;; https://github.com/ejmr/php-mode/releases
(add-to-list 'load-path "~/.emacs.d/packages/php-mode/php-mode.el")
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(require 'php-mode)