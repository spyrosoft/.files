;; wget https://raw.githubusercontent.com/rust-lang/rust-mode/master/rust-mode.el
(add-to-list 'load-path "~/.emacs.d/packages/rust-mode.el")
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))