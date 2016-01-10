;; package-install ace-window
;; Easily switch windows
(global-set-key (kbd "<C-tab>") 'ace-window)

;; package-install avy
;; Jump to visible character in any window
(global-set-key (kbd "M-C-:") 'avy-goto-char)

;; Jump to next matching character
;; http://www.emacswiki.org/emacs/download/iy-go-to-char.el
(add-to-list 'load-path "~/.emacs.d/packages/iy-go-to-char.el")
(require 'iy-go-to-char)
(global-set-key (kbd "C-x j") 'iy-go-up-to-char)
(global-set-key (kbd "C-x J") 'iy-go-up-to-char-backward)