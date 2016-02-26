;; Lisp REPL
;; https://github.com/slime/slime
(add-to-list 'load-path "~/.emacs.d/packages/slime")
(require 'slime-autoloads)
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy))

;; Highlight nested delimiters rainbow colors
;; https://github.com/Fanael/rainbow-delimiters/releases
(add-to-list 'load-path "~/.emacs.d/packages/rainbow-delimiters")
(require 'rainbow-delimiters)
(add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
(custom-set-faces
 '(rainbow-delimiters-depth-1-face ((((class color)) (:foreground "cyan"))))
 '(rainbow-delimiters-depth-2-face ((((class color)) (:foreground "blue"))))
 '(rainbow-delimiters-depth-3-face ((((class color)) (:foreground "dark orange"))))
 '(rainbow-delimiters-depth-4-face ((((class color)) (:foreground "spring green"))))
 '(rainbow-delimiters-depth-5-face ((((class color)) (:foreground "dark gray"))))
 '(rainbow-delimiters-depth-6-face ((((class color)) (:foreground "yellow"))))
 '(rainbow-delimiters-depth-7-face ((((class color)) (:foreground "dark magenta"))))
 '(rainbow-delimiters-depth-8-face ((((class color)) (:foreground "green"))))
 '(rainbow-delimiters-depth-9-face ((((class color)) (:foreground "red"))))
 )