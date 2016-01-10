;; ---------- Individual Packages ----------

(load "~/.emacs.d/individual-packages/essential.el")

(load "~/.emacs.d/individual-packages/lisp.el")
(load "~/.emacs.d/individual-packages/go.el")

(load "~/.emacs.d/individual-packages/git.el")
(load "~/.emacs.d/individual-packages/keyfreq.el")

;(load "~/.emacs.d/individual-packages/R.el")
;(load "~/.emacs.d/individual-packages/haml.el")
;(load "~/.emacs.d/individual-packages/php.el")
;(load "~/.emacs.d/individual-packages/dot.el")



;; ---------- General Package Config ----------

;; Add third party packages to the load-path
(add-to-list 'load-path "~/.emacs.d/packages/")

;; JavaScript IDE
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook (lambda () (set-variable 'tab-width 4)))

;; Shell
(add-hook 'sh-mode-hook (lambda () (set-variable 'tab-width 4)))

;; CSS
(add-hook 'css-mode-hook (lambda () (set-variable 'tab-width 4)))

;; Conf
(add-hook 'conf-mode-hook (lambda () (set-variable 'tab-width 4)))

;; Load Shopify Liquid Templates in HTML Mode
(add-to-list 'auto-mode-alist '("\\.liquid$" . html-mode))

;; Load groovy files in java-mode
(add-to-list 'auto-mode-alist '("\\.groovy$" . java-mode))

;; Use spaces rather than tabs
(add-hook 'lisp-mode-hook (lambda () (set-variable 'indent-tabs-mode nil)))
(add-hook 'emacs-lisp-mode-hook (lambda () (set-variable 'indent-tabs-mode nil)))
(add-hook 'conf-mode-hook (lambda () (set-variable 'indent-tabs-mode nil)))
(add-hook 'java-mode-hook (lambda () (set-variable 'indent-tabs-mode nil)))



;; ----------Built In----------

;; For note taking/list building, such as to-do lists
(require 'org)

;; Prevent line truncation in org mode
(add-hook 'org-mode-hook (lambda () (toggle-truncate-lines -1)))

;; Prevent org mode from overriding <C-tab> functionality
(add-hook 'org-mode-hook '(lambda () (define-key org-mode-map [(control tab)] nil)))

;; Save Place remembers where point was when the file was last closed
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/save-place-log")

;; Rather than zapping through character, zap up to character
(autoload 'zap-up-to-char "misc"
	"Kill up to, but not including ARGth occurrence of CHAR." t)
(global-set-key (kbd "M-z") 'zap-up-to-char)