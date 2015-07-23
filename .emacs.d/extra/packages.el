;; Add third party packages to the load-path
(add-to-list 'load-path "~/.emacs.d/extra/")

;; Repeat last insert command package
;; http://www.emacswiki.org/emacs/dot-mode.el
(require 'dot-mode)
(add-hook 'find-file-hooks 'dot-mode-on)

;; Lisp REPL
;; https://github.com/slime/slime
(add-to-list 'load-path "~/.emacs.d/extra/slime")
(require 'slime-autoloads)
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy))

;; Lisp & Emacs Lisp Mode
;; Use spaces rather than tabs
(add-hook 'lisp-mode-hook (lambda () (set-variable 'indent-tabs-mode nil)))
(add-hook 'emacs-lisp-mode-hook (lambda () (set-variable 'indent-tabs-mode nil)))

;; Indent and dedent Lisp code
;; Origin: list-packages -> adjust-parens
(add-to-list 'load-path "~/.emacs.d/elpa/adjust-parens-3.0/")
(require 'adjust-parens)
(add-hook 'emacs-lisp-mode-hook #'adjust-parens-mode)
(add-hook 'lisp-mode-hook #'adjust-parens-mode)
(local-set-key (kbd "TAB") 'lisp-indent-adjust-parens)
(local-set-key (kbd "<backtab>") 'lisp-dedent-adjust-parens)

;; Go mode
;; https://github.com/dominikh/go-mode.el
(add-to-list 'load-path "~/.emacs.d/extra/go-mode.el")
(require 'go-mode-autoloads)

;; ESS mode for R
;; https://github.com/emacs-ess/ESS
(add-to-list 'load-path "~/.emacs.d/extra/ESS/lisp")
(autoload 'R-mode "ess-site.el" "ESS" t)
(add-to-list 'auto-mode-alist '("\\.R$" . R-mode))
(setq inferior-R-program-name "/usr/bin/R")
;; Note: I use underscores in my variable names, so it was important to
;; remap the underscore ess-smart-S-assign-key to dash. Open this file:
;; ~/.emacs.d/path/to/ESS/lisp/ess-custom.el and change:
;; (defcustom ess-smart-S-assign-key "_"
;; to:
;; (defcustom ess-smart-S-assign-key "-"

;; Git
;; https://github.com/magnars/dash.el.git
(add-to-list 'load-path "~/.emacs.d/extra/dash.el")
;; https://github.com/magit/magit/releases
(add-to-list 'load-path "~/.emacs.d/extra/magit")
(eval-after-load 'info
  '(progn (info-initialize)
          (add-to-list 'Info-directory-list "~/.emacs.d/extra/magit/")))
(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")

;; Highlight nested delimiters rainbow colors
;; https://github.com/Fanael/rainbow-delimiters/releases
(add-to-list 'load-path "~/.emacs.d/extra/rainbow-delimiters.el")
(require 'rainbow-delimiters)
(add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)

;; M-x list-packages
;; Install ace-window
;; Install avy-goto-char
;; Easily switch windows
(global-set-key (kbd "<C-tab>") 'ace-window)
;; Jump to visible character in any window
(global-set-key (kbd "M-C-:") 'avy-goto-char)


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

;; ----------End Built In----------