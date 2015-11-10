;; Add third party packages to the load-path
(add-to-list 'load-path "~/.emacs.d/extra/")

;; package-install ace-window
;; Easily switch windows
(global-set-key (kbd "<C-tab>") 'ace-window)

;; package-install avy
;; Jump to visible character in any window
(global-set-key (kbd "M-C-:") 'avy-goto-char)

;; JavaScript IDE
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook (lambda () (set-variable 'tab-width 4)))

;; Shell
(add-hook 'sh-mode-hook (lambda () (set-variable 'tab-width 4)))

;; CSS
(add-hook 'css-mode-hook (lambda () (set-variable 'tab-width 4)))

;; Lisp, Emacs Lisp, & Java Modes
;; Use spaces rather than tabs
(add-hook 'lisp-mode-hook (lambda () (set-variable 'indent-tabs-mode nil)))
(add-hook 'emacs-lisp-mode-hook (lambda () (set-variable 'indent-tabs-mode nil)))
(add-hook 'java-mode-hook (lambda () (set-variable 'indent-tabs-mode nil)))

;; Lisp REPL
;; https://github.com/slime/slime
(add-to-list 'load-path "~/.emacs.d/extra/slime")
(require 'slime-autoloads)
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy))

;; Indent and dedent Lisp code
;; Origin: list-packages -> adjust-parens
(add-to-list 'load-path "~/.emacs.d/elpa/adjust-parens-3.0/")
(require 'adjust-parens)
(add-hook 'emacs-lisp-mode-hook #'adjust-parens-mode)
(add-hook 'lisp-mode-hook #'adjust-parens-mode)
(local-set-key (kbd "TAB") 'lisp-indent-adjust-parens)
(local-set-key (kbd "<backtab>") 'lisp-dedent-adjust-parens)

;; Highlight nested delimiters rainbow colors
;; https://github.com/Fanael/rainbow-delimiters/releases
(add-to-list 'load-path "~/.emacs.d/extra/rainbow-delimiters.el")
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

;; Go mode
;; https://github.com/dominikh/go-mode.el
(add-to-list 'load-path "~/.emacs.d/extra/go-mode.el")
(require 'go-mode-autoloads)

;; Shopify Liquid Templates in HTML Mode
(add-to-list 'auto-mode-alist '("\\.liquid$" . html-mode))

;; Key Frequency Analysis
;; https://github.com/dacap/keyfreq
(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

;; HAML
;; http://www.emacswiki.org/emacs/HamlMode
;; https://github.com/nex3/haml-mode/blob/master/haml-mode.el
(add-to-list 'load-path "~/.emacs.d/extra/haml-mode.el")
(require 'haml-mode)
(add-hook 'haml-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (define-key haml-mode-map "\C-m" 'newline-and-indent)))
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))

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

;; Jump to next matching character
;; http://www.emacswiki.org/emacs/download/iy-go-to-char.el
(add-to-list 'load-path "~/.emacs.d/extra/iy-go-to-char.el")
(require 'iy-go-to-char)
(global-set-key (kbd "C-x j") 'iy-go-up-to-char)
(global-set-key (kbd "C-x J") 'iy-go-up-to-char-backward)

;; Repeat last insert command package
;; http://www.emacswiki.org/emacs/dot-mode.el
(require 'dot-mode)
(add-hook 'find-file-hooks 'dot-mode-on)



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

;; Add java-mode hook for groovy
(add-to-list 'auto-mode-alist '("\\.groovy$" . java-mode))

;; ----------End Built In----------