;; --------------------Minimal Startup--------------------

;; Stop annoying emacs startup message from being displayed
(setq-default inhibit-splash-screen t)
;; Remove split screen created by the splash screen
(delete-other-windows)
;; Start up a blank buffer called "empty"
(switch-to-buffer (get-buffer-create "emtpy"))
;; Prevent the file menu from taking up an extra line
(menu-bar-mode 0)
;; Prevent *scratch* buffer explanation message from being displayed
(setq initial-scratch-message nil)

;; --------------------End Minimal Startup--------------------


;; Prevent emacs from creating backups when files are saved - emacs default
(setq make-backup-files nil)
;; Prevent files from autosaving - this creates extraneous files/directories
(setq auto-save-default nil)

;; Wrap on words rather than adding a \ at the end of each line
(setq-default word-wrap t)
;; Keeps point at the same position when scrolling
(setq-default scroll-preserve-screen-position t)
;; Modify default C-l behavior - top and middle only
(setq-default recenter-positions '(top middle bottom))
;; Keep specified number of lines at top and bottom of buffer when scrolling or positioning
(setq-default scroll-margin 3)
;; Highlight trailing whitespace
;(setq-default show-trailing-whitespace t)
;; ls command flags for dired - use human readable file size, sort by modified
(setq-default dired-listing-switches "-Alht")
;; Cycle through completions with repeated tab presses
(setq-default completion-cycle-threshold t)
;; Allow permissions to be modified in dired
(setq-default wdired-allow-to-change-permission t)
;; Switch yes/no prompts to y/n prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; Highlight matching delimiters
(show-paren-mode 1)
;; Automatically add closing delimiter
(electric-pair-mode 1)
;; Display strings like lambda as the actual character
(global-prettify-symbols-mode 1)
;; Expand prespecified abbrevs in all modes
(abbrev-mode 1)
;; Save the open buffers, windows, and modes to be opened again upon starting emacs next time
;(desktop-save-mode 1)
;; Remember minibuffer history between sessions
(savehist-mode 1)
;; Set xterm gui mouse emulation - allows split window scrolling in the terminal (wow!)
(xterm-mouse-mode 1)
;; Display current time - mostly running emacs in fullscreen, so it's nice to have a clock
(display-time-mode 1)

;; Set focus follows mouse for inner windows
(setq-default mouse-autoselect-window t)
;; Paste wherever the point is on middle click
(setq-default mouse-yank-at-point t)

;; Remove copy/paste, etc. buttons from gui
(if window-system
    (tool-bar-mode -1))

;; Filesets are used for commonly opened groups of files
;; Does this actually do anything?
(filesets-init)

;; Set version control systems should be used and in what order
(setq-default vc-handled-backends '(Git SVN Hg))
;; Display the version control commands emacs executes
(setq-default vc-command-messages t)

;; Recompile .el file when it is newer than its old compiled version
(setq-default load-prefer-newer t)

;; --------------------Packages--------------------

;; Add third party packages to the load-path
(add-to-list 'load-path "~/.emacs.d/extra/")
;; Highlight nested delimiters rainbow colors
;(require 'rainbow-delimiters)
;(global-rainbow-delimiters-mode)

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

;; Go mode
;; https://github.com/dominikh/go-mode.el
(add-to-list 'load-path "~/.emacs.d/extra/go-mode.el")
(require 'go-mode-autoloads)

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


;; ----------Built In----------

;; For note taking/list building, such as to-do lists
(require 'org)

;; Save Place remembers where point was when the file was last closed
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/save-place-log")

;; ----------End Built In----------

;; --------------------End Packages--------------------


;; --------------------Custom Functions--------------------

(defun insert-current-date (&optional omit-day-of-week-p)
	"Insert today's date. C-u to omit day of week."
	(interactive "P*")
    (insert (calendar-date-string (calendar-current-date) nil omit-day-of-week-p)))
(global-set-key "\C-x\M-d" `insert-current-date)

;; Like C-u in the shell
;; Origin: https://github.com/scottjad/dotfiles/blob/master/.emacs
(defun backwards-kill-line ()
  (interactive)
  (kill-region (point) (progn (beginning-of-line) (point))))
(global-set-key (kbd "\C-c u") `backwards-kill-line)

;; Cut (C-w) or copy (M-w) current (line/word/list/string/etc) if nothing is selected
;; Modified from Origin: https://github.com/scottjad/dotfiles/blob/master/.emacs
(defun next-list-boundaries ()
  (list (progn (next-line) (point))
        (progn (beginning-of-line) (point))))
(defun no-region-default-behavior ()
  (cond (mark-active
         (list (region-beginning) (region-end)))
        (t
         (progn (message "Copy/cut line")
                (list (line-beginning-position) (line-beginning-position 2))))))
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive (no-region-default-behavior)))
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive (no-region-default-behavior)))

;; --------------------Custom Functions--------------------


;; --------------------Custom Key Bindings--------------------

;; Rather than zapping through character, zap up to character
(autoload 'zap-up-to-char "misc"
	"Kill up to, but not including ARGth occurrence of CHAR." t)
(global-set-key (kbd "M-z") 'zap-up-to-char)

;; M-s only has a few (fairly useless) search bindings - C-x C-s is one too many keystrokes
(global-set-key (kbd "M-s") 'save-buffer)

;; --------------------Custom Key Bindings--------------------


;; Some modes come disabled to prevent noobs from messing up - permanently enable them
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Prevent line truncation in org mode
(add-hook 'org-mode-hook (lambda () (toggle-truncate-lines -1)))

;; Default theme in gui mode
(when (display-graphic-p)
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   ; Changed theme to manoj-dark
   '(ansi-color-faces-vector
     [default default default italic underline success warning error])
   '(ansi-color-names-vector
     ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
   '(custom-enabled-themes (quote (manoj-dark)))))

(custom-set-variables
 ;; Added via M-x customize-browse
 ;; Settings should only appear once in an init file - be sure to avoid duplicates.
 '(standard-indent 2)
 '(tab-always-indent (quote complete)) ;apparently other major modes require their own, such as c-tab-always-indent
 '(tab-stop-list (nil))
 '(tab-width 2)
 '(kill-do-not-save-duplicates t)
 '(kill-whole-line t)
 '(track-eol t)
 '(require-final-newline nil)
 '(mode-require-final-newline nil)
 '(scroll-bar-mode nil)
 '(delete-selection-mode t)
; '(setq org-clock-idle-time 10) ;Need to test this out
 '(read-file-name-completion-ignore-case t)
)
