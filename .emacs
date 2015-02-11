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
;; Keep 4 lines at top and bottom of buffer when scrolling or positioning
(setq-default scroll-margin 3)
;; Highlight trailing whitespace
;(setq-default show-trailing-whitespace t)
;; Use human readable file size in dired mode
(setq-default dired-listing-switches "-Alh")
;; Cycle through completions with repeated tab presses
(setq-default completion-cycle-threshold t)
;; Allow permissions to be modified in dired
(setq-default wdired-allow-to-change-permission t)

;; Highlight matching delimiters
(show-paren-mode)
;; Automatically add closing delimiter
(electric-pair-mode)
;; Display strings like lambda as the actual character
(global-prettify-symbols-mode)
;; Expand prespecified abbrevs in all modes
(abbrev-mode)

;; Set xterm gui mouse emulation
(xterm-mouse-mode)
;; Set focus follows mouse for inner windows
(setq-default mouse-autoselect-window t)

;; At some point I may want to add libraries, this is the syntax.
;; See section 27.8
;(add-to-list 'load-path "/path/to/my/lisp/library")

;; Set gui to darkgrey background - not console
;(when (display-graphic-p)
;  (set-background-color "darkgrey"))
;; Remove copy/paste, etc. buttons from gui
(if window-system
    (tool-bar-mode -1))

;; Filesets are used for commonly opened groups of files
(filesets-init)

;; Set version control systems should be used and in what order
(setq-default vc-handled-backends '(Git SVN Hg))
;; Display the version control commands emacs executes
(setq-default vc-command-messages t)

;; Add third party packages to the load-path
(add-to-list 'load-path "~/.emacs.d/extra/")
;; Highlight nested delimiters rainbow colors
;(require 'rainbow-delimiters)
;(global-rainbow-delimiters-mode)

;; Slime lisp package
(add-to-list 'load-path "~/.emacs.d/extra/slime")
(require 'slime-autoloads)
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy))

;; Initialize built in org package
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; Some modes come disabled to prevent noobs from messing up - these eliminate the warnings
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

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
   '(custom-enabled-themes (quote (manoj-dark))))
	)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tab-width 2))
