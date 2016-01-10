;; --------------------Packages--------------------

(load "~/.emacs.d/packages.el")

;; --------------------Packages--------------------



;; --------------------Minimal Startup--------------------

;; Prevent emacs startup buffer
(setq-default inhibit-splash-screen t)

;; Prevent the file menu from taking up space
(menu-bar-mode 0)

;; Remove copy/paste, etc. buttons from gui
(if window-system
    (tool-bar-mode -1))

;; Prevent *scratch* buffer explanation message
(setq initial-scratch-message nil)

;; Remove the scroll bar
(set-scroll-bar-mode nil)

;; Remove fringes (gutters that display line wrap arrow)
(fringe-mode 0)

;; Create and use a blank buffer called "empty"
(switch-to-buffer (get-buffer-create "empty"))

;; Set to Lisp mode, or whatever mode you want the default to be
(lisp-mode)

;; --------------------Minimal Startup--------------------



;; --------------------Misc--------------------

;; Switch yes/no prompts to y/n prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; Group sets of files together for IDE functionality or version control
(filesets-init)

;; --------------------Misc--------------------



;; --------------------Modes--------------------

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

;; --------------------Modes--------------------



;; --------------------Custom Functions--------------------

(defun insert-current-date (&optional omit-day-of-week-p)
	"Insert today's date. C-u to omit day of week."
	(interactive "P*")
    (insert (calendar-date-string (calendar-current-date) nil omit-day-of-week-p)))
(global-set-key "\C-x\M-d" `insert-current-date)

(defun insert-current-time ()
	"Insert the current time."
	(interactive)
  (insert (format-time-string "%-I:%M %p")))
(global-set-key "\C-x\M-t" `insert-current-time)


;; Origin: https://github.com/scottjad/dotfiles/blob/master/.emacs
(defun backwards-kill-line ()
  "Kill to beginning of line like C-u in the shell"
  (interactive)
  (kill-region (point) (progn (beginning-of-line) (point))))
(global-set-key (kbd "\C-c u") `backwards-kill-line)


;; Modified from Origin: https://github.com/scottjad/dotfiles/blob/master/.emacs
(defun next-list-boundaries ()
  "Cut (C-w) or copy (M-w) current (line/word/list/string/etc) if nothing is selected"
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


;; C-c e -> evaluate region or preceding s-expression and replace it with the result
;; Origin: http://emacsredux.com/blog/2013/06/21/eval-and-replace/
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(global-set-key (kbd "C-c e") 'eval-and-replace)


(defun mark-paragraph-without-preceding-newline (&optional arg allow-extend)
  "Put point at beginning of this paragraph, mark at end. The paragraph marked is the one that contains point or follows point.

With argument ARG, puts mark at end of a following paragraph, so that the number of paragraphs marked equals ARG.

If ARG is negative, point is put at end of this paragraph, mark is put at beginning of this or a previous paragraph.

Interactively, if this command is repeated or (in Transient Mark mode) if the mark is active, it marks the next ARG paragraphs after the ones already marked."
  (interactive "p\np")
  (unless arg (setq arg 1))
  (when (zerop arg)
    (error "Cannot mark zero paragraphs"))
  (cond ((and allow-extend
              (or (and (eq last-command this-command) (mark t))
                  (and transient-mark-mode mark-active)))
         (set-mark
          (save-excursion
            (goto-char (mark))
            (forward-paragraph arg)
            (point))))
        (t
         (forward-paragraph arg)
         (push-mark nil t t)
         (backward-paragraph arg)
         (if (/= (line-number-at-pos) 1)
             (next-line)))))

(global-set-key (kbd "M-h") 'mark-paragraph-without-preceding-newline)


;; Origin: http://www.emacswiki.org/emacs/IncrementNumber
(defun increment-number-decimal (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))

(defun decrement-number-decimal (&optional arg)
  (interactive "p*")
  (increment-number-decimal (if arg (- arg) -1)))

(global-set-key (kbd "M-+") 'increment-number-decimal)
(global-set-key (kbd "C-M-+") 'decrement-number-decimal)


;; Modified from Origin: http://www.emacswiki.org/emacs/MoveLine
(defun drag-line-up ()
  (interactive)
  (if (> (line-number-at-pos) 1)
      (progn
        (transpose-lines 1)
        (previous-line 2))
    (message "The line is at the top.")))

(defun drag-line-down ()
  (interactive)
  (if (< (line-number-at-pos) (count-lines (point-min) (point-max)))
      (progn
        (next-line 1)
        (transpose-lines 1)
        (previous-line 1))
    (message "The line is at the bottom.")))

(defun drag-line-up-or-down (&optional arg)
  (interactive "p")
  (if (plusp arg)
      (progn
        (do ((counter 0 (1+ counter)))
          ((>= counter arg))
        (progn
          (drag-line-down))))
    (do ((counter 0 (1+ counter)))
        ((>= counter (abs arg)))
      (drag-line-up))))

(global-set-key (kbd "M-<up>") 'drag-line-up)
(global-set-key (kbd "M-<down>") 'drag-line-down)
(global-set-key (kbd "C-x C-t") 'drag-line-up-or-down)


(defun message-buffer-file-name ()
  "Display the full file path of the current buffer"
  (interactive)
  (message (buffer-file-name)))

;; Remap `set-fill-column' to `message-buffer-file-name'
(global-set-key (kbd "C-x f") 'message-buffer-file-name)


(defun remove-all-whitespace-around-point ()
  (interactive)
  (cycle-spacing 0))

;; Remap `just-one-space' to leave no spaces at all instead
(global-set-key (kbd "M-SPC") 'remove-all-whitespace-around-point)


;; Rename file and buffer at the same time
(defun rename (new-name)
  "Renames both the current buffer and its file."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file." name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists." new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;; --------------------Custom Functions--------------------



;; --------------------Custom Key Bindings--------------------

;; M-s only has a few (fairly useless) search bindings - C-x C-s is one too many keystrokes
(global-set-key (kbd "M-s") 'save-buffer)

;; Remap `kill-sexp' to quit Emacs without saving
(global-set-key (kbd "C-M-k") 'save-buffers-kill-terminal)

;; Remap `down-list' to delete the following s-expression and store it
(global-set-key (kbd "C-M-d") 'kill-sexp)

;; Map CL Hyperspec symbol lookup
(global-set-key (kbd "C-c C-d @") 'common-lisp-hyperspec)

;; Remap `fill-paragraph' to `kill-this-buffer'
(global-set-key (kbd "M-q") 'kill-this-buffer)

;; Map `next-buffer'
(global-set-key (kbd "<C-M-tab>") 'next-buffer)

;; Map `previous-buffer'
(global-set-key (kbd "<C-M-S-iso-lefttab>") 'previous-buffer)

;; --------------------Custom Key Bindings--------------------



;; --------------------Safety Off--------------------

;; Some modes come disabled to prevent noobs from messing up - permanently enable them
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; --------------------Safety Off--------------------



;; --------------------Custom Set Variables--------------------

;; Change the default theme in gui mode only
(when (display-graphic-p)
  (custom-set-variables
   ; Change theme to manoj-dark
   '(ansi-color-faces-vector
     [default default default italic underline success warning error])
   '(ansi-color-names-vector
     ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
   '(custom-enabled-themes (quote (manoj-dark)))))

(custom-set-variables
;; These are added via M-x customize-browse
;; Settings should only appear once in an init file - be sure to avoid duplicates.

 ;; Prevent emacs from creating backups when files are saved - emacs default
 '(make-backup-files nil)

 ;; Prevent files from autosaving - this creates extraneous files/directories
 '(auto-save-default nil)

 ;; Two tabs per indent
 '(tab-width 2)
 '(standard-indent 2)

 ;; Wrap on words rather than adding a \ at the end of each line
 '(word-wrap t)

 ;; Keeps point at the same position when scrolling
 '(scroll-preserve-screen-position t)

 ;; Keep specified number of lines (scroll-margin) at top and bottom of buffer when scrolling or positioning
 ;'(scroll-margin 3)

 ;; Use point to push the page up or down one line at a time rather than jumping the window a certain number of lines whenever poin scrolls off page
 '(scroll-conservatively 101)

 ;; Modify default C-l behavior - top, middle, & bottom are available options
 '(recenter-positions '(top middle bottom))

 ;; I don't remember what this does...
 ;; Apparently other major modes require their own, such as c-tab-always-indent
 '(tab-always-indent (quote complete))

 ;; This is like in wordpad where each consecutive tab places the point, although it accomplishes this with the number of spaces/tabs rather than just one tab
 '(tab-stop-list (nil))

 ;; Do not add a new string to `kill-ring' if it duplicates the last one
 '(kill-do-not-save-duplicates t)

 ;; C-k kills the whole line AND the following newline
 '(kill-whole-line t)

 ;; If point was farther to the right on a previous line when moving up or down, move back to that number of characters, or as many as possible towards it
 '(track-eol t)

 ;; Do not add a newline to the end of every file
 '(require-final-newline nil)
 '(mode-require-final-newline nil)

 ;; When an input key is pressed while a selection exists, write over the selection as is done in most other editors
 '(delete-selection-mode t)

 ;; Recompile .el file when it is newer than its old compiled version
 '(load-prefer-newer t)

 ;; May be useful for keeping track of how much time is spent on a project - need to test this out
 ;'(setq org-clock-idle-time 10)

 ;; ls command flags for dired
 ;; A: display hidden
 ;; l: list style
 ;; h: use human readable file size
 ;; t: sort by modified
 '(dired-listing-switches "-Alht")

 ;; Allow permissions to be modified in dired
 '(wdired-allow-to-change-permission t)

 ;; Set version control systems should be used and in what order
 '(vc-handled-backends '(Git SVN Hg))

 ;; Display the version control commands emacs executes
 '(vc-command-messages t)

 ;; When typing a file path to open, use case insensitive completion
 '(read-file-name-completion-ignore-case t)

 ;; Cycle through completions with repeated tab presses
 '(completion-cycle-threshold t)

 ;; Set focus follows mouse for inner windows
 '(mouse-autoselect-window t)

 ;; Paste wherever the point is on middle click
 '(mouse-yank-at-point t)

 ;; Replace full window visible bell with a two line flash
 '(visible-bell t)
)

;; --------------------Custom Set Variables--------------------