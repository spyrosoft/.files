;; https://github.com/magnars/dash.el.git
;(add-to-list 'load-path "~/.emacs.d/packages/dash.el")
;; https://github.com/magit/magit/releases
(add-to-list 'load-path "~/.emacs.d/packages/magit")
(eval-after-load 'info
  '(progn (info-initialize)
          (add-to-list 'Info-directory-list "~/.emacs.d/packages/magit/")))
(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")