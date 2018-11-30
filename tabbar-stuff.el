;;; tabbar-stuff.el --- Tabbar related config -*- lexical-binding: t; -*-
;;
;; Author:  Kiran Shenoy
;; URL:     https://github.com/kgs1992/.emacs.d/
;;
;;; Commentary:
;;
;; This package contains all of the tabbar related config.
;;
;;;

;;; Code:
;; Tabbar
(use-package tabbar
  :ensure t
  :demand t
  :config
  (tabbar-mode t)

  (defun tabbar-buffer-groups ()
    "Return the list of group names the current buffer belongs to.
     This function is a custom function for tabbar-mode's tabbar-buffer-groups.
     This function groups all buffers into 3 groups:
      - Dired buffers
      - User buffers
      - Emacs buffers"
    (list
     (cond
      ((string-equal "*" (substring (buffer-name) 0 1))
       "Emacs Buffer"
       )
      ((eq major-mode 'dired-mode)
       "Dired"
       )
      (t
       "User Buffer"
       )
      )))
  (setq tabbar-separator (quote (" | "))
        tabbar-buffer-groups-function 'tabbar-buffer-groups)
  :bind (("M-," . 'tabbar-backward-tab)
         ("M-." . 'tabbar-forward-tab)))
(provide 'tabbar-stuff)
;;; tabbar-stuff.el ends here
