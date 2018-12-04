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
  :commands (fix-tabbar-colors-for-frame tabbar-mode)
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

  (defun fix-tabbar-colors()
    "Fix the colors for tabbar."
    (set-face-attribute
     'tabbar-default nil
     :background "black"
     :foreground "black"
     :box '(:line-width 1 :color "gray20" :style nil))
    (set-face-attribute
     'tabbar-unselected nil
     :background "gray30"
     :foreground "white"
     :box '(:line-width 1 :color "gray30" :style nil))
    (set-face-attribute
     'tabbar-selected nil
     :background "bright black"
     :foreground "orange" ;;"#A41F99"
     :box '(:line-width 1 :color "gray75" :style nil))
    (set-face-attribute
     'tabbar-highlight nil
     :background "white"
     :foreground "black"
     :underline nil
     :box '(:line-width 1 :color "white" :style nil))
    (set-face-attribute
     'tabbar-button nil
     :box '(:line-width 1 :color "gray20" :style nil))
    (set-face-attribute
     'tabbar-separator nil
     :background "black"
     :height 0.1))

  (defun fix-tabbar-colors-for-frame(frame)
    "Fix the colors for tabbar for selected frame.
    FRAME: Frame to change attributes of"
    (with-selected-frame frame
	  (fix-tabbar-colors)))

  (unless (daemonp)
    (fix-tabbar-colors))

  :bind (("M-," . 'tabbar-backward-tab)
         ("M-." . 'tabbar-forward-tab)))

(if (daemonp)
	(add-hook 'after-make-frame-functions #'fix-tabbar-colors-for-frame))

(add-hook 'after-init-hook #'tabbar-mode)

(provide 'tabbar-stuff)
;;; tabbar-stuff.el ends here
