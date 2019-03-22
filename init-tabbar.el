;;; init-tabbar.el --- Tabbar related config -*- lexical-binding: t; -*-
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
  :commands (fix-tabbar-colors-for-frame)
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
  (setq tabbar-use-images nil)
  (setq tabbar-separator (quote (" | "))
        tabbar-buffer-groups-function 'tabbar-buffer-groups)

  ;; Add a buffer modification state indicator in the tab label, and place a
  ;; space around the label to make it looks less crowd.
  (defadvice tabbar-buffer-tab-label (after fixup_tab_label_space_and_flag activate)
    (setq ad-return-value
          (if (and (buffer-modified-p (tabbar-tab-value tab))
                   (buffer-file-name (tabbar-tab-value tab)))
              (concat " + " (concat ad-return-value " "))
            (concat " " (concat ad-return-value " ")))))

  (defun fix-tabbar-colors()
    "Fix the colors for tabbar."
    (set-face-attribute
     'tabbar-default nil
     :background "#1B2229"
     :foreground "#1B2229"
     :box '(:line-width 1 :color "#1B2229" :style nil))
    (set-face-attribute
     'tabbar-unselected nil
     :background "#3f444a"
     :foreground "#bbc2cf"
     :box '(:line-width 1 :color "#3f444a" :style nil))
    (set-face-attribute
     'tabbar-selected nil
     :background "#73797e"
     :foreground "#dd8844"
     :box '(:line-width 1 :color "#3f444a" :style nil))
    (set-face-attribute
     'tabbar-highlight nil
     :background "white"
     :foreground "black"
     :underline nil
     :box '(:line-width 1 :color "#73797e" :style nil))
    (set-face-attribute
     'tabbar-button nil
     :background "#1B2229"
     :foreground "white"
     :box '(:line-width 1 :color "#1B2229" :style nil))
    (set-face-attribute
     'tabbar-separator nil
     :background "#1B2229"
     :foreground "#1B2229"
     :height 0.1))

  (defun fix-tabbar-colors-for-frame(frame)
    "Fix the colors for tabbar for selected frame.
    FRAME: Frame to change attributes of"
    (with-selected-frame frame
	  (fix-tabbar-colors)))

  (unless (daemonp)
    (fix-tabbar-colors))

  :bind (("M-," . 'tabbar-backward-tab)
         ("M-." . 'tabbar-forward-tab))
  :hook (after-init . tabbar-mode))

(if (daemonp)
	(add-hook 'after-make-frame-functions #'fix-tabbar-colors-for-frame))

(message "Loaded init-tabbar.el")
(provide 'init-tabbar)
;;; init-tabbar.el ends here
