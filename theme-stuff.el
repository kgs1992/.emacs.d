;;; theme-stuff.el --- Theming related config
;;
;; Author:  Kiran Shenoy
;; URL:     https://github.com/kgs1992/.emacs.d/
;;
;;; Commentary:
;;
;; This package contains all of the theme related config.
;;
;;;

;;; Code:
;; Bar cursor
(setq-default cursor-type '(bar . 2))

;; Set the theme
;; (el-get-bundle 'monokai-theme)
;; (require 'monokai-theme)
;; (load-theme 'monokai t)
(el-get-bundle 'hlissner/emacs-doom-themes)
(require 'doom-themes)

;; Global settings (defaults)
(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)

;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;; may have their own settings.
(load-theme 'doom-one t)

;; Set tabbar style
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
 :background "gray75"
 :foreground "#A41F99"
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
 :height 0.1)

;; Adding spaces
(defun tabbar-buffer-tab-label (tab)
  "Return a label for TAB.
That is, a string used to represent it on the tab bar."
  (let ((label  (if tabbar--buffer-show-groups
                    (format " [%s] " (tabbar-tab-tabset tab))
                  (format " %s " (tabbar-tab-value tab)))))
    ;; Unless the tab bar auto scrolls to keep the selected tab
    ;; visible, shorten the tab label to keep as many tabs as possible
    ;; in the visible area of the tab bar.
    (if tabbar-auto-scroll-flag
        label
      (tabbar-shorten
       label (max 1 (/ (window-width)
                       (length (tabbar-view
                                (tabbar-current-tabset)))))))))

;; Syntax highlighting
(global-font-lock-mode 1)
(setq font-lock-maximum-decoration t)

;; Helm colours
(set-face-attribute 'helm-selection nil
                    :background "grey10"
                    :foreground "blue")
(set-face-attribute 'helm-source-header nil
                    :background "gray30"
                    :foreground "green")

;; Misc colours
(set-face-attribute 'region nil
                    :background "blue"
                    :foreground "black")
(set-face-background 'hl-line "grey10")
(set-face-attribute 'magit-diff-hunk-heading nil
                    :foreground "white")
(set-face-attribute 'magit-diff-hunk-heading-highlight nil
                    :foreground "white"
                    :background "grey10")
(set-face-attribute 'magit-diff-removed nil
                    :background "black")
(set-face-attribute 'magit-diff-removed-highlight nil
                    :background "black")

(provide 'theme-stuff)
;;; theme-stuff.el ends here
