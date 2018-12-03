;;; theme-stuff.el --- Theming related config -*- lexical-binding: t; -*-
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
;; Apply theme and custom faces
(defun apply-my-theme()
  "Apply my Emacs theme config."

  ;; DisableMenubars and Toolbars
  (menu-bar-mode -1)
  (tool-bar-mode -1)

  ;; Highlisht current line
  (global-hl-line-mode t)

  ;; Bar cursor
  (setq-default cursor-type '(bar . 2))

  ;; Syntax highlighting
  (global-font-lock-mode 1)
  (setq font-lock-maximum-decoration t)

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
   :height 0.1)

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
  (set-face-background 'hl-line "grey10"))

;; Apply theme config with frame context
(defun apply-my-theme-to-frame (frame)
  (with-selected-frame frame
    (apply-my-theme)))

;; Set the theme
(use-package hl-line
  :demand t)

(use-package frame
  :demand t)

(use-package doom-themes
  :if (daemonp)
  :demand t
  :ensure t
  :after (tabbar helm frame hl-line)
  :config
  (add-hook 'after-make-frame-functions 'apply-my-theme-to-frame))

(use-package doom-themes
  :unless (daemonp)
  :demand t
  :ensure t
  :after (tabbar helm frame hl-line)
  :config
  (apply-my-theme))

(provide 'theme-stuff)
;;; theme-stuff.el ends here
