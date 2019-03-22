;;; init-theme.el --- Theming related config -*- lexical-binding: t; -*-
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
;; Doom themes
(use-package doom-themes
  :ensure t
  :commands (apply-my-theme-to-frame)
  :defer t
  :config
  ;; Apply theme and custom faces
  (defun apply-my-theme()
    "Apply my Emacs theme config."

    ;; Disable Menubars and Toolbars
    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (toggle-scroll-bar -1)

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

    ;; Misc colours
    (set-face-attribute 'region nil
                        :background "#21242b"
                        :foreground "black")
    (set-face-background 'hl-line "#21242b"))

  (defun apply-my-theme-to-frame (frame)
    (with-selected-frame frame
      (apply-my-theme)))
  :hook (after-init . apply-my-theme))

(use-package fira-code-mode
  :load-path "local-packages"
  :commands (fira-code-mode)
  :config
  (setq fira-code-mode--only-ligatures t)
  :hook (prog-mode . fira-code-mode))

(if (daemonp)
	(add-hook 'after-make-frame-functions #'apply-my-theme-to-frame))

(message "Loaded init-theme.el")
(provide 'init-theme)
;;; init-theme.el ends here
