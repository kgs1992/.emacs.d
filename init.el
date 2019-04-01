;;; init.el --- user init file -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-
;;
;; Author:  Kiran Shenoy
;; URL:     https://github.com/kgs1992/.emacs.d/
;;
;;; Commentary:
;;
;; Most of this is borrowed and is very messy.
;;
;;;

;;; Code:
;; Use UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Frame customization
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(defun set-mac-defaults ()
  "Set defaults for macOS."
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(font . "Fira Code Retina"))
  (setq mac-option-modifier 'meta)
  )

(if (eq system-type 'darwin)
    (set-mac-defaults))

;; Speed up startup
(defvar old--file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold 402653184      ; Increase memory threshold
      gc-cons-percentage 0.6           ; for garbage collection.
      file-name-handler-alist nil)     ; Unset file handlers

;; Set custom file
(setq custom-file "~/.emacs.d/init-custom.el")

;; Package and compile related
(setq load-prefer-newer t)
;; Init package
(load "~/.emacs.d/init-package.el")

;; Auto compile all
(use-package auto-compile
  :ensure t
  :demand t
  :config
  (setq auto-compile-display-buffer nil
        auto-compile-mode-line-counter t)
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;; Set exec-path from $PATH
(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config
  (exec-path-from-shell-initialize))


;;;; Configure useful emacs extensions


(load "~/.emacs.d/init-helm.el")
(load "~/.emacs.d/init-projectile.el")
(load "~/.emacs.d/init-grep.el")
(load "~/.emacs.d/init-tramp.el")
(load "~/.emacs.d/init-magit.el")
(load "~/.emacs.d/init-editing.el")
;; (load "~/.emacs.d/init-tabbar.el")
(load "~/.emacs.d/init-modeline.el")

;; PYTHON RELATED
(load "~/.emacs.d/init-python.el")

;; GO RELATED
(load "~/.emacs.d/init-go.el")

;; JSX RELATED
(load "~/.emacs.d/init-jsx.el")


;; Theme related
(load "~/.emacs.d/init-theme.el")

;; Unbind C-z
(if (daemonp)
    (global-unset-key (kbd "C-z")))

(load custom-file :noerror)

;; Reset startup optimizations
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold 20000000
                           gc-cons-percentage 0.1
                           file-name-handler-alist old--file-name-handler-alist)))

(message "Loaded init.el")
(provide 'init)
;;; init.el ends here
