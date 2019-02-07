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
(setq custom-file "~/.emacs.d/custom-stuff.el")

;; Package and compile related
(setq load-prefer-newer t)
;; Init package
(load "~/.emacs.d/package-stuff.el")

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


(load "~/.emacs.d/helm-stuff.el")
(load "~/.emacs.d/projectile-stuff.el")
(load "~/.emacs.d/grep-stuff.el")
(load "~/.emacs.d/tramp-stuff.el")
(load "~/.emacs.d/magit-stuff.el")
(load "~/.emacs.d/editing-stuff.el")
(load "~/.emacs.d/tabbar-stuff.el")
(load "~/.emacs.d/modeline-stuff.el")

;; PYTHON RELATED
(load "~/.emacs.d/python-stuff.el")

;; GO RELATED
(load "~/.emacs.d/go-stuff.el")

;; JSX RELATED
(load "~/.emacs.d/jsx-stuff.el")


;; Theme related
(load "~/.emacs.d/theme-stuff.el")

;; Unbind C-z
(if (daemonp)
    (global-unset-key (kbd "C-z")))

(load custom-file :noerror)

;; Reset startup optimizations
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold 16777216
                           gc-cons-percentage 0.1
                           file-name-handler-alist old--file-name-handler-alist)))

(provide 'init)
;;; init.el ends here
