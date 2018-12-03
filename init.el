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
;; Speed up startup
(defvar old--file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold 402653184      ; Increase memory threshold
      gc-cons-percentage 0.6           ; for garbage collection.
      file-name-handler-alist nil)      ; Unset file handlers

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

;; ;; JSX RELATED
;; (load "~/.emacs.d/jsx-stuff.el")


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
