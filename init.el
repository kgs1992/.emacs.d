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
(set-charset-priority 'unicode)
(setq locale-coding-system   'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system        'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; (setq debug-on-error t)

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
(defvar default-gc-cons-threshold 16777216) ; 16mb
(defvar old--file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold most-positive-fixnum      ; Increase memory threshold
      gc-cons-percentage 0.6                      ; for garbage collection.
      file-name-handler-alist nil)                ; Unset file handlers
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold default-gc-cons-threshold
                  gc-cons-percentage 0.1
                  file-name-handler-alist old--file-name-handler-alist)))

;; Speedup minibuffer & desktop
(defun defer-garbage-collection-h ()
  "Raise `gc-cons-threshold` while the minibuffer is active."
  (setq gc-cons-threshold most-positive-fixnum))

(defun restore-garbage-collection-h ()
  "Restore `gc-cons-threshold` after exiting the minibuffer."
  ;; Defer it so that commands launched immediately after will enjoy the
  ;; benefits.
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold default-gc-cons-threshold))))

(add-hook 'emacs-startup-hook
          (lambda ()
            (add-hook 'minibuffer-setup-hook #'defer-garbage-collection-h)
            (add-hook 'minibuffer-exit-hook #'restore-garbage-collection-h)))

;; Set custom file
(setq custom-file "~/.emacs.d/init-custom.el")

;; Suppress redefinition warnings
(setq ad-redefinition-action 'accept)

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
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))


;;;; Configure useful emacs extensions

(load "~/.emacs.d/init-ivy.el")
(load "~/.emacs.d/init-projectile.el")
(load "~/.emacs.d/init-grep.el")
(load "~/.emacs.d/init-tramp.el")
(load "~/.emacs.d/init-term.el")
(load "~/.emacs.d/init-magit.el")
(load "~/.emacs.d/init-editing.el")
(load "~/.emacs.d/init-polymode.el")
;; (load "~/.emacs.d/init-tabbar.el")
(load "~/.emacs.d/init-modeline.el")

;; ORG RELATED
(load "~/.emacs.d/init-org.el")

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

(message "Loaded init.el")
(provide 'init)
;;; init.el ends here
