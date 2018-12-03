;;; go-stuff.el --- Golang related config  -*- lexical-binding: t; -*-
;;
;; Author:  Kiran Shenoy
;; URL:     https://github.com/kgs1992/.emacs.d/
;;
;;; Commentary:
;;
;; This package contains all of the golang related config.
;;
;;;

;;; Code:
;; Flycheck (lint)
(use-package go-mode
  :ensure t
  :defer t
  :config
  (add-to-list 'load-path (getenv "GOPATH"))
  (defun flycheck-go-setup ()
    (flycheck-mode))
  (defun my-go-mode-hook ()
    ;; Calling go-fmt before save is done by format-all
    ;; (add-hook 'before-save-hook 'gofmt-before-save)
    (add-hook 'go-mode-hook #'flycheck-go-setup)
    (local-set-key (kbd "C-z") 'godef-jump))
  :hook (go-mode . my-go-mode-hook))

(provide 'go-stuff)
;;; go-stuff.el ends here
