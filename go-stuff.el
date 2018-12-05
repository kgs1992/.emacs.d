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
(use-package go-mode
  :ensure t
  :defer t
  :config
  (defun flycheck-go-setup ()
    (flycheck-mode))
  (defun my-go-mode-hook ()
    ;; Calling go-fmt before save is done by format-all
    ;; (add-hook 'before-save-hook 'gofmt-before-save)
    (add-hook 'go-mode-hook #'flycheck-go-setup)
    (local-set-key (kbd "C-z") 'godef-jump))
  :hook (go-mode . my-go-mode-hook))

(use-package company-go
  :ensure t
  :defer t
  :after (company)
  :hook (go-mode . (lambda ()
                     (set (make-local-variable 'company-backends) '(company-go))
                     (company-mode))))

(provide 'go-stuff)
;;; go-stuff.el ends here
