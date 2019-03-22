;;; init-go.el --- Golang related config  -*- lexical-binding: t; -*-
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
  (defun my-go-mode-hook ()
    ;; Calling go-fmt before save is done by format-all
    ;; (add-hook 'before-save-hook 'gofmt-before-save)
    (local-set-key (kbd "C-c .") 'godef-jump)
    (local-set-key (kbd "C-c ,") 'pop-global-mark))
  :hook (go-mode . my-go-mode-hook))

(use-package company-go
  :ensure t
  :defer t
  :after (company)
  :hook (go-mode . (lambda ()
                     (set (make-local-variable 'company-backends) '(company-go))
                     (company-mode))))

(message "Loaded init-go.el")
(provide 'init-go)
;;; init-go.el ends here
