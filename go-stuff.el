;;; go-stuff.el --- Golang related config
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
(el-get-bundle flycheck)
(require 'flycheck)
(add-to-list 'load-path (getenv "GOPATH"))
(defun flycheck-go-setup ()
  (flycheck-mode))
(defun my-go-mode-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook #'flycheck-go-setup)
  (local-set-key (kbd "C-z") 'godef-jump)
  )
;; (add-hook 'go-mode-hook 'my-go-mode-hook)

(provide 'go-stuff)
;;; go-stuff.el ends here
