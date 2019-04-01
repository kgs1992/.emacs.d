;;; init-grep.el --- Searching related config -*- lexical-binding: t; -*-
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
;; ripgrep - Super fast grep
(use-package rg
  :ensure t
  :defer t
  :after (projectile)
  :config
  (defun ripgrep ()
    "Use rg-project if within a project context."
    (interactive
     (if (projectile-project-p)
         (call-interactively 'rg-project)
       (call-interactively 'rg))))
  :bind (("M-s" . ripgrep)))

;; wgrep - Edit and save grep buffers
(use-package wgrep
  :ensure t
  :defer t
  :after (rg))

(message "Loaded init-grep.el")
(provide 'init-grep)
;;; init-grep.el ends here
