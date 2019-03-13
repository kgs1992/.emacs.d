;;; grep-stuff.el --- Searching related config -*- lexical-binding: t; -*-
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
  :after (rg))

(message "Loaded grep-stuff.el")
(provide 'grep-stuff)
;;; grep-stuff.el ends here
