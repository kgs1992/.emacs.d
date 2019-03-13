;;; tramp-stuff.el --- Tramp related config -*- lexical-binding: t; -*-
;;
;; Author:  Kiran Shenoy
;; URL:     https://github.com/kgs1992/.emacs.d/
;;
;;; Commentary:
;;
;; This package contains all of the tramp related config.
;;
;;;

;;; Code:
;; Tramp for remote editing
(use-package tramp
  :config
  (setq tramp-default-method "sshx")
  (setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t)))
  (setq tramp-backup-directory-alist backup-directory-alist))

(message "Loaded tramp-stuff.el")
(provide 'tramp-stuff)
;;; tramp-stuff.el ends here
