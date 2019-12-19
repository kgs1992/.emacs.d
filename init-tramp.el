;;; init-tramp.el --- Tramp related config -*- lexical-binding: t; -*-
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
  :demand t
  :config
  (setq tramp-default-method "sshx")
  (setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t)))
  (setq tramp-backup-directory-alist backup-directory-alist)
  (setq tramp-save-ad-hoc-proxies t)
  (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
  ;; (setq tramp-chunksize 500)
  )



(use-package counsel-tramp
  :ensure t
  :demand t
  :config
  (add-hook 'counsel-tramp-pre-command-hook '(lambda ()
                                               (projectile-mode 0)))
  (add-hook 'counsel-tramp-quit-hook '(lambda ()
                                        (projectile-mode 1) )))


(message "Loaded init-tramp.el")
(provide 'init-tramp)
;;; init-tramp.el ends here
