;;; projectile-stuff.el --- Projectile related config -*- lexical-binding: t; -*-
;;
;; Author:  Kiran Shenoy
;; URL:     https://github.com/kgs1992/.emacs.d/
;;
;;; Commentary:
;;
;; This package contains all of the projectile related config.
;;
;;;

;;; Code:
;; Recent files
(use-package recentf
  :config
  (recentf-mode 1)
  (setq recentf-max-saved-items 500)
  (setq recentf-max-menu-items 60))

;; Projectile
(use-package projectile
  :ensure t
  :demand t
  :config
  (projectile-mode 1)
  (setq projectile-completion-system 'helm)
  (setq projectile-enable-caching t)
  (setq projectile-mode-line '(:eval (format " [%s]" (projectile-project-name))))
  :bind-keymap
  ("C-c p" . projectile-command-map))

;; Helm Projectile
(use-package helm-projectile
  :ensure t
  :demand t
  :after (helm helm-for-files projectile)
  :config
  (helm-projectile-on)
  (setq projectile-switch-project-action 'helm-projectile)
  (add-to-list 'helm-for-files-preferred-list helm-source-projectile-projects)
  (add-to-list 'helm-for-files-preferred-list helm-source-projectile-files-list)
  (add-to-list 'helm-for-files-preferred-list helm-source-projectile-directories-list)
  (setq helm-find-file-ignore-thing-at-point t))

(provide 'projectile-stuff)
;;; projectile-stuff.el ends here
