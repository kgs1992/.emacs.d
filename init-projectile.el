;;; init-projectile.el --- Projectile related config -*- lexical-binding: t; -*-
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
  :defer t
  :config
  (setq recentf-max-saved-items 500)
  (setq recentf-max-menu-items 60)
  :hook (after-init . recentf-mode))

;; Projectile
(use-package projectile
  :ensure t
  :defer t
  :config
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t)
  (setq projectile-mode-line-function (lambda () (format " [%s]" (projectile-project-name))))
  (setq projectile-globally-ignored-modes (append '("vterm-mode")
                                                  projectile-globally-ignored-modes))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :bind (("s-<right>" . projectile-next-project-buffer)
         ("s-<left>" . projectile-previous-project-buffer))
  :hook (after-init . projectile-mode))

;; Counsel projectile
(use-package counsel-projectile
  :ensure t
  :defer t
  :hook (after-init . counsel-projectile-mode)
  :bind ("C-x f" . counsel-projectile-find-file))

(message "Loaded init-projectile.el")
(provide 'init-projectile)
;;; init-projectile.el ends here
