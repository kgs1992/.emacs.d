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
  (setq projectile-mode-line '(:eval (format " [%s]" (projectile-project-name))))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :hook (after-init . projectile-mode))

;; Counsel projectile
(use-package counsel-projectile
  :ensure t
  :defer t
  :after (counsel projectile)
  :hook (after-init . counsel-projectile-mode)
  :bind ("C-x f" . counsel-projectile-find-file-dwim))

;; Neotree
(use-package neotree
  :ensure t
  :defer t
  :after (projectile doom-themes)
  :config
  (setq neo-theme 'icons)
  ;; Neotree theme
  (doom-themes-neotree-config)

  ;; NeoTree can be opened (toggled) at projectile project root
  (defun neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root."))))
  :bind   (("C-c C-p" . neotree-project-dir)))

(message "Loaded init-projectile.el")
(provide 'init-projectile)
;;; init-projectile.el ends here
