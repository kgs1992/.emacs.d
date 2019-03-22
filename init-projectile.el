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
  :after (helm helm-for-files projectile doom-themes)
  :config
  (helm-projectile-on)
  (setq projectile-switch-project-action 'helm-projectile)
  (add-to-list 'helm-for-files-preferred-list helm-source-projectile-projects)
  (add-to-list 'helm-for-files-preferred-list helm-source-projectile-files-list)
  (add-to-list 'helm-for-files-preferred-list helm-source-projectile-directories-list)
  (setq helm-find-file-ignore-thing-at-point t))

;; Neotree
(use-package neotree
  :ensure t
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
