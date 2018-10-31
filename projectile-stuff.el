;; Projectile
(el-get-bundle projectile)
(require 'projectile)
(projectile-mode 1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-enable-caching t)
(require 'helm)
(setq projectile-completion-system 'helm)

(setq projectile-mode-line '(:eval (format " [%s]" (projectile-project-name))))

;; Recent files
(recentf-mode 1)
(setq recentf-max-saved-items 500)
(setq recentf-max-menu-items 60)

;; Helm Projectile
(el-get-bundle helm-projectile)
(require 'helm-projectile)
(helm-projectile-on)
(setq projectile-switch-project-action 'helm-projectile)
(require 'helm-for-files)
(add-to-list 'helm-for-files-preferred-list helm-source-projectile-projects)
(add-to-list 'helm-for-files-preferred-list helm-source-projectile-files-list)
(add-to-list 'helm-for-files-preferred-list helm-source-projectile-directories-list)
(global-set-key (kbd "C-x f") #'helm-for-files)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(setq helm-find-file-ignore-thing-at-point t)
(define-key helm-find-files-map (kbd "M-DEL") 'helm-find-files-up-one-level)
