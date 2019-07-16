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

;; Treemacs
(use-package treemacs
  :ensure t
  :defer t
  :config
  (setq treemacs-collapse-dirs                 (if (treemacs--find-python3) 3 0)
        treemacs-deferred-git-apply-delay      0.5
        treemacs-display-in-side-window        t
        treemacs-eldoc-display                 t
        treemacs-file-event-delay              5000
        treemacs-file-follow-delay             0.2
        treemacs-follow-after-init             t
        treemacs-git-command-pipe              ""
        treemacs-goto-tag-strategy             'refetch-index
        treemacs-indentation                   2
        treemacs-indentation-string            " "
        treemacs-is-never-other-window         t
        treemacs-max-git-entries               5000
        treemacs-missing-project-action        'ask
        treemacs-no-png-images                 nil
        treemacs-no-delete-other-windows       t
        treemacs-project-follow-cleanup        nil
        treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
        treemacs-recenter-distance             0.1
        treemacs-recenter-after-file-follow    nil
        treemacs-recenter-after-tag-follow     nil
        treemacs-recenter-after-project-jump   'always
        treemacs-recenter-after-project-expand 'on-distance
        treemacs-show-cursor                   nil
        treemacs-show-hidden-files             t
        treemacs-silent-filewatch              t
        treemacs-silent-refresh                t
        treemacs-sorting                       'alphabetic-desc
        treemacs-space-between-root-nodes      t
        treemacs-tag-follow-cleanup            t
        treemacs-tag-follow-delay              1.5
        treemacs-width                         35)

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null (treemacs--find-python3))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :ensure t
  :config
  (defun treemacs-show-current-file ()
    (interactive)
    (treemacs)
    (other-window -1))
  :hook (treemacs-mode . (lambda () (require 'treemacs-projectile))))

(message "Loaded init-projectile.el")
(provide 'init-projectile)
;;; init-projectile.el ends here
