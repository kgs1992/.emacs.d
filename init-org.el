;;; init-org.el --- Org related config -*- lexical-binding: t; -*-
;;
;; Author:  Kiran Shenoy
;; URL:     https://github.com/kgs1992/.emacs.d/
;;
;;; Commentary:
;;
;; This package contains all of the org related config.
;;
;;;

;;; Code:
(use-package org
  :ensure nil
  :defer t
  :config
  (setq org-startup-indented 'f)
  (setq org-directory "~/orgfiles")
  (setq org-agenda-files
      (append
       (file-expand-wildcards "~/orgfiles/*.org")))
  (setq org-special-ctrl-a/e 't)
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-src-fontify-natively 't)
  (setq org-src-tab-acts-natively t)
  (setq org-src-window-setup 'current-window)
  :bind
  ("C-c a" . org-agenda))

(use-package org-bullets
  :ensure t
  :config
  (setq org-bullets-bullet-list '("∙"))
  :hook
  (org-mode . org-bullets-mode))

(message "Loaded init-org.el")
(provide 'init-org)
;;; init-org.el ends here
