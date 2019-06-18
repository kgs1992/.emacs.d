;;; init-ivy.el --- Ivy related config -*- lexical-binding: t; -*-
;;
;; Author:  Kiran Shenoy
;; URL:     https://github.com/kgs1992/.emacs.d/
;;
;;; Commentary:
;;
;; This package contains all of the Ivy related config.
;;
;;;

;;; Code:
;; Session
(use-package session
  :ensure t
  :defer t
  :hook (after-init . session-initialize))

;; Ivy
(use-package counsel
  :ensure t
  :defer t
  :config
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-switch-buffer)
         ("C-x C-f" . counsel-find-files))
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode)))

(message "Loaded init-ivy.el")
(provide 'init-ivy)
;;; init-ivy.el ends here
