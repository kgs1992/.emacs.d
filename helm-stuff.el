;;; helm-stuff.el --- Helm related config -*- lexical-binding: t; -*-
;;
;; Author:  Kiran Shenoy
;; URL:     https://github.com/kgs1992/.emacs.d/
;;
;;; Commentary:
;;
;; This package contains all of the Helm related config.
;;
;;;

;;; Code:
;; Session
(use-package session
  :ensure t
  :hook (after-init-hook . session-initialize))

;; Helm
(use-package helm
  :ensure t
  :config
  (helm-mode 1)
  :bind (("M-x" . 'helm-M-x)
         ("C-x f" . 'helm-for-files)
         ("C-x C-f" . 'helm-find-files)
         ("M-DEL" . 'helm-find-files-up-one-level)))

(provide 'helm-stuff)
;;; helm-stuff.el ends here
