;;; python-stuff.el --- Python related config -*- lexical-binding: t; -*-
;;
;; Author:  Kiran Shenoy
;; URL:     https://github.com/kgs1992/.emacs.d/
;;
;;; Commentary:
;;
;; This package contains all of the python related config.
;;
;;;

;;; Code:
;; Virtualenv related
(use-package virtualenvwrapper
  :ensure t
  :defer t
  :config
  (setq venv-location (expand-file-name "~/.virtualenvs"))
  (setq python-environment-directory venv-location))

(use-package auto-virtualenvwrapper
  :ensure t
  :defer t
  :hook (python-mode . #'auto-virtualenvwrapper-activate))

;; Default indent
(setq python-indent-offset 4)

;; Jedi
(use-package jedi
  :ensure t
  :defer t
  :config
  (setq jedi:setup-keys t)
  (setq jedi:complete-on-dot t)
  :hook (python-mode . jedi:setup))

(provide 'python-stuff)
;;; python-stuff.el ends here
