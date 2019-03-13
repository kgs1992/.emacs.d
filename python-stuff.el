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
  :after (virtualenvwrapper)
  :hook ((python-mode . #'auto-virtualenvwrapper-activate)
         (window-configuration-change . 'auto-virtualenv-set-virtualenv)
         (focus-in . 'auto-virtualenv-set-virtualenv)))

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

;; Company mode
(use-package company-jedi
  :ensure t
  :defer t
  :after (jedi company helm-company)
  :hook (python-mode . (lambda () (add-to-list 'company-backends 'company-jedi))))

;; Python formatytting
(use-package yapfify
  :ensure t
  :defer t
  :init
  (defvar yapf-formatting t "Enable formatting through yapf. (setq-local yapf-formatting nil) to disable")
  (defun toggle-yapf-formatting ()
    "Enable/disable yapf formatting for this buffer"
    (interactive)
    (if yapf-formatting
        (setq-local yapf-formatting nil)
      (setq-local yapf-formatting t)))
  (defun enable-global-yapf-formatting ()
    "Enable yapf formatting globally"
    (interactive)
    (setq yapf-formatting t))
  (defun disable-global-yapf-formatting ()
    "Disable yapf formatting globally"
    (interactive)
    (setq yapf-formatting nil))
  :hook (python-mode . (lambda () (add-hook 'before-save-hook
                                            (lambda ()
                                              (if yapf-formatting
                                                  (yapfify-region (region-beginning) (region-end))))
                                            nil 'local))))

(provide 'python-stuff)
;;; python-stuff.el ends here
