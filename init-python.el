;;; init-python.el --- Python related config -*- lexical-binding: t; -*-
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
(use-package auto-virtualenvwrapper
  :ensure t
  :defer t
  :config
  (defun set-flychecker-executables ()
    "Configure virtualenv for flake8 and lint."
    (flycheck-set-checker-executable (quote python-flake8)
                                     (locate-file "flake8" exec-path))
    (flycheck-set-checker-executable (quote python-pylint)
                                     (locate-file "flake8" exec-path)))
  (defun activate-if-python-mode ()
      (when (eq (with-current-buffer (current-buffer)
                major-mode) 'python-mode)
          (auto-virtualenvwrapper-activate)
          (set-flychecker-executables)))
  :hook ((python-mode . activate-if-python-mode)
         (window-configuration-change . activate-if-python-mode)
         (after-focus . activate-if-python-mode)))

;; Jedi
(use-package jedi-core
  :ensure t
  :defer t
  ;; :config
  ;; (setq jedi:complete-on-dot nil)
  ;; (setq jedi:tooltip-method nil)
  :hook (python-mode . jedi:setup))

;; Company mode
(use-package company-jedi
  :ensure t
  :defer t
  :after company
  :hook (python-mode . (lambda () (add-to-list 'company-backends 'company-jedi))))

;; Python formatting
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

(message "Loaded init-python.el")
(provide 'init-python)
;;; init-python.el ends here
