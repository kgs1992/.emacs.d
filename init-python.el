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
                                     (locate-file "pylint" exec-path)))
  (defun activate-if-python-mode ()
      (when (eq (with-current-buffer (current-buffer)
                major-mode) 'python-mode)
          (auto-virtualenvwrapper-activate)
          ;; (set-flychecker-executables)))
          ))
  :hook ((python-mode . activate-if-python-mode)
         (window-configuration-change . activate-if-python-mode)
         (after-focus . activate-if-python-mode)))

(use-package lsp-jedi
  :ensure t
  :after lsp-mode
  :config
  (setq lsp-pyls-disable-warning t)
  ;; (add-to-list lsp-disabled-clients 'pyls)
  ;; (add-to-list lsp-enabled-clients 'jedi)
  :hook
  (python-mode . lsp))

(message "Loaded init-python.el")
(provide 'init-python)
;;; init-python.el ends here
