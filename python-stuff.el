;;; python-stuff.el --- Python related config
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
(el-get-bundle virtualenvwrapper)
(require 'virtualenvwrapper)
(setq venv-location (expand-file-name "~/.virtualenvs"))
(setq python-environment-directory venv-location)

(el-get-bundle auto-virtualenvwrapper
  :url "https://raw.githubusercontent.com/robert-zaremba/auto-virtualenvwrapper.el/master/auto-virtualenvwrapper.el")
(require 'auto-virtualenvwrapper)
(add-hook 'python-mode-hook #'auto-virtualenvwrapper-activate)

;; Flycheck (linter)
(el-get-bundle flycheck)
(require 'flycheck)
(setq flycheck-python-pylint-executable "nulint-editor")
(add-hook 'python-mode-hook 'flycheck-mode)

;; Default indent
(setq python-indent-offset 4)

;; Jedi
(el-get-bundle jedi)
(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

(provide 'python-stuff)
;;; python-stuff.el ends here
