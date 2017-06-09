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

(defun my-python-mode-hook ()
  (flycheck-mode)
  (local-set-key (kbd "C-z") 'jedi:goto-definition))
(add-hook 'python-mode-hook 'my-python-mode-hook)

;; Default indent
(setq python-indent-offset 4)

;; Jedi
(el-get-bundle jedi)
(require 'jedi)
(provide 'jedi-setup)
;; base setup
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)
;; default keyboard shortcuts
;;(setq jedi:setup-keys t)
;; complete on `dot`
(setq jedi:complete-on-dot t)

;; Code browser that is quite useless
(eval-after-load "python"
  '(define-key python-mode-map "\M-/" 'jedi-direx:pop-to-buffer))
(add-hook 'jedi-mode-hook 'jedi-direx:setup)
