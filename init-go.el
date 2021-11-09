;;; init-go.el --- Golang related config  -*- lexical-binding: t; -*-
;;
;; Author:  Kiran Shenoy
;; URL:     https://github.com/kgs1992/.emacs.d/
;;
;;; Commentary:
;;
;; This package contains all of the golang related config.
;;
;;;

;;; Code:
(use-package go-mode
  :ensure t
  :defer t
  :after (lsp-mode company)
  :bind
  (("C-c ." . lsp-find-definition)
   ("C-c ," . pop-global-mark))
  :config
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t)
     ("gopls.experimentalTemplateSupport" t t)))
  (setq lsp-go-codelenses nil)
  :hook ((go-mode . lsp)
         (before-save . lsp-format-buffer)
         ;; (before-save . lsp-organize-imports)
         ))

(use-package company-go
  :ensure t
  :defer t
  :config
  (setq company-go-show-annotation t)
  :hook (go-mode . (lambda ()
                     (set (make-local-variable 'company-backends) '(company-go))
                     (company-mode 1))))

(message "Loaded init-go.el")
(provide 'init-go)
;;; init-go.el ends here
