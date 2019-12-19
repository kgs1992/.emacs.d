;;; init-polymode.el --- Editor related config
;;
;; Author:  Kiran Shenoy
;; URL:     https://github.com/kgs1992/.emacs.d/
;;
;;; Commentary:
;;
;; This package contains polymode definitions.
;;
;;;

;;; Code:
;; Polymode
(use-package
  yaml-mode
  :ensure t
  :defer t)

(use-package
  jinja2-mode
  :ensure t
  :defer t)

(use-package polymode
  :ensure t
  :defer t
  :mode ("\.yaml$" . poly-yaml-jinja2-mode)
  :config (define-hostmode poly-yaml-hostmode
            :mode 'yaml-mode)
  (define-innermode poly-jinja2-yaml-innermode
    :mode 'jinja2-mode
    :head-matcher "{[%{#][+-]?"
    :tail-matcher "[+-]?[%}#]}"
    :head-mode 'body
    :tail-mode 'body)
  (define-polymode
    poly-yaml-jinja2-mode
    :hostmode 'poly-yaml-hostmode
    :innermodes '(poly-jinja2-yaml-innermode)))

(message "Loaded init-polymode.el")
(provide 'init-polymode)
;;; init-polymode.el ends here
