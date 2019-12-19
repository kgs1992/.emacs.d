;;; init-term.el --- Term related config -*- lexical-binding: t; -*-
;;
;; Author:  Kiran Shenoy
;; URL:     https://github.com/kgs1992/.emacs.d/
;;
;;; Commentary:
;;
;; This package contains all of the term related config.
;;
;;;

;;; Code:
;; Term for remote editing
(use-package vterm
  :ensure t
  :defer t
  :hook (after-init . (lambda () (require 'vterm))))

(use-package vterm-toggle
  :ensure t
  :defer t
  :after vterm
  :bind (("C-`" . vterm-toggle)
         :map vterm-mode-map
         ("s-<right>" . vterm-toggle-forward)
         ("s-<left>" . vterm-toggle-backward)))


(message "Loaded init-term.el")
(provide 'init-term)
;;; init-term.el ends here
