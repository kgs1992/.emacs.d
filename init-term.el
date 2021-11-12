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
  :config
  (setq vterm-module-cmake-args "-USE_SYSTEM_LIBVTERM=yes")
  :hook (after-init . (lambda () (require 'vterm)))
  :bind ("s-~" . vterm))

(use-package multi-vterm
  :ensure t
  :defer t
  :after vterm
  :bind (("s-`" . multi-vterm-project)
         :map vterm-mode-map
         ("s-<right>" . multi-vterm-next)
         ("s-<left>" . multi-vterm-prev)))

(message "Loaded init-term.el")
(provide 'init-term)
;;; init-term.el ends here
