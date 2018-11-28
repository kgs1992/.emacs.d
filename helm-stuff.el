;;; helm-stuff.el --- Helm related config
;;
;; Author:  Kiran Shenoy
;; URL:     https://github.com/kgs1992/.emacs.d/
;;
;;; Commentary:
;;
;; This package contains all of the Helm related config.
;;
;;;

;;; Code:
;; Session
(el-get-bundle session)
(require 'session)
(add-hook 'after-init-hook 'session-initialize)

;; Helm
(el-get-bundle helm)
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)

(provide 'helm-stuff)
;;; helm-stuff.el ends here
