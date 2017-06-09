;; Session
(el-get-bundle session)
(require 'session)
(add-hook 'after-init-hook 'session-initialize)

;; Helm
(el-get-bundle helm)
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
