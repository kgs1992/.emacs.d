;;; init-package.el --- Package related config -*- lexical-binding: t; -*-
;;
;; Author:  Kiran Shenoy
;; URL:     https://github.com/kgs1992/.emacs.d/
;;
;;; Commentary:
;;
;; This package contains all of the package related config.
;;
;;;

;;; Code:
;; Package config & init
(require 'package)
(setq package--init-file-ensured t ; Don't modify init
      package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(setq package-enable-at-startup nil)
(package-initialize)

;; Ensure we have use-package
(dolist (package '(use-package))
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))
(require 'use-package)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/local-packages"))

;; Auto update packages
(use-package auto-package-update
  :ensure t
  :defer t
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-interval 30)
  :hook (after-init .  auto-package-update-maybe))

(message "Loaded init-package.el")
(provide 'init-package)
;;; init-package.el ends here
