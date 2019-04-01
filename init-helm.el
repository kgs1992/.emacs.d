;;; init-helm.el --- Helm related config -*- lexical-binding: t; -*-
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
(use-package session
  :ensure t
  :defer t
  :hook (after-init . session-initialize))

;; Helm
(use-package helm
  :ensure t
  :defer t
  :commands (fix-helm-colors-for-frame)
  :config
  (defun fix-helm-colors()
    "Fix the colors for helm."
    (set-face-attribute 'helm-selection nil
			            :background "#202328"
			            :foreground "#51afef")
    (set-face-attribute 'helm-source-header nil
			            :background "#21242b"
			            :foreground "#98be65"))

  (defun fix-helm-colors-for-frame(frame)
    "Fix the colors for helm for selected frame.
    FRAME: Frame to change attributes of"
    (with-selected-frame frame
	  (fix-helm-colors)))

  (unless (daemonp)
    (fix-helm-colors))

  :bind (("M-x" . 'helm-M-x)
         ("C-x f" . 'helm-for-files)
         ("C-x b" . 'helm-buffers-list)
         ("C-x C-f" . 'helm-find-files)
         ("M-DEL" . 'helm-find-files-up-one-level))
  :hook (after-init . helm-mode))

(if (daemonp)
	(add-hook 'after-make-frame-functions #'fix-helm-colors-for-frame))

(message "Loaded init-helm.el")
(provide 'init-helm)
;;; init-helm.el ends here
