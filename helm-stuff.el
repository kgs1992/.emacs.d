;;; helm-stuff.el --- Helm related config -*- lexical-binding: t; -*-
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
  :hook (after-init . session-initialize))

;; Helm
(use-package helm
  :ensure t
  :defer t
  :commands (fix-helm-colors-for-frame)
  :config
  (helm-mode 1)
  (defun fix-helm-colors()
    "Fix the colors for helm."
    (set-face-attribute 'helm-selection nil
			            :background "grey10"
			            :foreground "blue")
    (set-face-attribute 'helm-source-header nil
			            :background "gray30"
			            :foreground "green"))

  (defun fix-helm-colors-for-frame(frame)
    "Fix the colors for helm for selected frame.
    FRAME: Frame to change attributes of"
    (with-selected-frame frame
	  (fix-helm-colors)))

  (unless (daemonp)
    (fix-helm-colors))

  :bind (("M-x" . 'helm-M-x)
         ("C-x f" . 'helm-for-files)
         ("C-x C-f" . 'helm-find-files)
         ("M-DEL" . 'helm-find-files-up-one-level)))

(if (daemonp)
	(add-hook 'after-make-frame-functions #'fix-helm-colors-for-frame))

(provide 'helm-stuff)
;;; helm-stuff.el ends here
