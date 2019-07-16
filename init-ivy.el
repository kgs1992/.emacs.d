;;; init-ivy.el --- Ivy related config -*- lexical-binding: t; -*-
;;
;; Author:  Kiran Shenoy
;; URL:     https://github.com/kgs1992/.emacs.d/
;;
;;; Commentary:
;;
;; This package contains all of the Ivy related config.
;;
;;;

;;; Code:
;; Session
(use-package session
  :ensure t
  :defer t
  :hook (after-init . session-initialize))

;; Smex
(use-package smex
  :ensure t
  :defer t
  :hook (after-init . smex-initialize))

;; Ivy
(use-package ivy
  :ensure t
  :defer t
  :config
  (setq ivy-ignore-buffers '("\\` " "\\`\\*")
        ivy-initial-inputs-alist nil)
  :bind (("C-x b" . ivy-switch-buffer))
  :hook (after-init . ivy-mode))

(use-package counsel
  :ensure t
  :defer t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file))
  :hook (after-init . counsel-mode))

(use-package ivy-rich
  :ensure t
  :defer t
  :config
  (setq ivy-virtual-abbreviate 'full
        ivy-rich-path-style 'abbrev)
  (defun ivy-rich-switch-buffer-icon (candidate)
    (with-current-buffer
        (get-buffer candidate)
      (let ((icon (all-the-icons-icon-for-mode major-mode)))
        (if (symbolp icon)
            (all-the-icons-icon-for-mode 'fundamental-mode)
          icon))))
  (setq ivy-rich-display-transformers-list
        (nconc
         ivy-rich-display-transformers-list
         '(ivy-switch-buffer
           (:columns
            ((ivy-rich-switch-buffer-icon :width 2)
             (ivy-rich-candidate (:width 30))
             (ivy-rich-switch-buffer-size (:width 7))
             (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
             (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
             (ivy-rich-switch-buffer-project (:width 15 :face success))
             (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
            :predicate
            (lambda (cand) (get-buffer cand))))))

  :hook (after-init . ivy-rich-mode))

(message "Loaded init-ivy.el")
(provide 'init-ivy)
;;; init-ivy.el ends here
