;;; editing-stuff.el --- Editor related config
;;
;; Author:  Kiran Shenoy
;; URL:     https://github.com/kgs1992/.emacs.d/
;;
;;; Commentary:
;;
;; This package contains all of the editor related config.
;;
;;;

;;; Code:
;; General purpose stuff to make editing more peaceful
(setq inhibit-startup-screen t)
(setq debug-on-quit nil)
(setq message-log-max 2000)
(size-indication-mode t)
(delete-selection-mode 0)
(transient-mark-mode 0)
(setq-default wrap-lines t)

;; Auto refresh buffers
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose t)

;; Open files in the same frame
(setq ns-pop-up-frames nil)

;; Delete trailing whitespaces
(use-package whitespace
  :ensure t
  :hook (before-save-hook . delete-trailing-whitespace))

;;Set backup dir
(setq auto-save-directory-fallback "~/.saves")
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Rainbow delimiters - Match parens
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode-hook . #'rainbow-delimiters-mode)
  :config
  (show-paren-mode 1)
  (setq show-paren-style 'expression))

;; Autocomplete
(use-package auto-complete
  :ensure t
  :config
  (ac-config-default)
  (setq
   ac-auto-show-menu 1
   ac-auto-start t
   ac-menu-height 20
   ac-delay 0.01
   )
  (global-auto-complete-mode t))


;; Swiper (better C-s)
(use-package swiper-helm
  :ensure t
  :after (helm)
  :bind (("C-s" . swiper-helm)))

;; Commenting
(global-set-key (kbd "M-;") 'comment-or-uncomment-region)

;; Show column number and line number
(use-package nlinum
  :ensure t
  :config
  (setq nlinum-format "%d\u2502")
  (dolist (mode '(column-number-mode line-number-mode))
    (when (fboundp mode) (funcall mode t)))
  :hook ((text-mode-hook prog-mode-hook) . nlinum-mode))

;; ;; Show current function/class
;; (which-function-mode 1)

;; Tab related stuff
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; Undo-redo keybindings
(use-package undo-tree
  :ensure t
  :bind (("C--" . undo-only)
         ("M--" . 'undo-tree-redo)))

;; Backward delete word instead of kill
(use-package evil
  :ensure t
  :bind (("M-DEL" . evil-delete-backward-word)))

(use-package flycheck
  :ensure t
  :config
  (defun flycheck-list-errors-only-when-errors ()
    "Show buffer with flycheck error."
    (if flycheck-current-errors
        (flycheck-list-errors)
      (-when-let (buffer (get-buffer flycheck-error-list-buffer))
        (dolist (window (get-buffer-window-list buffer))
          (quit-window nil window)))))
  :hook ((after-init-hook . global-flycheck-mode)
         (before-save-hook . #'flycheck-list-errors-only-when-errors)))

(use-package format-all
  :hook (prog-mode-hook . #'format-all-mode))

;; ;; easy-kill
;; (el-get-bundle leoliu/easy-kill)
;; (require 'easy-kill)
;; (global-set-key [remap kill-ring-save] 'easy-kill)

(provide 'editing-stuff)
;;; editing-stuff.el ends here
