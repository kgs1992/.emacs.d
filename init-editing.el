;;; init-editing.el --- Editor related config
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
;; General purpose config to make editing more peaceful
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

;; Clenup whitespaces
(use-package whitespace-cleanup-mode
  :ensure t
  :defer t
  :hook (after-init . global-whitespace-cleanup-mode))

;; Turn on spell check
(use-package flyspell
  :defer t
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

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
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (setq show-paren-style 'expression))

;; Volatile highlights- visual feedback for operations (example: undo)
(use-package volatile-highlights
  :ensure t
  :defer t
  :hook (after-init . volatile-highlights-mode))

;; Autocomplete - company
(use-package company
  :ensure t
  :defer t
  :config
  (setq company-tooltip-limit 20)                      ; Bigger popup window
  (setq company-idle-delay .3)                         ; Decrease delay before autocompletion popup shows
  (setq company-echo-delay 0)                          ; Remove annoying blinking
  (setq company-begin-commands '(self-insert-command)) ; Start autocompletion only after typing
  (setq company-tooltip-align-annotations t)
  :hook (after-init . global-company-mode))

(use-package company-posframe
  :ensure t
  :defer t
  :hook (after-init . company-posframe-mode))

(use-package company-prescient
  :ensure t
  :defer t
  :hook (after-init . company-prescient-mode))

;; Autocomplete - ac
;; (use-package auto-complete
;;   :ensure t
;;   :defer t
;;   :config
;;   ;; (ac-config-default)
;;   ;; (ac-flyspell-workaround)
;;   ;; (setq
;;   ;;  ac-auto-show-menu 1
;;   ;;  ac-auto-start t
;;   ;;  ac-menu-height 20
;;   ;;  ac-delay 0.01
;;   ;;  )
;;   (global-auto-complete-mode t))


;; Swiper (better C-s)
(use-package swiper
  :ensure t
  :defer t
  :after ivy
  :bind (("C-s" . swiper)))
;; (use-package swiper-helm
;;   :ensure t
;;   :defer t
;;   :after (helm)
;;   :bind (("C-s" . swiper-helm)))

;; Commenting
(global-set-key (kbd "M-;") 'comment-or-uncomment-region)

;; Show column number and line number
(use-package nlinum
  :ensure t
  :defer t
  :config
  (setq nlinum-format "%d\u2502"
        nlinum-highlight-current-line t)
  (dolist (mode '(column-number-mode line-number-mode))
    (when (fboundp mode) (funcall mode t)))
  :hook ((text-mode prog-mode) . nlinum-mode))

;; ;; Show current function/class
(use-package which-func
  :defer t
  :hook (prog-mode . which-function-mode))

;; Tab/spacing
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; Undo-redo keybindings
(use-package undo-fu
  :ensure t
  :defer t
  :bind (("C--" . undo-fu-only-undo)
         ("M--" . undo-fu-only-redo)))

(use-package undo-fu-session
  :ensure t
  :defer t
  :hook (after-init . global-undo-fu-session-mode))

;; Symbol highlighting
(use-package symbol-overlay
  :ensure t
  :defer t
  :hook (prog-mode . (lambda ()
                       (symbol-overlay-mode)
                       (setq symbol-overlay-scope t))))

;; Backward delete word instead of kill
(use-package evil
  :ensure t
  :defer t
  :bind (("M-DEL" . evil-delete-backward-word)))

(use-package flycheck
  :ensure t
  :defer t
  :config
  (defun flycheck-list-errors-only-when-errors ()
    "Show buffer with flycheck error."
    (if flycheck-current-errors
        (flycheck-list-errors)
      (-when-let (buffer (get-buffer flycheck-error-list-buffer))
        (dolist (window (get-buffer-window-list buffer))
          (quit-window nil window)))))
  :hook ((after-init . global-flycheck-mode)
         (before-save . flycheck-list-errors-only-when-errors)))

(use-package lsp-mode
  :ensure t
  :config
  ;; use flycheck, not flymake
  (setq lsp-prefer-flymake nil)
  (setq lsp-auto-guess-root nil)
  (setq lsp-print-performance nil)
  (setq lsp-log-io nil)
  (setq lsp-eldoc-hook nil)
  (setq lsp-idle-delay 0.500))

;; optional - provides fancy overlay information
(use-package lsp-ui
  :ensure t
  :defer t
  :config
  (setq lsp-ui-doc-delay 2)
  (setq lsp-ui-doc-position 'at-point))

(use-package flycheck-posframe
  :ensure t
  :defer t
  :after flycheck
  :hook (flycheck-mode . flycheck-posframe-mode))

;; (use-package format-all
;;   :ensure t
;;   :defer t
;;   :hook (prog-mode . format-all-mode))

;; Text scaling
(use-package default-text-scale
  :ensure t
  :defer t
  :bind (("s-=" . default-text-scale-increase)
         ("s--" . default-text-scale-decrease)
         ("s-0" . default-text-scale-reset))
  )

;; Highlight indent guides
(use-package highlight-indent-guides
  :ensure t
  :defer t
  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\┊)
  :hook (prog-mode . highlight-indent-guides-mode))

;; Dim unfocused windows
(use-package dimmer
  :ensure t
  :defer t
  :config
  (setq dimmer-adjustment-mode :foreground)
  (setq dimmer-fraction 0.5)
  (setq dimmer-watch-frame-focus-events nil)
  (dimmer-configure-posframe)
  (dimmer-configure-magit)
  (dimmer-configure-company-box)
  (defun dimmer-lsp-ui-doc-p ()
    (string-prefix-p " *lsp-ui-doc-" (buffer-name)))
  (add-to-list 'dimmer-prevent-dimming-predicates #'dimmer-lsp-ui-doc-p)

  (defun advices/dimmer-config-change-handler ()
    (dimmer--dbg-buffers 1 "dimmer-config-change-handler")
    (let ((ignore (cl-some (lambda (f) (and (fboundp f) (funcall f)))
                           dimmer-prevent-dimming-predicates)))
      (dimmer-process-all (not ignore))))
  (advice-add 'dimmer-config-change-handler :override #'advices/dimmer-config-change-handler)
  :hook (after-init . dimmer-mode))

(message "Loaded init-editing.el")
(provide 'init-editing)
;;; init-editing.el ends here
