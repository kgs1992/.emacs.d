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
  :bind (("C-x b" . ivy-switch-buffer)
         (:map ivy-minibuffer-map ("TAB" . ivy-partial)))
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
  :defines (all-the-icons-icon-alist
            all-the-icons-dir-icon-alist
            bookmark-alist)
  :functions (all-the-icons-icon-for-file
              all-the-icons-icon-for-mode
              all-the-icons-icon-family
              all-the-icons-faicon
              all-the-icons-octicon
              all-the-icons-material
              all-the-icons-match-to-alist
              all-the-icons-auto-mode-match?
              all-the-icons-dir-is-submodule
              my-ivy-rich-bookmark-type)
  :commands (ivy-rich-bookmark-filename
             ivy-rich-bookmark-type)
  :preface
  (defun ivy-rich-bookmark-name (candidate)
    (car (assoc candidate bookmark-alist)))

  (defun ivy-rich-buffer-icon (candidate)
    "Display buffer icons in `ivy-rich'."
    (when (display-graphic-p)
      (let* ((buffer (get-buffer candidate))
             (buffer-file-name (buffer-file-name buffer))
             (major-mode (buffer-local-value 'major-mode buffer))
             (icon (if (and buffer-file-name
                            (all-the-icons-auto-mode-match?))
                       (all-the-icons-icon-for-file (file-name-nondirectory buffer-file-name) :v-adjust -0.05)
                     (all-the-icons-icon-for-mode major-mode :v-adjust -0.05))))
        (if (symbolp icon)
            (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0)
          icon))))

  (defun ivy-rich-file-icon (candidate)
    "Display file icons in `ivy-rich'."
    (when (display-graphic-p)
      (let* ((path (file-local-name (concat ivy--directory candidate)))
             (file (file-name-nondirectory path))
             (icon (cond
                    ((file-directory-p path)
                     (cond
                      ((and (fboundp 'tramp-tramp-file-p)
                            (tramp-tramp-file-p default-directory))
                       (all-the-icons-octicon "file-directory" :height 1.0 :v-adjust 0.01))
                      ((file-symlink-p path)
                       (all-the-icons-octicon "file-symlink-directory" :height 1.0 :v-adjust 0.01))
                      ((all-the-icons-dir-is-submodule path)
                       (all-the-icons-octicon "file-submodule" :height 1.0 :v-adjust 0.01))
                      ((file-exists-p (format "%s/.git" path))
                       (all-the-icons-octicon "repo" :height 1.1 :v-adjust 0.01))
                      (t (let ((matcher (all-the-icons-match-to-alist path all-the-icons-dir-icon-alist)))
                           (apply (car matcher) (list (cadr matcher) :v-adjust 0.01))))))
                    ((string-match "^/.*:$" path)
                     (all-the-icons-material "settings_remote" :height 1.0 :v-adjust -0.2))
                    ((not (string-empty-p file))
                     (all-the-icons-icon-for-file file :v-adjust -0.05)))))
        (if (symbolp icon)
            (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0)
          icon))))

  (defun ivy-rich-dir-icon (_candidate)
    "Display directory icons in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-octicon "file-directory" :height 1.0 :v-adjust 0.01)))

  (defun ivy-rich-function-icon (_candidate)
    "Display function icons in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-faicon "cube" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-purple)))

  (defun ivy-rich-variable-icon (_candidate)
    "Display variable icons in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-faicon "tag" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-lblue)))

  (defun ivy-rich-symbol-icon (_candidate)
    "Display symbol icons in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-octicon "gear" :height 0.9 :v-adjust -0.05)))

  (defun ivy-rich-theme-icon (_candidate)
    "Display theme icons in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-material "palette" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-lblue)))

  (defun ivy-rich-keybinding-icon (_candidate)
    "Display keybindings icons in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-material "keyboard" :height 1.0 :v-adjust -0.2)))

  (defun ivy-rich-library-icon (_candidate)
    "Display library icons in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-material "view_module" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-lblue)))

  (defun ivy-rich-package-icon (_candidate)
    "Display package icons in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-faicon "archive" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-silver)))

  (when (display-graphic-p)
    (defun my-ivy-rich-bookmark-type (candidate)
      (let ((filename (file-local-name (ivy-rich-bookmark-filename candidate))))
        (cond ((null filename)
               (all-the-icons-material "block" :v-adjust -0.2 :face 'warning))  ; fixed #38
              ((file-remote-p filename)
               (all-the-icons-material "wifi_tethering" :v-adjust -0.2 :face 'mode-line-buffer-id))
              ((not (file-exists-p filename))
               (all-the-icons-material "block" :v-adjust -0.2 :face 'error))
              ((file-directory-p filename)
               (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust -0.05))
              (t (all-the-icons-icon-for-file (file-name-nondirectory filename) :height 0.9 :v-adjust -0.05)))))
    (advice-add #'ivy-rich-bookmark-type :override #'my-ivy-rich-bookmark-type))
  :hook ((ivy-mode . ivy-rich-mode)
         (ivy-rich-mode . (lambda ()
                            (setq ivy-virtual-abbreviate
                                  (or (and ivy-rich-mode 'abbreviate) 'name)))))
  :init
  ;; For better performance
  (setq ivy-rich-parse-remote-buffer nil)

  ;; Setting tab size to 1, to insert tabs as delimiters
  (add-hook 'minibuffer-setup-hook
            (lambda ()
              (setq tab-width 1)))

  (setq ivy-rich-display-transformers-list
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand))
           :delimiter "\t")
          ivy-switch-buffer-other-window
          (:columns
           ((ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand))
           :delimiter "\t")
          counsel-switch-buffer
          (:columns
           ((ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand))
           :delimiter "\t")
          counsel-switch-buffer-other-window
          (:columns
           ((ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand))
           :delimiter "\t")
          persp-switch-to-buffer
          (:columns
           ((ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand))
           :delimiter "\t")
          counsel-M-x
          (:columns
           ((ivy-rich-function-icon)
            (counsel-M-x-transformer (:width 50))
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
          counsel-describe-function
          (:columns
           ((ivy-rich-function-icon)
            (counsel-describe-function-transformer (:width 50))
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
          counsel-describe-variable
          (:columns
           ((ivy-rich-variable-icon)
            (counsel-describe-variable-transformer (:width 50))
            (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
          counsel-apropos
          (:columns
           ((ivy-rich-symbol-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-info-lookup-symbol
          (:columns
           ((ivy-rich-symbol-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-descbinds
          (:columns
           ((ivy-rich-keybinding-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-find-file
          (:columns
           ((ivy-rich-file-icon)
            (ivy-read-file-transformer))
           :delimiter "\t")
          counsel-file-jump
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-dired
          (:columns
           ((ivy-rich-file-icon)
            (ivy-read-file-transformer))
           :delimiter "\t")
          counsel-dired-jump
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-fzf
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-git
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-recentf
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate (:width 0.8))
            (ivy-rich-file-last-modified-time (:face font-lock-comment-face)))
           :delimiter "\t")
          counsel-bookmark
          (:columns
           ((ivy-rich-bookmark-type)
            (ivy-rich-bookmark-name (:width 40))
            (ivy-rich-bookmark-info))
           :delimiter "\t")
          counsel-package
          (:columns
           ((ivy-rich-package-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-find-library
          (:columns
           ((ivy-rich-library-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-load-library
          (:columns
           ((ivy-rich-library-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-load-theme
          (:columns
           ((ivy-rich-theme-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-projectile-switch-project
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-projectile-find-file
          (:columns
           ((ivy-rich-file-icon)
            (counsel-projectile-find-file-transformer))
           :delimiter "\t")
          counsel-projectile-find-dir
          (:columns
           ((ivy-rich-dir-icon)
            (counsel-projectile-find-dir-transformer))
           :delimiter "\t")
          treemacs-projectile
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate))
           :delimiter "\t"))))

(use-package ivy-posframe
  :ensure t
  :defer t
  :config
  (setq ivy-posframe-display-functions-alist
        '((swiper          . nil)
          (complete-symbol . ivy-posframe-display-at-point)
          (t               . ivy-posframe-display-at-window-center)))
  (setq ivy-posframe-parameters
        '((left-fringe . left-fringe-width)
          (right-fringe . right-fringe-width)))
  (setq ivy-posframe-border-width 2)
  (setq ivy-posframe-hide-minibuffer t)
  :hook (after-init . ivy-posframe-mode))

(use-package ivy-prescient
  :ensure t
  :defer t
  :hook (after-init . ivy-prescient-mode))

(message "Loaded init-ivy.el")
(provide 'init-ivy)
;;; init-ivy.el ends here
