;;; init-magit.el --- Magit related config -*- lexical-binding: t; -*-
;;
;; Author:  Kiran Shenoy
;; URL:     https://github.com/kgs1992/.emacs.d/
;;
;;; Commentary:
;;
;; This package contains all of the Magit related config.
;;
;;;

;;; Code:
;; Magit config
(use-package magit
  :ensure t
  :defer t
  :after (flyspell)
  :commands (fix-magit-colors-for-frame)
  :config
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq magit-buffer-name-format "*%x%M%v: %t%x*")
  (setq magit-diff-use-overlays nil)

  (defun magit-toggle-whitespace ()
    "Toggle between ignore and don't ignore whitespace."
    (interactive)
    (if (member "-w" magit-diff-options)
        (magit-dont-ignore-whitespace)
      (magit-ignore-whitespace)))

  (defun magit-ignore-whitespace ()
    "Ignore whitespace."
    (interactive)
    (add-to-list 'magit-diff-options "-w")
    (magit-refresh))

  (defun magit-dont-ignore-whitespace ()
    "Don't ignore whitespace."
    (interactive)
    (setq magit-diff-options (remove "-w" magit-diff-options))
    (magit-refresh))

  (defun custom/kill-buffers (regexp)
    "Kill buffers matching REGEXP without asking for confirmation."
    (interactive "sKill buffers matching this regular expression: ")
    (cl-letf (((symbol-function 'kill-buffer-ask)
               (lambda (buffer) (kill-buffer buffer))))
      (kill-matching-buffers regexp)))

  (defadvice magit-status (around magit-fullscreen activate)
    "Turn fullscreen on for magit-status."
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun magit-quit-session ()
    "Restore previous window configuration and cleanup buffers."
    (interactive)
    (custom/kill-buffers "^\\*magit.*\\*")
    (jump-to-register :magit-fullscreen))

  (defun fix-magit-colors()
    "Fix the colors for magit diffs."
    (set-face-attribute 'magit-diff-hunk-heading nil
                        :foreground "white")
    (set-face-attribute 'magit-diff-hunk-heading-highlight nil
                        :foreground "white"
                        :background "grey10")
    (set-face-attribute 'magit-diff-removed nil
                        :background "black")
    (set-face-attribute 'magit-diff-removed-highlight nil
                        :background "black"))

  (defun fix-magit-colors-for-frame(frame)
    "Fix the colors for magit diffs for selected frame."
    (with-selected-frame frame
      (fix-magit-colors)))

  (defmacro pretty-magit (WORD ICON PROPS &optional NO-PROMPT?)
    "Replace sanitized WORD with ICON, PROPS and by default add to prompts."
    `(prog1
         (add-to-list 'pretty-magit-alist
                      (list (rx bow (group ,WORD (eval (if ,NO-PROMPT? "" ":"))))
                            ,ICON ',PROPS))
       (unless ,NO-PROMPT?
         (add-to-list 'pretty-magit-prompt (concat ,WORD ": ")))))

  (setq pretty-magit-alist nil)
  (setq pretty-magit-prompt nil)
  (pretty-magit "Feature" "" '(:foreground "slate gray" :height 1.0 :family "FontAwesome"))
  (pretty-magit "Add" "" '(:foreground "#375E97" :height 1.0 :family "FontAwesome"))
  (pretty-magit "Fix" "" '(:foreground "#FB6542" :height 1.0 :family "FontAwesome"))
  (pretty-magit "Clean" "" '(:foreground "#FFBB00" :height 1.0 :family "FontAwesome"))
  (pretty-magit "Docs" "" '(:foreground "#3F681C" :height 1.0 :family "FontAwesome"))
  (pretty-magit "master" "" '(:box nil :height 1.0 :family "github-octicons") t)
  (pretty-magit "origin" "" '(:box nil :height 1.0 :family "github-octicons") t)

  (defun add-magit-faces ()
    "Add face properties and compose symbols for buffer from pretty-magit."
    (interactive)
    (with-silent-modifications
      (--each pretty-magit-alist
        (-let (((rgx icon props) it))
          (save-excursion
            (goto-char (point-min))
            (while (search-forward-regexp rgx nil t)
              (compose-region
               (match-beginning 1) (match-end 1) icon)
              (when props
                (add-face-text-property
                 (match-beginning 1) (match-end 1) props))))))))

  ;; (advice-add 'magit-status :after 'add-magit-faces)
  (advice-add 'magit-refresh-buffer :after 'add-magit-faces)

  (unless (daemonp)
    (fix-magit-colors))

  :hook (git-commit-setup . git-commit-turn-on-flyspell)

  :bind (:map magit-status-mode-map
              ("q". #'magit-quit-session))
  :bind* (("C-x g" . magit-status)))

(if (daemonp)
	(add-hook 'after-make-frame-functions #'fix-magit-colors-for-frame))

;; Git gutter
(use-package git-gutter-fringe+
  :ensure t
  :defer t
  :bind (("M-p" . git-gutter+-previous-hunk)
         ("M-n" . git-gutter+-next-hunk))
  :config
  (setq git-gutter-fr+-side 'right-fringe)
  :hook ((after-init . (lambda () (require 'git-gutter-fringe+)))
         (after-init . global-git-gutter+-mode)))

(message "Loaded init-magit.el")
(provide 'init-magit)
;;; init-magit.el ends here
