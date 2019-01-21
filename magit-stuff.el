;;; magit-stuff.el --- Magit related config -*- lexical-binding: t; -*-
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
;; Magit stuff
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
  :config
  (global-git-gutter+-mode)
  :bind (("M-p" . 'git-gutter+-previous-hunk)
         ("M-n" . 'git-gutter+-next-hunk)))

(provide 'magit-stuff)
;;; magit-stuff.el ends here
