;; Magit stuff
(el-get-bundle magit)
(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")
(setq magit-buffer-name-format "*%x%M%v: %t%x*")
(setq magit-diff-use-overlays nil)

(defun magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
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

(bind-key "q" #'magit-quit-session magit-status-mode-map)

(global-set-key (kbd "C-x g") 'magit-status)

;; Git gutter
(el-get-bundle 'git-gutter-fringe+)
(require 'git-gutter-fringe+)
(global-git-gutter+-mode)
(global-set-key (kbd "M-p") 'git-gutter+-previous-hunk)
(global-set-key (kbd "M-n") 'git-gutter+-next-hunk)
