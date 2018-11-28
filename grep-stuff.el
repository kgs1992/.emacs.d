;;; grep-stuff.el --- Searching related config
;;
;; Author:  Kiran Shenoy
;; URL:     https://github.com/kgs1992/.emacs.d/
;;
;;; Commentary:
;;
;; This package contains all of the theme related config.
;;
;;;

;;; Code:
;; wgrep - Edit and save grep buffers
(el-get-bundle wgrep)
(require 'wgrep)

;; ripgrep - Super fast grep
(el-get-bundle s)
(el-get-bundle names)
(el-get-bundle dajva/rg.el)
(require 'rg)

(defun ripgrep ()
  "Use rg-project if within a project context."
  (interactive
   (if (projectile-project-p)
       (call-interactively 'rg-project)
     (call-interactively 'rg))))
(global-set-key (kbd "M-s") 'ripgrep)

(provide 'grep-stuff)
;;; grep-stuff.el ends here
