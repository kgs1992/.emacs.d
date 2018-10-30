;; wgrep - Edit and save grep buffers
(el-get-bundle wgrep)
(require 'wgrep)

;; ripgrep - Super fast grep
(el-get-bundle s)
(el-get-bundle names)
(el-get-bundle rg
  :url "https://raw.githubusercontent.com/dajva/rg.el/master/rg.el")
(require 'rg)

(defun ripgrep ()
  (interactive
   (if (projectile-project-p)
       (call-interactively 'rg-project)
     (call-interactively 'rg))))
(global-set-key (kbd "M-s") 'ripgrep)