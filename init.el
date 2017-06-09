;; Startup stuff
(load "~/.emacs.d/package-stuff.el")

;; Set exec-path and stuff
(el-get-bundle exec-path-from-shell)
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; Configure useful emacs extensions
(load "~/.emacs.d/helm-stuff.el")
(load "~/.emacs.d/projectile-stuff.el")
(load "~/.emacs.d/tramp-stuff.el")
(load "~/.emacs.d/magit-stuff.el")
(load "~/.emacs.d/editing-stuff.el")
(load "~/.emacs.d/tabbar-stuff.el")

;; PYTHON RELATED
(load "~/.emacs.d/python-stuff.el")

;; GO RELATED
(load "~/.emacs.d/go-stuff.el")


;; Theme related
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (load "~/.emacs.d/theme-stuff.el"))))
  (load "~/.emacs.d/theme-stuff.el"))

(el-get 'sync)
