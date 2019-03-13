;;; jsx-stuff.el --- JSX related config -*- lexical-binding: t; -*-
;;
;; Author:  Kiran Shenoy
;; URL:     https://github.com/kgs1992/.emacs.d/
;;
;;; Commentary:
;;
;; This package contains all of the JSX related config.
;;
;;;

;;; Code:
;; rjsx-mode
(use-package rjsx-mode
  :ensure t
  :after (flycheck)
  :defer t
  :config
  (defun my-jsx-mode-hook()
    (setq indent-tabs-mode nil) ;;Use space instead of tab
    (setq js-indent-level 4) ;;space width is 2 (default is 4)
    (setq js2-strict-missing-semi-warning nil)) ;;disable the semicolon warning

  ;; disable jshint since we prefer eslint checking
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))

  ;; use eslint with rjsx-mode for jsx files
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode)

  ;; use local eslint from node_modules before global
  (defun my/use-eslint-from-node-modules ()
    "Use local eslint from node_modules before global."
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))

  :hook ((flycheck-mode . my/use-eslint-from-node-modules)
         (rjsx-mode . my-rjsx-mode-hook)))

(add-to-list 'auto-mode-alist '(".*\.js\'" . 'rjsx-mode))

(message "Loaded jsx-stuff.el")
(provide 'jsx-stuff)
;;; jsx-stuff.el ends here
