;; rjsx-mode
(el-get-bundle rjsx-mode)
(require 'rjsx-mode)

(add-to-list 'auto-mode-alist '(".*\.js\'" . rjsx-mode))

(add-hook 'rjsx-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil) ;;Use space instead of tab
            (setq js-indent-level 4) ;;space width is 2 (default is 4)
            (setq js2-strict-missing-semi-warning nil))) ;;disable the semicolon warning

;; rjsx-mode
(el-get-bundle flycheck)
(require 'flycheck)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; use eslint with rjsx-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'rjsx-mode)

;; use local eslint from node_modules before global
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
