;;; modeline-stuff.el --- Modeline related config
;;
;; Author:  Kiran Shenoy
;; URL:     https://github.com/kgs1992/.emacs.d/
;;
;;; Commentary:
;;
;; This package contains all of the modeline related config.
;;
;;;

;;; Code:
;; Cleanup the modeline
;; source: https://www.masteringemacs.org/article/hiding-replacing-modeline-strings
(defvar mode-line-cleaner-alist
  `((auto-complete-mode . " α")
    (yas/minor-mode . " υ")
    (paredit-mode . " π")
    (eldoc-mode . "")
    (abbrev-mode . "")
    (smartparens-mode . "")
    (helm-mode . "")
    (auto-revert-mode . "")
    (flycheck-mode . "FC")
    ;; Major modes
    (lisp-interaction-mode . "λ")
    (hi-lock-mode . "")
    (python-mode . "Py")
    (emacs-lisp-mode . "EL")
    (nxhtml-mode . "nx"))
  "Alist for `clean-mode-line'.

When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")


(defun clean-mode-line ()
  (interactive)
  (loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                  (mode-str (cdr cleaner))
                  (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
               (setcar old-mode-str mode-str))
             ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))


(add-hook 'after-change-major-mode-hook 'clean-mode-line)

;;doom-modeline
(el-get-bundle 'eldoc-eval)
(el-get-bundle 'all-the-icons)
(el-get-bundle 'f)
(el-get-bundle 'dash)
(el-get-bundle 's)
(el-get-bundle rx
  :url "https://raw.githubusercontent.com/typester/emacs/master/lisp/emacs-lisp/rx.el")
(el-get-bundle shrink-path
  :url "https://gitlab.com/bennya/shrink-path.el/raw/master/shrink-path.el")
(el-get-bundle seagle0128/doom-modeline)
(require 'doom-modeline)
(doom-modeline-init)
(setq doom-modeline-buffer-file-name-style 'relative-to-project)

(provide modeline-stuff)
;;; modeline-stuff.el ends here
