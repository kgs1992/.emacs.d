;;; init-modeline.el --- Modeline related config -*- lexical-binding: t; -*-
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
;;doom-modeline
(use-package doom-modeline
  :ensure t
  :defer t
  :init
  ;; Cleanup the modeline
  ;; source: https://www.masteringemacs.org/article/hiding-replacing-modeline-strings
  (defvar mode-line-cleaner-alist
    `((auto-complete-mode . " α")
      (yas/minor-mode . " υ")
      (paredit-mode . " π")
      (eldoc-mode . "")
      (abbrev-mode . "")
      (smartparens-mode . "")
      (ivy-mode . "")
      (counsel-mode . "")
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
    (cl-loop for cleaner in mode-line-cleaner-alist
             do (let* ((mode (car cleaner))
                       (mode-str (cdr cleaner))
                       (old-mode-str (cdr (assq mode minor-mode-alist))))
                  (when old-mode-str
                    (setcar old-mode-str mode-str))
                  ;; major mode
                  (when (eq mode major-mode)
                    (setq mode-name mode-str)))))

  :config
  (setq doom-modeline-buffer-file-name-style 'relative-to-project)
  :hook ((after-init . doom-modeline-mode)
         (find-file . (lambda () (if (not doom-modeline-icon) (setq doom-modeline-icon t))))
         (after-change-major-mode . clean-mode-line)))

(message "Loaded init-modeline.el")
(provide 'init-modeline)
;;; init-modeline.el ends here
