;;; package-stuff.el --- Package related config -*- lexical-binding: t; -*-
;;
;; Author:  Kiran Shenoy
;; URL:     https://github.com/kgs1992/.emacs.d/
;;
;;; Commentary:
;;
;; This package contains all of the package related config.
;;
;;;

;;; Code:
;; Package config & init
(require 'package)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(dolist (package '(use-package))
  (unless (package-installed-p package)
    (package-install package)))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/local-packages"))

(provide 'package-stuff)
;;; package-stuff.el ends here
