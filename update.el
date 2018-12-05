;;; update.el --- Update installed packages -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-
;;
;; Author:  Kiran Shenoy
;; URL:     https://github.com/kgs1992/.emacs.d/
;;
;;; Commentary:
;;
;; Most of this is borrowed and is very messy.
;;
;;;

;;; Code:
;; Package and compile related
(setq load-prefer-newer t)
;; Init package
(load "~/.emacs.d/package-stuff.el")

(require 'package)
(require 'auto-package-update)

(auto-package-update-now)

(provide 'update)
;;; update.el ends here
