;;; fira-code-mode.el --- Show ligatures for Fira code -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-
;;
;;; Commentary:
;;
;; Copied from:
;; https://github.com/tonsky/FiraCode/wiki/Emacs-instructions#using-prettify-symbols
;;
;; Remember to install this font:
;; https://github.com/tonsky/FiraCode/issues/211#issuecomment-239058632
;;
;;;

;;; Code:

(defun fira-code-mode--make-alist (list)
  "Generate prettify-symbols alist from LIST."
  (let ((idx -1))
    (mapcar
     (lambda (s)
       (setq idx (1+ idx))
       (let* ((code (+ #Xe100 idx))
              (width (string-width s))
              (prefix ())
              (suffix '(?\s (Br . Br)))
              (n 1))
         (while (< n width)
           (setq prefix (append prefix '(?\s (Br . Bl))))
           (setq n (1+ n)))
         (cons s (append prefix suffix (list (decode-char 'ucs code))))))
     list)))

(defconst fira-code-mode--ligatures
  '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\"
    "{-" "[]" "::" ":::" ":=" "!!" "!=" "!==" "-}"
    "--" "---" "-->" "->" "->>" "-<" "-<<" "-~"
    "#{" "#[" "##" "###" "####" "#(" "#?" "#_" "#_("
    ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*"
    "/**" "/=" "/==" "/>" "//" "///" "&&" "||" "||="
    "|=" "|>" "^=" "$>" "++" "+++" "+>" "=:=" "=="
    "===" "==>" "=>" "=>>" "<=" "=<<" "=/=" ">-" ">="
    ">=>" ">>" ">>-" ">>=" ">>>" "<*" "<*>" "<|" "<|>"
    "<$" "<$>" "<!--" "<-" "<--" "<->" "<+" "<+>" "<="
    "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<" "<~"
    "<~~" "</" "</>" "~@" "~-" "~=" "~>" "~~" "~~>" "%%"
    "x" ":" "+" "+" "*"))

(defvar fira-code-mode--old-prettify-alist)
(defvar fira-code-mode--only-ligatures nil
  "Only use prettify-symbols for ligatures.
This will drop the `prettify-symbols-alist` defined by other modes on the buffer.")

(defun fira-code-mode--enable ()
  "Enable Fira Code ligatures in current buffer."
  (setq-local fira-code-mode--old-prettify-alist prettify-symbols-alist)
  (if fira-code-mode--only-ligatures
      (setq-local prettify-symbols-alist (fira-code-mode--make-alist fira-code-mode--ligatures))
    (setq-local prettify-symbols-alist (append (fira-code-mode--make-alist fira-code-mode--ligatures) fira-code-mode--old-prettify-alist)))
  (prettify-symbols-mode t))

(defun fira-code-mode--disable ()
  "Disable Fira Code ligatures in current buffer."
  (setq-local prettify-symbols-alist fira-code-mode--old-prettify-alist)
  (prettify-symbols-mode -1))

(define-minor-mode fira-code-mode
  "Fira Code ligatures minor mode"
  :lighter " Fira Code"
  (setq-local prettify-symbols-unprettify-at-point 'right-edge)
  (if fira-code-mode
      (fira-code-mode--enable)
    (fira-code-mode--disable)))

(defun fira-code-mode--setup ()
  "Setup Fira Code Symbols."
  (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol"))

(provide 'fira-code-mode)
;;; fira-code-mode.el ends here
