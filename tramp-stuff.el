;; Tramp for remote editing
(require 'tramp)
(setq tramp-default-method "sshx")
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq tramp-backup-directory-alist backup-directory-alist)
