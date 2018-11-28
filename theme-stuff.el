;; Bar cursor
(setq-default cursor-type '(bar . 2))

;; Set the theme
;; (el-get-bundle 'monokai-theme)
;; (require 'monokai-theme)
;; (load-theme 'monokai t)
(el-get-bundle hlissner/emacs-doom-themes)
(require 'doom-themes)

;; Global settings (defaults)
(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)

;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;; may have their own settings.
(load-theme 'doom-one t)

;; set monoaki style tabbar
(set-face-attribute
 'tabbar-default nil
 :background "black"
 :foreground "black"
 :box '(:line-width 1 :color "gray20" :style nil))
(set-face-attribute
 'tabbar-unselected nil
 :background "gray30"
 :foreground "white"
 :box '(:line-width 1 :color "gray30" :style nil))
(set-face-attribute
 'tabbar-selected nil
 :background "gray75"
 :foreground "#A41F99"
 :box '(:line-width 1 :color "gray75" :style nil))
(set-face-attribute
 'tabbar-highlight nil
 :background "white"
 :foreground "black"
 :underline nil
 :box '(:line-width 1 :color "white" :style nil))
(set-face-attribute
 'tabbar-button nil
 :box '(:line-width 1 :color "gray20" :style nil))
(set-face-attribute
 'tabbar-separator nil
 :background "black"
 :height 0.1)

;; ;; Change padding of the tabs
;; ;; we also need to set separator to avoid overlapping tabs by highlighted tabs
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(ansi-color-names-vector
;;    ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
;;  '(compilation-message-face (quote default))
;;  '(custom-safe-themes
;;    (quote
;;     ("e9df267a1c808451735f2958730a30892d9a2ad6879fb2ae0b939a29ebf31b63" default)))
;;  '(fci-rule-color "#3C3D37")
;;  '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
;;  '(highlight-tail-colors
;;    (quote
;;     (("#3C3D37" . 0)
;;      ("#679A01" . 20)
;;      ("#4BBEAE" . 30)
;;      ("#1DB4D0" . 50)
;;      ("#9A8F21" . 60)
;;      ("#A75B00" . 70)
;;      ("#F309DF" . 85)
;;      ("#3C3D37" . 100))))
;;  '(magit-diff-use-overlays nil)
;;  '(pos-tip-background-color "#A6E22E")
;;  '(pos-tip-foreground-color "#272822")
;;  '(tabbar-separator (quote (1.0)))
;;  '(vc-annotate-background nil)
;;  '(vc-annotate-color-map
;;    (quote
;;     ((20 . "#F92672")
;;      (40 . "#CF4F1F")
;;      (60 . "#C26C0F")
;;      (80 . "#E6DB74")
;;      (100 . "#AB8C00")
;;      (120 . "#A18F00")
;;      (140 . "#989200")
;;      (160 . "#8E9500")
;;      (180 . "#A6E22E")
;;      (200 . "#729A1E")
;;      (220 . "#609C3C")
;;      (240 . "#4E9D5B")
;;      (260 . "#3C9F79")
;;      (280 . "#A1EFE4")
;;      (300 . "#299BA6")
;;      (320 . "#2896B5")
;;      (340 . "#2790C3")
;;      (360 . "#66D9EF"))))
;;  '(vc-annotate-very-old-color nil)
;;  '(weechat-color-list
;;    (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))

;; adding spaces
(defun tabbar-buffer-tab-label (tab)
  "Return a label for TAB.
That is, a string used to represent it on the tab bar."
  (let ((label  (if tabbar--buffer-show-groups
                    (format " [%s] " (tabbar-tab-tabset tab))
                  (format " %s " (tabbar-tab-value tab)))))
    ;; Unless the tab bar auto scrolls to keep the selected tab
    ;; visible, shorten the tab label to keep as many tabs as possible
    ;; in the visible area of the tab bar.
    (if tabbar-auto-scroll-flag
        label
      (tabbar-shorten
       label (max 1 (/ (window-width)
                       (length (tabbar-view
                                (tabbar-current-tabset)))))))))

;; Syntax highlighting
(global-font-lock-mode 1)
(setq font-lock-maximum-decoration t)

;; Helm monokai
(set-face-attribute 'helm-selection nil
                    :background "grey10"
                    :foreground "blue")
(set-face-attribute 'helm-source-header nil
                    :background "gray30"
                    :foreground "green")
(set-face-attribute 'region nil
                    :background "grey10")
(set-face-background 'hl-line "grey10")
(set-face-attribute 'magit-diff-hunk-heading nil
                    :foreground "white")
(set-face-attribute 'magit-diff-hunk-heading-highlight nil
                    :foreground "white"
                    :background "grey10")
