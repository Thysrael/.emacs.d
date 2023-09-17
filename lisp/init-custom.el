(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(cua-mode t)
 '(custom-safe-themes
   '("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(display-time-mode t)
 '(git-gutter:added-sign "+")
 '(git-gutter:deleted-sign "-")
 '(git-gutter:lighter " G")
 '(git-gutter:modified-sign "%")
 '(git-gutter:window-width 2)
 '(global-display-line-numbers-mode t)
 '(inhibit-startup-screen t)
 '(mini-frame-show-parameters '((top . 0.35) (width . 0.7) (left . 0.5)))
 '(package-selected-packages
   '(chinese-pyim treemacs-icons-dired timu-rouge-theme esh-autosuggest amx no-littering all-the-icons-dired dired-sidebar rime zweilight-theme diminish git-gutter symbol-overlay centaur-tabs magit powerline display-time-mode doom-modeline-icon org-cua-dwim treemacs-all-the-icons atom-one-dark-theme monokai-theme dracula-theme all-the-icons zygospore zzz-to-char lsp-treemacs treemacs-projectile treemacs counsel-projectile projectile lsp-ui yasnippet-snippets yasnippet lsp-ivy lsp-mode company-box company dashboard rainbow-delimiters marginalia which-key smart-mode-line good-scroll undo-tree ace-window counsel mwim flycheck))
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Sarasa Term SC Nerd" :foundry "????" :slant normal :weight regular :height 130 :width normal))))
 '(dired-broken-symlink ((t nil)))
 '(dired-directory ((t (:foreground "#db6e8f" :underline nil))))
 '(dired-flagged ((t nil)))
 '(dired-header ((t nil)))
 '(dired-ignored ((t (:inherit shadow))))
 '(dired-mark ((t (:foreground "#b58900" :weight bold))))
 '(dired-marked ((t (:foreground "#d33682" :weight bold))))
 '(dired-perm-write ((t (:foreground "#839496" :underline t))))
 '(dired-set-id ((t (:inherit font-lock-warning-face))))
 '(dired-sidebar-face ((t (:family "LXGW WenKai Mono"))) t)
 '(dired-special ((t (:inherit font-lock-variable-name-face))))
 '(dired-symlink ((t (:foreground "#2aa198" :slant italic :weight normal))))
 '(dired-warning ((t (:foreground "#cb4b16" :underline t))))
 '(git-gutter:added ((t (:foreground "light sea green"))))
 '(git-gutter:modified ((t (:foreground "medium purple")))))

;; (defun centaur-setup-fonts ()
;;   "Setup fonts."
;;   (when (display-graphic-p)
;;     ;; Set default font
;;     (set-face-attribute 'default nil :family "Sarasa Term SC Nerd" :height 130)

    
;;     ;; (cl-loop for font in '("Cascadia Code" "Fira Code" "Jetbrains Mono"
;;     ;;                        "SF Mono" "Hack" "Source Code Pro" "Menlo"
;;     ;;                        "Monaco" "DejaVu Sans Mono" "Consolas")
;;     ;;          when (font-installed-p font)
;;     ;;          return (set-face-attribute 'default nil
;;     ;;                                     :family font
;;     ;;                                     :height (cond (sys/macp 130)
;;     ;;                                                   (sys/win32p 110)
;;     ;;                                                   (t 100))))
;;     ;; Set mode-line font
;;     ;; (cl-loop for font in '("Menlo" "SF Pro Display" "Helvetica")
;;     ;;          when (font-installed-p font)
;;     ;;          return (progn
;;     ;;                   (set-face-attribute 'mode-line nil :family font :height 120)
;;     ;;                   (when (facep 'mode-line-active)
;;     ;;                     (set-face-attribute 'mode-line-active nil :family font :height 120))
;;     ;;                   (set-face-attribute 'mode-line-inactive nil :family font :height 120)))

;;     ;; Specify font for all unicode characters
;;     ;; (cl-loop for font in '("Segoe UI Symbol" "Symbola" "Symbol")
;;     ;;          when (font-installed-p font)
;;     ;;          return (set-fontset-font t 'unicode font nil 'prepend))
;;     ;; (set-fontset-font t 'unicode "Noto Sans Symbols" nil 'prepend)

;;     ;; Emoji
;;     ;; (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji")
;;     ;;          when (font-installed-p font)
;;     ;;          return (if (>= emacs-major-version 28)
;;     ;;                     (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend)
;;     ;;                   (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend)))
;;     ;; (set-fontset-font t 'emoji (font-spec :family "Noto Color Emoji") nil 'prepend)
    
;;     ;; Specify font for Chinese characters
;;     ;; (cl-loop for font in '("Sarasa Term SC Nerd")
;;     ;;          return (progn
;;     ;;                   (setq face-font-rescale-alist `((,font . 1.3)))
;;     ;;                   (set-fontset-font t '(#x4e00 . #x9fff) (font-spec :family font))))
;;     ))

;; (centaur-setup-fonts)
;; (add-hook 'window-setup-hook #'centaur-setup-fonts)
;; (add-hook 'server-after-make-frame-hook #'centaur-setup-fonts)
;; (add-hook 'dired-mode-hook #'centaur-setup-fonts)

(provide 'init-custom)
;;; init-custom.el ends here
