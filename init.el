;; speed up startup: adjust garbage collection thresholds during startup, and thereafter
(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'emacs-startup-hook
	  (lambda () (setq gc-cons-threshold (* 20 1024 1024))))

;; use proxy
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq url-proxy-services '(("no_proxy" . "^\\(192\\.168\\..*\\)")
                           ("http" . "127.0.0.1:20171")
			   ("https" . "127.0.0.1:20171")))

;; add the ~/.emacs.d/lisp to the load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; avoid system custom to add in the init.el
(setq custom-file "~/.emacs.d/lisp/init-custom.el")
(require 'init-custom)
(require 'init-basic)
(require 'init-melpa)

(eval-when-compile
  (require 'use-package))

;; mvwin can enhanced <C-a> and the <C-e>
(use-package mwim
  :ensure t
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

;; some symbol search?
(use-package counsel
  :ensure t)

;; some minibuffer complete?
(use-package ivy
  :ensure 
  :init
  (ivy-mode 1)
  (counsel-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq search-default-mode #'char-fold-to-regexp)
  (setq ivy-count-format "(%d/%d) ")
  :bind
  ("C-f" . 'swiper)
  ("C-x b" . 'ivy-switch-buffer))

;; remember the M-x history
(use-package amx
  :ensure t
  :init (amx-mode))

(use-package ace-window
  :ensure t
  :bind
  ("C-w" . 'ace-window))

;; undo-tree is a graphic tree to undo history
(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history nil))

;; better scroll
(use-package good-scroll
  :ensure t
  :if window-system
  :init (good-scroll-mode)
  :bind
  ("<f10>" . 'undo-tree-visualize))

;; add comments for the minibuffer
(use-package marginalia
  :ensure t
  :init (marginalia-mode)
  :bind (:map minibuffer-local-map
	      ("M-A" . marginalia-cycle)))

;; amazing move
(use-package avy
  :ensure t
  :bind
  (("C-x SPC" . avy-goto-char-timer)))

;; highlight the same symbol
(use-package highlight-symbol
  :ensure t
  :init (highlight-symbol-mode)
  :bind ("<f3>" . highlight-symbol))

;; colorful pairs
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; fonts align
(use-package cnfonts
  :ensure t
  :init (cnfonts-mode)
  )
;; dashboard
(use-package dashboard
  :ensure t
  :config
  (setq dashboard-banner-logo-title "te echo de menos")
  ;; (setq dashboard-projects-backend 'projectile) 
  (setq dashboard-startup-banner "~/.emacs.d/static/banner.png") ; banner pircture
  (setq dashboard-items '((recents  . 5)))
  (setq dashboard-set-footer nil)
  (setq dashboard-center-content t)
  (dashboard-setup-startup-hook))

;; company
(use-package company
  :ensure t
  :init
  (global-company-mode)
  :config
  (setq company-minimum-prefix-length 1) 
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0.0)
  (setq company-selection-wrap-around t)
  (setq company-transformers '(company-sort-by-occurrence)))

;; (remove-hook 'org-mode-hook 'company-mode)
;; (remove-hook 'org-mode-hook 'company-box-mode)
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (company-mode)))

;; remove the company-mode and the company-box-mode from the org-mode 
(add-hook 'org-mode-hook (lambda ()
			   (progn
			     (company-mode -1)
			     (company-box-mode -1))))

;; solve the conflict in cua and org-mode 
;; (use-package org-cua-dwim)

(use-package company-box
  :ensure t
  :if window-system
  :hook
  (company-mode . company-box-mode))

(use-package yasnippet
  :ensure t
  :hook
  (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all)
  ;; add company-yasnippet to company-backends
  (defun company-mode/backend-with-yas (backend)
    (if (and (listp backend) (member 'company-yasnippet backend))
	backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  ;; unbind <TAB> completion
  (define-key yas-minor-mode-map [(tab)]        nil)
  (define-key yas-minor-mode-map (kbd "TAB")    nil)
  (define-key yas-minor-mode-map (kbd "<tab>")  nil)
  :bind
  (:map yas-minor-mode-map ("S-<tab>" . yas-expand)))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package which-key
    :config
    (which-key-mode))

(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  :hook 
  (lsp-mode . lsp-enable-which-key-integration) ; which-key integration
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-completion-provider :none) ;; 阻止 lsp 重新设置 company-backend 而覆盖我们 yasnippet 的设置
  (setq lsp-headerline-breadcrumb-enable t))

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

(use-package lsp-ui
  :ensure t
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (setq lsp-ui-doc-position 'top))

(use-package lsp-ivy
  :ensure t
  :after (lsp-mode))

(use-package projectile
  :ensure t
  :bind (("C-c p" . projectile-command-map))
  :config
  (setq projectile-mode-line "Projectile")
  (setq projectile-track-known-projects-automatically nil))

(use-package counsel-projectile
  :ensure t
  :after (projectile)
  :init (counsel-projectile-mode))

(use-package neotree
  :ensure t
  :config
  (setq neo-theme (if (display-graphic-p) 'nerd 'arrow))
  :bind
  ("<f7>" . neotree-toggle))


(use-package all-the-icons
  :ensure t)
	     
(load-theme 'solarized-dark t)

(defun smart-comment (&optional arg)
  "Replacement for the comment-dwim command. If no region is selected and current line is not blank and we are not at the end of the line, then comment current line. Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))
;; (define-key  (kbd "your-key") 'your-command)
(define-key key-translation-map (kbd "C-/") (kbd "M-/"))
(global-set-key (kbd "M-/") 'smart-comment)


;; smart mode line
;; (use-package smart-mode-line
;;   :ensure t
;;   :init
;;   (sml/setup))

;; powerline is good, more visually beautiful than smart mode line and more faster than doom-modeline
(use-package powerline
  :init
  (powerline-center-theme))

(use-package magit)
