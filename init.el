;;; package --- Summary
;;; Commentary:
;;; Code:

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
  :bind
  ("C-a" . mwim-beginning-of-code-or-line-or-comment)
  ("C-e" . mwim-end-of-code-or-line))

;; some symbol search?
(use-package counsel
  :diminish)

;; some minibuffer complete?
(use-package ivy
  :diminish
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

(use-package ace-window
  :bind
  ("C-w" . 'ace-window))

;; undo-tree is a graphic tree to undo history
(defun remove-undo-tree-mode-bindings ()
  "Remove C-/ in the undo-tree-mode."
  (define-key undo-tree-map (kbd "C-/") nil))
(use-package undo-tree
  :diminish
  :init (global-undo-tree-mode)
  ;; :hook
  ;; (undo-tree-mode . remove-undo-tree-mode-bindings)
  :config
  (define-key undo-tree-map (kbd "C-/") nil)
  :bind
  ("<f10>" . 'undo-tree-visualize)
  :custom
  (undo-tree-auto-save-history nil))

;; (define-key undo-tree-map (kbd "C-/") nil)

;; better scroll
(use-package good-scroll
  :diminish
  :if window-system
  :init (good-scroll-mode))

;; add comments for the minibuffer
(use-package marginalia
  :ensure t
  :init (marginalia-mode)
  :bind (:map minibuffer-local-map
	      ("M-A" . marginalia-amxacycle)))

;; amazing move
(use-package avy
  :ensure t
  :bind
  (("M-;" . avy-goto-char-timer)))

;; highlight the same symbol
;; (use-package highlight-symbol
;;   :ensure t
;;   :init (highlight-symbol-mode)
;;   :bind ("<f3>" . highlight-symbol))

(use-package symbol-overlay
  :diminish
  :init
  (symbol-overlay-mode)
  :bind
  ("<f3>" . symbol-overlay-put)
  ("<f2>" . symbol-overlay-rename))

;; colorful pairs
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; fonts align
;; (use-package cnfonts
;;   :ensure t
;;   :init (cnfonts-mode)
;;   :hook
;;   (org-mode . cnfonts-mode)
;;   )

;; dashboard
(use-package dashboard
  :config
  (setq dashboard-banner-logo-title "te echo de menos")
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-startup-banner "~/.emacs.d/static/banner.png") ; banner pircture
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (setq dashboard-items '((recents  . 5)
			  (projects . 5)))
  (setq dashboard-set-footer nil)
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
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
  :diminish
  :if window-system
  :hook
  (company-mode . company-box-mode))

(use-package yasnippet
  :diminish
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
  :diminish
  :after yasnippet)

(use-package which-key
  :diminish
  :config
  (which-key-mode)
  (setq which-key-idle-delay 500)
  (setq which-key-idle-secondary-delay 0.05))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l"
	lsp-file-watch-threshold 500)
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
  :delight '(:eval (concat " [" (projectile-project-name) "]"))
  :bind
  (("C-c p" . projectile-command-map) ; it's a good way to use which-key
   ("M-p" . projectile-find-file)
   ("M-P" . projectile-find-dir)
   ("M-f" . projectile-grep)
   ("M-h" . projectile-replace)))

;; enhanced project grep
(use-package counsel-projectile
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

(defun smart-comment (&optional arg)
  "Replacement for the comment-dwim command. If no region is selected and current line is not blank and we are not at the end of the line, then comment current line. Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))
;; (define-key  (kbd "your-key") 'your-command)
;; (define-key key-translation-map (kbd "C-/") (kbd "M-/"))
(global-set-key (kbd "C-/") 'smart-comment)


;; smart mode line
;; (use-package smart-mode-line
;;   :ensure t
;;   :init
;;   (sml/setup))

;; powerline is good, more visually beautiful than smart mode line and more faster than doom-modeline
(use-package powerline
  :init
  (powerline-center-theme))

;; (use-package doom-modeline
;;   :ensure t
;;   :init (doom-modeline-mode 1))

(use-package magit)

;; (use-package vterm
;;   :bind
;;   ("C-c C-v" . 'vterm-yank))
;; ;; (add-hook 'vterm-mode-hook (lambda () (read-only-mode -1)))
;; (add-hook 'vterm-mode-hook (lambda ()
;; 			     (vterm-copy-mode t)))

;; (use-package vterm-toggle
;;   :bind
;;   ("<f4>" . 'vterm-toggle))
;; (define-key vterm-mode-map (kbd "<f4>")  'vterm-toggle)



(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-height 36)
  (set-face-attribute 'tab-line nil ;; background behind tabs
		      :background "#073642")
  (setq centaur-tabs-gray-out-icons 'buffer)
  (setq centaur-tabs-set-bar 'under)
  (setq x-underline-at-descent-line t)
  (setq centaur-tabs-label-fixed-length 16)
  (setq centaur-tabs-show-navigation-buttons t)
  (setq centaur-tabs-set-modified-marker t)
  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.
Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \"Emacs\".
Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ((string-equal "*" (substring (buffer-name) 0 1))
       "Emacs")
      ((derived-mode-p 'dired-mode)
       "Dired")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))
  :hook
  (dired-mode . centaur-tabs-local-mode)
  (dashboard-mode . centaur-tabs-local-mode)
  (eshell-mode . centaur-tabs-local-mode)
  (vterm-mode . centaur-tabs-local-mode))


;; smart switch buffer
(defun normal-buffer? ()
  (or (not buffer-read-only)
      (buffer-file-name)))

(defun smart-switch-to-next-buffer ()
  (interactive)
  (unless (minibufferp)
    (let ((p t) (bn (buffer-name)))
      (switch-to-next-buffer)
      (while (and p (not (normal-buffer?)))
	(switch-to-next-buffer)
	(when (string= bn (buffer-name)) (setq p nil))))))

(defun smart-switch-to-prev-buffer ()
  (interactive)
  (unless (minibufferp)
    (let ((p t) (bn (buffer-name)))
      (switch-to-prev-buffer)
      (while (and p (not (normal-buffer?)))
	(switch-to-prev-buffer)
	(when (string= bn (buffer-name)) (setq p nil))))))

(global-set-key (kbd "C-h") 'smart-switch-to-prev-buffer)
(global-set-key (kbd "C-l") 'smart-switch-to-next-buffer)


;; (use-package solarized-theme
;;   :init ;; settings need to be set before load-theme is invoked for Solarized.
;;   (setq x-underline-at-descent-line t)
;;   (setq solarized-use-variable-pitch nil)
;;   (setq solarized-height-minus-1 1.0)
;;   (setq solarized-height-plus-1 1.0)
;;   (setq solarized-height-plus-2 1.0)
;;   (setq solarized-height-plus-3 1.0)
;;   (setq solarized-height-plus-4 1.0)
;;   :config
;;   (load-theme 'solarized-dark t))

(use-package timu-rouge-theme
  :init
  (customize-set-variable 'timu-rouge-mode-line-border t)
  :config
  (load-theme 'timu-rouge t))


;; (global-tab-line-mode t)
;; (defvar my/tab-height 22)
;; (defvar my/tab-left (powerline-wave-right 'tab-line nil my/tab-height))
;; (defvar my/tab-right (powerline-wave-left nil 'tab-line my/tab-height))

;; (defun my/tab-line-tab-name-buffer (buffer &optional _buffers)
;;   (powerline-render (list my/tab-left
;;                           (format "%s" (buffer-name buffer))
;;                           my/tab-right)))
;; (setq tab-line-tab-name-function #'my/tab-line-tab-name-buffer)

;; tab color settings
;; (set-face-attribute 'tab-line nil ;; background behind tabs
;;       :background "#073642")

(use-package git-gutter
  :diminish
  :init
  (global-git-gutter-mode)
  :config
  (custom-set-variables
   '(git-gutter:window-width 2)
   '(git-gutter:modified-sign "%")
   '(git-gutter:added-sign "+")
   '(git-gutter:deleted-sign "-")
   '(git-gutter:lighter " G"))
  :bind
  ("M-u" . git-gutter:revert-hunk)
  ("M-g p" . git-gutter:previous-hunk)
  ("M-g n" . git-gutter:next-hunk))

(use-package no-littering)

;; remember the M-x history
(use-package amx
  :after (no-littering)
  :init (amx-mode)
  :custom
  (amx-save-file "~/.emacs.d/amx-items"))

(use-package diminish
  :config
  (diminish 'visual-line-mode)
  (diminish 'hs-minor-mode))

(require 'posframe)
(use-package rime
  :custom
  (default-input-method "rime")
  :config
  (setq mode-line-mule-info '((:eval (rime-lighter))))
  (setq rime-show-candidate 'posframe)
  (setq rime-disable-predicates
	'(rime-predicate-after-alphabet-char-p
          rime-predicate-prog-in-code-p
	  rime-predicate-space-after-cc-p
	  rime-predicate-punctuation-line-begin-p
	  rime-predicate-after-ascii-char-p)))

(use-package flycheck
  :config
  (setq truncate-lines nil) ; 如果单行信息很长会自动换行
  :hook
  (prog-mode . flycheck-mode))

;; (load "all-the-icons-dired.el")
(add-hook 'dired-mode-hook 'treemacs-icons-dired-mode)

(use-package dired-sidebar
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
  (setq dired-sidebar-use-term-integration t))

(centaur-tabs-change-fonts "LXGW WenKai Mono" 130)

(use-package ibuffer-sidebar
  :load-path "~/.emacs.d/lisp/ibuffer-sidebar"
  :ensure nil
  :commands (ibuffer-sidebar-toggle-sidebar)
  :config
  (setq dired-sidebar-theme "vscode")
  (setq ibuffer-sidebar-use-custom-font t)
  (setq ibuffer-sidebar-face `(:family "LXGW WenKai Mono" :height 130)))

(defun +sidebar-toggle ()
  "Toggle both `dired-sidebar' and `ibuffer-sidebar'."
  (interactive)
  (dired-sidebar-toggle-sidebar)
  (ibuffer-sidebar-toggle-sidebar))

(global-set-key (kbd "<f6>") '+sidebar-toggle)

(use-package eshell-toggle
  :ensure nil
  :load-path ".emacs.d/lisp/eshell-toggle.el"
  :bind
  ("<f4>" . eshell-toggle))

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode)
  ;; If you have use-package-hook-name-suffix set to nil, uncomment and use the
  ;; line below instead:
  :hook (eshell-mode-hook . esh-autosuggest-mode))
(setq ivy-do-completion-in-region t) ; this is the default

(defun setup-eshell-ivy-completion ()
  (define-key eshell-mode-map [remap eshell-pcomplete] 'completion-at-point)
  ;; only if you want to use the minibuffer for completions instead of the
  ;; in-buffer interface
  (setq-local ivy-display-functions-alist
              (remq (assoc 'ivy-completion-in-region ivy-display-functions-alist)
                    ivy-display-functions-alist)))

(add-hook 'eshell-mode-hook #'setup-eshell-ivy-completion)

(provide 'init)
;;; init.el ends here

