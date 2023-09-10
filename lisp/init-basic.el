;;; basic key bindings
;; use C-j and C-k to subscribe the C-x and the M-x
;; (global-set-key (kbd "C-j") (lookup-key global-map (kbd "C-x")))
(define-key key-translation-map (kbd "C-j") (kbd "C-x"))
(global-set-key (kbd "C-k") (lookup-key global-map (kbd "M-x")))

;; use C-h, C-l to backward-char, forward-char
;; (global-set-key (kbd "C-h") 'forward-char)
;; (global-set-key (kbd "C-l") 'forward-char)

;; use C-s to save buffer
(global-set-key (kbd "C-s") 'save-buffer)

;; use <f9> to clear multi
(global-set-key (kbd "<f9>") 'delete-other-windows)

;; use C-x C-j to smart copy
(defun smart-copy ()
  (interactive)
  (if mark-active
      (kill-ring-save (region-beginning)
      (region-end))
    (progn
     (kill-ring-save (line-beginning-position)
     (line-end-position))
     (message "copied line"))))
(global-set-key (kbd "C-x C-j") 'smart-copy)

;; use C-x C-k to smart kill
(defun smart-kill ()
  (interactive)
  (if mark-active
      (kill-region (region-beginning)
   (region-end))
    (progn
     (kill-region (line-beginning-position)
  (line-end-position))
     (message "killed line"))))
(global-set-key (kbd "C-x C-k") 'smart-kill)

;; use C-x C-p to paste
(global-set-key (kbd "C-x C-p") 'cua-paste)

;; use <f2> to open init file quikly
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f2>") 'open-init-file)

;; use <C-9>, <C-0> to move fast
(defun next-fast-lines()
  "Move cursor to next 10 lines."
  (interactive)
  (next-line 5))
(defun previous-fast-lines()
  "Move cursor to previous 10 lines."
  (interactive)
  (previous-line 5))
(global-set-key (kbd "C-9") 'next-fast-lines)
(global-set-key (kbd "C-0") 'previous-fast-lines)

(global-set-key (kbd "<f9>") 'delete-other-windows)
(global-set-key (kbd "<f8>") 'split-window-right)

;;; display settings
;; change the cursor shape
(setq-default cursor-type 'bar)
;; show column number at the mode line
(column-number-mode t)

;; no tool bar 
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

;;; other settings
;; auto complete the pair
(electric-pair-mode t)
;; when in the program mode, show the pair
(add-hook 'prog-mode-hook #'show-paren-mode)
;; when some other apps changed the file, update the buffer timely
(global-auto-revert-mode t)
;; when in the program mode, can fold the code
(add-hook 'prog-mode-hook #'hs-minor-mode)

;; no backup, no auto-save
(setq make-backup-files nil)
(setq auto-save-list-file-prefix nil)
(setq auto-save-default nil)

;; font render faster
(setq inhibit-compacting-font-caches t)

;; fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(provide 'init-basic)
