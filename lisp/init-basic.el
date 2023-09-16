;;; basic key bindings
;; use C-j and C-k to subscribe the C-x and the M-x
;; (global-set-key (kbd "C-j") (lookup-key global-map (kbd "C-x")))
(define-key key-translation-map (kbd "C-j") (kbd "C-x"))
;; (global-set-key (kbd "C-j") ctl-x-map)
;; (add-hook 'org-mode-hook #'(lambda () (local-unset-key (kbd "C-j"))))
;; (define-key org-mode-map (kbd "C-j") ctl-x-map)
(global-set-key (kbd "C-k") 'execute-extended-command)

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
(global-set-key (kbd "C-x C-x") 'smart-copy)

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

;; use <f2> to open init file quikly
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f2>") 'open-init-file)

;; use <f8> to split the window
(global-set-key (kbd "<f8>") 'split-window-right)

;; use <f9> to unique the window 
(global-set-key (kbd "<f9>") 'delete-other-windows)

;; use C-h to 


;;; display settings
;; change the cursor shape
(setq-default cursor-type 'bar)
;; show column number at the mode line
(column-number-mode t)

;; no tool bar 
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

;; highlight cur line 
(global-hl-line-mode t)

;; some thing i don't know 
(put 'dired-find-alternate-file 'disabled nil)

;; display time
(setq display-time-default-load-average nil) ; remove the load average 
(setq display-time-day-and-date t)
(setq display-time-format "%m.%d %H:%M %A")
(display-time-mode t)

;; line height
(add-hook 'org-mode-hook
	  (lambda()
	    (setq-local line-spacing 0.5)))

;;; other settings
;; auto complete the pair
(electric-pair-mode t)
;; when in the program mode, show the pair
(add-hook 'prog-mode-hook #'show-paren-mode)
;; when some other apps changed the file, update the buffer timely
(global-auto-revert-mode t)
;; when in the program mode, can fold the code
(add-hook 'prog-mode-hook #'hs-minor-mode)
;; trunc line when one line is too long
(global-visual-line-mode t) 
;; no backup, no auto-save
(setq backup-by-copying nil) 
(setq make-backup-files nil)
(setq auto-save-list-file-prefix nil)
(setq auto-save-default nil)
;; remove comment at scratch
(setq initial-scratch-message "")

;; font render faster
(setq inhibit-compacting-font-caches t)

;; fullscreen
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
(set-frame-parameter nil 'fullscreen 'fullboth)

;; use 'y or n' to replace 'yes or no'
(defun yes-or-no-p->-y-or-n-p (orig-fun &rest r)
  (cl-letf (((symbol-function 'yes-or-no-p) #'y-or-n-p))
    (apply orig-fun r)))
(advice-add 'kill-buffer :around #'yes-or-no-p->-y-or-n-p)

;; something need to place at the util
(defun kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer 
          (delq (current-buffer) 
                (cl-remove-if-not 'buffer-file-name (buffer-list)))))
;; swiper
;; delete M-x ^
(with-eval-after-load 'counsel
  (setq ivy-initial-inputs-alist nil))

(provide 'init-basic)
