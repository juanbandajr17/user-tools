;; Install emacs
;; http://lars.ingebrigtsen.no/2014/11/13/welcome-new-emacs-developers/

;; Moved the custom.el stuff into its own file called ~/.emacs.d/customize.el
(setq custom-file "~/.emacs.d/customize.el")
(when (file-exists-p custom-file)
  (load custom-file))

(column-number-mode t)
(delete-selection-mode t)
(electric-indent-mode t)
(electric-pair-mode t)
(fringe-mode nil)
(global-auto-revert-mode t) ;; Auto refresh buffers
(global-whitespace-mode t)
(ido-mode t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq dired-listing-switches "-hal")
(setq global-auto-revert-non-file-buffers t) ;; Also auto refresh dired, but be quiet about it
(setq ido-enable-flex-matching t)
(setq ido-max-directory-size 300000)
(setq ido-use-virtual-buffers t)
(setq imenu-max-item-length 100)
(setq inhibit-splash-screen t)
(setq initial-scratch-message ";; Scratch Paper")
(setq whitespace-line-column 100)
(setq whitespace-style '(face lines-tail trailing empty))
(setq-default indent-tabs-mode -1)
(setq backup-by-copying t)
(setq backup-directory-alist '(("." . "~/.emacs.d/.saves")))
(setq delete-old-versions nil) ;; Prompt for deletion
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t)
(setq vc-make-backup-files t)

(show-paren-mode t)
(size-indication-mode t)
(tool-bar-mode -1)
(transient-mark-mode t)
(set-face-attribute 'fringe nil
		    :foreground (face-foreground 'default)
		    :background (face-background 'default))
(add-hook 'before-save-hook 'whitespace-cleanup)

(defun my-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* (* 1024 1024) 10)) ;; 10 MB
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))
(add-hook 'find-file-hook 'my-find-file-check-make-large-file-read-only-hook)

;; ;; Requirement: Install xsel program
;; ;; https://hugoheden.wordpress.com/2009/03/08/copypaste-with-emacs-in-terminal/
;; (setq x-select-enable-clipboard t)
;; (unless (display-graphic-p)
;;   (when (getenv "DISPLAY")
;;     (defun xsel-cut-function (text &optional push)
;;       (with-temp-buffer
;; 	(insert text)
;; 	(call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))
;;     (defun xsel-paste-function()
;;       (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
;; 	(unless (string= (car kill-ring) xsel-output)
;; 	  xsel-output )))
;;     (setq interprogram-cut-function 'xsel-cut-function)
;;     (setq interprogram-paste-function 'xsel-paste-function))))


;; ;; Mac copy and paste
;; (defun pt-pbpaste ()
;;   "Paste data from pasteboard."
;;   (interactive)
;;   (shell-command-on-region
;;    (point)
;;    (if mark-active (mark) (point))
;;    "pbpaste" nil t))
;; (global-set-key [?\C-x ?\C-y] 'pt-pbpaste)
;; (defun pt-pbcopy ()
;;   "Copy region to pasteboard."
;;   (interactive)
;;   (print (mark))
;;   (when mark-active
;;     (shell-command-on-region
;;      (point) (mark) "pbcopy")
;;     (kill-buffer "*Shell Command Output*")))
;; (global-set-key [?\C-x ?\M-w] 'pt-pbcopy))

;; ;;  thirt party packages  and package settings
;; (require 'package)
;; (package-initialize)
;; (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;; 			 ("melpa" . "http://melpa.milkbox.net/packages/")))
;; (setq package-list
;;       '(ace-jump-mode
;;         ace-window
;;         ag
;;         auto-complete
;;         expand-region
;;         flx-ido
;;         ido-ubiquitous
;;         ido-vertical-mode
;;         imenu-anywhere
;;         multiple-cursors
;;         neotree
;;         projectile
;;         smex))

;; (unless package-archive-contents
;;   (package-refresh-contents))

;; (dolist (package package-list)
;;   (unless (package-installed-p package)
;;     (package-install package)))

;; ;; ace-jump-mode
;; (define-key global-map (kbd "M-j") 'ace-jump-mode)

;; ;; ace-window
;; (define-key global-map (kbd "M-o") 'ace-window)

;; ;; ag
;; (setq ag-reuse-buffers t)

;; ;; auto-complete
;; (ac-config-default)

;; ;; expand-region
;; (global-set-key (kbd "C-o") 'er/expand-region)

;; ;; flx-ido
;; (flx-ido-mode t)

;; ;; ido-ubiquitous
;; (ido-ubiquitous-mode t)
;; (ido-everywhere t)

;; ;; ido-vertical-mode
;; (ido-vertical-mode t)
;; (setq ido-vertical-define-keys 'C-n-and-C-p-only)

;; ;; imenu-anywhere
;; (global-set-key (kbd "M-i") 'imenu-anywhere)

;; ;; magit
;; (global-set-key (kbd "C-x g") 'magit-status)

;; ;; multiple-cursors
;; (global-set-key (kbd "M-p") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "M-n") 'mc/mark-next-like-this)
;; (global-set-key (kbd "M-m") 'mc/mark-all-like-this)

;; ;; projectile
;; (projectile-global-mode t)
;; (setq projectile-mode-line '(:eval (format " Proj[%s]" (projectile-project-name))))

;; ;; smex
;; (global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)
