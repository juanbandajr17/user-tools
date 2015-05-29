;;;;; PACKAGES
;; auto-complete
;; projectile
;; smex
;; multiple-cursors
;; flx-ido
;; expand-region
;; smart-mode-line

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("melpa" . "http://melpa.milkbox.net/packages/"))))

;; smart-mode-line
(sml/setup)
(sml/apply-theme 'dark)

;; Multiple cursor key bindings
(global-set-key (kbd "M-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-n") 'mc/mark-next-like-this)

;; Smex key-bindings
(global-set-key (kbd "M-x") 'smex)

;; flx-ido
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)

;; auto-complete
(ac-config-default)

;; expand-region
(global-set-key (kbd "C-o") 'er/expand-region)

;; projectile
(projectile-global-mode)
(setq projectile-require-project-root nil)
(setq projectile-mode-line '(:eval (format " Proj[%s]" (projectile-project-name))))

;;;;; Other Settings
(ido-mode 1)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(show-paren-mode 1)
(setq inhibit-splash-screen t)
(global-auto-revert-mode 1) ;; Auto refresh buffers
(setq global-auto-revert-non-file-buffers t) ;; Also auto refresh dired, but be quiet about itp
(setq auto-revert-verbose nil)
(delete-selection-mode t)
(pending-delete-mode t)
(transient-mark-mode t)
(column-number-mode t)
(setq x-select-enable-clipboard t)
(setq-default indent-tabs-mode nil)
(setq initial-scratch-message nil)
(setq auto-window=-vscroll nil)
(global-hl-line-mode -1)

(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)
      (global-linum-mode t)
      (load-theme 'wombat))
  (menu-bar-mode -1))

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups
;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))
(add-hook 'before-save-hook 'cleanup-buffer-safe)  ;; Various superfluous white-space. Just say no.

(global-set-key (kbd "M-o") 'other-window)