(show-paren-mode 1)
(setq inhibit-splash-screen t)
(global-auto-revert-mode 1) ;; Auto refresh buffers
(setq global-auto-revert-non-file-buffers t) ;; Also auto refresh dired, but be quiet about itp
(setq auto-revert-verbose nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(delete-selection-mode t)
(pending-delete-mode t)
(transient-mark-mode t)
(column-number-mode t)
(setq x-select-enable-clipboard t)
(setq-default indent-tabs-mode nil)
(setq initial-scratch-message nil)
(setq auto-window-vscroll nil)
(global-hl-line-mode -1)

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

;; Prevent warning messages (ansi-term issue)
(setq warning-suppress-types nil)
(add-to-list 'warning-suppress-types '(undo discard-info))

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))
(global-set-key [remap goto-line] 'goto-line-with-feedback)

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))
(add-hook 'before-save-hook 'cleanup-buffer-safe)  ;; Various superfluous white-space. Just say no.

;; Packages
;; auto-complete
;; projectile && projectile-helm
;; smex
;; multiple-cursors
;; flx-ido
;; expand-region
;; ace-jump mode
;; fly-check
;; magit
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("marmalade" . "https://marmalade-repo.org/packages/")
                           ("melpa" . "http://melpa.milkbox.net/packages/"))))

;; Multiple cursor key bindings
(global-set-key (kbd "M-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-n") 'mc/mark-next-like-this)
(global-set-key (kbd "") 'mc/mark-all-like-this)

;; Smex key-bindings
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; flx-ido
(ido-mode 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
;;(setq ido-use-faces nil)

;; auto-complete
(ac-config-default)

;; expand-region
(global-set-key (kbd "C-o") 'er/expand-region)

;; ace-jump-mode
(define-key global-map (kbd "C-j") 'ace-jump-mode)

;; projectile
(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-require-project-root nil)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; flycheck
;;(global-flycheck-mode t)
(setq flycheck-check-syntax-automatically '(save mode-enabled))
(setq flycheck-flake8-maximum-line-length 100)
