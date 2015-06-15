;; TERM=xterm-mono emacs -nw

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;; PACKAGES
;; pyenv-mode
;; hlinum
;; ace-jump-mode
;; auto-complete
;; multiple-cursors
;; expand-region
;; autopair
;; flx-ido
;; ido-ubiquitous
;; smex
;; smart-mode-line
;; powerline
;; monochrome
;; ido-vertical-mode
;; helm
;; yaml-mode
;; haml-mode
;; jinja2-mode
;; web-mode
;; jedi
;; flycheck
;; projectile

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("melpa" . "http://melpa.milkbox.net/packages/"))))

;; pyenv-mode
;; (pyenv-mode)
;; (pyenv-mode-set 'demonbrandt)

;; hlinum
;; (hlinum-activate)

;; ace-jump-mode
(define-key global-map (kbd "M-j") 'ace-jump-mode)

;; auto-complete
(ac-config-default)

;; multiple-cursors
(global-set-key (kbd "M-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-n") 'mc/mark-next-like-this)

;; expand-region
(global-set-key (kbd "C-o") 'er/expand-region)

;; autopair
(autopair-global-mode t)

;; flx-ido
(flx-ido-mode 1)

;; ido-ubiquitous
(ido-everywhere 1)

;; smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command) ;; This is your old M-x.

;; smart-mode-line
(sml/setup)
(sml/apply-theme 'dark)

;; ido-vertical-mode
(ido-vertical-mode 1)

;; helm
;; Activate once flx-ido is integrated to work with helm

;; yaml-mode
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

;; jinja2-mode
;; (add-to-list 'auto-mode-alist '("\\.jinja$" . jinja2-mode))

;; web-mode
(add-to-list 'auto-mode-alist '("\\.jinja$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$". web-mode))
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-attr-indent-offset 2)
(setq web-mode-enable-auto-closing t)
(setq web-mode-enable-auto-indentation t)
;; (setq web-mode-enable-auto-pairing t)

;; flycheck
(setq flycheck-check-syntax-automatically '(mode-enabled save))
(add-hook 'python-mode-hook (lambda () (flycheck-mode 1)))

;; projectile
(projectile-global-mode)
(setq projectile-require-project-root nil)
(setq projectile-mode-line '(:eval (format " Proj[%s]" (projectile-project-name))))

;;;;; Other Settings
(ido-mode 1)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-use-virtual-buffers t)
(setq ido-max-directory-size 300000)
(show-paren-mode 1)
(setq inhibit-splash-screen t)
;; (global-auto-revert-mode 1) ;; Auto refresh buffers
;; (setq auto-revert-verbose nil)
(setq global-auto-revert-non-file-buffers t) ;; Also auto refresh dired, but be quiet about itp
(delete-selection-mode t)
(pending-delete-mode t)
(transient-mark-mode t)
(column-number-mode t)
(setq x-select-enable-clipboard t)
(setq-default indent-tabs-mode nil)
(setq initial-scratch-message nil)
(setq auto-window=-vscroll nil)
;; (global-linum-mode 1)
(setq linum-format "%d ")
(global-hl-line-mode 1)
;; (set-face-background 'hl-line "#333333")
(set-face-foreground 'highlight nil)
(setq dired-listing-switches "-hal")

(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)
      (global-linum-mode t)
      (load-theme 'wombat))
  (progn (menu-bar-mode -1)))


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
;; Or turn off backup-system
;;(setq make-backup-files nil)

(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))
(add-hook 'before-save-hook 'cleanup-buffer-safe)  ;; Various superfluous white-space. Just say no.

(defun sudo ()
  "Use TRAMP to `sudo' the current buffer"
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
             buffer-file-name))))

(global-set-key (kbd "M-o") 'other-window)
