;; export TERM=xterm-256color

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

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("melpa" . "http://melpa.milkbox.net/packages/"))))

(setq package-list
      '(ace-jump-mode
        ag
        auto-complete
        autopair
        csv-mode
        expand-region
        flatland-black-theme
        flx-ido
        flycheck
        helm-projectile
        helm-ag
        ido-ubiquitous
        ido-vertical-mode
        imenu-anywhere
        multiple-cursors
        neotree
        perspective
        persp-projectile
        projectile
        pyenv-mode
        smart-mode-line
        smex
        symon
        web-mode
        whole-line-or-region
        yaml-mode))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; ace-jump-mode
(define-key global-map (kbd "M-j") 'ace-jump-mode)

;; ag
(setq ag-reuse-buffers 't)

;; auto-complete
(ac-config-default)

;; autopair
(autopair-global-mode t)

;; expand-region
(global-set-key (kbd "C-o") 'er/expand-region)

;; flx-ido
(flx-ido-mode 1)

;; flycheck
(setq flycheck-check-syntax-automatically '(mode-enabled save))
;; (add-hook 'python-mode-hook (lambda () (flycheck-mode 1)))
(setq flycheck-flake8-maximum-line-length 100)

;; helm-projectile
;; (helm-projectile-on)
;; (setq projectile-completion-system 'helm)
;; (add-to-list 'grep-find-ignored-files "*.log")
;; (add-to-list 'grep-find-ignored-directories "logs")

;; ido-ubiquitous
(ido-everywhere 1)

;; ido-vertical-mode
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

;; imenu-anywhere
(global-set-key (kbd "M-i") 'imenu-anywhere)

;; multiple-cursors
(global-set-key (kbd "M-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-n") 'mc/mark-next-like-this)

;; neotree
(global-set-key [f8] 'neotree-toggle)
;; (setq projectile-switch-project-action 'neotree-projectile-action)

;; perspective
;; (persp-mode)

;; projectile
(projectile-global-mode)
;; (setq projectile-require-project-root nil)
(setq projectile-mode-line '(:eval (format " Proj[%s]" (projectile-project-name))))

;; pyenv-mode
(unless (display-graphic-p)
  (pyenv-mode))

;; smart-mode-line
(sml/setup)
(sml/apply-theme 'respectful)

;; smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; symon
(symon-mode)

;; web-mode
(add-to-list 'auto-mode-alist '("\\.jinja$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$". web-mode))
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-attr-indent-offset 2)
(setq web-mode-enable-auto-closing t)
(setq web-mode-enable-auto-indentation t)

;; whole-line-or-region
(whole-line-or-region-mode 1)

;; yaml-mode
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

;;;;; Other Settings
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-virtual-buffers t)
(setq ido-max-directory-size 300000)
(show-paren-mode 1)
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)
(global-auto-revert-mode 1) ;; Auto refresh buffers
(setq auto-revert-verbose nil)
(setq global-auto-revert-non-file-buffers t) ;; Also auto refresh dired, but be quiet about it
(delete-selection-mode t)
(transient-mark-mode t)
(column-number-mode t)
(setq-default indent-tabs-mode nil)
(setq dired-listing-switches "-hal")
(toggle-truncate-lines 1)
(setq split-width-threshold nil)
(setq split-height-threshold nil)

;; org key-bindings
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(global-set-key (kbd "C-M-j") 'org-insert-todo-heading)
(setq org-log-done t)

;; Redefine keys
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "<RET>") 'newline-and-indent)

(if (display-graphic-p)
    (progn
      (fringe-mode -1)
      (tool-bar-mode -1)
      (scroll-bar-mode -1))
  (progn (menu-bar-mode -1)
         ;; (load-theme 'flatland-black)
         ))

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(setq backup-by-copying t      ; don't clobber symlinks
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
(add-hook 'before-save-hook 'cleanup-buffer-safe)

(defun my-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))
(add-hook 'find-file-hook 'my-find-file-check-make-large-file-read-only-hook)

;; Requirement: Install xsel program
;; https://hugoheden.wordpress.com/2009/03/08/copypaste-with-emacs-in-terminal/
(setq x-select-enable-clipboard t)
(unless (display-graphic-p)
  (when (getenv "DISPLAY")
    (defun xsel-cut-function (text &optional push)
      (with-temp-buffer
        (insert text)
        (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))
    (defun xsel-paste-function()
      (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
        (unless (string= (car kill-ring) xsel-output)
          xsel-output )))
    (setq interprogram-cut-function 'xsel-cut-function)
    (setq interprogram-paste-function 'xsel-paste-function)))

;; Mac copy and paste
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
;; (global-set-key [?\C-x ?\M-w] 'pt-pbcopy)
