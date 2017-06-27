;; Notes

;; To search for string/phrase for files in a directory:
;; M-x regrep

;; To search for file in a directory by name:
;; M-x find-name-dired

;; End Notes

;; Moved the custom.el stuff into its own file called ~/.emacs.d/customize.el
(setq custom-file "~/.emacs.d/customize.el")
(when (file-exists-p custom-file)
  (load custom-file))

;; Flags
(setq apropos-do-all t  ;; Apropos - searching emacs functions / symbols / etc.
      dired-listing-switches "-hal"
      global-auto-revert-non-file-buffers t  ;; Also auto refresh dired, but be quiet about it
      ido-enable-flex-matching t
      ido-everywhere t
      ido-max-directory-size 100000  ;; Able to show dir listing containing up to n files.
      ido-use-virtual-buffers t  ;; List recently opened files in buffer-list.
      imenu-max-item-length 100  ;; Useful when matching against long module/method names.
      inhibit-splash-screen t  ;; No message
      whitespace-line-column 100  ;; whitespace-mode highlight text after exceeding 100 chars
      whitespace-style '(face lines-tail trailing empty)  ;;  whitespace-mode highlights
      backup-by-copying t  ;; Copy all files, don't rename them.
      backup-directory-alist '(("." . "~/.emacs.d/backup/per-save"))
      delete-old-versions t  ;; Don't ask to delete excess backup versions.
      kept-new-versions 10  ;; Number of newest versions to keep.
      kept-old-versions 0  ;; Number of oldest versions to keep.
      version-control t  ;; Use version numbers for backups
      vc-make-backup-files t)
(setq-default indent-tabs-mode nil)

;; Modes
(column-number-mode t)  ;; Show col position in mode-line
(delete-selection-mode t)  ;; Delete entire highlighted/selected region
(electric-indent-mode t)  ;; Auto-indent on newline.
(electric-pair-mode -1)  ;; Auto-create ending pair.
(global-auto-revert-mode t)  ;; Auto refresh buffers
(ido-mode t)  ;; Display list of selection.
(show-paren-mode t)  ;; Highlight matching paren.
(size-indication-mode t)  ;; Display size of buffer in mode-line.
(transient-mark-mode t)  ;; Highlight selected region
(menu-bar-mode -1)
;; (fringe-mode nil)
;; (scroll-bar-mode -1)
;; (tool-bar-mode -1)
;; (global-whitespace-mode t)

;; Hooks
(add-hook 'before-save-hook 'whitespace-cleanup) ;; cleanup whitespace on save
(add-hook 'before-save-hook  'force-backup-of-buffer)
(add-hook 'find-file-hook 'my-find-file-check-make-large-file-read-only-hook)

;; Key-Bindings
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Org
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-link-file-path-type 'absolute
      org-completion-use-ido t
      org-agenda-log-mode-items '(scheduled deadline started closed)
      ;; org-agenda-files '("~/Dropbox/Documents/spokeo.org")
      org-log-done 'time
      ;; Only top level top level tasks
      org-refile-targets '((nil :maxlevel . 1) (org-agenda-files :maxlevel . 1))
      org-todo-keywords '((sequence "BACKLOG" "TODO" "ACTIVE" "|" "DONE"))
      org-todo-keyword-faces '(("BACKLOG" . "#404040") ("ACTIVE" . "#00d279")))

;; Functions
(defun force-backup-of-buffer ()
    ;; Make a special "per session" backup at the first save of each
    ;; emacs session.
    (when (not buffer-backed-up)
      ;; Override the default parameters for per-session backups.
      (let ((backup-directory-alist '(("" . "~/.emacs.d/backup/per-session")))
            (kept-new-versions 3))
        (backup-buffer)))
    ;; Make a "per save" backup on each save.  The first save results in
    ;; both a per-session and a per-save backup, to keep the numbering
    ;; of per-save backups consistent.
    (let ((buffer-backed-up nil))
      (backup-buffer)))

(defun my-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* (* 1024 1024) 10)) ;; 10 MB
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))

;; (require 'package)
;; (package-initialize)
;; (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;; 			 ("melpa" . "http://melpa.milkbox.net/packages/")))

;; (unless package-archive-contents
;;   (package-refresh-contents))

;; (setq package-list
;;       '(
;;         ;; ace-jump-mode
;;         ;; ample-theme
;;         ;; monochrome-theme
;;         ;; restclient
;;         ;; tao-theme
;;         ;; twilight-theme
;;         ;; white-theme
;;         ;; ace-window
;;         ;; ag
;;         ;; auto-complete
;;         ;; expand-region
;;         ;; flx-ido
;;         ;; ido-ubiquitous
;;         ;; ido-vertical-mode
;;         ;; imenu-anywhere
;;         ;; multiple-cursors
;;         ;; neotree
;;         ;; projectile
;;         ;; magit
;;         ;; smex
;;         ))

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
;; (global-set-key (kbd "C-=") 'er/expand-region)

;; ;; flx-ido
;; (flx-ido-mode t)

;; ;; ido-ubiquitous
;; (ido-ubiquitous-mode t)

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

;; ;; neotree
;; (global-set-key [f8] 'neotree-toggle)

;; ;; projectile
;; (projectile-global-mode t)
;; (setq projectile-mode-line '(:eval (format " Proj[%s]" (projectile-project-name))))

;; ;; smex
;; (global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)
