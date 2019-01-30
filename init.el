;; Notes

;; To search for string/phrase for files in a directory:
;; M-x rgrep

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
      vc-make-backup-files t
      ring-bell-function 'ignore)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

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
;; (tool-bar-mode -1)
;; (scroll-bar-mode -1)
;; (global-linum-mode t)
;; (fringe-mode nil)
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
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "M-i") 'imenu)

;; Org
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; https://orgmode.org/worg/org-configs/org-customization-guide.html
(setq
 org-directory "~/org"
 org-default-notes-file "~/org/todo.org"
 org-agenda-files '("~/org/todo.org")
 org-startup-folded "content"
 org-archive-location "~/org/.archive.org::* From %s"
 org-hide-leading-stars nil
 org-completion-use-ido t
 org-link-file-path-type 'absolute
 org-log-done 'time
 org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)"))
 org-todo-keyword-faces '(("TODO" . "#7f7f7f")
                          ("STARTED" . "#00d279")
                          ("WAITING". "#cdcd00")
                          ("DONE" . "#00cd00")
                          ("CANCELED" . "#cd0000"))
 org-agenda-start-with-log-mode t
 org-agenda-log-mode-items '(scheduled deadline started closed)
 x-select-enable-clipboard t
 org-reverse-note-order t
 org-agenda-skip-scheduled-if-done t
 org-agenda-skip-deadline-if-done t
 )

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
  (when (> (buffer-size) (* (* 1024 1024) 100)) ;; 100 MB
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))

(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

;; Requirement: Install xsel program
;; https://hugoheden.wordpress.com/2009/03/08/copypaste-with-emacs-in-terminal/
(defun xsel-cut-function (text &optional push)
  (with-temp-buffer
    (insert text)
    (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))

(defun xsel-paste-function()
  (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
    (unless (string= (car kill-ring) xsel-output)
      xsel-output )))


;; (if (display-graphic-p)
;;     (progn
;;       ;; (set-face-attribute 'default nil :font "Courier New-11")
;;       ;; (set-frame-font "Courier New-11" nil t)
;;       )
;;   (progn

;;     ;; Unix
;;     ;; (setq interprogram-cut-function 'xsel-cut-function)
;;     ;; (setq interprogram-paste-function 'xsel-paste-function)))

;;     ;; Mac
;;     ;; (setq interprogram-cut-function 'paste-to-osx)
;;     ;; (setq interprogram-paste-function 'copy-from-osx))
;;   )


;; (require 'package)
;; (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
;;                     (not (gnutls-available-p))))
;;        (proto (if no-ssl "http" "https")))
;;   ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
;;   ;; (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
;;   (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
;;   (when (< emacs-major-version 24)
;;     ;; For important compatibility libraries like cl-lib
;;     (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
;; (package-initialize)

;; (unless package-archive-contents
;;   (package-refresh-contents))

;; (setq package-list
;;       '(
;;         ;; ace-jump-mode
;;         ;; ace-window
;;         ;; ag
;;         ;; ample-theme
;;         ;; auto-complete
;;         ;; basic-theme
;;         ;; color-theme-sanityinc-tomorrow
;;         ;; exec-path-from-shell
;;         ;; expand-region
;;         ;; flx-ido
;;         ;; go-mode
;;         ;; go-autocomplete
;;         ;; gotest
;;         ;; ido-completing-read+
;;         ;; ido-vertical-mode
;;         ;; imenu-anywhere
;;         ;; magit
;;         ;; monochrome-theme
;;         ;; multiple-cursors
;;         ;; neotree
;;         ;; projectile
;;         ;; restclient
;;         ;; smex
;;         ;; sql-indent
;;         ;; sqlup-mode
;;         ;; tao-theme
;;         ;; twilight-theme
;;         ;; web-mode
;;         ;; white-theme
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

;; ;; exec-path-from-shell
;; (when (memq window-system '(mac ns x))
;;   (exec-path-from-shell-initialize))

;; ;; expand-region
;; (global-set-key (kbd "C-=") 'er/expand-region)

;; ;; flx-ido
;; (flx-ido-mode t)

;; ;; flycheck
;; (add-hook 'after-init-hook #'global-flycheck-mode)

;; ;; go-mode
;; ;; http://tleyden.github.io/blog/2014/05/22/configure-emacs-as-a-go-editor-from-scratch/
;; ;; go get golang.org/x/tools/cmd/...
;; ;; go get github.com/rogpeppe/godef
;; ;; go get -u github.com/nsf/gocode
;; (setq gofmt-command "goimports")
;; (add-hook 'go-mode-hook
;;           (lambda ()
;;             (add-hook 'before-save-hook 'gofmt-before-save)
;;             (local-set-key (kbd "M-.") 'godef-jump)
;;             (local-set-key (kbd "M-*") 'pop-tag-mark)
;;             (setq tab-width 2)))

;; gotest

;; ;; ido-completing-read+
;; (ido-ubiquitous-mode 1)

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

;; ;; sql-indent
;; (add-hook 'sql-mode-hook 'sqlind-minor-mode)

;; ;; sqlup-mode
;; (add-hook 'sql-mode-hook 'sqlup-mode)

;; ;; web-mode
;; (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
;; (defun my-web-mode-hook ()
;;   (setq web-mode-markup-indent-offset 2)
;;   (setq web-mode-css-indent-offset 2)
;;   (setq web-mode-code-indent-offset 2))
;; (add-hook 'web-mode-hook  'my-web-mode-hook)
