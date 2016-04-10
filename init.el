;; Install emacs
;; http://lars.ingebrigtsen.no/2014/11/13/welcome-new-emacs-developers/

;; Install emacs on Mac OSX
;; git clone git://git.savannah.gnu.org/emacs.git
;; cd emacs
;; ./autogen.sh
;; ./configure --with-ns
;; make install
;; cd nextstep
;; open Emacs.app

;; TERM=xterm-256color
;; TERM=xterm

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
        cider
        clojure-mode
        column-marker
        csv-mode
        diminish
        enh-ruby-mode
        exec-path-from-shell
        expand-region
        flatland-theme
        flatland-black-theme
        flx-ido
        flycheck
        helm-projectile
        helm-ag
        ido-ubiquitous
        ido-vertical-mode
        imenu-anywhere
        inf-ruby
        jedi
        json-mode
        magit
        multi-eshell
        multiple-cursors
        neotree
        perspective
        persp-projectile
        powerline
        projectile
        pyenv-mode
        restclient
        rspec-mode
        rvm
        smart-mode-line
        smex
        solarized-theme
        symon
        visual-regexp
        web-mode
        yaml-mode
        zenburn-theme
        zencoding-mode))

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
;; (autopair-global-mode t)

;; column-marker
(column-marker-1 nil) ;; Load more markers
(add-hook 'ruby-mode-hook (lambda () (interactive) (column-marker-3 100)))

;; diminish
(diminish 'auto-complete-mode)

;; enh-ruby-mode
;; (add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
;; (add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

;; expand-region
(global-set-key (kbd "C-o") 'er/expand-region)

;; exec-path-from-shell
(when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))

;; flx-ido
(flx-ido-mode 1)

;; flycheck
;; (setq flycheck-check-syntax-automatically '(mode-enabled save))
;; (add-hook 'python-mode-hook (lambda () (flycheck-mode 1)))
;; (setq flycheck-flake8-maximum-line-length 100)

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

;; jedi
;; (add-hook 'python-mode-hook 'jedi:ac-setup)

;; multiple-cursors
(global-set-key (kbd "M-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-n") 'mc/mark-next-like-this)

;; neotree
(global-set-key [f8] 'neotree-toggle)
;; (setq projectile-switch-project-action 'neotree-projectile-action)

;; perspective
;; (persp-mode)

;; powerline
;; (powerline-default-theme)

;; projectile
(projectile-global-mode)
(setq projectile-mode-line '(:eval (format " Proj[%s]" (projectile-project-name))))
;; (define-key projectile-mode-map (kbd "C-c p p") 'projectile-persp-switch-project)

;; pyenv-mode
;; (pyenv-mode)

;; rspec-mode
;; (add-hook 'after-init-hook 'inf-ruby-switch-setup)

;; rvm
;; (rvm-use-default)
;; (add-hook 'ruby-mode-hook (lambda () (rvm-activate-corresponding-ruby)))

;; smart-mode-line
;; (sml/setup)
;; (sml/apply-theme 'respectful)

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

;; yaml-mode
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

;; zencoding-mode
(add-hook 'sgml-mode-hook 'zencoding-mode)

;;;;; Emacs Settings
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
(electric-indent-mode 1)
(electric-pair-mode -1)


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

(unless (memq window-system '(mac ns))
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
      (setq interprogram-paste-function 'xsel-paste-function))))

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

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))
(global-set-key [remap goto-line] 'goto-line-with-feedback)

(when (memq window-system '(mac ns))
  ;; Mac copy and paste
  (defun pt-pbpaste ()
    "Paste data from pasteboard."
    (interactive)
    (shell-command-on-region
     (point)
     (if mark-active (mark) (point))
     "pbpaste" nil t))
  (global-set-key [?\C-x ?\C-y] 'pt-pbpaste)
  (defun pt-pbcopy ()
    "Copy region to pasteboard."
    (interactive)
    (print (mark))
    (when mark-active
      (shell-command-on-region
       (point) (mark) "pbcopy")
      (kill-buffer "*Shell Command Output*")))
  (global-set-key [?\C-x ?\M-w] 'pt-pbcopy))
