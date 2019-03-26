;; TODO: Improved org settings.

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq custom-file "~/.emacs.d/customize.el")
(when (file-exists-p custom-file)
  (load custom-file))

(setq-default line-spacing 1)
(setq-default tab-width 4)
(setq-default indent-line-function 'insert-tab)
(setq-default indent-tabs-mode nil)
(setq inhibit-splash-screen t)
(setq ring-bell-function 'ignore)
(setq initial-scratch-message nil)

(use-package use-package-ensure-system-package
  :ensure t)

(use-package auto-package-update
  :ensure t
  :custom ((auto-package-update-interval (* 7 4))
           (auto-package-update-delete-old-versions t)
           (auto-package-update-hide-results t)
           (auto-package-update-maybe)
           (auto-package-update-prompt-before-update t)))

(use-package apropos
  :custom (apropos-do-all t))

(defun find-large-file-hook ()
  (when (> (buffer-size) (* (* 1024 1024) 128)) ;; 128 MB
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))

(use-package files
  :hook (find-file . find-large-file-hook)
  :custom
  ((backup-by-copying t)
   (backup-directory-alist '(("." . "~/.emacs.d/backup/per-save")))
   (confirm-kill-emacs 'y-or-n-p)
   (delete-old-versions t)
   (kept-old-versions 0)
   (version-control t)))

(use-package dired
  :custom (dired-listing-switches "-hal"))

(use-package autorevert
  :custom (global-auto-revert-non-file-buffers t))

(use-package ido
  :bind (("M-l" . ido-switch-buffer)
         ("C-x C-b" . ido-switch-buffer))
  :custom ((ido-enable-flex-matching t)
           (ido-everywhere t)
           (ido-max-directory-size 100000)
           (ido-use-virtual-buffers t))
  :config (ido-mode t))

(use-package imenu
  :bind ("M-i" . imenu)
  :custom (imenu-max-item-length 100))

(use-package linum
  :custom (linum-format "%4d")
  :config (global-linum-mode t))

(use-package vc-hooks
  :custom (vc-make-backup-files t))

(use-package select
  :custom (select-enable-clipboard t))

(use-package simple
  :bind ("C-M-l" . goto-line)
  :config
  (column-number-mode t)
  (transient-mark-mode t)
  (size-indication-mode t))

(use-package darkburn)

(use-package delsel
  :config (delete-selection-mode t))

(use-package electric
  :config (electric-indent-mode t))

(use-package elec-pair
  :config (electric-pair-mode -1))

(use-package autorevert
  :config (global-auto-revert-mode t))

(use-package paren
  :config (show-paren-mode t))

(use-package menu-bar
  :config (menu-bar-mode -1))

(use-package fringe
  :config
  (set-fringe-mode '(10 . 0))
  (when (boundp 'fringe-indicator-alist)
    (setq-default fringe-indicator-alist
                  '((continuation . nil)
                    (overlay-arrow . nil)
                    (up . nil)
                    (down . nil)
                    (top . nil)
                    (bottom . nil)
                    (top-bottom . nil)
                    (empty-line . nil)
                    (unknown . nil)))))

(use-package windmove
  :config
  (when (fboundp 'windmove-default-keybindings)
    (windmove-default-keybindings)))

(use-package hippie-exp
  :bind ("M-/" . hippie-expand))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package isearch
  :bind (("C-s" . isearch-forward)
         ("C-r" . 'isearch-backward)))

(use-package replace
  :bind ("M-%" . query-replace-regexp))

(use-package window
  :bind (("C-M-3" . split-window-horizontally)
         ("C-M-2" . split-window-vertically)
         ("C-M-1" . delete-other-windows)
         ("C-o" . other-window)))

(use-package org
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb))
  :custom
  ((org-directory "~/org")
   (org-default-notes-file "~/org/todo.org")
   (org-agenda-files '("~/org/todo.org"))
   (org-startup-folded "content")
   (org-archive-location "~/org/.archive.org::* From %s")
   (org-hide-leading-stars t)
   (org-completion-use-ido t)
   (org-link-file-path-type 'absolute)
   (org-log-done 'time)
   (org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))
   (org-todo-keyword-faces '(("TODO" . "#7f7f7f")
                             ("STARTED" . "#00d279")
                             ("WAITING". "#cdcd00")
                             ("DONE" . "#00cd00")
                             ("CANCELED" . "#cd0000")))
   (org-agenda-start-with-log-mode t)
   (org-agenda-log-mode-items '(scheduled deadline started closed))
   (org-reverse-note-order t)
   (org-agenda-skip-scheduled-if-done t)
   (org-agenda-skip-deadline-if-done t)))

(use-package ace-jump-mode
  :ensure t
  :bind ("M-j" . ace-jump-mode))

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window))

(when (memq window-system '(mac ns x))
  (use-package ag
    :ensure t
    :config
    (setq ag-reuse-buffers t
          ag-highlight-search t)))

(use-package anzu
  :ensure t
  :bind (("M-%" . anzu-query-replace-at-cursor)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode t))

(use-package auto-complete
  :ensure t
  :config (ac-config-default))

(use-package beacon
  :disabled
  :config (beacon-mode t))

(use-package clojure-mode)

(use-package cider)

(use-package dumb-jump
  :ensure t
  :bind (("C-M-g" . dumb-jump-go)
         ("C-M-p" . dumb-jump-back))
  :config (dumb-jump-mode t))

(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :ensure t
    :config (exec-path-from-shell-initialize)))

(use-package expand-region
  :ensure t
  :bind ("M-O" . er/expand-region))

(use-package flx-ido
  :ensure t
  :config (flx-ido-mode t))

(use-package flycheck
  :ensure t
  :after go-mode
  :hook (go-mode . flycheck-mode))

(use-package go-autocomplete
  :ensure t
  :config (ac-config-default))

(use-package go-eldoc
  :ensure t
  :hook ((go-mode . go-eldoc-setup)))

;; go get -u golang.org/x/tools/cmd/...
;; go get -u github.com/rogpeppe/godef
;; go get -u github.com/nsf/gocode
;; go get -u github.com/jstemmer/gotags
(use-package go-mode
  :ensure t
  :bind (:map go-mode-map
              ("M-." . godef-jump)
              ("M-*" . pop-tag-mark))
  :hook (go-mode . (lambda ()
                     (add-hook 'before-save-hook 'gofmt-before-save)
                     (setq tab-width 4)))
  :config
  (setq gofmt-command "goimports"))

(use-package gotest
  :ensure t)

(use-package ido-completing-read+
  :ensure t
  :config (ido-ubiquitous-mode t))

(use-package ido-vertical-mode
  :ensure t
  :custom
  (ido-vertical-define-keys 'C-n-and-C-p-only)
  :config
  (ido-vertical-mode t))

(use-package imenu-anywhere
  :ensure t
  :bind ("M-i" . imenu-anywhere))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package markdown-mode+)

(use-package multiple-cursors
  :ensure t
  :bind (("M-p" . mc/mark-previous-like-this)
         ("M-n" . mc/mark-next-like-this)
         ("M-m" . mc/mark-all-like-this)))

(use-package neotree
  :ensure t
  :bind (("<f9>" . neotree-toggle)
         ("<f10>" . neotree-find)))

(use-package perspective
  :ensure t
  :config (persp-mode))

(use-package persp-projectile
  :ensure t
  :after (projectile perspective)
  :config
  (define-key projectile-mode-map (kbd "C-c p p") 'projectile-persp-switch-project))

(use-package projectile
  :ensure t
  :demand
  :custom
  (projectile-mode-line-function
   (lambda () (format " Proj[%s]" (projectile-project-name))))
  :config
  (projectile-mode t)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package smart-mode-line
  :ensure t
  :custom
  (sml/name-width 25)
  (sml/mode-width 'full)
  (sml/shorten-directory t)
  (sml/shorten-modes t)
  :config
  (sml/setup))

(use-package smex
  :ensure t
  :bind
  (("M-x" . smex) ("M-X" . smex-major-mode-commands)))

(use-package sql-indent
  :ensure t
  :hook (sql-mode . sqlind-minor-mode))

(use-package sqlup-mode
  :ensure t
  :hook (sql-mode . sqlup-mode))

(use-package whitespace
  :hook (before-save . whitespace-cleanup)
  :custom ((whitespace-line-column 99)
           (whitespace-style '(face lines-tail trailing empty))))

(use-package web-mode
  :ensure t
  :mode "\\.gohtml\\'"
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))

(use-package zone
  :disabled
  :config (zone-when-idle 1200))

(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

;; Requirement: Install xsel program
(defun xsel-cut-function (text &optional push)
  (with-temp-buffer
    (insert text)
    (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))

(defun xsel-paste-function()
  (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
    (unless (string= (car kill-ring) xsel-output)
      xsel-output )))

;; When using GUI
(when (display-graphic-p)
  (when (memq window-system '(mac ns))
    (set-face-attribute 'default nil :font "Courier New-11")
    (set-frame-font "Courier New-11" nil t))
  (set-face-foreground 'vertical-border (face-background 'default))
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

;; When using terminal
(when (not (display-graphic-p))
  (when (memq window-system '()) ;; When unix
    (setq interprogram-cut-function 'xsel-cut-function)
    (setq interprogram-paste-function 'xsel-paste-function))
  (when (memq window-system '(mac ns)) ;; When Mac
    (setq interprogram-cut-function 'paste-to-osx)
    (setq interprogram-paste-function 'copy-from-osx)))
