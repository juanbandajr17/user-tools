;;; rhtml mode
(add-to-list 'load-path "~/.emacs.d/rhtml")
(require 'rhtml-mode)

;;color-theme
(add-to-list 'load-path "~/.emacs.d/themes")
(require 'zenburn-theme)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tool-bar-mode nil))

;; auto reload files that have changed on disk
(global-auto-revert-mode t)
;; inhibit starting screen on emacs
(setq inhibit-splash-screen t)
;; Prompts before exiting emacs
(setq confirm-kill-emacs 'yes-or-no-p)

