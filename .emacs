(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(show-paren-mode t)
 '(tool-bar-mode nil))


(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;;color-theme
(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0")
(require 'color-theme)

;;; rhtml mode
(add-to-list 'load-path "~/.emacs.d/rhtml")
(require 'rhtml-mode)

;; zenburn color theme
;;(add-to-list 'custom-theme-load-path "~/.emacs.d/zenburn-emacs")
