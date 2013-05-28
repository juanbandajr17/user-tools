;;color-theme
;;(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0")
;;(require 'color-theme)
;;(eval-after-load "color-theme"
;;  '(progn
;;     (color-theme-initialize)
;;     (color-theme-clarity)))

;;; rhtml mode
(add-to-list 'load-path "~/.emacs.d/rhtml")
(require 'rhtml-mode)

;;color-theme
(add-to-list 'load-path "~/.emacs.d/themes")
(if window-system
    (require 'zenburn-theme)
  (begin
   (require 'color-theme-tango)
   (color-theme-tango))
  ())

;; Original HOWTO-link to setup rails with emacs: http://appsintheopen.com/articles/1-setting-up-emacs-for-rails-development

;; Load CEDET.
;; This is required by ECB which will be loaded later.
;; See cedet/common/cedet.info for configuration details.
;;(load-file "~/.emacs.d/cedet-1.0/common/cedet.el")

;; * This enables some tools useful for coding, such as summary mode
;;   imenu support, and the semantic navigator
;;(semantic-load-enable-code-helpers)

;; Load ECB
;;(add-to-list 'load-path "~/.emacs.d/ecb-2.40")
;;(require 'ecb)

;; auto-indent after 'enter' in ruby mode
;;(add-hook 'ruby-mode-hook
;;      (lambda()
;;        (add-hook 'local-write-file-hooks
;;                  '(lambda()
;;                     (save-excursion
;;                       (untabify (point-min) (point-max))
;;                       (delete-trailing-whitespace)
;;                       )))
;;        (set (make-local-variable 'indent-tabs-mode) 'nil)
;;        (set (make-local-variable 'tab-width) 2)
;;        (imenu-add-to-menubar "IMENU")
     ;   (define-key ruby-mode-map "\C-m" 'newline-and-indent) ;Not sure if this line is 100% right!
     ;   (require 'ruby-electric)
     ;   (ruby-electric-mode t)
;;        )) 

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


;;(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
;; '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
;; '(erb-delim-face ((t (:inherit font-lock-preprocessor-face :background "#171717" :slant italic :weight bold))))
;; '(erb-face ((t nil)))
;; '(erb-out-delim-face ((((background dark)) (:inherit erb-delim-face))))
;; '(font-lock-comment-face ((((class color) (min-colors 88) (background dark)) (:foreground "red")))))
