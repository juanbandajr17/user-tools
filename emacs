;;Emacs 24.3.1

;; Package Manager
(require 'package)
(package-initialize)

;;Screen-Saver
(require 'zone)
(zone-when-idle 300)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes nil)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tool-bar-mode nil))

;;backup-files
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacs.d/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;; auto reload files that have changed on disk
(global-auto-revert-mode t)

;; line numbers
(global-linum-mode t)
(defun linum-format-func (line)
  (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
     (propertize (format (format "%%%dd " w) line) 'face 'linum)))
(setq linum-format 'linum-format-func)

;; inhibit starting screen on emacs
(setq inhibit-splash-screen t)

;; Prompts before exiting emacs
;;(setq confirm-kill-emacs 'yes-or-no-p)

;; Smooth Scrolling
(setq mouse-wheel-scroll-amount '(0.02))
(setq mouse-wheel-progressive-speed nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
