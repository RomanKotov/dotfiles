;;; .emacs --- My simple configuration

;;; Commentary:
;; Simple configuration

;;; Code:

;; remap keys
(global-set-key [remap list-buffers] 'ibuffer)
(global-set-key (kbd "M-o") 'other-window)

;; Window movement
(windmove-default-keybindings)

;; completion
(icomplete-vertical-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(wombat))
 '(display-line-numbers 'visual)
 '(display-line-numbers-type 'relative)
 '(global-display-line-numbers-mode t)
 '(global-visual-line-mode t)
 '(inhibit-startup-screen t)
 '(line-move-visual t)
 '(repeat-mode t)
 '(winner-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide '.emacs)

;;; .emacs ends here
