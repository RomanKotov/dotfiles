;;; .emacs --- My simple configuration

;;; Commentary:
;; Simple configuration

;;; Code:

;; configuration
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(confirm-kill-emacs 'y-or-n-p)
 '(custom-enabled-themes '(leuven-dark))
 '(display-line-numbers-type 'relative)
 '(global-visual-line-mode t)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-use-filename-at-point 'guess)
 '(ido-use-url-at-point nil)
 '(inhibit-startup-screen t)
 '(isearch-allow-motion t)
 '(line-move-visual t)
 '(repeat-mode t)
 '(use-package-always-ensure t)
 '(winner-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

; packages
(use-package avy
  :bind (("C-;" . avy-goto-char-timer)))

(use-package helm)

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :bind (("C-c h n" . git-gutter:next-hunk)
	 ("C-c h p" . git-gutter:previous-hunk)
	 ("C-c h s" . git-gutter:stage-hunk)
	 ("C-c h v" . git-gutter:popup-hunk)
	 ("C-c h r" . git-gutter:revert-hunk))
  :config (setq git-gutter:update-interval 0.02))

;; remap keys
(global-set-key [remap list-buffers] 'ibuffer)
(global-set-key (kbd "M-o") 'other-window)

;; Window movement
(windmove-default-keybindings)

;; completion
(ido-mode 1)

;; confirmation prompts
(fset 'yes-or-no-p 'y-or-n-p)

(provide '.emacs)

;;; .emacs ends here
