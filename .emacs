;;; .emacs --- My simple configuration

;;; Commentary:
;; Simple configuration

;;; Code:

;; package configuration
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(eval-when-compile
  (require 'use-package))

(package-initialize)

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; treesitter configuration
(defvar treesit-language-source-alist
  '(
    (bash "https://github.com/tree-sitter/tree-sitter-bash")
    (cmake "https://github.com/uyha/tree-sitter-cmake")
    (css "https://github.com/tree-sitter/tree-sitter-css")
    (elisp "https://github.com/Wilfred/tree-sitter-elisp")
    (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
    (erlang "https://github.com/WhatsApp/tree-sitter-erlang")
    (heex "https://github.com/phoenixframework/tree-sitter-heex")
    (html "https://github.com/tree-sitter/tree-sitter-html")
    (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
    (json "https://github.com/tree-sitter/tree-sitter-json")
    (make "https://github.com/alemuller/tree-sitter-make")
    (markdown "https://github.com/ikatyang/tree-sitter-markdown")
    (python "https://github.com/tree-sitter/tree-sitter-python")
    (toml "https://github.com/tree-sitter/tree-sitter-toml")
    (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
    (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
    (yaml "https://github.com/ikatyang/tree-sitter-yaml")
    )
  )

(defun install-treesitter-languages ()
  (mapc
   #'treesit-install-language-grammar
   (mapcar #'car treesit-language-source-alist)))

;; Should use after a fresh installation or changing treesitter options
;; (install-treesitter-languages)

;; configuration
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-by-copying-when-linked t)
 '(backup-directory-alist '((".*" . "~/.emacs.d/backup/per-save")))
 '(confirm-kill-emacs 'y-or-n-p)
 '(custom-enabled-themes '(leuven-dark))
 '(delete-old-versions t)
 '(display-line-numbers-type 'relative)
 '(global-hl-line-mode t)
 '(global-visual-line-mode t)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-use-filename-at-point 'guess)
 '(ido-use-url-at-point nil)
 '(inhibit-startup-screen t)
 '(isearch-allow-motion t)
 '(kept-old-versions 2)
 '(line-move-visual t)
 '(make-backup-files t)
 '(package-selected-packages '(git-gutter helm avy))
 '(repeat-mode t)
 '(version-control t)
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
