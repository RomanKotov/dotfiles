;;; .emacs --- My simple configuration

;;; Commentary:
;; Simple configuration

;;; Code:

;; Package configuration
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

;; Treesitter configuration
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
  "Install treesitter language grammars."
  (mapc
   #'treesit-install-language-grammar
   (mapcar #'car treesit-language-source-alist)))

;; Should use after a fresh installation or changing treesitter options
;; (install-treesitter-languages)

;; Custom variables
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
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(isearch-allow-motion t)
 '(kept-old-versions 2)
 '(line-move-visual t)
 '(make-backup-files t)
 '(package-selected-packages
   '(avy company company-mode company-quickhelp editorconfig
         eglot erlang flycheck git-gutter helm magit markdown-mode
         multiple-cursors yasnippet yasnippet-snippets))
 '(repeat-mode t)
 '(version-control t)
 '(winner-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Configure file mode mapping
(add-to-list 'auto-mode-alist '("\\.exs?\\'" . elixir-ts-mode))
(add-to-list 'auto-mode-alist '("\\.heex\\'" . heex-ts-mode))

;; Packages
(use-package avy
  :bind (("C-;" . avy-goto-char-timer)
	 ("M-g l" . avy-goto-line)))

(use-package company
  :hook
  (after-init . global-company-mode))

(use-package company-quickhelp
  :config (company-quickhelp-mode))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package eglot
  :hook
  (elixir-ts-mode . eglot-ensure)
  (erlang-mode . eglot-ensure)
  (js-mode . eglot-ensure)
  )

(use-package elixir-ts-mode)

(use-package erlang)

(use-package flycheck
  :init (global-flycheck-mode))

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :bind (("C-c h n" . git-gutter:next-hunk)
	 ("C-c h p" . git-gutter:previous-hunk)
	 ("C-c h s" . git-gutter:stage-hunk)
	 ("C-c h v" . git-gutter:popup-hunk)
	 ("C-c h r" . git-gutter:revert-hunk))
  :config (setq git-gutter:update-interval 0.02))

(use-package helm)

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package markdown-mode)

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package yasnippet
  :bind (:map yas-minor-mode-map
              ("TAB" . nil)
              ("<tab>" . nil)
              ("C-c & e" . yas-expand))
  :config (use-package yasnippet-snippets)
  :init (yas-global-mode))

;; Terminal workarounds
(require 'term)
(defun mp-term-custom-settings ()
  "Customize keybindings for term."
  (local-set-key (kbd "M-p") 'term-send-up)
  (local-set-key (kbd "M-n") 'term-send-down))
(add-hook 'term-load-hook #'mp-term-custom-settings)

(define-key term-raw-map (kbd "M-o") 'other-window)
(define-key term-raw-map (kbd "M-p") 'term-send-up)
(define-key term-raw-map (kbd "M-n") 'term-send-down)

(defun ansi-term-send-line-or-region (&optional step)
  "Send line or region to the shell, and scroll to the end if STEP is true."
  (interactive ())
  (let ((proc (get-process "*ansi-term*"))
        pbuf
        min
        max
        command)
    (unless proc
      (let ((currbuff (current-buffer)))
        (call-interactively #'ansi-term)
        (switch-to-buffer currbuff)
        (setq proc (get-process "*ansi-term*"))))

    (setq pbuff (process-buffer proc))

    (if (use-region-p)
        (setq min (region-beginning)
              max (region-end))
      (setq min (point-at-bol)
            max (point-at-eol)))

    (setq command (concat (buffer-substring min max) "\n"))
    (process-send-string proc command)
    (display-buffer (process-buffer proc) t)
    (when step
      (goto-char max)
      (next-line))))

(defun sh-send-line-or-region-and-step ()
  "Send a region to the terminal and jump to the end."
  (interactive)
  (ansi-term-send-line-or-region t))

(defun sh-switch-to-process-buffer ()
  "Switch to the terminal."
  (interactive)
  (pop-to-buffer (process-buffer (get-process "*ansi-term*")) t))

;; Keybindings MacOS
(when (eq system-type 'darwin)
  (setq-default
   mac-option-modifier 'meta
   mac-command-modifier 'super))

;; Remap keys
(global-set-key [remap list-buffers] 'ibuffer)
(global-set-key [remap dabbrev-expand] 'hippie-expand)
(global-set-key (kbd "C-<return>") (kbd "C-e C-m TAB"))
(global-set-key (kbd "M-<return>") (kbd "C-a C-m C-p TAB"))
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-c e") 'sh-send-line-or-region-and-step)
(global-set-key (kbd "C-c z") 'sh-switch-to-process-buffer)

;; Window movement
(windmove-default-keybindings)

;; Completion
(ido-mode 1)

;; Confirmation prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; Remap modes
(setq
 major-mode-remap-alist
 '((yaml-mode . yaml-ts-mode)
   (bash-mode . bash-ts-mode)
   (js2-mode . js-ts-mode)
   (typescript-mode . typescript-ts-mode)
   (json-mode . json-ts-mode)
   (css-mode . css-ts-mode)
   (python-mode . python-ts-mode)))

;; Dired configuration
(require 'dired-x)

;; Custom commands
(defun sudo ()
  "Use TRAMP to `sudo' the current buffer."
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
             buffer-file-name))))

(provide '.emacs)

;;; .emacs ends here
