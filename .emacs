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

(defun my/install-treesitter-languages ()
  "Install treesitter language grammars."
  (mapc
   #'treesit-install-language-grammar
   (mapcar #'car treesit-language-source-alist)))

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
         multiple-cursors vterm yasnippet yasnippet-snippets))
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
  :hook (after-init . global-company-mode))

(use-package company-quickhelp
  :config (company-quickhelp-mode))

(use-package editorconfig
  :config (editorconfig-mode 1))

(use-package eglot
  :hook
  (elixir-ts-mode . eglot-ensure)
  (erlang-mode . eglot-ensure)
  (js-mode . eglot-ensure))

(use-package elixir-ts-mode)

(use-package erlang)

(use-package flycheck
  :init (global-flycheck-mode))

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :bind
  (:repeat-map my/git-gutter-repeat-map
               ("n" . git-gutter:next-hunk)
	       ("p" . git-gutter:previous-hunk)
	       ("s" . git-gutter:stage-hunk)
	       ("v" . git-gutter:popup-hunk)
	       ("r" . git-gutter:revert-hunk))
  :bind-keymap ("C-c g" . my/git-gutter-repeat-map)
  :config (setq git-gutter:update-interval 0.02))

(use-package helm)

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package markdown-mode)

(use-package multiple-cursors
  :bind
  (:repeat-map my/mc-repeat-map
               ("w" . mc/mark-next-like-this-word)
               ("W" . mc/mark-previous-like-this-word)
               ("s" . mc/mark-next-like-this-word)
               ("S" . mc/mark-previous-like-this-word)
               ("a" . mc/mark-all-like-this)
               ("A" . mc/edit-ends-of-lines)
               ("I" . mc/edit-beginnings-of-lines)
               ("e" . mc/mark-more-like-this-extended)
               ("L" . mc/edit-lines))
  :bind-keymap ("C-c m" . my/mc-repeat-map))

(use-package vterm)

(use-package yasnippet
  :bind (:map yas-minor-mode-map
              ("TAB" . nil)
              ("<tab>" . nil)
              ("C-c & e" . yas-expand))
  :config (use-package yasnippet-snippets)
  :init (yas-global-mode))

(use-package window
  :ensure nil
  :bind
  (:repeat-map my/window-repeat-map
               ("o" . other-window)
               ("b" . windmove-left)
               ("f" . windmove-right)
               ("n" . windmove-down)
               ("p" . windmove-up)
               ("P" . enlarge-window)
               ("N" . shrink-window)
               ("F" . enlarge-window-horizontally)
               ("B" . shrink-window-horizontally)
               ("=" . balance-windows)
               ("-" . split-window-vertically)
               ("|" . split-window-horizontally)
               ("0" . delete-window)
               ("1" . delete-other-windows)
               ("u" . winner-undo)
               ("<down>" . windmove-swap-states-down)
               ("<up>" . windmove-swap-states-up)
               ("<left>" . windmove-swap-states-left)
               ("<right>" . windmove-swap-states-right))
  :bind-keymap ("C-c w" . my/window-repeat-map))

(defun vterm-send-line-or-region (&optional step)
  "Send line or region to the shell, and scroll to the end if STEP is true."
  (interactive ())
  (let ((proc (get-process "vterm"))
        pbuf
        min
        max
        command)
    (unless proc
      (let ((currbuff (current-buffer)))
        (vterm)
        (switch-to-buffer currbuff)
        (setq proc (get-process "vterm"))))

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
  (vterm-send-line-or-region t))

(defun sh-switch-to-process-buffer ()
  "Switch to the terminal."
  (interactive)
  (pop-to-buffer (process-buffer (get-process "vterm")) t))

;; Keybindings MacOS
(when (eq system-type 'darwin)
  (setq-default
   mac-option-modifier 'meta
   mac-command-modifier 'super))

;; Remap keys
(global-set-key [remap list-buffers] 'ibuffer)
(global-set-key [remap dabbrev-expand] 'hippie-expand)
(global-set-key (kbd "M-o") (kbd "C-e C-m TAB"))
(global-set-key (kbd "M-O") (kbd "C-a C-m C-p TAB"))
(global-set-key (kbd "C-c e") 'sh-send-line-or-region-and-step)
(global-set-key (kbd "C-c z") 'sh-switch-to-process-buffer)
(global-set-key (kbd "s-b") 'windmove-left)
(global-set-key (kbd "s-f") 'windmove-right)
(global-set-key (kbd "s-p") 'windmove-up)
(global-set-key (kbd "s-n") 'windmove-down)

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
