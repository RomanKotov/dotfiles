;; Proper evil startup
(setq evil-want-integration nil)
(setq evil-want-C-u-scroll t)

;; Package management
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

;; Package list for el-get
(setq required-packages
      (append
       '(
         all-the-icons
         company-lsp
         company-mode
         general
         evil
         evil-matchit
         evil-leader
         evil-numbers
         evil-surround
         evil-visualstar
         flycheck
         git-gutter
         helm
         lsp-mode
         lsp-ui
         magit
         multi-term
         neotree
         projectile
         switch-window
         which-key
         )
       (mapcar 'el-get-as-symbol (mapcar 'el-get-source-name el-get-sources))))

(el-get 'sync required-packages)

;; Install packages from Elpa
(require 'package)
(require 'cl)

;; Elpa package list
(defvar elpa-packages '(
                        dap-mode
                        evil-collection
                        linum-relative
                        spaceline
                        spacemacs-theme
                        use-package
                        ))

(defun cfg:install-packages ()
  (let ((pkgs (remove-if #'package-installed-p elpa-packages)))
    (when pkgs
      (message "%s" "Emacs refresh packages database...")
      (package-refresh-contents)
      (message "%s" " done.")
      (dolist (p elpa-packages)
        (package-install p)))))

(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(cfg:install-packages)

;; General configuration
(setq x-select-enable-clipboard t) ; clipboard integration
(show-paren-mode 1)
(setq-default word-wrap t)

;; Evil configuration
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))
(global-evil-visualstar-mode)
(eval-after-load "evil"
  '(progn
     (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
     (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
     (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
     (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
     (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
     (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right))
  )

;; Git gutter configuration
(global-git-gutter-mode +1)

;; Helm configuration
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x

;; Neotree configuration
(setq neo-autorefresh nil)
(global-set-key [f4] 'neotree-toggle)
(setq-default neo-show-hidden-files t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8790269696322ff6821d75414c7d6ea8726d204cdeadedfd04c87b0c915296f7" default)))
 '(display-line-numbers-type (quote visual))
 '(global-display-line-numbers-mode t)
 '(neo-cwd-line-style (quote text))
 '(neo-vc-integration (quote (face)))
 '(package-selected-packages (quote (intellij-theme use-package evil-collection))))
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)

;; Which-key configuration
(which-key-mode)

;; Switch-window configuration
(global-set-key (kbd "C-x o") 'switch-window)
(global-set-key (kbd "C-x 1") 'switch-window-then-maximize)
(global-set-key (kbd "C-x 2") 'switch-window-then-split-below)
(global-set-key (kbd "C-x 3") 'switch-window-then-split-right)
(global-set-key (kbd "C-x 0") 'switch-window-then-delete)

(global-set-key (kbd "C-x 4 d") 'switch-window-then-dired)
(global-set-key (kbd "C-x 4 f") 'switch-window-then-find-file)
(global-set-key (kbd "C-x 4 m") 'switch-window-then-compose-mail)
(global-set-key (kbd "C-x 4 r") 'switch-window-then-find-file-read-only)

(global-set-key (kbd "C-x 4 C-f") 'switch-window-then-find-file)
(global-set-key (kbd "C-x 4 C-o") 'switch-window-then-display-buffer)

(global-set-key (kbd "C-x 4 0") 'switch-window-then-kill-buffer)

(makunbound 'switch-window-extra-map)
(defcustom switch-window-extra-map
           (let ((map (make-sparse-keymap)))
             (define-key map (kbd "k") 'switch-window-mvborder-up)
             (define-key map (kbd "j") 'switch-window-mvborder-down)
             (define-key map (kbd "h") 'switch-window-mvborder-left)
             (define-key map (kbd "l") 'switch-window-mvborder-right)
             (define-key map (kbd "b") 'balance-windows)
             (define-key map (kbd "SPC") 'switch-window-resume-auto-resize-window)
             map)
"Extra keymap for ‘switch-window’ input.
Note: at the moment, it cannot bind commands, which will
increase or decrease window's number, for example:
`split-window-below' `split-window-right' `maximize'.")
(setq switch-window-shortcut-appearance 'asciiart)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Which key configuration
(which-key-mode)

;; Line numbering configuration
(setq linum-relative-backend 'display-line-numbers-mode)

;; change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; Matchit configuration
(global-evil-matchit-mode 1)

;; Leader configuration
(global-evil-leader-mode)

(general-define-key
:keymaps '(normal visual)
:prefix "SPC"
:non-normal-prefix "M-SPC"
"f" '(:ignore t :which-key "files")
"h" '(:ignore t :which-key "hunk")
"g" '(:ignore t :which-key "git")
"f f" 'helm-find-files
"h s" 'git-gutter:stage-hunk
"h p" 'git-gutter:popup-hunk
"h u" 'git-gutter:unstage-hunk
"g s" 'magit-status
"g c" 'magit-commit
)

;; Markdown support
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
(custom-set-variables
 '(markdown-command "/usr/bin/pandoc"))

;; Spacemacs theme and visual
(load-theme 'spacemacs-dark t)
(setq powerline-default-separator 'arrow-fade)
(require 'spaceline-config)
(spaceline-spacemacs-theme)
(use-package all-the-icons)

;; Flycheck configuration
(global-flycheck-mode)

;; Company mode
(add-hook 'after-init-hook 'global-company-mode)

;; Lsp configuraion
(use-package lsp-mode
  :config
  (setq lsp-prefer-flymake nil) ;; Prefer using lsp-ui (flycheck) over flymake.
  )

(add-hook 'prog-mode-hook #'lsp)

(use-package lsp-ui
  :requires lsp-mode flycheck
  :config

  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable nil
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-flycheck-live-reporting t
        lsp-ui-peek-enable t
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25)

  (add-hook 'lsp-mode-hook 'lsp-ui-mode))
(use-package company
  :config
  (global-company-mode 1)
  (global-set-key (kbd "<tab>") 'company-complete))

(use-package company-lsp
  :requires company
  :config
  (push 'company-lsp company-backends)

   ;; Disable client-side cache because the LSP server does a better job.
  (setq company-transformers nil
        company-lsp-async t
        company-lsp-cache-candidates nil))

(dap-mode 1)
(dap-ui-mode 1)
(require 'dap-python)

(setq lsp-restart 'auto-restart)
