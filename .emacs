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
         company-quickhelp
         general
         evil
         evil-magit
         evil-matchit
         evil-leader
         evil-nerd-commenter
         evil-numbers
         evil-surround
         evil-visualstar
         flycheck
         git-gutter
         helm
         helm-ag
         helm-projectile
         lsp-mode
         lsp-ui
         magit
         multi-term
         neotree
	 pos-tip
         projectile
	 rainbow-delimiters
         which-key
	 yasnippet
	 yasnippet-snippets
	 zoom-window
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
                        eyebrowse
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
(show-paren-mode 1) ; highlight corresponding paren
(setq-default word-wrap t) ; wrap words
; disable emacs heading
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
; backups configuration
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "/tmp/.emacs/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

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
     (setq-default evil-cross-lines t)
     (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
     (define-key evil-normal-state-map (kbd "] c") 'git-gutter:next-hunk)
     (define-key evil-normal-state-map (kbd "[ c") 'git-gutter:previous-hunk)
     (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
     (define-key evil-normal-state-map (kbd "C-w z") 'zoom-window-zoom)
     (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
     (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
     (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
     (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right))
  )
(require 'evil-magit)

;; Git gutter configuration
(global-git-gutter-mode +1)

;; Helm configuration
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8790269696322ff6821d75414c7d6ea8726d204cdeadedfd04c87b0c915296f7" default)))
 '(display-line-numbers-type (quote visual))
 '(helm-follow-mode-persistent t)
 '(helm-mode-fuzzy-match t)
 '(markdown-command "/usr/bin/pandoc")
 '(neo-cwd-line-style (quote text))
 '(neo-vc-integration (quote (face)))
 '(package-selected-packages (quote (yaml-mode use-package evil-collection)))
 '(zoom-window-mode-line-color "DarkGreen"))
(helm-projectile-on)
(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x

;; Neotree configuration
(setq neo-autorefresh nil)
(global-set-key [f4] 'neotree-toggle)
(setq-default neo-show-hidden-files t)
(setq neo-window-fixed-size nil)

(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)

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
(add-hook 'prog-mode-hook 'linum-relative-mode)

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
"b" '(:ignore t :which-key "buffers")
"g" '(:ignore t :which-key "git")
";" '(:ignore t :which-key "misc")
"ag" 'helm-do-ag-project-root
"b b" 'helm-buffers-list
"f f" 'helm-projectile-find-file
"f d" 'neotree-find
"h s" 'git-gutter:stage-hunk
"h p" 'git-gutter:popup-hunk
"h r" 'git-gutter:revert-hunk
"g s" 'magit-status
"g c" 'magit-commit
"; ;" 'evilnc-comment-or-uncomment-lines
"; m" 'menu-bar-mode
"; s" 'toggle-scroll-bar
"; t" 'tool-bar-mode
)

;; Markdown support
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

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
(company-quickhelp-mode)

;; Lsp configuration
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

;; Yasnipet configuration
(yas-global-mode 1)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-c & e") #'yas-expand)

;; Rainbow delimiters configuration
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Eyebrowse configuration
(eyebrowse-mode t)
(setq eyebrowse-wrap-around t)
(eyebrowse-setup-opinionated-keys)
