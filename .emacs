;; Proper evil startup
(setq evil-want-integration nil)
(setq evil-want-keybinding nil)
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
         company-mode
         company-quickhelp
         general
	 editorconfig
         evil
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
         multi-term
	 pos-tip
         projectile
	 rainbow-delimiters
	 rust-mode
	 web-mode
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
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

;; Elpa package list
(defvar elpa-packages '(use-package spacemacs-theme))

(defun cfg:install-packages ()
  (let ((pkgs (remove-if #'package-installed-p elpa-packages)))
    (when pkgs
      (message "%s" "Emacs refresh packages database...")
      (package-refresh-contents)
      (message "%s" " done.")
      (dolist (p elpa-packages)
        (package-install p)))))

(package-initialize)

(cfg:install-packages)

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package spaceline
  :config
  (load-theme 'spacemacs-dark t)
  (setq powerline-default-separator 'arrow-fade)
  (require 'spaceline-config)
  (spaceline-spacemacs-theme))

(use-package linum-relative
  :config
  (setq linum-relative-backend 'display-line-numbers-mode)
  (add-hook 'prog-mode-hook 'linum-relative-mode))

(use-package eyebrowse
  :config
  (eyebrowse-mode t)
  (setq eyebrowse-wrap-around t)
  (eyebrowse-setup-opinionated-keys))

;; bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package helm
  :config
  (helm-mode 1)
  (setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
  (global-set-key (kbd "M-x") 'helm-M-x))

;; Evil configuration
(use-package evil
  :defer .1 ;; don't block emacs when starting, load evil immediately after startup
  :init
  (setq-default evil-cross-lines t)
  (setq evil-want-integration nil) ;; required by evil-collection
  (setq evil-want-keybinding nil)
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t) ;; like vim's 'splitright'
  (setq evil-split-window-below t) ;; like vim's 'splitbelow'
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)

  :config
  (evil-mode 1)

  ;; vim-like keybindings everywhere in emacs
  (use-package evil-collection
    :after evil
    :config
    (delete 'company evil-collection-mode-list)
    (evil-collection-init)
    (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
    (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
    (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
    (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
    (evil-define-key 'operator global-map "s" 'evil-surround-edit)
    (evil-define-key 'operator global-map "S" 'evil-Surround-edit)
    (evil-define-key 'visual global-map "S" 'evil-surround-region)
    (evil-define-key 'visual global-map "gS" 'evil-Surround-region))

  (use-package evil-nerd-commenter)

  ;; * operator in vusual mode
  (use-package evil-visualstar
    :init (global-evil-visualstar-mode)
    :bind (:map evil-visual-state-map
                ("*" . evil-visualstar/begin-search-forward)
                ("#" . evil-visualstar/begin-search-backward)))

  ;; evil numbers
  (use-package evil-numbers)

  ;; visual hints while editing
  (use-package evil-goggles
    :config
    (evil-goggles-use-diff-faces)
    (evil-goggles-mode))

  ;; like vim-surround
  (use-package evil-surround
    :commands
    (evil-surround-edit
     evil-Surround-edit
     evil-surround-region
     evil-Surround-region))

  (use-package evil-matchit
    :config
    (global-evil-matchit-mode 1))

  (setq-default evil-cross-lines t)
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "] c") 'git-gutter:next-hunk)
  (define-key evil-normal-state-map (kbd "[ c") 'git-gutter:previous-hunk)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "C-w z") 'zoom-window-zoom)
  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
  )

;; Git support
(use-package magit
  :ensure t)

(use-package all-the-icons)

(use-package general
  :config
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
  ))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; Neotree configuration
(use-package neotree
   :config
   (setq neo-autorefresh nil)
   (global-set-key [f4] 'neotree-toggle)
   (setq neo-theme (if (display-graphic-p) 'icons 'arrow 'nerd))
   (setq-default neo-show-hidden-files t)
   (setq neo-window-fixed-size nil))

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package git-gutter
  :config
  (global-git-gutter-mode +1))

(use-package helm-ag)

(use-package helm-projectile
  :config
  (helm-projectile-on))

;; Markdown support
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Lsp configuration
(use-package lsp-mode
  :hook (lsp-mode . (lambda ()
		      (let ((lsp-keymap-prefix "M-l"))
			(lsp-enable-which-key-integration))))
  :config
  (define-key lsp-mode-map (kbd "M-l") lsp-command-map)
  :init
      (setq lsp-prefer-flymake nil ;; Prefer using lsp-ui (flycheck) over flymake.
	    lsp-restart 'auto-restart)
      (add-hook 'prog-mode-hook #'lsp)
      (add-to-list 'exec-path "~/projects/elixir-ls/release"))

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

(use-package lsp-tailwindcss)

(use-package company
  :config
  (global-company-mode 1)
  (global-set-key (kbd "<tab>") 'company-complete)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package pos-tip)

(use-package company-quickhelp
  :defines company-quickhelp-delay
  :hook (global-company-mode . company-quickhelp-mode)
  :custom (company-quickhelp-delay 0.8))

(use-package magit)

(use-package multi-term)

(use-package neotree
  :config
  (setq neo-autorefresh nil)
  (global-set-key [f4] 'neotree-toggle)
  (setq-default neo-show-hidden-files t)
  (setq neo-window-fixed-size nil)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(use-package projectile)

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.l?eex\\'" . web-mode)))

(use-package which-key
  :config
  (which-key-mode))

(use-package yasnippet
  :config
  (yas-global-mode 1)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "C-c & e") #'yas-expand))

(use-package yasnippet-snippets)

(use-package zoom-window)

(use-package dap-mode
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1))

(use-package elixir-mode
  :config
  (require 'dap-elixir)
  (defun dap-elixir--populate-start-file-args (conf)
    "Populate CONF with the required arguments."
    (-> conf
	(dap--put-if-absent :dap-server-path '("debugger.sh"))
	(dap--put-if-absent :type "mix_task")
	(dap--put-if-absent :name "mix test")
	(dap--put-if-absent :request "launch")
	(dap--put-if-absent :task "test")
	(dap--put-if-absent :projectDir (lsp-find-session-folder (lsp-session) (buffer-file-name)))
	(dap--put-if-absent :cwd (lsp-find-session-folder (lsp-session) (buffer-file-name)))))

  (dap-register-debug-template
   "Elixir::Phoenix::Start"
   (list :type "Elixir"
	 :request "launch"
	 :excludeModules (list "Bcrypt.Base")
	 :task "phx.server"
	 :name "mix phx.server"))

  (dap-register-debug-template
   "Elixir::Test"
   (list :type "Elixir"
	 :request "launch"
	 :excludeModules (list "Bcrypt.Base")
	 :startApps 1
	 :task "test"
	 :taskArgs (list "--failed")
	 :name "mix test"
	 :requireFiles (list
			"test/**/test_helper.exs"
			"test/**/*_test.exs"))
   ))

;; Erlang configuration
(use-package erlang
  :config
  (add-hook 'erlang-mode-hook #'lsp))

;; General configuration
(setq x-select-enable-clipboard t) ; clipboard integration
(fset 'yes-or-no-p 'y-or-n-p) ;; change all prompts to y or n
(show-paren-mode 1) ; highlight corresponding paren
(setq-default word-wrap t) ; wrap words
(setq inhibit-startup-screen t)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(css-indent-offset 2)
 '(custom-safe-themes
   '("8790269696322ff6821d75414c7d6ea8726d204cdeadedfd04c87b0c915296f7" default))
 '(display-line-numbers-type 'visual)
 '(helm-follow-mode-persistent t)
 '(helm-minibuffer-history-key "M-p")
 '(helm-mode-fuzzy-match t)
 '(js-indent-level 2)
 '(js-paren-indent-offset 2)
 '(js-square-indent-offset 2)
 '(js-switch-indent-offset 2)
 '(markdown-command "/usr/bin/pandoc")
 '(neo-cwd-line-style 'text)
 '(neo-vc-integration '(face))
 '(package-selected-packages
   '(vue-mode evil-collection evil-goggles evil-leader evil-magit evil-matchit evil-nerd-commenter evil-numbers evil-surround evil-visualstar use-package yaml-mode))
 '(require-final-newline nil)
 '(zoom-window-mode-line-color "DarkGreen"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed)))))

(provide '.emacs)
;;; .emacs ends here
