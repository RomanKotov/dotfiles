;; Proper evil startup
(setq evil-want-integration nil)

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
         auto-complete
         color-theme-solarized
         evil
         evil-numbers
         evil-surround
         evil-visualstar
         git-gutter
         helm
         magit
         multi-term
         neotree
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
                        evil-collection
                        use-package
                        linum-relative
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

(package-initialize)

(cfg:install-packages)

;; General configuration
(setq x-select-enable-clipboard t) ; clipboard integration

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
     (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
     (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
     (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
     (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)))

;; Git gutter configuration
(global-git-gutter-mode +1)

;; Helm configuration
(require 'helm)
(helm-mode 1)

;; Neotree configuration
(require 'neotree)
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
 '(package-selected-packages (quote (use-package evil-collection))))
(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)

;; Which-key configuration
(require 'which-key)
(which-key-mode)

;; Switch-window configuration
(require 'switch-window)
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

;; Multi term config
(require 'multi-term)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Which key configuration
(require 'which-key)
(which-key-mode)

;; Line numbering configuration
(require 'linum-relative)
(setq linum-relative-backend 'display-line-numbers-mode)

;; Appearance configuration
(use-package intellij-theme :ensure t)
(add-to-list 'default-frame-alist '(font . "Hack-11"))

;; change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)
