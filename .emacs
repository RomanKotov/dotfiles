;; Package management
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/"))
  (package-refresh-contents)
  (package-initialize)
  (package-install 'el-get)
  (require 'el-get))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

;; Package list
(el-get-bundle evil)
(el-get-bundle helm)
(el-get-bundle which-key)
(el-get-bundle switch-window)
(el-get-bundle multi-term)
(el-get-bundle magit)
(el-get-bundle git-gutter)
(el-get-bundle neotree)
(el-get-bundle evil-surround)
(el-get-bundle evil-visualstar)
(el-get-bundle evil-numbers)
(el-get-bundle which-key)

;; General configuration
(setq x-select-enable-clipboard t)

;; Evil configuration
(require 'evil)
(evil-mode 1)
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
 '(neo-cwd-line-style (quote text))
 '(neo-vc-integration (quote (face)))
 '(package-selected-packages (quote (evil-collection))))
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8790269696322ff6821d75414c7d6ea8726d204cdeadedfd04c87b0c915296f7" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Which key configuration
(require 'which-key)
(which-key-mode)
