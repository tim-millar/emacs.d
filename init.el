;; ==============================
;; Configure setup
;; ==============================

;; Start emacs as a server
(load "server")
(unless (server-running-p)
  (server-start))

(require 'package)

(setq package-enable-at-startup nil)

(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents) ; updage packages archive
  (package-install 'use-package)) ; and install the most recent version of use-package

(require 'use-package)

;; ==============================
;; Basic config
;; ==============================

;; creamsody theme
(use-package creamsody-theme
  :ensure t
  :config
  (load-theme 'creamsody t)
  (creamsody-modeline))

;; load source code pro font
(set-frame-font "Source Code Pro 11")

;; icons
(use-package all-the-icons
  :ensure t
  :config
  (setq all-the-icons-color-icons t)
  (setq all-the-icons-for-buffer t)
  (use-package all-the-icons-dired
    :ensure t
    :diminish all-the-icons-dired-mode
    :init
    (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)))

;; switch off splash
(setq inhibit-splash-screen t)

;; start initial emacs in full-screen mode
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; neaten up UI
(tool-bar-mode -1)
(menu-bar-mode -1)
(show-paren-mode t)
(scroll-bar-mode -1)

(blink-cursor-mode 0)
(setq column-number-mode t)
(global-hl-line-mode 1)
(setq sentence-end-double-space nil)
(setq default-fill-column 80)
(setq default-indent-tabs-mode nil)

;; Backup
(setq backup-directory-alist
      '(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/auto-save-list/" t)))
;; Stop backup changing file creation data
(setq backup-by-copying t)

;; winner mode
(winner-mode 1)
;; windmove
(windmove-default-keybindings)
(setq windmove-wrap-around t)

;; set up some shortcuts to dotfiles & orgfiles
(set-register ?e (cons 'file "~/.emacs.d/init.el"))

(require 'tramp)

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

;; ==============================
;; dired
;; ==============================

;; allow dired to be able to delete or copy a whole dir.
(setq dired-recursive-copies (quote always)) ; “always” means no asking
(setq dired-recursive-deletes (quote top)) ; “top” means ask once
(setq dired-dwim-target t)
; dired-detailsauto-save
(use-package dired-details
  :ensure t
  :config
  (dired-details-install)
  (setq dired-details-hidden-string ""))

;; ==============================
;; UI Config
;; ==============================

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init
  (which-key-mode)
  :config
  (which-key-setup-side-window-right-bottom)
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-side-window-max-width 0.33
        which-key-idle-delay 0.75))

(use-package avy
  :ensure t
  :bind (("C-'" . avy-goto-char-2)))

(use-package ivy
  :ensure t
  :diminish (ivy-mode . "") ; does not display ivy in the modeline
  :init (ivy-mode 1)        ; enable ivy globally at startup
  :bind (:map ivy-mode-map  ; bind in the ivy buffer
              ("C-;" . ivy-avy)
              ("C-j" . ivy-next-line)
              ("C-k" . ivy-previous-line)
              )
  :config
  (setq ivy-use-virtual-buffers t)   ; extend searching to bookmarks and …
  (setq ivy-height 20)               ; set height of the ivy window
  (setq ivy-count-format "(%d/%d) ") ; count format, from the ivy help page
  )

(use-package swiper
  :ensure t)

(use-package counsel
  :ensure t)

(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  (use-package counsel-projectile
    :ensure t
    :config
    (counsel-projectile-on))
  (use-package projectile-rails
    :ensure t
    :diminish projectile-rails-mode
    :config
    (projectile-rails-global-mode)))

(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode t)
  (use-package smartparens-config)
  (use-package smartparens-ruby))

(use-package evil
  :ensure t
  :config
  (evil-mode 1))

(use-package magit
  :ensure t)

(use-package general
  :ensure t
  :config
  (general-evil-setup t)
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

   "SPC" '(counsel-M-x :which-key "M-x")

   "h"  '(:ignore t :which-key "help")
   "ha" 'apropos-command
   "hd" 'apropos-documentation
   "hi" 'info
   "hk" 'describe-key
   "hf" 'counsel-describe-function
   "hv" 'counsel-describe-variable
   "hb" 'describe-bindings
   "hm" 'describe-mode
   "hP" 'describe-package
   "hr" 'info-emacs-manual
   "hs" 'describe-syntax
   "hl" 'counsel-find-library

   "b"  '(:ignore t :which-key "buffers")
   "bb" 'ivy-switch-buffer
   "ss" 'save-buffer
   "bo" 'ivy-switch-buffer-other-window
   "bi" 'ibuffer

   "f"  '(:ignore t :which-key "files")
   "ff" 'counsel-find-file
   "fo" 'find-file-other-window

   "d"  '(:ignore t :which-key "directories")
   "dd" 'ido-dired
   "do" 'dired-other-window

   "g"  '(:ignore t :which-key "git")
   "gs" 'magit-status
   "gg" 'counsel-git-grep
   "gf" 'counsel-git

   "p"   '(:ignore t :which-key "projectile")
   "pb"  '(counsel-projectile-switch-to-buffer :which-key "switch-buffer")
   "pd"  '(counsel-projectile-find-dir :which-key "find-dir")
   "pD"  '(projectile-dired :which-key "dired")
   "pI"  '(projectile-ibuffer :which-key "iBuffer")
   "pf"  '(counsel-projectile-find-file :which-key "find-file")
   "pp"  '(counsel-projectile-switch-project :which-key "switch-project")
   "pss" '(counsel-projectile-ag :which-key "ag")

   "r"  '(:ignore t :which-key "rails")
   "r!" '(:ignore t :which-key "run")

   "rg" '(:ignore t :which-key "goto")
   "rgd" '(projectile-rails-goto-schema :which-key "goto-schema")
   "rgf" '(projectile-rails-goto-file-at-point :which-key "goto-file-at-point")
   "rgg" '(projectile-rails-goto-gemfile :which-key "goto-gemfile")
   "rgh" '(projectile-rails-goto-spec-helper :which-key "goto-spec-helper")
   "rgr" '(projectile-rails-goto-routes :which-key "goto-routes")
   "rgs" '(projectile-rails-goto-seeds :which-key "goto-seed")

   "rc" '(projectile-rails-find-controller :which-key "find-controller")
   "rC" '(projectile-rails-find-current-controller :which-key "find-current-controller")
   "rm" '(projectile-rails-find-model :which-key "find-model")
   "rM" '(projectile-rails-find-current-model :which-key "find-current-model")
   "rp" '(projectile-rails-find-spec :which-key "find-spec")
   "rP" '(projectile-rails-find-current-spec :which-key "find-current-spec")
   "rr" '(projectile-rails-console :which-key "console")
   "rv" '(projectile-rails-find-view :which-key "find-view")
   "rV" '(projectile-rails-find-current-view :which-key "find-current-view")
   ))

;; ==============================
;; Language Support
;; ==============================

(use-package ruby-mode
  :ensure t
  :diminish ruby-mode
  ; :mode
  ; (("\\.rb\\" . ruby-mode) ("\\.ru\\" . ruby-mode)
  ;  ("\\.rake\\" . ruby-mode) ("Gemfile" . ruby-mode))
  :config
  (use-package inf-ruby
    :ensure t))

(use-package web-mode
  :ensure t)

(use-package rvm
  :ensure t
  :config
  (rvm-use-default))

(use-package yaml-mode
  :ensure t)

(use-package rspec-mode
  :ensure t)

(use-package bundler
  :ensure t)

(use-package erlang
  :ensure t
  :diminish (erlang-mode . "")
  :config
  (use-package erlang-start))

;; ==============================
;; auto-generated config
;; ==============================

(put 'dired-find-alternate-file 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
