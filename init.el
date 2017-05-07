;; ==============================
;; Configure setup
;; ==============================

;; Start emacs as a server
(load "server")
(unless (server-running-p)
  (server-start))

(require 'package)

(setq package-enable-at-startup nil)

(setq package-archives '(("org-elpa"     . "http://orgmode.org/elpa/")
			 ("gnu"          . "http://elpa.gnu.org/packages/")
			 ("melpa"        . "https://melpa.org/packages/")
			 ("melpa-stable" . "http://stable.melpa.org/packages/")
			 ("marmalade"    . "http://marmalade-repo.org/packages/"))
     package-archive-priorities '(("melpa-stable" . 1)))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents) ; updage packages archive
  (package-install 'use-package)) ; and install the most recent version of use-package

(require 'use-package)
(require 'json)

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
  (setq all-the-icons-for-buffer t))

(use-package all-the-icons-dired
  :ensure t
  :diminish all-the-icons-dired-mode
  :after all-the-icons
  :init
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package rainbow-delimiters
  :ensure t
  :pin melpa-stable
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

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
(defalias 'list-buffers 'ibuffer)
(setq-default indent-tabs-mode nil)

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

;; y's and n's everywhere
(fset 'yes-or-no-p 'y-or-n-p)

;; ask before exit
(add-hook 'kill-emacs-query-functions
          (lambda () (y-or-n-p "Do you really want to exit Emacs? "))
          'append)

;; set up some shortcuts to dotfiles & orgfiles
(set-register ?e (cons 'file "~/.emacs.d/init.el"))
(set-register ?z (cons 'file "~/.zshrc"))
(set-register ?l (cons 'file "~/Org/logs.org"))

;; handle ansi escape seqs in log files
(use-package ansi-color
  :config
  (defun display-ansi-colors ()
    (interactive)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))
  (add-to-list 'auto-mode-alist '("\\.log\\'" . display-ansi-colors)))

(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-tail-mode))

(use-package try
  :ensure t
  :defer t)

(use-package eshell
  :init
  (setenv "PAGER" "cat")
  (setq eshell-visual-commands
	'("less" "tmux" "htop" "top" "bash" "zsh"
	  "fish" "ssh" "tail" "vi" "more"))
  (setq eshell-visual-subcommands
	'("git" ("log" "diff" "show"))))

(use-package eshell-git-prompt
  :ensure t
  :config
  (eshell-git-prompt-use-theme 'powerline))

(use-package tramp)

(use-package undohist
  :ensure t
  :defer t
  :init
  (customize-set-variable 'udohist-directory "~/.emacs.d/.emacs-undo")
  :config
  (undohist-initialize))

;; ==============================
;; dired
;; ==============================

;; allow dired to be able to delete or copy a whole dir.
(setq dired-recursive-copies (quote always)) ; “always” means no asking
(setq dired-recursive-deletes (quote top)) ; “top” means ask once
(setq dired-dwim-target t)

; dired-details
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
  :bind (("C-'" . avy-goto-char-2))
  :config
  (avy-setup-default))

(use-package ivy
  :ensure t
  :diminish (ivy-mode . "")
  :init (ivy-mode 1)
  :bind
  (:map ivy-mode-map
	("C-;" . ivy-avy)
	("C-j" . ivy-next-line)
	("C-k" . ivy-previous-line)
	("M-j" . ivy-next-history-element)
	("M-k" . ivy-previous-history-element)
	)
  :config
  (setq enable-recursive-minibuffers t)
  (setq ivy-use-virtual-buffers t)   ; extend searching to bookmarks
  (setq ivy-height 15)               ; set height of the ivy window
  (setq ivy-count-format "(%d/%d) ") ; count format, from the ivy help page
  )

(use-package swiper
  :ensure t)

(use-package counsel
  :ensure t
  :bind
  (:map read-expression-map
	("C-r" . counsel-expression-history)))

(use-package counsel-gtags
  :ensure t
  :after counsel
  :defer t)

(use-package dumb-jump
  :ensure t
  :defer t
  :config
  (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-aggressive nil)
  (dumb-jump-mode))

(use-package projectile
  :ensure t
  :config
  (setq projectile-completion-system 'ivy)
  (setq projectile-mode-line '(:eval (format " [%s]" (projectile-project-name))))
  (projectile-mode))

(use-package counsel-projectile
  :ensure t
  :after projectile
  :config
  (counsel-projectile-on))

(use-package projectile-rails
  :ensure t
  :diminish projectile-rails-mode
  :after projectile
  :config
  (projectile-rails-global-mode))

(use-package evil
  :ensure t
  :config
  (setq evil-disable-insert-state-bindings t)
  (evil-set-initial-state 'eshell-mode 'emacs)
  (evil-set-initial-state 'inf-ruby-mode 'emacs)
  (evil-mode 1))

(use-package evil-escape
    :ensure t
    :diminish evil-escape-mode
    :after evil
    :config
    (evil-escape-mode 1))

(use-package evil-nerd-commenter
  :ensure t
  :after evil
  :config
  (evilnc-default-hotkeys))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (smartparens-global-mode t)
  (use-package smartparens-config)
  (use-package smartparens-html
    :defer t)
  (use-package smartparens-ruby
    :defer t)
  (use-package evil-smartparens
    :diminish evil-smartparens-mode
    :config
    (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)))

(use-package magit
  :ensure t
  :init
  (setq magit-refs-local-branch-format "%4c %-25n %h %U%m\n"))

(use-package evil-magit
  :ensure t
  :after (evil magit)
  :config
  (setq evil-magit-state 'normal))

(use-package git-timemachine
  :ensure t
  :defer t
  :config
  (evil-make-overriding-map git-timemachine-mode-map 'normal)
  (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))

(use-package mo-git-blame
  :ensure t
  :defer t
  :config
  (setq mo-git-blame-use-magit 'always)
  (evil-make-overriding-map mo-git-blame-mode-map 'normal)
  (add-hook 'mo-git-blame-mode-hook #'evil-normalize-keymaps))

(use-package general
  :ensure t
  :config
  (general-evil-setup t)
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

   "SPC" '(counsel-M-x :which-key "M-x")
   "TAB" '(next-buffer :which-key "next-buffer")
   "DEL" '(previous-buffer :which-key "previous-buffer")
   "." '(counsel-gtags-find-symbol :which-key "find-symbol")
   "u" '(undo-tree-visualize :which-key "undo tree")

   "j"  '(:ignore t :which-key "dumb jump")
   "jj" 'dumb-jump-go
   "jo" 'dumb-jump-go-other-window
   "jb" 'dumb-jump-back
   "jq" 'dumb-jump-quick-look

   "h"  '(:ignore t :which-key "help")
   "ha" 'apropos-command
   "hd" 'apropos-documentation
   "hi" 'info
   "hk" 'describe-key
   "hf" 'counsel-describe-function
   "hv" 'counsel-describe-variable
   "hb" 'describe-bindings
   "hm" 'describe-mode
   "hp" 'describe-package
   "hr" 'info-emacs-manual
   "hs" 'describe-syntax
   "hl" 'counsel-find-library

   "b"  '(:ignore t :which-key "buffers")
   "bb" 'ivy-switch-buffer
   "bB" 'ivy-switch-buffer-other-window
   "ss" 'save-buffer
   "bi" 'ibuffer
   "bs" 'swiper
   "bk" 'kill-this-buffer
   "bK" 'kill-buffer
   "b-" 'split-window-vertically
   "b/" 'split-window-horizontally

   "f"  '(:ignore t :which-key "files")
   "ff" 'counsel-find-file
   "fF" 'find-file-other-window
   "fr" 'counsel-recentf

   "d"  '(:ignore t :which-key "directories")
   "dd" 'ido-dired
   "do" 'dired-other-window

   "g"  '(:ignore t :which-key "git")
   "gs" 'magit-status
   "gg" 'counsel-git-grep
   "gf" 'counsel-git
   "gt" 'git-timemachine-toggle
   "gb" 'mo-git-blame-current

   "c"  '(:ignore t :which-key "counsel")
   "cs" '(swiper :which-key "swiper")
   "cd" '(counsel-dpkg :which-key "dpkg")
   "ci" '(counsel-imenu :which-key "imenu")
   "cl" '(counsel-locate :which-key "locate")
   "ct" '(counsel-load-theme :which-key "themes")
   "cx" '(counsel-linux-app :which-key "linux-apps")
   "cy" '(counsel-yank-pop :which-key "yank-pop")

   "p"  '(:ignore t :which-key "projectile")
   "pb" '(counsel-projectile-switch-to-buffer :which-key "switch-buffer")
   "pB" '(projectile-switch-to-buffer-other-window :which-key "switch-buffer other-window")
   "pd" '(counsel-projectile-find-dir :which-key "find-dir")
   "pD" '(projectile-dired :which-key "dired")
   "pe" '(projectile-recentf :which-key "recentf")
   "pi" '(projectile-ibuffer :which-key "iBuffer")
   "pj" '(projectile-find-tag :which-key "find-tag")
   "po" '(projectile-multi-occur :which-key "multi-occur")
   "pp" '(counsel-projectile-find-file :which-key "find-file")
   "pf" '(counsel-projectile-switch-project :which-key "switch-project")
   "ps" '(counsel-projectile-ag :which-key "ag")
   "pS" '(projectile-save-project-buffers :which-key "save-buffers")
   "pk" '(projectile-kill-buffers :which-key "kill-buffers")

   "n" '(:ignore t :which-key "bundler-and-rvm")
   "na" 'rvm-activate-corresponding-ruby
   "nd" 'bundle-update
   "ne" 'bundle-exec
   "ni" 'bundle-install
   "no" 'bundle-open
   "nr" 'bundle-console
   "ns" 'bundle-show
   "nu" 'rvm-use

   "r"  '(:ignore t :which-key "rails")
   "r!" '(:ignore t :which-key "run")

   "rg"  '(:ignore t :which-key "goto")
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
   "rR" '(projectile-rails-server :which-key "server")
   "rv" '(projectile-rails-find-view :which-key "find-view")
   "rV" '(projectile-rails-find-current-view :which-key "find-current-view")
   ))

;; ==============================
;; org mode
;; ==============================

(use-package org
  :pin org-elpa
  :ensure t
  :defer t
  )

(use-package org-bullets
  :ensure t
  :defer t
  :after org
  :pin melpa-stable
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; ==============================
;; Language Support
;; ==============================

(use-package ruby-mode
  :ensure t
  ; :mode
  ; (("\\.rb\\" . ruby-mode) ("\\.ru\\" . ruby-mode)
  ;  ("\\.rake\\" . ruby-mode) ("Gemfile" . ruby-mode))
  :config
  (use-package inf-ruby
    :ensure t
    :defer t))

(use-package web-mode
  :ensure t
  :defer t
  :mode
  (("\\.erb\\'" . web-mode)
   ("\\.html?\\'" . web-mode))
  :config
  (setq web-mode-code-indent-offset 2
	web-mode-css-indent-offset 2
	web-mode-enable-css-colorization t
	web-mode-markup-indent-offset 2
	web-mode-script-padding 2
	web-mode-style-padding 2)
  (general-define-key
   :states 'normal
   :keymaps 'web-mode-map
   "%" 'web-mode-tag-match))

(use-package rvm
  :ensure t
  :config
  (rvm-use-default))

(use-package yaml-mode
  :ensure t
  :defer t)

(use-package rspec-mode
  :ensure t
  :diminish rspec-mode
  :config
  (add-hook 'dired-mode-hook 'rspec-dired-mode)
  (general-define-key
   :states '(normal visual insert emacs)
   :keymaps 'rspec-verifiable-mode-map
   :prefix "SPC ,"
   :non-normal-prefix "C-SPC ,"
   "a" 'rspec-verify-all
   "f" 'rspec-run-last-failed
   "v" 'rspec-verify)
  (general-define-key
   :states '(normal visual emacs)
   :keymaps 'rspec-dired-mode-map
   :prefix "SPC ,"
   :non-normal-prefix "C-SPC ,"
   "a" 'rspec-verify-all
   "f" 'rspec-run-last-failed))

(use-package bundler
  :ensure t)

(use-package feature-mode
  :ensure t)

(use-package markdown-mode
  :ensure t
  :defer t
  :pin melpa-stable
  :commands (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown"))

(use-package erlang
  :ensure t
  :defer t
  :init
  (setq erlang-indent-level 2)
  :config
  (use-package erlang-start))

(use-package anaconda-mode
  :ensure t
  :defer t
  :diminish anaconda-mode
  :init
  (setq python-shell-interpreter "/home/timmillar/anaconda2/bin/ipython"
	python-shell-interpreter-args "--simple-prompt -i")
  :config
  (add-hook 'python-hook 'anaconda-mode)
  (add-hook 'python-hook 'anaconda-eldoc-mode))

(use-package ensime
  :ensure t
  :defer t
  :pin melpa-stable)

(use-package haskell-mode
  :ensure t
  :defer t
  :pin melpa-stable)

;; ==============================
;; Tidy-up modeline
;; ==============================

(use-package diminish
  :config
  (diminish 'auto-revert-mode)
  (diminish 'eldoc-mode)
  (diminish 'compilation-shell-minor-mode))

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
