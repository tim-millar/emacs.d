;; ==============================
;; Configure setup
;; ==============================

;; Start emacs as a server
(load "server")
(unless (server-running-p)
  (server-start))

;; set up tls-signing for emacs packages
;; taken from "your editor is malware" article
(require 'cl)
(setq tls-checktrust t)

(setq python (or (executable-find "py.exe")
                 (executable-find "python")
                 ))

(let ((trustfile
       (replace-regexp-in-string
        "\\\\" "/"
        (replace-regexp-in-string
         "\n" ""
         (shell-command-to-string (concat python " -m certifi"))))))
  (setq tls-program
        (list
         (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                 (if (eq window-system 'w32) ".exe" "") trustfile)))
  (setq gnutls-verify-error t)
  (setq gnutls-trustfiles (list trustfile)))

;; Load package and set archives
(require 'package)

(setq package-enable-at-startup nil)

(setq package-archives '(("org-elpa"     . "http://orgmode.org/elpa/")
			 ("gnu"          . "https://elpa.gnu.org/packages/")
			 ("melpa"        . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("marmalade"    . "https://marmalade-repo.org/packages/")
                         )
     package-archive-priorities '(("melpa-stable" . 1)))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents) ; updage packages archive
  (package-install 'use-package)) ; and install the most recent version of use-package

(require 'use-package)
(require 'json)

;; add custom config files to load path
(add-to-list 'load-path "~/.emacs.d/custom")

;; ==============================
;; Basic config
;; ==============================

;; improve list packages
(use-package paradox
  :ensure t
  :diminish (paradox-menu-mode . "Paradox")
  :config
  (paradox-enable))

;; creamsody theme
(use-package creamsody-theme
  :ensure t)

(use-package darktooth-theme
  :ensure t
  :config
  (load-theme 'darktooth t)
  (darktooth-modeline))

;; load source code pro font
;; (set-frame-font "Source Code Pro 11")
;; this is not loaded when running as daemon since there are
;; no frames when the init file is evaluated

;; Stolen from somewhere
(defun alist-insert (alist key value)
  "Returns a new alist with (key,value) inserted into it."
  (let ((newlist nil) (found nil) k v)
	(dolist (e alist)
	  (setq k (car e))
	  (setq v (cdr e))
	  (if (equal k key)
		  (progn (unless found (setq newlist (cons (cons k value) newlist)))
				 (setq found t))
		  (setq newlist (cons (cons k v) newlist))))
	(unless found
	  (setq newlist (cons (cons key value) newlist)))
	(reverse newlist)))

(setq default-frame-alist
      (alist-insert default-frame-alist 'font "Source Code Pro 11"))

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

(use-package all-the-icons-ivy
  :ensure t
  :pin melpa-stable
  :after (all-the-icons ivy counsel)
  :config
  (all-the-icons-ivy-setup))

(use-package rainbow-delimiters
  :ensure t
  :pin melpa-stable
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package pretty-lambdada
  :ensure t
  :config
  (pretty-lambda-for-modes))

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

;; resize windows
(global-set-key (kbd "C-S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-S-<down>") 'shrink-window)
(global-set-key (kbd "C-S-<up>") 'enlarge-window)

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
(set-register ?g (cons 'file "~/.rvm/gems/ruby-2.3.0/gems"))

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
  :commands try)

(use-package eshell
  :init
  (setenv "PAGER" "cat")
  (setenv "PATH"
          (concat
           "/home/timmillar/.rvm/bin:/home/timmillar/.nvm/versions/node/v4.4.7/bin:/home/timmillar/.local/bin:/home/timmillar/anaconda2/bin:"
           (getenv "PATH")))
  (setq eshell-visual-commands
        '("less" "tmux" "htop" "top" "bash" "zsh"
          "fish" "ssh" "tail" "vi" "more"))
  (setq eshell-visual-subcommands
        '("git" ("log" "diff" "show"))))

(use-package eshell-git-prompt
  :ensure t
  :config
  (eshell-git-prompt-use-theme 'powerline))

;; eshell popups
(defun eshell-here ()
  "Opens up a new shell in the directory associated with the current buffer's file.
The eshell is renamed to match that directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))
    (insert (concat "ls"))
    (eshell-send-input)))

(defun eshell/cdp ()
  "Change directory to the project's root."
  (eshell/cd (projectile-project-root)))

(defun eshell/d (&rest args)
  "Opens ARGS in dired mode."
  (dired (pop args) "."))

(defun tm/eshell-quit-or-delete-char (arg)
  "Quits the eshell or deletes ARG (a char)."
  (interactive "p")
  (if (and (eolp) (looking-back eshell-prompt-regexp))
      (progn
        (eshell-life-is-too-much) ; Why not? (eshell/exit)
        (delete-window))
    (delete-forward-char arg)))

;; define aliases
(add-hook 'eshell-mode-hook (lambda ()

    (eshell/alias "ec" "find-file $1")
    (eshell/alias "ff" "find-file $1")
    (eshell/alias "emacs" "find-file $1")
    (eshell/alias "oo" "find-file-other-window $1")
    (eshell/alias "ll" "ls -AlohG")
    (eshell/alias "d" "dired $1")

    (eshell/alias "be" "bundle exec")
    (eshell/alias "befs" "bundle exec foreman start")

    (eshell/alias "gco" "git checkout $1")
    (eshell/alias "gbv" "git branch -vv")
    (eshell/alias "gfa" "git fetch --all")
    (eshell/alias "gfb" "gfa && gbv")

    (eshell/alias "gd" "magit-diff-unstaged")
    (eshell/alias "gds" "magit-diff-staged")
    (eshell/alias "gst" "magit-status")
    (eshell/alias "gl" "magit-log-current") ; why does this not work?
    (define-key eshell-mode-map (kbd "C-d")
      'tm/eshell-quit-or-delete-char)))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package tramp)

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :commands undo-tree-visualise)

(use-package undohist
  :ensure t
  :init
  (customize-set-variable 'udohist-directory "~/.emacs.d/.emacs-undo")
  :config
  (undohist-initialize))

(use-package ibuffer-vc
  :ensure t
  :pin melpa-stable
  :init
  (setq ibuffer-formats
      '((mark modified read-only vc-status-mini " "
              (name 18 18 :left :elide)
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              (vc-status 16 16 :left)
              " "
              filename-and-process)))
  :config
  (add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (add-to-list 'ibuffer-never-show-predicates "^\\*helm")
            (add-to-list 'ibuffer-never-show-predicates "^\\*magit")
            (setq ibuffer-show-empty-filter-groups nil)
            (ibuffer-vc-set-filter-groups-by-vc-root)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic)))))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

;; ==============================
;; Custom packages
;; ==============================

(use-package setup-utils)

;; ==============================
;; dired
;; ==============================

;; allow dired to be able to delete or copy a whole dir.
(setq dired-recursive-copies (quote always)) ; “always” means no asking
(setq dired-recursive-deletes (quote top)) ; “top” means ask once
(setq dired-dwim-target t)
(setq dired-use-ls-dired nil) ; macos doesn't support ls --dired

(use-package async
  :ensure t
  :config
  (dired-async-mode 1)
  (async-bytecomp-package-mode 1))

(use-package dired-details
  :ensure t
  :config
  (dired-details-install)
  (setq dired-details-hidden-string ""))

;; (use-package dired-details+
;;   :ensure t
;;   :after (dired dired-details))

(use-package docker
  :ensure t
  :pin melpa-stable
  :diminish docker
  :config
  (docker-global-mode))

;; ==============================
;; UI Config
;; ==============================

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init
  (which-key-mode)
  :bind
  (("<f5>" . which-key-show-top-level))
  :config
  (which-key-setup-side-window-bottom)
  (setq which-key-sort-order 'which-key-key-order-alpha
	which-key-side-window-max-width 0.33
	which-key-idle-delay 0.75))

(use-package avy
  :ensure t
  :pin melpa-stable
  :bind
  (("C-'" . avy-goto-char-2)
   ("C-#" . avy-goto-char-timer))
  :config
  (avy-setup-default))

(use-package ivy
  :ensure t
  :diminish (ivy-mode . "")
  :pin melpa-stable
  :init (ivy-mode 1)
  :bind
  (:map ivy-minibuffer-map
	("C-;" . ivy-avy)
	("C-j" . ivy-next-line)
	("C-k" . ivy-previous-line)
	("M-j" . ivy-next-history-element)
	("M-k" . ivy-previous-history-element))
  :config
  (setq enable-recursive-minibuffers t)
  (setq ivy-use-virtual-buffers t)   ; extend searching to bookmarks
  (setq ivy-virtual-abbreviate "full")
  (setq ivy-height 15)               ; set height of the ivy window
  (setq ivy-count-format "(%d/%d) ") ; count format, from the ivy help page
  (ivy-set-occur 'counsel-git-grep 'counsel-git-grep-occur)
  (ivy-set-occur 'swiper 'swiper-occur)
  )

(use-package hydra
  :ensure t
  :pin melpa-stable)

(use-package ivy-hydra
  :ensure t
  :pin melpa-stable
  :after (ivy hydra))

(use-package ace-window
  :ensure t
  :defer t
  :pin melpa-stable
  :bind
  (("C-x o" . ace-window))
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package swiper
  :ensure t
  :pin melpa-stable)

(use-package counsel
  :ensure t
  :pin melpa-stable
  :bind
  (:map read-expression-map
	("C-r" . counsel-expression-history))
  :config
  (setq recentf-max-saved-items 500))

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
  :pin melpa-stable
  :init
  (setq projectile-switch-project-action 'projectile-dired)
  :config
  (setq projectile-completion-system 'ivy)
  (setq projectile-mode-line '(:eval (format " [%s]" (projectile-project-name))))
  (projectile-mode))

(use-package counsel-projectile
  :ensure t
  :after projectile
  :config
  (counsel-projectile-on))

;; (use-package persp-mode
;;   :ensure t
;;   :pin melpa-stable
;;   :defer t
;;   :config
;;   (persp-mode 1))

;; (use-package persp-projectile
;;   :ensure t
;;   :after (persp-mode projectile-mode)
;;   :pin melpa-stable
;;   :defer t)

(use-package eyebrowse
  :ensure t
  :pin melpa-stable
  :bind
  ("C-c C-w 0" . eyebrowse-switch-to-window-config-0)
  :config
  (eyebrowse-mode t))

(use-package projectile-rails
  :ensure t
  :pin melpa-stable
  :diminish projectile-rails-mode
  :after projectile
  :bind
  (:map projectile-rails-mode-map
        ("M-r" . hydra-projectile-rails/body))
  :config
  (projectile-rails-global-mode))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (global-flycheck-mode))

(use-package evil
  :ensure t
  :config
  (setq evil-disable-insert-state-bindings t)
  (evil-set-initial-state 'eshell-mode 'emacs)
  (evil-set-initial-state 'inf-ruby-mode 'emacs)
  (evil-set-initial-state 'commint-mode 'normal)
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

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-avy
  :ensure t
  :after (evil avy)
  :config
  (evil-avy-mode 1))

;; (use-package evil-collection
;;   :ensure t
;;   :after evil
;;   :init
;;   (evil-collection-init))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :init
  ;; (setq smartparens-strict-mode nil)
  :config
  (use-package smartparens-config)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))

(use-package evil-smartparens
  :ensure t
  :diminish evil-smartparens-mode
  :after (evil smartparens)
  :config
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

(use-package treemacs
  :ensure t
  :defer t
  :config
  (progn
    (use-package treemacs-evil
      :ensure t
      :demand t)
    (setq treemacs-follow-after-init          t
          treemacs-width                      35
          treemacs-indentation                2
          treemacs-collapse-dirs              (if (executable-find "python") 3 0)
          treemacs-silent-refresh             nil
          treemacs-change-root-without-asking nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-show-hidden-files          t
          treemacs-never-persist              nil
          treemacs-is-never-other-window      nil
          treemacs-goto-tag-strategy          'refetch-index)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    ;; (pcase (cons (not (null (executable-find "git")))
    ;;              (not (null (executable-find "python"))))
    ;;   (`(t . t)
    ;;    (treemacs-git-mode 'extended))
    ;;   (`(t . _)
    ;;    (treemacs-git-mode 'simple)))
    )
  :bind
  (:map global-map
        ([f6]         . treemacs-toggle)
        ("M-0"        . treemacs-select-window)
        ("C-c 1"      . treemacs-delete-other-windows)
        ))

(use-package treemacs-projectile
  :defer t
  :ensure t
  :after (treemacs projectile)
  :config
  (setq treemacs-header-function #'treemacs-projectile-create-header))

(use-package magit
  :ensure t
  :pin melpa-stable
  :init
  (setq magit-repository-directories
       '(("~/Code/hiring-hub/" . 1) ("~/Code/moi/" . 2) ("~/.emacs.d" . 0)))
  (setq magit-refs-local-branch-format "%4c %-25n %h %U%m\n")
  (setq magit-completing-read-function 'ivy-completing-read)
  :config
  (add-to-list 'magit-repolist-columns
               '("Unpulled" 25 magit-repolist-column-unpulled-from-upstream nil)  1)
  (add-to-list 'magit-repolist-columns
               '("Unpushed" 25 magit-repolist-column-unpushed-to-upstream nil) 1))

(use-package evil-magit
  :ensure t
  :after (evil magit)
  :pin melpa-stable
  :config
  (setq evil-magit-state 'normal))

(use-package magithub
  :ensure t
  :after magit
  :init
  (defalias 'ghub-request 'ghub--request)
  (defvar ghub-extra-headers nil)
  :config
  (magithub-feature-autoinject t))

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

(use-package git-messenger
  :ensure t
  :defer t
  :pin melpa-stable
  :init
  (setq git-messenger:use-magit-popup t))

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
   "bS" 'save-some-buffers
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
   "dD" 'dired-other-window
   "dj" 'dired-jump
   "dJ" 'dired-jump-other-window

   "g"  '(:ignore t :which-key "git")
   "gs" 'magit-status
   "gg" 'counsel-git-grep
   "gf" 'counsel-git
   "gt" 'git-timemachine-toggle
   "gb" 'mo-git-blame-current
   "gm" 'magit-dispatch-popup
   "gS" 'magit-stage-file
   "gU" 'magit-unstage-file
   "gc" '(git-messenger:popup-message :which-key "git message")

   "c"  '(:ignore t :which-key "counsel")
   "cs" '(swiper :which-key "swiper")
   "cm" '(swiper-multi :which-key "swiper-multi")
   "cc" '(ivy-resume :which-key "ivy resume")
   "ca" '(counsel-ag :which-key "ag")
   "cd" '(counsel-dpkg :which-key "dpkg")
   "ci" '(counsel-imenu :which-key "imenu")
   "cl" '(counsel-locate :which-key "locate")
   "ct" '(counsel-load-theme :which-key "themes")
   "cx" '(counsel-linux-app :which-key "linux-apps")
   "cy" '(counsel-yank-pop :which-key "yank-pop")

   "c"  '(:ignore t :which-key "counsel")
   "cs" '(swiper :which-key "swiper")
   "cm" '(swiper-multi :which-key "swiper-multi")
   "cc" '(ivy-resume :which-key "ivy resume")
   "ca" '(counsel-ag :which-key "ag")
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
   "pg" '(projectile-find-file-dwim :which-key "find-file-dwim")
   "pi" '(projectile-ibuffer :which-key "iBuffer")
   "pj" '(projectile-find-tag :which-key "find-tag")
   "po" '(projectile-multi-occur :which-key "multi-occur")
   "pp" '(counsel-projectile-find-file :which-key "find-file")
   "pf" '(counsel-projectile-switch-project :which-key "switch-project")
   "pq" '(counsel-projectile-switch-open-project :which-key "switch-open-project")
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
   "r!b" '(projectile-rails-dbconsole :which-key "db console")
   "r!c" '(projectile-rails-console :which-key "rails console")
   "r!d" '(projectile-rails-destroy :which-key "rails destroy")
   "r!g" '(projectile-rails-generate :which-key "rails generate")
   "r!r" '(projectile-rails-rake :which-key "rails rake")
   "r!s" '(projectile-rails-server :which-key "rails server")

   "rg"  '(:ignore t :which-key "goto")
   "rgd" '(projectile-rails-goto-schema :which-key "goto-schema")
   "rgf" '(projectile-rails-goto-file-at-point :which-key "goto-file-at-point")
   "rgg" '(projectile-rails-goto-gemfile :which-key "goto-gemfile")
   "rgh" '(projectile-rails-goto-spec-helper :which-key "goto-spec-helper")
   "rgr" '(projectile-rails-goto-routes :which-key "goto-routes")
   "rgs" '(projectile-rails-goto-seeds :which-key "goto-seed")

   "ra" '(projectile-rails-find-locale :which-key "find-locale")
   "rb" '(projectile-rails-find-job :which-key "find job")
   "rc" '(projectile-rails-find-controller :which-key "find-controller")
   "rC" '(projectile-rails-find-current-controller :which-key "find-current-controller")
   "rk" '(projectile-rails-find-rake :which-key "find rake")
   "rl" '(projectile-rails-find-lib :which-key "find lib")
   "ri" '(projectile-rails-find-initializer :which-key "find initializer")
   "rm" '(projectile-rails-find-model :which-key "find-model")
   "rM" '(projectile-rails-find-current-model :which-key "find-current-model")
   "rn" '(projectile-rails-find-migration :which-key "find migration")
   "rN" '(projectile-rails-find-current-migration :which-key "find current migration")
   "ro" '(projectile-rails-find-log :which-key "find log")
   "rp" '(projectile-rails-find-spec :which-key "find-spec")
   "rP" '(projectile-rails-find-current-spec :which-key "find-current-spec")
   "rr" '(projectile-rails-console :which-key "console")
   "rR" '(projectile-rails-server :which-key "server")
   "rv" '(projectile-rails-find-view :which-key "find-view")
   "rV" '(projectile-rails-find-current-view :which-key "find-current-view")

   "ft" 'treemacs-toggle
   "fT" 'treemacs
   "fB" 'treemacs-bookmark
   "f C-t" 'treemacs-find-file
   "f M-t" 'treemacs-find-tag
   "fp" 'treemacs-projectile-toggle
   "fP" 'treemacs-projectile

   "xx" '(eshell-here :which-key "eshell-here")
   ))

;; ==============================
;; org mode
;; ==============================

(use-package org
  :ensure t
  :pin org-elpa
  :init
  (setq org-hide-emphasis-markers t)
  (setq org-src-fontify-natively t)
  (bind-key "C-c c" 'org-capture)
  :config
  (font-lock-add-keywords 'org-mode
                        '(("^ +\\([-*]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (setq org-capture-templates
        '(("t" "Agenda Todo" entry
           (file+headline "~/Org/todo.org" "Agenda")
           "\n\n** TODO %?\n%T\n\n%i\n%a\n\n\n"
           :empty-lines 1)

          ("n" "Agenda Notes" entry
           (file+headline "~/Org/notes.org" "Notes")
           "\n\n** %?\n%T\n%i\n%a\n\n\n"
           :empty-lines 1)

          ("l" "Link" entry
           (file+headline "~/Org/links.org" "Links")
           "* %? %^L %^g \n%T"
           :prepend t)

          ("j" "Journal" entry
           (file+datetree "~/Org/journal.org")
           "* %?\n\nEntered on %U\n  %i\n  %a"
           :prepend t)))
  )

(use-package org-bullets
  :ensure t
  :after org
  :pin melpa-stable
  :config
  (add-hook 'org-mode-hook
            (lambda ()
              (org-bullets-mode 1))))

; ==============================
;; Language Support
;; ==============================

(use-package ruby-mode
  :ensure t
  ; :mode
  ; (("\\.rb\\" . ruby-mode) ("\\.ru\\" . ruby-mode)
  ;  ("\\.rake\\" . ruby-mode) ("Gemfile" . ruby-mode))
  :init
  (setq ruby-insert-encoding-magic-comment nil)
  (setq ruby-deep-indent-paren nil)
  ;; (eval-after-load 'projectile
  ;;   '(setq rake-completion-system projectile-completion-system))
  :config
  (add-hook 'ruby-mode-hook 'whitespace-cleanup)
  )

(use-package inf-ruby
  :ensure t
  :defer t
  :after ruby-mode)

(use-package ruby-tools
  :ensure t
  :diminish ruby-tools-mode
  :after ruby-mode)

(use-package ruby-hash-syntax
  :ensure t
  :bind (("C->" . ruby-toggle-hash-syntax)))

(use-package robe
  :ensure t
  :after ruby-mode
  :pin melpa-stable
  :diminish robe-mode
  :config
  (add-hook 'ruby-mode-hook 'robe-mode)
  (add-hook 'ruby-mode-hook 'eldoc-mode)
  (defadvice inf-ruby-console-auto
      (before activate-rvm-for-robe activate)
    (rvm-activate-corresponding-ruby)))

(use-package web-mode
  :ensure t
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

(use-package ruby-refactor
  :ensure t
  :config
  (add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch))

(use-package yaml-mode
  :ensure t
  :defer t)

(use-package rspec-mode
  :ensure t
  :diminish rspec-mode
  :after ruby-mode
  :config
  (rspec-install-snippets)
  (add-hook 'dired-mode-hook 'rspec-dired-mode))

(use-package bundler
  :ensure t)

(use-package feature-mode
  :ensure t
  :defer t)

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

;; (use-package js2-mode
;;   :ensure t
;;   :pin melpa-stable
;;   :mode
;;   (("\\.js\\'" . js2-mode))
;;   :init
;;   (setq-default js2-basic-offset 2)
;;   (setq-default js2-indent-level 2))

;; (use-package rjsx-mode
;;   :ensure t
;;   :pin melpa-stable)

;; (use-package skewer-mode
;;   :ensure t
;;   :pin melpa-stable
;;   :bind
;;   (("C-x C-e" . skewer-eval-last-expression)
;;    ("C-M-x" . skewer-eval-defun)
;;    ("C-c C-k" . skewer-load-buffer))
;;   :config
;;   (skewer-setup))

;; (use-package erlang
;;   :ensure t
;;   :defer t
;;   :init
;;   (setq erlang-indent-level 2)
;;   :config
;;   (use-package erlang-start))

;; (use-package anaconda-mode
;;   :ensure t
;;   :defer t
;;   :diminish anaconda-mode
;;   :init
;;   (setq python-shell-interpreter "/home/timmillar/anaconda2/bin/ipython"
;; 	python-shell-interpreter-args "--simple-prompt -i")
;;   :config
;;   (add-hook 'python-mode-hook 'anaconda-mode)
;;   (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

;; (use-package virtualenvwrapper
;;   :ensure t
;;   :defer t
;;   :pin melpa-stable
;;   :init
;;   (setq venv-location "~/.tensorflow/")
;;   :config
;;   (venv-initialize-interactive-shells)
;;   (venv-initialize-eshell))

;; (use-package ensime
;;   :ensure t
;;   :defer t
;;   :pin melpa-stable)

;; (use-package haskell-mode
;;   :ensure t
;;   :defer t
;;   :pin melpa-stable
;;   :init
;;   (defvar haskell-font-lock-symbols)
;;   (setq haskell-font-lock-symbols t
;;         haskell-process-type 'stack-ghci
;;         haskell-process-path-ghci "stack"
;;         haskell-compile-cabal-build-command "cd %s stack build --ghc-option=-ferror-spans")
;;   :config
;;   (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
;;   (add-hook 'haskell-mode-hook '(lambda ()
;;                                   (setq haskell-indentation-mode t))))

;; (use-package intero
;;   :ensure t
;;   :defer t
;;   :pin melpa-stable
;;   :config
;;   (add-hook 'haskell-mode-hook 'intero-mode))

;; (use-package racket-mode
;;   :ensure t
;;   :defer t)

;; ==============================
;; Tidy-up modeline
;; ==============================

(use-package diminish
  :ensure t
  :config
  (diminish 'auto-revert-mode)
  (diminish 'eldoc-mode)
  (diminish 'compilation-shell-minor-mode)
  (diminish 'rspec-mode)
  (diminish 'yas-minor-mode)
  (diminish 'hideshow)
  (diminish 'smerge-mode)
  (diminish 'abbrev-mode)
  (diminish 'evil-org)
  (diminish 'ruby-tools)
  (diminish 'docker-mode)
  (diminish 'docker-global-mode)
  (diminish 'docker))

;; hmmmmm ....

(use-package evil-org ;; erroring?
  :ensure t
  :after (org evil)
  :diminish evil-org
  :pin melpa
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme))))

(org-babel-do-load-languages ; could be where problem with init is introduced
 'org-babel-load-languages
 '(
   (sh . t)
   (python . t)
   (R . t)
   (ruby . t)
   (ditaa . t)
   (dot . t)
   (octave . t)
   (scheme . t)
   (sqlite . t)
   (sql . t)
   (makefile . t)
   (perl . t)
   (C . t)
   (gnuplot . t)
   (latex . t)
   (lisp . t)
   (emacs-lisp . t)
   (java . t)
   (scala . t)
   (haskell . t)
   ))

;; ==============================
;; auto-generated config
;; ==============================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(package-selected-packages
   (quote
    (evil-surround evil-avy sicp treemacs-evil treemacs-projectile evil-collection ruby-refactor magithub yasnippet yaml-mode which-key web-mode use-package undohist try sbt-mode rvm ruby-tools ruby-hash-syntax rspec-mode robe rainbow-delimiters projectile-rails pretty-lambdada paradox org-bullets mo-git-blame markdown-mode ivy-hydra ibuffer-vc haskell-mode git-timemachine git-messenger general flycheck feature-mode faceup eyebrowse exec-path-from-shell evil-smartparens evil-org evil-nerd-commenter evil-magit evil-escape eshell-git-prompt dumb-jump docker dired-details darktooth-theme creamsody-theme counsel-projectile counsel-gtags company bundler anaconda-mode all-the-icons-ivy all-the-icons-dired ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Possible cause of errors in init file
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
