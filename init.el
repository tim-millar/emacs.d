;; ==============================
;; Configure setup
;; ==============================

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
(use-package creamsody-theme :ensure t :defer t)
(load-theme 'creamsody t)
(creamsody-modeline)

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

;; backups
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) )
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) )

;; winner mode
(winner-mode 1)
;; windmove
(windmove-default-keybindings)
(setq windmove-wrap-around t)

;; no tabs
(setq-default indent-tabs-mode nil)

(require 'tramp)

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

  (setq ivy-use-virtual-buffers t)   ; extend searching to bookmarks and …
  (setq ivy-height 20)               ; set height of the ivy window
  (setq ivy-count-format "(%d/%d) ") ; count format, from the ivy help page
  )

(use-package counsel
  :ensure t)

(use-package evil
  :ensure t
  :config
  (evil-mode 1))

(use-package magit
  :ensure t)

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode))

(use-package general
  :ensure t
  :config
  (general-evil-setup t)
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

   "SPC" '(counsel-M-x :which-key "M-x")

   "h" '(:ignore t :which-key "help")
   "ha" 'apropos-command
   "hd" 'apropos-documentation
   "hi" 'info
   "hk" 'describe-key
   "hf" 'describe-function
   "hv" 'describe-variable
   "hb" 'describe-bindings
   "hm" 'describe-mode
   "hP" 'describe-package
   "hr" 'info-emacs-manual
   "hs" 'describe-syntax

   "b" '(:ignore t :which-key "buffers")
   "bb" 'ivy-switch-buffer
   "ss" 'save-buffer
   "bo" 'ivy-switch-buffer-other-window

   "f" '(:ignore t :which-key "files")
   "ff" 'counsel-find-file
   "fo" 'find-file-other-window
   "fl" 'counsel-find-library

   "d" '(:ignore t :which-key "directories")
   "dd" 'ido-dired

   "g" '(:ignore t :which-key "git")
   "gs" 'magit-status
   "gg" 'counsel-git-grep
   ))

;; ==============================
;; auto-generated config
;; ==============================

(put 'dired-find-alternate-file 'disabled nil)
