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

;; winner mode
(winner-mode 1)
;; windmove
(windmove-default-keybindings)
(setq windmove-wrap-around t)

;; no tabs
(setq-default indent-tabs-mode nil)

(require 'tramp)

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
        which-key-idle-delay 0.5))

(use-package ivy
  :ensure t
  :diminish (ivy-mode . "") ; does not display ivy in the modeline
  :init (ivy-mode 1)        ; enable ivy globally at startup
  :bind (:map ivy-mode-map  ; bind in the ivy buffer
         ("C-'" . ivy-avy)) ; C-' to ivy-avy
  :config
  (setq ivy-use-virtual-buffers t)   ; extend searching to bookmarks and …
  (setq ivy-height 20)               ; set height of the ivy window
  (setq ivy-count-format "(%d/%d) ") ; count format, from the ivy help page
  )

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

   "TAB" '(switch-to-other-buffer :which-key "prev-buffer")
   "b" '(:ignore t :which-key "buffers")
   "bb" 'switch-to-buffer
   "f" '(:ignore t :which-key "files")
   "ff" 'ido-find-file
   "d" '(:ignore t :which-key "directories")
   "dd" 'ido-dired
   "g" '(:ignore t :which-key "git")
   "gs" 'magit-status
   ))
