(tool-bar-mode -1)
(menu-bar-mode -1)
(global-display-line-numbers-mode +1)
(text-scale-increase 3)

(ido-mode +1)
(ido-everywhere +1)

;; Set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
;;(package-refresh-contents)
(require 'use-package-ensure)


(setq inhibit-startup-screen t)
(setq use-package-always-ensure t)
(setq-default display-line-numbers-type 'relative)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq visible-bell t)
(setq completions-format 'one-column)
(setq completion-styles '(flex substring initials partial-completion))

;;; Evil
(use-package evil
  :ensure t
  :config
  (evil-mode +1))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(setq my/monofont "FixedsysExcelsior Nerd Font"
      my/varfont "Monocraft Nerd Font")

;;; Fonts
(create-fontset-from-fontset-spec
 (font-xlfd-name
  (font-spec
   :name my/monofont :foundry "POOP" :size 20
   :registry "fontset-sg")))

(defun my/do-fonts () "fonts"
       (display-line-numbers-mode 1)
       (setq display-line-numbers 'relative)
       (global-display-fill-column-indicator-mode 1)
       (setq use-default-font-for-symbols t)
       (setq default-frame-alist '((font . "fontset-sg"))))

;; THESE settings dont work in the server context.
(add-hook 'server-after-make-frame-hook
     	  #'my/do-fonts)
(my/do-fonts)

(use-package corfu
  :config
 ;; Enable auto completion and configure quitting
  (global-corfu-mode)
  (setq corfu-auto t
	corfu-quit-no-match 'separator))
(setq tab-always-indent 'complete)

;;; Theme
(load-theme 'leuven t)

;;; Magit
(use-package magit)

;;; envrc
(use-package envrc
  :config
  (envrc-global-mode))

;;; Vertico
(use-package vertico
  :init
  (vertico-mode)
  :bind (:map vertico-map
	      ("C-j" . vertico-next)))

(use-package savehist
  :init
  (savehist-mode))

