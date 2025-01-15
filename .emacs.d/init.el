(tool-bar-mode -1)
(menu-bar-mode -1)
(global-display-line-numbers-mode +1)
(text-scale-set 0.5)

(add-to-list 'auth-sources "~/.authinfo.gpg")

(setq ispell-alternate-dictionary
 "/home/sohamg/.nix-profile/lib/aspell/en_GB-ise.multi")

;; Garbage Collection
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(ido-mode -1)
(ido-everywhere -1)
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)
(setq-default confirm-nonexistent-file-or-buffer nil)

(bind-key (kbd "C-c y") #'yank-from-kill-ring)

(bind-key (kbd "C-k") #'(lambda () (kill-buffer nil)))

;; Set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
;;(package-refresh-contents)
(require 'use-package-ensure)

;; Add extensions
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-ts-mode))

(add-hook 'eshell-mode-hook #'compilation-shell-minor-mode)

(setq inhibit-startup-screen t)
(setq use-package-always-ensure t)
(setq-default display-line-numbers-type 'relative)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq visible-bell t)
(setq completions-format 'one-column)
;;(setq completion-styles '(flex substring initials partial-completion))


(setq backup-directory-alist '((".*" (expand-file-name
				      "trash" user-emacs-directory))))

(setq-default fill-column 80)

;;; Theme
(load-theme 'leuven t)

;;; Evil
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil
	evil-want-integration t)
  :config
  (evil-set-initial-state 'calc-mode 'emacs) 
  (add-to-list 'evil-emacs-state-modes 'calc-mode)
  (add-to-list 'evil-emacs-state-modes 'gnus-group-mode)
  (evil-mode +1))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Evil in more places
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :bind
  ("C-c ;" . evilnc-comment-or-uncomment-lines))


;;; Fonts
(setq my/monofont "FixedsysExcelsior Nerd Font"
      my/varfont "Monocraft Nerd Font")
(create-fontset-from-fontset-spec
 (font-xlfd-name
  (font-spec
   :name my/monofont :foundry "POOP" :size 20
   :registry "fontset-sg")))

(set-fontset-font "fontset-sg" 'emoji
                  (font-spec :family "Noto Color Emoji" :foundry "NONE"))

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

;;; New age completion only.
(use-package corfu
  :config
 ;; Enable auto completion and configure quitting
  (global-corfu-mode)
  (setq tab-always-indent 'complete)
  (setq corfu-auto t
	corfu-quit-no-match 'separator))

;; (use-package company
;;   :config
;;   (global-company-mode))

(use-package cape
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative keys: M-p, M-+, ...
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  ;; ...
  :config
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-noninterruptible)
)

;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  ;; (orderless-style-dispatchers '(orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))


;;; Magit
(use-package magit
  :bind ("C-c m" . magit))

;;; envrc
(use-package envrc
  :config
  (envrc-global-mode))

;;; Vertico
(use-package vertico
  :init
  (vertico-mode)
  :bind (:map vertico-map
	      ("C-j" . vertico-next)
	      ("C-k" . vertico-previous)))

(use-package savehist
  :init
  (savehist-mode))

(use-package org
  :bind
  (("C-c a" . org-agenda)
  ("C-c t" . org-capture))
  :config
  (setq org-latex-pdf-process
        (list
	 "latexmk -auxdir=%o/.aux -f -pdflua %f -output-directory=%o"))
  (setq org-directory (expand-file-name "~/Nextcloud/Notes")
        org-default-notes-file (concat org-directory "/main.org")
        org-capture-templates
        '(
	  ("n" "Note" entry
	   (file+headline org-default-notes-file "Master Notes")
           "* %^{TITLE|untitled} %^g %i\n %T \n %?\n")
          ("t" "Todo" entry
	   (file org-default-notes-file)
           "* TODO %^{TITLE|untitled} %^g %i\n %T \n %?\n"
	   :tree-type month))
        org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "IN-PROG(i)" "|" "DONE(d)" "WONT-DO(w@)" "LIMBO(l)"))
        org-id-link-to-org-use-id t
        org-refile-targets '((nil :maxlevel . 3))
        org-agenda-files (list org-default-notes-file))
    (setq org-todo-keyword-faces
     '(("TODO" . org-warning)
       ("IN-PROG" . "green")
       ("DONE" . "black")
       ("NEXT" . "yellow")
       ("LIMBO" . "brown"))))

(use-package evil-org
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-contrib)


(use-package auctex)
(use-package pdf-tools
  :config
  ;; Note to future self
  ;; This is probably better installed thru nix
  ;; as it attempts to compile a C program.
  ;; MAYBE add shell.nix to emacs directory.
  (pdf-tools-install)
  (add-hook 'pdf-view-mode-hook #'(lambda nil (setq-local display-line-numbers nil))))

(use-package org-roam
  :custom
  (org-roam-completion-everywhere t)
  (org-roam-graph-executable "~/.nix-profile/bin/dot")
  (org-roam-directory "~/Nextcloud/org-roam/")
  (org-roam-graph-viewer nil)
  (org-roam-capture-templates '(("d" "default" plain "%?"
                                 :target (file+head "roam_${slug}.org.gpg"
                                                    "#+title: ${title}\n %<%T %d>\n")
                                 :unnarrowed t)))
  :bind (("C-c o l" . org-roam-buffer-toggle)
         ("C-c o f" . org-roam-node-find)
         ("C-c o i" . org-roam-node-insert))
  :bind-keymap ("C-c o d" . org-roam-dailies-map)

  :config
  (require 'org-roam-dailies)
  (setq org-roam-dailies-directory "daily/")
  (setq org-id-link-to-org-use-id 'create-if-interactive)
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?"
           :target (file+head "%<%Y-%m-%d>.org.gpg"
                              "#+title: Journal for %<%Y-%m-%d> %<%a>\n"))))
  (org-roam-db-autosync-enable))




(use-package calc
  :config
  (add-hook 'calc-mode-hook #'turn-off-evil-mode)
  :defer t)
(use-package eglot
  :ensure nil
  :bind (("C-c e i" . eglot-format-buffer)
	 ("C-c e a" . eglot-code-actions))
  :config
  (fset #'jsonrpc--log-event #'ignore))

;; (use-package casual-calc
;;   :ensure t
;;   :bind (:map
;;          calc-mode-map
;;          ("C-o" . casual-calc-tmenu)
;;          :map
;;          calc-alg-map
;;          ("C-o" . casual-calc-tmenu))
;;   :config
;;   (add-hook 'calc-mode-hook #'turn-off-evil-mode nil)
;;   :after (calc))

;; (use-package oauth2
;;   :ensure nil
;;   :load-path "/home/sohamg/work/elpa/packages/oauth2/")

(load-file "/home/sohamg/work/elpa/packages/oauth2/oauth2.el")

(setq gnus-select-method
      '(nnimap "imap.gmail.com"
        (nnimap-inbox "INBOX")
        (nnimap-split-methods default)
        (nnimap-expunge t)
        (nnimap-stream ssl)
	(nnimap-authenticator xoauth2)))

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes '("letter" "\\documentclass{letter}")))

(setq ispell-aspell-data-dir nil
      ispell-library-directory nil
      ispell-extra-args '("--dict-dir=/home/sohamg/.nix-profile/lib/aspell")
      ispell-dictionary "british")

;; GDB
(setq-default gdb-many-windows t
      gdb-speedbar-auto-raise t)

(use-package markdown-mode)

(use-package gptel
  :custom
  (gptel-default-mode 'org-mode)
  :config
  (gptel-make-anthropic "Claude Haiku"
    :stream t
    :key (funcall (plist-get (nth 0 (auth-source-search :host "anthropic.com")) :secret)))
  :bind (("C-c g g" . gptel)
         ("C-c g s" . gptel-send)
         ("C-c g a" . gptel-add)))

(use-package nix-ts-mode)

(use-package lsp-mode
  :commands lsp
  :config
  (add-hook 'c-mode-hook 'lsp)
  (add-hook 'c++-mode-hook 'lsp)
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  :init
  (setq lsp-log-io nil) ; if set to true can cause a performance hit
  (setq lsp-use-plists t)
  (setq lsp-clangd-binary-path (expand-file-name "/home/sohamg/.nix-profile/bin/clangd"))
  (setq lsp-keymap-prefix "C-c l"))
(use-package lsp-ui)
(use-package flycheck)
(use-package which-key
  :config
  (which-key-mode))


(when (boundp 'org-latex-classes)
  (add-to-list 'org-latex-classes
	     '("mla"
	       "\\documentclass[12pt,letterpaper]{article}

\\ifdefined\\pdfpagewidth
  % Already defined; no action needed
\\else
  % Define for LuaTeX compatibility
  \\let\\pdfpagewidth\\pagewidth
  \\let\\pdfpageheight\\pageheight
\\fi
\\usepackage{ifpdf}
\\usepackage{mla}
[DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
	        ("\\section{%s}" . "\\section*{%s}")
  ("\\subsection{%s}" . "\\subsection*{%s}")
  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
  ("\\paragraph{%s}" . "\\paragraph*{%s}")
  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
)))

(use-package parinfer-rust-mode)


;; Local Variables:
;; no-byte-compile: t
;; End:

