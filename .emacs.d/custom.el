(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-list '(("emacs" ("") "emacsclient")))
 '(TeX-view-program-selection
   '(((output-dvi has-no-display-manager) "dvi2tty")
     ((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi")
     (output-pdf "PDF Tools") (output-html "xdg-open")))
 '(column-number-mode t)
 '(connection-local-criteria-alist
   '(((:application tramp :protocol "kubernetes")
      tramp-kubernetes-connection-local-default-profile)
     ((:machine "sohamg.xyz") sohamg.xyz-vars)
     ((:application tramp :protocol "flatpak")
      tramp-container-connection-local-default-flatpak-profile
      tramp-flatpak-connection-local-default-profile)
     ((:application eshell) eshell-connection-default-profile)
     ((:application tramp) tramp-connection-local-default-system-profile
      tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((tramp-flatpak-connection-local-default-profile
      (tramp-remote-path "/app/bin" tramp-default-remote-path "/bin" "/usr/bin"
                         "/sbin" "/usr/sbin" "/usr/local/bin" "/usr/local/sbin"
                         "/local/bin" "/local/freeware/bin" "/local/gnu/bin"
                         "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin"
                         "/opt/bin" "/opt/sbin" "/opt/local/bin"))
     (tramp-kubernetes-connection-local-default-profile
      (tramp-config-check . tramp-kubernetes--current-context-data)
      (tramp-extra-expand-args 97
                               (tramp-kubernetes--container
                                (car tramp-current-connection))
                               104
                               (tramp-kubernetes--pod
                                (car tramp-current-connection))
                               120
                               (tramp-kubernetes--context-namespace
                                (car tramp-current-connection))))
     (sohamg.xyz-vars (company-gtags--executable-connection))
     (tramp-container-connection-local-default-flatpak-profile
      (tramp-remote-path "/app/bin" tramp-default-remote-path "/bin" "/usr/bin"
                         "/sbin" "/usr/sbin" "/usr/local/bin" "/usr/local/sbin"
                         "/local/bin" "/local/freeware/bin" "/local/gnu/bin"
                         "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin"
                         "/opt/bin" "/opt/sbin" "/opt/local/bin"))
     (eshell-connection-default-profile (eshell-path-env-list))
     (tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o"
                                        "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "-o" "state=abcde" "-o"
                                        "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format (pid . number) (euid . number)
                                          (user . string) (egid . number)
                                          (comm . 52) (state . 5)
                                          (ppid . number) (pgrp . number)
                                          (sess . number) (ttname . string)
                                          (tpgid . number) (minflt . number)
                                          (majflt . number)
                                          (time . tramp-ps-time) (pri . number)
                                          (nice . number) (vsize . number)
                                          (rss . number) (etime . tramp-ps-time)
                                          (pcpu . number) (pmem . number) (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o"
                                        "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "-o" "stat=abcde" "-o"
                                        "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format (pid . number) (user . string)
                                          (group . string) (comm . 52)
                                          (state . 5) (ppid . number)
                                          (pgrp . number) (ttname . string)
                                          (time . tramp-ps-time) (nice . number)
                                          (etime . tramp-ps-time) (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o"
                                        "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "-o"
                                        "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format (pid . number) (euid . number)
                                          (user . string) (egid . number)
                                          (group . string) (comm . 52)
                                          (state . string) (ppid . number)
                                          (pgrp . number) (sess . number)
                                          (ttname . string) (tpgid . number)
                                          (minflt . number) (majflt . number)
                                          (time . tramp-ps-time) (pri . number)
                                          (nice . number) (vsize . number)
                                          (rss . number) (etime . number)
                                          (pcpu . number) (pmem . number) (args)))
     (tramp-connection-local-default-shell-profile (shell-file-name . "/bin/sh")
                                                   (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile (path-separator . ":")
                                                    (null-device . "/dev/null"))))
 '(custom-safe-themes
   '("10e5d4cc0f67ed5cafac0f4252093d2119ee8b8cb449e7053273453c1a1eb7cc"
     "ffafb0e9f63935183713b204c11d22225008559fa62133a69848835f4f4a758c"
     "7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184"
     "691d671429fa6c6d73098fc6ff05d4a14a323ea0a18787daeb93fde0e48ab18b"
     "c5878086e65614424a84ad5c758b07e9edcf4c513e08a1c5b1533f313d1b17f1"
     "7964b513f8a2bb14803e717e0ac0123f100fb92160dcf4a467f530868ebaae3e"
     "4e2e42e9306813763e2e62f115da71b485458a36e8b4c24e17a2168c45c9cf9d"
     "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69"
     "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098" default))
 '(display-line-numbers 'relative)
 '(epg-pinentry-mode 'loopback)
 '(fill-column 80)
 '(font-lock-global-modes '(not))
 '(fsharp-smart-indentation nil)
 '(geiser-racket-binary
   "/nix/store/5mjkn702qn4ij16awlchndkibjn0kdb9-racket-8.9/bin/racket")
 '(global-undo-tree-mode t)
 '(global-whitespace-mode t)
 '(global-whitespace-newline-mode nil)
 '(guix-guile-program "/home/sohamg/.nix-profile/bin/guile")
 '(guix-repl-startup-time 50000)
 '(highlight-indent-guides-method 'bitmap)
 '(initial-scratch-message "Scratch Bufferino lol\12")
 '(ispell-alternate-dictionary "/home/sohamg/.nix-profile/lib/aspell/en_US.multi")
 '(mail-sources '((maildir :path "~/Maildir/")))
 '(make-backup-files nil)
 '(mu4e-bookmarks
   '((:name "Unread messages" :query
            "maildir:/gmail/Inbox AND flag:unread AND NOT flag:trashed" :key 117)
     (:name "Today's messages" :query "date:today..now" :key 116)
     (:name "Last 7 days" :query "date:7d..now" :hide-unread t :key 119)
     (:name "Messages with images" :query "mime:image/*" :key 112)))
 '(notmuch-saved-searches
   '((:name "inbox" :query "tag:inbox" :key [105] :sort-order newest-first)
     (:name "unread" :query "tag:unread" :key [117] :sort-order newest-first)
     (:name "flagged" :query "tag:flagged" :key [102])
     (:name "sent" :query "tag:sent" :key [116])
     (:name "drafts" :query "tag:draft" :key [100])
     (:name "all mail" :query "*" :key [97])))
 '(org-agenda-custom-commands
   '(("n" "Agenda and all TODOs" ((agenda "" nil) (alltodo "" nil)) nil)
     ("x" "Custom Agenda" alltodo ""
      ((org-agenda-overriding-header "Priorities")
       (org-agenda-sorting-strategy
        '(scheduled-up deadline-up todo-state-down time-up))
       (org-agenda-prefix-format "  %?T%?l%?:t%10s%i")
       (org-overriding-columns-format "%25ITEM %TODO")))))
 '(org-agenda-files
   '("~/Nextcloud/Notes/fall2024classes.org"
     "/home/sohamg/Nextcloud/Notes/main.org"))
 '(org-agenda-todo-ignore-with-date t)
 '(org-deadline-warning-days 3)
 '(package-selected-packages nil)
 '(plantuml-jar-path
   "/nix/store/yzjplznj8n02c789npiflcyzs45q3kri-plantuml-1.2022.5/lib/plantuml.jar")
 '(safe-local-variable-directories
   '("/home/sohamg/Nextcloud/org-roam/daily/" "/home/sohamg/Nextcloud/org-roam/"
     "/home/sohamg/.emacs.d/straight/repos/org-roam/"))
 '(safe-local-variable-values
   '((geiser-scheme-implementation . "guix repl")
     (eval progn (require 'lisp-mode)
           (defun emacs27-lisp-fill-paragraph (&optional justify)
             (interactive "P")
             (or (fill-comment-paragraph justify)
                 (let
                     ((paragraph-start
                       (concat paragraph-start
                               "\\|\\s-*\\([(;\"]\\|\\s-:\\|`(\\|#'(\\)"))
                      (paragraph-separate
                       (concat paragraph-separate "\\|\\s-*\".*[,\\.]$"))
                      (fill-column
                       (if
                           (and (integerp emacs-lisp-docstring-fill-column)
                                (derived-mode-p 'emacs-lisp-mode))
                           emacs-lisp-docstring-fill-column
                         fill-column)))
                   (fill-paragraph justify))
                 t))
           (setq-local fill-paragraph-function #'emacs27-lisp-fill-paragraph))
     (eval modify-syntax-entry 43 "'") (eval modify-syntax-entry 36 "'")
     (eval modify-syntax-entry 126 "'") (geiser-repl-per-project-p . t)
     (eval with-eval-after-load 'yasnippet
           (let
               ((guix-yasnippets
                 (expand-file-name "etc/snippets/yas"
                                   (locate-dominating-file default-directory
                                                           ".dir-locals.el"))))
             (unless (member guix-yasnippets yas-snippet-dirs)
               (add-to-list 'yas-snippet-dirs guix-yasnippets) (yas-reload-all))))
     (eval setq-local guix-directory
           (locate-dominating-file default-directory ".dir-locals.el"))
     (eval add-to-list 'completion-ignored-extensions ".go")
     (plantuml-jar-path concat (getenv "UMLPATH"))))
 '(send-mail-function 'sendmail-send-it)
 '(sp-show-pair-from-inside t)
 '(tab-stop-list '(2 4 6 8))
 '(tab-width 4)
 '(undo-tree-auto-save-history nil)
 '(warning-suppress-log-types '((emacs) (use-package)))
 '(whitespace-global-modes nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
