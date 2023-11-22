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
     ((:application tramp)
      tramp-connection-local-default-system-profile
      tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((tramp-flatpak-connection-local-default-profile
      (tramp-remote-path "/app/bin" tramp-default-remote-path "/bin"
                         "/usr/bin" "/sbin" "/usr/sbin"
                         "/usr/local/bin" "/usr/local/sbin"
                         "/local/bin" "/local/freeware/bin"
                         "/local/gnu/bin" "/usr/freeware/bin"
                         "/usr/pkg/bin" "/usr/contrib/bin" "/opt/bin"
                         "/opt/sbin" "/opt/local/bin"))
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
      (tramp-remote-path "/app/bin" tramp-default-remote-path "/bin"
                         "/usr/bin" "/sbin" "/usr/sbin"
                         "/usr/local/bin" "/usr/local/sbin"
                         "/local/bin" "/local/freeware/bin"
                         "/local/gnu/bin" "/usr/freeware/bin"
                         "/usr/pkg/bin" "/usr/contrib/bin" "/opt/bin"
                         "/opt/sbin" "/opt/local/bin"))
     (eshell-connection-default-profile (eshell-path-env-list))
     (tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o"
                                        "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "-o" "state=abcde" "-o"
                                        "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format (pid . number)
                                          (euid . number)
                                          (user . string)
                                          (egid . number) (comm . 52)
                                          (state . 5) (ppid . number)
                                          (pgrp . number)
                                          (sess . number)
                                          (ttname . string)
                                          (tpgid . number)
                                          (minflt . number)
                                          (majflt . number)
                                          (time . tramp-ps-time)
                                          (pri . number)
                                          (nice . number)
                                          (vsize . number)
                                          (rss . number)
                                          (etime . tramp-ps-time)
                                          (pcpu . number)
                                          (pmem . number) (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o"
                                        "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "-o" "stat=abcde" "-o"
                                        "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format (pid . number)
                                          (user . string)
                                          (group . string) (comm . 52)
                                          (state . 5) (ppid . number)
                                          (pgrp . number)
                                          (ttname . string)
                                          (time . tramp-ps-time)
                                          (nice . number)
                                          (etime . tramp-ps-time)
                                          (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o"
                                        "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "-o"
                                        "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format (pid . number)
                                          (euid . number)
                                          (user . string)
                                          (egid . number)
                                          (group . string) (comm . 52)
                                          (state . string)
                                          (ppid . number)
                                          (pgrp . number)
                                          (sess . number)
                                          (ttname . string)
                                          (tpgid . number)
                                          (minflt . number)
                                          (majflt . number)
                                          (time . tramp-ps-time)
                                          (pri . number)
                                          (nice . number)
                                          (vsize . number)
                                          (rss . number)
                                          (etime . number)
                                          (pcpu . number)
                                          (pmem . number) (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh") (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":") (null-device . "/dev/null"))))
 '(display-line-numbers 'relative)
 '(fsharp-smart-indentation nil)
 '(global-undo-tree-mode t)
 '(global-whitespace-mode t)
 '(global-whitespace-newline-mode nil)
 '(initial-scratch-message "Scratch Bufferino lol\12")
 '(mail-sources '((maildir :path "~/Maildir/")))
 '(make-backup-files nil)
 '(mu4e-bookmarks
   '((:name "Unread messages" :query
            "maildir:/gmail/Inbox AND flag:unread AND NOT flag:trashed"
            :key 117)
     (:name "Today's messages" :query "date:today..now" :key 116)
     (:name "Last 7 days" :query "date:7d..now" :hide-unread t :key
            119)
     (:name "Messages with images" :query "mime:image/*" :key 112)))
 '(notmuch-saved-searches
   '((:name "inbox" :query "tag:inbox" :key [105] :sort-order
            newest-first)
     (:name "unread" :query "tag:unread" :key [117] :sort-order
            newest-first)
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
 '(org-agenda-todo-ignore-with-date t)
 '(org-deadline-warning-days 3)
 '(package-selected-packages
   '(xcscope p4_16-mode p4lang-mode quelpa-use-package vagrant-tramp p4
             eglot-java go-mode all-the-icons evil-surround
             merlin-company editorconfig dune tuareg typescript-mode
             lsp-mode yasnippet vertico tree-sitter-langs svelte-mode
             pdf-tools web-mode ox-rss eglot evil company hydra
             dap-mode consult-eglot eglot-fsharp plantuml-mode
             fsharp-mode undo-tree slime rainbow-delimiters
             highlight-indentation zig-mode use-package emmet-mode
             auto-package-update spinner writeroom-mode nyan-mode
             which-key envrc org-roam marginalia clang-format
             org-contrib lsp-ui auctex gnuplot org-bullets
             emacsql-sqlite3 evil-org frames-only-mode ac-geiser
             nix-mode yaml-mode orderless dockerfile-mode
             command-log-mode general fira-code-mode projectile
             rust-mode yasnippet-snippets evil-nerd-commenter emojify
             deft counsel smartparens minions notmuch eterm-256color
             company-box magit geiser-guile consult evil-collection
             wc-mode org-present evil-paredit lsp-tailwindcss
             company-shell doom-themes))
 '(plantuml-jar-path
   "/nix/store/yzjplznj8n02c789npiflcyzs45q3kri-plantuml-1.2022.5/lib/plantuml.jar")
 '(safe-local-variable-values '((plantuml-jar-path concat (getenv "UMLPATH"))))
 '(send-mail-function 'sendmail-send-it)
 '(sp-show-pair-from-inside t)
 '(tab-stop-list '(2 4 6 8))
 '(tab-width 4)
 '(undo-tree-auto-save-history nil)
 '(warning-suppress-log-types '((use-package)))
 '(whitespace-global-modes nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bold ((t (:foreground "red" :overline nil :weight bold :height 1.2))))
 '(italic ((t (:underline t :slant oblique :weight normal :height 1.3 :width extra-expanded :family "default"))))
 '(org-verbatim ((t (:foreground "#50fa7b" :height 1.3))))
 '(whitespace-newline ((t (:foreground "#565761" :weight ultra-light :height 0.2)))))
