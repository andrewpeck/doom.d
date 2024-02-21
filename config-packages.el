;;; config-packages.el -*- lexical-binding: t; -*-

;;------------------------------------------------------------------------------ 
;; Delight
;;------------------------------------------------------------------------------

(use-package! delight
  :defer-incrementally t
  :config
  (delight '+org-pretty-mode         " 🌻"          "org")
  (delight 'better-jumper-local-mode ""             "better-jumper")
  (delight 'company-mode             ""             "company")
  (delight 'dtrt-indent-mode         ""             "dtrt-indent")
  (delight 'eldoc-mode               ""             "eldoc")
  (delight 'evil-escape-mode         ""             "evil-escape")
  (delight 'evil-goggles-mode        ""             "evil-goggles")
  (delight 'evil-org-mode            ""             "evil-org")
  (delight 'evil-snipe-local-mode    ""             "evil-snipe")
  (delight 'evil-traces-mode         ""             "evil-traces")
  (delight 'evil-traces-mode         ""             "evil-traces")
  (delight 'gcmh-mode                ""             "gcmh")
  (delight 'hi-lock-mode             ""             "hi-lock")
  (delight 'lispy-mode               ""             "lispy")
  (delight 'lispyville-mode          ""             "lispyville")
  (delight 'org-indent-mode          ""             "org-indent")
  (delight 'outline-minor-mode       ""             "outline")
  (delight 'persp-mode               "view"         "perspective-mode")
  (delight 'projectile-mode          ""             "projectile")
  (delight 'subword-mode             ""             "subword")
  (delight 'undo-tree-mode           ""             "undo-tree")
  (delight 'which-key-mode           ""             "which-key")
  (delight 'whitespace-mode          ""             "whitespace")
  (delight 'ws-butler-mode           ""             "ws-butler")
  (delight 'yas-minor-mode           ""             "yasnippet")
  (delight 'evil-markdown-mode       ""             "evil-markdown")
  (delight 'format-all-mode          ""             "format-all")
  (delight 'markdown-mode            "Md"           "markdown")
  (delight 'poly-markdown-mode       " Pm"          "poly-markdown")
  (delight 'magit-gitflow-mode       ""             "magit-gitflow")
  (delight 'git-gutter-mode          ""             "git-gutter")
  (delight 'rainbow-mode             " 🌈"          "rainbow-mode")
  ;; (delight 'emacs-lisp-mode          " λ"           "emacs-lisp-mode")
  ;; (delight 'a-mode                   ""             "a")
  ;; (delight 'a-mode                   ""             "a")
  ;; (delight 'a-mode                   ""             "a")
  ;; (delight 'a-mode                   ""             "a")
  ;; (delight 'a-mode                   ""             "a")
  ;; (delight 'a-mode                   ""             "a")
  )

;;------------------------------------------------------------------------------
;; Git Gutter
;;------------------------------------------------------------------------------

(use-package! diff-hl
  :init
  (setq diff-hl-global-modes '(not image-mode org-mode markdown-mode))
  (remove-hook! 'find-file-hook #'diff-hl-mode)
  (remove-hook! 'find-file-hook #'diff-hl-update-once)
  (diff-hl--global-turn-on))

;;------------------------------------------------------------------------------
;; Treesitter
;;------------------------------------------------------------------------------

(use-package! treesit-auto)

;;------------------------------------------------------------------------------
;; Copyright
;;------------------------------------------------------------------------------

(use-package! copyright

  :config

  (setq copyright-names-regexp "Andrew Peck")
  (setq copyright-year-ranges t)

  (add-hook! 'before-save-hook
    (defun hook/update-copyright ()
      "Automatically update copyright on save."

      (save-excursion
        (when (and copyright-names-regexp
                   (progn (goto-char (point-min))
                          (copyright-re-search copyright-names-regexp)))
          (copyright-update nil t)
          (copyright-fix-years))))))

;;------------------------------------------------------------------------------
;; Wavedrom
;;------------------------------------------------------------------------------

(use-package! ob-wavedrom
  :config
  (setq ob-wavedrom-cli-path "wavedrom"))

;;------------------------------------------------------------------------------ 
;; Apheleia
;;------------------------------------------------------------------------------

(use-package! apheleia
  :config

  ;; (add-to-list 'apheleia-formatters '(isort "isort"  "-ca" "--stdout" "-"))
  ;; (add-to-list 'apheleia-mode-alist '(python-mode autopep8))
  ;; (add-to-list 'apheleia-mode-alist '(python-ts-mode autopep8))

  (add-to-list 'apheleia-formatters '(autopep8 "autopep8" "-"))
  (add-to-list 'apheleia-formatters '(isort "isort"  "-ca" "--stdout" "-"))

  (add-to-list 'apheleia-mode-alist '(python-mode autopep8))
  (add-to-list 'apheleia-mode-alist '(python-ts-mode autopep8))


  ;; (add-hook! 'python-mode-hook (apheleia-mode))

  )

;;------------------------------------------------------------------------------
;; PDF View + Image Mode
;;------------------------------------------------------------------------------

(use-package! pdf-view

  :defer-incrementally t

  :init

  (add-hook! 'pdf-view-mode-hook #'auto-revert-mode)

  :config

  (evil-define-key '(motion normal) pdf-view-mode-map
    (kbd "q") #'kill-current-buffer)

  (defun pdf-rotate (dir)
    "Rotate a pdf using Qpdf. Dir should either be + or -"
    (if (and (stringp dir) (or (string= "+" dir) (string= "-" dir)))
        (if (string= "pdf" (file-name-extension (buffer-file-name)))
            (shell-command (format "qpdf --rotate=%s90 --replace-input %s" dir (buffer-file-name)))
          (message (format "File %s is not a pdf" (buffer-file-name))))
      (message (format "Direction \'%s\' is not valid. Please use a direction \'+\' or \'-\'." dir))))

  (defun pdf-rotate-clockwise ()
    "Rotate a pdf clockwise using Qpdf"
    (interactive)
    (pdf-rotate "+"))

  (defun pdf-rotate-counterclockwise ()
    "Rotate a pdf counterclockwise using Qpdf"
    (interactive)
    (pdf-rotate "-")))

(use-package! image-mode
  :config
  (add-hook! 'image-mode-hook #'auto-revert-mode))

;;------------------------------------------------------------------------------
;; Affe
;;------------------------------------------------------------------------------

(use-package! affe

  :config

  ;; (setq affe-find-command "rg --color=never --files")
  (setq affe-find-command "fd --color=never -L")

  (defun affe-find-home    () (interactive) (affe-find "~/"))
  (defun affe-find-work    () (interactive) (affe-find "~/work"))
  (defun affe-find-project () (interactive) (affe-find (projectile-project-root)))
  (defun affe-grep-project () (interactive) (affe-grep (projectile-project-root)))
  (defun affe-find-notes   () (interactive) (affe-find "~/notes"))
  (defun affe-find-dotfile () (interactive) (affe-find "~/.dotfiles"))

  ;; Affe
  (after! evil-maps
    (evil-define-key '(motion normal) 'global
      (kbd "C-o")   #'affe-find-home
      (kbd "C-y")   #'affe-find-work
      (kbd "C-p")   #'affe-find-project
      (kbd "C-S-p") #'affe-grep-project
      (kbd "C-n")   #'affe-find-notes
      (kbd "C-n")   #'affe-find-notes)))

;;------------------------------------------------------------------------------
;; Awk
;;------------------------------------------------------------------------------

(add-hook! 'awk-mode-hook
  (setq-local  comment-start "# "))

;;------------------------------------------------------------------------------
;; Backups
;;------------------------------------------------------------------------------ 

(use-package! backup-each-save

  :config

  (setq backup-each-save-mirror-location
        (expand-file-name "~/emacs-backups"))
  (when (not (file-directory-p backup-each-save-mirror-location))
    (make-directory backup-each-save-mirror-location))

  (add-hook 'after-save-hook #'backup-each-save))

;;------------------------------------------------------------------------------
;; Dired
;;------------------------------------------------------------------------------

(use-package! diredfl
  :config
  (add-to-list 'diredfl-compressed-extensions ".zst"))

(use-package! dired-aux
  :config
  (setq dired-compress-file-default-suffix ".zst")
  (setq dired-compress-directory-default-suffix ".tar.zst"))

(use-package! dired-x

  :after dired

  :config

  (setq dired-omit-extensions (remove ".bin" dired-omit-extensions))
  (setq dired-omit-extensions (remove ".bit" dired-omit-extensions))

  (add-to-list 'dired-omit-extensions ".tmp")

  (setq dired-omit-files
        (concat dired-omit-files
                "\\|vivado.*\\.jou\\'"
                "\\|vivado.*\\.backup\\.log\\'"
                "\\|vivado\\.log\\'"
                "\\|^\\.Xil\\'"
                "\\|^\\.mypy_cache\\'"
                "\\|^__pycache__\\'"
                "\\|^\\.pytest_cache\\'")))

(use-package! dired
  :config

  ;; better dired soring
  (setq dired-listing-switches "-a1vBhl  --group-directories-first")

  (defun +dired/quit-all ()
    "Kill all `dired-mode' buffers."
    (interactive)
    (mapc #'kill-buffer (doom-buffers-in-mode 'dired-mode))
    (message "Killed all dired buffers"))

  (map! :map dired-mode-map
        ;; Kill all dired buffers on q
        :ng "q" #'+dired/quit-all
        ;; To be consistent with ivy/helm+wgrep integration
        "C-c C-e" #'wdired-change-to-wdired-mode)

  :init

  ;; this breaks sensible dired sorting on remote hosts;
  ;; it is there for DOS compatibility which I don't care about
  (remove-hook! 'dired-mode-hook
    #'+dired-disable-gnu-ls-flags-maybe-h)

  (remove-hook! 'dired-mode-hook
    #'diff-hl-dired-mode)

  (add-hook 'dired-mode-hook
            #'diff-hl-dired-mode-unless-remote)

  (add-hook 'dired-mode-hook
            #'dired-omit-mode)

  (add-hook! 'dired-mode-hook
    (defun hook/dired-hide-details ()
      "Hide details by default in dired to reduce line noise."
      (dired-hide-details-mode 1)))

  (add-hook! 'dired-mode-hook
    (defun hook/enable-dired-git-filter ()
      ""
      (unless (file-remote-p default-directory)
        (when (locate-dominating-file "." ".git")
          (dired-filter-mode)
          (dired-filter-by-git-ignored)))))

  (add-hook! 'dired-after-readin-hook
    (defun hook/dired-git-info-mode ()
      "Enable dired git info on local files."
      (unless (file-remote-p default-directory)
         (dired-git-info-auto-enable))))

  ;; Stolen from doom: Disable the prompt about whether I want to kill the Dired
  ;; buffer for a deleted directory. Of course I do!
  (setq dired-clean-confirm-killing-deleted-buffers nil)

  (defun my/dired-open()
    (interactive)
    (start-process (concat "dired-open-" (uuidgen-1)) nil
                   "setsid"
                   "xdg-open"
                   (dired-get-file-for-visit))))

;;------------------------------------------------------------------------------
;; DWIM Shell
;;------------------------------------------------------------------------------

(use-package! dwim-shell-command
  :after dired
  :config
  (defun my/dwim-shell-command-archive-zstd ()
    "Tar marked files as zst"
    (interactive)
    (dwim-shell-command-on-marked-files
     "Archive as zstd"
     "tar -cavf '<<fne>>.tar.zst' '<<f>>'"
     :utils "tar"))

  (defun my/dwim-shell-command-archive-gz ()
    "Tar marked files as gz"
    (interactive)
    (dwim-shell-command-on-marked-files
     "Archive as zstd"
     "tar -cavf '<<fne>>.tar.gz' '<<f>>'"
     :utils "tar"))

  (defun my/dwim-shell-command-extract ()
    "Extract tar archive at point."
    (interactive)
    (dwim-shell-command-on-marked-files
     "Unarchive."
     "tar -xavf '<<f>>'"
     :utils "tar")))

;;------------------------------------------------------------------------------
;; Undo
;;------------------------------------------------------------------------------

(use-package! undo-fu-session
  ;; persistent undo
  :after undo-fu
  :config
  (setq undo-fu-session-directory (concat doom-user-dir ".undo-fu")))

(use-package! undo-fu
  :config
  ;; disable confusing undo-fu behavior
  ;; https://codeberg.org/ideasman42/emacs-undo-fu/issues/6
  (setq undo-fu-ignore-keyboard-quit t))

;;------------------------------------------------------------------------------
;; Whitespace
;;------------------------------------------------------------------------------

(use-package! ws-butler
  :defer-incrementally t
  :config
  (setq ws-butler-global-exempt-modes
        '(special-mode comint-mode term-mode eshell-mode)))

;;------------------------------------------------------------------------------
;; Highlight Todos
;;------------------------------------------------------------------------------

(use-package! hl-todo
  :defer-incrementally t
  :config
  (setq global-hl-todo-mode t))

;;------------------------------------------------------------------------------
;; Savehist
;;------------------------------------------------------------------------------

(use-package! savehist
  ;; save macros and other registers peristently
  :config
  (add-to-list 'savehist-additional-variables 'register-alist)
  (add-hook! 'savehist-save-hook
    (defun doom-clean-up-registers-h ()
      (setq-local register-alist
                  (cl-remove-if-not #'savehist-printable register-alist)))))

;;------------------------------------------------------------------------------
;; Tramp
;;------------------------------------------------------------------------------

(use-package! tramp
  :defer-incrementally t
  :config

  (setq tramp-ssh-controlmaster-options "-o ControlMaster=auto"
        tramp-use-ssh-controlmaster-options t
        tramp-histfile-override "~/.tramp_history"
        tramp-inline-compress-start-size 1000
        tramp-copy-size-limit 10000
        vc-handled-backends '(Git)
        tramp-verbose 1
        tramp-default-method "ssh")

  ;; Another way to find the remote path is to use the path assigned to the remote user by the
  ;; remote host. TRAMP does not normally retain this remote path after login. However,
  ;; tramp-own-remote-path preserves the path value, which can be used to update tramp-remote-path.
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;;------------------------------------------------------------------------------
;; Projectile
;;------------------------------------------------------------------------------

(use-package! projectile
  :defer-incrementally t
  :config

  ;; Due to a very obnoxious bug it seems that if I am on a host system where
  ;; the fd executable is fdfind but connecting to a remote-system where fd is
  ;; found as fd, emacs will try to execute fdfind on the remote system;
  ;;
  ;; should report this
  ;; (setq projectile-fd-executable "fdfind")
  (add-hook! 'find-file-hook
    (when (file-remote-p default-directory)
      (setq-local projectile-git-use-fd nil)))

  (setq projectile-sort-order 'recently-active))

;;------------------------------------------------------------------------------
;; Yasnippet
;;------------------------------------------------------------------------------

(use-package! yasnippet
  :defer-incrementally t
  :config
  ;; Don't add newlines to snippet endings
  (setq yas-also-auto-indent-first-line t)
  (add-hook 'snippet-mode-hook
            (lambda () (setq require-final-newline nil))))

;;------------------------------------------------------------------------------
;; Ispell
;;------------------------------------------------------------------------------

(use-package! ispell
  :config
  ;; Save user defined words to the dictionary
  (setq ispell-personal-dictionary "~/.aspell.en.pws")
  (defun my-save-word ()
    (interactive)
    (let ((current-location (point))
          (word (flyspell-get-word)))
      (when (consp word)
        (flyspell-do-correct 'save nil
                             (car word) current-location (cadr word)
                             (caddr word) current-location)))))

;;------------------------------------------------------------------------------
;; Jinx
;;------------------------------------------------------------------------------

;; (add-hook! 'org-mode-hook #'jinx-mode)
;; (add-hook! 'markdown-mode-hook #'jinx-mode)

;;------------------------------------------------------------------------------
;; LaTex
;; LaTeX-mode-hook is used by AUCTeX's LaTeX mode.
;; latex-mode-hook is used by Emacs' built-in latex mode.
;;------------------------------------------------------------------------------

(use-package! tex-fold
  :defer-incrementally t
  :config
  ;; https://emacs.stackexchange.com/questions/33663/in-auctex-how-could-i-fold-acronyms
  (setq TeX-fold-macro-spec-list
        (append TeX-fold-macro-spec-list
                '(("{1}" ("gls"))         ; used in l0mdt
                  ("{1}" ("cite"))        ; used in l0mdt
                  ("{1}" ("yearsago"))    ; used in resume
                  ("{1}" ("heading")))))) ; used in resume

(use-package! tex
  :defer-incrementally t
  :config

  (setq reftex-toc-split-windows-horizontally t
        reftex-toc-split-windows-fraction 0.15
        TeX-master nil
        +latex-viewers '(okular atril evince zathura)
        TeX-fold-auto t)

  (add-hook! 'LaTeX-mode-hook
    (TeX-fold-mode 1)
    (reftex-mode 1)
    (variable-pitch-mode 1)

    ;; (make-variable-buffer-local 'font-lock-type-face)
    ;; (set-face-attribute 'font-lock-type-face nil
    ;;                     :inherit 'default
    ;;                     :family "Courier New"
    ;;                     :height 120)

    ;; https://www.flannaghan.com/2013/01/11/tex-fold-mode
    (add-hook! 'find-file-hook :local (TeX-fold-region (point-min) (point-max)))
    (add-hook! 'write-contents-functions :local (TeX-fold-region (point-min) (point-max)))
    ;; (add-hook! 'after-change-functions :local 'TeX-fold-paragraph)

    (flycheck-add-next-checker 'proselint 'tex-chktex))

  ;; Semantic Linefeeds
  ;;------------------------------------------------------------------------------
  ;; https://abizjak.github.io/emacs/2016/03/06/latex-fill-paragraph.html
  ;; TODO: check for common things at end of line:
  ;; c.f. e.g. i.e.

  (defun ap/line-fill-paragraph (&optional P)
    "When called with prefix argument call `fill-paragraph'.
   Otherwise split the current paragraph into one sentence per line."
    (interactive "P")
    (if (not P)
        (save-excursion
          (let ((fill-column 12345678)) ;; relies on dynamic binding
            (fill-paragraph) ;; this will not work correctly if the paragraph is
            ;; longer than 12345678 characters (in which case the
            ;; file must be at least 12MB long. This is unlikely.)
            (let ((end (save-excursion
                         (forward-paragraph 1)
                         (backward-sentence)
                         (point-marker))))  ;; remember where to stop
              (beginning-of-line)
              (while (progn (forward-sentence)
                            (<= (point) (marker-position end)))
                (just-one-space) ;; leaves only one space, point is after it
                (delete-char -1) ;; delete the space
                (newline)        ;; and insert a newline
                (evil-indent-line (line-beginning-position) (line-end-position))))))

      ;; otherwise do ordinary fill paragraph
      (fill-paragraph P)))

  (defun tex-bold ()
    "Make the current TeX selection bold."
    (interactive)
    (TeX-font nil 2))

  (defun tex-italic ()
    "Make the current TeX selection italic."
    (interactive)
    (TeX-font nil 9))

  (defun tex-tt ()
    "Make the current TeX selection italic."
    (interactive)
    (TeX-font nil 20))

  ;; Electric Space
  ;;------------------------------------------------------------------------------

  (defun electric-space ()              ; Trying to get Emacs to do semantic linefeeds
    (interactive)
    (if (looking-back (sentence-end) nil)
        (insert "\n")
      (self-insert-command 1)))

  (defvar electric-space-on-p nil)

  (defun toggle-electric-space ()
    (interactive)
    (global-set-key
     " "
     (if (setq electric-space-on-p
               (not electric-space-on-p))
         'electric-space
       'self-insert-command))))

;;------------------------------------------------------------------------------
;; Elfeed
;;------------------------------------------------------------------------------ 

(use-package! elfeed

  :defer-incrementally t

  :config

  ;; Run `elfeed-update' every 8 hours
  (run-at-time nil (* 8 60 60) #'elfeed-update)

  (setq elfeed-feeds
        '("https://www.evalapply.org/index.xml"
          ;; "https://hackaday.com/blog/feed/"
          "https://nullprogram.com/feed/"
          "https://bzg.fr/index.xml"
          "https://www.mattblaze.org/blog/rss20.xml"
          "https://jackrusher.com/feed.xml"
          "http://mbork.pl/?action=rss;days=30;all=0;showedit=0"
          "https://watchguy.co.uk/feed/"
          "https://sachachua.com/blog/feed/")))

;;------------------------------------------------------------------------------
;; Eglot
;;------------------------------------------------------------------------------

(use-package! eglot

  :defer-incrementally t

  :config

  (setq eglot-prefer-plaintext t
        eglot-autoshutdown t
        ;; eglot-events-buffer-size 0
        eldoc-echo-area-prefer-doc-buffer t
        eldoc-echo-area-use-multiline-p t
        help-at-pt-display-when-idle t)
  


  ;; (add-hook! python-mode-hook
  ;;   (setq eglot-workspace-configuration
  ;;         '((pyright
  ;;            (plugins
  ;;             (mccabe (enabled . t))    ; Remove this if you want mccabe.
  ;;             (pycodestyle (enabled . nil))
  ;;             (flake8 (enabled . t)))))))

  ;; (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
  ;; (add-to-list 'eglot-server-programs '(python-ts-mode . ("pyright-langserver" "--stdio")))

                                        ; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=61373
  ;; pyright generates html :(
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=61373

  ;; (dolist (provider '(:hoverProvider :documentHighlightProvider))
  ;;   (add-to-list 'eglot-ignored-server-capabilities provider))


  ;; (add-to-list 'eglot-workspace-configuration
  ;;              '(:svlangserver (:settings (:systemverilog.launchConfiguration: "verilator -sv -Wall --lint-only",
  ;;                                          :systemverilog.formatCommand: "verible-verilog-format"))))

  ;; (add-to-list 'eglot-server-programs
  ;;              '(verilog-mode . ("svls")))

  ;; (add-hook! verilog-mode-hook
  ;;   (setq eglot-workspace-configuration
  ;;         `((:systemverilog
  ;;            (:includeIndexing '["**/*.{sv,svh}"])
  ;;            (:excludeIndexing '["test/**/*.{sv,svh}"])
  ;;            (:defines nil)
  ;;            (:launchConfiguration "verilator -sv --lint-only -Wall")
  ;;            (:lintOnUnsaved t)
  ;;            (:formatCommand "verible-verilog-format")
  ;;            (:disableCompletionProvider nil)
  ;;            (:disableHoverProvider nil)
  ;;            (:disableSignatureHelpProvider nil)
  ;;            (:disableLinting nil)))))

  (add-to-list 'eglot-server-programs
               '(vhdl-mode . ("ghdl-ls"))))

(use-package! eglot-booster
  :after eglot
  :init
  (cl-remprop 'buffer-local-value 'byte-obsolete-generalized-variable)
  :config
  (eglot-booster-mode))

;;------------------------------------------------------------------------------
;; Magit
;;------------------------------------------------------------------------------

(use-package! magit

  :defer-incrementally t

  :config

  (setq magit-log-margin
        '(t
          "%Y/%m/%d"
          magit-log-margin-width t 18))

  (setq +vc-gutter-in-remote-files t)

  ;; set vc-ignore-dir-regexp to the default emacs value; doom overwrites this to
  ;; a value that ignores any remote directories, causing git-gutter etc to not
  ;; work through tramp
  (setq vc-ignore-dir-regexp
        "\\`\\(?:[\\/][\\/][^\\/]+[\\/]\\|/\\(?:net\\|afs\\|\\.\\.\\.\\)/\\)\\'")

  ;;(magit-todos-mode)
  (setq-default magit-mode-hook nil)
  (add-hook! 'magit-diff-mode-hook
    (setq-local truncate-lines nil))
  (setq magit-repository-directories '(("~/work" . 1)))
  (setq-default magit-diff-refine-hunk 'all)

  ;; from: https://www.reddit.com/r/emacs/comments/1187mmq/can_magit_update_synce_and_init_all_submodules/
  (unless (condition-case nil
              (transient-get-suffix 'magit-submodule "U")
            (error nil)) ;; catch error if suffix does not exist, and return nil to (unless)
    (transient-define-suffix magit-submodule-update-all (args)
      "Update all submodules"
      :class 'magit--git-submodule-suffix
      :description "Update all modules    git submodule update --init [--recursive]"
      (interactive (list (magit-submodule-arguments "--recursive")))
      (magit-with-toplevel
        (magit-run-git-async "submodule" "update" "--init" args)))
    (transient-append-suffix 'magit-submodule '(2 -1) ;; add as last entry in the 3rd section
      '("U" magit-submodule-update-all))))

(use-package! browse-at-remote

  :defer-incrementally t

  :config

  (add-to-list 'browse-at-remote-remote-type-regexps
               '(:host "^gitlab\\.cern.ch$" :type "gitlab"))
  (add-to-list 'browse-at-remote-remote-type-regexps
               '(:host "^gitlab\\.psiquantum\\.com$" :type "gitlab")))

(use-package! forge

  :defer-incrementally t

  :config

  (add-to-list 'auth-sources "~/.authinfo")

  (add-to-list 'forge-alist
               '("gitlab.psiquantum.com" "gitlab.psiquantum.com/api/v4" "gitlab.psiquantum.com" forge-gitlab-repository)
               '("gitlab.cern.ch" "gitlab.cern.ch/api/v4" "gitlab.cern.ch" forge-gitlab-repository))

  (setq forge-topic-list-limit '(60 . 0)
        forge-owned-accounts '(("andrewpeck")
                               ("BU-EDF")
                               ("atlas-tdaq-phase2-l0mdt-electronics")
                               ("ucla-gaps-tof")
                               ("emu")
                               ("andrewpeck1")
                               ("apeck"))))

;;------------------------------------------------------------------------------
;; OpenSCAD
;;------------------------------------------------------------------------------

(use-package! scad-mode

  :init

  (add-hook! 'scad-mode-hook
    (defun scad--reindent-buffer-hook ()
      "Reindent buffer on save."
      (add-hook 'write-contents-functions
                #'re-indent-buffer nil t)))

  :config

  (defun open-in-openscad ()
    "Open the current buffer in openscad"
    (interactive)
    (start-process (format "*scad-%s*" (file-name-base (buffer-file-name)))
                   nil "setsid" "openscad" (buffer-file-name)))

  (define-key scad-mode-map (kbd "C-c C-p")
              'open-in-openscad)

  (defun scad-cheatshet ()
    "Open the SCAD Cheatsheet in a web browser"
    (interactive)
    (browse-url  "https://openscad.org/cheatsheet/")))

;;------------------------------------------------------------------------------
;; Verilog
;;------------------------------------------------------------------------------

(use-package! verilog-mode

  :defer-incrementally t

  :init 

  (add-hook 'verilog-mode-hook
            (defun hook/verilog-beautify-symbols-hook ()
              "Beautify Verilog Symbols"
              (setq prettify-symbols-alist
                    '(("begin" . "《")
                      ("end"   . "⟫")
                      ("function" . "󰡱")))
              (prettify-symbols-mode)))

  ;; (font-lock-add-keywords
  ;;  'verilog-mode
  ;;  '(("\\(always_ff @(posedge \\)"
  ;;     1 '(face nil display  "󰁥(pos "))))

  ;; (font-lock-add-keywords
  ;;  'verilog-mode
  ;;  '(("\\(always_ff @(negedge \\)"
  ;;     1 '(face nil display  "󰁥(neg "))))

  (add-hook 'verilog-mode-hook
            (defun hook/verilog-configure-indent-and-comment ()
              "Wrap verilog-do-indent in a save excursion so it doesn't jump around.... uhg"
              (setq-local indent-line-function
                          (lambda () (save-excursion (verilog-indent-line-relative))))
              (setq-local comment-multi-line t)))

  ;; (add-hook 'verilog-mode-hook
  ;;           (defun hook/set-fill-prefix ()
  ;;             "Set the verilog fill prefix."
  ;;             (setq-local fill-prefix "// ")))

  :config

  (defun verilog-name-to-port-inst ()
    "Convert symbol at point into a verilog port instantiation.

e.g. if you place the point at `outcome_cycle_idx_0' in the
following line and execute this function:

output reg [TBINB-1:0]       outcome_cycle_idx_0,

it will be transformed into:

.outcome_cycle_idx_0 (outcome_cycle_idx_0)


This makes for easy conversion of some port list or wire list
into Verilog ports."

    (interactive)
    (let ((name (symbol-at-point)))
      (beginning-of-line)
      (kill-line)
      (insert (format ".%s (%s)," name name))
      (verilog-indent-line)
      (re-search-forward (format "%s" name))
      (re-search-forward (format "%s" name))))

  (setq verilog-align-ifelse t
        verilog-tab-always-indent nil
        ;; verilog-align-typedef-regexp (concat "\\<" verilog-identifier-re "_\\(t\\)\\>")  ;; https://github.com/veripool/verilog-mode/issues/1823
        verilog-auto-delete-trailing-whitespace t
        verilog-auto-inst-param-value t
        verilog-indent-lists nil ;; Fix the dumb indentation inside of port lists
        verilog-auto-inst-vector nil
        verilog-auto-lineup (quote all)
        verilog-auto-newline nil
        verilog-auto-save-policy nil
        verilog-auto-template-warn-unused t
        verilog-highlight-grouping-keywords t
        verilog-highlight-modules t
        verilog-case-indent 2
        verilog-cexp-indent 2
        verilog-indent-level 2
        verilog-indent-level-behavioral 2
        verilog-indent-level-declaration 2
        verilog-indent-level-module 2
        verilog-tab-to-comment nil))



;;------------------------------------------------------------------------------
;; VHDL Mode
;;------------------------------------------------------------------------------

(use-package! vhdl-mode

  :defer-incrementally t

  :config

  (require 'er-basic-expansions)
  (require 'flycheck)

  ;; vhdl mode will wrap comments after some # of characters
  (setq vhdl-end-comment-column 200
        vhdl-standard '(08)
        vhdl-platform-spec nil
        vhdl-prompt-for-comments nil
        flycheck-ghdl-language-standard "08")

  (defun vhdl-unsigned->slv ()
    "Convert a VHDL unsigned to standard logic vector."
    (interactive)
    (when (not (region-active-p))
      (er/mark-symbol))
    (let ((sig (buffer-substring-no-properties (mark) (point))))
      (delete-region (mark) (point))
      (insert (format  "std_logic_vector(%s)" sig))
      (backward-char 1))
    (when (functionp 'evil-insert)
      (evil-insert 0)))

  (defun vhdl-int->slv ()
    "Convert a VHDL integer to standard logic vector."
    (interactive)
    (when (not (region-active-p))
      (er/mark-symbol))
    (let ((sig (buffer-substring-no-properties (mark) (point))))
      (delete-region (mark) (point))
      (insert (format  "std_logic_vector(to_unsigned(%s, ))" sig))
      (backward-char 2))
    (when (functionp 'evil-insert)
      (evil-insert 0)))

  (defun vhdl-slv->int ()
    "Convert a VHDL standard logic vector to integer."
    (interactive)
    (when (not (region-active-p))
      (er/mark-symbol))
    (let ((sig (buffer-substring-no-properties (mark) (point))))
      (delete-region (mark) (point))
      (insert (format  "to_integer(unsigned(%s))" sig))))

  (defun vhdl-slv->unsigned ()
    "Convert a VHDL standard logic vector to unsigned."
    (interactive)
    (when (not (region-active-p))
      (er/mark-symbol))
    (let ((sig (buffer-substring-no-properties (mark) (point))))
      (delete-region (mark) (point))
      (insert (format  "unsigned(%s)" sig))
      (backward-char 1))))

;;------------------------------------------------------------------------------
;; Tcl
;;------------------------------------------------------------------------------

(use-package! tcl

  :defer-incrementally t

  :config
  ;; (dolist (key vivado-builtin-list)
  ;;   (add-to-list 'tcl-builtin-list key))
  ;; (dolist (key vivado-keyword-list)
  ;;   (add-to-list 'tcl-keyword-list key))
  ;; (dolist (key vivado-constant-list)
  ;;   (add-to-list 'tcl-constant-list key))

  ;; make $ not part of a symbol in tcl-mode

  (setq-local smartparens-mode t
              auto-fill-mode nil)

  (setq tcl-help-directory-list '("/usr/share/doc/tclx"))

  (modify-syntax-entry ?$ "'" tcl-mode-syntax-table))

;;------------------------------------------------------------------------------
;; Markdown
;;------------------------------------------------------------------------------

(use-package! markdown 

  :defer-incrementally t

  :config

  (defun markdown->pdf ()
    "Export markdown to PDF with Pandoc and open."
    (interactive)
    (let* ((base (file-name-base (buffer-file-name)))
           (md (concat base ".md"))
           (pdf (concat base ".pdf")))

      (if (not (executable-find "pandoc"))
          (message "Pandoc not installed!")
        (progn
          (message (shell-command-to-string (format "pandoc %s -o %s" md pdf)))
          (if (f-file-p pdf)
              (async-shell-command (format "xdg-open %s" pdf))))))))

;;------------------------------------------------------------------------------
;; C mode
;;------------------------------------------------------------------------------

(use-package! c

  :defer-incrementally t

  :init
  ;; double slashes // instead of slash-stars /* ... */
  (add-hook! 'c-mode-common-hook
             ;; Preferred comment style
             (defun hook/set-c-comment-start ()
               (setq comment-start "// " comment-end ""))))

;;------------------------------------------------------------------------------
;; XML
;;------------------------------------------------------------------------------

(use-package! nxml

  :defer-incrementally t

  :init 

  (add-hook! 'nxml-mode-hook
    (defun hook/disable-visual-fill-column-mode ()
      (visual-fill-column-mode -1)))

  :config
  
  (setq nxml-child-indent 2
        nxml-attribute-indent 2)

  (defun nxml-pretty-format ()
    (interactive)
    (save-excursion
      (shell-command-on-region (point-min) (point-max)
                               "xmllint --format -" (buffer-name) t)
      (nxml-mode)
      (indent-region begin end))))

;;------------------------------------------------------------------------------
;; Python
;;------------------------------------------------------------------------------

(use-package! python
  :init

  (remove-hook! 'python-mode-local-vars-hook #'lsp!)
  (add-hook! 'python-mode-local-vars-hook
    (defun +python-init-lsp-mode-maybe-h ()
      (unless (file-remote-p (buffer-file-name)) (lsp!))))

  (remove-hook! 'python-mode-local-vars-hook #'tree-sitter!)
  (add-hook! 'python-mode-local-vars-hook
    (defun +python-init-tree-sitter-mode-maybe-h ()
      (unless (file-remote-p (buffer-file-name)) (tree-sitter!))))

  ;; modify the hook found in doom;
  ;; activating anaconda on tramp buffers is slow as hell
  (remove-hook! 'python-mode-local-vars-hook
    #'+python-init-anaconda-mode-maybe-h)
  (add-hook! 'python-mode-local-vars-hook :append
    (defun +python-init-anaconda-mode-maybe-h ()
      "Enable `anaconda-mode' if `lsp-mode' is absent and
`python-shell-interpreter' is present and we aren't on a tramp buffer."
      (unless (or
               (file-remote-p (buffer-file-name))
               (bound-and-true-p lsp-mode)
               (bound-and-true-p eglot--managed-mode)
               (bound-and-true-p lsp--buffer-deferred)
               (not (executable-find python-shell-interpreter t)))
        (anaconda-mode +1))))
  
  :config

  (setq python-shell--interpreter "python3"
        python-flymake-command '("flake8" "-")
        py-isort-options '("--combine-as"))

  )


;;------------------------------------------------------------------------------
;; ielm
;;------------------------------------------------------------------------------ 

(use-package! ielm

  :init

  ;; remember ielm history
  ;; global copy of the buffer-local variable
  (defvar ielm-comint-input-ring nil)

  (defun set-ielm-comint-input-ring ()
    ;; create a buffer-local binding of kill-buffer-hook
    (make-local-variable 'kill-buffer-hook)
    ;; save the value of comint-input-ring when this buffer is killed
    (add-hook 'kill-buffer-hook #'save-ielm-comint-input-ring)
    ;; restore saved value (if available)
    (when ielm-comint-input-ring
      (message "Restoring comint-input-ring...")
      (setq comint-input-ring ielm-comint-input-ring)))

  (defun save-ielm-comint-input-ring ()
    (message "Saving comint-input-ring...")
    (setq ielm-comint-input-ring comint-input-ring))

  (add-hook 'ielm-mode-hook
            #'set-ielm-comint-input-ring))

;;------------------------------------------------------------------------------
;; Elisp
;;------------------------------------------------------------------------------

;;  set the tab width for emacs lisp mode to 4 for compatibility with emacs libs
(use-package! elisp-mode

  :init
  (remove-hook! 'emacs-lisp-mode-hook #'outline-minor-mode)
  (remove-hook! 'emacs-lisp-mode-hook #'highlight-quoted-mode)
  (remove-hook! 'emacs-lisp-mode-hook #'embrace-emacs-lisp-mode-hook)
  (remove-hook! 'emacs-lisp-mode-hook #'doom--setq-tab-width-for-emacs-lisp-mode-h)
  (remove-hook! 'emacs-lisp-mode-hook #'doom--setq-outline-level-for-emacs-lisp-mode-h)
  (remove-hook! 'emacs-lisp-mode-hook #'doom--setq-outline-regexp-for-emacs-lisp-mode-h)

  (add-hook! 'emacs-lisp-mode-hook
    (defun hook/set-elisp-tab-width ()
      (setq-local tab-width 4))))

;;------------------------------------------------------------------------------
;; Common Lisp
;;------------------------------------------------------------------------------

(use-package! slime
  :defer-incrementally t
  :config
  (setq inferior-lisp-program "sbcl"
        org-babel-lisp-eval-fn 'slime-eval))

;;------------------------------------------------------------------------------
;; Clojure
;;------------------------------------------------------------------------------

(use-package! clojure-mode

  :defer-incrementally t

  :config

  ;; cider-edit-jack-in-command
  (setq org-babel-clojure-backend "cider")
  (setq cider-save-file-on-load t))

(use-package! flycheck-clj-kondo
  :after clojure-mode
  :defer-incrementally t)

;;------------------------------------------------------------------------------
;; Graphviz
;;------------------------------------------------------------------------------

(use-package! graphviz-dot-mode

  :config

  ;; png seems to have a bug right now
  (setq graphviz-dot-preview-extension "jpg")

  (defun graphviz--display-preview-buffer (stdout-buffer)
    "Display STDOUT-BUFFER as the dot preview."
    (save-excursion
      (with-current-buffer stdout-buffer
        (goto-char (point-min))
        (image-mode)
        (display-buffer stdout-buffer)))))

;;------------------------------------------------------------------------------
;; Flycheck
;;------------------------------------------------------------------------------

(use-package! flycheck

  :config

  (setq flycheck-temp-prefix ".flycheck"
        flycheck-check-syntax-automatically '(save idle-buffer-switch mode-enabled)
        flycheck-markdown-markdownlint-cli-config (concat doom-user-dir "markdownlint-config.yml")
        flycheck-yamllintrc (concat doom-user-dir "yamllintrc.yml")
        flycheck-flake8rc (concat doom-user-dir "flake8.rc")
        flycheck-pylintrc (concat doom-user-dir "pylint.rc"))

  ;; (flycheck-add-next-checker 'python-flake8 'python-pylint)

  ;;------------------------------------------------------------------------------
  ;; Verilator Modifications
  ;;------------------------------------------------------------------------------

  ;; add --timing opt to verilator
  (flycheck-define-checker verilog-verilator
    "A Verilog syntax checker using the Verilator Verilog HDL simulator.

    See URL `https://www.veripool.org/wiki/verilator'."
    :command ("verilator" "--timing" "--lint-only" "-Wall" "--quiet-exit"
              (option-list "-I" flycheck-verilator-include-path)
              source)
    :error-patterns
    ((warning line-start "%Warning"
              (? "-" (id (+ (any "0-9A-Z_")))) ": "
              (? (file-name) ":" line ":" (? column ":") " ")
              (message) line-end)
     (error line-start "%Error"
            (? "-" (id (+ (any "0-9A-Z_")))) ": "
            (? (file-name) ":" line ":" (? column ":") " ")
            (message) line-end))
    :modes verilog-mode)
  ;;;
  ;;;
  ;;------------------------------------------------------------------------------
  ;; Tcl Nagelfar
  ;; modified from the original to add filters and change options
  ;;------------------------------------------------------------------------------

  (flycheck-define-checker tcl-nagelfar
    "An extensible tcl syntax checker
See URL `http://nagelfar.sourceforge.net/'."
    :command ("nagelfar" "-Wunusedvar" "-filter" "*Unknown command*" "-H" source)
    :error-patterns
    ;; foo.tcl: 29: E Wrong number of arguments (4) to "set"
    ;; foo.tcl: 29: W Expr without braces
    ((info    line-start (file-name) ": " line ": N " (message) line-end)
     (warning line-start (file-name) ": " line ": W " (message) line-end)
     (error   line-start (file-name) ": " line ": E " (message) line-end))
    :modes tcl-mode)

  ;;------------------------------------------------------------------------------
  ;; Prose lint
  ;; https://unconj.ca/blog/linting-prose-in-emacs.html
  ;;------------------------------------------------------------------------------

  (flycheck-define-checker proselint
    "A linter for prose."
    :command ("proselint"
              ;;
              ;;            (option-flag "--external-sources" flycheck-shellcheck-follow-sources)
              "--config" (eval (expand-file-name "~/.doom.d/proselint.rc"))
              source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
              (id (one-or-more (not (any " "))))
              (message) line-end))
    :modes (text-mode latex-mode markdown-mode gfm-mode org-mode))

  ;; (add-to-list 'flycheck-checkers 'proselint)
  (setq-default flycheck-disabled-checkers '(proselint))
  ;; (add-hook! 'org-mode-hook
  ;;   (lambda ()
  ;;     (flycheck-disable-checker 'proselint)))

  (flycheck-define-checker
      hog-src-checker
    "Checker for Hog source files"

    :command ("emacs" (eval flycheck-emacs-args)
              "--load" (eval (file-name-sans-extension (locate-library "hog")))
              "--visit" source-inplace
              "-f" "hog-check-src-file")

    :error-patterns
    ((error line-start "Error:" line (one-or-more blank) (message) line-end)
     (info line-start "Info:" line (one-or-more blank) (message) line-end)
     (warning line-start "Warning:" (one-or-more blank) line (message) line-end))
    :modes (hog-src-mode))

  (add-to-list 'flycheck-checkers 'hog-src-checker)

  (flycheck-def-config-file-var flycheck-vhdl-vsg-config vhdl-vsg ".vsgrc")

  (flycheck-define-checker vhdl-vsg
    "VHDL Style Guide (VSG) provides coding style guide enforcement for VHDL code.
 https://vhdl-style-guide.readthedocs.io/en/latest/index.html"
    :command ("vsg" "-f"
              (config-file "-c" flycheck-vhdl-vsg-config)
              source)
    ;; https://vhdl-style-guide.readthedocs.io/en/latest/formatting_terminal_output.html
    ;; use syntastic format?
    :error-patterns
    ((error line-start (zero-or-more blank) (one-or-more (not blank)) (one-or-more blank) "|" (one-or-more blank)
            "Error"
            (one-or-more blank) "|" (one-or-more blank) line (one-or-more blank) "|" (one-or-more blank) (message) eol)
     (warning line-start (zero-or-more blank) (one-or-more (not blank)) (one-or-more blank) "|" (one-or-more blank)
              "Warning"
              (one-or-more blank) "|" (one-or-more blank) line (one-or-more blank) "|" (one-or-more blank) (message) eol))

    :modes vhdl-mode)


  ;; (flycheck-define-checker tcl-nagelfar
  ;;   "An extensible tcl syntax checker See URL `http://nagelfar.sourceforge.net/'."
  ;;   :command ("nagelfar" "-H" source)
  ;;   :error-patterns
  ;;   ;; foo.tcl: 29: E Wrong number of arguments (4) to "set"
  ;;   ;; foo.tcl: 29: W Expr without braces
  ;;   ((info    line-start (file-name) ": " line ": N " (message) line-end)
  ;;    (warning line-start (file-name) ": " line ": W " (message) line-end)
  ;;    (error   line-start (file-name) ": " line ": E " (message) line-end))
  ;;   :modes tcl-mode)
  ;; )

  ;; architecture_010          | Error      |        643 | Add *architecture* keyword.


  ;; (flycheck-define-checker vhdl-tool
  ;;   "A VHDL syntax checker, type checker and linter using VHDL-Tool.
  ;;         See URL `http://vhdltool.com'."
  ;;   :command ("vhdl-tool" "client" "lint" "--compact" "--stdin" "-f" source
  ;;             )
  ;;   :standard-input t
  ;;   :error-patterns
  ;;   ((warning line-start (file-name) ":" line ":" column ":w:" (message) line-end)
  ;;    (error line-start (file-name) ":" line ":" column ":e:" (message) line-end))
  ;;   :modes (vhdl-mode))
  ;;
  ;; (add-to-list 'flycheck-checkers 'vhdl-tool)


  ;; redefine the awk checker to have no-ext enabled on the linter "--lint=no-ext"
  (flycheck-define-checker awk-gawk
    "GNU awk's built-in --lint checker."
    :command ("gawk"
              ;; Avoid code execution.  See https://github.com/w0rp/ale/pull/1411
              "--source" "'BEGIN{exit} END{exit 1}'"
              "-f" source
              "--lint=no-ext"
              "/dev/null")
    :standard-input nil
    :error-patterns
    ((warning line-start
              "gawk: "
              (file-name) ":" line ":" (optional column ":")
              (message (one-or-more not-newline)
                       (optional "\n"
                                 (one-or-more not-newline)
                                 " ^ "
                                 (one-or-more not-newline)))
              line-end))
    :error-filter flycheck-awk-gawk-error-filter
    :modes awk-mode)

  (flycheck-define-checker openscad
    "Runs openscad"
    :command ("openscad"
              (eval (concat "-o" (flycheck-temp-dir-system) "/scad-tmp.png"))
              source-inplace)
    :error-patterns
    ;; different versions of scad produce slightly different error messages... uhg
    ((error line-start "ERROR:" (message) " " (file-name)  ", line " line line-end)
     (error line-start "ERROR:" (message) "\"" (file-name) "\", line " line ": syntax error" line-end))
    :modes (scad-mode))
  (add-to-list 'flycheck-checkers 'openscad))

;;------------------------------------------------------------------------------
;; Hog
;;------------------------------------------------------------------------------

(use-package! hog
  :after (:any verilog-mode vhdl-mode)
  :config
  (pcase (system-name)
    ("strange" (setq hog-vivado-path "~/Xilinx/Vivado/2021.1"
                     hog-number-of-jobs 16))
    ("larry" (setq hog-vivado-path "/storage/Xilinx/Vivado/2021.1"
                   hog-number-of-jobs 4))
    ("pepper" (setq hog-vivado-path "/opt/Xilinx/Vivado/2021.1"))
    ("apeck-len01" (setq hog-vivado-path "/opt/Xilinx/Vivado/2022.2"))
    )

  (setq hog-ieee-library
        '("ieee" ("/usr/local/lib/ghdl/src/synopsys/*.vhdl"
                  "/usr/local/lib/ghdl/src/std/v08/*.vhdl"
                  "/usr/local/lib/ghdl/src/ieee2008/*.vhdl"
                  "/usr/lib/ghdl/src/synopsys/*.vhdl"
                  "/usr/lib/ghdl/src/std/v08/*.vhdl"
                  "/usr/lib/ghdl/src/ieee2008/*.vhdl"))))

;;------------------------------------------------------------------------------
;; svg-tag-mode
;;------------------------------------------------------------------------------

(use-package! svg-tag-mode
  ;; https://github.com/rougier/svg-tag-mode

  :init

  ;; Replaces any occurrence of :Xxx: with a dynamic SVG tag displaying Xxx ;;
  (add-hook! 'org-mode-hook

             (setq-local svg-tag-tags
                         '(("\\(:[A-z]+:\\)" . ((lambda (tag)
                                                  (svg-tag-make tag :beg 1 :end -1))))))
             (svg-tag-mode t))

  )

;;------------------------------------------------------------------------------
;; Cape
;;------------------------------------------------------------------------------

(use-package! corfu

  ;; cape-dabbrev: Complete word from current buffers. See also dabbrev-capf on Emacs 29.
  ;; cape-elisp-block: Complete Elisp in Org or Markdown code block.
  ;; cape-file: Complete file name.
  ;; cape-history: Complete from Eshell, Comint or minibuffer history.
  ;; cape-keyword: Complete programming language keyword.
  ;; cape-symbol: Complete Elisp symbol.
  ;; cape-abbrev: Complete abbreviation (add-global-abbrev, add-mode-abbrev).
  ;; cape-dict: Complete word from dictionary file.
  ;; cape-line: Complete entire line from current buffer.
  ;; cape-tex: Complete Unicode char from TeX command, e.g. \hbar.
  ;; cape-sgml: Complete Unicode char from SGML entity, e.g., &alpha.
  ;; cape-rfc1345: Complete Unicode char using RFC 1345 mnemonics.


  :config

  (setq corfu-auto-delay 0.3

        ;; If t, check all other buffers (subject to dabbrev ignore rules).
        ;; Any other non-nil value only checks some other buffers, as per
        ;; dabbrev-select-buffers-function.
        cape-dabbrev-check-other-buffers nil)

  :init

  (add-hook! 'verilog-mode-hook
    (defun hook/add-verilog-keywords ()
      (with-eval-after-load 'cape-keyword
        (add-to-list 'cape-keyword-list
                     (append '(verilog-mode) verilog-keywords)))))

  (add-hook! 'vhdl-mode-hook
    (defun hook/add-vhdl-keywords ()
      (with-eval-after-load 'cape-keyword
        (add-to-list 'cape-keyword-list
                     (append '(vhdl-mode)
                             vhdl-keywords
                             vhdl-types
                             vhdl-attributes
                             vhdl-enum-values
                             vhdl-constants
                             vhdl-functions
                             vhdl-packages
                             vhdl-directives)))))



  (add-hook! 'emacs-lisp-mode-hook
    (defun hook/set-elisp-capf-functions ()
      (setq-local completion-at-point-functions
                  (list
                   (cape-company-to-capf #'company-yasnippet)
                   'cape-elisp-symbol
                   'cape-keyword
                   'cape-dabbrev
                   'cape-history
                   'cape-file))))

  (add-hook! 'verilog-mode-hook
    (defun hook/set-verilog-capf ()
      (setq-local completion-at-point-functions
                  (list (cape-capf-super
                         'cape-dabbrev
                         ;; 'complete-tag
                         'cape-keyword
                         (cape-company-to-capf #'company-yasnippet))))))

  (add-hook! 'vhdl-mode-hook
    (defun hook/set-vhdl-capf ()
      (setq-local completion-at-point-functions
                  (list (cape-capf-super
                         'cape-dabbrev
                         'cape-keyword
                         (cape-company-to-capf #'company-yasnippet))))))

  (dolist (mode '(python-ts-mode-hook python-mode-hook))
    (add-hook! mode
      (setq-local completion-at-point-functions
                  (list
                   (cape-capf-super
                    'cape-keyword
                    'cape-file
                    'eglot-completion-at-point
                    'cape-capf-buster
                    'cape-dabbrev
                    (cape-company-to-capf #'company-yasnippet))))))

  (add-hook! 'tcl-mode-hook
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       'cape-dabbrev
                       'cape-keyword
                       'cape-file
                       (cape-company-to-capf #'company-yasnippet)))))

  (add-hook! 'tcl-mode-hook
    (with-eval-after-load 'cape-keyword
      (add-to-list 'cape-keyword-list
                   (append '(tcl-mode)

                           ;; vivado
                           '("set_property" "add_files" "generate_target"
                             "report_utilization"
                             "report_timing_summary"
                             "import_ip" "create_project"
                             "get_files" "get_clocks" "get_cells" "get_pins" "get_ports"
                             "get_nets" "font-lock-builtin-face" "create_generated_clock"
                             "create_clock" "set_input_jitter" "set_input_delay" "set_output_delay"
                             "set_property" "set_clock_groups" "set_multicycle_path" "set_false_path"
                             "set_max_delay" "create_pblock" "add_cells_to_pblock" "resize_pblock")

                           tcl-keyword-list
                           tcl-typeword-list
                           tcl-builtin-list)))))


(use-package! yasnippet

  :config

  (setq yas-snippet-dirs "~/.doom.d/snippets")

  )

;;------------------------------------------------------------------------------
;; Unused
;;------------------------------------------------------------------------------

;; (use-package! writegood
;;   :config
;;   (writegood-passive-voice-turn-off))

;; ;; persistent undo
;; (use-package! undo
;;   :config
;;   (setq undo-tree-auto-save-history t))

(use-package! undo-tree
  :when (featurep 'undo-tree)
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.undo-tree")))

  ;; https://github.com/doomemacs/doomemacs/issues/902
  (advice-remove '+undo--append-zst-extension-to-file-name-a
                 'undo-tree-make-history-save-file-name))

(use-package! emojify-mode
  :when (featurep 'emojify-mode)
  :config
  (setq global-emojify-mode t))

(use-package! langtool
  :when (featurep 'langtool)
  :config
  (setq langtool-java-classpath "/snap/languagetool/current/usr/share/java")
  (setq langtool-language-tool-server-jar "/snap/languagetool/current/usr/bin/languagetool.jar"))

;;------------------------------------------------------------------------------
;; Company
;;------------------------------------------------------------------------------

;; (after! company

;;   (setq company-idle-delay 1.0
;;         company-minimum-prefix-length 2
;;         company-icon-size '(auto-scale . 24)
;;         company-backends
;;         '(company-bbdb company-semantic company-cmake company-capf company-clang company-files
;;           (company-dabbrev-code company-gtags company-etags company-keywords)
;;           company-oddmuse company-dabbrev))

;;   ;; (set-company-backend! 'text-mode nil)

;;   (after! org
;;     (set-company-backend! 'org-mode nil)
;;     (set-company-backend! 'org-mode '(company-capf company-files company-yasnippet company-dabbrev)))

;;   (add-hook! 'tcl-mode-hook
;;     (setq company-backends
;;           '((:separate company-dabbrev-code company-capf company-keywords
;;              :with company-yasnippet company-files))))

;;   (after! vhdl-mode
;;     (set-company-backend! 'vhdl-mode nil)
;;     (set-company-backend! 'vhdl-mode
;;                           '(company-capf company-keywords company-dabbrev-code company-yasnippet)))

;;   (after! hog-src-mode
;;     (set-company-backend! 'hog-src-mode nil)
;;     (set-company-backend! 'hog-src-mode 'company-files))

;;   (after! clojure-mode

;;     (add-hook! 'cider-repl-mode-hook #'company-mode)
;;     (add-hook! 'cider-mode-hook #'company-mode)
;;     (add-hook! 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
;;     (add-hook! 'cider-mode-hook #'cider-company-enable-fuzzy-completion)

;;     (defun my-clojure-mode-hook ()
;;       (setq-local company-backends
;;                   '((:separate company-capf company-keywords company-dabbrev-code company-yasnippet company-files))))
;;     (add-hook 'clojure-mode-hook #'my-clojure-mode-hook))

;;   (set-company-backend! 'clojure-mode
;;                         '(:separate company-capf company-keywords company-dabbrev-code company-yasnippet)))

;;------------------------------------------------------------------------------
;; Org Cliplink
;;------------------------------------------------------------------------------

(advice-add 'org-cliplink :before (lambda () (insert " ")))

;;------------------------------------------------------------------------------
;; Locals
;;------------------------------------------------------------------------------

;; Local Variables:
;; eval: (make-variable-buffer-local 'kill-buffer-hook)
;; eval: (+fold/close-all)
;; End:
