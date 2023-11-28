;; Backups
(use-package! backup-each-save

  :config

  (setq backup-each-save-mirror-location
        (expand-file-name "~/emacs-backups"))
  (when (not (file-directory-p backup-each-save-mirror-location))
    (make-directory backup-each-save-mirror-location))

  (add-hook 'after-save-hook 'backup-each-save))

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
     :utils "tar")))

;; (use-package! langtool
;;   :config
;;   (setq langtool-java-classpath "/snap/languagetool/current/usr/share/java")
;;   (setq langtool-language-tool-server-jar "/snap/languagetool/current/usr/bin/languagetool.jar"))

;; disable confusing undo-fu behavior
;; https://gitlab.com/ideasman42/emacs-undo-fu/-/issues/6
(use-package! undo-fu
  :config
  (setq undo-fu-ignore-keyboard-quit t))

;; ;; persistent undo
;; (use-package! undo
;;   :config
;;   (setq undo-tree-auto-save-history t))

;; (use-package! undo-tree
;;   :config
;;   (setq undo-tree-history-directory-alist '(("." . "~/.undo-tree"))))

;; https://github.com/doomemacs/doomemacs/issues/902
;; ~/.emacs.d/.local/cache/undo-tree-hist
;; (advice-remove '+undo--append-zst-extension-to-file-name-a
;;                'undo-tree-make-history-save-file-name)

;; (use-package! emojify-mode
;;   :config
;;   (setq global-emojify-mode t))

(use-package! ws-butler
  :defer-incrementally t
  :config
  (setq ws-butler-global-exempt-modes
        '(special-mode comint-mode term-mode eshell-mode)))

(use-package! hl-todo
  :defer-incrementally t
  :config
  (setq global-hl-todo-mode t))

;; save macros and other registers peristently
(use-package! savehist
  :config
  (add-to-list 'savehist-additional-variables 'register-alist)
  (add-hook! 'savehist-save-hook
    (defun doom-clean-up-registers-h ()
      (setq-local register-alist
                  (cl-remove-if-not #'savehist-printable register-alist)))))

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

(use-package! projectile
  :defer-incrementally t
  :config
  (setq projectile-sort-order 'recently-active))

;; (use-package! writegood
;;   :config
;;   (writegood-passive-voice-turn-off))

(use-package! yasnippet
  :defer-incrementally t
  :config
  ;; Don't add newlines to snippet endings
  (setq yas-also-auto-indent-first-line t)
  (add-hook 'snippet-mode-hook
            (lambda () (setq require-final-newline nil))))

(use-package! hog
  :after (:any verilog vhdl)
  :config
  (pcase (system-name)
    ("strange" (setq hog-vivado-path "~/Xilinx/Vivado/2021.1"
                     hog-number-of-jobs 16))
    ("larry" (setq hog-vivado-path "/storage/Xilinx/Vivado/2021.1"
                   hog-number-of-jobs 4))
    ("pepper" (setq hog-vivado-path "/opt/Xilinx/Vivado/2021.1")))

  (setq hog-ieee-library
        '("ieee" ("/usr/local/lib/ghdl/src/synopsys/*.vhdl"
                  "/usr/local/lib/ghdl/src/std/v08/*.vhdl"
                  "/usr/local/lib/ghdl/src/ieee2008/*.vhdl"
                  "/usr/lib/ghdl/src/synopsys/*.vhdl"
                  "/usr/lib/ghdl/src/std/v08/*.vhdl"
                  "/usr/lib/ghdl/src/ieee2008/*.vhdl"))))

;;------------------------------------------------------------------------------
;; Ispell
;;------------------------------------------------------------------------------

;; Save user defined words to the dictionary
(use-package! ispell
  :config
  (setq ispell-personal-dictionary "~/.aspell.en.pws")
  (defun my-save-word ()
    (interactive)
    (let ((current-location (point))
          (word (flyspell-get-word)))
      (when (consp word)
        (flyspell-do-correct 'save nil
                             (car word) current-location (cadr word)
                             (caddr word) current-location)))))

(add-hook! 'org-mode-hook #'jinx-mode)
(add-hook! 'markdown-mode-hook #'jinx-mode)

;;------------------------------------------------------------------------------
;; LaTex
;; LaTeX-mode-hook is used by AUCTeX's LaTeX mode.
;; latex-mode-hook is used by Emacs' built-in latex mode.
;;------------------------------------------------------------------------------

;; https://emacs.stackexchange.com/questions/33663/in-auctex-how-could-i-fold-acronyms
(use-package! tex-fold
  :defer-incrementally t
  :config
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
;; Cape
;;------------------------------------------------------------------------------
;;
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

(use-package! corfu

  :config

  (setq corfu-auto-delay 0.3)

  ;; If t, check all other buffers (subject to dabbrev ignore rules).
  ;; Any other non-nil value only checks some other buffers, as per
  ;; dabbrev-select-buffers-function.
  (setq cape-dabbrev-check-other-buffers nil)

  (defun cape-add-yasnippet ()
    (add-to-list 'completion-at-point-functions
                 (cape-company-to-capf #'company-yasnippet)))

  (add-hook! 'emacs-lisp-mode-hook
    (setq-local completion-at-point-functions
                (list
                 (cape-company-to-capf #'company-yasnippet)
                 'cape-symbol
                 'cape-keyword
                 'cape-dabbrev
                 'cape-history
                 'cape-file)))

  (add-hook! 'verilog-mode-hook
    (setq-local completion-at-point-functions
                (list (cape-super-capf
                       'cape-dabbrev
                       'complete-tag
                       'cape-keyword
                       (cape-company-to-capf #'company-yasnippet)))))

  (add-hook! 'vhdl-mode-hook
    (setq-local completion-at-point-functions
                (list (cape-super-capf
                       'cape-dabbrev
                       'cape-keyword
                       (cape-company-to-capf #'company-yasnippet)))))

  (dolist (mode '(python-ts-mode-hook python-mode-hook))
    (add-hook! mode
      (setq-local completion-at-point-functions
                  (list
                   (cape-super-capf
                    'cape-keyword
                    'cape-file
                    'eglot-completion-at-point
                    'cape-capf-buster
                    'cape-dabbrev
                    (cape-company-to-capf #'company-yasnippet))))))

  (add-hook! 'tcl-mode-hook
    (setq-local completion-at-point-functions
                (list (cape-super-capf
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

(add-hook! 'verilog-mode-hook
  (with-eval-after-load 'cape-keyword
    (add-to-list 'cape-keyword-list
                 (append '(verilog-mode) verilog-keywords))))

(add-hook! 'vhdl-mode-hook
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
                         vhdl-directives))))

;;------------------------------------------------------------------------------
;; Company
;;------------------------------------------------------------------------------

(after! company

  (setq company-idle-delay 1.0
        company-minimum-prefix-length 2
        company-icon-size '(auto-scale . 24)
        company-backends
        '(company-bbdb company-semantic company-cmake company-capf company-clang company-files
          (company-dabbrev-code company-gtags company-etags company-keywords)
          company-oddmuse company-dabbrev))

  ;; (set-company-backend! 'text-mode nil)

  (after! org
    (set-company-backend! 'org-mode nil)
    (set-company-backend! 'org-mode '(company-capf company-files company-yasnippet company-dabbrev)))

  (add-hook! 'tcl-mode-hook
    (setq company-backends
          '((:separate company-dabbrev-code company-capf company-keywords
             :with company-yasnippet company-files))))

  (after! vhdl-mode
    (set-company-backend! 'vhdl-mode nil)
    (set-company-backend! 'vhdl-mode
                          '(company-capf company-keywords company-dabbrev-code company-yasnippet)))

  (after! hog-src-mode
    (set-company-backend! 'hog-src-mode nil)
    (set-company-backend! 'hog-src-mode 'company-files))

  (after! clojure-mode

    (add-hook! 'cider-repl-mode-hook #'company-mode)
    (add-hook! 'cider-mode-hook #'company-mode)
    (add-hook! 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
    (add-hook! 'cider-mode-hook #'cider-company-enable-fuzzy-completion)

    (defun my-clojure-mode-hook ()
      (setq-local company-backends
                  '((:separate company-capf company-keywords company-dabbrev-code company-yasnippet company-files))))
    (add-hook 'clojure-mode-hook #'my-clojure-mode-hook))

  (set-company-backend! 'clojure-mode
                        '(:separate company-capf company-keywords company-dabbrev-code company-yasnippet)))

;;; -*- lexical-binding: t; -*-

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
        help-at-pt-display-when-idle t
        )


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
               '(vhdl-mode . ("ghdl-ls")))
  )

;; Magit
;;------------------------------------------------------------------------------

(use-package! magit

  :defer-incrementally t

  :config

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
    (browse-url  "https://openscad.org/cheatsheet/"))

  (after! flycheck

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
    (add-to-list 'flycheck-checkers 'openscad)))