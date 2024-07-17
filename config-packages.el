;; config-packages.el -*- lexical-binding: t; -*-

;;------------------------------------------------------------------------------
;; Olivetti Mode
;;------------------------------------------------------------------------------

(use-package! olivetti
  :init
  (add-hook! 'olivetti-mode-on-hook
    (setq-local doom--line-number-style nil)
    (setq-local display-line-numbers nil)))

;;------------------------------------------------------------------------------
;; Hydra
;;------------------------------------------------------------------------------

(use-package hydra
  :config
  ;; from dario https://codingstruggles.com/about/
  (defhydra doom-window-resize-hydra (:hint nil)
    "
             _k_ increase height
_h_ decrease width    _l_ increase width
             _j_ decrease height
"
    ("h" evil-window-decrease-width)
    ("j" evil-window-increase-height)
    ("k" evil-window-decrease-height)
    ("l" evil-window-increase-width)

    ("q" nil))

  (map! :leader (:prefix "w" :desc "Hydra resize" :n "SPC" #'doom-window-resize-hydra/body)))

;;------------------------------------------------------------------------------
;; Evil initial states
;;------------------------------------------------------------------------------

(after! log-view
  (set-evil-initial-state!
    '(log-view-mode
      vc-git-log-view-mode
      vc-hg-log-view-mode
      vc-bzr-log-view-mode
      vc-svn-log-view-mode)
    'normal))

(set-evil-initial-state! '(fundamental-mode) 'normal)

;;------------------------------------------------------------------------------
;; Casual Dired
;;------------------------------------------------------------------------------

(use-package! casual-dired
  :config
  (define-key dired-mode-map (kbd "C-o") #'casual-dired-tmenu)
  (map! :map dired-mode-map :localleader :desc "Casual Dired" "h" #'casual-dired-tmenu))

(sp-local-pair 'verilog-mode "begin" "end")

;;------------------------------------------------------------------------------
;; Emacs Everywhere
;;------------------------------------------------------------------------------

(use-package! emacs-everywhere
  :defer-incrementally t
  :config
  (define-key emacs-everywhere-mode-map "\C-c\C-c" #'emacs-everywhere-finish))

;;------------------------------------------------------------------------------
;; Jupyter Code Cells
;;------------------------------------------------------------------------------

(use-package! code-cells
  :defer t
  :mode ("\\.ipynb\\'")
  :config
  (map! :localleader
        :map code-cells-mode-map
        :prefix "m"
        (:prefix ("e" . "eval")
                 "c" #'code-cells-eval
                 "C" #'code-cells-eval-above)))

;;------------------------------------------------------------------------------
;; Midnight Mode
;;------------------------------------------------------------------------------

(use-package! midnight
  :defer-incrementally t
  :config
  (add-to-list 'clean-buffer-list-kill-buffer-names "*Native-compile-Log*")
  (add-to-list 'clean-buffer-list-kill-buffer-names "*Async-native-compile-log*"))

;;------------------------------------------------------------------------------
;; Dash Docs
;;------------------------------------------------------------------------------

(use-package! dash-docs
  :defer-incrementally t
  :config
  (defvar dash-docs-my-docsets
    '("Tcl" "Python_3" "C" "C++" "Bash" "Clojure")
    "Docsets to be installed via `dash-docs-install-my-docsets'")

  (defun dash-docs-install-my-docsets ()

    "Install docsets specified by the list `dash-docs-my-docsets'"

    (interactive)

    (require 'dash-docs)

    (let ((dash-docs--ensure-created-docsets-path
           (lambda (docset-path) (mkdir docset-path t)))
          (dash-docs-use-workaround-for-emacs-bug nil))

      (cl-flet ((setup-docset
                  (docset)
                  (when (not (file-exists-p (concat  dash-docs-docsets-path
                                                     (string-replace "_" " " docset) ".docset")))

                    (message (concat "Installing " docset "..."))
                    (dash-docs-install-docset docset))))

        (dolist (docset dash-docs-my-docsets)
          (setup-docset docset))))

    (message "Docsets installed.")))

;;------------------------------------------------------------------------------
;; Delight
;;------------------------------------------------------------------------------

(use-package! delight
  :demand t
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
  :defer-incrementally t

  :init
  (remove-hook! 'find-file-hook #'diff-hl-mode)
  (remove-hook! 'find-file-hook #'diff-hl-update-once)
  (add-hook! 'prog-mode-hook #'diff-hl-mode)

  :config
  (setq diff-hl-global-modes '(not image-mode org-mode markdown-mode pdf-view-mode)))

;;------------------------------------------------------------------------------
;; Treesitter
;;------------------------------------------------------------------------------

;; (use-package! treesit-auto)

;;------------------------------------------------------------------------------
;; Copyright
;;------------------------------------------------------------------------------

(use-package! copyright

  :demand t
  :init

  (add-hook! 'before-save-hook
    (defun hook/update-copyright ()
      "Automatically update copyright on save."

      (save-excursion
        (setq copyright-names-regexp "Andrew Peck")

        (when (and copyright-names-regexp
                   (progn (goto-char (point-min))
                          (copyright-re-search copyright-names-regexp nil t)))
          (copyright-update nil t)
          (copyright-fix-years)))))

  :config

  (setq copyright-year-ranges t)
  )

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

  (add-hook! 'python-mode-hook (apheleia-mode)))

;;------------------------------------------------------------------------------
;; PDF View + Image Mode
;;------------------------------------------------------------------------------

(use-package! pdf-view

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

  :demand t
  :config
  ;; (setq affe-find-command "rg --color=never --files")
  (setq affe-find-command (concat (or (executable-find "fd")
                                      (executable-find "fdfind"))
                                  " --color=never -L"))

  (defun affe-find-home    () (interactive) (affe-find "~/"))
  (defun affe-find-work    () (interactive) (affe-find "~/work"))
  (defun affe-find-project () (interactive) (affe-find (projectile-project-root)))
  (defun affe-grep-project () (interactive) (affe-grep (projectile-project-root)))
  (defun affe-find-notes   () (interactive) (affe-find "~/notes"))
  (defun affe-find-dotfile () (interactive) (affe-find "~/.dotfiles"))

  ;; Affe
  (after! evil-maps
    (map! :leader :prefix "f" :desc "Open dotfile"         "."  #'affe-find-dotfile)
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
  :init

  (add-hook! 'find-file-hook
    (add-hook 'after-save-hook #'backup-each-save nil t))

  :config

  (require 'backup-each-save)

  (setq backup-each-save-mirror-location
        (expand-file-name "~/emacs-backups"))
  (when (not (file-directory-p backup-each-save-mirror-location))
    (make-directory backup-each-save-mirror-location)))

;;------------------------------------------------------------------------------
;; Dired
;;------------------------------------------------------------------------------

(use-package! diredfl
  :after dired
  :config
  (add-to-list 'diredfl-compressed-extensions ".zst"))

(use-package! dired-aux
  :after dired
  :config
  (setq dired-compress-file-default-suffix ".zst")
  (setq dired-compress-directory-default-suffix ".tar.zst"))

(use-package! dired-x
  :after dired
  :config

  ;; Dired omit mode is causing errors as of 7/15/24
  ;; (add-hook 'dired-mode-hook #'dired-omit-mode)
  ;; (setq dired-omit-extensions (remove ".bin" dired-omit-extensions))
  ;; (setq dired-omit-extensions (remove ".bit" dired-omit-extensions))

  ;; (add-to-list 'dired-omit-extensions ".tmp")

  ;; (setq dired-omit-files
  ;;       (concat dired-omit-files
  ;;               "\\|vivado.*\\.jou\\'"
  ;;               "\\|vivado.*\\.backup\\.log\\'"
  ;;               "\\|vivado\\.log\\'"
  ;;               "\\|^\\.Xil\\'"
  ;;               "\\|^\\.mypy_cache\\'"
  ;;               "\\|^__pycache__\\'"
  ;;               "\\|^\\.pytest_cache\\'"))
  )

(use-package! dired

  :config

  ;; better dired soring
  (setq dired-listing-switches "-a1vBhl  --group-directories-first"
        dired-dwim-target t  ; suggest a target for moving/copying intelligently
        dired-hide-details-hide-symlink-targets nil
        ;; don't prompt to revert, just do it
        dired-auto-revert-buffer #'dired-buffer-stale-p
        ;; Always copy/delete recursively
        dired-recursive-copies  'always
        dired-recursive-deletes 'top)

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
  (remove-hook! 'dired-mode-hook #'+dired-disable-gnu-ls-flags-maybe-h)

  (remove-hook! 'dired-mode-hook #'diff-hl-dired-mode)

  (add-hook 'dired-mode-hook #'diff-hl-dired-mode-unless-remote)

  (add-hook 'dired-mode-hook #'dired-omit-mode)

  (add-hook! 'dired-mode-hook
    (defun hook/dired-hide-details ()
      "Hide details by default in dired to reduce line noise."
      (dired-hide-details-mode 1)))

  ;; (add-hook! 'dired-mode-hook
  ;;   (defun hook/enable-dired-git-filter ()
  ;;     ""
  ;;     (unless (file-remote-p default-directory)
  ;;       (when (locate-dominating-file "." ".git")
  ;;         (dired-filter-mode)
  ;;         (dired-filter-by-git-ignored)))))

  (add-hook! 'dired-after-readin-hook
    (defun hook/dired-git-info-mode ()
      "Enable dired git info on local files."
      (unless (file-remote-p default-directory)
         (dired-git-info-auto-enable))))

  ;; Stolen from doom: Disable the prompt about whether I want to kill the Dired
  ;; buffer for a deleted directory. Of course I do!
  (setq dired-clean-confirm-killing-deleted-buffers nil)

  (advice-add 'dired-find-file
              :before-until
              (lambda () (when (and (member (file-name-extension (dired-get-file-for-visit))
                                            auto-external-handle-extensions)
                                    (not (file-remote-p (dired-get-file-for-visit))))
                           (ap/dired-external-open))))

  (defun ap/external-open (file)
    (let* ((ext-handler (cdr (assoc (file-name-extension file) external-program-handlers)))
           (program (or ext-handler "xdg-open")))
      (message (format  "Opening %s in %s" file program))
      (call-process program nil 0 nil file)))

  (defun ap/dired-external-open()
    (interactive)
    (ap/external-open (dired-get-file-for-visit)))

  (defvar auto-external-handle-extensions
    '("drawio")
    "List of extensions to automatically open via external program.")

  (defvar external-program-handlers
    '(("drawio" . "drawio"))
    "alist of extensions and the program which should be used to open them, e.g.
\\='((\".mp3\" .\"mplayer\")
   (\".avi\" .\"vlc\")).

If not specified it will default to xdg-open."))

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
  :config
  (setq ws-butler-global-exempt-modes
        '(special-mode comint-mode term-mode eshell-mode)))

;;------------------------------------------------------------------------------
;; Highlight Todos
;;------------------------------------------------------------------------------

(use-package! hl-todo
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

  :load-path "~/.emacs.d/.local/straight/repos/tramp"
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

(use-package! project

  :config

  ;; doom overwrites this to ignore tramp uhg damnit
  ;; restore the original value
  ;;
  ;; but when it is restored the bookmarks don't work because the bookmark tool
  ;; apparently tries to create tramp connections? UHG
  (setq vc-ignore-dir-regexp "\\`\\(?:[\\/][\\/][^\\/]+[\\/]\\|/\\(?:net\\|afs\\|\\.\\.\\.\\)/\\)\\'")

  ;; doom has project.el calling projectile, just revert to original value
  (setq project-find-functions (list #'project-try-vc))

  )

(use-package! projectile
  :after project
  :config

  ;; doom overwrites this to ignore tramp uhg damnit
  ;; restore the original value
  ;;
  ;; but when it is restored the bookmarks don't work because the bookmark tool
  ;; apparently tries to create tramp connections? UHG
  (setq vc-ignore-dir-regexp "\\`\\(?:[\\/][\\/][^\\/]+[\\/]\\|/\\(?:net\\|afs\\|\\.\\.\\.\\)/\\)\\'")

  ;; doom has project.el calling projectile, just revert to original value
  (setq project-find-functions (list #'project-try-vc))

  ;; use project.el instead of projectile
  ;; (advice-add 'projectile-find-file
  ;;             :override
  ;;             (lambda (_) (project-find-file)))

  ;; (advice-add 'projectile-find-file :override
  ;;             (lambda (&optional _)
  ;;               (call-interactively #'project-find-file)))

  ;; (defun projectile-project-root (&optional _)
  ;;   (let ((pc (project-current)))
  ;;     (when pc (project-root pc))))

  ;; (defun projectile-switch-project (&optional _)
  ;;   (interactive)
  ;;   (call-interactively #'project-switch-project))

  ;; Due to a very obnoxious bug it seems that if I am on a host system where
  ;; the fd executable is fdfind but connecting to a remote-system where fd is
  ;; found as fd, emacs will try to execute fdfind on the remote system;
  ;; should report this
  (setq projectile-fd-executable "fdfind")

  (add-hook! 'find-file-hook
    (when (file-remote-p default-directory)
      (setq-local projectile-git-use-fd nil)))

  ;; https://github.com/bbatsov/projectile/issues/1232
  ;; don't try to retrieve project name via projectile on remote dirs
  (advice-add 'projectile-project-root
              :before-until
              (lambda (&optional _)
                (if (file-remote-p default-directory)
                    (let ((pc (project-current)))
                      (if pc
                          (project-root pc))))))

  (setq projectile-sort-order 'recently-active))

;;------------------------------------------------------------------------------
;; Yasnippet
;;------------------------------------------------------------------------------

(use-package! yasnippet
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

(use-package! jinx
  :config
  (global-jinx-mode nil)
  (add-hook 'org-mode-hook (lambda () (jinx-mode 1)))
  (add-hook 'tex-mode-hook (lambda () (jinx-mode 1)))
  (add-hook 'markdown-mode-hook (lambda () (jinx-mode 1)))

  (global-set-key [remap ispell-word] #'jinx-correct-word))

;;------------------------------------------------------------------------------
;; LaTex
;; LaTeX-mode-hook is used by AUCTeX's LaTeX mode.
;; latex-mode-hook is used by Emacs' built-in latex mode.
;;------------------------------------------------------------------------------

(use-package! tex-fold
  :config
  ;; https://emacs.stackexchange.com/questions/33663/in-auctex-how-could-i-fold-acronyms
  (setq TeX-fold-macro-spec-list
        (append TeX-fold-macro-spec-list
                '(
                  ;; glossary
                  ((lambda (x) (capitalize x)) ("Gls"))
                  ("{1}" ("gls"))

                  ;; just put backticks around typewriter font
                  ("`{1}`" ("texttt"))

                  ;; shorten references
                  ("[r:{1}]" ("autoref"))
                  ("[l:{1}]" ("label"))
                  ("[r:{1}]" ("ref"))
                  ("[c:{1}]" ("cite"))

                  ;; used in resume
                  ((lambda (x) (concat (number-to-string (- (string-to-number (format-time-string "%Y"))
                                                            (string-to-number x))) " years")) ("yearsago"))

                  ;; used in resume
                  ("{1}" ("heading"))))))

;; auctex + tex
(use-package! latex
  :config

  (setq-default TeX-master nil)         ; Query for master file.

  (evil-define-key '(motion normal visual) reftex-toc-mode-map
    (kbd "<") #'reftex-toc-promote)
  (evil-define-key '(motion normal visual) reftex-toc-mode-map
    (kbd ">") #'reftex-toc-demote)
  (evil-define-key '(motion normal visual) reftex-toc-mode-map
    (kbd "r") #'reftex-toc-Rescan)
  (evil-define-key '(motion normal visual) reftex-toc-mode-map
    (kbd "L") #'reftex-toc-set-max-level)


  (setq  reftex-cite-format nil)
  (setq  reftex-find-reference-format nil)
  ;; (setq  reftex-ref-style-alist '(("\\ref" t)))
  (setq  reftex-ref-macro-prompt nil)


  (define-key LaTeX-mode-map (kbd "C-c C-s") #'LaTeX-section)

  (add-hook 'latex-mode-hook #'outline-minor-mode)

  (define-key global-map (kbd "M-<right>") nil) ; bound by drag-stuff

  (define-key TeX-mode-map (kbd "M-<right>") #'outline-demote)
  (define-key TeX-mode-map (kbd "M-<left>") #'outline-promote)
  (define-key TeX-mode-map (kbd "TAB") #'nil)

  (defun TeX-toggle-folding ()
    (interactive)
    (call-interactively #'TeX-fold-mode)
    (if TeX-fold-mode
        (TeX-fold-buffer)
      (TeX-fold-clearout-buffer)))

  (map! :map TeX-mode-map
        :localleader

        ;; olivetti
        :desc "Olivetti Mode" "o" #'olivetti-mode

        ;; formatting macros
        :desc "TeX Format Bold"      "tb" #'tex-bold
        :desc "TeX Format Underline" "tu" #'tex-underline
        :desc "TeX Format Folding"   "ti" #'tex-italic
        :desc "TeX Format Folding"   "tt" #'tex-tt
        :desc "Tex glossarify"       "tg" #'tex-glossarify
        :desc "Tex Glossarify"       "tG" #'tex-Glossarify

        ;; folding
        :desc "Toggle TeX Folding" "b" #'TeX-toggle-folding
        )

  (defun tex-link-insert ()
    "Insert TeX href link"
    (interactive)
    (let ((url-at-point (thing-at-point 'url)))
      (let ((url (read-string "URL: " url-at-point))
            (text (read-string (format "Text: "))))
        (when (and url text)

          (when url-at-point
            (let ((bounds (bounds-of-thing-at-point 'url)))
              (delete-region (car bounds)
                             (cdr bounds))))

          (insert (format "\\href{%s}{%s}" url text))))))

  (define-key TeX-mode-map (kbd "C-c C-l") #'tex-link-insert)

  (define-key TeX-mode-map (kbd "C-c C-s") #'LaTeX-section)

  (defun reftex-toc-set-max-level ()
    (interactive)
    (let ((level
           (read-number "Level: " reftex-toc-max-level)))
      (setq reftex-toc-max-level level))
    (reftex-toc-Rescan))

  (setq pdf-sync-backward-display-action nil)
  (setq pdf-sync-forward-display-action t)

  (setq TeX-view-program-selection
        '((output-pdf "PDF Tools")
          (output-dvi "xdvi")))

  (setq TeX-view-program-list
        '(("PDF Tools" TeX-pdf-tools-sync-view)))

  (setq reftex-toc-max-level 2
        reftex-toc-split-windows-horizontally t
        reftex-toc-split-windows-fraction 0.2
        TeX-master nil
        +latex-viewers '(okular atril evince zathura))

  (setq-default TeX-command-extra-options " -shell-escape -synctex=1")

  (add-hook! 'LaTeX-mode-hook

             (olivetti-mode)
             (reftex-mode 1)
             (jinx-mode t)
             ;; (variable-pitch-mode 1)

             ;; (make-variable-buffer-local 'font-lock-type-face)
             ;; (set-face-attribute 'font-lock-type-face nil
             ;;                     :inherit 'default
             ;;                     :family "Courier New"
             ;;                     :height 120)

             ;; https://www.flannaghan.com/2013/01/11/tex-fold-mode
             ;; (add-hook! 'find-file-hook :local (TeX-fold-region (point-min) (point-max)))
             ;; (add-hook! 'write-contents-functions :local (TeX-fold-region (point-min) (point-max)))
             ;; (add-hook! 'after-change-functions :local 'TeX-fold-paragraph)

             ;; doom has some annoying hooks after macro insertion that cause obnoxious folding
             ;; (setq TeX-after-insert-macro-hook nil)

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
                         (point-marker)))) ;; remember where to stop
              (beginning-of-line)
              (while (progn (forward-sentence)
                            (<= (point) (marker-position end)))
                (just-one-space) ;; leaves only one space, point is after it
                (delete-char -1) ;; delete the space
                (newline)        ;; and insert a newline
                (evil-indent-line (line-beginning-position) (line-end-position))))))

      ;; otherwise do ordinary fill paragraph
      (fill-paragraph P)))

  ;; treat hyphenated words as one
  (add-hook 'latex-mode-hook #'(lambda () (modify-syntax-entry ?- "w")))

  (defun tex-expand-and-insert (macro)
    (interactive)
    (when (not (region-active-p))
      (er/mark-word))
    (TeX-insert-macro macro))

  (defun tex-underline ()
    "Make the current TeX selection bold."
    (interactive)
    (tex-expand-and-insert "underline"))

  (defun tex-bold ()
    "Make the current TeX selection bold."
    (interactive)
    (tex-expand-and-insert "textbf"))

  (defun tex-italic ()
    "Make the current TeX selection italic."
    (interactive)
    (tex-expand-and-insert "textit"))

  (defun tex-tt ()
    "Make the current TeX selection typewriter."
    (interactive)
    (tex-expand-and-insert "texttt"))

  (defun tex-glossarify ()
    "Make the current TeX selection a glossary entry."
    (interactive)
    (tex-expand-and-insert "gls"))

  (defun tex-Glossarify ()
    "Make the current TeX selection a Glossary entry."
    (interactive)
    (tex-expand-and-insert "Gls"))

  ;; Electric Space
  ;;------------------------------------------------------------------------------

  (defun electric-space ()        ; Trying to get Emacs to do semantic linefeeds
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
;; LSP
;;------------------------------------------------------------------------------

(when (and (modulep! :tools lsp)
           (not (modulep! :tools lsp +eglot)))

  (use-package lsp-mode
    :config

    (setq lsp-enable-symbol-highlighting nil
          lsp-vhdl-server 'hdl-checker
          ;; lsp-vhdl-server-path "~/.local/bin/hdl_checker"; only needed if hdl_checker is not already on the PATH
          )

    (add-to-list 'lsp-file-watch-ignored (expand-file-name "~/.pyenv"))
    (add-to-list 'lsp-language-id-configuration '(verilog-mode . "verilog"))
    (add-to-list 'lsp-disabled-clients 'svlangserver)
    (add-to-list 'lsp-disabled-clients 'lsp-verilog-verible)

    ;; (add-hook 'verilog-mode-hook 'lsp)
    ;; (add-hook 'vhdl-mode-hook 'lsp)

    ;; https://github.com/doomemacs/doomemacs/issues/7491
    (setq lsp-auto-register-remote-clients nil) ; ?? does not seem to work
    )

  ;;------------------------------------------------------------------------------
  ;; Booster
  ;;------------------------------------------------------------------------------


  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?) ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection)) ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))

  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

;;------------------------------------------------------------------------------
;; Eglot
;;------------------------------------------------------------------------------

(use-package! eldoc
  :config

  ;; calling +lookup/documentation annoyingly moves the cursor to the other window
  ;; just add some advice to move it back
  (advice-add '+lookup/documentation :around
              (lambda (orig-fun &rest args)
                (let ((current (selected-window)))
                  (apply orig-fun args)
                  (select-window current))))

  (setq eldoc-echo-area-prefer-doc-buffer t
        eldoc-idle-delay 1.5
        eldoc-echo-area-use-multiline-p nil)

  (add-hook! 'python-mode-hook
    (defun hook/hide-python-eldoc ()
      )

    (when (modulep! :tools lsp +eglot)
      (use-package! eglot

        :init

        (setq eglot-managed-mode-hook
              (list (lambda () (eldoc-mode -1))))

        :config

        (setq eglot-prefer-plaintext nil
              eglot-autoshutdown t
              help-at-pt-display-when-idle t)

        ;; (add-to-list 'eglot-server-programs '(python-mode . ("ruff-lsp")))
        ;; (add-to-list 'eglot-server-programs '(python-ts-mode . ("ruff-lsp")))
        ;; (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
        ;; (add-to-list 'eglot-server-programs '(python-ts-mode . ("pyright-langserver" "--stdio")))

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
        (setq eglot-booster-no-remote-boost t)))
    )
  )

;;------------------------------------------------------------------------------
;; Magit
;;------------------------------------------------------------------------------

(use-package! magit


  :config

  (define-key transient-map (kbd "C-c C-c") #'transient-save)

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
  :demand t
  :config
  (add-to-list 'browse-at-remote-remote-type-regexps '(:host "^gitlab\\.cern.ch$" :type "gitlab")))

(use-package! forge

  :after magit

  :config

  (add-to-list 'auth-sources "~/.authinfo")

  (add-to-list 'forge-alist '("gitlab.cern.ch" "gitlab.cern.ch/api/v4" "gitlab.cern.ch" forge-gitlab-repository))

  (setq forge-topic-list-limit '(60 . 0)
        forge-owned-accounts '(("andrewpeck")
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
    (call-process
     "openscad" nil 0 nil
     (buffer-file-name)))

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

  :mode ("\\.v\\'" "\\.sv\\'" "\\.svh\\'")

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

  (require 'verilog-port-copy)

  (defun yosys-make-schematic ()
    "Make a yosys schematic from the module at point.

TODO: need to process dependencies.
TODO: use something less intrusive and focus stealing than shell-command (e.g. comint)
TODO: catch errors, e.g. if read-module-name fails

"

    (interactive)
    (let* ((module (verilog-read-module-name))
           (sources (buffer-file-name))
           (cmd
            (concat "yosys -p " "\"plugin -i systemverilog; read_systemverilog "
                    sources " ; "
                    "proc; stat; synth -top "
                    module ";"
                    " ; write_json -compat-int netlist.json; ltp; stat\""
                    (format
                     "&& netlistsvg netlist.json -o %s.svg && xdg-open %s.svg" module module))))
      (shell-command cmd)))


  (defun verilog-pretty-expr (&optional quiet)
    "Line up expressions around point.
If QUIET is non-nil, do not print messages showing the progress of line-up."
    (interactive)
    (let* ((basic-complete-pretty-expr-re (if verilog-align-assign-expr
                                              verilog-basic-complete-expr-no-assign-re
                                            verilog-basic-complete-expr-re))
           (complete-pretty-expr-re (concat verilog-extended-complete-re "\\|\\(" basic-complete-pretty-expr-re "\\)"))
           (discard-re (concat "^\\s-*\\(" complete-pretty-expr-re "\\)"))
           rstart rend)
      (save-excursion
        (when (region-active-p)
          (setq rstart (region-beginning))
          (setq rend (region-end))
          (goto-char rstart))
        (unless (verilog-in-comment-or-string-p)
          (beginning-of-line)
          (when (and (verilog--pretty-expr-assignment-found discard-re)
                     (save-excursion
                       (goto-char (match-end 2))
                       (and (not (verilog-in-attribute-p))
                            (not (verilog-in-comment-or-string-p)))))
            (let* ((start (cond (;; Using region
                                 (region-active-p)
                                 rstart)
                                (;; Parameter list
                                 (verilog-in-parenthesis-p)
                                 (progn
                                   (verilog-backward-up-list 1)
                                   (forward-char)
                                   (verilog-re-search-forward verilog-assignment-operation-re-2 nil 'move)
                                   (goto-char (match-beginning 0))
                                   (point)))
                                (t ;; Declarations
                                 (save-excursion ; BOL of the first line of the assignment block
                                   (beginning-of-line)
                                   (let ((pt (point)))
                                     (verilog-backward-syntactic-ws)
                                     (beginning-of-line)
                                     (while (and (verilog--pretty-expr-assignment-found discard-re)
                                                 (not (bobp)))
                                       (setq pt (point))
                                       (verilog-backward-syntactic-ws)
                                       (beginning-of-line)) ; Ack, need to grok `define
                                     pt)))))
                   (startpos (set-marker (make-marker) start))
                   (end (cond (;; Using region
                               (region-active-p)
                               (verilog--pretty-expr-find-end discard-re rend))
                              (;; Parameter list
                               (verilog-in-parenthesis-p)
                               (verilog--pretty-expr-find-end))
                              (t ;; Declarations
                               (verilog--pretty-expr-find-end discard-re))))
                   (endpos (set-marker (make-marker) end))
                   (contains-2-char-operator (string-match "<=" (buffer-substring-no-properties start end))))
              ;; Start with alignment
              (goto-char startpos)
              (unless (save-excursion
                        (beginning-of-line)
                        (looking-at discard-re))
                (verilog-do-indent (verilog-calculate-indent)))
              (when (and (not quiet)
                         (> (- (marker-position endpos) (marker-position startpos)) 100))
                (message "Lining up expressions.. (please stand by)"))
              ;; Set indent to minimum throughout region
              ;; Rely on mark rather than on point as the indentation changes can
              ;; make the older point reference obsolete
              (while (< (point) (marker-position endpos))
                (beginning-of-line)
                (save-excursion
                  (if (looking-at verilog-complete-re)
                      (progn (goto-char (marker-position startpos))
                             (verilog-just-one-space verilog-assignment-operation-re-2))
                    (verilog-just-one-space verilog-assignment-operation-re)))
                (verilog-do-indent (verilog-calculate-indent))
                (end-of-line)
                (verilog-forward-syntactic-ws))

              (let ((ind (verilog-get-lineup-indent-2 verilog-assignment-operation-re (marker-position startpos) (marker-position endpos))) ; Find the biggest prefix
                    e)
                ;; Now indent each line.
                (goto-char (marker-position startpos))
                (while (progn
                         (setq e (marker-position endpos))
                         (> e (point)))
                  (unless quiet
                    (message " verilog-pretty-expr: %d" (- e (point))))
                  (setq e (point))
                  (cond
                   ((or (looking-at verilog-assignment-operation-re)
                        (and (verilog-in-parenthesis-p)
                             (looking-at verilog-assignment-operation-re-2)))
                    (goto-char (match-beginning 2))
                    (unless (or (and (verilog-in-parenthesis-p) ; Leave attributes and comparisons alone
                                     (save-excursion ; Allow alignment of some expressions inside param/port list
                                       (verilog-backward-up-list 1)
                                       (verilog-beg-of-statement-1)
                                       (not (looking-at verilog-defun-level-re))))
                                (verilog-in-coverage-p))
                      (if (and contains-2-char-operator
                               (eq (char-after) ?=))
                          (indent-to (1+ ind)) ; Line up the = of the <= with surrounding =
                        (indent-to ind)))
                    (forward-line 1))
                   ((and (save-excursion

                           (verilog-forward-syntactic-ws)
                           (not (looking-at verilog-complete-re)))

                         ;; AP:: added this condition to not indent non-trailing comments when lining up declaration
                         ;; previously something like this:
                         ;;
                         ;;    int A = 6,
                         ;;    // some comment
                         ;;    int B = 2,
                         ;;
                         ;; would get aligned to
                         ;;
                         ;;    int A = 6,
                         ;;          // some comment
                         ;;    int B = 2,
                         ;;
                         ;; consider filing a bug-- reproduce w/ vanilla emacs first
                         ;; it seems like from the comment below (the t condition) that this is not intended behavior

                         (save-excursion
                           (beginning-of-line)
                           (not (looking-at "^[[:space:]]*\/\/.*")))

                         (verilog-continued-line-1 (marker-position startpos)))
                    (goto-char e)
                    (indent-line-to ind)

                    (forward-line 1))
                   (t             ; Must be comment, white space or syntax error
                    (goto-char e)
                    (forward-line 1))))
                ;; Align comments if enabled
                (when verilog-align-decl-expr-comments
                  (verilog-align-comments startpos endpos))
                (unless quiet
                  (message "")))))))))

  (define-key verilog-mode-map (kbd "RET") nil)
  (define-key verilog-mode-map (kbd "TAB") nil)
  (define-key verilog-mode-map (kbd "<backspace>") nil)

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

(use-package! pyenv-mode
  :config
  ;; damn pyenv-mode sets C-c C-s and it shows up everywhere (e.g. in latex)
  (define-key pyenv-mode-map (kbd "C-c C-s") nil))

(use-package! python
  :defer-incrementally t
  :init

  ;; Initialize LSP unless the python file is remote
  (remove-hook! 'python-mode-local-vars-hook #'lsp!)
  (defun +python-init-lsp-mode-maybe-h ()
    "Initialize LSP unless the python file is remote."
    (unless (and (buffer-file-name)
                 (file-remote-p (buffer-file-name)))
      (call-interactively #'lsp!)))
  (add-hook! 'python-mode-local-vars-hook #'+python-init-lsp-mode-maybe-h)

  (remove-hook! 'python-mode-local-vars-hook #'tree-sitter!)
  (add-hook! 'python-mode-local-vars-hook
    (defun +python-init-tree-sitter-mode-maybe-h ()
      (unless (and (buffer-file-name)
                   (file-remote-p (buffer-file-name)))
        (tree-sitter!))))

  ;; modify the hook found in doom;
  ;; activating anaconda on tramp buffers is slow as hell
  (remove-hook! 'python-mode-local-vars-hook
    #'+python-init-anaconda-mode-maybe-h)
  (add-hook! 'python-mode-local-vars-hook :append
    (defun +python-init-anaconda-mode-maybe-h ()
      "Enable `anaconda-mode' if `lsp-mode' is absent and
`python-shell-interpreter' is present and we aren't on a tramp buffer."
      (unless (or (and (buffer-file-name) (file-remote-p (buffer-file-name)))
                  (bound-and-true-p lsp-mode)
                  (bound-and-true-p eglot--managed-mode)
                  (bound-and-true-p lsp--buffer-deferred)
                  (not (executable-find python-shell-interpreter t)))
        (anaconda-mode +1))))

  :config

  (advice-add 'run-python :around
              (lambda (orig-fun &rest args)
                (let ((current (selected-window)))
                  (apply orig-fun args)
                  (select-window current))))

  (setq python-shell--interpreter "python3"
        python-flymake-command '("flake8" "-")
        py-isort-options '("--line-length" "300"))

  (defun python-sort-imports ()
    "Sort Python imports in the current buffer."
    (interactive)
    (if (apply #'python--do-isort py-isort-options)
        (message "Sorted imports")
      (message "(No changes in Python imports needed)"))))

;;------------------------------------------------------------------------------
;; Comint
;;------------------------------------------------------------------------------

(use-package! comint

  :init

  (add-hook 'comint-mode-hook 'comint-add-scroll-to-bottom)

  :config

  (require 'comint-scroll-to-bottom)

  (add-hook 'inferior-python-mode-hook
            (lambda ()
              (setq comint-move-point-for-output t)))

  (setq comint-move-point-for-output t
        comint-scroll-to-bottom-on-input t
        comint-scroll-to-bottom-on-output t
        comint-scroll-show-maximum-output t)

  )

;;------------------------------------------------------------------------------
;; ielm
;;------------------------------------------------------------------------------

(use-package! ielm
  :defer-incrementally t

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

(use-package! elisp-mode
  :defer-incrementally t

  :init
  (remove-hook! 'emacs-lisp-mode-hook #'outline-minor-mode)
  (remove-hook! 'emacs-lisp-mode-hook #'highlight-quoted-mode)
  (remove-hook! 'emacs-lisp-mode-hook #'embrace-emacs-lisp-mode-hook)
  (remove-hook! 'emacs-lisp-mode-hook #'doom--setq-tab-width-for-emacs-lisp-mode-h)
  (remove-hook! 'emacs-lisp-mode-hook #'doom--setq-outline-level-for-emacs-lisp-mode-h)
  (remove-hook! 'emacs-lisp-mode-hook #'doom--setq-outline-regexp-for-emacs-lisp-mode-h)

  ;; set the tab width for emacs lisp mode to 4 for compatibility with emacs libs
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
  :defer-incrementally t
  :after clojure-mode
  :defer-incrementally t)

;;------------------------------------------------------------------------------
;; Graphviz
;;------------------------------------------------------------------------------

(use-package! graphviz-dot-mode
  :defer-incrementally t

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
  :defer-incrementally t

  :config

  (setq flycheck-temp-prefix ".flycheck"
        flycheck-check-syntax-automatically '(save idle-buffer-switch mode-enabled)
        flycheck-markdown-markdownlint-cli-config (concat doom-user-dir "markdownlint-config.yml")
        flycheck-yamllintrc (concat doom-user-dir "yamllintrc.yml")
        flycheck-flake8rc (concat doom-user-dir "flake8.rc")
        flycheck-pylintrc (concat doom-user-dir "pylint.rc"))

  ;; i hate mypy
  (setq flycheck-checkers (delete 'python-mypy flycheck-checkers))

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
  :defer-incrementally t
  :mode ("\\.src\\'" "\\.lst\\'"  "\\.lst\\'")
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
  :after org-mode
  ;; https://github.com/rougier/svg-tag-mode

  :init

  (setq svg-tag-tags
        '(("NOTE" . ((lambda (tag) (svg-tag-make :face 'font-lock-string-face "TODO"))))
          ("TODO" . ((lambda (tag) (svg-tag-make :face 'org-warning "TODO"))))
          ("FIXME" . ((lambda (tag) (svg-tag-make :face 'error "FIXME"))))))

  :config

  ;; Replaces any occurrence of :Xxx: with a dynamic SVG tag displaying Xxx ;;
  (add-hook 'org-mode-hook
            (defun hook/org-configure-svg-tags ()
              (require 'svg-tag-mode)
              (setq-local svg-tag-tags '(("[[:space:]]\\(:[A-z:]+:\\)" .
                                          ((lambda (tag)
                                             (svg-tag-make tag :beg 1 :end -1))))))
              (svg-tag-mode t))))

;;------------------------------------------------------------------------------
;; Cape
;;------------------------------------------------------------------------------

(use-package! corfu
  :defer-incrementally t

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
        corfu-auto-prefix 5
        ;; corfu-on-exact-match 'show
        ;; corfu-preselect 'prompt         ; prompt first valid directory

        ;; corfu-preview-current nil
        ;; No preview vs Non-inserting preview
        ;;
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

  (add-hook! 'LaTeX-mode-hook
    (defun hook/set-latex-capf-functions ()
      (setq-local completion-at-point-functions
                  (list
                   ;; 'lsp-completion-at-point
                   'TeX--completion-at-point
                   'LaTeX--arguments-completion-at-point
                   'yasnippet-capf
                   'cape-tex
                   'cape-file))))

  (add-hook! 'emacs-lisp-mode-hook
    (defun hook/set-elisp-capf-functions ()
      (setq-local completion-at-point-functions
                  (list
                   ;; (cape-company-to-capf #'company-yasnippet)
                   'yasnippet-capf
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
                         'cape-keyword
                         'yasnippet-capf)))))

  (add-hook! 'vhdl-mode-hook
    (defun hook/set-vhdl-capf ()
      (setq-local completion-at-point-functions
                  (list (cape-capf-super
                         'cape-dabbrev
                         'cape-keyword
                         'yasnippet-capf)))))

  (dolist (mode '(python-ts-mode-hook python-mode-hook))
    (add-hook mode
              (defun hook/set-capf ()
                (setq-local completion-at-point-functions
                            (list
                             (cape-capf-super
                              'eglot-completion-at-point
                              'cape-keyword
                              'cape-file
                              'cape-dabbrev
                              'yasnippet-capf))))))

  (add-hook! 'tcl-mode-hook
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       'cape-dabbrev
                       'cape-keyword
                       'cape-file
                       'yasnippet-capf))))

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
  :defer-incrementally t

  :config

  (setq yas-snippet-dirs '("~/.doom.d/snippets"))

  )

;;------------------------------------------------------------------------------
;; Unused
;;------------------------------------------------------------------------------

(use-package! undo-tree
  :defer-incrementally t
  :when (featurep 'undo-tree)
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.undo-tree")))

  ;; https://github.com/doomemacs/doomemacs/issues/902
  (advice-remove '+undo--append-zst-extension-to-file-name-a
                 'undo-tree-make-history-save-file-name))

(use-package! emojify-mode
  :defer-incrementally t
  :when (featurep 'emojify-mode)
  :config
  (setq global-emojify-mode t))

(use-package! langtool
  :defer-incrementally t
  :when (featurep 'langtool)
  :config
  (setq langtool-java-classpath "/snap/languagetool/current/usr/share/java")
  (setq langtool-language-tool-server-jar "/snap/languagetool/current/usr/bin/languagetool.jar"))

;;------------------------------------------------------------------------------
;; Org Cliplink
;;------------------------------------------------------------------------------

;; add a space before cliplink insertion, otherwise the link just runs into the
;; preceding character e.g. ***[[...]]
(advice-add 'org-cliplink :before (lambda () (insert " ")))

;;------------------------------------------------------------------------------
;; Locals
;;------------------------------------------------------------------------------

;; Local Variables:
;; eval: (+fold/close-all)
;; End:
