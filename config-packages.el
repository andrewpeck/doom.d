;; config-packages.el -*- lexical-binding: t; -*-

;; was getting this error for some reason:
;; (2 nil)
;; (let nil (* 2 (symbol-value +word-wrap--major-mode-indent-var)))
;; (cond ((eq +word-wrap-extra-indent 'double) (let nil (* 2 (symbol-value +word-wrap--major-mode-indent-var)))) ((eq +word-wrap-extra-indent 'single) (let nil (symbol-value +word-wrap--major-mode-indent-var))) ((integerp +word-wrap-extra-indent) (let ((fixed +word-wrap-extra-indent)) fixed)) (t (let nil 0)))
;; (if (not (or +word-wrap--major-mode-is-text (doom-point-in-string-or-comment-p p))) (cond ((eq +word-wrap-extra-indent 'double) (let nil (* 2 (symbol-value +word-wrap--major-mode-indent-var)))) ((eq +word-wrap-extra-indent 'single) (let nil (symbol-value +word-wrap--major-mode-indent-var))) ((integerp +word-wrap-extra-indent) (let ((fixed +word-wrap-extra-indent)) fixed)) (t (let nil 0))) 0)
;; +word-wrap--calc-extra-indent(1)
;; (let ((adaptive-wrap-extra-indent (+word-wrap--calc-extra-indent beg))) (funcall fn beg end))
;; +word-wrap--adjust-extra-indent-a(#<subr adaptive-wrap-fill-context-prefix> 1 22)
;; apply(+word-wrap--adjust-extra-indent-a #<subr adaptive-wrap-fill-context-prefix> (1 22))
(setq-default +word-wrap--major-mode-indent-var 'standard-indent)

;;------------------------------------------------------------------------------
;; Citar
;;------------------------------------------------------------------------------

(use-package citar
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup))

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
;; Emacs Everywhere
;;------------------------------------------------------------------------------

(use-package! emacs-everywhere
  :defer-incrementally t
  :config
  (define-key emacs-everywhere-mode-map "\C-c\C-c" #'emacs-everywhere-finish))

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
;; Treesitter
;;------------------------------------------------------------------------------

(use-package! treesit-auto
  :commands (global-treesit-auto-mode
             treesit-auto-install-all
             treesit-auto-add-to-auto-mode-alist)
  :custom
  (setopt treesit-font-lock-level 5)
  (treesit-auto-install 'prompt)
  :config
  (delete 'janet treesit-auto-langs)
  (delete 'markdown treesit-auto-langs)
  (delete 'latex treesit-auto-langs)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package! org-excalidraw
  :config

  (defun org-excalidraw--shell-cmd-to-svg (path)
    "Construct shell cmd for converting excalidraw file with PATH to svg."
    (concat "excalidraw_export " (format "\"%s\"" path)))

  ;; https://github.com/wdavew/org-excalidraw/issues/7
  (defun org-excalidraw--handle-file-change (event)
    "Handle file update EVENT to convert files to svg."
    ;; (cadr event) can be 'changed or 'renamed
    ;; e.g. for changed (10 changed /some/where/ID.excalidraw)
    ;; e.g. for renamed (10 renamed /some/where/ID.excalidraw.crswap /some/where/ID.excalidraw)
    ;; note we use memq, because comparing symbols
    (when (memq (cadr event) '(changed renamed))
      ;; use eq because comparing symbols
      (let ((filename (if (eq (cadr event) 'changed)
                          (caddr event)
                        (cadddr event))))
        (when (string-suffix-p ".excalidraw" filename)
          (shell-command (org-excalidraw--shell-cmd-to-svg filename))))))

  )

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


(use-package! vc-hooks
  :config
  ;; set vc-ignore-dir-regexp to the default emacs value; doom overwrites this to
  ;; a value that ignores any remote directories, causing git-gutter etc to not
  ;; work through tramp
  (setq vc-ignore-dir-regexp
        "\\`\\(?:[\\/][\\/][^\\/]+[\\/]\\|/\\(?:net\\|afs\\|\\.\\.\\.\\)/\\)\\'"))

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

(use-package! pdf-sync
  :config
  (pop-to-buffer (or (find-buffer-visiting "~/.doom.d/config-packages.el")
                     (find-file-noselect  "~/.doom.d/config-packages.el")))


  )
;;------------------------------------------------------------------------------
;; Yasnippet
;;------------------------------------------------------------------------------

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
;; Drag-Stuff
;;------------------------------------------------------------------------------

(use-package! drag-stuff
  :config
  (drag-stuff-define-keys))

;;------------------------------------------------------------------------------
;; Locals
;;------------------------------------------------------------------------------

;; Local Variables:
;; eval: (+fold/close-all)
;; End:
