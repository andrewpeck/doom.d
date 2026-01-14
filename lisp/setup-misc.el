;; config-packages.el -*- lexical-binding: t; -*-

;;------------------------------------------------------------------------------
;; System Install
;;------------------------------------------------------------------------------

(use-package system-install
  :commands (system-install-auto-refresh)
  :config
  (system-install-auto-refresh)
  (map! :map system-install-run-minor-mode-map
        :after system-install
        :n "q" #'bury-buffer))


;;------------------------------------------------------------------------------
;; Vterm
;;------------------------------------------------------------------------------

(use-package vterm
  :config
  (map! :leader (:prefix "o"
                 :desc "Vterm Toggle"         "t"  #'+vterm/toggle-here
                 :desc "Vterm Toggle Root"    "T"  #'+vterm/toggle)))

;;------------------------------------------------------------------------------
;; Rainbow
;;------------------------------------------------------------------------------

(use-package rainbow-delimiters-mode
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package highlight-indent-guides
  :config
  (defvar highlight-indent-guides-responsive) ;; pacify the byte compiler
  (defvar highlight-indent-guides-method)     ;; pacify the byte compiler
  (setq highlight-indent-guides-responsive nil
        ;; highlight-indent-guides-auto-enabled nil
        highlight-indent-guides-method 'bitmap))

;; clean the recent file list on idle
(use-package recentf
  :config
  (add-to-list 'recentf-exclude "^/mnt/Media/")
  (add-to-list 'recentf-exclude "^~/Downloads")
  (setq recentf-auto-cleanup 120))

;;------------------------------------------------------------------------------
;; Visual Fill Column
;;------------------------------------------------------------------------------

(use-package visual-fill-column
  :init

  (map! :leader :prefix "v" :desc "Toggle Visual Wrap"   "w"  #'my/toggle-wrap)
  (map! :leader :prefix "v" :desc "Toggle Visual Wrap"   "c"  #'my/toggle-wrap)
  (map! :leader :prefix "t" :desc "Toggle Visual Wrap"   "w"  #'my/toggle-wrap)
  (map! :localleader :desc "Olivetti Mode" "o" 'my/toggle-wrap-and-center) 

  ;; wrap in magit status mode
  (add-hook 'magit-status-mode-hook #'my/wrap))

;;------------------------------------------------------------------------------
;; Casual
;;------------------------------------------------------------------------------

(use-package casual
  :config
  (map! :map calc-mode-map     :n "?" #'casual-calc-tmenu
        :map dired-mode-map    :n "?" #'casual-dired-tmenu
        :map calendar-mode-map :n "?" #'casual-calendar-tmenu
        :map reb-mode-map      :n "?" #'casual-re-builder-tmenu))

;;------------------------------------------------------------------------------
;; Devdocs
;;------------------------------------------------------------------------------

(use-package devdocs

  :hook

  (prog-mode-hook . devdocs-setup-major-mode)
  
  :config

  (defun devdocs-setup-major-mode ()
    (require 'devdocs)
    (setq-local devdocs-current-docs
                (pcase major-mode
                  ;; doom-laserwave doom-one doom-gruvbox
                  ('emacs-lisp-mode (list "elisp"))
                  ('python-mode     (list "python~3.13"))
                  ('python-ts-mode  (list "python~3.13"))
                  ('tcl-mode        (list "tcl_tk~9.0"))
                  (_                nil))))

  (defun devdocs-lookup-at-pt ()
    (interactive)
    (let ((symbol (symbol-name (symbol-at-point))))
      (devdocs-lookup
       nil                              ; ASK-DOCS
       symbol)))                        ; INITIAL-INPUT

  (map! :map prog-mode-map "C-k" 'devdocs-lookup-at-pt))

;;------------------------------------------------------------------------------
;; Calc
;;------------------------------------------------------------------------------

(use-package calc
  :config 
  (setq calc-window-height 18))

;;------------------------------------------------------------------------------
;; Uniline
;;------------------------------------------------------------------------------

(use-package uniline
  :commands (uniline-launch-interface)
  :config
  (map! :map uniline-mode-map "I" #'uniline-launch-interface))

;;------------------------------------------------------------------------------
;; Bookmark
;;------------------------------------------------------------------------------

(use-package bookmark
  :custom
  ;; place bookmarks in the doom folder for version control
  (bookmark-default-file (concat doom-user-dir "bookmarks" "-" (system-name)))
  (bookmark-save-flag 1)) ;; save bookmarks after every op, not on kill

;;------------------------------------------------------------------------------
;; Auto revert
;;------------------------------------------------------------------------------

(use-package autorevert
  :init
  (run-when-idle 1 (global-auto-revert-mode t))
  :custom
  (auto-revert-avoid-polling nil)
  (auto-revert-interval 1)
  (auto-revert-use-notify nil)
  (auto-revert-check-vc-info t))

;;------------------------------------------------------------------------------
;; GPTel
;;------------------------------------------------------------------------------

(use-package gptel-magit
  :hook (magit-mode . gptel-magit-install)
  :commands (gptel-magit-commit-generate))

(defun ChatGPT ()
  (interactive)
  (if (member "Chat.md" (mapcar #'buffer-name (doom-visible-buffers)))
      (+popup/toggle)
    (find-file (concat doom-user-dir "Chat.md"))
    (gptel-mode)))

(use-package gptel
  :init

  ;; https://platform.openai.com/docs/models

  (set-popup-rule! "Chat.md"
    :side 'bottom
    :size 0.3
    :select t
    :quit t)

  (map! :leader :prefix "o"
        (:desc "GPTel" "g" #'ChatGPT
         :desc "GPTel Rewrite" "G"  #'gptel-rewrite))

  (map! :mode git-commit-mode :leader :prefix "m"
        :desc "GPTel Magit Commit Generate" "g"  #'gptel-magit-commit-generate)

  (map! :map org-mode-map      "C-c <return>" #'gptel-send
        :map markdown-mode-map "C-c <return>" #'gptel-send)

  (after! org-mode
    (require 'gptel)
    (require 'gptel-org))

  :config


  :custom

  (gptel-use-tools nil)
  (gptel-log-level 1)
  (gptel-model 'gpt-5)
  (gptel-default-mode 'org-mode))

(use-package gptel-org
  :after gptel
  :custom
  (gptel-org-branching-context nil))

;;------------------------------------------------------------------------------
;; Citar
;;------------------------------------------------------------------------------

(use-package citar
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup))

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
    ("j" evil-window-decrease-height)
    ("k" evil-window-increase-height)
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

(map! :after emacs-everywhere
      :map emacs-everywhere-mode-map
      "\C-c\C-c" #'emacs-everywhere-finish)

;;------------------------------------------------------------------------------
;; Midnight Mode
;;------------------------------------------------------------------------------

(use-package midnight
  :defer-incrementally t
  :config
  (add-to-list 'clean-buffer-list-kill-buffer-names "*Native-compile-Log*")
  (add-to-list 'clean-buffer-list-kill-buffer-names "*Async-native-compile-log*"))

;;------------------------------------------------------------------------------
;; Copyright
;;------------------------------------------------------------------------------

(use-package copyright

  :init

  (defun my/update-copyright ()
    "Update copyright on save."
    (interactive)
    (require 'copyright)
    (save-excursion
      (when (and copyright-names-regexp
                 (progn (goto-char (point-min))
                        (copyright-re-search copyright-names-regexp nil t)))
        (copyright-update nil t)
        (copyright-fix-years))))

  (add-to-list 'before-save-hook t)
  (add-hook 'before-save-hook 'my/update-copyright)

  :custom

  (copyright-year-ranges t))

;;------------------------------------------------------------------------------
;; Colorpicker
;;------------------------------------------------------------------------------

(use-package colorpicker
  :config
  (setq colorpicker--script "~/.config/emacs/.local/straight/repos/emacs-colorpicker/script/colorpicker.py"))

;;------------------------------------------------------------------------------
;; Large Table Edition
;;------------------------------------------------------------------------------

(use-package lte
  :init
  (add-hook 'org-mode-hook #'lte-truncate-table-mode)
  (add-hook 'markdown-mode-hook #'lte-truncate-table-mode)

  (map! (:map org-mode-map      "C-c C-e" #'my/org-dwim-edit-at-point)
        (:map markdown-mode-map "C-c C-e" #'lte-edit-table)))

;;------------------------------------------------------------------------------
;; Smartparens
;;------------------------------------------------------------------------------

(use-package smartparens
  :custom
  ; disable smartparens/automatic parentheses completion
  (smartparens-global-mode nil)
  (smartparens-mode nil))

;;------------------------------------------------------------------------------
;; Wavedrom
;;------------------------------------------------------------------------------

(use-package ob-wavedrom
  :config
  (setq ob-wavedrom-cli-path "wavedrom"))

;;------------------------------------------------------------------------------ 
;; Apheleia
;;------------------------------------------------------------------------------

(use-package apheleia
  :config

  ;; don't want to apply autoformatter for files with conflict markers in them
  ;; just add some simple advice to look for conflict markers and skip the
  ;; formatter, issuing a message along the way.
  (advice-add #'apheleia-format-buffer :before-until
              (defun file-has-conflict-markers (&rest _)
                (let ((is-conflict (save-excursion
                                     (goto-char (point-min))
                                     (re-search-forward "^<<<<<<< [A-z]+$" nil t))))
                  (if is-conflict
                      (message "File has conflict markers; not formatting.")
                    (message "Formatting buffer..."))

                  is-conflict)))

  (add-to-list 'apheleia-formatters '(autopep8 "autopep8" "-"))
  (add-to-list 'apheleia-formatters '(python-mode isort "isort"  "-ca" "--stdout" "-"))

  (add-to-list 'apheleia-mode-alist '(python-mode . ruff))
  (add-to-list 'apheleia-mode-alist '(python-ts-mode . ruff))

  (add-to-list 'apheleia-mode-alist '(python-mode . autopep8))
  (add-to-list 'apheleia-mode-alist '(python-ts-mode . autopep8)))

;;------------------------------------------------------------------------------
;; PDF View + Image Mode
;;------------------------------------------------------------------------------

(use-package pdf-sync
  :custom
  (pdf-sync-backward-display-action t))

(use-package pdf-view

  :commands (pdf-view-midnight-minor-mode pdf-view-midnight-update-colors pdf-rotate)

  :init

  (add-hook! 'pdf-view-mode-hook #'auto-revert-mode)
  (add-hook! 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode)

  :config

  (defun pdf-view-midnight-update-colors ()
    "Sync pdf view midnight colors to currently selected theme."
    (interactive)
    (setq pdf-view-midnight-colors
          `(,(face-attribute 'default :foreground) .
            ,(face-attribute 'default :background)))
    (dolist (buf (buffer-list (current-buffer)))
      (with-current-buffer buf
        (when pdf-view-midnight-minor-mode
          (pdf-view-midnight-minor-mode 1)))))

  (add-hook 'doom-load-theme-hook #'pdf-view-midnight-update-colors)

  (defun pdf-rotate (dir)
    "Rotate a pdf using Qpdf. Dir should either be + or -"
    (if (and (stringp dir) (or (string= "+" dir) (string= "-" dir)))
        (if (string= "pdf" (file-name-extension (buffer-file-name)))
            (shell-command (format "qpdf --rotate=%s90 --replace-input \"%s\"" dir (buffer-file-name)))
          (message (format "File %s is not a pdf" (buffer-file-name))))
      (message (format "Direction \'%s\' is not valid. Please use a direction \'+\' or \'-\'." dir))))

  (defun pdf-rotate-clockwise ()
    "Rotate a pdf clockwise using Qpdf"
    (interactive)
    (pdf-rotate "+")
    (revert-buffer))

  (defun pdf-rotate-counterclockwise ()
    "Rotate a pdf counterclockwise using Qpdf"
    (interactive)
    (pdf-rotate "-")
    (revert-buffer)))

(use-package image-mode
  :config
  (add-hook! 'image-mode-hook #'auto-revert-mode))

;;------------------------------------------------------------------------------
;; Affe
;;------------------------------------------------------------------------------

(use-package affe

  :commands (affe-find affe-grep)

  :init

  ;; (setq affe-find-command "rg --color=never --files")
  (setq affe-find-command (concat (or (executable-find "fd")
                                      (executable-find "fdfind"))
                                  " --color=never -L"))

  (defun affe-find-home    () (interactive) (affe-find "~/"))
  (defun affe-find-work    () (interactive) (affe-find "~/work"))
  (defun affe-find-project () (interactive) (affe-find (project-root-dir)))
  (defun affe-grep-project () (interactive) (affe-grep (project-root-dir)))
  (defun affe-find-notes   () (interactive) (affe-find "~/notes"))
  (defun affe-find-dotfile () (interactive) (affe-find "~/dotfiles"))

  ;; Affe
  (map! :leader :prefix "f" :desc "Open dotfile"         "."  'affe-find-dotfile)
  (map! :mn "C-o"   'affe-find-home
        :mn "C-S-Y" 'affe-find-work
        :mn "C-p"   'affe-find-project
        :mn "C-S-p" 'affe-grep-project
        :mn "C-S-n" 'affe-find-notes
        :mn "C-n"   'affe-find-psiq))

;;------------------------------------------------------------------------------
;; Undo
;;------------------------------------------------------------------------------

(use-package undo-fu-session
  ;; persistent undo
  :after undo-fu
  :config
  (setq undo-fu-session-directory (concat doom-user-dir ".undo-fu")))

(use-package undo-fu
  :config
  ;; disable confusing undo-fu behavior
  ;; https://codeberg.org/ideasman42/emacs-undo-fu/issues/6
  (setq undo-fu-ignore-keyboard-quit t))

;;------------------------------------------------------------------------------
;; Whitespace
;;------------------------------------------------------------------------------

(use-package ws-butler
  :config
  (setq ws-butler-global-exempt-modes
        '(special-mode comint-mode term-mode eshell-mode)))

;;------------------------------------------------------------------------------
;; Highlight Todos
;;------------------------------------------------------------------------------

(use-package hl-todo
  :config
  (setq global-hl-todo-mode t))

;;------------------------------------------------------------------------------
;; Savehist
;;------------------------------------------------------------------------------

(use-package savehist
  ;; save macros and other registers peristently
  :config
  (add-to-list 'savehist-additional-variables 'register-alist)

  (defun doom-clean-up-registers-h ()
    (setq-local register-alist (cl-remove-if-not 'savehist-printable register-alist)))
  (add-hook! 'savehist-save-hook 'doom-clean-up-registers-h))

;;------------------------------------------------------------------------------
;; Yasnippet
;;------------------------------------------------------------------------------

(use-package yasnippet-capf
  :after cape
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(use-package yasnippet

  :init

  (add-hook 'prog-mode-hook (lambda () (yas-global-mode 1)))
  (add-hook 'text-mode-hook (lambda () (yas-global-mode 1)))

  :custom

  (yas-snippet-dirs '("~/.doom.d/snippets"))
  (yas-also-auto-indent-first-line t)

  :config

  (setq yas-verbosity 2)

  ;; HACK: for some unknown reason yasnippet has started producing duplicate
  ;; templates managed to remove them just by applying this advice but it really
  ;; should be fixed properly
  (defun advice/remove-duplicate-snippets (orig-fun &rest args)
    (let ((templates (apply orig-fun args)))
      (when (car templates)
        ;; (debug)
        (cl-callf
            (lambda (tmpl) (cl-remove-duplicates tmpl :test (lambda (x y) (string= (car x) (car y)))))
            (car templates)) templates)))

  (advice-add 'yas--templates-for-key-at-point :around
              'advice/remove-duplicate-snippets)

  ;; Don't add newlines to snippet endings
  (add-hook 'snippet-mode-hook
            (lambda () (setq require-final-newline nil))))

;;------------------------------------------------------------------------------
;; Ispell
;;------------------------------------------------------------------------------

(use-package flyspell
  :commands (flyspell-get-word flyspell-do-correct)
  :config
  (defun my-save-word ()
    "Save user defined words to the dictionary"
    (interactive)
    (let ((current-location (point))
          (word (flyspell-get-word)))
      (when (consp word)
        (flyspell-do-correct 'save nil
                             (car word) current-location (cadr word)
                             (caddr word) current-location)))))

(use-package ispell
  :init
  ;; Find Local Dictionaries
  (defun hook/set-ispell-dict ()
    (when-let* ((buf (buffer-file-name))
                (dict (locate-dominating-file buf ".aspell.en.pws")))
      (setq-local ispell-personal-dictionary
                  (concat dict ".aspell.en.pws"))))
  (add-hook 'flycheck-mode-hook 'hook/set-ispell-dict))

(use-package jinx
  :config
  (global-jinx-mode nil)
  (add-hook 'org-mode-hook (lambda () (jinx-mode 1)))
  (add-hook 'tex-mode-hook (lambda () (jinx-mode 1)))
  (add-hook 'markdown-mode-hook (lambda () (jinx-mode 1)))

  (global-set-key [remap ispell-word] #'jinx-correct-word))

;;------------------------------------------------------------------------------
;; Lookup
;;------------------------------------------------------------------------------

(run-when-idle 1

          ;; calling +lookup/documentation annoyingly moves the cursor to the other window
          ;; just add some advice to move it back
          (advice-add '+lookup/documentation :around
                      (lambda (orig-fun &rest args)
                        (let ((current (selected-window)))
                          (apply orig-fun args)
                          (select-window current))))

          (defun advice/close-lookup-maybe (_id &rest _arg)
            "Repeated invotations of +lookup/documentation will simply toggle the
help instead of keeping it open."
            (let ((buffers (seq-filter (lambda (buf)
                                         (or (string-prefix-p "*eglot-help" (buffer-name buf))
                                             (string-prefix-p "*helpful " (buffer-name buf))))
                                       (buffer-list))))
              (when buffers
                (mapc #'kill-buffer buffers))

              buffers))

          (advice-add #'+lookup/documentation :before-until
                      #'advice/close-lookup-maybe))

;;------------------------------------------------------------------------------
;; Time
;;------------------------------------------------------------------------------

(use-package time
  :config
  (setq world-clock-list
        '(("America/Los_Angeles" "Los Angeles")
          ("America/New_York" "Boston")
          ("Europe/London" "London")
          ("Europe/Paris" "Geneva"))))

;;------------------------------------------------------------------------------
;; Eldoc
;;------------------------------------------------------------------------------

(use-package eldoc
  :config
  (setq eldoc-echo-area-prefer-doc-buffer t
        eldoc-idle-delay 0.5
        eldoc-echo-area-use-multiline-p t))

;;------------------------------------------------------------------------------
;; Vc hooks
;;------------------------------------------------------------------------------

(use-package vc-hooks
  :config
  ;; set vc-ignore-dir-regexp to the default emacs value; doom overwrites this to
  ;; a value that ignores any remote directories, causing git-gutter etc to not
  ;; work through tramp
  (setq vc-ignore-dir-regexp
        "\\`\\(?:[\\/][\\/][^\\/]+[\\/]\\|/\\(?:net\\|afs\\|\\.\\.\\.\\)/\\)\\'"))

;;------------------------------------------------------------------------------
;; Comint
;;------------------------------------------------------------------------------

(use-package comint

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
        comint-scroll-show-maximum-output t))

;;------------------------------------------------------------------------------
;; ielm
;;------------------------------------------------------------------------------

(use-package ielm
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
;; svg-tag-mode
;;------------------------------------------------------------------------------

(use-package svg-tag-mode
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
;;
;;------------------------------------------------------------------------------

(use-package undo-tree
  :defer-incrementally t
  :when (featurep 'undo-tree)
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.undo-tree")))

  ;; https://github.com/doomemacs/doomemacs/issues/902
  (advice-remove '+undo--append-zst-extension-to-file-name-a
                 'undo-tree-make-history-save-file-name))

;;------------------------------------------------------------------------------
;;
;;------------------------------------------------------------------------------

(use-package emojify-mode
  :defer-incrementally t
  :when (featurep 'emojify-mode)
  :config
  (setq global-emojify-mode t))

;;------------------------------------------------------------------------------
;; Org Cliplink
;;------------------------------------------------------------------------------

(use-package org-cliplink
  :after org
  :init
  ;; add a space before cliplink insertion, otherwise the link just runs into the
  ;; preceding character e.g. ***[[...]]
  (advice-add 'org-cliplink :before (lambda () (insert " "))))

;;------------------------------------------------------------------------------
;; Drag-Stuff
;;------------------------------------------------------------------------------

(use-package drag-stuff
  :after (:any text-mode prog-mode)
  :commands (drag-stuff-define-keys)
  :init
  (drag-stuff-define-keys))

;;------------------------------------------------------------------------------
;; Vim Tab Bar
;;------------------------------------------------------------------------------

(use-package vim-tab-bar
  :commands vim-tab-bar-mode
  :init
  (vim-tab-bar-mode))

;;------------------------------------------------------------------------------
;; Locals
;;------------------------------------------------------------------------------

;; Local Variables:
;; eval: (when nil (+fold/close-all))
;; End:
