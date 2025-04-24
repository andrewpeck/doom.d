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

(defun copy-file-name-to-kill ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

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
  (global-auto-revert-mode t)
  :custom
  (auto-revert-avoid-polling nil)
  (auto-revert-interval 1)
  (auto-revert-use-notify nil)
  (auto-revert-check-vc-info t))

;;------------------------------------------------------------------------------
;; GPTel
;;------------------------------------------------------------------------------

(use-package! gptel
  :config
  (setq gptel-model 'gpt-4o))

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

;;------------------------------------------------------------------------------
;; Copyright
;;------------------------------------------------------------------------------

(use-package! copyright

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
;; Large Table Edition
;;------------------------------------------------------------------------------

(use-package! lte
  :init
  (add-hook 'org-mode-hook #'lte-truncate-table-mode)
  (add-hook 'markdown-mode-hook #'lte-truncate-table-mode)

  (defun my/org-dwim-edit-at-point ()
    "Try to edit the thing at point in some sensible way."
    (interactive)
    (cond
     ((string= "TBLFM" (cdr (org-thing-at-point))) (org-edit-special))
     ((org-at-table-p) (lte-edit-table))
     ((org--link-at-point) (org-insert-link))
     ((org-in-src-block-p) (org-edit-src-code))
     ((org-inside-LaTeX-fragment-p) (org-edit-latex-fragment))
     ((org-footnote-at-reference-p) (org-edit-footnote-reference))))


  (after! org (define-key org-mode-map  (kbd  "C-c C-e") #'my/org-dwim-edit-at-point))

  (after! markdown-mode (define-key markdown-mode-map  (kbd  "C-c C-e") #'lte-edit-table)))

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

(use-package pdf-sync
  :custom
  (pdf-sync-backward-display-action t))

(use-package! pdf-view

  :init

  (add-hook! 'pdf-view-mode-hook #'auto-revert-mode)
  (add-hook! 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode)

  :config

  (evil-define-key '(motion normal) pdf-view-mode-map
    (kbd "q") #'kill-current-buffer)

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

(use-package! image-mode
  :config
  (add-hook! 'image-mode-hook #'auto-revert-mode))

;;------------------------------------------------------------------------------
;; Affe
;;------------------------------------------------------------------------------

(use-package! affe

  :commands (affe-find affe-grep)

  :init

  ;; (setq affe-find-command "rg --color=never --files")
  (setq affe-find-command (concat (or (executable-find "fd")
                                      (executable-find "fdfind"))
                                  " --color=never -L"))

  (defun affe-find-home    () (interactive) (affe-find "~/"))
  (defun affe-find-work    () (interactive) (affe-find "~/work"))
  (defun affe-find-project () (interactive) (affe-find (projectile-project-root)))
  (defun affe-grep-project () (interactive) (affe-grep (projectile-project-root)))
  (defun affe-find-notes   () (interactive) (affe-find "~/notes"))
  (defun affe-find-dotfile () (interactive) (affe-find "~/dotfiles"))

  ;; Affe
  (after! evil-maps
    (map! :leader :prefix "f" :desc "Open dotfile"         "."  #'affe-find-dotfile)
    (evil-define-key '(motion normal) 'global
      (kbd "C-o")   #'affe-find-home
      (kbd "C-S-Y") #'affe-find-work
      (kbd "C-p")   #'affe-find-project
      (kbd "C-S-p") #'affe-grep-project
      (kbd "C-n")   #'affe-find-notes
      (kbd "C-n")   #'affe-find-notes)))

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
  :init

  ;; Find Local Dictionaries
  (defun hook/set-ispell-dict ()
    (when-let*
        ((buf (buffer-file-name))
         (dict (locate-dominating-file buf ".aspell.en.pws")))
      (setq-local ispell-personal-dictionary
                  (concat dict ".aspell.en.pws"))))
  (add-hook 'flycheck-mode-hook 'hook/set-ispell-dict)

  :config
  ;; Save user defined words to the dictionary
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
;; Lookup
;;------------------------------------------------------------------------------

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
            #'advice/close-lookup-maybe)

;;------------------------------------------------------------------------------
;; Eldoc
;;------------------------------------------------------------------------------

(use-package! eldoc
  :init

  (add-hook 'eglot-managed-mode-hook #'eglot-inlay-hints-mode t)
  ;; (add-hook 'eglot-managed-mode-hook (lambda () (eldoc-mode nil)) t)
  ;; (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t)

  :config

  (map! :leader :desc "Eldoc Help at Point" "kk" 'eldoc-box-help-at-point)
  (map! :leader :desc "Eldoc Help at Point" "kh" 'eldoc-box-hover-at-point-mode)

  (setq eldoc-echo-area-prefer-doc-buffer t
        eldoc-idle-delay 0.5
        eldoc-echo-area-use-multiline-p t))

;;------------------------------------------------------------------------------
;; Vc hooks
;;------------------------------------------------------------------------------

(use-package! vc-hooks
  :config
  ;; set vc-ignore-dir-regexp to the default emacs value; doom overwrites this to
  ;; a value that ignores any remote directories, causing git-gutter etc to not
  ;; work through tramp
  (setq vc-ignore-dir-regexp
        "\\`\\(?:[\\/][\\/][^\\/]+[\\/]\\|/\\(?:net\\|afs\\|\\.\\.\\.\\)/\\)\\'"))

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
                     (find-file-noselect  "~/.doom.d/config-packages.el"))))

;;------------------------------------------------------------------------------
;; Yasnippet
;;------------------------------------------------------------------------------

(use-package! yasnippet
  :defer-incrementally t

  :config

  (setq yas-snippet-dirs '("~/.doom.d/snippets")))

;;------------------------------------------------------------------------------
;;
;;------------------------------------------------------------------------------

(use-package! undo-tree
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

(use-package! emojify-mode
  :defer-incrementally t
  :when (featurep 'emojify-mode)
  :config
  (setq global-emojify-mode t))

;;------------------------------------------------------------------------------
;;
;;------------------------------------------------------------------------------

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

;;---------------------------------------------------------------------------------
;; Mixed Pitch Mode
;;---------------------------------------------------------------------------------

;; (add-hook 'org-mode-hook      #'mixed-pitch-mode)
;; (add-hook 'markdown-mode-hook #'mixed-pitch-mode)
;; (add-hook 'latex-mode-hook    #'mixed-pitch-mode)

;;------------------------------------------------------------------------------
;; Drag-Stuff
;;------------------------------------------------------------------------------

(use-package! drag-stuff
  :commands (drag-stuff-define-keys)
  :init
  (drag-stuff-define-keys))

;;------------------------------------------------------------------------------
;; Locals
;;------------------------------------------------------------------------------

;; Local Variables:
;; eval: (+fold/close-all)
;; End:
