;; config.el -*- lexical-binding: t; -*-
;;
;; TODO tabular-like alignment
;; TODO magit.sh like functionality
;; TODO outline folding
;;
;; Tecosaur: https://github.com/tecosaur/emacs-config/blob/master/config.org
;; Steve Purcell: https://github.com/purcell/emacs.d/

;; Bookmarks

(setq bookmark-default-file "~/.doom.d/bookmarks")

(load  "~/.doom.d/config-git.el")
(load  "~/.doom.d/config-evil.el")
(load  "~/.doom.d/config-theme.el")
(load  "~/.doom.d/config-lsp.el")
(load  "~/.doom.d/config-doom.el")
(load  "~/.doom.d/config-tex.el")
(load  "~/.doom.d/config-align.el")
(load  "~/.doom.d/config-org.el")
(load  "~/.doom.d/config-company.el")
(load  "~/.doom.d/lisp/system-install.el")
(load  "~/.doom.d/lisp/regulator.el")
(load  "~/.doom.d/lisp/tracking.el")
(load  "~/.doom.d/lisp/verilog-port-copy.el")
(load  "~/.doom.d/lisp/doctor.el")

(defun +vc--remote-homepage ()
  (require 'browse-at-remote)
  (or (let ((url (browse-at-remote--remote-ref)))
        (cdr (browse-at-remote--get-url-from-remote (car url))))
      (user-error "Can't find homepage for current project")))

;;------------------------------------------------------------------------------
;;;;; to sort
;;------------------------------------------------------------------------------

;;(ad-remove-advice 'replace-highlight 'before 'anzu-replace-highlight)

;; save macros and other registers peristently
(after! savehist
  (add-to-list 'savehist-additional-variables 'register-alist)
  (add-hook! 'savehist-save-hook
    (defun doom-clean-up-registers-h ()
      (setq-local register-alist
                  (cl-remove-if-not #'savehist-printable register-alist)))))

(after! tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(map! :n [mouse-8] #'previous-buffer
      :n [mouse-9] #'next-buffer)

(setq-default delete-by-moving-to-trash t         ; Delete files to trash
              window-combination-resize t         ; take new window space from all other windows (not just current)
              x-stretch-cursor t)                 ; Stretch cursor to the glyph width

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…")               ; Unicode ellispis are nicer than "...", and also save /precious/ space

(display-time-mode 1)                             ; Enable time in the mode-line

(global-subword-mode 1)                           ; Iterate through CamelCase words
;; (modify-syntax-entry ?_ "w")                   ; Treat underscore as part of a word to match vim behavior

;; better dired soring
(after! dired
  (setq dired-listing-switches "-a1vBhl  --group-directories-first"))

;; add a margin while scrolling
(setq scroll-margin 30)

;; persistent undo
(after! undo
  (setq undo-tree-auto-save-history t))

;; Bookmarks
(setq bookmark-default-file "~/.doom.d/bookmarks")

(defun fix-ssh-permissions ()
  "Fix the ssh permissions on host computer"
  (interactive)
  (shell-command "chmod o-w ~/")
  (shell-command "chmod 700 ~/.ssh")
  (shell-command "chmod 600 ~/.ssh/authorized_keys"))

(setq enable-local-variables t)

;;; Random
;;;------------------------------------------------------------------------------

;; WINDOW TITLE :: https://www.emacswiki.org/emacs/FrameTitle
(setq frame-title-format
      `((buffer-file-name "%f" "%b")
        ,(format " - emacs %s" emacs-version)))

;; Start emacs in full screen by default
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Fill column width
(setq-default fill-column 80)

;; Turn on menu bar
(menu-bar-mode 1)

;; Increase the amount of data which Emacs reads from the process.
;; Again the emacs default is too low 4k considering that the some
;; of the language server responses are in 800k - 3M range.
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Etags search depth
(setq etags-table-search-up-depth 10)

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;;
(setq-default tab-width 2)

;; Clear buffers at midnight
(midnight-mode)

;; disable smartparens/automatic parentheses completion
(setq smartparens-global-mode nil)
(setq smartparens-mode nil)
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

;;; SLIME
;;------------------------------------------------------------------------------

;; slime
;;(after! slime
;;  (setq inferior-lisp-program "sbcl")
;;  (setq org-babel-lisp-eval-fn 'slime-eval))

;;; Spell-Checking
;;------------------------------------------------------------------------------

(after! writegood
  (writegood-passive-voice-turn-off))

;; Save user defined words to the dictionary
(after! ispell
  (setq ispell-personal-dictionary "~/.aspell.en.pws")
  (defun my-save-word () (interactive)
         (let ((current-location (point)) (word (flyspell-get-word)))
           (when (consp word)
             (flyspell-do-correct 'save nil
                                  (car word) current-location (cadr word)
                                  (caddr word) current-location)))))

;;; Snippets
;;------------------------------------------------------------------------------

;; Don't add newlines to snippet endings
(after! yasnippet
  (setq-default yas-also-auto-indent-first-line t)
  (add-hook 'snippet-mode-hook
            (lambda () (setq require-final-newline nil))))

;;; Projectile
;;------------------------------------------------------------------------------

(after! projectile
  (setq projectile-sort-order 'recently-active))

;;; Appearance
;;------------------------------------------------------------------------------

(after! highlight-indent-guides
  ;;(setq highlight-indent-guides-auto-enabled nil)
  (setq highlight-indent-guides-responsive nil
        highlight-indent-guides-method 'bitmap))

;; All the icons
(use-package! all-the-icons
  :defer-incrementally t
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  (all-the-icons-ibuffer-mode 1))

(after! ivy
  (all-the-icons-ivy-setup))

;; Theme
(add-to-list 'load-path "~/.doom.d/themes/")
(add-to-list 'custom-theme-load-path "~/.doom.d/themes/")

(cond
 ((string= (system-name) "strange") (setq doom-theme 'doom-material))
 ((string= (system-name) "pepper")  (setq doom-theme 'doom-gruvbox))
 ((string= (system-name) "larry")   (setq doom-theme 'summerfruit))
 (t (setq doom-theme 'summerfruit)))

;;; FONT
;;------------------------------------------------------------------------------

(setq doom-font (font-spec :family "JetBrains Mono" :size 14)
      doom-big-font (font-spec :family "JetBrains Mono" :size 14)
      doom-variable-pitch-font (font-spec :family "IBM Plex Mono"   :size 17)
      ;;doom-variable-pitch-font (font-spec :family "Comic Sans MS"   :size 17)
      doom-serif-font (font-spec :family "IBM Plex Mono" :weight 'light))

;; Syntax Highlighting
;;------------------------------------------------------------------------------

;; enable syntax highlighting for vimrc files
(add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode))

(add-to-list 'auto-mode-alist '("\\.xdc\\'" . tcl-mode))  ;; tcl mode for xdc files
(add-to-list 'auto-mode-alist '("\\.ltx\\'" . json-mode)) ;; json mode for ltx files
(add-to-list 'auto-mode-alist '("\\.src\\'" . tcl-mode))  ;; tcl mode for hog files
(add-to-list 'auto-mode-alist '("\\.con\\'" . tcl-mode))  ;; tcl mode for hog files
(add-to-list 'auto-mode-alist '("\\.lst\\'" . tcl-mode))  ;; tcl mode for hog files
(add-to-list 'auto-mode-alist '("\\.ino\\'" . cpp-mode))  ;; cpp mode for arduino files

;;; Rainbow Delimeters
;;------------------------------------------------------------------------------

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;; Mixed Pitch Mode
;;------------------------------------------------------------------------------

;; (add-hook 'org-mode-hook      #'mixed-pitch-mode)
;; (add-hook 'markdown-mode-hook #'mixed-pitch-mode)
;; (add-hook 'latex-mode-hook    #'mixed-pitch-mode)

;;; Line wrapping
;;------------------------------------------------------------------------------

(defun ap/no-wrap ()
  (interactive)
  (visual-line-mode 0)
  (toggle-truncate-lines 1)
  (visual-fill-column-mode 0))

;; Disable auto fill mode in text modes
(remove-hook 'text-mode-hook #'auto-fill-mode)

;; Don't wrap text modes unless we really want it
(remove-hook 'text-mode-hook #'+word-wrap-mode)

(defun fix-visual-fill-column-mode (&optional ARG)
  (setq visual-fill-column-mode visual-line-mode))

;; toggle visual-fill column mode when chaing word wrap settings
(advice-add '+word-wrap-mode
            :after 'fix-visual-fill-column-mode)
;;
(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)

;;; True/False Faces
;;------------------------------------------------------------------------------

(defface my/highlight-true-face
  '((t :foreground "#2a2" :weight bold))
  "Highlight face for true"
  :group 'basic-faces)

(defface my/highlight-false-face
  '((t :foreground "#f22" :weight bold))
  "Highlight face for false"
  :group 'basic-faces)

(defun my/highlight-true ()
  "Use hi-lock to highlight specific words"
  (hi-lock-face-buffer "\\b\\(true\\|True\\)\\b" 'my/highlight-true-face))

(defun my/highlight-false ()
  "Use hi-lock to highlight specific words"
  (hi-lock-face-buffer "\\b\\(false\\|False\\)\\b" 'my/highlight-false-face))

(add-hook 'vhdl-mode-hook #'my/highlight-false)
(add-hook 'vhdl-mode-hook #'my/highlight-true)
(add-hook 'python-mode-hook #'my/highlight-false)
(add-hook 'python-mode-hook #'my/highlight-true)

;;; VHDL Mode
;;------------------------------------------------------------------------------

(defvar hog-vivado-path "~/Xilinx/Vivado/2020.2/settings64.sh")
(defvar hog-number-of-jobs 4)

;; vhdl mode will wrap comments after some # of characters
(setq vhdl-end-comment-column 200
      vhdl-prompt-for-comments nil
      auto-fill-mode nil)


;;; Notes
;;------------------------------------------------------------------------------

;;(set-time-zone-rule "GMT-4")


;;; Elfeed
;;------------------------------------------------------------------------------

(after! elfeed
  (setq
   elfeed-feeds
   '("https://hackaday.com/blog/feed/")))

;;; IELM
;;------------------------------------------------------------------------------

;; remember ielm history
;; global copy of the buffer-local variable
(after! ielm
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

  (require 'ielm)
  (add-hook 'inferior-emacs-lisp-mode-hook #'set-ielm-comint-input-ring)
  )

;;; XML
;;------------------------------------------------------------------------------

(after! nxml
  (add-hook
   'nxml-mode-hook
   (setq nxml-child-indent 2 nxml-attribute-indent 2)))

;; (add-hook 'nxml-mode-hook (lambda () (visual-fill-column-mode -1)))
;; (defun nxml-pretty-format ()
;;   (interactive)
;;   (save-excursion
;;     (shell-command-on-region (point-min) (point-max)
;;     "xmllint --format -" (buffer-name) t)
;;     (nxml-mode)
;;     (indent-region begin end)))

;;; Markdown
;;------------------------------------------------------------------------------

;; Make Fundamental Mode GFM by default
(after! gfm
  (setq initial-major-mode 'gfm-mode))

;; C mode
;;-----------------------------------------------------------------------------------------

;; For example, if you prefer double slashes // instead of slash-stars /* ... */
;; in c-mode, insert below code into your ~/.emacs:
(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Preferred comment style
            (setq comment-start "// " comment-end "")))

;; Clojure
;;-----------------------------------------------------------------------------------------

(setq org-babel-clojure-backend "cider")
