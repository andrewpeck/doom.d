;; config.el -*- lexical-binding: t; no-byte-compile: t;-*-
;;
;; Interesting Packages:
;; https://github.com/alphapapa/taxy.el
;; https://github.com/alphapapa/activities.el
;;
;; Interesting Emacs Configurations
;;
;; Tecosaur: https://github.com/tecosaur/emacs-config/blob/master/config.org
;; Karl Fogel: https://svn.red-bean.com/repos/kfogel/trunk/.emacs
;; Steve Purcell: https://github.com/purcell/emacs.d/
;; Henrik: https://github.com/hlissner/doom-emacs-private/blob/master/config.el
;; Collection of emacs configs: https://github.com/caisah/emacs.dz
;; https://github.com/danilevy1212/doom
;; https://github.com/jishnusen/emacs-config
;;
;; - TODO replace with thing from kill ring ;;
;; - TODO +compile-to-vterm-global-minor-mode ???
;; - TODO error with SPC-o-d
;; https://news.ycombinator.com/item?id=6965433

;; If non-nil, display instructions on how to exit the client on connection.
(setopt server-client-instructions nil)

(setopt use-package-hook-name-suffix nil
        use-package-always-defer t
        use-package-compute-statistics t)

(setopt browse-url-browser-function 'browse-url-firefox)

(load! "core-defuns.el" doom-user-dir t)

;;------------------------------------------------------------------------------
;; Shhh...
;;------------------------------------------------------------------------------

(advise-inhibit-messages 'recentf-cleanup)
(advise-inhibit-messages 'eglot)
;; Suppress `Package cl is deprecated` warnings
(setq byte-compile-warnings '(not obsolete)) ;; errors with setopt; bug?

(add-to-list 'warning-suppress-types '(iedit))

;;------------------------------------------------------------------------------
;; Packages & Loads
;;------------------------------------------------------------------------------

(when-let ((env (getenv "VIRTUAL_ENV")))
  (message (concat "VIRTUAL_ENV already set to " env
                   ". It is probably set in doom env and should be removed."))
  (setenv "VIRTUAL_ENV" nil))

;; something was setting this to C
(setopt current-locale-environment "en_US.UTF-8")

(setopt copyright-names-regexp ".*Andrew Peck*")

(add-load-path! "")
(add-load-path! "lisp/")
(add-load-path! "my-autoloads/")

(load (expand-file-name "loaddefs.el" doom-user-dir) nil t)

;;------------------------------------------------------------------------------
;; Shell Config
;;------------------------------------------------------------------------------

;; Fish (and possibly other non-POSIX shells) is known to inject garbage
;; output into some of the child processes that Emacs spawns. Many Emacs
;; packages/utilities will choke on this output, causing unpredictable
;; issues. To get around this, either:
(setopt shell-file-name (executable-find "bash"))
(with-eval-after-load 'vterm
  (setopt vterm-shell (executable-find "fish")))
(with-eval-after-load 'shell
  (setopt explicit-shell-file-name (executable-find "fish")))

;;------------------------------------------------------------------------------
;; Mode aliases
;;------------------------------------------------------------------------------

;; enable syntax highlighting for abnormal files
(add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode))   ; vimrc
(add-to-list 'auto-mode-alist '("\\.xdc\\'"          . vivado-mode))  ; vivado xdc
(add-to-list 'auto-mode-alist '("\\.ltx\\'"          . json-mode))    ; vivado ltx
(add-to-list 'auto-mode-alist '("\\.qel\\'"          . tcl-mode))     ; cadence script
(add-to-list 'auto-mode-alist '("\\.sdc\\'"          . tcl-mode))     ; synopsys sdc
(add-to-list 'auto-mode-alist '("\\.ino\\'"          . cpp-mode))     ; arduino
(add-to-list 'auto-mode-alist '("\\.src\\'"          . hog-src-mode)) ; hog
(add-to-list 'auto-mode-alist '("\\.cheby\\'"        . yaml-mode))    ; cheby
(add-to-list 'auto-mode-alist '("\\.bb\\'"           . clojure-mode)) ; babashka

;; Sibling File Rules
(add-to-list 'find-sibling-rules '("\\([^/]+\\)\\.c\\'" "\\1.h")) ;; c to h
(add-to-list 'find-sibling-rules '("\\([^/]+\\)\\.h\\'" "\\1.c")) ;; h to c
(add-to-list 'find-sibling-rules '("\\([^/]+\\)\\.drawio\\'" "\\1.pdf")) ;; drawio to pdf
(add-to-list 'find-sibling-rules '("\\([^/]+\\)\\.pdf\\'" "\\1.drawio")) ;; pdf to drawio

;;------------------------------------------------------------------------------
;; Misc
;;------------------------------------------------------------------------------

(add-hook 'text-mode-hook #'abbrev-mode)

;; Start emacs in full screen by default
(add-to-list 'default-frame-alist
             '(fullscreen . maximized))

(use-package emacs

  :config

  ;; buffer-local variables
  (setq-default display-line-numbers nil
                fill-column 80
                tab-width 2
                delete-by-moving-to-trash t ; Delete files to trash
                window-combination-resize t) ; take new window space from all other windows (not just current)

  ;; something is overriding these
  (setopt compilation-scroll-output t
          auto-revert-mode t              ;
          auto-revert-remote-files t      ;
          undo-limit 80000000)            ; Raise undo-limit to 80Mb

  (scroll-bar-mode)

  (setopt mouse-wheel-scroll-amount-horizontal 32
          mouse-wheel-tilt-scroll t
          save-interprogram-paste-before-kill t
          enable-local-variables t        ;
          help-at-pt-display-when-idle 'never ; this prevents a pretty annoying help display that pops up in the minibuffer, esp in dired
          scroll-margin 30                    ; add a margin while scrolling
          so-long-threshold 800               ; so-long-threshold can increase
          auto-save-default t      ; Nobody likes to loose work, I certainly don't
          truncate-string-ellipsis "…" ; Unicode ellispis are nicer than "...", and also save /precious/ space
          x-stretch-cursor t           ; Stretch cursor to the glyph width

          abbrev-file-name (concat doom-user-dir "abbrev_defs")

          ;; +format-on-save-enabled-modes
          ;; '(not yaml-mode python-mode emacs-lisp-mode
          ;;   sql-mode tex-mode latex-mode org-msg-edit-mode)

          ;; Increase the amount of data which Emacs reads from the process.
          ;; Again the emacs default is too low 4k considering that the some
          ;; of the language server responses are in 800k - 3M range.
          read-process-output-max (* 1024 1024) ;; 1mb

          ;; Window Title
          ;; https://www.emacswiki.org/emacs/FrameTitle
          frame-title-format
          '(:eval
            (cond
             (dired-directory (concat (abbreviate-file-name dired-directory) " - Dired" ))
             (buffer-file-name (concat
                                (abbreviate-file-name buffer-file-name)
                                (when (buffer-modified-p) " * ")))
             (t (buffer-name))))
          ;; window title when minimzed--- just make it the same
          icon-title-format frame-title-format)

  (midnight-mode)           ; Clear buffers at midnight
  (display-time-mode 1)     ; Enable time in the mode-line
  (global-subword-mode 0))  ; Separate CamelCase words?

;;------------------------------------------------------------------------------
;; Config Loading
;;------------------------------------------------------------------------------

(load!! "custom")
(load!! "lisp/setup-appearance.el")
(load!! "lisp/setup-modeline.el")
(load!! "lisp/setup-doom.el")
(load!! "lisp/setup-fonts.el")

;; Load setup files
(dolist (file (file-expand-wildcards (concat doom-user-dir "lisp/setup*.el")))
  (load-idle (file-name-sans-extension file)))

(load-idle "passwords")

;; avoid slow org startup when its really needed, e.g. for capture
;; just require it during idle time
(run-with-idle-timer 3 nil (lambda () (require 'org)))
(run-with-idle-timer 3 nil (lambda () (require 'psi-emacs)))

;; Local Variables:
;; eval: (make-variable-buffer-local 'kill-buffer-hook)
;; eval: (add-hook 'kill-buffer-hook (lambda () (sort-elisp-block) (save-buffer)) nil t)
;; End:
