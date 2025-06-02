;; config.el -*- lexical-binding: t; -*-
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

;;------------------------------------------------------------------------------
;; Packages & Loads
;;------------------------------------------------------------------------------

(when-let ((env (getenv "VIRTUAL_ENV")))
  (message (concat "VIRTUAL_ENV already set to " env
                   ". It is probably set in doom env and should be removed."))
  (setenv "VIRTUAL_ENV" nil))

(setq copyright-names-regexp ".*Andrew Peck*")

(add-load-path! "lisp/")

(setq use-package-always-defer t)

;; Suppress `Package cl is deprecated` warnings
(setq byte-compile-warnings '(not obsolete))

;; memoize the call to file-remote-p, since on remote (TRAMP) buffers it is VERY slow
(unless (functionp 'file-remote-p-memo)
  (require 'memoize)
  (defmemoize file-remote-p-memo (path)
    (file-remote-p path 'host)))

(defun remote-host? (path)
  ;; this is just tramp-remote-file-name-spec-regexp
  ;; have a copy here so we don't need
  (let ((remote-name-regexp "\\(-\\|[[:alnum:]]\\{2,\\}\\)\\(?::\\)\\(?:\\([^/:|[:blank:]]+\\)\\(?:@\\)\\)?\\(\\(?:[%._[:alnum:]-]+\\|\\(?:\\[\\)\\(?:\\(?:[[:alnum:]]*:\\)+[.[:alnum:]]*\\)?\\(?:]\\)\\)\\(?:\\(?:#\\)\\(?:[[:digit:]]+\\)\\)?\\)?"))
    (or (string-match-p remote-name-regexp path)
        (file-remote-p-memo path))))

(defun home-manager-switch ()
  "Reload home manager configuration."
  (interactive)
  (async-shell-command "home-manager switch"))

;;------------------------------------------------------------------------------
;; Shell Config
;;------------------------------------------------------------------------------

;; Fish (and possibly other non-POSIX shells) is known to inject garbage
;; output into some of the child processes that Emacs spawns. Many Emacs
;; packages/utilities will choke on this output, causing unpredictable
;; issues. To get around this, either:
(setq shell-file-name (executable-find "bash"))
(setq-default vterm-shell (executable-find "fish"))
(setq-default explicit-shell-file-name (executable-find "fish"))

;;------------------------------------------------------------------------------
;; Auto Cleanup
;;------------------------------------------------------------------------------

;; clean the recent file list on idle
(setq recentf-auto-cleanup 120)

;;------------------------------------------------------------------------------
;; Mode aliases
;;------------------------------------------------------------------------------

;; enable syntax highlighting for vimrc files
(add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode))   ; vimrc
(add-to-list 'auto-mode-alist '("\\.xdc\\'"          . vivado-mode))  ; tcl mode for xdc files
(add-to-list 'auto-mode-alist '("\\.qel\\'"          . tcl-mode))     ; tcl mode for cadence files
(add-to-list 'auto-mode-alist '("\\.sdc\\'"          . tcl-mode))     ; tcl mode for sdc files
(add-to-list 'auto-mode-alist '("\\.ltx\\'"          . json-mode))    ; json mode for ltx files
(add-to-list 'auto-mode-alist '("\\.ino\\'"          . cpp-mode))     ; cpp mode for arduino files
(add-to-list 'auto-mode-alist '("\\.src\\'"          . hog-src-mode)) ;
(add-to-list 'auto-mode-alist '("\\.cheby\\'"        . yaml-mode))    ; yaml mode for cheby
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

(add-to-list 'warning-suppress-types '(iedit))

;; buffer-local variables
(setq-default display-line-numbers nil
              fill-column 80
              tab-width 2
              delete-by-moving-to-trash t ; Delete files to trash
              window-combination-resize t) ; take new window space from all other windows (not just current)

;; something is overriding these
(setq compilation-scroll-output t
      auto-revert-mode t                  ;
      auto-revert-remote-files t             ;
      undo-limit 80000000)                 ; Raise undo-limit to 80Mb

(setq mouse-wheel-scroll-amount-horizontal 32
      mouse-wheel-tilt-scroll t
      enable-local-variables t               ;
      help-at-pt-display-when-idle 'never ; this prevents a pretty annoying help display that pops up in the minibuffer, esp in dired
      scroll-margin 30                    ; add a margin while scrolling
      so-long-threshold 800               ; so-long-threshold can increase
      auto-save-default t             ; Nobody likes to loose work, I certainly don't
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
        (if dired-directory
            (concat (abbreviate-file-name dired-directory) " - Dired" )
          (concat (abbreviate-file-name (expand-file-name "%b"))
                  (when (buffer-modified-p) " * "))))

      ;; window title when minimzed--- just make it the same
      icon-title-format frame-title-format)

(defun xclip ()
  (interactive)
  (let* ((buffer (buffer-file-name))
         (mimetype (shell-command-to-string (concat "mimetype " buffer))))
    (shell-command (concat "xclip -sel c -target " mimetype " " (buffer-file-name)))))

(midnight-mode)                     ; Clear buffers at midnight
(display-time-mode 1)               ; Enable time in the mode-line
(global-subword-mode 0)             ; Separate CamelCase words?

;;------------------------------------------------------------------------------
;; Config Loading
;;------------------------------------------------------------------------------

(defun load!! (path) (load! path doom-user-dir t))

(defun load-timer (pkg &optional timer)
  "Load package on a timer."
  (let ((timer (if timer timer 1.5)))
    (run-with-timer timer nil #'load!! pkg)))

(defun load-idle (pkg &optional timer)
  "Load package on idle."
  (let ((timer (if timer timer 1.5)))
    (run-with-idle-timer timer nil #'load!! pkg)))

(defun user-load-idle (path) (load-idle (concat doom-user-dir path)))
(defun user-load-timer (path) (load-timer (concat doom-user-dir path)))

(load!! "custom")

;; Load setup files
(dolist (file (file-expand-wildcards (concat doom-user-dir "lisp/setup*.el")))
  (load (file-name-sans-extension file) 0.1))

(load!! "lisp/plotting")
(load!! "lisp/regulator")
(load!! "lisp/tracking")
(load!! "passwords")
(load!! "psiq")

;; Local Variables:
;; eval: (make-variable-buffer-local 'kill-buffer-hook)
;; eval: (add-hook 'kill-buffer-hook (lambda () (sort-elisp-block) (save-buffer)) nil t)
;; End:
