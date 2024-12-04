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

(setq use-package-always-defer t)

;; Suppress `Package cl is deprecated` warnings
(setq byte-compile-warnings '(not obsolete))

(setq doom-scratch-dir doom-user-dir)

;; memoize the call to file-remote-p, since on remote (TRAMP) buffers it is VERY slow
(unless (functionp 'file-remote-p-memo)
  (require 'memoize)
  (defmemoize file-remote-p-memo (path)
    (file-remote-p path 'host)))

(defun remote-host? (path)
  (or (string-match-p tramp-remote-file-name-spec-regexp path)
      (file-remote-p-memo path)))

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
(run-with-idle-timer 600 t #'recentf-cleanup)

;;------------------------------------------------------------------------------
;; Mode aliases
;;------------------------------------------------------------------------------

;; enable syntax highlighting for vimrc files
(add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode))   ; vimrc
(add-to-list 'auto-mode-alist '("\\.xdc\\'"          . vivado-mode))  ; tcl mode for xdc files
(add-to-list 'auto-mode-alist '("\\.ltx\\'"          . json-mode))    ; json mode for ltx files
(add-to-list 'auto-mode-alist '("\\.ino\\'"          . cpp-mode))     ; cpp mode for arduino files
(add-to-list 'auto-mode-alist '("\\.cheby\\'"        . yaml-mode))    ; yaml mode for cheby
(add-to-list 'auto-mode-alist '("\\.bb\\'"           . clojure-mode)) ; babashka

;; Sibling File Rules
(add-to-list 'find-sibling-rules '("\\([^/]+\\)\\.c\\'" "\\1.h")) ;; c to h
(add-to-list 'find-sibling-rules '("\\([^/]+\\)\\.h\\'" "\\1.c")) ;; h to c
(add-to-list 'find-sibling-rules '("\\([^/]+\\)\\.drawio\\'" "\\1.pdf")) ;; drawio to pdf
(add-to-list 'find-sibling-rules '("\\([^/]+\\)\\.pdf\\'" "\\1.drawio")) ;; pdf to drawio

;; Find Local Dictionaries
(add-hook 'text-mode-hook
          (defun hook/set-ispell-dict ()
            (setq-local ispell-personal-dictionary
                        (let ((local (concat (vc-root-dir) ".aspell.en.pws")))
                          (if (file-exists-p local) local "~/.aspell.en.pws")))))

(add-hook 'text-mode-hook #'abbrev-mode)

(setopt mouse-wheel-scroll-amount-horizontal 32
        mouse-wheel-tilt-scroll t)

(global-auto-revert-mode t)
(setopt auto-revert-avoid-polling nil
        auto-revert-interval 1
        auto-revert-use-notify nil
        auto-revert-check-vc-info t)

(add-to-list 'warning-suppress-types '(iedit))

(setq enable-local-variables t     ;
      auto-revert-mode t           ;
      scroll-margin 30             ; add a margin while scrolling
      auto-revert-remote-files t   ;
      so-long-threshold 800        ; so-long-threshold can increase
      smartparens-global-mode nil  ; disable smartparens/automatic parentheses completion
      smartparens-mode nil         ; disable smartparens/automatic parentheses completion
      undo-limit 80000000          ; Raise undo-limit to 80Mb
      auto-save-default t          ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…" ; Unicode ellispis are nicer than "...", and also save /precious/ space

      abbrev-file-name (concat doom-user-dir "abbrev_defs")

      ;; place bookmarks in the doom folder for version control

      bookmark-default-file (concat doom-user-dir "bookmarks" "-" (system-name))

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
            (concat (abbreviate-file-name dired-directory) " - Emacs Dired" )
          (concat (abbreviate-file-name (expand-file-name "%b"))
                  (if (buffer-modified-p) " • " " - ")
                  "Emacs" )))

      ;; window title when minimzed--- just make it the same
      icon-title-format frame-title-format)

(setq-default fill-column 80
              tab-width 2
              delete-by-moving-to-trash t ; Delete files to trash
              window-combination-resize t ; take new window space from all other windows (not just current)
              x-stretch-cursor t)         ; Stretch cursor to the glyph width

(midnight-mode)                     ; Clear buffers at midnight
(display-time-mode 1)               ; Enable time in the mode-line
(global-subword-mode 0)             ; Separate CamelCase words?
;; (modify-syntax-entry ?_ "w")     ; Treat underscore as part of a word to match vim behavior
;; (modify-syntax-entry ?- "w")     ; Treat dash as part of a word

(setq pdf-sync-backward-display-action t)

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

(defun doom-load-idle (path) (load-idle (concat doom-user-dir path)))
(defun doom-load-timer (path) (load-timer (concat doom-user-dir path)))

(load!! "custom")

;; start:sort
(load!! "lisp/plotting")
(load!! "lisp/regulator")
(load!! "lisp/tracking")
(load!! "passwords")
(load!! "psiq")
;; end:sort

;; Load setup files
(dolist (file (file-expand-wildcards (concat doom-user-dir "lisp/setup*.el")))
  (load (file-name-sans-extension file)))

;; Local Variables:
;; eval: (make-variable-buffer-local 'kill-buffer-hook)
;; eval: (add-hook 'kill-buffer-hook (lambda () (sort-elisp-block) (save-buffer)) nil t)
;; End:
