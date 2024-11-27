;;; -*- lexical-binding: t; -*-
;;;
;; once an hour, clean the recent file list
(run-with-timer 60 3600 'recentf-cleanup)

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


;; try to make sure that fundamental mode buffers use evil
;; https://emacs.stackexchange.com/questions/16693/auto-enable-minor-modes-in-fundamental-mode
(add-hook 'after-change-major-mode-hook
  (defun hook/turn-on-evil-mode ()
    "Turn on evil mode in fundamental mode"
    (when (eq major-mode 'fundamental-mode)
      (evil-local-mode))))

;; don't make escape annoyingly close popups UHG
(advice-remove 'evil-force-normal-state
               '+evil-escape-a)

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

;; use mouse forward/backward to jump between buffers
(map! :n [mouse-8] #'previous-buffer
      :n [mouse-9] #'next-buffer)

;;
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

(setq pdf-sync-backward-display-action t)

