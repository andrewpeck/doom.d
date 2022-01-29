;;; ../.dotfiles/doom.d/config-random.el -*- lexical-binding: t; -*-

;;------------------------------------------------------------------------------
;;; Random
;;;------------------------------------------------------------------------------

(setq ws-butler-global-exempt-modes
      '(special-mode comint-mode term-mode eshell-mode))

(setq org-ditaa-jar-path (executable-find "ditaa"))


(defun copy-html-to-ohm ()
  (start-process
   "copy-to-ohm"
   nil "rsync" "-av"
   (format "%s.html" (file-name-base))
   "ohm:~/public_html/notes/"))

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
  (shell-command "chmod 600 ~/.ssh/authorized_keys")
  t)

(setq flycheck-markdown-markdownlint-cli-config
      (concat doom-private-dir "markdownlint-config.yml"))

(setq flycheck-flake8rc
      (concat doom-private-dir "flake8.rc"))

(setq flycheck-pylintrc
      (concat doom-private-dir "pylint.rc"))

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

(setq python-shell--interpreter  "python3")
(setq enable-local-variables t)

(after! projectile
  (setq projectile-sort-order 'recently-active))


(defun reload-this-buffer ()
  "Kill and re-open the current buffer"
  (interactive)
  (revert-buffer)
  (let ((tmp-buffer-file (buffer-file-name)))
    (kill-buffer (buffer-name))
    (find-file tmp-buffer-file)))

(map! :leader :desc "Reload buffer" "b r" #'reload-this-buffer)
