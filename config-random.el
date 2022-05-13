;;; ../.dotfiles/doom.d/config-random.el -*- lexical-binding: t; -*-

;;------------------------------------------------------------------------------
;;; Random
;;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; Command to suspend the system
;;------------------------------------------------------------------------------

(defun suspend ()
  "Suspend the system using systemctl syspend"
  (interactive)
  (start-process "suspend" nil "systemctl" "suspend"))

;;------------------------------------------------------------------------------
;; C-backspace without modifying kill ring
;; https://emacs.stackexchange.com/questions/22266/backspace-without-adding-to-kill-ring
;;------------------------------------------------------------------------------

(defun my-delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

(defun my-backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (my-delete-word (- arg)))

(global-set-key (kbd "C-<backspace>") #'my-backward-delete-word)


;;
(after! tree-sitter
  (global-tree-sitter-mode)
  (tree-sitter-hl-mode))

;; make $ not part of a symbol in tcl-mode
(with-eval-after-load "tcl"
  (modify-syntax-entry ?$ "'" tcl-mode-syntax-table))

;; so-long-threshold can increase, it is still reasonably performant at 800
(setq so-long-threshold 800)

(setq ws-butler-global-exempt-modes
      '(special-mode comint-mode term-mode eshell-mode))

(after! org
 (setq org-ditaa-jar-path
       "~/.doom.d/ditaa.jar"))

;; (executable-find "ditaa")

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

(defun fix-evil ()
  (interactive)
  (setq-local transient-mark-mode t))
