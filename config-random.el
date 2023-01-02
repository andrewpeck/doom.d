;;; -*- lexical-binding: t; -*-

(add-to-list 'warning-suppress-types '(iedit))

(setq enable-local-variables t      ;
      scroll-margin 30              ; add a margin while scrolling
      auto-revert-remote-files t    ;
      so-long-threshold 800         ; so-long-threshold can increase
      smartparens-global-mode nil   ; disable smartparens/automatic parentheses completion
      smartparens-mode nil          ; disable smartparens/automatic parentheses completion
      undo-limit 80000000           ; Raise undo-limit to 80Mb
      auto-save-default t           ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…"  ; Unicode ellispis are nicer than "...", and also save /precious/ space

      bookmark-default-file "~/.doom.d/bookmarks" ;

      ;; Increase the amount of data which Emacs reads from the process.
      ;; Again the emacs default is too low 4k considering that the some
      ;; of the language server responses are in 800k - 3M range.
      read-process-output-max (* 1024 1024) ;; 1mb

      ;; Etags search depth
      etags-table-search-up-depth 10)

(setq-default fill-column 80
              tab-width 2
              delete-by-moving-to-trash t ; Delete files to trash
              window-combination-resize t ; take new window space from all other windows (not just current)
              x-stretch-cursor t)         ; Stretch cursor to the glyph width

(menu-bar-mode 0)                   ; Turn off menu bar
(midnight-mode)                     ; Clear buffers at midnight
(display-time-mode 1)               ; Enable time in the mode-line
(global-subword-mode 1)             ; Iterate through CamelCase words
;; (modify-syntax-entry ?_ "w")     ; Treat underscore as part of a word to match vim behavior

;; Window Title
;; https://www.emacswiki.org/emacs/FrameTitle
(setq frame-title-format
      '(:eval
        (if dired-directory
            (concat (abbreviate-file-name dired-directory) " - Dired" )
          (concat (abbreviate-file-name (expand-file-name "%b"))
                  (if (buffer-modified-p) " • " " - ")
                  "Emacs" ))))

;;
(map! :n [mouse-8] #'previous-buffer
      :n [mouse-9] #'next-buffer)

;; Start emacs in full screen by default
(add-to-list 'default-frame-alist
             '(fullscreen . maximized))

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; Backups
(setq backup-each-save-mirror-location "~/emacs-backups")
(add-hook 'after-save-hook 'backup-each-save)

;;
(add-hook! debugger-mode-hook #'turn-on-evil-mode)

;;
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

;;------------------------------------------------------------------------------
;; Package Configs
;;------------------------------------------------------------------------------

(use-package! dwim-shell-command
  :config
  (defun my/dwim-shell-command-archive-zstd ()
    "Convert all marked images to jpg(s)."
    (interactive)
    (dwim-shell-command-on-marked-files
     "Archive as zstd"
     "tar -cavf '<<fne>>.tar.zst' '<<f>>'"
     :utils "tar"))

  (defun my/dwim-shell-command-archive-gz ()
    "Convert all marked images to jpg(s)."
    (interactive)
    (dwim-shell-command-on-marked-files
     "Archive as zstd"
     "tar -cavf '<<fne>>.tar.gz' '<<f>>'"
     :utils "tar")))

(after! langtool
  (setq langtool-java-classpath "/snap/languagetool/current/usr/share/java")
  (setq langtool-language-tool-server-jar "/snap/languagetool/current/usr/bin/languagetool.jar"))

;; disable confusing undo-fu behavior
;; https://gitlab.com/ideasman42/emacs-undo-fu/-/issues/6
(after! undo-fu
  (setq undo-fu-ignore-keyboard-quit t))

(after! +popup
  ;; Completely disable management of the mode-line in popups:
  ;; (remove-hook '+popup-buffer-mode-hook #'+popup-set-modeline-on-enable-h)
  ;; Make sure evil is on in popups
  (add-hook '+popup-buffer-mode-hook #'turn-on-evil-mode)

  (evil-define-key
    'motion +popup-buffer-mode-map "q" #'bury-buffer)
  (evil-define-key
    'motion +popup-buffer-mode-map (kbd "C-<up>") #'+popup/raise)

  ;; (set-popup-rules!
  ;;  '(("^ \\*" :slot -1) ; fallback rule for special buffers
  ;;    ("^\\*" :select t)
  ;;    ("^\\*Completions" :slot -1 :ttl 0)
  ;;    ("^\\*\\(?:scratch\\|Messages\\)" :ttl t)
  ;;    ("^\\*Help" :slot -1 :size 0.2 :select t)
  ;;    ("^\\*doom:"
  ;;     :size 0.35 :select t :modeline t :quit t :ttl t)))
  )

(after! tree-sitter
  (add-hook! tcl-mode-hook    #'tree-sitter-hl-mode)
  (add-hook! python-mode-hook #'tree-sitter-hl-mode))

(after! emojify-mode
  (setq global-emojify-mode t))

(after! ws-butler
  (setq ws-butler-global-exempt-modes
        '(special-mode comint-mode term-mode eshell-mode)))

(after! hl-todo
  (setq global-hl-todo-mode t))

;; better dired soring
(after! dired-x
  (setq dired-omit-extensions (remove ".bin" dired-omit-extensions))
  (setq dired-omit-extensions (remove ".bit" dired-omit-extensions))
  (setq dired-listing-switches "-a1vBhl  --group-directories-first"))

(after! diredfl
  (add-to-list 'diredfl-compressed-extensions ".zst"))

(after! dired-aux
  (setq dired-compress-file-default-suffix ".zst")
  (setq dired-compress-directory-default-suffix ".tar.zst"))

;; persistent undo
(after! undo
  (setq undo-tree-auto-save-history t))

(after! undo-tree
  (setq undo-tree-history-directory-alist '(("." . "~/.doom.d/undo"))))

;; save macros and other registers peristently
(after! savehist
  (add-to-list 'savehist-additional-variables 'register-alist)
  (add-hook! 'savehist-save-hook
    (defun doom-clean-up-registers-h ()
      (setq-local register-alist
                  (cl-remove-if-not #'savehist-printable register-alist)))))

(after! tramp
  (setq tramp-ssh-controlmaster-options "-o ControlMaster=no")
  (setq tramp-histfile-override "~/.tramp_history")

  ;; Another way to find the remote path is to use the path assigned to the remote user by the
  ;; remote host. TRAMP does not normally retain this remote path after login. However,
  ;; tramp-own-remote-path preserves the path value, which can be used to update tramp-remote-path.
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(after! projectile
  (setq projectile-sort-order 'recently-active))

(after! writegood
  (writegood-passive-voice-turn-off))

(after! yasnippet
  ;; Don't add newlines to snippet endings
  (setq-default yas-also-auto-indent-first-line t)
  (add-hook 'snippet-mode-hook
            (lambda () (setq require-final-newline nil))))

(after! hog
  (pcase (system-name)
    ("strange" (progn (setq hog-vivado-path "~/Xilinx/Vivado/2021.1")
                      (setq hog-number-of-jobs 16)))
    ("larry" (progn (setq hog-vivado-path "/storage/Xilinx/Vivado/2021.1")
                    (setq hog-number-of-jobs 4)))
    ("pepper" (progn
                (setq hog-template-xml-path "/home/andy/.doom.d/lisp/hog-emacs/")))))

;;------------------------------------------------------------------------------
;; Mixed Pitch Mode
;;------------------------------------------------------------------------------

;; (add-hook 'org-mode-hook      #'mixed-pitch-mode)
;; (add-hook 'markdown-mode-hook #'mixed-pitch-mode)
;; (add-hook 'latex-mode-hook    #'mixed-pitch-mode)

;;------------------------------------------------------------------------------
;; Line wrapping
;;------------------------------------------------------------------------------

(defun ap/no-wrap ()
  (interactive)
  (visual-line-mode 0)
  (toggle-truncate-lines 1)
  (visual-fill-column-mode 0))

;; (add-hook 'text-mode-hook #'visual-fill-column-mode)

;; Disable auto fill mode in text modes
;; (remove-hook 'text-mode-hook #'auto-fill-mode)

;; Don't wrap text modes unless we really want it
(remove-hook 'text-mode-hook #'+word-wrap-mode)
(add-hook 'latex-mode-hook #'+word-wrap-mode)
(add-hook 'markdown-mode-hook #'+word-wrap-mode)

;; (defun fix-visual-fill-column-mode (&optional ARG)
;;   (setq visual-fill-column-mode visual-line-mode))

;; toggle visual-fill column mode when chaing word wrap settings
;; (advice-add '+word-wrap-mode
;;             :after 'fix-visual-fill-column-mode)
;;
;; (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)

;;------------------------------------------------------------------------------
;; Utility Functions
;;------------------------------------------------------------------------------

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
  (pdf-rotate "-"))

(defun copy-html-to-ohm ()
  (start-process
   "copy-to-ohm"
   nil "rsync" "-av"
   (format "%s.html" (file-name-base buffer-file-name))
   "ohm:~/public_html/notes/"))

;; C-backspace without modifying kill ring
;; https://emacs.stackexchange.com/questions/22266/backspace-without-adding-to-kill-ring

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

(defun suspend ()
  "Suspend the system using systemctl syspend"
  (interactive)
  (start-process "*suspend*" nil "setsid" "systemctl" "suspend"))

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

(defun arrayify (start end quote separator)
  "Turn selection into a QUOTEd, separated one-liner."
  (interactive
   (list
    (region-beginning)
    (region-end)
    (read-string "Quote: " "\"")
    (read-string "Separator: "
                 (if (member major-mode
                             '(emacs-lisp-mode
                               clojure-mode
                               racket-mode
                               lisp-mode)) "" ","))))
  (let ((insertion
         (mapconcat
          (lambda (x) (format "%s%s%s" quote x quote))
          (split-string (buffer-substring start end)) (concat separator " "))))
    (delete-region start end)
    (insert insertion)))

;; Buffer Mode Histogram
;; http://blogs.fluidinfo.com/terry/2011/11/10/emacs-buffer-mode-histogram/

(defun buffer-mode-histogram ()
  "Display a histogram of Emacs buffer modes."
  (interactive)
  (let* ((totals ())
         (buffers (buffer-list()))
         (total-buffers (length buffers))
         (ht (make-hash-table :test 'equal)))

    ;;
    (save-excursion
      (dolist (buffer buffers)
        (set-buffer buffer)
        (let
            ((mode-name (symbol-name major-mode)))
          (puthash mode-name (1+ (gethash mode-name ht 0)) ht))))

    ;;
    (maphash (lambda (key value)
               (setq totals (cons (list key value) totals))) ht)

    ;;
    (setq totals (sort totals (lambda (x y) (> (cadr x) (cadr y)))))

    ;; printout
    (with-output-to-temp-buffer "Buffer mode histogram"
      (princ (format "%d buffers open, in %d distinct modes\n\n"
                     total-buffers (length totals)))
      (dolist (item totals)
        (let ((key (car item))
              (count (cadr item)))
          (if (equal (substring key -5) "-mode")
              (setq key (substring key 0 -5)))
          (princ (format "%2d %20s %s\n" count key
                         (make-string count ?+))))))))
