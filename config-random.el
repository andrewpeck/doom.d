;;; ../.dotfiles/doom.d/config-random.el -*- lexical-binding: t; -*-

;;------------------------------------------------------------------------------
;; Emojify
;;------------------------------------------------------------------------------

(after! emojify-mode
  (setq global-emojify-mode t))

;;------------------------------------------------------------------------------
;; Arrayify
;;------------------------------------------------------------------------------

(defun arrayify (start end quote)
  "Turn strings on newlines into a QUOTEd, comma-separated one-liner."
  (interactive "r\nMQuote: ")
  (let ((insertion
         (mapconcat
          (lambda (x) (format "%s%s%s" quote x quote))
          (split-string (buffer-substring start end)) ", ")))
    (delete-region start end)
    (insert insertion)))

;;------------------------------------------------------------------------------
;;; Random
;;;------------------------------------------------------------------------------

;; https://github.com/tecosaur/screenshot/issues/11
(load-file (locate-library "screenshot.el"))

;; Fix issue with this
;; https://github.com/tecosaur/screenshot/issues/7

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

(setq bookmark-default-file "~/.doom.d/bookmarks")

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
(setq-default fill-column 100)

;; Turn on menu bar
(menu-bar-mode 0)

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

(after! hl-todo
  (setq global-hl-todo-mode t))

;; better dired soring
(after! dired
  (setq dired-listing-switches "-a1vBhl  --group-directories-first"))

;; add a margin while scrolling
(setq scroll-margin 30)

;; persistent undo
(after! undo
  (setq undo-tree-auto-save-history t))

(after! undo-tree
  (setq undo-tree-history-directory-alist '(("." . "~/.doom.d/undo"))))

;; Bookmarks
(setq bookmark-default-file "~/.doom.d/bookmarks")

(defun ap/url->org ()
  "Convert the URL at point into an Org mode formatted link. The
title of the page is retrieved from the web page"
  (interactive)
  (let* ((link (thing-at-point 'url))
         (bounds (bounds-of-thing-at-point 'url))
         (start (car bounds))
         (end   (cdr bounds))
         (description
          (if (org-url-p link)
              (www-get-page-title link) link)))
    (when (and link description start end)
      (delete-region start end)
      (org-insert-link nil link description))))

(defun ap/url->md ()
  "Convert the URL at point into an md mode formatted link. The
title of the page is retrieved from the web page"
  (interactive)
  (ap/url->org)
  (org-link->markdown))

(defun ap/inline-org-inbox-link ()
  (interactive)
  (save-excursion
    (let ((link nil)
          (description nil))

      (progn
        (progn
          (next-line)
          (end-of-line)
          (push-mark (point) t t)
          (move-beginning-of-line 1)
          (setq link (buffer-substring-no-properties (region-beginning) (region-end)))
          (setq link (replace-regexp-in-string "^- " "" link))
          (previous-line)))

      (progn
        (end-of-line)
        (push-mark (point) t t)
        (re-search-backward "^\*+ ")
        (re-search-forward " ")
        (setq description (buffer-substring-no-properties (region-beginning) (region-end))))

      (org-insert-link nil link description)

      (replace-regexp-in-region "^\*+" "-" (line-beginning-position) (line-end-position))

      (next-line)
      (beginning-of-line)
      (kill-line)
      (kill-line))))

;; save macros and other registers peristently
(after! savehist
  (add-to-list 'savehist-additional-variables 'register-alist)
  (add-hook! 'savehist-save-hook
    (defun doom-clean-up-registers-h ()
      (setq-local register-alist
                  (cl-remove-if-not #'savehist-printable register-alist)))))

(setq auto-revert-remote-files t)

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

(after! tramp
  (setq tramp-ssh-controlmaster-options "-o ControlMaster=no")
  (setq tramp-histfile-override "~/.tramp_history")

  ;; Another way to find the remote path is to use the path assigned to the remote user by the
  ;; remote host. TRAMP does not normally retain this remote path after login. However,
  ;; tramp-own-remote-path preserves the path value, which can be used to update tramp-remote-path.
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


;;------------------------------------------------------------------------------
;; SLIME
;;------------------------------------------------------------------------------

;; slime
;;(after! slime
;;  (setq inferior-lisp-program "sbcl")
;;  (setq org-babel-lisp-eval-fn 'slime-eval))

;;------------------------------------------------------------------------------
;; Spell-Checking
;;------------------------------------------------------------------------------

(after! writegood
  (writegood-passive-voice-turn-off))

;;------------------------------------------------------------------------------
;; Snippets
;;------------------------------------------------------------------------------

;; Don't add newlines to snippet endings
(after! yasnippet
  (setq-default yas-also-auto-indent-first-line t)
  (add-hook 'snippet-mode-hook
            (lambda () (setq require-final-newline nil))))

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
;; Backups
;;------------------------------------------------------------------------------

(setq backup-each-save-mirror-location "~/emacs-backups")
(add-hook 'after-save-hook 'backup-each-save)

;;------------------------------------------------------------------------------
;; Hog
;;------------------------------------------------------------------------------

(after! hog
  (pcase (system-name)
    ("strange" (progn (setq hog-vivado-path "~/Xilinx/Vivado/2021.1/settings64.sh")
                      (setq hog-number-of-jobs 16)))
    ("larry" (progn (setq hog-vivado-path "/storage/Xilinx/Vivado/2021.1/settings64.sh")
                    (setq hog-number-of-jobs 4)))
    ("pepper" (progn
                (setq hog-template-xml-path "/home/andy/.doom.d/lisp/hog-emacs/")))))

;;------------------------------------------------------------------------------
;; VHDL Mode
;;------------------------------------------------------------------------------

;; vhdl mode will wrap comments after some # of characters
(after! vhdl
  (setq vhdl-end-comment-column 200
        vhdl-prompt-for-comments nil
        auto-fill-mode nil))

;;-----------------------------------------------------------------------------------------
;; Elfeed
;;------------------------------------------------------------------------------

;; Run `elfeed-update' every 8 hours
(run-at-time nil (* 8 60 60) #'elfeed-update)

(after! elfeed
  (setq elfeed-feeds
        '(
          ;; "https://hackaday.com/blog/feed/"
          "https://www.evalapply.org/index.xml"
          "https://nullprogram.com/feed/"
          "https://bzg.fr/index.xml"
          "https://www.mattblaze.org/blog/rss20.xml"
          "https://jackrusher.com/feed.xml"
          "http://mbork.pl/?action=rss;days=30;all=0;showedit=0"
          "https://isc.sans.edu/rssfeed_full.xml"
          "https://watchguy.co.uk/feed/"
          "https://sachachua.com/blog/feed/")))

;;-----------------------------------------------------------------------------------------
;; IELM
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

  (add-hook 'inferior-emacs-lisp-mode-hook #'set-ielm-comint-input-ring))

;;-----------------------------------------------------------------------------------------
;; XML
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

;;-----------------------------------------------------------------------------------------
;; Markdown
;;------------------------------------------------------------------------------

;; Make Fundamental Mode GFM by default
(after! gfm
  (setq initial-major-mode 'gfm-mode))

;;-----------------------------------------------------------------------------------------
;; C mode
;;-----------------------------------------------------------------------------------------

;; For example, if you prefer double slashes // instead of slash-stars /* ... */
;; in c-mode, insert below code into your ~/.emacs:
(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Preferred comment style
            (setq comment-start "// " comment-end "")))

;;-----------------------------------------------------------------------------------------
;; Clojure
;;-----------------------------------------------------------------------------------------

;; (setq org-babel-clojure-backend "cider")
