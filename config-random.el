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

(defun advice-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

(defun fix-latex-pasted-text ()

  "Fix text pasted from Latex PDFs.

Text copy pasted from Latex likes to hyphen- ate inappropriately.

This function tries to de hyphenate them."

  (interactive)
  (require 'expand-region)
  (unless (region-active-p)
    (progn
      (er/mark-paragraph)
      (goto-char (region-beginning))
      (when (= (string-match paragraph-separate (thing-at-point 'line)) 0)
        (forward-line))))

  (join-line nil (region-beginning) (region-end))

  (replace-regexp-in-region "\\([[:word:]]\\)-[[:blank:]]+\\([[:word:]]\\)" "\\1\\2"
                            (line-beginning-position) (line-end-position))

  (if (eq major-mode #'org-mode)
      (org-fill-paragraph)
    (fill-paragraph)))


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

;; ;; synctex view
;; (set-popup-rule! ".*\.tex"
;;   :quit nil
;;   :actions nil
;;   :side 'right
;;   :slot -10
;;   :select t
;;   :ttl 0)

(defun mult-by (by)
  ""
  (when (re-search-forward "\\b[-0-9.]+\\b" nil t)
    (replace-match (number-to-string (* by (string-to-number (match-string 0)))) nil t) t))

(defun mult-in-rectangle (start end)
  "Operates on a rectangular region and multiplies all numbers by some coefficient.

FIXME: should support non-rectangular regions
FIXME: does not work when the region is not rectangular at the end of lines, e.g.

2 + 2
2
2
2 "

  (interactive "r")
  (let ((coefficient (string-to-number (read-string "Coefficient: "))))
    (save-excursion
      (let ((rect (delete-extract-rectangle start end)))
        (with-temp-buffer
          (insert-rectangle rect)
          (goto-char (point-min))
          (while (mult-by coefficient))
          (setq rect (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n" t)))
        (goto-char start)
        (insert-rectangle rect)))))

(defun crontab-e ()
  "Run `crontab -e' in a emacs buffer."
  (interactive)
  (with-editor-async-shell-command "crontab -e"))

;;---------------------------------------------------------------------------------
;; Line wrapping
;;---------------------------------------------------------------------------------

(defun ap/no-wrap ()
  (interactive)
  (let ((inhibit-message t))
    (setq ap/is-wrapped nil)
    (visual-line-mode 0)
    (toggle-truncate-lines 1)
    (visual-fill-column-mode 0))

  (message "Unwraping lines..."))

(defun ap/wrap ()
  (interactive)
  (let ((inhibit-message t))
    (setq ap/is-wrapped 1)
    (visual-line-mode 1)
    (toggle-truncate-lines 0)
    (visual-fill-column-mode 1))

  (message "Wrapping lines..."))

(defun ap/toggle-wrap ()
  (interactive)
  (if (and (boundp 'ap/is-wrapped) ap/is-wrapped)
      (ap/no-wrap)
    (ap/wrap)))

;; (add-hook 'text-mode-hook #'visual-fill-column-mode)

;; Disable auto fill mode in text modes
;; (remove-hook 'text-mode-hook #'auto-fill-mode)

;; Don't wrap text modes unless we really want it
(remove-hook 'text-mode-hook #'+word-wrap-mode)
(add-hook! 'latex-mode-hook #'+word-wrap-mode)
(add-hook! 'markdown-mode-hook #'+word-wrap-mode)

;; (defun fix-visual-fill-column-mode (&optional ARG)
;;   (setq visual-fill-column-mode visual-line-mode))

;; toggle visual-fill column mode when chaing word wrap settings
;; (advice-add '+word-wrap-mode
;;             :after 'fix-visual-fill-column-mode)
;;
;; (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)

;;---------------------------------------------------------------------------------
;; Utility Functions
;;---------------------------------------------------------------------------------

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
  (call-process "systemctl" nil 0 nil "suspend"))

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
          (split-string (buffer-substring-no-properties start end))
          (concat separator " "))))
    (delete-region start end)
    (insert insertion)
    (newline)))

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

;;---------------------------------------------------------------------------------
;; Functions for alphabetically sorting items
;;---------------------------------------------------------------------------------

(defun sort-code-block (comment-char)
  "Alphabetically sorts code blocks in a file, starting with #
start:sort and ending with # end:sort, where # is the comment
char of the language you are editing"

  (let (buffer-undo-list)
    (save-excursion
      (let ((home (point))
            (start-search (concat "^\s*" comment-char " start:sort"))
            (end-search (concat "^\s*" comment-char " end:sort")))
        (goto-char (point-min))
        (while (re-search-forward start-search nil t)

          (forward-line -1)
          (let ((start (re-search-forward start-search nil t))
                (end
                 ;; want to get the match *before* end:sort
                 (- (progn (re-search-forward end-search nil t)
                           (match-beginning 0)) 1)))
            (sort-lines 'nil start end)))
        (goto-char home))))

  ;; make sure to return nil here for write-contents-functions
  nil)

(defun sort-elisp-block ()
  (interactive)
  (save-excursion
    (sort-code-block ";;")))

;;---------------------------------------------------------------------------------
;; Run Vim for reindent
;;---------------------------------------------------------------------------------

(defun reindent-buffer-with-vim ()
  (interactive)
  (shell-command (format "vim --clean -c 'set tabstop=2 softtabstop=0 expandtab shiftwidth=2 smarttab' -c 'normal gg=G' -c 'wq' %s" buffer-file-name)))

;;------------------------------------------------------------------------------
;; Query Replace
;;------------------------------------------------------------------------------

(defun query-replace-symbol ()
  "Do query-replace current word with"

  (interactive)

  (save-excursion
    (let* ((active? (region-active-p))
           (from-string (symbol-name (symbol-at-point)))
           (to-string (read-string (format  "Query-replace %s with: " from-string) from-string)))
      (query-replace from-string to-string t
                     (if active? (region-beginning) (point-min))
                     (if active? (region-end) (point-max))))))

(defun github-package ()
  (let ((clip (current-kill 0))
        (repo "")
        (pkg  "")
        (host "github"))

    (when (string-match ".*github.com/\\([^/]*\\)/\\(.*\\)" clip)
      (setq repo (concat (match-string 1 clip) "/" (match-string 2 clip)))
      (setq pkg  (match-string 2 clip))
      (setq host "github"))

    (format "(package! %s :recipe (:host %s :repo \"%s\"))" pkg host repo)))

;; TODO: should copy the buffer to a temp file.. sometimes e.g. if trying to open a zipped file then
;; trying to exec gerbv fails

;;  convert gerber to svg
(defun gerber->svg--file (filename)
  (let* ((tmp-name
          (format  "%s.svg" (make-temp-file (concat (file-name-base filename) "-"))))
         (command
          (format  "gerbv --dpi=600 --border=0 --export=svg --output=%s %s" tmp-name filename)))
    (message tmp-name)
    (shell-command  command) tmp-name))

(defun gerber->svg ()
  (interactive)
  (progn
    (find-file
     (gerber->svg--file (buffer-file-name)))
    (revert-buffer)
    (image-mode)))

;;---------------------------------------------------------------------------------
;; HDL Helpers
;;---------------------------------------------------------------------------------

(defun hdl-self-op (op)
  (let ((sym (if (region-active-p)
                 (buffer-substring-no-properties (region-beginning) (region-end))
               (symbol-at-point))))
    (save-excursion
      (when sym
        (forward-line)
        (open-line 1)
        (indent-for-tab-command)
        (insert (format "%s <= %s %s 1;" sym sym op))))))

(defun hdl-i++ ()
  "Insert a hdl i++ for either the current selection or symbol at point."
  (interactive)
  (hdl-self-op "+"))

(defun hdl-i-- ()
  "Insert a hdl i-- for either the current selection or symbol at point."
  (interactive)
  (hdl-self-op "-"))

(defvar normalize-comment-strings-length 83
  "Number of characters to normalize comment strings to.")

(defun normalize-comment-strings ()
  (interactive)
  (save-excursion
    (goto-char (point-min))

    (let ((comment-char (pcase major-mode
                          ('verilog-mode "\/\/")
                          ('c-mode "\/\/")
                          ('c++-mode "\/\/")
                          ('python-mode "#")
                          ('python-ts-mode "#")
                          ('tcl-mode "#")
                          ('emacs-lisp-mode ";;"))))

      (unless comment-char
        (error (format "`comment-char' not defined for major mode %s" major-mode)))

      (while (re-search-forward
              (format "^\\([[:blank:]]*\\)\\(%s\\)[[:blank:]]*\\(-+\\)[[:blank:]]*$" comment-char))

        (let ((whitespace (match-string 1)))

          (goto-char (line-beginning-position))
          (kill-line)
          (insert whitespace)
          (insert comment-char)
          (insert-char #x2D
                       (- normalize-comment-strings-length
                          (length whitespace)
                          (length comment-char))))))))


(defvar current-date-format "%Y-%m-%d"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defun insert-current-date ()
  "insert the current date into the buffer.
The date will follow the format in `current-date-format'"
  (interactive)
  (insert (format-time-string current-date-format (current-time))))

(setq world-clock-list
      '(("America/Los_Angeles" "Los Angeles")
        ("America/New_York" "Boston")
        ("Europe/London" "London")
        ("Europe/Paris" "Geneva")))

;; Fish (and possibly other non-POSIX shells) is known to inject garbage
;; output into some of the child processes that Emacs spawns. Many Emacs
;; packages/utilities will choke on this output, causing unpredictable
;; issues. To get around this, either:
(setq shell-file-name (executable-find "bash"))
(setq vterm-shell (executable-find "fish"))
(setq explicit-shell-file-name (executable-find "fish"))


(defun copy-buffer-as-string ()
  "Yank / Copy the current buffer as a string."
  (interactive)
  ;; prin1-to-string ? no
  (kill-new
   (buffer-substring-no-properties (point-min) (point-max))))

(defun selected-window-number ()
  (let ((win (format "%s" (selected-window))))
    (if (string-match "#<window \\([[:digit:]]+\\) on .*>" win)
        (string-to-number (match-string-no-properties 1 win)) nil)))

;; (defvar window-list-hash-table
;;   (make-hash-table)
;;   "docs")

;; (defun switch-to-previous-buffer-with-hashtable ()
;;   "Switch to previously open buffer. Repeated invocations toggle
;; between the two most recently open buffers."
;;   (interactive)
;;   ;; (evil-switch-to-windows-last-buffer)
;;   (let* ((current (current-buffer))
;;          (other (other-buffer current 1)))
;;     (when (and  (buffer-file-name other)
;;                 (buffer-file-name current))
;;       (switch-to-buffer other))))

;;------------------------------------------------------------------------------
;; Mime type register
;;------------------------------------------------------------------------------

(defun register-new-mime-type (handler extension &optional executable logo comment)

  (when logo
    (shell-command (format "xdg-icon-resource install --context mimetypes --size 48 %s application-x-%s" logo handler)))


  (unless (and handler extension)
    (error "Must define handler (program to open files with) and extensions"))

  (unless executable
    (setq executable handler))

  (unless comment
    (setq comment ""))

  (let ((xml  (format "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<mime-info xmlns=\"http://www.freedesktop.org/standards/shared-mime-info\">
    <mime-type type=\"application/x-%s\">
        <comment>\"%s\"</comment>
        <icon name=\"application-x-%s\"/>
        <glob pattern=\"*.%s\"/>
    </mime-type>
</mime-info>" handler comment handler extension)))

    (make-directory "~/.local/share/mime/packages" t)
    (write-region xml nil (format  "~/.local/share/mime/packages/%s.xml" handler)))

  (shell-command (format  "xdg-mime default %s.desktop application/x-%s" handler handler))
  (shell-command "update-mime-database ~/.local/share/mime")
  (shell-command "update-desktop-database ~/.local/share/applications"))

(defun ap/register-mime-types ()
  (interactive)
  (register-new-mime-type "drawio" "drawio" "drawio")
  (register-new-mime-type "excalidraw" "excalidraw" "excalidraw"))
