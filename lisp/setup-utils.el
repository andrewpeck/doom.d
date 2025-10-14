;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'outline)
  (require 'org-table)
  (require 'org-fold)
  (require 'org)
  (require 'the-org-mode-expansions))

;;------------------------------------------------------------------------------
;; Utility Functions
;;------------------------------------------------------------------------------

(defun make-declare ()
  "Make a function declaration from the symbol at point."
  (interactive)
  (let* ((function (symbol-at-point))
         (file (file-name-base (symbol-file function)))
         (args (or (help-function-arglist function) "()")))
    (save-excursion
      (evil-open-above 1)
      (insert (format "(declare-function %s \"%s\" %s)" function file args)))))

(defun scratch-new ()
  "Create a new empty scratch buffer."
  (interactive)
  (tab-new)
  (let ((xbuf (generate-new-buffer "scratch")))
    (switch-to-buffer xbuf)
    (funcall initial-major-mode) xbuf))

(defun byte-compile-config ()
  (interactive)
  (byte-recompile-directory (expand-file-name "~/.doom.d") 0))

(defun open-link-or (fn)
  (cond
   ((thing-at-point 'url) (link-hint-open-link-at-point))
   (t (call-interactively fn))))

(defun re-indent-buffer ()
  (interactive)
  (evil-indent (point-min) (point-max)))

(defun ap/tab-fallthrough ()
  (interactive)
  (or
   ;; evil jump item throws a user-error if a jumpable item is not
   ;; found, so we have to catch the error with a condition case
   ;; and turn it into a nil
   (condition-case nil (evil-jump-item) (user-error nil))
   (progn
     (print "Outline Cycle")
     (outline-cycle))))

(defvar preferred-terminal
  'wezterm
  "Default terminal used by `open-pwd-in-terminal'")

(defun open-pwd-in-terminal ()
  "Opens the present working directory in external terminal."
  (interactive)
  (pcase preferred-terminal
    ('ghostty (call-process "ghostty" nil 0 nil (concat "--working-directory=" default-directory)))
    ('wezterm (call-process "wezterm" nil 0 nil "start" "--always-new-process" "--cwd" default-directory))
    ('konsole (call-process "konsole" nil 0 nil "--workdir" default-directory))
    ('terminator (call-process "terminator" nil 0 nil "--working-directory" default-directory))
    ('nil (error "`preferred-terminal' not set."))
    (_ (error (format  "`preferred-terminal' %s not recognized" preferred-terminal)))))

(defun run-in-terminal (cmd)
  (pcase preferred-terminal
    ('wezterm (apply #'call-process (nconc (list "wezterm" nil 0 nil "-e") (if (listp cmd) cmd (list cmd)))))
    ('nil (error "`preferred-terminal' not set."))
    (_ (error (format  "`preferred-terminal' %s not recognized" preferred-terminal)))))

(defun open-buffer-in-vim ()
  "Opens the current buffer in gvim."
  (interactive)
  (run-in-terminal (list "nvim" (buffer-file-name))))

(defalias 'gvim #'open-buffer-in-vim)

(defun org-make-tables-pretty ()
  "Makes org mode tables pretty in the current buffer."
  (interactive)
  (org-table-map-tables 'org-table-align))

(defun xdg-do (x)
  (call-process (executable-find "xdg-open") nil 0 nil x))

(defun xdg-browse-directory ()
  "Open the current file's directory however the OS would."
  (interactive)
  (xdg-do default-directory))

(defun xdg-open-file ()
  "Open the current file however the OS would."
  (interactive)
  (when (buffer-file-name)
    (xdg-do (buffer-file-name)))
  (when (eq major-mode 'dired-mode)
    (xdg-do default-directory)))

(defun py-black ()
  "Format python file with black."
  (interactive)
  (save-buffer)
  (let ((pt (point)))
    (save-excursion
      (if (not (executable-find "black"))
          (error "Python Black not found. Please install (pip install black)")
        (shell-command-on-region
         (point-min) (point-max)        ; beginning and end of buffer
         "black -l 100 -"               ; command and parameters
         (current-buffer)               ; output buffer
         t                              ; replace?
         "*Python Black Error Buffer*"  ; name of the error buffer
         nil))) ; show error buffer?
    (goto-char pt)))

(defun autopep ()
  "Format python file with autopep."
  (interactive)
  (save-buffer)
  (let ((pt (point)))
    (if (not (executable-find "black"))
        (error "Autopep not found. Please install (pip install autopep8)")
      (shell-command-on-region
       (point-min) (point-max)         ; beginning and end of buffer
       "autopep8 -"                    ; command and parameters
       (current-buffer)                ; output buffer
       t                               ; replace?
       "*Python Autopep Error Buffer*" ; name of the error buffer
       nil))
    (goto-char pt)))                   ; show error buffer?

(defun verible-format ()
  "Format verilog file with verible."
  (interactive)
  (save-buffer)
  (if (not (executable-find "verible-verilog-format"))
      (message "verible-verilog-format not found")
    (print (shell-command-to-string
            (string-join `("verible-verilog-format"
                           "--inplace "
                           "--port_declarations_alignment    align"
                           "--port_declarations_indentation  wrap"
                           "--named_port_alignment           align"
                           "--assignment_statement_alignment align"
                           "--formal_parameters_alignment"   "align"
                           "--try_wrap_long_lines"           "false"
                           "--port_declarations_right_align_unpacked_dimensions true"
                           "--struct_union_members_alignment align"
                           "--formal_parameters_indentation  indent"
                           "--named_parameter_alignment      align"
                           "--named_parameter_indentation    indent"
                           ,(buffer-file-name)) " "))))
  (revert-buffer))

(defun pyment ()
  "Format python file with pyment"
  (interactive)
  (save-buffer)
  (if (not (executable-find "pyment"))
      (message "Python pyment not found. Please install (pip install pyment)")
    (shell-command-on-region
     (point-min) (point-max)            ; beginning and end of buffer
     "pyment -o google -w -"            ; command and parameters
     (current-buffer)                   ; output buffer
     t                                  ; replace?
     "*Python Pyment Error Buffer*"     ; name of the error buffer
     t)))                               ; show error buffer?

;; Backspace to switch to last buffer
(defun switch-to-previous-buffer ()
  "Switch to previously open buffer. Repeated invocations toggle
between the two most recently open buffers."
  (interactive)
  ;; (evil-switch-to-windows-last-buffer)
  (let* ((current (current-buffer))
         (other (other-buffer current 1)))
    (when (and  (buffer-file-name other)
                (buffer-file-name current))
      (switch-to-buffer other))))

(defun open-todo ()
  "Open my todo file"
  (interactive)
  (require 'org)
  (find-file +org-capture-todo-file))

(defun org-agenda-and-todo ()
  "Open the full org agenda + todo"
  (interactive)
  (org-agenda nil "n"))

(defun open-timesheet ()
  "Open my EDF timesheet"
  (interactive)
  (find-file "~/work/billing/billing.org")
  (goto-char (point-max))
  (re-search-backward "TBLFM")
  (org-reveal)
  (forward-line -1)
  (forward-line -1)
  (recenter-top-bottom))

;;------------------------------------------------------------------------------
;; Crontab
;;------------------------------------------------------------------------------

(defun crontab-e ()
  "Run `crontab -e' in a emacs buffer."
  (interactive)
  (with-editor-async-shell-command "crontab -e"))

;;------------------------------------------------------------------------------
;; Remove all advice from symbol
;;------------------------------------------------------------------------------

(defun advice-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

;;------------------------------------------------------------------------------
;; Multiplication in Region
;;------------------------------------------------------------------------------

(defun mult-by (by)
  ""
  (when (re-search-forward "\\b[-0-9.]+\\b" nil t)
    (replace-match (number-to-string (* by (string-to-number (match-string 0)))) nil t) t))

(defun mult-in-rectangle (start end)
  "Multiplies all numbers in region by some coefficient.

FIXME: should support non-rectangular regions

FIXME: does not work when the region is not rectangular at the end of
lines, e.g.

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

;; Disable auto fill mode in text modes
;; (remove-hook 'text-mode-hook #'auto-fill-mode)
;; 
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

(defun reindent-buffer-with-vim ()
  "Reindent buffer file with vim."
  (interactive)
  ;; TODO: use a call process for this, use buffer contents and replace buffer
  (shell-command (format "vim --clean -c 'set tabstop=2 softtabstop=0 expandtab shiftwidth=2 smarttab' -c 'normal gg=G' -c 'wq' %s" buffer-file-name)))

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
  "Alphabetically sort a block of elisp code.

The block should be encased by
;; start:sort
;; end:sort"
  (interactive)
  (save-excursion
    (sort-code-block ";;")))

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

;;------------------------------------------------------------------------------
;; Comment Block Helper
;;------------------------------------------------------------------------------

(defvar normalize-comment-strings-length 83
  "Number of characters to normalize comment strings to.")

(defun normalize-comment-strings ()
  (interactive)
  (save-excursion
    (goto-char (point-min))

    (let* ((comment-char (pcase major-mode
                           ('verilog-mode "\/\/")
                           ('c-mode "\/\/")
                           ('c++-mode "\/\/")
                           ('python-mode "#")
                           ('python-ts-mode "#")
                           ('tcl-mode "#")
                           ('emacs-lisp-mode ";;")))
           (re (format "^\\([[:blank:]]*\\)\\(%s[[:blank:]]*\\)\\(-+\\)[[:blank:]]*$" comment-char)))

      (unless comment-char
        (error (format "`comment-char' not defined for major mode %s" major-mode)))

      (while (re-search-forward re nil t)

        (let ((whitespace (match-string 1))
              (comment (match-string 2))
              )

          (goto-char (line-beginning-position))
          (kill-line)
          (insert whitespace)
          (insert comment)
          (insert-char #x2D
                       (- normalize-comment-strings-length
                          (length whitespace)
                          (length comment))))))))

;;------------------------------------------------------------------------------
;; Date Format
;;------------------------------------------------------------------------------

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

;;------------------------------------------------------------------------------
;; De-latexify helper
;;------------------------------------------------------------------------------

(defun fix-latex-pasted-text ()

  "Fix text pasted from Latex PDFs.

Text copy pasted from Latex likes to hyphen- ate inappropriately.

This function tries to de hyphenate them."

  (interactive)

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

;;------------------------------------------------------------------------------
;; Calc with modified title for i3
;;------------------------------------------------------------------------------

(defun calc-popup ()
  (interactive)
  (switch-to-buffer (generate-new-buffer "Calc Popup"))
  (calc-mode)
  (calc-big-language)
  (with-current-buffer (calc-trail-buffer)
    (and calc-display-trail
         (calc-trail-display nil t)))
  (set-frame-width (selected-frame) 75)  
  (set-frame-height (selected-frame) 30))

(defvar my/xdg-data-dirs
  (mapcar (lambda (dir) (expand-file-name "applications" dir))
          (cons (xdg-data-home)
                (xdg-data-dirs)))
  "Directories in which to search for applications (.desktop files).")

(defun my/list-desktop-files ()
  "Return an alist of all Linux applications.
Each list entry is a pair of (desktop-name . desktop-file).
This function always returns its elements in a stable order."
  (let ((hash (make-hash-table :test #'equal))
        result)
    (dolist (dir my/xdg-data-dirs)
      (when (file-exists-p dir)
        (let ((dir (file-name-as-directory dir)))
          (dolist (file (directory-files-recursively dir ".*\\.desktop$"))
            (let ((id (subst-char-in-string ?/ ?- (file-relative-name file dir))))
              (when (and (not (gethash id hash)) (file-readable-p file))
                (push (cons id file) result)
                (puthash id file hash)))))))
    result))

(defun my/mime-get-mime-type ()
  (string-trim
   (shell-command-to-string
    (format "xdg-mime query filetype %s" buffer-file-name))))

(defun my/mime-set-mime-type (mime-type handler)
  (when-let* ((cmd (format "xdg-mime default %s %s" handler mime-type)))
    (shell-command-to-string cmd)
    (message (format "Mime type for \"%s\" is \"%s\"" mime-type
                     (string-trim (shell-command-to-string (format "xdg-mime query default %s" mime-type)))))))

(defun my/update-mime-type ()
  "Update the mime association of the current file."
  (interactive)
  (when-let* ((mime-type (my/mime-get-mime-type))
              (handler (completing-read "Handler: " (mapcar #'car (my/list-desktop-files)))))
    (when (yes-or-no-p (format "Update %s to be handled by %s?" mime-type handler))
      (my/mime-set-mime-type mime-type handler))))

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

  (let* ((filename (format  "~/.local/share/mime/packages/application-x-%s.xml" handler))
         (xml  (format "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<mime-info xmlns=\"http://www.freedesktop.org/standards/shared-mime-info\">
    <mime-type type=\"application/x-%s\">
        <comment>\"%s\"</comment>
        <icon name=\"application-x-%s\"/>
        <glob-deleteall/>
        <glob pattern=\"*.%s\"/>
    </mime-type>
</mime-info>" handler comment handler extension))
         )

    (make-directory "~/.local/share/mime/packages" t)
    (write-region xml nil filename)

    (shell-command (format  "xdg-mime default %s.desktop application/x-%s" handler handler))
    (shell-command "update-mime-database ~/.local/share/mime")
    (shell-command "update-desktop-database ~/.local/share/applications")
    (shell-command (format "xdg-mime install %s" filename))))

(defun my/register-mime-types ()
  "Register a few mimetypes that are useful for me.

Make sure perl-file-mimeinfo is installed."
  (interactive)
  (register-new-mime-type "drawio" "drawio" "drawio")
  (register-new-mime-type "emacs" "el" "emacs")
  (register-new-mime-type "excalidraw" "excalidraw" "excalidraw"))
