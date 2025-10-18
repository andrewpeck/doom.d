;; -*- Lexical-binding: t; -*-

(eval-when-compile
  (require 'outline)
  (require 'org-table)
  (require 'org-fold)
  (require 'org)
  (require 'the-org-mode-expansions))

;;------------------------------------------------------------------------------
;; Utility Functions
;;------------------------------------------------------------------------------

;;;###autoload
(defun copy-file-name-to-kill ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;;;###autoload
(defun make-declare ()
  "Make a function declaration from the symbol at point."
  (interactive)
  (let* ((function (symbol-at-point))
         (file (file-name-base (symbol-file function)))
         (args (or (help-function-arglist function) "()")))
    (save-excursion
      (evil-open-above 1)
      (insert (format "(declare-function %s \"%s\" %s)" function file args)))))

;;;###autoload
(defun scratch-new ()
  "Create a new empty scratch buffer."
  (interactive)
  (tab-new)
  (let ((xbuf (generate-new-buffer "scratch")))
    (switch-to-buffer xbuf)
    (funcall initial-major-mode) xbuf))

;;;###autoload
(defun byte-compile-config ()
  (interactive)
  (byte-recompile-directory (expand-file-name "~/.doom.d") 0))

;;;###autoload
(defun open-link-or (fn)
  (cond
   ((thing-at-point 'url) (link-hint-open-link-at-point))
   (t (call-interactively fn))))

;;;###autoload
(defun re-indent-buffer ()
  (interactive)
  (evil-indent (point-min) (point-max)))

;;;###autoload
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

;;;###autoload
(defvar preferred-terminal
  'wezterm
  "Default terminal used by `open-pwd-in-terminal'")

;;;###autoload
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

;;;###autoload
(defun run-in-terminal (cmd)
  (pcase preferred-terminal
    ('wezterm (apply #'call-process (nconc (list "wezterm" nil 0 nil "-e") (if (listp cmd) cmd (list cmd)))))
    ('nil (error "`preferred-terminal' not set."))
    (_ (error (format  "`preferred-terminal' %s not recognized" preferred-terminal)))))

;;;###autoload
(defun open-buffer-in-vim ()
  "Opens the current buffer in gvim."
  (interactive)
  (run-in-terminal (list "nvim" (buffer-file-name))))

(defalias 'gvim #'open-buffer-in-vim)

;;;###autoload
(defun org-make-tables-pretty ()
  "Makes org mode tables pretty in the current buffer."
  (interactive)
  (org-table-map-tables 'org-table-align))

;;;###autoload
(defun xdg-do (x)
  (call-process (executable-find "xdg-open") nil 0 nil x))

;;;###autoload
(defun xdg-browse-directory ()
  "Open the current file's directory however the OS would."
  (interactive)
  (xdg-do default-directory))

;;;###autoload
(defun xdg-open-file ()
  "Open the current file however the OS would."
  (interactive)
  (when (buffer-file-name)
    (xdg-do (buffer-file-name)))
  (when (eq major-mode 'dired-mode)
    (xdg-do default-directory)))

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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
;;;###autoload
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

;;;###autoload
(defun open-todo ()
  "Open my todo file"
  (interactive)
  (require 'org)
  (find-file +org-capture-todo-file))

;;;###autoload
(defun org-agenda-and-todo ()
  "Open the full org agenda + todo"
  (interactive)
  (org-agenda nil "n"))

;;;###autoload
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

;;;###autoload
(defun crontab-e ()
  "Run `crontab -e' in a emacs buffer."
  (interactive)
  (with-editor-async-shell-command "crontab -e"))

;;------------------------------------------------------------------------------
;; Remove all advice from symbol
;;------------------------------------------------------------------------------

;;;###autoload
(defun advice-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

;;------------------------------------------------------------------------------
;; Multiplication in Region
;;------------------------------------------------------------------------------

;;;###autoload
(defun mult-by (by)
  ""
  (when (re-search-forward "\\b[-0-9.]+\\b" nil t)
    (replace-match (number-to-string (* by (string-to-number (match-string 0)))) nil t) t))

;;;###autoload
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

;;;###autoload
(defun copy-html-to-ohm ()
  (start-process
   "copy-to-ohm"
   nil "rsync" "-av"
   (format "%s.html" (file-name-base buffer-file-name))
   "ohm:~/public_html/notes/"))

;; C-backspace without modifying kill ring
;; https://emacs.stackexchange.com/questions/22266/backspace-without-adding-to-kill-ring

;;;###autoload
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

;;;###autoload
(defun my-backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
  With argument, do this that many times.
  This command does not push text to `kill-ring'."
  (interactive "p")
  (my-delete-word (- arg)))

(global-set-key (kbd "C-<backspace>") #'my-backward-delete-word)

;;;###autoload
(defun suspend ()
  "Suspend the system using systemctl syspend"
  (interactive)
  (call-process "systemctl" nil 0 nil "suspend"))

;;;###autoload
(defun reload-this-buffer ()
  "Kill and re-open the current buffer"
  (interactive)
  (revert-buffer)
  (let ((tmp-buffer-file (buffer-file-name)))
    (kill-buffer (buffer-name))
    (find-file tmp-buffer-file)))

(map! :leader :desc "Reload buffer" "b r" #'reload-this-buffer)

;;;###autoload
(defun fix-evil ()
  (interactive)
  (setq-local transient-mark-mode t))

;;;###autoload
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
;;;###autoload
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

;;;###autoload
(defun reindent-buffer-with-vim ()
  "Reindent buffer file with vim."
  (interactive)
  ;; TODO: use a call process for this, use buffer contents and replace buffer
  (shell-command (format "vim --clean -c 'set tabstop=2 softtabstop=0 expandtab shiftwidth=2 smarttab' -c 'normal gg=G' -c 'wq' %s" buffer-file-name)))

;;---------------------------------------------------------------------------------
;; Functions for alphabetically sorting items
;;---------------------------------------------------------------------------------

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun hdl-i++ ()
  "Insert a hdl i++ for either the current selection or symbol at point."
  (interactive)
  (hdl-self-op "+"))

;;;###autoload
(defun hdl-i-- ()
  "Insert a hdl i-- for either the current selection or symbol at point."
  (interactive)
  (hdl-self-op "-"))

;;------------------------------------------------------------------------------
;; Comment Block Helper
;;------------------------------------------------------------------------------

(defvar normalize-comment-strings-length 83
  "Number of characters to normalize comment strings to.")

;;;###autoload
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

;;;###autoload
(defvar current-date-format "%Y-%m-%d"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

;;;###autoload
(defun insert-current-date ()
  "insert the current date into the buffer.
The date will follow the format in `current-date-format'"
  (interactive)
  (insert (format-time-string current-date-format (current-time))))

;;------------------------------------------------------------------------------
;; De-latexify helper
;;------------------------------------------------------------------------------

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defvar my/xdg-data-dirs
  (progn
    (require 'xdg)
    (mapcar (lambda (dir) (expand-file-name "applications" dir))
            (cons (xdg-data-home)
                  (xdg-data-dirs))))
  "Directories in which to search for applications (.desktop files).") 

;;;###autoload
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

;;;###autoload
(defun my/mime-get-mime-type ()
  (string-trim
   (shell-command-to-string
    (format "xdg-mime query filetype %s" buffer-file-name))))

;;;###autoload
(defun my/mime-set-mime-type (mime-type handler)
  (when-let* ((cmd (format "xdg-mime default %s %s" handler mime-type)))
    (shell-command-to-string cmd)
    (message (format "Mime type for \"%s\" is \"%s\"" mime-type
                     (string-trim (shell-command-to-string (format "xdg-mime query default %s" mime-type)))))))

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun my/register-mime-types ()
  "Register a few mimetypes that are useful for me.

Make sure perl-file-mimeinfo is installed."
  (interactive)
  (register-new-mime-type "drawio" "drawio" "drawio")
  (register-new-mime-type "emacs" "el" "emacs")
  (register-new-mime-type "excalidraw" "excalidraw" "excalidraw"))

;;------------------------------------------------------------------------------
;; Org
;;------------------------------------------------------------------------------

;;;###autoload
(defun my/resize-org-latex-overlays ()
  "Helper function for latex text scaling.

From https://www.reddit.com/r/orgmode/comments/165zeuu/delighted_by_org_svg_preview/"
  (eval-when-compile (require 'face-remap))
  (cl-loop for o in (car (overlay-lists))
           if (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay)
           do (plist-put (cdr (overlay-get o 'display))
                         :scale (expt text-scale-mode-step
                                      text-scale-mode-amount))))



;;;###autoload
(defun org-archive-done ()
  "Interactive wrapper for org-archive-all-done"
  (interactive)
  (require 'org-archive)
  (org-archive-all-done))


;;;###autoload
(defun org-link-get ()
  "Extract URL from org-mode link and add return it"
  (let* ((link (org-element-lineage (org-element-context) '(link) t))
         (url (org-element-property :path link))) url))


(defvar org-drawio-template "~/.doom.d/template.drawio.svg")

;;;###autoload
(defun org-edit-drawio ()
  "Open Drawio on the image at point."
  (interactive)

  (unless (or (eq major-mode 'markdown-mode)
              (eq major-mode 'org-mode))
    (error "This function is only supported in org-mode and markdown-mode."))

  (let ((link (or (if (eq major-mode 'markdown-mode) (markdown-link-url) (org-link-get))
                  (read-string "Filename: "))))

    (when (eq major-mode 'org-mode)
      (unless (org-link-get)
        (insert (format "[[%s]]" link))))

    (when (eq major-mode 'markdown-mode)
      (unless (markdown-link-p)
        (insert (format "[](%s)" link))))

    (when link
      (unless (file-exists-p link)
        (copy-file org-drawio-template link))
      ;; (call-process "drawio" nil 0 nil "-g" link)
      (call-process "flatpak" nil 0 nil "run" "com.jgraph.drawio.desktop" (concat (file-name-directory (buffer-file-name)) link)))))

;;;###autoload
(defun org-edit-inkscape ()
  "Open Inkscape on the image at point."
  (interactive)

  (unless (or (eq major-mode 'markdown-mode)
              (eq major-mode 'org-mode))
    (error "This function is only supported in org-mode and markdown-mode."))

  (let ((link (or (if (eq major-mode 'markdown-mode) (markdown-link-url) (org-link-get))
                  (read-string "Filename: "))))

    (when (eq major-mode 'org-mode)
      (unless (org-link-get)
        (insert (format "[[%s]]" link))))

    (when (eq major-mode 'markdown-mode)
      (unless (markdown-link-p)
        (insert (format "[](%s)" link))))

    (when link
      (unless (file-exists-p link)
        (with-temp-file link
          (insert
           (concat  "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n"
                    "<!-- Created with Inkscape (http://www.inkscape.org/) -->\n"
                    "\n"
                    "<svg\n"
                    "   width=\"5in\"\n"
                    "   height=\"3in\"\n"
                    "   viewBox=\"0 0 279.4 215.9\"\n"
                    "   version=\"1.1\"\n"
                    "   id=\"svg5\"\n"
                    "   inkscape:version=\"1.1.2 (0a00cf5339, 2022-02-04)\"\n"
                    "   sodipodi:docname=\"template.svg\"\n"
                    "   xmlns:inkscape=\"http://www.inkscape.org/namespaces/inkscape\"\n"
                    "   xmlns:sodipodi=\"http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd\"\n"
                    "   xmlns=\"http://www.w3.org/2000/svg\"\n"
                    "   xmlns:svg=\"http://www.w3.org/2000/svg\">\n"
                    "  <sodipodi:namedview\n"
                    "     id=\"namedview7\"\n"
                    "     pagecolor=\"#ffffff\"\n"
                    "     bordercolor=\"#666666\"\n"
                    "     borderopacity=\"1.0\"\n"
                    "     inkscape:pageshadow=\"2\"\n"
                    "     inkscape:pageopacity=\"0.0\"\n"
                    "     inkscape:pagecheckerboard=\"0\"\n"
                    "     inkscape:document-units=\"mm\"\n"
                    "     showgrid=\"false\"\n"
                    "     units=\"in\"\n"
                    "     inkscape:zoom=\"0.77771465\"\n"
                    "     inkscape:cx=\"-109.29459\"\n"
                    "     inkscape:cy=\"561.25984\"\n"
                    "     inkscape:window-width=\"3786\"\n"
                    "     inkscape:window-height=\"2089\"\n"
                    "     inkscape:window-x=\"54\"\n"
                    "     inkscape:window-y=\"34\"\n"
                    "     inkscape:window-maximized=\"1\"\n"
                    "     inkscape:current-layer=\"layer1\" />\n"
                    "  <defs\n"
                    "     id=\"defs2\" />\n"
                    "  <g\n"
                    "     inkscape:label=\"Layer 1\"\n"
                    "     inkscape:groupmode=\"layer\"\n"
                    "     id=\"layer1\" />\n"
                    "</svg>"))))
      (call-process "inkscape" nil 0 nil "-g" link))))

;;;###autoload
(defun my/edit-image-helper (helper)
  (let ((link (pcase major-mode
                ('org-mode (org-link-get))
                ('image-mode (buffer-file-name))
                (_ nil))))
    (when link (call-process helper nil 0 nil link))))

;;;###autoload
(defun my/edit-gimp ()
  "Open GIMP on the image at point."
  (interactive)
  (my/edit-image-helper "gimp"))

;;;###autoload
(defun my/edit-pinta ()
  "Open Pinta on the image at point."
  (interactive)
  (my/edit-image-helper "pinta"))

;;------------------------------------------------------------------------------
;; Sorting
;;------------------------------------------------------------------------------

;;;###autoload
(defun my/org-sort-all-org-entries ()
  ""
  (interactive)
  (let ((fun #'(lambda nil
                 (condition-case nil
                     (org-sort-entries nil ?o)
                   (user-error t)))))
    (org-map-entries fun)))

;;;###autoload
(defun +org-sort-entries-recursive (&optional key)
  "Call `org-sort-entries' recursively on tree at point.
If KEY, use it; otherwise read key interactively."
  (interactive)
  (cl-macrolet ((moves-p (form)
                  `(let ((pos-before (point)))
                     ,form
                     (/= pos-before (point)))))
    (cl-labels ((sort-tree
                  () (cl-loop do (when (children-p)
                                   (save-excursion
                                     (outline-next-heading)
                                     (sort-tree))
                                   (org-sort-entries nil key))
                              while (moves-p (org-forward-heading-same-level 1))))
                (children-p (&optional _)
                  ;; Return non-nil if entry at point has child headings.
                  ;; Only children are considered, not other descendants.
                  ;; Code from `org-cycle-internal-local'.
                  (save-excursion
                    (let ((level (funcall outline-level)))
                      (outline-next-heading)
                      (and (org-at-heading-p t)
                           (> (funcall outline-level) level))))))
      (save-excursion
        (save-restriction
          (widen)
          (unless key
            ;; HACK: Call the sort function just to get the key, then undo its changes.
            (cl-letf* ((old-fn (symbol-function 'read-char-exclusive))
                       ((symbol-function 'read-char-exclusive)
                        (lambda (&rest args)
                          (setf key (apply #'funcall old-fn args)))))
              ;; Sort the first heading and save the sort key.
              (org-sort-entries))
            (undo-only))
          (cond ((org-before-first-heading-p)
                 ;; Sort whole buffer. NOTE: This assumes the first heading is at level 1.
                 (org-sort-entries nil key)
                 (outline-next-heading)
                 (cl-loop do (sort-tree)
                          while (moves-p (org-forward-heading-same-level 1))))
                ((org-at-heading-p)
                 ;; Sort this heading.
                 (sort-tree))
                (t (user-error "Neither on a heading nor before first heading"))))))))


;;;###autoload
(defun +org-sort-entries-recursive-multi (&optional keys)
  "Call `+org-sort-entries-recursive'.
If KEYS, call it for each of them; otherwise call interactively
until \\[keyboard-quit] is pressed."
  (interactive)
  (if keys
      (dolist (key keys)
        (ap/org-sort-entries-recursive key))
    (with-local-quit
      ;; Not sure if `with-local-quit' is necessary, but probably a good
      ;; idea in case of recursive edit.
      (cl-loop while (progn
                       (call-interactively #'+org-sort-entries-recursive) t)))))

;;------------------------------------------------------------------------------
;; 1
;;------------------------------------------------------------------------------

;;;###autoload
(defun ap/inline-org-inbox-link ()
  "Should probably remove this."
  (interactive)
  (save-excursion
    (let ((link nil)
          (description nil))

      (progn
        (progn
          (forward-line)
          (end-of-line)
          (push-mark (point) t t)
          (move-beginning-of-line 1)
          (setq link (buffer-substring-no-properties (region-beginning) (region-end)))
          (setq link (replace-regexp-in-string "^- " "" link))
          (forward-line -1)))

      (progn
        (end-of-line)
        (push-mark (point) t t)
        (re-search-backward "^\*+ ")
        (re-search-forward " ")
        (setq description (buffer-substring-no-properties (region-beginning) (region-end))))

      (org-insert-link nil link description)

      (replace-regexp-in-region "^\*+" "-" (line-beginning-position) (line-end-position))

      (forward-line)
      (beginning-of-line)
      (kill-line)
      (kill-line))))

;;------------------------------------------------------------------------------
;; Unpackaged
;;------------------------------------------------------------------------------

;; https://github.com/alphapapa/unpackaged.el#download-and-attach-remote-files

;;;###autoload
(define-minor-mode unpackaged/org-export-html-with-useful-ids-mode
  "Attempt to export Org as HTML with useful link IDs.
Instead of random IDs like \"#orga1b2c3\", use heading titles,
made unique when necessary."
  :global t
  (if unpackaged/org-export-html-with-useful-ids-mode
      (advice-add #'org-export-get-reference :override #'unpackaged/org-export-get-reference)
    (advice-remove #'org-export-get-reference #'unpackaged/org-export-get-reference)))

;;;###autoload
(defun unpackaged/org-export-get-reference (datum info)
  "Like `org-export-get-reference',
except uses heading titles instead of random numbers."
  (let ((cache (plist-get info :internal-references)))
    (or (car (rassq datum cache))
        (let* ((crossrefs (plist-get info :crossrefs))
               (cells (org-export-search-cells datum))
               ;; Preserve any pre-existing association between
               ;; a search cell and a reference, i.e., when some
               ;; previously published document referenced a location
               ;; within current file (see
               ;; `org-publish-resolve-external-link').
               ;;
               ;; However, there is no guarantee that search cells are
               ;; unique, e.g., there might be duplicate custom ID or
               ;; two headings with the same title in the file.
               ;;
               ;; As a consequence, before re-using any reference to
               ;; an element or object, we check that it doesn't refer
               ;; to a previous element or object.
               (new (or (cl-some
                         (lambda (cell)
                           (let ((stored (cdr (assoc cell crossrefs))))
                             (when stored
                               (let ((old (org-export-format-reference stored)))
                                 (and (not (assoc old cache)) stored)))))
                         cells)
                        (when (org-element-property :raw-value datum)
                          ;; Heading with a title
                          (unpackaged/org-export-new-title-reference datum cache))
                        ;; NOTE: This probably breaks some Org Export
                        ;; feature, but if it does what I need, fine.
                        (org-export-format-reference
                         (org-export-new-reference cache))))
               (reference-string new))
          ;; Cache contains both data already associated to
          ;; a reference and in-use internal references, so as to make
          ;; unique references.
          (dolist (cell cells) (push (cons cell new) cache))
          ;; Retain a direct association between reference string and
          ;; DATUM since (1) not every object or element can be given
          ;; a search cell (2) it permits quick lookup.
          (push (cons reference-string datum) cache)
          (plist-put info :internal-references cache)
          reference-string))))

;;;###autoload
(defun unpackaged/org-export-new-title-reference (datum cache)
  "Return new reference for DATUM that is unique in CACHE."
  (cl-macrolet
      ((inc-suffixf (place)
         `(progn
            (string-match (rx bos
                              (minimal-match (group (1+ anything)))
                              (optional "--" (group (1+ digit)))
                              eos)
                          ,place)
            ;; HACK: `s1' instead of a gensym.
            (-let* (((s1 suffix) (list (match-string 1 ,place)
                                       (match-string 2 ,place)))
                    (suffix (if suffix
                                (string-to-number suffix)
                              0)))
              (setf ,place (format "%s--%s" s1 (cl-incf suffix)))))))
    (let* ((title (org-element-property :raw-value datum))
           (ref (url-hexify-string (substring-no-properties title)))
           (parent (org-element-property :parent datum)))
      (while (--any (equal ref (car it))
                    cache)
        ;; Title not unique: make it so.
        (if parent
            ;; Append ancestor title.
            (setf title (concat (org-element-property :raw-value parent)
                                "--" title)
                  ref (url-hexify-string (substring-no-properties title))
                  parent (org-element-property :parent parent))
          ;; No more ancestors: add and increment a number.
          (inc-suffixf ref)))
      ref)))

;;;###autoload
(defun org-get-linked-files (&optional buffer)
  "Get all of the `file' type links in a buffer.
Current buffer is assumed unless specified by BUFFER"
  (with-current-buffer
      (or buffer (current-buffer))
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (when (string= (org-element-property :type link) "file")
          (org-element-property :path link))))))

;;;###autoload
(defun org-get-links (&optional buffer)
  "Get all of the `file' type links in a buffer.
Current buffer is assumed unless specified by BUFFER"
  (with-current-buffer
      (or buffer (current-buffer))
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (org-element-property :raw-link link)))))

;;;###autoload
(defun org-global-props (&optional property buffer)
  "Get the plists of global org properties of current buffer."

  (unless property
    (setq property "PROPERTY"))

  (with-current-buffer
      (or buffer (current-buffer))
    (org-element-map (org-element-parse-buffer) 'keyword
      (lambda (el)
        (when (string-match property (org-element-property :key el))
          el)))))

;;;###autoload
(defun org-global-prop-value (key)
  "Get global org property KEY of current buffer."
  (org-element-property :value (car (org-global-props key))))

;;------------------------------------------------------------------------------
;; Publishing
;;------------------------------------------------------------------------------

;;;###autoload
(defvar org-default-publish-dest "nfs:/home/public"
  "Default org publishing destination for `org-publish-this-file'.
It will be pushed with rsync.")

;;;###autoload
(defun org-publish-this-file ()
  "Publish this Org mode file.

If a DEST property is specified in the org file it will by used
as the destination. e.g. the following will set the copy
destination of the file.

#+DEST: ohm:public_html/notes/


Copying is done with rsync, which must be installed on both the
local and remote servers."

  (interactive)

  ;; export the file to html
  ;; - export _synchronously_ so that it blocks
  ;; - use useful IDs so that the links are stable
  (message "Exporting to html...")
  (unpackaged/org-export-html-with-useful-ids-mode t)
  (org-html-export-to-html nil)

  (message "Publishing...")

  (let* ((base (file-name-base (buffer-file-name)))
         (outfile (or (org-global-prop-value "DESTFILE")
                      (concat base ".html"))))

    (when (not (string= (concat base ".html") outfile))
      (rename-file (concat base ".html") outfile))

    (let* ((dest (or (org-global-prop-value "DEST")
                     org-default-publish-dest))
           (args

            (cl-concatenate 'list
                            (list "-avz" "--relative" outfile)
                            (org-get-linked-files) ;; get attachments and publish them also
                            (list dest))))

      (unless dest
        (error "No Org publishing destination specified. Provide a value for `org-default-publish-dest' or in the #+DEST: header of your org file."))

      (message (mapconcat #'identity args " "))

      (message (concat "rsync " (string-join args " ")))

      (set-process-sentinel

       ;; copy
       (apply #'start-process "*copy-to-dest*" nil "rsync" args)

       ;; cleanup
       (lambda (_ event)
         (if (string= event "finished\n")
             (message "rsync finished, cleaning up.")
           (error "rsync FAILED, cleaning up."))
         (delete-file outfile))))))

;;------------------------------------------------------------------------------
;; Linked File Functions
;;------------------------------------------------------------------------------

;;;###autoload
(defun org-rename-file-at-point ()
  (interactive)
  (save-buffer)
  (let ((link (org-element-context)))
    ;;(print link)
    (if (string= (org-element-property :type link) "file")
        (let ((old (org-element-property :path link)))
          (progn
            ;;(print (format "mv %s %s" old (read-string "New name:" old)))
            (org-move-linked-file (file-name-directory (buffer-file-name))
                                  old (read-string "New name:" old))
            (revert-buffer))))))

;;;###autoload
(defun org-move-linked-file (rootpath old new)
  (if (file-exists-p new)
      (error "Destination file exists!"))
  (if (not (file-exists-p old))
      (error "Source file does not exist!"))

  (rename-file old new)

  (let ((org-files (split-string
                    (shell-command-to-string
                     (format "find %s -name \"*.org\" -type f" rootpath)))))
    (dolist (file org-files)
      (if (not (string= "" (shell-command-to-string (format "rg -l %s %s" old file))))
          (progn
            (princ (format "Renaming in %s\n" file))
            (shell-command (format "sed -i 's|%s|%s|g' %s" old new file)))))))

;;;###autoload
(defun org-report-dead-links ()
  "Create a report of dead files in my org mode notes directory."
  (interactive)
  (shell-command (format "cd %s && ./find-dead-links.sh" org-directory))
  (find-file (concat org-directory "/unused-links.org")))


;;------------------------------------------------------------------------------
;; Table Functions
;;------------------------------------------------------------------------------

;;;###autoload
(defun org-table-export-all ()
  "Export to CSV all named tables in current org mode file"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (outline-show-all)
    (let ((case-fold-search t))
      (while (search-forward-regexp "#\\(\\+TBLNAME: \\|\\+TBLNAME: \\)\\(.*\\)" nil t)
        (let ((name (match-string-no-properties 2)))
          (progn
            (forward-line)
            (princ (format "Exporting table to %s.csv\n" name))
            (org-table-export (format "%s.csv" name) "orgtbl-to-csv")))))))

;;------------------------------------------------------------------------------
;; LaTex
;;------------------------------------------------------------------------------

;;;###autoload
(defun org-latex-preview-all ()
  (interactive)
  (org-latex-preview '(16)))

;;;###autoload
(defun org-latex-preview-clear ()
  (interactive)
  (org-latex-preview '(64)))


;;------------------------------------------------------------------------------
;; SciMAX Org-Return
;;------------------------------------------------------------------------------

;;;###autoload
(defun scimax/org-return (&optional ignore)
  "Add new list item, heading or table row with RET.
   A double return on an empty element deletes it.
   Use a prefix arg to get regular RET. "
  (interactive "P")

  (eval-when-compile
    (require 'org-inlinetask)
    (require 'org))

  (if ignore
      (org-return)
    (cond

     ((eq 'line-break (car (org-element-context)))
      (org-return t))

     ;; Open links like usual, unless point is at the end of a line.
     ;; and if at beginning of line, just press enter.
     ((or (and (eq 'link (car (org-element-context))) (not (eolp)))
          (bolp))
      (org-return))

     ;; It doesn't make sense to add headings in inline tasks. Thanks Anders
     ;; Johansson!
     ((org-inlinetask-in-task-p)
      (org-return))

     ;; checkboxes too
     ((org-at-item-checkbox-p)
      (org-insert-todo-heading nil))

     ;; lists end with two blank lines, so we need to make sure we are also not
     ;; at the beginning of a line to avoid a loop where a new entry gets
     ;; created with only one blank line.
     ((org-in-item-p)
      (if (save-excursion (beginning-of-line) (org-element-property :contents-begin (org-element-context)))
          (org-insert-heading)
        (beginning-of-line)
        (delete-region (line-beginning-position) (line-end-position))
        (org-return)))

     ;; org-heading
     ((org-at-heading-p)
      (if (not (string= "" (org-element-property :title (org-element-context))))
          (progn (org-end-of-meta-data)
                 (org-insert-heading-respect-content)
                 (outline-show-entry))
        (beginning-of-line)
        (setf (buffer-substring
               (line-beginning-position) (line-end-position)) "")))

     ;; tables
     ((org-at-table-p)
      (if (-any?
           (lambda (x) (not (string= "" x)))
           (nth
            (- (org-table-current-dline) 1)
            (org-table-to-lisp)))
          (org-return)
        ;; empty row
        (beginning-of-line)
        (setf (buffer-substring
               (line-beginning-position) (line-end-position)) "")
        (org-return)))

     ;; fall-through case
     (t
      (org-return)))))

;;;###autoload
(defun org-toggle-checkbox-presence ()
  "Toggle the presence of org list checkboxes."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively #'org-toggle-checkbox)))


;;------------------------------------------------------------------------------
;; Helpers
;;------------------------------------------------------------------------------

;;;###autoload
(defun org-procrastinate-all ()

  "Carry forward uncompleted tasks.
Updates overdue tasks to be due today."

  (interactive)
  (org-carry-forward-uncompleted-tasks t))

;;;###autoload
(defun org-procrastinate ()
  (interactive)
  (org-deadline nil "+1"))

;;;###autoload
(defun org-carry-forward-uncompleted-tasks (&optional procrastinate)
  "Carry forward uncompleted tasks.

Updates overdue tasks to be due today."

  (interactive)

  (save-excursion

    (goto-char (point-max))

    (while (re-search-backward "^[\*]* \\(TODO\\|MEET\\)" nil t)
      (unless (org-at-heading-p)
        (org-back-to-heading t))

      (let* ((element (org-element-at-point))
             ;; (todo-state (org-element-property :todo-keyword element))
             (deadline (org-element-property :deadline element))
             (repeats (org-element-property :repeater-value deadline))
             (deadline-time-stamp
              (when deadline
                (time-to-days
                 (org-time-string-to-time
                  (org-element-property :raw-value deadline)))))
             (today (time-to-days (current-time))) )
        (when (and deadline-time-stamp
                   (or (> today deadline-time-stamp)
                       (and procrastinate
                            (>= today deadline-time-stamp)))) ;; deadline is overdue
          (if repeats (org-todo 'done)
            (if procrastinate
                (org-deadline nil "+1")
              (org-deadline nil "."))))))))

;;;###autoload
(defun ap/org-log-weight ()

  "Log today's weight in my notes file."

  (interactive)

  (let ((file  (concat org-directory "/weight.org"))
        (weight (string-to-number (read-string "Weight: " ""))))

    ;; only write if the weight conversion was sane
    (when (and (> weight 130)
               (< weight 250))
      (with-temp-buffer
        (org-mode)
        (insert-file-contents file)

        ;; goto weight table
        (search-forward "#+tblname: weight")

        ;; goto end of table
        (goto-char (org-table-end))
        (backward-char)
        (org-table-goto-column 1)

        ;; open a new row and log the weight
        (org-table-insert-row)
        (insert (format-time-string "%Y/%m/%d"))
        (org-forward-sentence)
        (insert (format "%5.1f" weight))

        ;; realign the table
        (org-table-align)

        ;; save
        (write-file file)

        (princ weight)))))


;;;###autoload
(defun +org-shrink-this-image ()
  (interactive)

  (let* ((name (buffer-file-name))
         (name-base (file-name-base name))
         (ext (downcase (file-name-extension name)))
         (new-name (concat name-base "-small.jpg")))

    ;; convert to jpg
    (when (not (or (string= ".jpeg" ext)
                   (string= ".jpg" ext)))

      (message "Converting to jpg...")
      (shell-command (format "convert %s %s.jpg" name name-base))
      (setq name (concat name-base ".jpg")))

    ;; shrink
    (message "Resizing $i...")
    (shell-command (format  "convert -resize 1024X768 %s %s-small.jpg" name name-base))

    ;; return the new-name
    new-name))

;;;###autoload
(defun +org-shrink-and-replace-this-image ()
  (interactive)
  (let ((new-name (+org-shrink-this-image)))
    (when new-name
      (rename-file new-name  (buffer-file-name)))))

;; https://github.com/doomemacs/doomemacs/pull/7002
;; (defun +org/return ()
;;   "Call `org-return' then indent (if `electric-indent-mode' is on)."
;;   (interactive)
;;   (if (and (modulep! :completion corfu)
;;            corfu--frame
;;            (frame-visible-p corfu--frame))
;;       (corfu-insert)
;;     (org-return electric-indent-mode)))

;;;###autoload
(defun pandoc-buffer-to-org ()
  (interactive)
  (shell-command
   (concat "pandoc " (shell-quote-argument (buffer-file-name)) " -o "
           (shell-quote-argument (file-name-sans-extension (buffer-file-name))) ".org")))

;;;###autoload
(defun org-shorten-url-by-title ()
  "Use title of URL at point as link name."
  (interactive)
  (let* ((org-link-pos (org-in-regexp org-link-any-re))
         (beg (car org-link-pos))
         (end (cdr org-link-pos))
         (url (buffer-substring-no-properties beg end))
         (desc (www-get-page-title url)))
    (when desc
      (delete-region beg end)
      (org-insert-link nil url desc))))

;;;###autoload
(defun md-shorten-url-by-title ()
  (interactive)
  (org-shorten-url-by-title)
  (org-link->markdown))

;;;###autoload
(defun org-capture-url-from-clipboard (_)
  "Capture a URL from clipboard and paste it as an org link"
  (interactive)
  (org-capture-url (current-kill 0)))

;;;###autoload
(defun my/url->org ()
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

;;;###autoload
(defun my/url->md ()
  "Convert the URL at point into an md mode formatted link. The
  title of the page is retrieved from the web page"
  (interactive)
  (ap/url->org)
  (org-link->markdown))

;;;###autoload
(defun org-shorten-indico-url ()
  "Takes an indico (or some other url) of the form xxxxx...xxx/some_file.pdf
and shortens it into an org mode link consisting of just `some file`"
  (interactive)
  (let* ((org-link-pos (org-in-regexp org-link-any-re))
         (beg (car org-link-pos))
         (end (cdr org-link-pos))
         (url (buffer-substring-no-properties beg end))
         (desc (unless (string-blank-p url) url))
         (desc (replace-regexp-in-string  "^.*\/" "" desc))
         (desc (replace-regexp-in-string  "_" " " desc))
         (desc (replace-regexp-in-string  "\%20" " " desc))
         (desc (replace-regexp-in-string  "\s+" " " desc))
         (desc (file-name-sans-extension desc)))

    (when desc
      (delete-region beg end)
      (org-insert-link nil url desc))))

;;;###autoload
(defun md-shorten-indico-url ()
  (interactive)
  (org-shorten-indico-link)
  (org-link->markdown))

;; Functions to Convert to/from org / markdown links

;;;###autoload
(defun org-link->markdown ()
  (interactive)
  (let* ((ctx (org-in-regexp org-link-any-re))
         (beg (car ctx)) (end (cdr ctx))
         (link-txt (buffer-substring beg end))
         (parsed (unless (string-blank-p link-txt)
                   (seq-map
                    ;; escape square brackets and parens, see:
                    ;; https://emacs.stackexchange.com/questions/68814/escape-all-square-brackets-with-replace-regexp-in-string
                    (lambda (m)
                      (replace-regexp-in-string "\\[\\|\\]\\|(\\|)" "\\\\\\&" m))
                    (org-link-parse link-txt)))))
    (when parsed
      (delete-region beg end)
      (insert (apply 'format "[%s](%s)" (reverse parsed))))))

;;;###autoload
(defun markdown-link->org ()
  (interactive)
  (require 'markdown)
  (when (markdown-link-p)
    (let* ((l (markdown-link-at-pos (point)))
           (desc (nth 2 l))
           (url (nth 3 l)))
      (markdown-kill-thing-at-point)
      (org-insert-link nil url desc))))

;; from
;; (https://github.com/SqrtMinusOne/dotfiles/blob/4b176a5bb1a5e20a7fdd7398b74df79701267a7e/.emacs.d/init.el)
;;;###autoload
(defun org-link-copy (&optional arg)
  "Extract URL from org-mode link and add it to kill ring."
  (interactive "P")
  (let* ((link (org-element-lineage (org-element-context) '(link) t))
         (type (org-element-property :type link))
         (url (org-element-property :path link))
         (url (concat type ":" url)))
    (kill-new url)
    (message (concat "Copied URL: " url))))

;;;###autoload
(defun org-remove-link-and-trash-linked-file ()
  "Remove `org-mode' link at point and trash linked file."
  (interactive)
  (let* ((link (org-element-context))
         (path (org-element-property :path link)))
    (move-file-to-trash path)
    (delete-region (org-element-property :begin link)
                   (org-element-property :end link))))

;;------------------------------------------------------------------------------
;; conversions
;;------------------------------------------------------------------------------

;; http://mbork.pl/2021-05-02_Org-mode_to_Markdown_via_the_clipboard
;;;###autoload
(defun org-copy-region-as-markdown ()
  "Copy the region (in Org) to the system clipboard as Markdown."
  (interactive)
  (if (use-region-p)
      (let* ((region
              (buffer-substring-no-properties
               (region-beginning)
               (region-end)))
             (markdown
              (org-export-string-as region 'md t '(:with-toc nil))))
        (gui-set-selection 'CLIPBOARD markdown))))

;;;###autoload
(defun org-web-tools--get-url (url)
  "Return content for URL as string.
This uses `url-retrieve-synchronously' to make a request with the
URL, then returns the response body.  Since that function returns
the entire response, including headers, we must remove the
headers ourselves."
  (let* ((response-buffer (url-retrieve-synchronously url nil t))
         (encoded-html (with-current-buffer response-buffer
                         ;; Skip HTTP headers.
                         ;; FIXME: Byte-compiling says that `url-http-end-of-headers' is a free
                         ;; variable, which seems to be because it's not declared by url.el with
                         ;; `defvar'.  Yet this seems to work fine...
                         (delete-region (point-min) url-http-end-of-headers)
                         (buffer-string))))
    ;; NOTE: Be careful to kill the buffer, because `url' doesn't close it automatically.
    (kill-buffer response-buffer)
    (with-temp-buffer
      ;; For some reason, running `decode-coding-region' in the
      ;; response buffer has no effect, so we have to do it in a
      ;; temp buffer.
      (insert encoded-html)
      (condition-case nil
          ;; Fix undecoded text
          (decode-coding-region (point-min) (point-max) 'utf-8)
        (coding-system-error nil))
      (buffer-string))))

;;;###autoload
(defun org-web-tools--html-title (html)
  "Return title of HTML page, or nil if it has none.
Uses the `dom' library."
  ;; Based on `eww-readable'
  (let* ((dom (with-temp-buffer
                (insert html)
                (libxml-parse-html-region (point-min) (point-max))))
         (title (cl-caddr (car (dom-by-tag dom 'title)))))
    (when title
      (org-web-tools--cleanup-title title))))

;;;###autoload
(defun www-get-page-title (url)
  "Gets the title of a webpage at URL"
  (org-web-tools--html-title (org-web-tools--get-url url)))

;;;###autoload
(defun org-capture-url (url)
  (insert (org-insert-link nil url (www-get-page-title url))))

;; taken from:
;; https://github.com/agzam/.doom.d/blob/main/modules/custom/org/autoload/custom.el#L181-L214
;;;###autoload
(defun org-link-parse (link)
  ;; borrowed and adopted from:
  ;; github.com/xuchunyang/emacs.d/blob/5f4f873cf7a671a36f686f3d1346fd7c5a5462bc/lisp/chunyang-misc.el#L488-L526
  (if (string-match
       (rx "[[" (group (0+ anything)) "][" (group (0+ anything)) "]]")
       link)
      (list (match-string 1 link)
            (match-string 2 link))
    (error "Cannot parse %s as Org link" link)))

;;------------------------------------------------------------------------------
;; Fini
;;------------------------------------------------------------------------------

(loaddefs-generate "~/.doom.d/autoload/"
                   "~/.doom.d/loaddefs.el")

(provide 'my-defuns)
;;; my-defuns.el ends here
