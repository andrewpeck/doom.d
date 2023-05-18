;; -*- lexical-binding: t; -*-

;;; Org Mode
;;------------------------------------------------------------------------------

(after! org

  ;; https://list.orgmode.org/87iljvgp2z.fsf@localhost/
  (defun org-babel-execute-src-block (&optional arg info params executor-type)
    "Execute the current source code block and return the result.
Insert the results of execution into the buffer.  Source code
execution and the collection and formatting of results can be
controlled through a variety of header arguments.

With prefix argument ARG, force re-execution even if an existing
result cached in the buffer would otherwise have been returned.

Optionally supply a value for INFO in the form returned by
`org-babel-get-src-block-info'.

Optionally supply a value for PARAMS which will be merged with
the header arguments specified at the front of the source code
block.

EXECUTOR-TYPE is the type of the org element responsible for the
execution of the source block.  If not provided then informed
guess will be made."
    (interactive)
    (let* ((org-babel-current-src-block-location
            (or org-babel-current-src-block-location
                (nth 5 info)
                (org-babel-where-is-src-block-head)))
           (info (if info (copy-tree info) (org-babel-get-src-block-info)))
           (executor-type
            (or executor-type
                ;; If `executor-type' is unset, then we will make an
                ;; informed guess.
                (pcase (char-after org-babel-current-src-block-location)
                  (?s 'inline-src-block)
                  (?c 'inline-babel-call)
                  (?# (pcase (char-after (+ 2 org-babel-current-src-block-location))
                        (?b 'src-block)
                        (?c 'call-block)
                        (_ 'unknown)))
                  (_ 'unknown)))))
      ;; Merge PARAMS with INFO before considering source block
      ;; evaluation since both could disagree.
      (cl-callf org-babel-merge-params (nth 2 info) params)
      (when (org-babel-check-evaluate info)
        (cl-callf org-babel-process-params (nth 2 info))
        (let* ((params (nth 2 info))
               (cache (let ((c (cdr (assq :cache params))))
                        (and (not arg) c (string= "yes" c))))
               (new-hash (and cache (org-babel-sha1-hash info :eval)))
               (old-hash (and cache (org-babel-current-result-hash)))
               (current-cache (and new-hash (equal new-hash old-hash))))
          (cond
           (current-cache
            (save-excursion		;Return cached result.
              (goto-char (org-babel-where-is-src-block-result nil info))
              (forward-line)
              (skip-chars-forward " \t")
              (let ((result (org-babel-read-result)))
                (message (format "Cached: %s"
                                 (replace-regexp-in-string "%" "%%" (format "%S" result))))
                result)))
           ((org-babel-confirm-evaluate info)
            (let* ((lang (nth 0 info))
                   (result-params (cdr (assq :result-params params)))
                   (body (org-babel--expand-body info))
                   (dir (cdr (assq :dir params)))
                   (mkdirp (cdr (assq :mkdirp params)))
                   (default-directory
                     (cond
                      ((not dir) default-directory)
                      ((member mkdirp '("no" "nil" nil))
                       (file-name-as-directory (expand-file-name dir)))
                      (t
                       (let ((d (file-name-as-directory (expand-file-name dir))))
                         (make-directory d 'parents)
                         d))))
                   (cmd (intern (concat "org-babel-execute:" lang)))
                   result exec-start-time)
              (unless (fboundp cmd)
                (error "No org-babel-execute function for %s!" lang))
              (message "Executing %s %s %s..."
                       (capitalize lang)
                       (pcase executor-type
                         ('src-block "code block")
                         ('inline-src-block "inline code block")
                         ('babel-call "call")
                         ('inline-babel-call "inline call")
                         (e (symbol-name e)))
                       (let ((name (nth 4 info)))
                         (if name
                             (format "(%s)" name)
                           (format "at position %S" (nth 5 info)))))
              (setq exec-start-time (current-time)
                    result
                    (let ((r (funcall cmd body params)))
                      (if (and (eq (cdr (assq :result-type params)) 'value)
                               (or (member "vector" result-params)
                                   (member "table" result-params))
                               (not (listp r)))
                          (list (list r))
                        r)))
              (let ((file (and (member "file" result-params)
                               (cdr (assq :file params)))))
                ;; If non-empty result and :file then write to :file.
                (when file
                  ;; If `:results' are special types like `link' or
                  ;; `graphics', don't write result to `:file'.  Only
                  ;; insert a link to `:file'.
                  (when (and result
                             (not (or (member "link" result-params)
                                      (member "graphics" result-params))))
                    (with-temp-file file
                      (insert (org-babel-format-result
                               result
                               (cdr (assq :sep params)))))
                    ;; Set file permissions if header argument
                    ;; `:file-mode' is provided.
                    (when (assq :file-mode params)
                      (set-file-modes file (cdr (assq :file-mode params)))))
                  (setq result file))
                ;; Possibly perform post process provided its
                ;; appropriate.  Dynamically bind "*this*" to the
                ;; actual results of the block.
                (let ((post (cdr (assq :post params))))
                  (when post
                    (let ((*this* (if (not file) result
                                    (org-babel-result-to-file
                                     file
                                     (org-babel--file-desc params result)
                                     'attachment))))
                      (setq result (org-babel-ref-resolve post))
                      (when file
                        (setq result-params (remove "file" result-params))))))
                (if (member "none" result-params)
                    (message "result silenced")
                  (org-babel-insert-result
                   result result-params info new-hash lang
                   (time-subtract (current-time) exec-start-time))))
              (run-hooks 'org-babel-after-execute-hook)
              result)))))))



  (use-package! org-download
    :defer-incrementally t)

  (setq org-mode-hook nil)
  (add-hook 'org-mode-hook #'evil-org-mode)
  (add-hook 'org-mode-hook #'+org-enable-auto-reformat-tables-h)

  (org-crypt-use-before-save-magic)

  (setq org-tags-exclude-from-inheritance (list "crypt")
        org-crypt-key nil
        org-ditaa-jar-path "~/.doom.d/ditaa.jar"
        org-plantuml-jar-path "~/.doom.d/plantuml.jar"
        org-crypt-disable-auto-save t
        org-export-in-background nil
        org-confirm-babel-evaluate nil
        org-display-remote-inline-images 'download
        ;; Latex Previews
        org-preview-latex-default-process 'dvisvgm
        org-format-latex-options (plist-put org-format-latex-options :scale 1.5)

        ;; html export
        ;; (setq org-html-htmlize-output-type 'inline-css) ;; default
        ;; (setq org-html-htmlize-font-prefix "") ;; default
        org-html-htmlize-output-type 'css
        org-html-htmlize-font-prefix "org-"
        user-full-name "A.P."


        ;; Toggle Displays
        org-startup-folded 'f

        ;; Turn on inline images by default
        org-startup-with-inline-images t
        ;; (org-display-inline-images t t)

        ;; Allow M-Ret to split list items
        org-M-RET-may-split-line t
        org-log-done 'time)

  (require 'org-web-tools)

  (defun www-get-page-title (url)
    "Gets the title of a webpage at URL"
    (org-web-tools--html-title (org-web-tools--get-url url)))

  (defun org-capture-url (url)
    (insert (org-insert-link nil url (www-get-page-title url))))

  (defun org-capture-url-from-clipboard (url)
    "Capture a URL from clipboard and paste it as an org link"
    (interactive)
    (org-capture-url (current-kill 0)))

  (define-key org-mode-map
    (kbd "M-RET")
    (lambda ()
      (interactive)
      (org-meta-return t)))

  (defun org-archive-done ()
    (interactive)
    (org-archive-all-done))

  (defun md-shorten-indico-url ()
    (org-shorten-indico-link)
    (org-link->markdown))

  (defun org-link-get ()
    "Extract URL from org-mode link and add return it"
    (let* ((link (org-element-lineage (org-element-context) '(link) t))
           (url (org-element-property :path link))) url))

  (defun org-edit-image ()
    ""
    (interactive)
    (let ((link (org-link-get)))
      (when link
        (start-process "*gimp*" nil "setsid" "gimp" link))))

  (defun org-shorten-url-by-title ()
    ""
    (interactive)
    (let* ((org-link-pos (org-in-regexp org-link-any-re))
           (beg (car org-link-pos))
           (end (cdr org-link-pos))
           (url (buffer-substring-no-properties beg end))
           (desc (www-get-page-title url)))
      (when desc
        (delete-region beg end)
        (org-insert-link nil url desc))))

  (defun md-shorten-url-by-title ()
    (interactive)
    (org-shorten-url-by-title)
    (org-link->markdown))

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

  ;; http://mbork.pl/2021-05-02_Org-mode_to_Markdown_via_the_clipboard
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

  (defun pandoc-buffer-to-org ()
    (interactive)
    (shell-command
     (concat "pandoc " (shell-quote-argument (buffer-file-name)) " -o "
             (shell-quote-argument (file-name-sans-extension (buffer-file-name))) ".org")))

  (defun org-latex-preview-all ()
    (interactive)
    (org-latex-preview '(16)))

  (defun org-latex-preview-clear ()
    (interactive)
    (org-latex-preview '(64)))

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


  ;; Latex Export
  ;;  Latex Export Class
  (with-eval-after-load 'ox-latex
    (add-to-list
     'org-latex-classes
     `("article"
       ,(concat
         "\\documentclass[11pt]{article}\n"
         "\\usepackage[utf8]{inputenc}\n"
         "\\usepackage[T1]{fontenc}\n"
         "\\usepackage{fixltx2e}\n"
         "\\usepackage{fullpage}\n"
         "\\usepackage{graphicx}\n"
         "\\usepackage{longtable}\n"
         "\\usepackage{float}\n"
         "\\usepackage{wrapfig}\n"
         "\\usepackage{rotating}\n"
         "\\usepackage[normalem]{ulem}\n"
         "\\usepackage{amsmath}\n"
         "\\usepackage{textcomp}\n"
         "\\usepackage{marvosym}\n"
         "\\usepackage{wasysym}\n"
         "\\usepackage{amssymb}\n"
         "\\usepackage{hyperref}\n"
         "%\\usepackage{mathpazo}\n"
         "\\renewcommand{\\familydefault}{\\sfdefault}\n"
         "\\usepackage{color}\n"
         "\\usepackage{enumerate}\n"
         "\\definecolor{bg}{rgb}{0.95,0.95,0.95}\n"
         "\\tolerance=1000\n"
         "[NO-DEFAULT-PACKAGES]\n"
         "[PACKAGES]\n"
         "[EXTRA]\n"
         "\\linespread{1.1}\n"
         "\\hypersetup{pdfborder=0 0 0}")

       ("\\section{%s}"       . "\\section*{%s}")
       ("\\subsection{%s}"    . "\\subsection*{%s}")
       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
       ("\\paragraph{%s}"     . "\\paragraph*{%s}")
       ("\\subparagraph{%s}"  . "\\subparagraph*{%s}"))))

  ;; Appearance
  ;;------------------------------------------------------------------------------

  (add-hook 'org-mode-hook
            (lambda ()
              (progn
                (make-variable-buffer-local 'scroll-margin)
                (setq scroll-margin 1))))

  ;; normal evil-org-end-of-line is broken
  ;; https://github.com/Somelauw/evil-org-mode/issues/50
  ;; just use the regular evil mode.. there doesn't seem to be any downside
  (add-hook 'org-mode-hook
            (lambda () (define-key evil-visual-state-map "$" #'evil-end-of-line)))

  (add-hook 'org-mode-hook
            (lambda () (define-key evil-normal-state-map "zs" #'org-toggle-link-display)))

  ;; (add-to-list 'load-path "~/Sync/org")

  ;;(mapc 'load
  ;;      '("org-sync" "org-sync-bb" "org-sync-github" "org-sync-gitlab"))

  (defun sort-all-org-entries ()
    (interactive)
    (let ((fun #'(lambda nil
                   (condition-case nil
                       (org-sort-entries nil ?o)
                     (user-error t)))))
      (org-map-entries fun)))


  ;;  Turn on Bullets Mode
  ;;(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

  ;; Org mode plain list bullets
  ;; (font-lock-add-keywords
  ;;  'org-mode
  ;;  '(("^[[:space:]]*\\(-\\) "
  ;;     0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "  ⚫ ")))))

  (add-hook 'evil-org-agenda-mode-hook
            'evil-org-agenda-set-keys)


  ;; only turn this on in org mode!!
  ;; (when (boundp '+org-pretty-mode)
  ;;   (+org-pretty-mode t))

  ;; (setq org-hide-emphasis-markers nil)
  (setq org-link-file-path-type 'relative
        org-agenda-files (list "~/Sync/notes")
        org-id-locations-file "~/Sync/notes/.org-id-locations"
        org-export-with-sub-superscripts nil
        org-directory "~/Sync/notes"
        org-attach-id-dir "./images/"
        org-default-notes-file (concat org-directory "/todo.org")
        +org-capture-todo-file "~/Sync/notes/todo.org"

        ;; https://github.com/sk8ingdom/.emacs.d/blob/master/org-mode-config/org-capture-templates.el
        ;; https://cestlaz.github.io/posts/using-emacs-26-gcal/
        org-capture-templates
        '(;; ("a" "Appointment" entry (file  "~/Sync/org/gcal-peck.org" ) "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")

          ;; https://orgmode.org/manual/Template-elements.html

          ("t" "TODO" entry
           (file+headline +org-capture-todo-file "To do") "** TODO %?" :prepend t)

          ("w" "Web site" entry
           (file+headline "~/Sync/notes/inbox.org" "Captured")
           "* %a :website:\n\n%U %?\n\n%:initial" :unnarrowed t)

          ("a" "capture-clipboard" entry
           ;; %i == body
           ;; %u == date
           (file+headline "~/Sync/notes/inbox.org" "To do") "** TODO %:link %i"
           :immediate-finish t
           :unnarrowed t)

          ("n" "Note" entry
           (file+headline (concat org-directory  "/notes-to-file.org") "Notes") "* %?" :prepend t)

          ("s" "Shopping" item
           (file+headline +org-capture-todo-file "Shopping") "- [ ] %?" :prepend t)))

  ;;
  (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))
  (add-to-list 'org-file-apps '("\\.odt\\'" . "xdg-open %s"))


  ;; Org Image Attach
  ;;------------------------------------------------------------------------------

  (setq org-attach-id-dir "./images/screenshots")

  (defun org-remove-link-and-trash-linked-file ()
    "Remove `org-mode' link at point and trash linked file."
    (interactive)
    (let* ((link (org-element-context))
           (path (org-element-property :path link)))
      (move-file-to-trash path)
      (delete-region (org-element-property :begin link)
                     (org-element-property :end link))))

  (defun ap/inline-org-inbox-link ()
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

  )

(after! org-download

  (map! :leader
        :prefix "ma"
        :desc "Download Screenshot" "c" #'org-download-screenshot
        :desc "Download Clipboard" "p" #'org-download-clipboard
        :desc "Download Yank" "P" #'org-download-yank
        :desc "Edit Image" "e" #'org-download-edit
        :desc "Delete Image" "d" #'org-download-delete
        :desc "Move Image" "m" #'org-download-rename)

  ;; Drag-and-drop to `dired`
  (add-hook 'dired-mode-hook 'org-download-enable)

  ;; (defun org-download-named-screenshot (fname)
  ;;   (interactive "FEnter Filename:")
  ;;   (make-directory (file-name-directory fname) t)
  ;;   (if (functionp org-download-screenshot-method)
  ;;       (funcall org-download-screenshot-method fname)
  ;;     (shell-command-to-string
  ;;      (format org-download-screenshot-method fname)))
  ;;   (org-download-image fname))


  (setq org-download-method            'directory
        ;;org-download-screenshot-method 'nil
        ;;org-download-screenshot-method "spectacle -b -r -o %s"
        org-download-screenshot-method "xfce4-screenshooter -r -s %s"
        org-download-image-dir         "./images/screenshots"
        org-download-heading-lvl       0
        org-download-link-format       (concat  "[[file:" org-download-image-dir "/%s]]\n")
        org-download-annotate-function (lambda (text) "")
        org-download-image-org-width   200
        ;; org-download-image-dir "./images/screenshots"
        org-download-dir               "download/"))

(after! org-attach-screenshot
  (setq org-attach-screenshot-command-line "xfce4-screenshooter -r -s %f"))

;;; Org roam
;;------------------------------------------------------------------------------

(after! org-roam

  (setq org-roam-db-autosync-mode t
        org-roam-directory (file-truename "~/Sync/notes/")
        org-roam-graph-extra-config '(("rankdir" . "RL"))
        ;; (setq org-roam-graph-edge-extra-config '(("dir" . "back")))
        org-roam-graph-link-hidden-types '("file" "http" "https"))

  ;; add a space before inserting a node for lists etc so it does
  ;; not come out as -[link] but rather as - [link]
  (advice-add 'org-roam-node-insert :before (lambda () (insert " ")))

  (map! :leader
        :prefix "y"
        :desc "Org Link Copy"       "y" #'org-link-copy)

  ;; (map! :leader
  ;;       :prefix "n"
  ;;       :desc "Org-Roam-Insert"     "i" #'org-roam-node-insert
  ;;       :desc "Org-Roam-Find"       "/" #'org-roam-find-file
  ;;       :desc "Org-Roam-Buffer"     "r" #'org-roam
  ;;       :desc "Org-Roam-Show-Graph" "g" #'org-roam-graph)

  (setq org-roam-db-location "~/.org-roam.db")
  (setq org-roam-link-title-format "Org:%s"))

;;------------------------------------------------------------------------------
;; Functions to Convert to/from org / markdown links
;;------------------------------------------------------------------------------

(after! org
  ;; taken from:
  ;; https://github.com/agzam/.doom.d/blob/main/modules/custom/org/autoload/custom.el#L181-L214
  (defun org-link-parse (link)
    ;; borrowed and adopted from:
    ;; github.com/xuchunyang/emacs.d/blob/5f4f873cf7a671a36f686f3d1346fd7c5a5462bc/lisp/chunyang-misc.el#L488-L526
    (if (string-match
         (rx "[[" (group (0+ anything)) "][" (group (0+ anything)) "]]")
         link)
        (list (match-string 1 link)
              (match-string 2 link))
      (error "Cannot parse %s as Org link" link)))

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

  (defun markdown-link->org ()
    (interactive)
    (when (markdown-link-p)
      (let* ((l (markdown-link-at-pos (point)))
             (desc (nth 2 l))
             (url (nth 3 l)))
        (markdown-kill-thing-at-point)
        (org-insert-link nil url desc))))

  ;; from
  ;; (https://github.com/SqrtMinusOne/dotfiles/blob/4b176a5bb1a5e20a7fdd7398b74df79701267a7e/.emacs.d/init.el)
  (defun org-link-copy (&optional arg)
    "Extract URL from org-mode link and add it to kill ring."
    (interactive "P")
    (let* ((link (org-element-lineage (org-element-context) '(link) t))
           (type (org-element-property :type link))
           (url (org-element-property :path link))
           (url (concat type ":" url)))
      (kill-new url)
      (message (concat "Copied URL: " url)))))


;; (eval-when-compile
;;   (require 'easy-mmode)
;;   (require 'ox))

;; https://github.com/alphapapa/unpackaged.el#download-and-attach-remote-files
(use-package! ox
  :defer 4.0
  :config

  (define-minor-mode unpackaged/org-export-html-with-useful-ids-mode
    "Attempt to export Org as HTML with useful link IDs.
Instead of random IDs like \"#orga1b2c3\", use heading titles,
made unique when necessary."
    :global t
    (if unpackaged/org-export-html-with-useful-ids-mode
        (advice-add #'org-export-get-reference :override #'unpackaged/org-export-get-reference)
      (advice-remove #'org-export-get-reference #'unpackaged/org-export-get-reference)))

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
        ref))))


(defun org-get-linked-files (&optional buffer)
  "Get all of the 'file' type links in a buffer.
Current buffer is assumed unless specified by BUFFER"
  (with-current-buffer
      (or buffer (current-buffer))
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (when (string= (org-element-property :type link) "file")
          (org-element-property :path link))))))

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

(defun org-global-prop-value (key)
  "Get global org property KEY of current buffer."
  (org-element-property :value (car (org-global-props key))))

(setq org-default-publish-dest "nfs:/home/public")

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

  (let* ((dest (or (org-global-prop-value "DEST")
                   org-default-publish-dest))
         (args

          (cl-concatenate 'list
                          `("-avz" "--relative"
                            ,(concat (file-name-base (buffer-file-name)) ".html"))
                          (org-get-linked-files)
                          (list dest))))

    ;; (message (mapconcat #'identity args " "))

    (set-process-sentinel

     ;; copy
     (apply #'start-process "*copy-to-dest*" nil "rsync" args)

     ;; cleanup
     (lambda (_ event)
       (when (string= event "finished\n")
         (message "rsync finished, cleaning up...")
         (delete-file (concat (file-name-base (buffer-file-name)) ".html")))))))
