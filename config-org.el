;; -*- lexical-binding: t; -*-

(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance '("crypt"))
(setq org-crypt-key nil)

;;; Org Mode
;;------------------------------------------------------------------------------

(after! emojify-mode
  (setq global-emojify-mode t))

(after! org

  (setq org-export-in-background t)
  (define-key org-mode-map (kbd "M-RET") (lambda () (interactive) (org-meta-return t)))
  (defun org-insert-monthly-timesheet ()
    "Insert a new timesheet for the current month"
    (interactive)
    (let* ((time (current-time))
           (month (format-time-string "%B" time))
           (mm (format-time-string "%m" time))
           (year (format-time-string "%Y" time)))

      (insert
       (concat
        ;; (s-lex-format)
        (format "*** %s 2022\n" month)
        ":PROPERTIES:\n"
        ":VISIBILITY: showall\n"
        ":END:\n"
        "#+ATTR_HTML: :border 2 :frame none\n"
        "\n"
        (format  "#+TBLNAME: %s-%s\n" year mm)
        "|---+---+----------+---------+--------------------+-----+-------|\n"
        "|   | D |     Time | Project | Task               | Day | Hours |\n"
        "|---+---+----------+---------+--------------------+-----+-------|\n"
        "| # |   |          |         |                    |     |       |\n"
        "|---+---+----------+---------+--------------------+-----+-------|\n"
        (format  "#+TBLFM: $6='(org-sbe ymd_to_weekday (k $$2) (path $\"%s %s\"))::$7='(org-sbe subtract (a $$3))\n" month year)
        "\n"
        (format  "#+begin_src emacs-lisp :exports results :results output :var data=%s-%s\n" year mm)
        "(plot-monthly-work-chart data)\n"
        "#+end_src\n"))))

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


  (defun org-archive-done ()
    (interactive)
    (org-archive-all-done))

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

  (setq org-display-remote-inline-images 'download)

  (load "~/.doom.d/lisp/scimax-org-return.el")

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

  ;; html export
  ;; (setq org-html-htmlize-output-type 'inline-css) ;; default
  (setq org-html-htmlize-output-type 'css)
  ;; (setq org-html-htmlize-font-prefix "") ;; default
  (setq org-html-htmlize-font-prefix "org-")

  ;; Latex Export
  (setq org-return-follows-links t)

  (setq user-full-name "A.P.")

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

  ;; Latex Previews
  (setq org-preview-latex-default-process 'dvisvgm)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

  ;; Toggle Displays
  (setq org-startup-folded 'f)

  ;; Turn on inline images by default
  (setq org-startup-with-inline-images t)
  ;; (org-display-inline-images t t)

  ;; Allow M-Ret to split list items
  (setq org-M-RET-may-split-line t)

  ;;  Turn on Bullets Mode
  ;;(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

  ;; Org mode plain list bullets
  ;; (font-lock-add-keywords
  ;;  'org-mode
  ;;  '(("^[[:space:]]*\\(-\\) "
  ;;     0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "  ⚫ ")))))

  (add-hook 'evil-org-agenda-mode-hook
            'evil-org-agenda-set-keys)

  (setq org-log-done 'time)

  (+org-pretty-mode t)
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
                     (org-element-property :end link))))) ;; after! evil

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

  (map! :leader
        :prefix "n"
        :desc "Org-Roam-Insert"     "i" #'org-roam-node-insert
        :desc "Org-Roam-Find"       "/" #'org-roam-find-file
        :desc "Org-Roam-Buffer"     "r" #'org-roam
        :desc "Org-Roam-Show-Graph" "g" #'org-roam-graph)

  (setq org-roam-db-location "~/.org-roam.db")
  (setq org-roam-link-title-format "Org:%s"))

;;------------------------------------------------------------------------------
;; Functions to Convert to/from org / markdown links
;;------------------------------------------------------------------------------

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
  (let* ((ctx (org-in-regexp org-any-link-re))
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
    (message (concat "Copied URL: " url))))


(eval-when-compile
  (require 'easy-mmode)
  (require 'ox))

;; https://github.com/alphapapa/unpackaged.el#download-and-attach-remote-files
(use-package ox
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
    "Like `org-export-get-reference', except uses heading titles instead of random numbers."
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
    (cl-macrolet ((inc-suffixf (place)
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
