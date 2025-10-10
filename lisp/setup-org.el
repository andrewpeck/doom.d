;; -*- lexical-binding: t; -*-

;;------------------------------------------------------------------------------
;; Appearance
;;------------------------------------------------------------------------------

(use-package! poporg
  :config
  (map! :leader :prefix "e" :desc "Poporg Edit Comment"  "c"  #'poporg-dwim))

(use-package! org
  :init

  (add-hook 'org-mode-hook 'org-appear-mode)
  (add-hook 'org-mode-hook (defun hook/org-set-user-name () (setq-local user-full-name "A.P.")))
  (add-hook 'org-mode-hook (defun hook/org-disable-diff-hl-mode () (diff-hl-mode -1))) ;; diff-hl just makes line noise for org mode
  (add-hook 'org-mode-hook (defun hook/org-set-scroll-margin () (setq-local scroll-margin 1)))
  ;; (add-hook 'org-mode-hook (defun hook/org-auto-fill-mode () (auto-fill-mode t)))
  (add-hook 'org-mode-hook (defun hook/org-enable-evil-org-mode () (evil-org-mode)))
  (add-hook 'org-mode-hook (defun hook/org-enable-word-wrap-mode () (+word-wrap-mode)))
  ;; (add-hook 'org-mode-hook (defun hook/org-enable-auto-format () (+org-enable-auto-reformat-tables-h)))
  (add-hook 'org-mode-hook (defun hook/org-latex-text-scale-mode () (add-hook 'text-scale-mode-hook #'my/resize-org-latex-overlays nil t)))
  (add-hook 'org-mode-hook (defun hook/org-crypt-before-save-magic () (add-hook 'before-save-hook #'org-encrypt-entries nil t)))

  :config

  (require 'org-tempo)

  ;;------------------------------------------------------------------------------
  ;; keybindings
  ;;------------------------------------------------------------------------------

  (map! :leader             :desc "Org Capture TODO"     "T" (lambda () (interactive) (org-capture nil "t")))
  (map! :leader :prefix "t" :desc "Open org TODOs"       "t" #'open-todo)
  (map! :leader :prefix "y" :desc "Org Link Copy"        "y"  #'org-link-copy)
  (map! :localleader :map org-mode-map :desc "Latexify"    "lp" #'org-latex-preview-all :map org-mode-map)
  (map! :localleader :map org-mode-map :desc "De-latexify" "lP" #'org-latex-preview-clear :map org-mode-map)

  (map! :map org-mode-map "RET" #'scimax/org-return)

  (map! :localleader
        :map org-mode-map
        :prefix "a" ;; Actions
        :after org-download
        :desc "Download Screenshot" "c" #'org-download-screenshot
        :desc "Download Clipboard" "p" #'org-download-clipboard
        :desc "Download Yank" "P" #'org-download-yank
        :desc "Edit Image" "e" #'org-download-edit
        :desc "Delete Image" "d" #'org-download-delete
        :desc "Move Image" "m" #'org-download-rename-at-point)

  (map! :map org-mode-map
        "C-c x" #'org-toggle-checkbox-presence)

  ;;------------------------------------------------------------------------------
  ;; config
  ;;------------------------------------------------------------------------------

  (defun org-fill-paragraph-t ()
    (interactive)
    (org-fill-paragraph t))

  (setq org-todo-keywords
        '((sequence "MEET" "MET")
          (sequence "NOTE" "NOTED")
          (sequence "TODO(t)" "|" "DONE(d)" "KILL(k)")
          (sequence  "LOOP(r)" "PROJ(p)" "STRT(s)" "WAIT(w)" "HOLD(h)" "IDEA(i)" "|" "DONE(d)" "KILL(k)")
          (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
          (sequence "|" "OKAY(o)" "YES(y)" "NO(n)")))

  (setq org-image-max-width 1280
        org-image-align  'left
        org-image-actual-width t)

  (defun +org-http-or-https-image-data-fn (ov link _elem type)
    "Interpret LINK as an URL to an image file."
    (when (and (image-supported-file-p link)
               (not (eq org-display-remote-inline-images 'skip)))
      (if-let (buf (url-retrieve-synchronously (concat type ":" link)))
          (with-current-buffer buf
            (goto-char (point-min))
            (re-search-forward "\r?\n\r?\n" nil t)
            (overlay-put ov 'display (create-image (buffer-substring-no-properties (point) (point-max)) nil t)))
        (message "Download of image \"%s\" failed" link)
        nil)))

  (defun +org-http-or-https-image-data-fn (ov link _elem type)
    "Interpret LINK as an URL to an image file."
    (when (and (image-supported-file-p link)
               (not (eq org-display-remote-inline-images 'skip)))
      (let ((dest (concat temporary-file-directory
                          (car (last (split-string link "/" t))))))
        (when (not (file-exists-p dest))
          (url-copy-file (concat type ":" link) dest t))
        (org-link-preview-file ov dest link))))

  (defun +org-http-image-data-fn (ov link elem)
    "Interpret LINK as an URL to an image file."
    (+org-http-or-https-image-data-fn ov link elem "http"))

  (defun +org-https-image-data-fn (ov link elem)
    "Interpret LINK as an URL to an image file."
    (+org-http-or-https-image-data-fn ov link elem "https"))

  (defun +org-inline-image-data-fn (ov link _elem)
    "Interpret LINK as base64-encoded image data."
    (overlay-put ov 'display (create-image link nil t)))

  (org-link-set-parameters "http"  :preview #'+org-http-image-data-fn)
  (org-link-set-parameters "https" :preview #'+org-https-image-data-fn)
  (org-link-set-parameters "img"   :preview #'+org-inline-image-data-fn)

  (org-link-set-parameters
   "docview" :preview #'org-link-docview-preview)

  (add-hook 'org-mode-hook #'org-excalidraw-initialize)

  ;; org should open html with a browser
  ;; don't know why this isn't the default
  (add-to-list 'org-file-apps '("\\.x?html?\\'" . "xdg-open %s"))

  (setq org-appear-autoemphasis t
        org-appear-inside-latex t
        org-appear-autolinks t
        org-appear-delay 0.1)

  (require 'evil-org)

  (setq ob-mermaid-cli-path "aa-exec --profile=chrome mmdc")

  ;;------------------------------------------------------------------------------
  ;; General
  ;;------------------------------------------------------------------------------

  (after! ob-ditaa
    (setopt org-ditaa-jar-path "~/.doom.d/ditaa.jar"))

  (setq org-tags-exclude-from-inheritance (list "crypt")
        org-indent-indentation-per-level 2
        org-startup-numerated nil
        org-crypt-key nil
        org-crypt-disable-auto-save t
        org-export-in-background nil
        org-confirm-babel-evaluate nil
        org-display-remote-inline-images 'download

        ;; visual-fill-column-mode 1

        ;; org-plantuml-jar-path "~/.doom.d/plantuml.jar"

        ;; Latex Previews
        org-preview-latex-default-process 'dvisvgm
        org-format-latex-options (plist-put org-format-latex-options :scale 1.5)

        ;; html export
        ;; (setq org-html-htmlize-output-type 'inline-css) ;; default
        ;; (setq org-html-htmlize-font-prefix "") ;; default
        org-html-htmlize-output-type 'css
        org-html-htmlize-font-prefix "org-"

        ;; Toggle Displays
        org-startup-folded 'f

        ;; Turn on inline images by default
        org-startup-with-inline-images t
        ;; (org-display-inline-images t t)

        org-attach-id-dir "./images/screenshots"

        ;; Allow M-Ret to split list items
        org-M-RET-may-split-line t
        org-log-done 'time

        ;; (setq org-hide-emphasis-markers nil)
        org-link-file-path-type 'relative
        org-id-locations-file "~/notes/.org-id-locations"
        org-export-with-sub-superscripts nil
        org-directory "~/notes"
        org-agenda-start-day "0d"
        org-agenda-span 2
        org-attach-id-dir "./images/"
        org-agenda-files (list "~/todo")
        org-default-notes-file "~/todo/notes.org"
        +org-capture-todo-file "~/todo/todo.org"
        +org-capture-meetings-file "~/todo/meetings.org"

        ;; https://github.com/sk8ingdom/.emacs.d/blob/master/org-mode-config/org-capture-templates.el
        ;; https://cestlaz.github.io/posts/using-emacs-26-gcal/
        org-capture-templates
        '(;; ("a" "Appointment" entry (file  "~/Sync/org/gcal-peck.org" ) "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")

          ;; https://orgmode.org/manual/Template-elements.html

          ("t" "TODO" entry
           (file+headline +org-capture-todo-file "To do")
           "** TODO %?\n"  :prepend t)

          ("d" "DEADLINE" entry
           (file+headline +org-capture-todo-file "To do")
           "** TODO %?\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \".\"))"  :prepend t)

          ("u" "Url" entry
           (file+headline "~/notes/inbox.org" "Captured")
           "** %a :website:\n\n%U %?\n\n%:initial" :unnarrowed t)

          ("m" "Meeting" entry
           (file+headline "~/work/psiq/meetings.org" "Meetings")
           "** %t %? [/]\n- [ ] " :prepend t)

          ("l" "Log" entry
           (file+headline "~/work/todo/log.org" "Log")
           "** %t %?\n- last week\n    - \n- this week\n    - " :prepend t)

          ("w" "Waiting" entry
           (file+headline +org-capture-todo-file "To do")
           "** WAIT %?\n %U" :prepend t)

          ("i" "Idea" entry
           (file+headline +org-capture-todo-file "Ideas")
           "** IDEA %?\n%U")

          ("n" "Note" entry
           (file+headline org-default-notes-file "Notes")
           "** %?\n%U")

          ("a" "capture-clipboard" entry
           ;; %i == body
           ;; %u == date
           (file+headline "~/notes/inbox.org" "To do") "** TODO %:link %i"
           :immediate-finish t
           :unnarrowed t)

          ("s" "Shopping" item
           (file+headline +org-capture-todo-file "Shopping") "- [ ] %?" :prepend t)))

  ;; Applications for opening file:path items in a document
  (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))
  (add-to-list 'org-file-apps '("\\.odt\\'" . "xdg-open %s"))


  ;;------------------------------------------------------------------------------
  ;; https://www.reddit.com/r/orgmode/comments/165zeuu/delighted_by_org_svg_preview/
  ;;------------------------------------------------------------------------------

  (defun my/resize-org-latex-overlays ()
    (cl-loop for o in (car (overlay-lists))
             if (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay)
             do (plist-put (cdr (overlay-get o 'display))
                           :scale (expt text-scale-mode-step
                                        text-scale-mode-amount))))

  ;;------------------------------------------------------------------------------
  ;; Helpers
  ;;------------------------------------------------------------------------------

  (defun org-procrastinate-all ()

    "Carry forward uncompleted tasks.
Updates overdue tasks to be due today."

    (interactive)
    (org-carry-forward-uncompleted-tasks t))

  (defun org-procrastinate ()
    (interactive)
    (org-deadline nil "+1"))

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

  (defun pandoc-buffer-to-org ()
    (interactive)
    (shell-command
     (concat "pandoc " (shell-quote-argument (buffer-file-name)) " -o "
             (shell-quote-argument (file-name-sans-extension (buffer-file-name))) ".org")))


  ;;------------------------------------------------------------------------------
  ;; Appearance
  ;;------------------------------------------------------------------------------

  (setq org-html-xml-declaration
        '(("html" . "")
          ("php" . "<?php echo \"<?xml version=\\\"1.0\\\" encoding=\\\"%s\\\" ?>\"; ?>")))

  ;; Tag colors
  (setq org-tag-faces
        '(("etl"       . (:foreground "gray"))
          ("atlas"     . (:foreground "gray"))
          ("l0mdt"     . (:foreground "gray"))

          ("BU"        . (:foreground "red2"       :weight bold))
          ("bu"        . (:foreground "red2"       :weight bold))

          ("csc"       . (:foreground "steelblue"  :weight bold))
          ("gaps"      . (:foreground "steelblue"  :weight bold))
          ("hog"       . (:foreground "steelblue"  :weight bold))
          ("gem"       . (:foreground "steelblue"  :weight bold))
          ("me0"       . (:foreground "steelblue"  :weight bold))

          ("move"      . (:background "#666" :foreground "#eee"          :weight bold))
          ("family"    . (:foreground "red4"          :weight bold))
          ("home"      . (:foreground "lightorange"   :weight bold))
          ("meeting"   . (:foreground "gray" :slant italic))
          ("CRITICAL"  . (:background "red3" :foreground "#fff" :weight bold))))

  ;; change DEADLINE to a short symbol to reduce line noise
  (font-lock-add-keywords
   'org-mode
   '(("^\\(DEADLINE:\\)"
      (0 (prog1 ()
           (compose-region (match-beginning 1) (match-end 1) ""))))))

  (setq org-agenda-prefix-format
        '((agenda  . " %t")
          (timeline  . "  % s")
          (todo  . " %i")
          (tags  . " %i %-12:c")
          (search . " %i %-12:c")))

  (custom-declare-face
   '+org-todo-idea
   `((t :weight bold :foreground "#94e2d5")) "IDEA todo keyword")

  (custom-declare-face
   '+org-todo-meet
   `((t :weight bold :foreground "#cdf"))    "MEET todo keyword")

  (custom-declare-face
   '+org-todo-note
   `((t :weight bold :foreground "#288"))    "NOTE todo keyword")

  (setq org-todo-keyword-faces
        '(("[-]"  . +org-todo-active)
          ("STRT" . +org-todo-active)
          ("[?]"  . +org-todo-onhold)
          ("WAIT" . +org-todo-onhold)
          ("HOLD" . +org-todo-onhold)
          ("MEET" . +org-todo-meet)
          ("NOTE" . +org-todo-note)
          ("PROJ" . +org-todo-project)
          ("NO"   . +org-todo-cancel)
          ("IDEA" . +org-todo-idea)
          ("KILL" . +org-todo-cancel)))

  (setq org-todo-keywords
        '((sequence "MEET" "MET")
          (sequence "NOTE" "NOTED")
          (sequence "TODO(t)" "PROJ(p)" "LOOP(r)" "STRT(s)" "WAIT(w)" "HOLD(h)" "IDEA(i)" "|" "DONE(d)" "KILL(k)")
          (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
          (sequence "|" "OKAY(o)" "YES(y)" "NO(n)")))

  ;; (setq org-startup-indented nil)
  ;; (setq-default org-indent-mode nil)
  (with-eval-after-load 'org-modern-mode
    (progn
      (setq org-modern-checkbox
            '((?X  . "✓")
              (?-  . "␣")
              (?\s . "☐")))
      (setq org-modern-table nil)
      (global-org-modern-mode)))



  (defun org-archive-done ()
    "Interactive wrapper for org-archive-all-done"
    (interactive)
    (require 'org-archive)
    (org-archive-all-done))


  (defun org-link-get ()
    "Extract URL from org-mode link and add return it"
    (let* ((link (org-element-lineage (org-element-context) '(link) t))
           (url (org-element-property :path link))) url))

  (defvar org-drawio-template "~/.doom.d/template.drawio.svg")

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

  (defun my/edit-image-helper (helper)
    (let ((link (pcase major-mode
                  ('org-mode (org-link-get))
                  ('image-mode (buffer-file-name))
                  (_ nil))))
      (when link (call-process helper nil 0 nil link))))

  (defun my/edit-gimp ()
    "Open GIMP on the image at point."
    (interactive)
    (my/edit-image-helper "gimp"))

  (defun my/edit-pinta ()
    "Open Pinta on the image at point."
    (interactive)
    (my/edit-image-helper "pinta"))

  ;;------------------------------------------------------------------------------
  ;; Sorting
  ;;------------------------------------------------------------------------------

  (defun my/org-sort-all-org-entries ()
    ""
    (interactive)
    (let ((fun #'(lambda nil
                   (condition-case nil
                       (org-sort-entries nil ?o)
                     (user-error t)))))
      (org-map-entries fun)))

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
        ref)))

  (defun org-get-linked-files (&optional buffer)
    "Get all of the `file' type links in a buffer.
Current buffer is assumed unless specified by BUFFER"
    (with-current-buffer
        (or buffer (current-buffer))
      (org-element-map (org-element-parse-buffer) 'link
        (lambda (link)
          (when (string= (org-element-property :type link) "file")
            (org-element-property :path link))))))

  (defun org-get-links (&optional buffer)
    "Get all of the `file' type links in a buffer.
Current buffer is assumed unless specified by BUFFER"
    (with-current-buffer
        (or buffer (current-buffer))
      (org-element-map (org-element-parse-buffer) 'link
        (lambda (link)
          (org-element-property :raw-link link)))))

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

  ;;------------------------------------------------------------------------------
  ;; Publishing
  ;;------------------------------------------------------------------------------

  (defvar org-default-publish-dest "nfs:/home/public"
    "Default org publishing destination for `org-publish-this-file'.
It will be pushed with rsync.")

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

  (defun org-report-dead-links ()
    "Create a report of dead files in my org mode notes directory."
    (interactive)
    (shell-command (format "cd %s && ./find-dead-links.sh" org-directory))
    (find-file (concat org-directory "/unused-links.org")))


  ;;------------------------------------------------------------------------------
  ;; Table Functions
  ;;------------------------------------------------------------------------------

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

  (defun org-latex-preview-all ()
    (interactive)
    (org-latex-preview '(16)))

  (defun org-latex-preview-clear ()
    (interactive)
    (org-latex-preview '(64)))


  ;;------------------------------------------------------------------------------
  ;; SciMAX Org-Return
  ;;------------------------------------------------------------------------------

  (require 'org-inlinetask)
  (defun scimax/org-return (&optional ignore)
    "Add new list item, heading or table row with RET.
   A double return on an empty element deletes it.
   Use a prefix arg to get regular RET. "
    (interactive "P")
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

  (defun org-toggle-checkbox-presence ()
    "Toggle the presence of org list checkboxes."
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively #'org-toggle-checkbox)))

  ;; HACK: patch issue with eldoc help
  ;; sometimes what gets passed into this function has nil values, e.g.
  ;; org-babel-merge-params(((:session . "none") (:results . "replace") (:exports . "code") (:cache . "no") (:noweb . "no") (:hlines . "no") (:tangle . "no")) nil nil nil ((:session . "test") (:resulhk))) ;;
  (defun org-babel-filter-nil-params (orig-fun &rest alists)
    (apply orig-fun (-filter #'-non-nil alists)))

  (advice-add 'org-babel-merge-params
              :around
              #'org-babel-filter-nil-params)

  ) ;; end use-package! org

;;------------------------------------------------------------------------------
;; Org Download
;;------------------------------------------------------------------------------

(use-package! org-download
  :defer-incrementally t
  :after org
  :config

  (setq-default org-download-image-dir "./images/screenshots")

  (setq org-download-method            'directory
        org-download-screenshot-method "import %s"
        org-download-heading-lvl       0

        ;; annotate the width of the image with the actual width of the screenshot
        org-download-annotate-function
        (lambda (s)
          (let ((width (string-to-number (shell-command-to-string (format "identify -format \"%%w\" %s" s)))))
            (concat (format "#+ATTR_ORG: :width %spx\n" width)
                    (format "#+ATTR_HTML: :style max-width:100%%;width:%spx\n" width))))

        org-download-image-org-width 0
        org-download-image-attr-list nil
        org-download-link-format "[[file:%s]]\n"
        org-download-abbreviate-filename-function #'file-relative-name
        org-download-link-format-function #'org-download-link-format-function-default))

;;------------------------------------------------------------------------------
;;  Org web tools + url parsing
;;------------------------------------------------------------------------------

(use-package! org-web-tools
  :after org
  :commands (org-web-tools--get-url
             www-get-page-title
             org-capture-url
             org-link-parse
             org-web-tools--html-title
             www-get-page-title org-capture-url
             org-link->markdown)
  :init

  ;; supress warning of obsolete generalized variable in org-web-tools
  ;; should check this periodically and remove if org-web-tools updates
  ;; for 29.1
  (cl-remprop 'buffer-substring 'byte-obsolete-generalized-variable)

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

  (defun md-shorten-url-by-title ()
    (interactive)
    (org-shorten-url-by-title)
    (org-link->markdown))

  (defun org-capture-url-from-clipboard (_)
    "Capture a URL from clipboard and paste it as an org link"
    (interactive)
    (org-capture-url (current-kill 0)))

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

  (defun my/url->md ()
    "Convert the URL at point into an md mode formatted link. The
  title of the page is retrieved from the web page"
    (interactive)
    (ap/url->org)
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

  (defun md-shorten-indico-url ()
    (interactive)
    (org-shorten-indico-link)
    (org-link->markdown))

  ;; Functions to Convert to/from org / markdown links

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
    (require 'markdown)
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

  :config

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

  (defun www-get-page-title (url)
    "Gets the title of a webpage at URL"
    (org-web-tools--html-title (org-web-tools--get-url url)))

  (defun org-capture-url (url)
    (insert (org-insert-link nil url (www-get-page-title url))))

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
      (error "Cannot parse %s as Org link" link))))

;;------------------------------------------------------------------------------
;; Latex
;;------------------------------------------------------------------------------

;; Latex Export Class
(use-package! ox-latex
  :defer-incrementally t
  :after org
  :config

  (setq org-latex-article-header
        (concat
         "\\documentclass[10pt]{article}\n"
         "\\usepackage[letterpaper, margin=2.54cm]{geometry}\n"
         "\\usepackage[utf8]{inputenc}\n"
         "\\usepackage{fixltx2e}\n"
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
         ;; "\\usepackage{mathpazo}\n"
         "\\renewcommand{\\familydefault}{\\sfdefault}\n"
         "\\usepackage{color}\n"
         "\\usepackage{enumerate}\n"
         "\\definecolor{bg}{rgb}{0.95,0.95,0.95}\n"
         "\\tolerance=1000\n"
         "[NO-DEFAULT-PACKAGES]\n"
         "[PACKAGES]\n"
         "[EXTRA]\n"
         "\\linespread{1.0}\n"
         "\\hypersetup{pdfborder=0 0 0}"))

  (setq org-latex-pdf-process
        '("latexmk -shell-escape -f -pdf -%latex -interaction=nonstopmode -output-directory=%o %f"))

  (add-to-list
   'org-latex-classes
   `("article" ,org-latex-article-header
     ("\\section{%s}"       . "\\section*{%s}")
     ("\\subsection{%s}"    . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}"     . "\\paragraph*{%s}")
     ("\\subparagraph{%s}"  . "\\subparagraph*{%s}"))))

;;------------------------------------------------------------------------------
;; Org roam
;;------------------------------------------------------------------------------

;; (use-package! org-roam
;;   :after org
;;   :config

;;   (setq org-roam-db-autosync-mode t
;;         org-roam-directory (file-truename "~/notes/")
;;         org-roam-graph-extra-config '(("rankdir" . "RL"))
;;         ;; (setq org-roam-graph-edge-extra-config '(("dir" . "back")))
;;         org-roam-graph-link-hidden-types '("file" "http" "https"))

;;   ;; add a space before inserting a node for lists etc so it does
;;   ;; not come out as -[link] but rather as - [link]
;;   (advice-add 'org-roam-node-insert :before (lambda () (insert " ")))
;;   (setq org-roam-db-location "~/.org-roam.db")
;;   (setq org-roam-link-title-format "Org:%s"))


