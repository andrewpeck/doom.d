;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'org))

(use-package org
  :init

  (add-hook 'org-mode-hook 'org-appear-mode)
  (add-hook 'org-mode-hook (defun hook/org-set-user-name () (setq-local user-full-name "A.P.")))
  (add-hook 'org-mode-hook (defun hook/org-disable-diff-hl-mode () (diff-hl-mode -1))) ;; diff-hl just makes line noise for org mode
  (add-hook 'org-mode-hook (defun hook/org-set-scroll-margin () (setq-local scroll-margin 1)))
  ;; (add-hook 'org-mode-hook (defun hook/org-auto-fill-mode () (auto-fill-mode t)))
  (add-hook 'org-mode-hook (defun hook/org-enable-evil-org-mode () (evil-org-mode)))
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

  ;; HACK: patch issue with eldoc help
  ;; sometimes what gets passed into this function has nil values, e.g.
  ;; org-babel-merge-params(((:session . "none") (:results . "replace") (:exports . "code") (:cache . "no") (:noweb . "no") (:hlines . "no") (:tangle . "no")) nil nil nil ((:session . "test") (:resulhk))) ;;
  (defun org-babel-filter-nil-params (orig-fun &rest alists)
    (apply orig-fun (-filter #'-non-nil alists)))

  (advice-add 'org-babel-merge-params :around #'org-babel-filter-nil-params)

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
  ) ;; end use-package! org

;;------------------------------------------------------------------------------
;; Poporg
;;------------------------------------------------------------------------------

(use-package poporg
  :config
  (map! :leader :prefix "e" :desc "Poporg Edit Comment"  "c"  #'poporg-dwim))

;;------------------------------------------------------------------------------
;; Org Appear
;;------------------------------------------------------------------------------

(use-package org-appear
  :after org
  :config
  (setq org-appear-autoemphasis t
        org-appear-inside-latex t
        org-appear-autolinks t
        org-appear-delay 0.1))

;;------------------------------------------------------------------------------
;; Org Modern
;;------------------------------------------------------------------------------

(comment
 (use-package org-modern-mode
   :after org
   :config

   (setq org-modern-checkbox
         '((?X  . "✓")
           (?-  . "␣")
           (?\s . "☐")))
   (setq org-modern-table nil)
   (global-org-modern-mode)))

;;------------------------------------------------------------------------------
;; Org Download
;;------------------------------------------------------------------------------

(use-package org-download
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
  :init
  ;; supress warning of obsolete generalized variable in org-web-tools
  ;; should check this periodically and remove if org-web-tools updates
  ;; for 29.1
  (cl-remprop 'buffer-substring 'byte-obsolete-generalized-variable))

;;------------------------------------------------------------------------------
;; Latex
;;------------------------------------------------------------------------------

;; Latex Export Class
(use-package ox-latex
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
