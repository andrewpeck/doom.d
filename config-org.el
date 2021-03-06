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
  ;; FIXME: this should be removed at some point, but it is crashing emacs after
  ;; the latest update
  (remove-hook 'org-babel-after-execute-hook
               #'+org-redisplay-inline-images-in-babel-result-h)

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

  (add-to-list 'load-path "~/Sync/org")

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
  ;;     0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "  ??? ")))))

  (add-hook 'evil-org-agenda-mode-hook
            'evil-org-agenda-set-keys)

  (setq org-log-done 'time)

  ;; https://cestlaz.github.io/posts/using-emacs-26-gcal/#.WIqBud9vGAk
  ;; https://console.cloud.google.com/apis/dashboard?pli=1
  ;;(setq
  ;; org-gcal-file-alist '(
  ;;              ("7rlvcq7qs49tb3ed0rpe97f2us@group.calendar.google.com" . "~/Sync/org/gcal-medical.org")
  ;;              ("ericshazen@gmail.com"                                 . "~/Sync/org/gcal-hazen.org")
  ;;              ("peckandrew@gmail.com"                                 . "~/Sync/org/gcal-peck.org")
  ;;              ("ijavvtk9nsrs89e1h3oahltgko@group.calendar.google.com" . "~/Sync/org/gcal-work.org")
  ;;              )
  ;;      org-gcal-remove-api-cancelled-events t
  ;;      )
  ;;      ;;org-gcal-auto-archive nil
  ;;      ;;org-gcal-notify-p nil

  ;;(add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
  ;;(add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-fetch) ))

  (setq org-link-file-path-type 'relative)
  (setq org-agenda-files (list "~/Sync/org"))
  (setq org-id-locations-file "~/Sync/org/.org-id-locations")
  ;; (setq org-hide-emphasis-markers nil)
  (+org-pretty-mode t)
  (setq org-export-with-sub-superscripts nil)
  (setq org-directory "~/Sync/org")
  (setq org-default-notes-file (concat org-directory "/todo.org"))
  ;; https://github.com/sk8ingdom/.emacs.d/blob/master/org-mode-config/org-capture-templates.el
  ;; https://cestlaz.github.io/posts/using-emacs-26-gcal/
  (setq org-capture-templates '(
                                ("t" "TODO" entry (file+headline +org-capture-todo-file "To do")
                                 "** TODO %?" :prepend t)
                                ("a" "Appointment" entry (file  "~/Sync/org/gcal-peck.org" )
                                 "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
                                ("s" "Shopping" item (file+headline +org-capture-todo-file "Shopping")
                                 "- [ ] %?" :prepend t)
                                ))

  ;;
  (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))
  (add-to-list 'org-file-apps '("\\.odt\\'" . "xdg-open %s"))


  ;; Org Image Attach
  ;;------------------------------------------------------------------------------

  (setq org-attach-id-dir "./images/screenshots")

  (map! :leader
        :prefix "ma"
        :desc "Download Screenshot" "c" #'org-download-screenshot
        :desc "Download Clipboard" "p" #'org-download-clipboard
        :desc "Download Yank" "P" #'org-download-yank
        )

  (defun org-remove-link-and-trash-linked-file ()
    "Remove `org-mode' link at point and trash linked file."
    (interactive)
    (let* ((link (org-element-context))
           (path (org-element-property :path link)))
      (move-file-to-trash path)
      (delete-region (org-element-property :begin link)
                     (org-element-property :end link))))

  ) ;; after! evil

(after! org-download

  ;; Drag-and-drop to `dired`
  (add-hook 'dired-mode-hook 'org-download-enable)

  (setq org-download-image-dir "./images/screenshots")

  (defun org-download-named-screenshot (fname)
    (interactive "FEnter Filename:")
    (make-directory (file-name-directory fname) t)
    (if (functionp org-download-screenshot-method)
        (funcall org-download-screenshot-method fname)
      (shell-command-to-string
       (format org-download-screenshot-method fname)))
    (org-download-image fname))

  (setq org-directory "~/projects/org"
        org-attach-id-dir "./images/"
        org-download-dir "download/")

  (setq-default org-download-method            'directory
                ;;org-download-screenshot-method 'nil
                ;;org-download-screenshot-method "spectacle -b -r -o %s"
                org-download-screenshot-method "xfce4-screenshooter -r -s %s"
                org-download-image-dir         "./images/screenshots"
                org-download-heading-lvl       0
                org-download-link-format       "[[file:%s]]\n"
                ;;org-download-image-attr-list   ("#+attr_org: :width 800px")
                org-download-annotate-function (lambda (text) "")
                org-download-image-org-width   800))

(after! org-attach-screenshot
  (setq org-attach-screenshot-command-line "xfce4-screenshooter -r -s %f"))

;; Org publishing
;;;;;(after! org
;;;;;  (setq org-list-allow-alphabetical t)
;;;;;  (setq org-publish-project-alist
;;;;;        '(
;;;;;          ;; ... add all the components here (see below)...
;;;;;          ("org-notes"
;;;;;           :base-directory "~/Sync/notes/"
;;;;;           :base-extension "org"
;;;;;           :publishing-directory "~/notes_html/"
;;;;;           :recursive t
;;;;;           :publishing-function org-html-publish-to-html
;;;;;           :headline-levels 4  ; Just the default for this project.
;;;;;           :auto-preamble t
;;;;;           )
;;;;;
;;;;;          ("org-static"
;;;;;           :base-directory "~/Sync/notes/"
;;;;;           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
;;;;;           :publishing-directory "~/notes_html/"
;;;;;           :recursive t
;;;;;           :publishing-function org-publish-attachment
;;;;;           )
;;;;;
;;;;;          ("org" :components ("org-notes" "org-static"))
;;;;;
;;;;;          )
;;;;;        )
;;;;;  )
;;;;;

;;; Org roam
;;------------------------------------------------------------------------------

(after! org-roam
  (setq org-roam-db-autosync-mode t)
  (setq org-roam-directory (file-truename "~/Sync/notes/"))
  (setq org-roam-graph-extra-config '(("rankdir" . "RL")))
  ;; (setq org-roam-graph-edge-extra-config '(("dir" . "back")))
  (setq org-roam-graph-link-hidden-types '("file" "http" "https"))

  ;; add a space before inserting a node for lists etc so it does
  ;; not come out as -[link] but rather as - [link]
  (advice-add 'org-roam-node-insert :before (lambda () (insert " ")))

  (map! :leader
        :prefix "n"
        :desc "Org-Roam-Insert"     "i" #'org-roam-node-insert
        :desc "Org-Roam-Find"       "/" #'org-roam-find-file
        :desc "Org-Roam-Buffer"     "r" #'org-roam
        :desc "Org-Roam-Show-Graph" "g" #'org-roam-graph
        )

  (setq org-roam-db-location "~/.org-roam.db")
  (setq org-roam-link-title-format "Org:%s"))

  ;; (use-package! org-roam-server
  ;;   :ensure t
  ;;   :config
  ;;   (setq org-roam-server-host "127.0.0.1"
  ;;         org-roam-server-port 8080
  ;;         org-roam-server-authenticate nil
  ;;         org-roam-server-export-inline-images t
  ;;         org-roam-server-serve-files nil
  ;;         org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
  ;;         org-roam-server-network-poll t
  ;;         org-roam-server-network-arrows nil
  ;;         org-roam-server-network-label-truncate t
  ;;         org-roam-server-network-label-truncate-length 60
  ;;         org-roam-server-network-label-wrap-length 20))




;;(require 'org-download)


;;
;; (setq org-roam-backlinks-mode-hook
;;       '(
;;         (flyspell-mode)
;;         (define-key evil-motion-state-map (kbd "RET") 'org-roam-open-at-point)
;;         )
;;       )

;; (setq org-roam-completion-system 'ivy)

;; (setq org-roam-capture-templates
;;       '(("d" "default" plain (function org-roam--capture-get-point)
;;          "%?"
;;          :file-name "${title}"
;;          :head "#+SETUPFILE: \"org.setup\"\n#+TITLE: ${title}\n#"

;;          :unnarrowed t))

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
