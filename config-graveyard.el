;; -*- lexical-binding: t; -*-


;;(defvar +company-backend-alist
;;  '((text-mode company-yasnippet company-dabbrev  company-ispell)
;;    (prog-mode company-yasnippet company-capf )
;;    (conf-mode company-yasnippet company-capf company-dabbrev-code )))

;;(set-company-backend! 'python-mode-hook '(company-yasnippet company-jedi
;;                                       company-files
;;                                      company-keywords company-capf company-dabbrev-code
;;                                      company-etags company-dabbrev))

;;(setq company-frontends '(company-preview-if-just-one-frontend company-box-frontend company-echo-metadata-frontend))
;;(setq company-frontends '(company-preview-if-just-one-frontend  company-echo-metadata-frontend))
;;(setq company-frontends '(company-box-frontend company-echo-metadata-frontend))
;;(add-to-list 'company-backends 'company-irony)

;;(define-key company-active-map (kbd "<return>") nil)
;;(define-key company-active-map (kbd "<tab>") #'company-complete-selection )

;;   (set-company-backend! '(company-tabnine company-yasnippet  company-files))
;;                                         ;(set-company-backend! 'org-mode '(company-roam company-files company-dabbrev))
;;   ;;(set-company-backend! '(tcl-mode) '(company-tabnine company-yasnippet))
;;   (set-company-backend! '(prog-mode) '(company-tabnine company-yasnippet))
;;   )

;;; "Recycle Bin"

;; Scrolling.
;; Good speed and allow scrolling through large images (pixel-scroll).
;; Note: Scroll lags when point must be moved but increasing the number
;;       of lines that point moves in pixel-scroll.el ruins large image
;;       scrolling. So unfortunately I think we'll just have to live with
;;       this.
;; (pixel-scroll-mode)
;; (setq pixel-dead-time 0) ; Never go back to the old scrolling behaviour.
;; (setq pixel-resolution-fine-flag t) ; Scroll by number of pixels instead of lines (t = frame-char-height pixels).
;; (setq mouse-wheel-scroll-amount '(2)) ; Distance in pixel-resolution to scroll each mouse wheel event.
;; (setq mouse-wheel-progressive-speed nil) ; Progressive speed is too fast for me.




;;(add-to-list 'default-frame-alist '(drag-internal-border . 1))
;;(add-to-list 'default-frame-alist '(right-divider-width . 5))
;;(add-to-list 'default-frame-alist '(bottom-divider-width . 5))
;;(add-to-list 'default-frame-alist '(left-fringe . 5))
;;(add-to-list 'default-frame-alist '(right-fringe . 5))
;;(add-to-list 'default-frame-alist '(internal-border-width . 5))



;; (setq speedbar-show-unknown-files t) ; show all files
;; (setq speedbar-use-images nil) ; use text for buttons
;;                                         ;(setq sr-speedbar-right-side nil) ; put on left side


;; ;; active Babel languages
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((gnuplot . t)))
;; ;; add additional languages with '((language . t)))

;; port this to xclip....
;; (defun formatted-copy ()
;;   "Export region to HTML, and copy it to the clipboard."
;;   (interactive)
;;   (save-window-excursion
;;     (let* ((buf (org-export-to-buffer 'html "*Formatted Copy*" nil nil t t))
;;            (html (with-current-buffer buf (buffer-string))))
;;       (with-current-buffer buf
;;         (shell-command-on-region
;;          (point-min)
;;          (point-max)
;;          "textutil -stdin -format html -convert rtf -stdout | pbcopy"))
;;       (kill-buffer buf))))


;;(add-hook 'org-mode-hook (lambda ()
;;                           "Beautify Org Checkbox Symbol"
;;                           (push '("[ ]" . "☐") prettify-symbols-alist)
;;                           (push '("[X]" . "☑") prettify-symbols-alist)
;;                           (push '("[-]" . "❍") prettify-symbols-alist)
;;                           (prettify-symbols-mode)))
;;(add-hook 'vhdl-mode-hook (lambda ()
;;                            "Beautify VHDL Symbols"
;;                            ;;(push '("/=" . "≠") prettify-symbols-alist)
;;                            ;;(push '("=>" . "⇒") prettify-symbols-alist)
;;                            ;;(push '("<=" . "⇐") prettify-symbols-alist)
;;                            (prettify-symbols-mode)))


;;; Outline Mode
;;; Clean code folding via Outline minor mode.
;;(add-hook 'latex-mode-hook 'outline-minor-mode)

;; Show all headings but no content in Outline mode.
;;(add-hook 'outline-minor-mode-hook
;;          (defun baba/outline-overview ()
;;            "Show only outline headings."
;;            (outline-show-all)
;;            (outline-hide-body)))
;;
;; TODO: useful things here
;; https://www.reddit.com/r/emacs/comments/e2u5n9/code_folding_with_outlineminormode/
;;
;; Customize the distracting folding markers.
;;(after! outline-mode
;;  (set-display-table-slot
;;   standard-display-table
;;   'selective-display
;;   (let ((face-offset (* (face-id 'shadow) (lsh 1 22))))
;;     (vconcat (mapcar (lambda (c) (+ face-offset c)) " +"))))
;;  )

;; (defun my-mode-line/padding ()
;;   (let ((r-length (length (format-mode-line mode-line-end-spaces))))
;;     (propertize " "
;;                 'display `(space :align-to (- right ,r-length)))))

;; (setq mode-line-format
;;       (quote
;;        (""

;;         ;; (eldoc-mode-line-string (" " eldoc-mode-line-string " "))

;;         ("%e" mode-line-front-space

;;          (:propertize
;;           (""
;;            mode-line-mule-info
;;            mode-line-client
;;            mode-line-modified
;;            mode-line-remote)
;;           display (min-width (5.0)))

;;          mode-line-frame-identification

;;          ;;  system name
;;          "   "
;;          (:eval (substring (system-name) 0 (string-match "\\..+" (system-name))))

;;          ;; buffer path + file name
;;          ":" default-directory
;;          mode-line-buffer-identification


;;          "   "
;;          mode-line-position

;;          (:eval (my-mode-line/padding))

;;          ;; git / vc status
;;          (vc-mode vc-mode) "  "

;;          mode-line-process
;;          ;; mode-line-modes
;;          ;; mode-line-misc-info
;;          mode-line-end-spaces))))

;; (use-package doom-modeline
;;   :hook (after-init . doom-modeline-mode)
;;   :custom
;;   (doom-modeline-height 25)
;;   (doom-modeline-bar-width 1)
;;   (doom-modeline-icon t)
;;   (doom-modeline-major-mode-icon t)
;;   (doom-modeline-major-mode-color-icon t)
;;   (doom-modeline-buffer-file-name-style 'truncate-upto-project)
;;   (doom-modeline-buffer-state-icon t)
;;   (doom-modeline-buffer-modification-icon t)
;;   (doom-modeline-minor-modes nil)
;;   (doom-modeline-enable-word-count nil)
;;   (doom-modeline-buffer-encoding t)
;;   (doom-modeline-indent-info nil)
;;   (doom-modeline-checker-simple-format t)
;;   (doom-modeline-vcs-max-length 12)
;;   (doom-modeline-env-version t)
;;   (doom-modeline-irc-stylize 'identity)
;;   (doom-modeline-github-timer nil)
;;   (doom-modeline-gnus-timer nil))

;; https://github.com/mclear-tools/bespoke-modeline
;; (setq bespoke-modeline-position 'bottom
;;       ;; Set mode-line height
;;       bespoke-modeline-size 2
;;       ;; Show diff lines in mode-line
;;       bespoke-modeline-git-diff-mode-line t
;;       ;; Set mode-line cleaner
;;       bespoke-modeline-cleaner t
;;       ;; Use mode-line visual bell
;;       bespoke-modeline-visual-bell t)

;; (bespoke-modeline-mode t)


;; (setq vc-make-backup-files t
;;       ;; auto-save-default t
;;       ;; version-control t ; Use version numbers for backups.
;;       delete-old-versions t ; Don't ask to delete excess backup versions.
;;       delete-by-moving-to-trash nil
;;       kept-new-versions 10 ; Number of newest versions to keep.
;;       backup-by-copying t ; Copy all files, don't rename them.
;;       kept-old-versions 0  ; Number of oldest versions to keep.
;;       backup-inhibited nil
;;       make-backup-files t
;;       backup-directory-alist `(("." . ,(expand-file-name "~/emacs-backups")))
;;       tramp-backup-directory-alist `(("." . ,(expand-file-name "~/emacs-backups")))
;;       )


;; (defun force-backup-of-buffer ()
;;   (setq buffer-backed-up nil))
;; (add-hook 'before-save-hook  'force-backup-of-buffer)

;; (defun force-backup-of-buffer ()
;;   ;; Make a special "per session" backup at the first save of each
;;   ;; emacs session.
;;   (when (not buffer-backed-up)
;;     ;; Override the default parameters for per-session backups.
;;     (let ((backup-directory-alist `(("" . ,(expand-file-name "~/emacs-backups"))))
;;           (kept-new-versions 3))
;;       (backup-buffer)))
;;   ;; Make a "per save" backup on each save.  The first save results in
;;   ;; both a per-session and a per-save backup, to keep the numbering
;;   ;; of per-save backups consistent.
;;   (let ((buffer-backed-up nil))
;;     (backup-buffer)))

;; (add-hook 'before-save-hook  'force-backup-of-buffer)


;; make backup to a designated dir, mirroring the full path
;; http://xahlee.info/emacs/emacs/emacs_set_backup_into_a_directory.html ;;
(defun my-backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backupRootDir "~/emacs-backups/")
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path, for example, “C:”
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~"))))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))

(setq make-backup-file-name-function 'my-backup-file-name)

;; (setq backup-directory-alist `(("." . ,(expand-file-name "~/emacs-backups"))))
;; (setq tramp-backup-directory-alist `(("." . ,(expand-file-name "~/emacs-backups"))))

;; (setq mode-line-modes "    mode-line-mode    ")

;; (after! doom-modeline (doom-modeline-def-segment process ""))

;; (setq mode-line-modes

;;     (list (propertize "%[" 'help-echo recursive-edit-help-echo)
;;           "("
;;           `(:propertize ("" mode-name)
;;             help-echo "Major mode\n\
;; mouse-1: Display major mode menu\n\
;; mouse-2: Show help for major mode\n\
;; mouse-3: Toggle minor modes"
;;             mouse-face mode-line-highlight
;;             local-map ,mode-line-major-mode-keymap)
;;
;;           ;;'("" mode-line-process)
;;           `(:propertize ("" minor-mode-alist)
;;             mouse-face mode-line-highlight
;;             help-echo "Minor mode\n\
;; mouse-1: Display minor mode menu\n\
;; mouse-2: Show help for minor mode\n\
;; mouse-3: Toggle minor modes"
;;             local-map ,mode-line-minor-mode-keymap)
;;           (propertize "%n" 'help-echo "mouse-2: Remove narrowing from buffer"
;;                       'mouse-face 'mode-line-highlight
;;                       'local-map (make-mode-line-mouse-map
;;                                   'mouse-2 #'mode-line-widen))
;;           ")"
;;           (propertize "%]" 'help-echo recursive-edit-help-echo)
;;           " "))


;; https://jblevins.org/log/mmm
;;
;; (require 'mmm-mode)
;; (setq mmm-global-mode 'maybe)
;; (setq mmm-parse-when-idle 't)

;; (defun my-mmm-markdown-auto-class (lang &optional submode)
;;   "Define a mmm-mode class for LANG in `markdown-mode' using SUBMODE.
;; If SUBMODE is not provided, use `LANG-mode' by default."
;;   (let ((class (intern (concat "markdown-" lang)))
;;         (submode (or submode (intern (concat lang "-mode"))))
;;         (front (concat "^```" lang "[\n\r]+"))
;;         (back "^```"))
;;     (mmm-add-classes (list (list class :submode submode :front front :back back)))
;;     (mmm-add-mode-ext-class 'markdown-mode nil class)))

;; ;; Mode names that derive directly from the language name
;; (mapc 'my-mmm-markdown-auto-class
;;       '("vhdl" "toml" "bash" "ini" "awk" "bibtex" "c" "cpp" "css" "html" "latex" "lisp" "makefile"
;;         "markdown" "python" "r" "ruby" "sql" "stata" "xml"))


;; https://github.com/rougier/svg-tag-mode
;; :
;; :firmware:
(setq svg-tag-tags
      '(("\\(:[A-Z]+:\\)" . ((lambda (tag)
                               (svg-tag-make tag :beg 1 :end -1))))))

(setq svg-tag-tags
      '(("\\(:#[A-Za-z0-9]+\\)" . ((lambda (tag)
                                     (svg-tag-make tag :beg 2))))
        ("\\(:#[A-Za-z0-9]+:\\)$" . ((lambda (tag)
                                       (svg-tag-make tag :beg 2 :end -1))))))

(setq svg-tag-tags
      '(
        ("\\(:[A-Z]+\\)\|[a-zA-Z#0-9]+:" . ((lambda (tag)
                                              (svg-tag-make tag :beg 1 :inverse t
                                                            :margin 0 :crop-right t))))
        (":[A-Z]+\\(\|[a-zA-Z#0-9]+:\\)" . ((lambda (tag)
                                              (svg-tag-make tag :beg 1 :end -1
                                                            :margin 0 :crop-left t))))

        ))

(setq svg-tag-tags
      '((":TODO:" . ((lambda (tag) (svg-tag-make "TODO"))))))

;; )

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


;; https://kitchingroup.cheme.cmu.edu/blog/2014/12/21/Capturing-stderr-from-Python-in-org-mode-take-2/
(setq org-babel-python-command "python -i -c \"import pycse.orgmode\"")
