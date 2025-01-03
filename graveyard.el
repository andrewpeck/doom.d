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


;; (add-hook 'c++-mode-hook 'irony-mode)
;; (add-hook 'c-mode-hook 'irony-mode)
;; (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; (add-hook 'vhdl-mode-hook #'lsp)
;; (add-hook 'vhdl-mode-hook #'lsp-ui-mode)

;; (add-hook 'python-mode-hook #'lsp)
;; (add-hook 'python-mode-hook #'lsp-ui-mode)


(after! lsp-ui
  (setq-default lsp-headerline-breadcrumb-enable nil
                lsp-ui-doc-enable t
                lsp-ui-doc-show-with-mouse t
                lsp-ui-doc-show-with-cursor nil
                lsp-ui-doc-max-width 150
                lsp-ui-sideline-show-hover nil
                lsp-ui-sideline-enable nil
                lsp-ui-doc-delay 0.2))

(after! lsp-mode

  ;; vhdl-tool, hdl-checker vhdl-ls ghdl-ls
  (setq lsp-vhdl-server 'ghdl-ls)
  (cl-case lsp-vhdl-server
    ('vhdl-tool (setq lsp-vhdl-server-path (executable-find "vhdl-tool")))
    ('ghdl-ls (setq lsp-vhdl-server-path (executable-find "ghdl-ls")))
    ('vhdl-ls (setq lsp-vhdl-server-path (executable-find "vhdl_ls"))))

  (setq lsp-progress-via-spinner nil
        ccls-sem-highlight-method nil
        lsp-enable-on-type-formatting nil
        lsp-enable-indentation nil
        +format-with-lsp nil)

  ;; https://github.com/chipsalliance/verible/blob/master/verilog/tools/ls/README.md
  (add-to-list 'lsp-language-id-configuration '(verilog-mode . "verilog"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "verible-verilog-ls")
                    :major-modes '(verilog-mode)
                    :server-id 'verible-ls))

  (add-hook 'verilog-mode-hook 'lsp)
  )


;; (require 'lsp)
;; (add-hook 'vhdl-mode-hook #'lsp)
;; (add-to-list 'lsp-language-id-configuration '(vhdl-mode . "vhdl"))
;; (lsp-register-client
;;  (make-lsp-client :new-connection (lsp-stdio-connection '("ghdl-ls"))
;;                   :major-modes '(vhdl-mode)
;;                   :priority -1
;;                   :server-id 'lsp-vhdl-mode))

;; (setq lsp-log-io nil
;;       lsp-auto-configure t
;;       lsp-auto-guess-root t
;;       lsp-completion-enable t
;;       lsp-enable-xref t
;;       lsp-prefer-flymake nil
;;       lsp-use-native-json t
;;       lsp-enable-indentation t
;;       lsp-response-timeout 10
;;       lsp-restart 'auto-restart
;;       lsp-keep-workspace-alive t
;;       lsp-eldoc-render-all nil
;;       lsp-enable-snippet nil
;;       lsp-enable-folding t
;;       )

;; ;; lsp-ui gives us the blue documentation boxes and the sidebar info
;; (setq lsp-ui-sideline-ignore-duplicate t
;;       lsp-ui-sideline-delay 0.5
;;       lsp-ui-sideline-show-symbol t
;;       lsp-ui-sideline-show-hover t
;;       lsp-ui-sideline-show-diagnostics t
;;       lsp-ui-sideline-show-code-actions t
;;       lsp-ui-peek-always-show t
;;       lsp-ui-doc-use-childframe t)

;; (lsp-ui-flycheck-enable t)
;; (lsp-ui-sideline-enable t)
;; (lsp-ui-imenu-enable t)
;; (lsp-lens-mode t)
;; (lsp-ui-peek-enable t)
;; (lsp-ui-doc-enable t)

;;   ;;; company lsp
;; ;; install LSP company backend for LSP-driven completion
;; (setq company-lsp-cache-candidates t
;;       company-lsp-enable-recompletion t
;;       company-lsp-enable-snippet t
;;       company-lsp-async t)

;; (setq lsp-vhdl-server 'ghdl-ls
;;       lsp-vhdl-server-path (executable-find "ghdl-ls")
;;       lsp-vhdl--params nil)

;; (defun ap/toggle-theme ()
;;   (interactive)
;;   (if (eq doom-theme 'summerfruit)
;;       (progn
;;         (setq highlight-indent-guides-auto-enabled nil)
;;         (setq highlight-indent-guides-responsive "stack")
;;         (setq doom-theme 'doom-gruvbox)
;;         (load-theme 'doom-gruvbox t)
;;         (set-face-foreground 'highlight-indent-guides-character-face "#375c3c644822"))

;;     (progn
;;       (setq highlight-indent-guides-auto-enabled nil)
;;       (setq highlight-indent-guides-responsive "stack")
;;       (setq doom-theme 'summerfruit)
;;       (load-theme 'summerfruit t)
;;       (set-face-foreground 'highlight-indent-guides-character-face "#efefef"))))

(map! :leader :desc "Toggle Themes" "t t" #'ap/toggle-theme)

;; -*- lexical-binding: t; -*-
;;
;;; from https://raw.githubusercontent.com/hlissner/doom-emacs-private/master/lisp/modeline.el

;; This is a slimmed down modeline that manipulates `mode-line-format' directly.
;; Its purpose is to be a *significantly* lighter modeline for doom. Doom's
;; modeline has grown so much to meet the demand of users in general that it has
;; become overkill for my needs, so I've returned to the roots.

(defface mode-line-success-highlight '((t (:inherit mode-line-highlight)))
  "TODO")


(defvar mode-line-height 33)

(defvar modeline--redisplayed-p nil)
(defadvice! modeline-recalculate-height-a (&optional _force &rest _ignored)
  :before '(fit-window-to-buffer resize-temp-buffer-window)
  (unless modeline--redisplayed-p
    (setq-local modeline--redisplayed-p t)
    (redisplay t)))

;;; `active'
(defvar selected-window (selected-window))

(defun active ()
  (eq (selected-window) selected-window))

(add-hook! 'pre-redisplay-functions
  (defun set-selected-window (&rest _)
    "Set the variable `selected-window' appropriately."
    (let ((win (selected-window)))
      (unless (minibuffer-window-active-p win)
        (setq selected-window (frame-selected-window))))))

;;; Helpers
(defun make-xpm (color width height)
  "Create an XPM bitmap via COLOR, WIDTH and HEIGHT. Inspired by `powerline''s `pl/make-xpm'."
  (propertize
   " " 'display
   (let ((data (make-list height (make-list width 1)))
         (color (or color "None")))
     (ignore-errors
       (create-image
        (concat
         (format "/* XPM */\nstatic char * percent[] = {\n\"%i %i 2 1\",\n\". c %s\",\n\"  c %s\","
                 (length (car data))
                 (length data)
                 color
                 color)
         (apply #'concat
                (cl-loop with idx = 0
                         with len = (length data)
                         for dl in data
                         do (cl-incf idx)
                         collect
                         (concat "\""
                                 (cl-loop for d in dl
                                          if (= d 0) collect (string-to-char " ")
                                          else collect (string-to-char "."))
                                 (if (eq idx len) "\"};" "\",\n")))))
        'xpm t :ascent 'center)))))

(defun mode-line-format-icon (icon label &optional face help-echo voffset)
  (propertize (concat (all-the-icons-material
                       icon
                       :face face
                       :height 1.1
                       :v-adjust (or voffset -0.225))
                      (propertize label 'face face))
              'help-echo help-echo))


;;
;;; Segments

;; `mode-line-bar'
(defvar mode-line-bar
  (make-xpm nil 1 (max mode-line-height (frame-char-height))))
(put 'mode-line-bar 'risky-local-variable t)

(defvar mode-line--old-height nil)
(defun adjust-mode-line-height ()
  (unless mode-line--old-height
    (setq mode-line--old-height mode-line-height))
  (let ((default-height mode-line--old-height)
        (scale (or (frame-parameter nil 'font-scale) 0)))
    (if (> scale 0)
        (let* ((font-size (string-to-number
                           (aref (doom--font-name (frame-parameter nil 'font)
                                                  (selected-frame))
                                 xlfd-regexp-pixelsize-subnum)))
               (scale (frame-parameter nil 'font-scale)))
          (setq mode-line-height (+ default-height (* scale doom-font-increment))))
      (setq mode-line-height default-height))
    (setq mode-line-bar (make-xpm nil 1 mode-line-height))))
(add-hook 'doom-change-font-size-hook #'adjust-mode-line-height)

;; `mode-line-matches'
(progn
  (use-package! anzu
    :after-call isearch-mode
    :config
    ;; anzu and evil-anzu expose current/total state that can be displayed in the
    ;; mode-line.
    (defun doom-modeline-fix-anzu-count (positions here)
      "Calulate anzu counts via POSITIONS and HERE."
      (cl-loop for (start . end) in positions
               collect t into before
               when (and (>= here start) (<= here end))
               return (length before)
               finally return 0))

    (advice-add #'anzu--where-is-here :override #'doom-modeline-fix-anzu-count)

    (setq anzu-cons-mode-line-p nil) ; manage modeline segment ourselves
    ;; Ensure anzu state is cleared when searches & iedit are done
    (add-hook 'isearch-mode-end-hook #'anzu--reset-status t)
    (add-hook 'iedit-mode-end-hook #'anzu--reset-status)
    (advice-add #'evil-force-normal-state :before #'anzu--reset-status)
    ;; Fix matches segment mirroring across all buffers
    (mapc #'make-variable-buffer-local
          '(anzu--total-matched anzu--current-position anzu--state
            anzu--cached-count anzu--cached-positions anzu--last-command
            anzu--last-isearch-string anzu--overflow-p)))

  (use-package! evil-anzu
    :when (featurep! :editor evil)
    :after-call (evil-ex-start-search evil-ex-start-word-search evil-ex-search-activate-highlight))

  (defun mode-line--anzu ()
    "Show the match index and total number thereof.
Requires `anzu', also `evil-anzu' if using `evil-mode' for compatibility with
`evil-search'."
    (when (and (bound-and-true-p anzu--state)
               (not (bound-and-true-p iedit-mode)))
      (propertize
       (let ((here anzu--current-position)
             (total anzu--total-matched))
         (cond ((eq anzu--state 'replace-query)
                (format " %d replace " anzu--cached-count))
               ((eq anzu--state 'replace)
                (format " %d/%d " here total))
               (anzu--overflow-p
                (format " %s+ " total))
               (t
                (format " %s/%d " here total))))
       'face (if (active) 'mode-line-highlight))))

  (defun mode-line--evil-substitute ()
    "Show number of matches for evil-ex substitutions and highlights in real time."
    (when (and (bound-and-true-p evil-local-mode)
               (or (assq 'evil-ex-substitute evil-ex-active-highlights-alist)
                   (assq 'evil-ex-global-match evil-ex-active-highlights-alist)
                   (assq 'evil-ex-buffer-match evil-ex-active-highlights-alist)))
      (propertize
       (let ((range (if evil-ex-range
                        (cons (car evil-ex-range) (cadr evil-ex-range))
                      (cons (line-beginning-position) (line-end-position))))
             (pattern (car-safe (evil-delimited-arguments evil-ex-argument 2))))
         (if pattern
             (format " %s matches " (how-many pattern (car range) (cdr range)))
           " - "))
       'face (if (active) 'mode-line-highlight))))

  (defun mode-line--multiple-cursors ()
    "Show the number of multiple cursors."
    (when (bound-and-true-p evil-mc-cursor-list)
      (let ((count (length evil-mc-cursor-list)))
        (when (> count 0)
          (let ((face (cond ((not (active)) 'mode-line-inactive)
                            (evil-mc-frozen 'mode-line-highlight)
                            ('mode-line-success-highlight))))
            (concat (propertize " " 'face face)
                    (all-the-icons-faicon "i-cursor" :face face :v-adjust -0.0575)
                    (propertize " " 'face `(:inherit (variable-pitch ,face)))
                    (propertize (format "%d " count)
                                'face face)))))))

  (defun mode-line--overlay< (a b)
    "Sort overlay A and B."
    (< (overlay-start a) (overlay-start b)))

  (defun mode-line--iedit ()
    "Show the number of iedit regions matches + what match you're on."
    (when (and (bound-and-true-p iedit-mode)
               (bound-and-true-p iedit-occurrences-overlays))
      (propertize
       (let ((this-oc (or (let ((inhibit-message t))
                            (iedit-find-current-occurrence-overlay))
                          (save-excursion
                            (iedit-prev-occurrence)
                            (iedit-find-current-occurrence-overlay))))
             (length (length iedit-occurrences-overlays)))
         (format " %s/%d "
                 (if this-oc
                     (- length
                        (length (memq this-oc (sort (append iedit-occurrences-overlays nil)
                                                    #'mode-line--overlay<)))
                        -1)
                   "-")
                 length))
       'face (if (active) 'mode-line-highlight))))

  (defun mode-line--macro-recording ()
    "Display current Emacs or evil macro being recorded."
    (when (and (active)
               (or defining-kbd-macro
                   executing-kbd-macro))
      (let ((sep (propertize " " 'face 'mode-line-highlight)))
        (concat sep
                (propertize (if (bound-and-true-p evil-this-macro)
                                (char-to-string evil-this-macro)
                              "Macro")
                            'face 'mode-line-highlight)
                sep
                (all-the-icons-octicon "triangle-right"
                                       :face 'mode-line-highlight
                                       :v-adjust -0.05)
                sep))))

  (defvar mode-line-matches
    '(:eval
      (let ((meta (concat (mode-line--macro-recording)
                          (mode-line--anzu)
                          (mode-line--evil-substitute)
                          (mode-line--iedit)
                          (mode-line--multiple-cursors))))
        (or (and (not (equal meta "")) meta)
            " %I "))))
  (put 'mode-line-matches 'risky-local-variable t))

;; `mode-line-modes'
(setq-default
 mode-line-modes ; remove minor modes
 '(""
   (:propertize mode-name
                face bold
                mouse-face mode-line-highlight)
   mode-line-process
   "%n"
   "%]"
   " ")

 ;; `mode-line-buffer-identification'
 mode-line-buffer-identification ; slightly more informative buffer id
 '((:eval
    (propertize
     (let ((buffer-file-name (buffer-file-name (buffer-base-buffer))))
       (or (when buffer-file-name
             (if-let (project (doom-project-root buffer-file-name))
                 (let ((filename (or buffer-file-truename (file-truename buffer-file-name))))
                   (file-relative-name filename (concat project "..")))))
           "%b"))
     'face (cond ((buffer-modified-p)
                  '(error mode-line-buffer-id))
                 ((active)
                  'mode-line-buffer-id))
     'help-echo buffer-file-name))
   (buffer-read-only (:propertize " RO" face warning))))

;; `mode-line-position'
(setq mode-line-position '("  %l:%C %p  "))

;; `mode-line-checker'
(defun mode-line-checker-update (&optional status)
  "Update flycheck text via STATUS."
  (setq mode-line-checker
        (pcase status
          (`finished
           (if flycheck-current-errors
               (let-alist (flycheck-count-errors flycheck-current-errors)
                 (let ((error (or .error 0))
                       (warning (or .warning 0))
                       (info (or .info 0)))
                   (mode-line-format-icon "do_not_disturb_alt"
                                          (number-to-string (+ error warning info))
                                          (cond ((> error 0)   'error)
                                                ((> warning 0) 'warning)
                                                ('success))
                                          (format "Errors: %d, Warnings: %d, Debug: %d"
                                                  error
                                                  warning
                                                  info))))
             (mode-line-format-icon "check" "" 'success)))
          (`running     (mode-line-format-icon "access_time" "*" 'font-lock-comment-face "Running..."))
          (`errored     (mode-line-format-icon "sim_card_alert" "!" 'error "Errored!"))
          (`interrupted (mode-line-format-icon "pause" "!" 'font-lock-comment-face "Interrupted"))
          (`suspicious  (mode-line-format-icon "priority_high" "!" 'error "Suspicious")))))
(add-hook 'flycheck-status-changed-functions #'mode-line-checker-update)
(add-hook 'flycheck-mode-hook #'mode-line-checker-update)

(defvar-local mode-line-checker nil
  "Displays color-coded error status in the current buffer with pretty
icons.")
(put 'mode-line-checker 'risky-local-variable t)

;; `mode-line-selection-info'
(defsubst doom-modeline-column (pos)
  "Get the column of the position `POS'."
  (save-excursion (goto-char pos)
                  (current-column)))

(defun add-selection-segment ()
  (add-to-list 'mode-line-format-left 'mode-line-selection-info 'append))
(defun remove-selection-segment ()
  (delq! 'mode-line-selection-info mode-line-format-left))

(if (featurep 'evil)
    (progn
      (add-hook 'evil-visual-state-entry-hook #'add-selection-segment)
      (add-hook 'evil-visual-state-exit-hook #'remove-selection-segment))
  (add-hook 'activate-mark-hook #'add-selection-segment)
  (add-hook 'deactivate-mark-hook #'remove-selection-segment))


(defvar mode-line-selection-info
  '(:eval
    (when (or mark-active
              (and (bound-and-true-p evil-local-mode)
                   (eq evil-state 'visual)))
      (cl-destructuring-bind (beg . end)
          (if (boundp 'evil-local-mode)
              (cons evil-visual-beginning evil-visual-end)
            (cons (region-beginning) (region-end)))
        (propertize
         (let ((lines (count-lines beg (min end (point-max)))))
           (concat " "
                   (cond ((or (bound-and-true-p rectangle-mark-mode)
                              (and (bound-and-true-p evil-visual-selection)
                                   (eq 'block evil-visual-selection)))
                          (let ((cols (abs (- (doom-modeline-column end)
                                              (doom-modeline-column beg)))))
                            (format "%dx%dB" lines cols)))
                         ((and (bound-and-true-p evil-visual-selection)
                               (eq evil-visual-selection 'line))
                          (format "%dL" lines))
                         ((> lines 1)
                          (format "%dC %dL" (- end beg) lines))
                         ((format "%dC" (- end beg))))
                   (when (derived-mode-p 'text-mode)
                     (format " %dW" (count-words beg end)))
                   " "))
         'face (if (active) 'success)))))
  "Information about the current selection, such as how many characters and
lines are selected, or the NxM dimensions of a block selection.")
(put 'mode-line-selection-info 'risky-local-variable t)

;; `mode-line-encoding'
(defconst mode-line-encoding
  '(:eval
    (concat (pcase (coding-system-eol-type buffer-file-coding-system)
              (0 " LF ")
              (1 " RLF ")
              (2 " CR "))
            (let ((sys (coding-system-plist buffer-file-coding-system)))
              (if (memq (plist-get sys :category)
                        '(coding-category-undecided coding-category-utf-8))
                  "UTF-8"
                (upcase (symbol-name (plist-get sys :name)))))
            "  ")))
(put 'mode-line-encoding 'risky-local-variable t)


;;
;;; Setup

(defvar-local mode-line-format-left nil)
(put 'mode-line-format-left 'risky-local-variable t)


(defvar-local mode-line-format-right nil)
(put 'mode-line-format-right 'risky-local-variable t)

(setq-default
 mode-line-format-left
 '(""
   mode-line-matches
   " "
   mode-line-buffer-identification
   mode-line-position)

 mode-line-format-right
 `(""
   mode-line-misc-info
   mode-line-modes
   (vc-mode ("  "
             ,(all-the-icons-octicon "git-branch" :v-adjust 0.0)
             vc-mode " "))
   " "
   mode-line-encoding
   (mode-line-checker ("" mode-line-checker "   ")))

 ;;
 mode-line-format
 '(""
   mode-line-bar
   mode-line-format-left
   (:eval
    (propertize
     " "
     'display
     `((space :align-to (- (+ right right-fringe right-margin)
                           ,(string-width
                             (format-mode-line '("" mode-line-format-right))))))))
   mode-line-format-right))

(with-current-buffer "*Messages*"
  (setq mode-line-format (default-value 'mode-line-format)))


;;
;;; Other modelines

(defun set-project-modeline ()
  (setq mode-line-format-left
        `(" "
          ,(all-the-icons-octicon
            "file-directory"
            :face 'bold
            :v-adjust -0.05
            :height 1.25)
          (:propertize (" " (:eval (abbreviate-file-name default-directory)))
                       face bold))
        mode-line-format-right
        '("" mode-line-modes)))

(defun set-special-modeline ()
  (setq mode-line-format-left
        '(""
          mode-line-matches
          " "
          mode-line-buffer-identification)
        mode-line-format-right
        '("" mode-line-modes)))

(defun set-pdf-modeline ()) ; TODO `set-pdf-modeline'


;;
;;; Bootstrap

(size-indication-mode +1) ; filesize in modeline
(add-hook '+doom-dashboard-mode-hook #'set-project-modeline)

;; Other modes
(defun set-modeline-in-magit ()
  (if (eq major-mode 'magit-status-mode)
      (set-project-modeline)
    (hide-mode-line-mode)))
(add-hook 'magit-mode-hook #'set-modeline-in-magit)

(add-hook 'special-mode-hook #'set-special-modeline)
(add-hook 'image-mode-hook #'set-special-modeline)
(add-hook 'circe-mode-hook #'set-special-modeline)
(add-hook 'pdf-tools-enabled-hook #'set-pdf-modeline)

;; (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
;; (setq-default visual-fill-column-center-text nil)


;; Doom
(after! doom-todo-ivy
  (setq doom/ivy-task-tags
        '(("TODO"  . warning)
          ("FIXME" . error))))
;;("NOTE"  . note)

;;------------------------------------------------------------------------------
;; Packages
;;------------------------------------------------------------------------------

;; start:sort                                       ;; (package! company-org-block :pin "4d96750" :recipe (:host github :repo "xenodium/company-org-block"))
;; (package! minibuffer-line)
;;(package! all-the-icons-ivy)
;;(package! ample-regexps)
;;(package! auctex)
;;(package! base16-theme)
;;(package! bespoke-modeline :recipe (:host github :repo "mclear-tools/bespoke-modeline"))
;;(package! bespoke-themes :recipe (:host github :repo "mclear-tools/bespoke-themes"))
;;(package! browse-at-remote)
;;(package! clang-format)
;;(package! clang-format+)
;;(package! company)
;;(package! company-box)
;;(package! company-jedi)
;;(package! company-tabnine)
;;(package! counsel-etags)
;;(package! doom-todo-ivy :recipe (:host github :repo "jsmestad/doom-todo-ivy"))
;;(package! dumb-jump)
;;(package! eaf :recipe (:host github :repo "manateelazycat/emacs-application-framework"))
;;(package! elfeed)
;;(package! elpy)
;;(package! emacs-tree-sitter :recipe  (:host github :repo   "emacs-tree-sitter/elisp-tree-sitter"))
;;(package! esup)
;;(package! etags-table)
;;(package! evil-anzu)
;;(package! ewp :recipe (:host github :repo "larsmagne/ewp"))
;;(package! fira-code-mode)
;;(package! flycheck-clang-tidy)
;;(package! flycheck-projectile :pin "ce6e9e8" :recipe (:host github :repo "nbfalcon/flycheck-projectile"))
;;(package! flymake-json)
;;(package! fzf :recipe (:host github :repo "seenaburns/fzf.el"))
;;(package! good-scroll :recipe (:host github :repo "io12/good-scroll.el"))
;;(package! hl-line :disable t)
;;(package! htmlize :recipe (:host github :repo "hniksic/emacs-htmlize"))
;;(package! infix.el :recipe (:host github :repo "rspeele/infix.el"))
;;(package! irony)
;;(package! leuven-theme)
;;(package! lsp-mode :recipe (:host github :repo "emacs-lsp/lsp-mode"))
;;(package! lsp-pyright)
;;(package! mermaid-mode :recipe (:host github :repo "abrochard/mermaid-mode"))
;;(package! mixed-pitch)
;;(package! mmm-mode)
;;(package! nano-modeline)
;;(package! nano-theme)
;;(package! ob-mermaid :recipe (:host github :repo "arnm/ob-mermaid"))
;;(package! olivetti)
;;(package! org-attach-screenshot)
;;(package! org-bullets)
;;(package! org-gcal :recipe (:host github :repo "kidd/org-gcal.el"))
;;(package! org-roam :recipe (:host github :repo "org-roam/org-roam"))
;;(package! org-roam-server)
;;(package! org-sync)
;;(package! org-table-comment)
;;(package! pcre2el)
;;(package! popup)
;;(package! prism)
;;(package! svg-tag-mode :recipe (:host github :repo "rougier/svg-tag-mode"))
;;(package! theme-changer :recipe (:host github :repo "hadronzoo/theme-changer"))
;;(package! tree-sitter)
;;(package! tree-sitter-langs)
;;(package! undo-hl :recipe (:host github :repo "casouri/undo-hl"))
;;(package! vlf)                          ; view large files
;;(package! vterm)
;; end:sort

  ;; Drag-and-drop to `dired`
  ;; (add-hook 'dired-mode-hook 'org-download-enable)

  ;; (defun org-download-named-screenshot (fname)
  ;;   (interactive "FEnter Filename:")
  ;;   (make-directory (file-name-directory fname) t)
  ;;   (if (functionp org-download-screenshot-method)
  ;;       (funcall org-download-screenshot-method fname)
  ;;     (shell-command-to-string
  ;;      (format org-download-screenshot-method fname)))
  ;;   (org-download-image fname))


  ;;org-download-screenshot-method "spectacle -b -r -o %s"

;; (after! org-attach-screenshot
;;   (setq org-attach-screenshot-command-line "xfce4-screenshooter -r -s %f"))

;; (when init-file-debug
;;   (benchmark-init/deactivate))

;; (defmacro measure-time (&rest body)
;;   "Measure the time it takes to evaluate BODY."
;;   `(let ((time (current-time)))
;;      ,@body
;;      (message "%.06f" (float-time (time-since time)))))

;; (remove-hook 'after-save-hook #'rmail-after-save-hook)
;; (remove-hook 'after-save-hook #'ws-butler-after-save)
;; (remove-hook 'after-change-functions #'ws-butler-after-change)
;; (remove-hook 'find-file-hook #'ws-butler-after-save)

;; (setq doom-incremental-idle-timer 0.1)
;; (setq doom-incremental-first-idle-timer 1.5)

;;; -*- lexical-binding: t; -*-

;;------------------------------------------------------------------------------
;; LSP Mode
;;------------------------------------------------------------------------------

(after! lsp-mode
  (setq lsp-enable-file-watchers nil
        lsp-progress-via-spinner nil
        ccls-sem-highlight-method nil
        lsp-enable-on-type-formatting nil
        lsp-enable-indentation nil
        +format-with-lsp nil)

  ;; lsp-terraform was crashing lsp mode for some reason (tbd)
  (delete 'lsp-terraform lsp-client-packages))

;;------------------------------------------------------------------------------
;; VHDL
;;------------------------------------------------------------------------------
;; vhdl-tool, hdl-checker vhdl-ls ghdl-ls

(after! lsp-mode
  (setq lsp-vhdl-server 'ghdl-ls)
  (let ((exe (cl-case lsp-vhdl-server
               (hdl-checker "hdl_checker")
               (vhdl-tool "vhdl-tool")
               (ghdl-ls "ghdl-ls")
               (vhdl-ls "vhdl_ls"))))
    (setq lsp-vhdl-server-path (executable-find exe))))

(add-hook! 'vhdl-mode-hook #'lsp)

;;------------------------------------------------------------------------------
;; Verilog
;;------------------------------------------------------------------------------

(after! lsp-mode
  ;; https://github.com/chipsalliance/verible/blob/master/verilog/tools/ls/README.md
  (add-to-list 'lsp-language-id-configuration '(verilog-mode . "verilog"))

  ;; (lsp-register-client
  ;;  (make-lsp-client
  ;;   :new-connection (lsp-stdio-connection '("svls"))
  ;;   :major-modes '(verilog-mode)
  ;;   :priority -1))

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection
                     '("verible-verilog-ls" "--rules_config_search" "true"))
    :major-modes '(verilog-mode)
    :priority -1))
  )

;; (after! lsp-mode
;;   (setq lsp-clients-svlangserver-launchConfiguration "/tools/verilator -sv --lint-only -Wall"
;;   lsp-clients-svlangserver-formatCommand "/tools/verible-verilog-format")
;;   )

;; (add-hook! 'verilog-mode-hook (lsp))

;;------------------------------------------------------------------------------
;; Company
;;------------------------------------------------------------------------------

;; (after! company

;;   (setq company-idle-delay 1.0
;;         company-minimum-prefix-length 2
;;         company-icon-size '(auto-scale . 24)
;;         company-backends
;;         '(company-bbdb company-semantic company-cmake company-capf company-clang company-files
;;           (company-dabbrev-code company-gtags company-etags company-keywords)
;;           company-oddmuse company-dabbrev))

;;   ;; (set-company-backend! 'text-mode nil)

;;   (after! org
;;     (set-company-backend! 'org-mode nil)
;;     (set-company-backend! 'org-mode '(company-capf company-files company-yasnippet company-dabbrev)))

;;   (add-hook! 'tcl-mode-hook
;;     (setq company-backends
;;           '((:separate company-dabbrev-code company-capf company-keywords
;;              :with company-yasnippet company-files))))

;;   (after! vhdl-mode
;;     (set-company-backend! 'vhdl-mode nil)
;;     (set-company-backend! 'vhdl-mode
;;                           '(company-capf company-keywords company-dabbrev-code company-yasnippet)))

;;   (after! hog-src-mode
;;     (set-company-backend! 'hog-src-mode nil)
;;     (set-company-backend! 'hog-src-mode 'company-files))

;;   (after! clojure-mode

;;     (add-hook! 'cider-repl-mode-hook #'company-mode)
;;     (add-hook! 'cider-mode-hook #'company-mode)
;;     (add-hook! 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
;;     (add-hook! 'cider-mode-hook #'cider-company-enable-fuzzy-completion)

;;     (defun my-clojure-mode-hook ()
;;       (setq-local company-backends
;;                   '((:separate company-capf company-keywords company-dabbrev-code company-yasnippet company-files))))
;;     (add-hook 'clojure-mode-hook #'my-clojure-mode-hook))

;;   (set-company-backend! 'clojure-mode
;;                         '(:separate company-capf company-keywords company-dabbrev-code company-yasnippet)))

;;------------------------------------------------------------------------------
;; Stuff that doesn't work
;;------------------------------------------------------------------------------

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
;; Desktop files
;;------------------------------------------------------------------------------

;; (let ((desktop-entry (format
;;                         "[Desktop Entry]
;; Name=%s
;; Exec=%s %%U
;; MimeType=application/x-%s
;; Icon=application-x-%s
;; Terminal=false
;; Type=Application
;; Categories=
;; Comment=%s" handler executable handler handler comment)))
;;     (write-region desktop-entry nil (format "~/.local/share/applications/%s.desktop" handler))
;;     )


(defun kill-buffer-if (name)
  (when (get-buffer name)
    (kill-buffer name)))

(kill-buffer-if "*Native-compile-Log*")
(kill-buffer-if "*Async-native-compile-log*")

;; (defun save-window (&rest args)
;;   (lambda (orig-fun &rest args)
;;     (let ((current (selected-window)))
;;       (apply orig-fun args)
;;       (select-window current)))
;;   )

;;   (advice-add 'python-shell-send-buffer :around
;;               (lambda (orig-fun &rest args)

;;                 (call-interactively #'run-python)
;;                 ;; (call-interactively #'run-python)

;;                   (apply orig-fun args)
;; ))

;; (defun run-python-unless (&rest _)
;;     "Run python (unless it is already running)"
;;     (interactive)

;;     (unless (python-shell-get-buffer)
;;       (python-shell-make-comint
;;        (python-shell-calculate-command)
;;        (python-shell-get-process-name t) t)

;; ))


;; (advice-add 'python-shell-send-buffer
;;             :before
;;             (lambda (&rest _)
;;               (call-interactively #'run-python)))

;; (advice-add 'python-shell-send-region
;;             :before
;;             (lambda (&rest _)
;;               (interactive)
;;               (call-interactively #'run-python)))
