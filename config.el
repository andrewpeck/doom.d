;; TODO tabular-like alignment
;; TODO fix completion
;; TODO magit.sh like functionality
;; DONE vim-like buffer while scrolling

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Random
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Increase the amount of data which Emacs reads from the process. Again the emacs default is too low 4k considering that the some of the language server responses are in 800k - 3M range.
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Etags search depth
(setq etags-table-search-up-depth 10)

;;(magit-todos-mode)
;;
(setq frame-title-format
      `((buffer-file-name "%f" "%b")
        ,(format " - emacs %s" emacs-version)))

(after! browse-at-remote
  (add-to-list 'browse-at-remote-remote-type-domains '("gitlab.cern.ch" . "gitlab"))
  )

(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
(setq smartparens-global-mode nil)
(setq smartparens-mode nil)
(setq-default tab-width 2)
(midnight-mode) ;; Clear buffers at midnight

;; Prevents some cases of Emacs flickering
;;(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(setq reftex-toc-split-windows-horizontally t)
(setq reftex-toc-split-windows-fraction 0.15)
(after! latex-mode
  (setq-default TeX-master nil)
  )
(add-hook 'LaTeX-mode-hook (lambda () (reftex-mode 1)))
(add-hook 'reftex-toc-mode-hook (lambda ()
                                  (define-key reftex-toc-mode-map (kbd "<return>") 'reftex-toc-goto-line)))


;; https://abizjak.github.io/emacs/2016/03/06/latex-fill-paragraph.html
(defun line-fill-paragraph (&optional P)
  "When called with prefix argument call `fill-paragraph'.
   Otherwise split the current paragraph into one sentence per line."
  (interactive "P")
  (if (not P)
      (save-excursion
        (let ((fill-column 12345678)) ;; relies on dynamic binding
          (fill-paragraph) ;; this will not work correctly if the paragraph is
          ;; longer than 12345678 characters (in which case the
          ;; file must be at least 12MB long. This is unlikely.)
          (let ((end (save-excursion
                       (forward-paragraph 1)
                       (backward-sentence)
                       (point-marker))))  ;; remember where to stop
            (beginning-of-line)
            (while (progn (forward-sentence)
                          (<= (point) (marker-position end)))
              (just-one-space) ;; leaves only one space, point is after it
              (delete-char -1) ;; delete the space
              (newline)        ;; and insert a newline
              (LaTeX-indent-line) ;; I only use this in combination with late, so this makes sense
              ))))

    ;; otherwise do ordinary fill paragraph
    (fill-paragraph P)))

(add-hook 'LaTex-mode-hook
  ;(lambda () (evil-local-set-key 'normal (kbd "M-q") 'line-fill-paragraph))
  (lambda () (define-key evil-normal-state-local-map (kbd "M-q") 'line-fill-paragraph))
  )
(evil-define-minor-mode-key 'normal 'latex-mode-map (kbd "M-q") #'line-fill-paragraph)
(evil-define-minor-mode-key 'normal 'markdown-mode-map (kbd "M-q") #'line-fill-paragraph)

(defun electric-space () ; Trying to get Emacs to do semantic linefeeds
  (interactive)
  (if (looking-back (sentence-end))
      (insert "\n")
       (self-insert-command 1))
       )

(defvar electric-space-on-p nil)

(defun toggle-electric-space ()
  (interactive)
  (global-set-key
   " "
   (if (setq electric-space-on-p
             (not electric-space-on-p))
       'electric-space
     'self-insert-command)))

;;;;; (setq speedbar-show-unknown-files t) ; show all files
;;;;; (setq speedbar-use-images nil) ; use text for buttons
;;;;;                                         ;(setq sr-speedbar-right-side nil) ; put on left side
;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SLIME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; slime
(after! slime
  (setq inferior-lisp-program "sbcl")
  (setq org-babel-lisp-eval-fn 'slime-eval))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; All the icons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(all-the-icons-ibuffer-mode 1)
(all-the-icons-ivy-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spell-Checking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! writegood
  (writegood-passive-voice-turn-off)
  )

;; Save user defined words to the dictionary
(setq ispell-personal-dictionary "~/.aspell.en.pws")
(defun my-save-word () (interactive)
       (let ((current-location (point)) (word (flyspell-get-word)))
         (when (consp word) (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))
;;(after! evil
(after! evil
  ;;(define-key evil-normal-state-map "zg" 'spell-fu-word-add)
  ;;(define-key evil-normal-state-map "zg" 'spell-fu-word-add)
  (define-key evil-normal-state-map "zg" #'my-save-word )
  (define-key evil-normal-state-map "z=" 'ispell-word)
  )

;; only substitute the 1st match by default (reverse vim behavior)
(after! evil
  (setq     evil-ex-substitute-global t)
  )

;;;;; ;; active Babel languages
;;;;; (org-babel-do-load-languages
;;;;;  'org-babel-load-languages
;;;;;  '((gnuplot . t)))
;;;;; ;; add additional languages with '((language . t)))
;;;;;
;;;;; (defun formatted-copy ()
;;;;;   "Export region to HTML, and copy it to the clipboard."
;;;;;   (interactive)
;;;;;   (save-window-excursion
;;;;;     (let* ((buf (org-export-to-buffer 'html "*Formatted Copy*" nil nil t t))
;;;;;            (html (with-current-buffer buf (buffer-string))))
;;;;;       (with-current-buffer buf
;;;;;         (shell-command-on-region
;;;;;          (point-min)
;;;;;          (point-max)
;;;;;          "textutil -stdin -format html -convert rtf -stdout | pbcopy"))
;;;;;       (kill-buffer buf))))

;; Switch to the new window after splitting
(after! evil
  (setq evil-split-window-below t evil-vsplit-window-right t)
  )

;; Don't add newlines to snippet endings
(add-hook 'snippet-mode-hook
  (lambda () (setq require-final-newline nil))
  )

;; WINDOW TITLE :: https://www.emacswiki.org/emacs/FrameTitle
(setq frame-title-format "%b – Emacs")

;;;;; (after! highlight-indent-guides
;;;;;   (setq highlight-indent-guides-auto-enabled nil)
;;;;;   (setq highlight-indent-guides-responsive "stack")
;;;;;   )

;;;;; (add-hook 'markdown-mode-hook (lambda () (visual-fill-column-mode -1)))


;; Make Fundamental Mode GFM by default
(after! gfm
  ;; MAKE SURE to lazy load this... otherwise it adds 1 second to startup time
  (setq initial-major-mode 'gfm-mode)
  )

;; XML
(add-hook 'nxml-mode-hook
  (setq nxml-child-indent 2 nxml-attribute-indent 2)
  )
;;;;; (add-hook 'nxml-mode-hook (lambda () (visual-fill-column-mode -1)))
;;;;; (defun nxml-pretty-format ()
;;;;;   (interactive)
;;;;;   (save-excursion
;;;;;     (shell-command-on-region (point-min) (point-max) "xmllint --format -" (buffer-name) t)
;;;;;     (nxml-mode)
;;;;;     (indent-region begin end)))
;;;;;
;;;;;

;; Start emacs in full screen by default
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;;;;
;;;;; ;; TRAMP
;;;;; (setq tramp-default-method "ssh")
;;;;;
;;;;; ;; Treat underscore as part of a word to match vim behavior
;;;;; (modify-syntax-entry ?_ "w")
;;;;;
;;;;; ;; Ivy fuzzy search enable
;;;;; (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))

;; Projectile
(after! projectile
  (setq projectile-sort-order 'recently-active)
  )

;;; Scrolling.
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

(after! dired
  (setq dired-listing-switches "-a1vBhl  --group-directories-first")
  )

;; add a margin while scrolling
(setq scroll-margin 5)

;; persistent undo
(setq undo-tree-auto-save-history t)

;; Make evil-mode up/down operate in screen lines instead of logical lines
(define-key evil-motion-state-map "j" 'evil-next-visual-line)
(define-key evil-motion-state-map "k" 'evil-previous-visual-line)

;; Also in visual mode
(define-key evil-visual-state-map "j" 'evil-next-visual-line)
(define-key evil-visual-state-map "k" 'evil-previous-visual-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;------------------------------------------------------------------------------
;; Theme
;;------------------------------------------------------------------------------

(add-to-list 'load-path "~/.doom.d/lisp/")
(add-to-list 'load-path "~/.doom.d/themes/")
(add-to-list 'custom-theme-load-path "~/.doom.d/themes/")

(if (string= (system-name) "larry")
    (setq doom-theme 'doom-one)
  (setq doom-theme 'leuven-summerfruit))

(defun ap/toggle-theme ()
  (interactive)
  (if (eq doom-theme 'leuven-summerfruit)
      ( progn
        (setq highlight-indent-guides-auto-enabled nil)
        (setq highlight-indent-guides-responsive "stack")
        (setq doom-theme 'doom-one)
        (load-theme 'doom-one t)
        (set-face-foreground 'highlight-indent-guides-character-face "#375c3c644822")
        )
    ( progn
      (setq highlight-indent-guides-auto-enabled nil)
      (setq highlight-indent-guides-responsive "stack")
      (setq doom-theme 'leuven-summerfruit)
      (load-theme 'leuven-summerfruit t)
      (set-face-foreground 'highlight-indent-guides-character-face "#efefef")
      )
    )
  )

(map! :leader :desc "Toggle Themes" "t t" #'ap/toggle-theme)

;; Syntax Highlighting
                                        ; enable syntax highlighting for vimrc files
(add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode))
;; tcl mode for xdc files
(add-to-list 'auto-mode-alist '("\\.xdc\\'" . tcl-mode))

;;------------------------------------------------------------------------------
;; Git Gutter
;;------------------------------------------------------------------------------

(after! git-gutter-fringe
  (fringe-mode 6)
  (fringe-helper-define 'git-gutter-fr:added nil
                        "...X...."
                        "...X...."
                        "...X...."
                        "...X...."
                        "XXXXXXXX"
                        "...X...."
                        "...X...."
                        "...X....")

  (fringe-helper-define 'git-gutter-fr:deleted nil
                        "........"
                        "........"
                        "........"
                        "XXXXXXXX"
                        "........"
                        "........"
                        "........"
                        "........")

  (fringe-helper-define 'git-gutter-fr:modified nil
                        "X......."
                        "XXX....."
                        "XXXXX..."
                        "XXXXXXXX"
                        "XXXXXXXX"
                        "XXXXX..."
                        "XXX....."
                        "X.......")
  )

;;------------------------------------------------------------------------------
;; Rainbow Delimeters
;;------------------------------------------------------------------------------

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;------------------------------------------------------------------------------
;; Outline Moode
;;------------------------------------------------------------------------------

;;;;;;; Clean code folding via Outline minor mode.
;;;;;(add-hook 'latex-mode-hook 'outline-minor-mode)

;;;;;;; Show all headings but no content in Outline mode.
;;;;;(add-hook 'outline-minor-mode-hook
;;;;;          (defun baba/outline-overview ()
;;;;;            "Show only outline headings."
;;;;;            (outline-show-all)
;;;;;            (outline-hide-body)))
;;;;;
;;;;;;; TODO: useful things here
;;;;;;; https://www.reddit.com/r/emacs/comments/e2u5n9/code_folding_with_outlineminormode/
;;;;;
;;;;;;; Customize the distracting folding markers.
;;;;;(after! outline-mode
;;;;;  (set-display-table-slot
;;;;;   standard-display-table
;;;;;   'selective-display
;;;;;   (let ((face-offset (* (face-id 'shadow) (lsh 1 22))))
;;;;;     (vconcat (mapcar (lambda (c) (+ face-offset c)) " +"))))
;;;;;  )

;; Mixed Pitch Mode
(add-hook 'org-mode-hook      #'mixed-pitch-mode)
(add-hook 'markdown-mode-hook #'mixed-pitch-mode)
(add-hook 'latex-mode-hook    #'mixed-pitch-mode)

;; FONT
(setq
 ;;doom-font                (font-spec :family "Fira Code" :size 14 :weight 'regular)
 doom-font                (font-spec :family "DejaVu Sans Mono"   :size 14 :weight 'regular )
 doom-variable-pitch-font (font-spec :family "Comic Sans MS"   :size 17 :weight 'regular )

 ;;doom-serif-font (font-spec :family "Comic Sans MS"   :size 17 :weight 'regular )

 ;;doom-variable-pitch-font (font-spec :family "DejaVu Sans"   :size 15 :weight 'bold )
 ;;doom-font                (font-spec :family "JuliaMono" :otf '(zero ss05 ss08) :size 13 :height 9 :weight 'regular)
 ;;doom-variable-pitch-font (font-spec :family "JuliaMono" :otf '(zero ss05 ss08) :size 13 :height 9 :weight 'regular)
 )

;;(setq doom-font                (font-spec :family "Deja Vu Sans Mono" :size 13 :weight 'regular))
;;(setq doom-variable-pitch-font (font-spec :family "Arial" :weight 'regular   :size 15))

;; Disable auto fill mode in doom text modes
(remove-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
;;(add-hook 'text-mode-hook #'visual-line-mode)

(defun ap/no-wrap ()
  (interactive)
  (visual-line-mode 0)
  (toggle-truncate-lines 1)
  (visual-fill-column-mode 0)
  )

(add-hook 'text-mode-hook 'ap/no-wrap)
(add-hook 'prog-mode-hook 'ap/no-wrap)
(add-hook 'org-mode-hook  'ap/no-wrap)
(add-hook 'nxml-mode-hook 'ap/no-wrap)
;;(ap/no-wrap)

(setq-default fill-column 100)

;; Turn on menu bar
(menu-bar-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company Completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'after-init-hook 'global-company-mode)
(setq-default yas-also-auto-indent-first-line t)
;;(defvar +company-backend-alist
;;  '((text-mode company-yasnippet company-dabbrev  company-ispell)
;;    (prog-mode company-yasnippet company-capf )
;;    (conf-mode company-yasnippet company-capf company-dabbrev-code )))

;;(set-company-backend! 'python-mode-hook '(company-yasnippet company-jedi
;;                                       company-files
;;                                      company-keywords company-capf company-dabbrev-code
;;                                      company-etags company-dabbrev))
(add-hook 'python-mode-hook (lambda () (add-to-list 'company-backends 'company-jedi)))

(add-hook 'Latex-mode-hook (lambda () (set-company-backend! 'LaTeX-mode-hook '(company-yasnippet company-reftex company-auctex
                                      company-math company-files
                                      company-keywords company-capf company-dabbrev-code
                                      company-etags company-dabbrev))
))

(after! company
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (setq company-minimum-prefix-length 1)
  (setq company-auto-commit nil)
  (setq company-idle-delay 0.2)
  (setq company-require-match 'never)
  (setq company-frontends '(company-box-frontend company-echo-metadata-frontend))
  (setq-default company-box-show-single-candidate 'always)

  ;;(setq company-frontends '(company-preview-if-just-one-frontend company-box-frontend company-echo-metadata-frontend))
  ;;(setq company-frontends '(company-preview-if-just-one-frontend  company-echo-metadata-frontend))
  ;;(setq company-frontends '(company-box-frontend company-echo-metadata-frontend))
  ;;(add-to-list 'company-backends 'company-irony)

  ;;(define-key company-active-map (kbd "<return>") nil)
  ;;(define-key company-active-map (kbd "<tab>") #'company-complete-selection )
  (set-company-backend! 'org-mode '(company-yasnippet company-tabnine
                                    company-files company-keywords company-capf))
  (set-company-backend! '(prog-mode tcl-mode python-mode vhdl-mode)
    '(company-yasnippet company-keywords company-capf company-files
                        company-dabbrev-code company-etags company-dabbrev ))

  (add-hook 'company-mode-hook 'company-box-mode)

  (setq-default company-box-backends-colors 'nil)

  ;;   (set-company-backend! '(company-tabnine company-yasnippet  company-files))
  ;;                                         ;(set-company-backend! 'org-mode '(company-roam company-files company-dabbrev))
  ;;   ;;(set-company-backend! '(tcl-mode) '(company-tabnine company-yasnippet))
  ;;   (set-company-backend! '(prog-mode) '(company-tabnine company-yasnippet))
  ;;   )

  )

(setq-default magit-diff-refine-hunk 'all)
;;(setq magit-repository-directories '(("~/" . 1)))
;; Doom
;; Keymappings
(after! doom-todo-ivy
  (setq doom/ivy-task-tags '(
                             ("TODO"  . warning)
                             ("FIXME" . error)
                             ;;("NOTE"  . note)
                             )))
;; Dashboard
(defun peck-dashboard-widget-banner ()
  (let ((point (point)))
    (mapc (lambda (line)
            (insert (propertize (+doom-dashboard--center +doom-dashboard--width line)
                                'face 'doom-dashboard-banner) " ")
            (insert "\n"))
          '(" emacs "))
    (when (and (display-graphic-p)
               (stringp fancy-splash-image)
               (file-readable-p fancy-splash-image))
      (let ((image (create-image (fancy-splash-image-file))))
        (add-text-properties
         point (point) `(display ,image rear-nonsticky (display)))
        (save-excursion
          (goto-char point)
          (insert (make-string
                   (truncate
                    (max 0 (+ 1 (/ (- +doom-dashboard--width
                                      (car (image-size image nil)))
                                   2))))
                   ? ))))
      (insert (make-string (or (cdr +doom-dashboard-banner-padding) 0)
                           ?\n)))))

(setq +doom-dashboard-functions
      '(peck-dashboard-widget-banner
        doom-dashboard-widget-shortmenu
        doom-dashboard-widget-loaded
        doom-dashboard-widget-footer))

;; Alignment functions
;; (defun align-to-colon (begin end)
;;   "Align region to colon (:) signs"
;;   (interactive "r")
;;   (align-regexp begin end
;;                 (rx (group (zero-or-more (syntax whitespace))) ":") 1 1 ))
;;
;; (defun align-to-comma (begin end)
;;   "Align region to comma  signs"
;;   (interactive "r")
;;   (align-regexp begin end
;;                 (rx "," (group (zero-or-more (syntax whitespace))) ) 1 1 t ))
;;
;; (defun bjm/align-& (start end)
;;   "Align columns by ampersand"
;;   (interactive "r")
;;   (align-regexp start end
;;                 "\\(\\s-*\\)&" 1 1 t))
;;
;; ;; http://pragmaticemacs.com/emacs/aligning-text/
;; (defun bjm/align-comma (start end)
;;   "Align columns by ampersand"
;;   (interactive "r")
;;   (align-regexp start end
;;                 "\\(\\s-*\\),\\(\\s-*\\)" 1 1 t))
;;
;; (defun align-to-equals (begin end)
;;   "Align region to equal signs"
;;   (interactive "r")
;;   (align-regexp begin end
;;                 (rx (group (zero-or-more (syntax whitespace))) "=") 1 1 ))
;;
;; (defun align-to-hash (begin end)
;;   "Align region to hash ( => ) signs"
;;   (interactive "r")
;;   (align-regexp begin end
;;                 (rx (group (zero-or-more (syntax whitespace))) "=>") 1 1 ))
;;
;; ; (defun align-regexp (beg end regexp &optional group spacing repeat)
;; ;; work with this
;; (defun align-to-comma-before (begin end)
;;   "Align region to equal signs"
;;   (interactive "r")
;;   (align-regexp begin end
;;                 (rx (group (zero-or-more (syntax whitespace))) ",") 1 1 t))
;;
;; ;; https://www.reddit.com/r/emacs/comments/6pak1o/configuration_for_alignment_commands/
;; (defun align-whitespace (start end)
;;   "Align columns by whitespace"
;;   (interactive "r")
;;   (align-regexp start end
;;                 "\\(\\s-*\\)\\s-" 1 0 t))

(defun align-whitespace (start end)
  "Align columns by whitespace"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\s-" 1 0 t))

;;;###autoload
;;(defun ap/align (start end x)
;;  "Align"
;;  (interactive
;;   (let ((string (read-string "Foo: " nil 'my-history)))
;;     ;;(list (region-beginning) (region-end) string)
;;     (align-regexp (region-beginning) (region-end) "\\(\\s-*\\) &" 1 1 t)
;;
;;     ))

  ;;(interactive
  ;; (let ((string (read-string "Align regexp: ")))
  ;;   ;;(print start)
  ;;   ;;(print end)
  ;;   (print x)
  ;;   ;;(align-regexp start end "\\(\\s-*\\)                   &") 1 1 t))
  ;;   )
  ;; )
 ;; )

;;(evil-ex-define-cmd "Tab[ular]" 'ap/align)

(defun align-ampersand (start end)
  "Align columns by ampersand"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)&" 1 1 t))

(defun align-quote-space (start end)
  "Align columns by quote and space"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\).*\\s-\"" 1 0 t))

(defun align-equals (start end)
  "Align columns by equals sign"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)=" 1 0 t))

(defun align-comma (start end)
  "Align columns by comma"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)," 1 1 t))

(defun align-dot (start end)
  "Align columns by dot"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\\." 1 1 t))

;; Evil Bindings

(global-evil-leader-mode)

;; unbind annoying emacs bindings
(define-key evil-normal-state-map "\C-p" nil)
(define-key evil-normal-state-map "\C-d" nil)
(global-set-key (kbd "M-`") nil)

;;------------------------------------------------------------------------------
;; 1
;;------------------------------------------------------------------------------

(after! evil-maps
  (define-key evil-normal-state-map (kbd "C-o")
    (lambda () (interactive)
      (counsel-fzf "" "~/")))
  (define-key evil-normal-state-map (kbd "C-p")
    (lambda () (interactive)
      (counsel-fzf "" (projectile-project-root))))
  (define-key evil-normal-state-map (kbd "C-n")
    (lambda () (interactive)
      (counsel-fzf "" "~/Dropbox/notes")))

  ;; Ctrl + Alt + equal to re-indent buffer
  (after! evil
    (define-key evil-normal-state-map (kbd "C-M-=") (kbd "mmgg=G`m")))

  (evil-leader/set-key "tt" 'doom/ivy-tasks)
  (evil-leader/set-key "f" 'searchdirs)
  (evil-leader/set-key "x" 'counsel-M-x) ;; leader x for helm execute
  (evil-leader/set-key "pp" '+ivy/project-search)
  (evil-leader/set-key "rr" 'projectile-recentf)
  (evil-leader/set-key "rtw" 'delete-trailing-whitespace) ;; delete trailing whitespace
  (evil-leader/set-key "g" 'magit-status)
  (evil-leader/set-key "E" 'dired-jump) ;; map dired to leader
  (evil-leader/set-key "pp" 'org-publish-current-project)
  ;;(evil-leader/set-key "rr" 'fzf-recentf)
  ;;(evil-leader/set-key "S" 'magit-stage-file)
  ;;(evil-leader/set-key "er" 'eval-region)
  ;;(evil-leader/set-key "f" 'helm-locate)
  ;;(evil-leader/set-key "b" 'helm-mini) ;; map helm mini to leader
  ;;(evil-leader/set-key "tag" 'projectile-regenerate-tags)

  (defun browse-file-directory ()
    "Open the current file's directory however the OS would."
    (interactive)
    (if default-directory
        (browse-url-of-file (expand-file-name default-directory))
      (error "No `default-directory' to open")))
  (evil-leader/set-key "bf" 'browse-file-directory)

  ;; Ctrl-P
  ;;(bind-key* "C-p" 'projectile-find-file)

  ;; let enter open org mode links
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "RET") 'org-open-at-point)
  (define-key evil-motion-state-map (kbd "TAB") nil)
  (setq org-return-follows-links t)

  ;; Backspace to switch to last buffer
  (defun er-switch-to-previous-buffer ()
    "Switch to previously open buffer. Repeated invocations toggle between the two most recently open buffers."
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) 1)))
  (define-key evil-normal-state-map (kbd "DEL") 'er-switch-to-previous-buffer)
  (add-hook 'verilog-mode-hook (lambda() (local-unset-key [backspace])))

  (define-key evil-normal-state-map (kbd "C-a")   'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-S-a") 'evil-numbers/dec-at-pt)
  (define-key evil-visual-state-map (kbd "C-a")   'evil-numbers/inc-at-pt-incremental)
  (define-key evil-visual-state-map (kbd "C-S-a") 'evil-numbers/dec-at-pt-incremental)

  (define-key evil-normal-state-map (kbd "C-t") 'evil-jump-backward)
  )

;;------------------------------------------------------------------------------
;; True/False Faces
;;------------------------------------------------------------------------------

(defface my/highlight-true-face
  '((t :foreground "#2a2" :weight bold))
  "Highlight face for true")

(defface my/highlight-false-face
  '((t :foreground "#f22" :weight bold))
  "Highlight face for false")

(defun my/highlight-true ()
  "Use hi-lock to highlight specific words"
  (hi-lock-face-buffer "\\b\\(true\\|True\\)\\b" 'my/highlight-true-face))

(defun my/highlight-false ()
  "Use hi-lock to highlight specific words"
  (hi-lock-face-buffer "\\b\\(false\\|False\\)\\b" 'my/highlight-false-face))

(add-hook 'vhdl-mode-hook #'my/highlight-false)
(add-hook 'vhdl-mode-hook #'my/highlight-true)
(add-hook 'python-mode-hook #'my/highlight-false)
(add-hook 'python-mode-hook #'my/highlight-true)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VHDL Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; vhdl mode will wrap comments after some # of characters
(setq vhdl-end-comment-column 200)
(setq vhdl-prompt-for-comments nil)
(setq auto-fill-mode nil)

;;------------------------------------------------------------------------------
;; VHDL Flycheck
;;------------------------------------------------------------------------------

;;;;;(after! flycheck
;;;;;  (setq-default flycheck-ghdl-language-standard "08")
;;;;;  (setq-default flycheck-ghdl-workdir           "~/ghdl")
;;;;;  (setq-default flycheck-ghdl-ieee-library      "synopsys")
;;;;;)

;;------------------------------------------------------------------------------
;; LSP
;;------------------------------------------------------------------------------

(after! lsp-ui
  (setq-default lsp-headerline-breadcrumb-enable t)
  (setq-default lsp-ui-doc-enable t)
  (setq-default lsp-ui-doc-show-with-mouse t)
  (setq-default lsp-ui-doc-show-with-cursor nil)
  (setq-default lsp-ui-doc-max-width 150)
  (setq-default lsp-ui-sideline-show-hover nil)
  (setq-default lsp-ui-sideline-enable nil)
  (setq-default lsp-ui-doc-delay 0.1)
  )

(setq lsp-completion-provider :capf)

(add-hook 'python-mode-hook #'lsp-mode)
(add-hook 'python-mode-hook #'lsp-ui-mode)
(add-hook 'vhdl-mode-hook #'lsp)
(add-hook 'vhdl-mode-hook #'lsp-ui-mode)

(after! lsp
    (setq lsp-enabled-clients nil)
)

;; VHDL Tool
(setq lsp-vhdl-server 'vhdl-tool)
(setq lsp-vhdl-server-path "~/bin/vhdl-tool")

;; HDL Checker
;;(setq lsp-vhdl-server 'hdl-checker)

;; VHDL Ls (Rust HDL)
;;(setq lsp-vhdl-server 'vhdl-ls)
;;(setq lsp-vhdl-server-path "~/rust_hdl/target/release/vhdl_ls")

;;(use-package lsp-mode
;;         :config
;;         (add-hook 'vhdl-mode-hook 'lsp))

;; (flycheck-define-checker vhdl-tool
;;   "A VHDL syntax checker, type checker and linter using VHDL-Tool.
;;         See URL `http://vhdltool.com'."
;;   :command ("vhdl-tool" "client" "lint" "--compact" "--stdin" "-f" source
;;             )
;;   :standard-input t
;;   :error-patterns
;;   ((warning line-start (file-name) ":" line ":" column ":w:" (message) line-end)
;;    (error line-start (file-name) ":" line ":" column ":e:" (message) line-end))
;;   :modes (vhdl-mode))
;;
;; (add-to-list 'flycheck-checkers 'vhdl-tool)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Latex Export
(setq user-full-name "A.P.")
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("article" "\\documentclass[11pt]{article}
             \\usepackage[utf8]{inputenc}
             \\usepackage[T1]{fontenc}
             \\usepackage{fixltx2e}
             \\usepackage{fullpage}
             \\usepackage{graphicx}
             \\usepackage{longtable}
             \\usepackage{float}
             \\usepackage{wrapfig}
             \\usepackage{rotating}
             \\usepackage[normalem]{ulem}
             \\usepackage{amsmath}
             \\usepackage{textcomp}
             \\usepackage{marvosym}
             \\usepackage{wasysym}
             \\usepackage{amssymb}
             \\usepackage{hyperref}
             \\usepackage{mathpazo}
             \\usepackage{color}
             \\usepackage{enumerate}
             \\definecolor{bg}{rgb}{0.95,0.95,0.95}
             \\tolerance=1000
             [NO-DEFAULT-PACKAGES]
             [PACKAGES]
             [EXTRA]
             \\linespread{1.1}
             \\hypersetup{pdfborder=0 0 0}"
                 ("\\section{%s}"       . "\\section*{%s}")
                 ("\\subsection{%s}"    . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}"     . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}"  . "\\subparagraph*{%s}"))
               )
)

;;------------------------------------------------------------------------------
;; Appearance
;;------------------------------------------------------------------------------

(add-hook 'org-mode-hook
;;(define-key evil-normal-state-map "zi" #'org-toggle-inline-images)
  (lambda () (define-key evil-normal-state-map "zs" #'org-toggle-link-display))
)

(after! org

  (add-to-list 'load-path "~/Dropbox/org")
  ;(mapc 'load
  ;      '("org-sync" "org-sync-bb" "org-sync-github" "org-sync-gitlab"))

  (defun sort-all-org-entries ()
    (interactive)
    (let ((fun #'(lambda nil
                   (condition-case nil
                       (org-sort-entries nil ?o)
                     (user-error t)))))
      (org-map-entries fun)))

  ;; Latex Previews
  (after! org
        (setq org-preview-latex-default-process 'dvisvgm)
        (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  )

  ;;------------------------------------------------------------------------------
  ;; Toggle Displays
  ;;------------------------------------------------------------------------------

  (setq org-startup-folded 'f)
  (setq org-startup-with-inline-images t)
  (org-display-inline-images t t)

  ;; Allow M-Ret to split list items
  (setq org-M-RET-may-split-line t)
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  ;; Org mode plain list bullets
  (font-lock-add-keywords
   'org-mode
   '(("^[[:space:]]*\\(-\\) "
      0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "  ⚫ ")))))
  )


;;

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

;;------------------------------------------------------------------------------
;; Evil Surround
;;------------------------------------------------------------------------------

;;(after! evil
;;  (add-hook 'org-mode-hook (lambda ()
;;                             (push '(?< . ("~" . "~")) evil-surround-pairs-alist)
;;                             (push '(?< . ("=" . "=")) evil-surround-pairs-alist)
;;                             (push '(?< . ("*" . "*")) evil-surround-pairs-alist)
;;                             (push '(?< . ("/" . "/")) evil-surround-pairs-alist)
;;                             )))

;;------------------------------------------------------------------------------
;; Notes
;;------------------------------------------------------------------------------

(add-hook 'evil-org-agenda-mode-hook
  'evil-org-agenda-set-keys
  )

(after! org

  (setq org-log-done 'time)

  (setq org-link-file-path-type 'relative)
  (setq org-agenda-files (list "~/Dropbox/org"))
  (setq org-id-locations-file "~/Dropbox/org/.org-id-locations")
  (setq org-hide-emphasis-markers t)
  (setq org-export-with-sub-superscripts nil)
  (setq org-directory "~/Dropbox/org")
  (setq org-default-notes-file (concat org-directory "/todo.org"))
  ;; https://github.com/sk8ingdom/.emacs.d/blob/master/org-mode-config/org-capture-templates.el
  ;; https://cestlaz.github.io/posts/using-emacs-26-gcal/
  (setq org-capture-templates '(
          ("t" "TODO" entry (file+headline +org-capture-todo-file "To do")
           "** TODO %?" :prepend t)
          ("a" "Appointment" entry (file  "~/Dropbox/org/gcal-peck.org" )
           "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
          ("s" "Shopping" item (file+headline +org-capture-todo-file "Shopping")
           "- [ ] %?" :prepend t)
          ))

  ;;
  (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))
  (add-to-list 'org-file-apps '("\\.odt\\'" . "xdg-open %s"))

  )

;; SciMax Org Return
;;;;;;; http://kitchingroup.cheme.cmu.edu/blog/2017/04/09/A-better-return-in-org-mode/
;;;;;(require 'org-inlinetask)
;;;;;(defun scimax/org-return (&optional ignore)
;;;;;  "Add new list item, heading or table row with RET.
;;;;;A double return on an empty element deletes it.
;;;;;Use a prefix arg to get regular RET. "
;;;;;  (interactive "P")
;;;;;  (if ignore
;;;;;      (org-return)
;;;;;    (cond
;;;;;
;;;;;     ((eq 'line-break (car (org-element-context)))
;;;;;      (org-return-indent))
;;;;;
;;;;;     ;; Open links like usual, unless point is at the end of a line.
;;;;;     ;; and if at beginning of line, just press enter.
;;;;;     ((or (and (eq 'link (car (org-element-context))) (not (eolp)))
;;;;;          (bolp))
;;;;;      (org-return))
;;;;;
;;;;;     ;; It doesn't make sense to add headings in inline tasks. Thanks Anders
;;;;;     ;; Johansson!
;;;;;     ((org-inlinetask-in-task-p)
;;;;;      (org-return))
;;;;;
;;;;;     ;; checkboxes too
;;;;;     ((org-at-item-checkbox-p)
;;;;;      (org-insert-todo-heading nil))
;;;;;
;;;;;     ;; lists end with two blank lines, so we need to make sure we are also not
;;;;;     ;; at the beginning of a line to avoid a loop where a new entry gets
;;;;;     ;; created with only one blank line.
;;;;;     ((org-in-item-p)
;;;;;      (if (save-excursion (beginning-of-line) (org-element-property :contents-begin (org-element-context)))
;;;;;          (org-insert-heading)
;;;;;        (beginning-of-line)
;;;;;        (delete-region (line-beginning-position) (line-end-position))
;;;;;        (org-return)))
;;;;;
;;;;;     ;; org-heading
;;;;;     ((org-at-heading-p)
;;;;;      (if (not (string= "" (org-element-property :title (org-element-context))))
;;;;;          (progn (org-end-of-meta-data)
;;;;;                 (org-insert-heading-respect-content)
;;;;;                 (outline-show-entry))
;;;;;        (beginning-of-line)
;;;;;        (setf (buffer-substring
;;;;;               (line-beginning-position) (line-end-position)) "")))
;;;;;
;;;;;     ;; tables
;;;;;     ((org-at-table-p)
;;;;;      (if (-any?
;;;;;           (lambda (x) (not (string= "" x)))
;;;;;           (nth
;;;;;            (- (org-table-current-dline) 1)
;;;;;            (org-table-to-lisp)))
;;;;;          (org-return)
;;;;;        ;; empty row
;;;;;        (beginning-of-line)
;;;;;        (setf (buffer-substring
;;;;;               (line-beginning-position) (line-end-position)) "")
;;;;;        (org-return)))
;;;;;
;;;;;     ;; fall-through case
;;;;;     (t
;;;;;      (org-return)))))
;;;;;
;;;;;
;;;;;(define-key org-mode-map (kbd "RET")
;;;;;  'scimax/org-return)

;;------------------------------------------------------------------------------
;; Org Image Attach
;;------------------------------------------------------------------------------

;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)

;; Org mode download images
;;(use-package! org-download
;;  :config
(setq org-attach-id-dir "./images/screenshots")
;;(add-hook! 'org-mode-hook (lambda () ))

(after! org
  (map! :leader
        :prefix "ma"
        :desc "Download Screenshot" "c" #'org-download-screenshot
        :desc "Download Clipboard" "p" #'org-download-clipboard
        :desc "Download Yank" "P" #'org-download-yank
        )
  )

(after! org-download

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
                org-download-screenshot-method "xfce4-screenshooter -r -s %s"
                org-download-image-dir         "./images/screenshots"
                org-download-heading-lvl       0
                org-download-link-format       "[[file:%s]]\n"
                ;;org-download-image-attr-list   ("#+attr_org: :width 800px")
                org-download-annotate-function (lambda (link) "")
                org-download-image-org-width   1000
                )
  )

(after! org-attach-screenshot
  (setq org-attach-screenshot-command-line "xfce4-screenshooter -r -s %f")
  )


;; Org publishing
;;;;;(after! org
;;;;;  (setq org-list-allow-alphabetical t)
;;;;;  (setq org-publish-project-alist
;;;;;        '(
;;;;;          ;; ... add all the components here (see below)...
;;;;;          ("org-notes"
;;;;;           :base-directory "~/Dropbox/notes/"
;;;;;           :base-extension "org"
;;;;;           :publishing-directory "~/notes_html/"
;;;;;           :recursive t
;;;;;           :publishing-function org-html-publish-to-html
;;;;;           :headline-levels 4  ; Just the default for this project.
;;;;;           :auto-preamble t
;;;;;           )
;;;;;
;;;;;          ("org-static"
;;;;;           :base-directory "~/Dropbox/notes/"
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

;;------------------------------------------------------------------------------
;; Org roam
;;------------------------------------------------------------------------------

(after! org
        (require 'org-download)
              (setq org-roam-directory "~/Dropbox/notes/")
              (setq org-roam-graph-extra-config '(("rankdir" . "RL")))
              (setq org-roam-graph-edge-extra-config '(("dir" . "back")))
              (map! :leader
                    :prefix "n"
                    :desc "Org-Roam-Insert" "i" #'org-roam-insert
                    :desc "Org-Roam-Find"   "/" #'org-roam-find-file
                    :desc "Org-Roam-Buffer" "r" #'org-roam
                    :desc "Org-Roam-Show-Graph" "g" #'org-roam-show-graph
                    )

                                        ;(setq org-roam-link-title-format "Org:%s")
              (setq org-roam-db-location "~/Dropbox/notes/org-roam.db")
              (setq org-roam-backlinks-mode-hook
                    '(
                      (flyspell-mode)
                      (define-key evil-motion-state-map (kbd "RET") 'org-roam-open-at-point)
                      )
                    )

              (setq org-roam-completion-system 'ivy)

              (setq org-roam-capture-templates
                    '(("d" "default" plain (function org-roam--capture-get-point)
                       "%?"
                       :file-name "${title}"
                       :head "#+SETUPFILE: \"org.setup\"\n#+TITLE: ${title}\n#"

                       :unnarrowed t))
                    )
              )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mu4e
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;(after! mu4e
;;;;;  (setq mail-user-agent 'mu4e-user-agent)
;;;;;  (setq mu4e-drafts-folder "/[Gmail].Drafts")
;;;;;  (setq mu4e-sent-folder   "/[Gmail].Sent Mail")
;;;;;  (setq mu4e-trash-folder  "/[Gmail].Trash")
;;;;;  (setq mu4e-sent-messages-behavior 'delete)
;;;;;  (setq mu4e-maildir-shortcuts
;;;;;        '( ("/INBOX"               . ?i)
;;;;;           ("/[Gmail].Sent Mail"   . ?s)
;;;;;           ("/[Gmail].Trash"       . ?t)
;;;;;           ("/[Gmail].All Mail"    . ?a)))
;;;;;  (setq mu4e-get-mail-command "offlineimap")
;;;;;  (setq mu4e-use-fancy-chars t)
;;;;;  (setq mu4e-view-show-addresses t)
;;;;;  (setq mu4e-view-show-images t)
;;;;;  (add-hook 'mu4e-view-mode-hook #'visual-line-mode)
;;;;;  (add-hook 'mu4e-compose-mode-hook 'flyspell-mode)
;;;;;)


;; IELM
;;
;; remember ielm history
;; global copy of the buffer-local variable
(defvar ielm-comint-input-ring nil)

(defun set-ielm-comint-input-ring ()
  ;; create a buffer-local binding of kill-buffer-hook
  (make-local-variable 'kill-buffer-hook)
  ;; save the value of comint-input-ring when this buffer is killed
  (add-hook 'kill-buffer-hook #'save-ielm-comint-input-ring)
  ;; restore saved value (if available)
  (when ielm-comint-input-ring
    (message "Restoring comint-input-ring...")
    (setq comint-input-ring ielm-comint-input-ring)))

(defun save-ielm-comint-input-ring ()
  (message "Saving comint-input-ring...")
  (setq ielm-comint-input-ring comint-input-ring))

(require 'ielm)
(add-hook 'inferior-emacs-lisp-mode-hook #'set-ielm-comint-input-ring)
