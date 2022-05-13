;; config.el -*- lexical-binding: t; -*-
;;
;; https://github.com/alphapapa/taxy.el
;; https://svn.red-bean.com/repos/kfogel/trunk/.emacs
;;
;; TODO tabular-like alignment
;; TODO magit.sh like functionality
;; TODO outline folding
;;
;; Tecosaur: https://github.com/tecosaur/emacs-config/blob/master/config.org
;; Steve Purcell: https://github.com/purcell/emacs.d/
;; Henrik: https://github.com/hlissner/doom-emacs-private/blob/master/config.el

;; Bookmarks
(load! "~/.doom.d/custom.el")
;; disable confusing undo-fu behavior
;; https://gitlab.com/ideasman42/emacs-undo-fu/-/issues/6
(after! undo-fu
  (setq undo-fu-ignore-keyboard-quit t))

;;------------------------------------------------------------------------------
;; Loads
;;------------------------------------------------------------------------------

(add-to-list 'load-path (expand-file-name "~/.doom.d/"))
(add-to-list 'load-path (expand-file-name "~/.doom.d/lisp/"))
(add-to-list 'load-path (expand-file-name "~/.doom.d/lisp/hog-emacs/"))
(add-to-list 'load-path (expand-file-name "~/.doom.d/lisp/doctor/"))
(add-to-list 'load-path (expand-file-name "~/.doom.d/lisp/hdl_deps/"))

(let ((default-directory (expand-file-name "lisp" doom-private-dir)))
  (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'load-path (expand-file-name "~/.doom.d/scad-preview/"))
;; https://github.com/hlissner/doom-emacs/issues/1213

(if (string= (system-name) "larry")
    (load! "~/.doom.d/config-mail.el"))

(use-package! doctor :ensure nil :load-path "~/.doom.d/lisp/doctor.el")
(use-package! hdl_deps :ensure nil :load-path "~/.doom.d/lisp/hdl_deps/hdl_deps.el")
(use-package! hog :ensure nil :load-path "~/.doom.d/lisp/hog-emacs/hog.el")
(use-package! regulator :ensure nil :load-path "~/.doom.d/lisp/regulator.el")
(use-package! doctor :ensure nil :load-path "~/.doom.d/lisp/doctor/doctor.el")
(use-package! system-install :ensure nil :load-path "~/.doom.d/lisp/system-install.el")
(use-package! tracking :ensure nil :load-path "~/.doom.d/lisp/tracking.el")
(use-package! ucf-mode :ensure nil :load-path "~/.doom.d/lisp/ucf-mode.el")
(use-package! vivado-mode :ensure nil :load-path "~/.doom.d/lisp/vivado-mode.el")

(require 'undo-hl)
(add-hook 'text-mode-hook #'undo-hl-mode)
(add-hook 'prog-mode-hook #'undo-hl-mode)

;; start:sort
(load! "~/.doom.d/config-align.el")
(load! "~/.doom.d/config-appearance.el")
(load! "~/.doom.d/config-company.el")
(load! "~/.doom.d/config-doom.el")
(load! "~/.doom.d/config-evil.el")
(load! "~/.doom.d/config-flycheck.el")
(load! "~/.doom.d/config-git.el")
(load! "~/.doom.d/config-lsp.el")
(load! "~/.doom.d/config-org.el")
(load! "~/.doom.d/config-random.el")
(load! "~/.doom.d/config-scad.el")
(load! "~/.doom.d/config-tex.el")
(load! "~/.doom.d/lisp/tracking.el")
(load! "~/.doom.d/lisp/work-plotting.el")
;; end:sort

;; (load! "~/.doom.d/config-modeline.el")
;; (load! "~/.doom.d/lisp/regulator.el")
;; (load! "~/.doom.d/lisp/system-install.el")
;; (load! "~/.doom.d/lisp/ucf-mode.el")
;; (load! "~/.doom.d/lisp/verilog-port-copy.el")
;; (load! "~/.doom.d/lisp/vivado-mode.el")

;;------------------------------------------------------------------------------
;; Backups
;;------------------------------------------------------------------------------

;; https://www.emacswiki.org/emacs/BackupDirectory
;; https://stackoverflow.com/questions/3893727/setting-emacs-tramp-to-store-local-backups
;; https://www.emacswiki.org/emacs/ForceBackups

(setq backup-each-save-mirror-location "~/emacs-backups")
(add-hook 'after-save-hook 'backup-each-save)

;;------------------------------------------------------------------------------
;; SLIME
;;------------------------------------------------------------------------------

;; slime
;;(after! slime
;;  (setq inferior-lisp-program "sbcl")
;;  (setq org-babel-lisp-eval-fn 'slime-eval))

;;------------------------------------------------------------------------------
;; Spell-Checking
;;------------------------------------------------------------------------------

(after! writegood
  (writegood-passive-voice-turn-off))

;;------------------------------------------------------------------------------
;; Snippets
;;------------------------------------------------------------------------------

;; Don't add newlines to snippet endings
(after! yasnippet
  (setq-default yas-also-auto-indent-first-line t)
  (add-hook 'snippet-mode-hook
            (lambda () (setq require-final-newline nil))))

;;------------------------------------------------------------------------------
;; Mixed Pitch Mode
;;------------------------------------------------------------------------------

;; (add-hook 'org-mode-hook      #'mixed-pitch-mode)
;; (add-hook 'markdown-mode-hook #'mixed-pitch-mode)
;; (add-hook 'latex-mode-hook    #'mixed-pitch-mode)

;;------------------------------------------------------------------------------
;; Line wrapping
;;------------------------------------------------------------------------------

(defun ap/no-wrap ()
  (interactive)
  (visual-line-mode 0)
  (toggle-truncate-lines 1)
  (visual-fill-column-mode 0))

;; Disable auto fill mode in text modes
(remove-hook 'text-mode-hook #'auto-fill-mode)

;; Don't wrap text modes unless we really want it
(remove-hook 'text-mode-hook #'+word-wrap-mode)

(defun fix-visual-fill-column-mode (&optional ARG)
  (setq visual-fill-column-mode visual-line-mode))

;; toggle visual-fill column mode when chaing word wrap settings
(advice-add '+word-wrap-mode
            :after 'fix-visual-fill-column-mode)
;;
(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)

;;------------------------------------------------------------------------------
;; Hog
;;------------------------------------------------------------------------------

(after! hog
  (cond

   ((string= (system-name) "strange")
    (progn (setq hog-vivado-path "~/Xilinx/Vivado/2020.2/settings64.sh")
           (setq hog-number-of-jobs 16)))

   ((string= (system-name) "larry")
    (progn (setq hog-vivado-path "~/Xilinx/Vivado/2020.2/settings64.sh")
           (setq hog-number-of-jobs 4)))))

;;------------------------------------------------------------------------------
;;; VHDL Mode
;;------------------------------------------------------------------------------

;; vhdl mode will wrap comments after some # of characters
(setq vhdl-end-comment-column 200
      vhdl-prompt-for-comments nil
      auto-fill-mode nil)

;;-----------------------------------------------------------------------------------------
;;; Elfeed
;;------------------------------------------------------------------------------

(after! elfeed
  (setq
   elfeed-feeds
   '("https://hackaday.com/blog/feed/")))

;;-----------------------------------------------------------------------------------------
;;; IELM
;;------------------------------------------------------------------------------

;; remember ielm history
;; global copy of the buffer-local variable
(after! ielm
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
  (add-hook 'inferior-emacs-lisp-mode-hook #'set-ielm-comint-input-ring))

;;-----------------------------------------------------------------------------------------
;;; XML
;;------------------------------------------------------------------------------

(after! nxml
  (add-hook
   'nxml-mode-hook
   (setq nxml-child-indent 2 nxml-attribute-indent 2)))

;; (add-hook 'nxml-mode-hook (lambda () (visual-fill-column-mode -1)))
;; (defun nxml-pretty-format ()
;;   (interactive)
;;   (save-excursion
;;     (shell-command-on-region (point-min) (point-max)
;;     "xmllint --format -" (buffer-name) t)
;;     (nxml-mode)
;;     (indent-region begin end)))

;;-----------------------------------------------------------------------------------------
;;; Markdown
;;------------------------------------------------------------------------------

;; Make Fundamental Mode GFM by default
(after! gfm
  (setq initial-major-mode 'gfm-mode))

;;-----------------------------------------------------------------------------------------
;; C mode
;;-----------------------------------------------------------------------------------------

;; For example, if you prefer double slashes // instead of slash-stars /* ... */
;; in c-mode, insert below code into your ~/.emacs:
(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Preferred comment style
            (setq comment-start "// " comment-end "")))

;;-----------------------------------------------------------------------------------------
;; Clojure
;;-----------------------------------------------------------------------------------------

(setq org-babel-clojure-backend "cider")
;; http://blogs.fluidinfo.com/terry/2011/11/10/emacs-buffer-mode-histogram/

;;-----------------------------------------------------------------------------------------
;; Buffer Mode Histogram
;;-----------------------------------------------------------------------------------------

(defun buffer-mode-histogram ()
  "Display a histogram of emacs buffer modes."
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

;;------------------------------------------------------------------------------
;; Functions for alphabetically sorting items
;;------------------------------------------------------------------------------

(defun sort-code-block (comment-char)
  "Alphabetically sorts code blocks in a file, starting with #
start:sort and ending with # end:sort, where # is the comment
char of the language you are editing"
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
      (goto-char home)))
  ;; make sure to return nil here for write-contents-functions
  nil)

(defun sort-elisp-block ()
  (interactive)
  (sort-code-block ";;"))

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

;;
;; Local Variables:
;; eval: (make-variable-buffer-local 'write-contents-functions)
;; eval: (add-hook 'write-contents-functions 'sort-elisp-block nil t)
;; End:
