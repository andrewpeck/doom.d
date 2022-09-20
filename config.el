;; config.el -*- lexical-binding: t; -*-
;;
;; https://github.com/alphapapa/taxy.el
;;
;; TODO tabular-like alignment
;; TODO magit.sh like functionality
;; TODO outline folding
;;
;; Tecosaur: https://github.com/tecosaur/emacs-config/blob/master/config.org
;; https://svn.red-bean.com/repos/kfogel/trunk/.emacs
;; Steve Purcell: https://github.com/purcell/emacs.d/
;; Henrik: https://github.com/hlissner/doom-emacs-private/blob/master/config.el
;; https://github.com/caisah/emacs.dz

(add-hook! emacs-lisp-mode-hook
  (progn
               (make-variable-buffer-local tab-width)
               (setq tab-width 4)))

(load! "~/.doom.d/custom.el")

;;------------------------------------------------------------------------------
;; to file
;;------------------------------------------------------------------------------
;; (setq langtool-java-classpath "/usr/share/languagetool:/usr/share/java/languagetool/*")
(after! langtool
  (setq langtool-java-classpath "/snap/languagetool/current/usr/share/java")
  (setq langtool-language-tool-server-jar "/snap/languagetool/current/usr/bin/languagetool.jar"))

;; disable confusing undo-fu behavior
;; https://gitlab.com/ideasman42/emacs-undo-fu/-/issues/6
(after! undo-fu
  (setq undo-fu-ignore-keyboard-quit t))

(add-to-list 'warning-suppress-types '(iedit))
(remove-hook
 '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)

;;------------------------------------------------------------------------------
;; Loads
;;------------------------------------------------------------------------------

(add-to-list 'load-path (expand-file-name "~/.doom.d/"))
(add-to-list 'load-path (expand-file-name "~/.doom.d/lisp/"))
(add-to-list 'load-path (expand-file-name "~/.doom.d/lisp/hog-emacs/"))
(add-to-list 'load-path (expand-file-name "~/.doom.d/lisp/doctor/"))
(add-to-list 'load-path (expand-file-name "~/.doom.d/lisp/hdl_deps/"))

(let ((default-directory (expand-file-name "lisp" doom-user-dir)))
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

;; (require 'undo-hl)
;; (add-hook 'text-mode-hook #'undo-hl-mode)
;; (add-hook 'prog-mode-hook #'undo-hl-mode)

;; start:sort
(load! "~/.doom.d/config-align.el")
(load! "~/.doom.d/config-appearance.el")
;; (load! "~/.doom.d/config-company.el")
(load! "~/.doom.d/config-doom.el")
(load! "~/.doom.d/config-langs.el")
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
;; (remove-hook 'text-mode-hook #'auto-fill-mode)

;; Don't wrap text modes unless we really want it
(remove-hook 'text-mode-hook #'+word-wrap-mode)
(add-hook 'latex-mode-hook #'+word-wrap-mode)
(add-hook 'markdown-mode-hook #'+word-wrap-mode)

;; (defun fix-visual-fill-column-mode (&optional ARG)
;;   (setq visual-fill-column-mode visual-line-mode))

;; toggle visual-fill column mode when chaing word wrap settings
;; (advice-add '+word-wrap-mode
;;             :after 'fix-visual-fill-column-mode)
;;
;; (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)

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
;; VHDL Mode
;;------------------------------------------------------------------------------

;; vhdl mode will wrap comments after some # of characters
(after! vhdl
  (setq vhdl-end-comment-column 200
        vhdl-prompt-for-comments nil
        auto-fill-mode nil))

;;-----------------------------------------------------------------------------------------
;; Elfeed
;;------------------------------------------------------------------------------

;; Run `elfeed-update' every 8 hours
(run-at-time nil (* 8 60 60) #'elfeed-update)

(after! elfeed
  (setq elfeed-feeds
        '(
          ;; "https://hackaday.com/blog/feed/"
          "https://www.evalapply.org/index.xml"
          "https://nullprogram.com/feed/"
          "https://bzg.fr/index.xml"
          "https://www.mattblaze.org/blog/rss20.xml"
          "http://mbork.pl/?action=rss;days=30;all=0;showedit=0"
          "https://isc.sans.edu/rssfeed_full.xml"
          "https://watchguy.co.uk/feed/"
          "https://sachachua.com/blog/feed/")))

;;-----------------------------------------------------------------------------------------
;; IELM
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

  (add-hook 'inferior-emacs-lisp-mode-hook #'set-ielm-comint-input-ring))

;;-----------------------------------------------------------------------------------------
;; XML
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
;; Markdown
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

;; (setq org-babel-clojure-backend "cider")

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
  (save-excursion
    (sort-code-block ";;")))

;;
;; Local Variables:
;; eval: (make-variable-buffer-local 'write-contents-functions)
;; eval: (add-hook 'write-contents-functions (lambda () (when (boundp 'sort-elisp-block) sort-elisp-block)) nil t)
;; End:
