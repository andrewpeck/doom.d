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
;; https://github.com/danilevy1212/doom

(load! "~/.doom.d/custom.el")
(load! "~/.doom.d/lisp/monochrome-theme.el")

;;------------------------------------------------------------------------------
;; to file
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; Loads
;;------------------------------------------------------------------------------

(add-to-list 'load-path (expand-file-name "~/.doom.d/"))
(add-to-list 'load-path (expand-file-name "~/.doom.d/lisp/"))
(add-to-list 'load-path (expand-file-name "~/.doom.d/lisp/doctor/"))
(add-to-list 'load-path (expand-file-name "~/.doom.d/lisp/hdl_deps/"))

(let ((default-directory (expand-file-name "lisp" doom-user-dir)))
  (normal-top-level-add-subdirs-to-load-path))

;; (add-to-list 'load-path (expand-file-name "~/.doom.d/scad-preview/"))
;; https://github.com/hlissner/doom-emacs/issues/1213

;; start:sort
(use-package! doctor :load-path "~/.doom.d/lisp/doctor/doctor.el")
(use-package! hog :load-path "~/.doom.d/lisp/hog-emacs/hog.el")
(use-package! regulator :load-path "~/.doom.d/lisp/regulator.el")
(use-package! system-install :load-path "~/.doom.d/lisp/system-install/system-install.el")
(use-package! tracking :load-path "~/.doom.d/lisp/tracking.el")
(use-package! ucf-mode :load-path "~/.doom.d/lisp/ucf-mode.el")
(use-package! vivado-mode :load-path "~/.doom.d/lisp/vivado-mode.el")
;; end:sort

;; start:sort
(if (string= (system-name) "larry") (load! "~/.doom.d/config-mail.el"))
(load! "~/.doom.d/config-align.el")
(load! "~/.doom.d/config-appearance.el")
(load! "~/.doom.d/config-company.el")
(load! "~/.doom.d/config-delight.el")
(load! "~/.doom.d/config-doom.el")
(load! "~/.doom.d/config-evil.el")
(load! "~/.doom.d/config-flycheck.el")
(load! "~/.doom.d/config-git.el")
(load! "~/.doom.d/config-langs.el")
(load! "~/.doom.d/config-org.el")
(load! "~/.doom.d/config-random.el")
(load! "~/.doom.d/config-scad.el")
(load! "~/.doom.d/config-tex.el")
(load! "~/.doom.d/config-vhdl.el")
(load! "~/.doom.d/gerb-view.el")
(load! "~/.doom.d/lisp/work-plotting.el")
;; (load! "~/.doom.d/config-modeline.el")
;; (load! "~/.doom.d/lisp/regulator.el")
;; (load! "~/.doom.d/lisp/scimax-org-return.el")
;; (load! "~/.doom.d/lisp/verilog-port-copy.el")
;; end:sort

;;------------------------------------------------------------------------------
;; Functions for alphabetically sorting items
;;------------------------------------------------------------------------------

(defun sort-code-block (comment-char)
  "Alphabetically sorts code blocks in a file, starting with #
start:sort and ending with # end:sort, where # is the comment
char of the language you are editing"

  (let (buffer-undo-list)
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
        (goto-char home))))
  
  ;; make sure to return nil here for write-contents-functions
  nil)

(defun sort-elisp-block ()
  (interactive)
  (save-excursion
    (sort-code-block ";;")))

;;------------------------------------------------------------------------------
;; Eglot
;;------------------------------------------------------------------------------

(after! eglot
  (add-to-list 'eglot-server-programs
               '(vhdl-mode . ("ghdl-ls"))))

;; Local Variables:
;; eval: (make-variable-buffer-local 'write-contents-functions)
;; eval: (add-hook 'write-contents-functions (lambda () (when (fboundp 'sort-elisp-block) (sort-elisp-block))) nil t)
;; End:
