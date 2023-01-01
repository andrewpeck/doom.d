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

;;------------------------------------------------------------------------------
;; to file
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; Loads
;;------------------------------------------------------------------------------

(let ((default-directory (expand-file-name "packages" doom-user-dir)))
  (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'load-path (expand-file-name "~/.doom.d/lisp/"))

(setq doom-incremental-idle-timer 0.1)
(setq doom-incremental-first-idle-timer 0.1)

;; https://github.com/hlissner/doom-emacs/issues/1213
;; start:sort
(use-package! doctor         :defer-incrementally t)
(use-package! gpt-emacs      :defer-incrementally t)
(use-package! hog            :defer-incrementally t)
(use-package! system-install :defer-incrementally t)
(use-package! ucf-mode       :defer-incrementally t)
(use-package! vivado-mode    :defer-incrementally t)
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
(load! "~/.doom.d/config-modeline.el")
(load! "~/.doom.d/config-org.el")
(load! "~/.doom.d/config-random.el")
(load! "~/.doom.d/config-scad.el")
(load! "~/.doom.d/lisp/gerb-view.el")
(load! "~/.doom.d/lisp/regulator.el")
(load! "~/.doom.d/lisp/tracking.el")
(load! "~/.doom.d/lisp/work-plotting.el")
(load! "~/.doom.d/passwords.el")
;; (load! "~/.doom.d/config-tex.el")
;; end:sort

;; (load! "~/.doom.d/lisp/scimax-org-return.el")
;; (load! "~/.doom.d/lisp/verilog-port-copy.el")

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
