;; config.el -*- lexical-binding: t; -*-
;;
;;https://www.reddit.com/r/emacs/comments/10ktqj0/weekly_tips_tricks_c_thread/
;; https://github.com/alphapapa/taxy.el
;; Tecosaur: https://github.com/tecosaur/emacs-config/blob/master/config.org
;; https://svn.red-bean.com/repos/kfogel/trunk/.emacs
;; Steve Purcell: https://github.com/purcell/emacs.d/
;; Henrik: https://github.com/hlissner/doom-emacs-private/blob/master/config.el
;; https://github.com/caisah/emacs.dz
;; https://github.com/danilevy1212/doom

;;------------------------------------------------------------------------------
;; Packages & Loads
;;------------------------------------------------------------------------------

(let ((default-directory (expand-file-name "packages" doom-user-dir)))
  (normal-top-level-add-subdirs-to-load-path))

(setq doom-incremental-idle-timer 0.1)
(setq doom-incremental-first-idle-timer 1.5)

;; start:sort
(use-package! gpt            :defer-incrementally t)
(use-package! hog            :defer-incrementally t)
(use-package! setup-system   :defer-incrementally t)
(use-package! system-install :defer-incrementally t)
(use-package! ucf-mode       :defer-incrementally t)
(use-package! vivado-mode    :defer-incrementally t)
;; end:sort

(defun load!! (pkg)
"Demote errors while loading a file to prevent errors in startup from cascading."
  (with-demoted-errors "Error %s" (load! pkg)))

(defun load-timer (pkg &optional timer)
  "Load package on a timer."
  (let ((timer (if timer timer 1.5)))
    (run-with-timer timer nil #'load!! pkg)))

(defun load-idle (pkg &optional timer)
  "Load package on idle."
  (let ((timer (if timer timer 1.5)))
    (run-with-idle-timer timer nil #'load!! pkg)))

;; start:sort
(load "~/.doom.d/config-core")
(load "~/.doom.d/config-doom")
(load! "~/.doom.d/config-org")
(load!! "~/.doom.d/config-appearance")
(load!! "~/.doom.d/config-lsp")
(load!! "~/.doom.d/config-langs")
(load!! "~/.doom.d/config-modeline")
(load!! "~/.doom.d/custom")
(load-idle "~/.doom.d/config-align")
(load-idle "~/.doom.d/config-completion")
(load-idle "~/.doom.d/config-dired")
(load-idle "~/.doom.d/config-elfeed")
(load-idle "~/.doom.d/config-flycheck")
(load-idle "~/.doom.d/config-git")
(load-idle "~/.doom.d/config-random")
(load-idle "~/.doom.d/config-scad")
(load-idle "~/.doom.d/config-tex.el")
(load-idle "~/.doom.d/lisp/gerb-view")
(load-idle "~/.doom.d/lisp/regulator")
(load-idle "~/.doom.d/lisp/tracking")
(load-idle "~/.doom.d/passwords")
(load-timer "~/.doom.d/config-keybinds")
;; end:sort


(when (string= (system-name) "larry")
  (load!! "~/.doom.d/config-mail"))

(when init-file-debug
  (benchmark-init/deactivate))

;; Local Variables:
;; eval: (make-variable-buffer-local 'write-contents-functions)
;; eval: (add-hook 'write-contents-functions (lambda () (when (fboundp 'sort-elisp-block) (sort-elisp-block))) nil t)
;; End:
