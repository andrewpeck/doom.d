;; config.el -*- lexical-binding: t; -*-
;;
;; https://github.com/alphapapa/taxy.el
;; Tecosaur: https://github.com/tecosaur/emacs-config/blob/master/config.org
;; https://svn.red-bean.com/repos/kfogel/trunk/.emacs
;; Steve Purcell: https://github.com/purcell/emacs.d/
;; Henrik: https://github.com/hlissner/doom-emacs-private/blob/master/config.el
;; https://github.com/caisah/emacs.dz
;; https://github.com/danilevy1212/doom

;;------------------------------------------------------------------------------
;; Loads
;;------------------------------------------------------------------------------

(let ((default-directory (expand-file-name "packages" doom-user-dir)))
  (normal-top-level-add-subdirs-to-load-path))

(setq doom-incremental-idle-timer 0.1)
(setq doom-incremental-first-idle-timer 0.2)

;; start:sort
(use-package! gpt            :defer-incrementally t)
(use-package! hog            :defer-incrementally t)
(use-package! org-mode       :defer-incrementally t)
(use-package! setup-system   :defer-incrementally t)
(use-package! system-install :defer-incrementally t)
(use-package! ucf-mode       :defer-incrementally t)
(use-package! vivado-mode    :defer-incrementally t)
;; end:sort

(defun load!! (pkg)
"Demote errors while loading a file to prevent errors in startup from cascading."
  (with-demoted-errors (load! pkg)))

(defun load!!! (pkg &optional timer)
  (let ((timer (if timer timer doom-incremental-idle-timer)))
    (run-with-idle-timer nil #'load!! pkg)))

;; start:sort
(load!! "~/.doom.d/config-appearance.el")
(load!!! "~/.doom.d/config-align.el")
(load!!! "~/.doom.d/config-company.el")
(load!!! "~/.doom.d/config-delight.el")
(load!!! "~/.doom.d/config-doom.el")
(load!!! "~/.doom.d/config-eglot.el")
(load!!! "~/.doom.d/config-elfeed.el")
(load!!! "~/.doom.d/config-evil.el")
(load!!! "~/.doom.d/config-flycheck.el")
(load!!! "~/.doom.d/config-git.el")
(load!!! "~/.doom.d/config-langs.el")
(load!!! "~/.doom.d/config-modeline.el")
(load!!! "~/.doom.d/config-org.el")
(load!!! "~/.doom.d/config-random.el")
(load!!! "~/.doom.d/config-scad.el")
(load!!! "~/.doom.d/custom.el")
(load!!! "~/.doom.d/lisp/gerb-view.el")
(load!!! "~/.doom.d/lisp/regulator.el")
(load!!! "~/.doom.d/lisp/tracking.el")
(load!!! "~/.doom.d/passwords.el")
;; end:sort

;; (load! "~/.doom.d/config-tex.el")

(when (string= (system-name) "larry")
  (load!!! "~/.doom.d/config-mail.el"))

;; Local Variables:
;; eval: (make-variable-buffer-local 'write-contents-functions)
;; eval: (add-hook 'write-contents-functions (lambda () (when (fboundp 'sort-elisp-block) (sort-elisp-block))) nil t)
;; End:
