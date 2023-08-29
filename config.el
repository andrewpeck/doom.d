;; config.el -*- lexical-binding: t; -*-
;;
;; Interesting Packages:
;; https://github.com/alphapapa/taxy.el
;;
;; Interesting Emacs Configs:
;;
;; Tecosaur: https://github.com/tecosaur/emacs-config/blob/master/config.org
;; Karl Fogel: https://svn.red-bean.com/repos/kfogel/trunk/.emacs
;; Steve Purcell: https://github.com/purcell/emacs.d/
;; Henrik: https://github.com/hlissner/doom-emacs-private/blob/master/config.el
;; Collection of emacs configs: https://github.com/caisah/emacs.dz
;; https://github.com/danilevy1212/doom

;;------------------------------------------------------------------------------
;; Packages & Loads
;;------------------------------------------------------------------------------

(let ((default-directory (expand-file-name "packages" doom-user-dir)))
  (normal-top-level-add-subdirs-to-load-path))

;; ;; (use-package! hdl-deps       :defer-incrementally t)

;; start:sort
(use-package! backup-each-save :defer-incrementally t)
(use-package! gpt              :defer-incrementally t)
(use-package! hog              :defer-incrementally t)
(use-package! setup-system     :defer-incrementally t)
(use-package! system-install   :defer-incrementally t)
(use-package! ucf-mode         :defer-incrementally t)
(use-package! vivado-mode      :defer-incrementally t)
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

(defun doom-load!! (path) (load!! (concat doom-user-dir path)))
(defun doom-load-idle (path) (load-idle (concat doom-user-dir path)))
(defun doom-load-timer (path) (load-timer (concat doom-user-dir path)))


;; start:sort
(after! lsp (doom-load!! "config-lsp"))
(after! org (doom-load!! "config-org"))
(when (file-directory-p (concat doom-user-dir "passwords.el")) (doom-load!! "passwords"))
(doom-load!! "config-core")
(doom-load!! "config-doom")
(doom-load!! "config-keybinds")
(doom-load!! "config-modeline")
(doom-load!! "custom")
(doom-load-idle "config-align")
(doom-load-idle "config-appearance")
(doom-load-idle "config-completion")
(doom-load-idle "config-dired")
(doom-load-idle "config-elfeed")
(doom-load-idle "config-flycheck")
(doom-load-idle "config-git")
(doom-load-idle "config-langs")
(doom-load-idle "config-random")
(doom-load-idle "config-scad")
(doom-load-idle "config-tex.el")
(doom-load-idle "lisp/gerb-view")
(doom-load-idle "lisp/regulator")
(doom-load-idle "lisp/tracking")
;; end:sort

;; Local Variables:
;; eval: (make-variable-buffer-local 'kill-buffer-hook)
;; eval: (add-hook 'kill-buffer-hook (lambda () (sort-elisp-block) (save-buffer)) nil t)
;; End:
