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
(use-package! backup-each-save  :defer-incrementally t)
(use-package! gpt               :defer-incrementally t)
(use-package! hog               :defer-incrementally t)
(use-package! setup-system      :defer-incrementally t)
(use-package! system-install    :defer-incrementally t)
(use-package! ucf-mode          :defer-incrementally t)
(use-package! verilog-port-copy :defer-incrementally t)
(use-package! vivado-mode       :defer-incrementally t)
;; end:sort

(defun load!! (path) (load! path doom-user-dir t))

(defun load-timer (pkg &optional timer)
  "Load package on a timer."
  (let ((timer (if timer timer 1.5)))
    (run-with-timer timer nil #'load!! pkg)))

(defun load-idle (pkg &optional timer)
  "Load package on idle."
  (let ((timer (if timer timer 1.5)))
    (run-with-idle-timer timer nil #'load!! pkg)))

(defun doom-load-idle (path) (load-idle (concat doom-user-dir path)))
(defun doom-load-timer (path) (load-timer (concat doom-user-dir path)))

(load!! "custom")
(load!! "config-core")

(setq doom-scratch-dir doom-user-dir)
(setq bookmark-file (concat doom-user-dir "bookmarks"))

;; start:sort
(after! org (load!! "config-org"))
(load!! "config-align")
(load!! "config-appearance")
(load!! "config-completion")
(load!! "config-dired")
(load!! "config-doom")
(load!! "config-eglot")
(load!! "config-elfeed")
(load!! "config-flycheck")
(load!! "config-git")
(load!! "config-keybinds")
(load!! "config-langs")
(load!! "config-lsp")
(load!! "config-modeline")
(load!! "config-random")
(load!! "config-scad")
(load!! "config-tex.el")
(load!! "lisp/gerb-view")
(load!! "lisp/regulator")
(load!! "lisp/tracking")
(load!! "passwords")
;; end:sort

(make-variable-buffer-local 'after-save-hook)
(make-variable-buffer-local 'write-contents-functions)

;; Local Variables:
;; eval: (make-variable-buffer-local 'kill-buffer-hook)
;; eval: (add-hook 'kill-buffer-hook (lambda () (sort-elisp-block) (save-buffer)) nil t)
;; End:
