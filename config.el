;; config.el -*- lexical-binding: t; -*-
;;
;; Interesting Packages:
;; https://github.com/alphapapa/taxy.el
;; https://github.com/alphapapa/activities.el
;;
;; Interesting Emacs Configurations
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

(setq use-package-always-defer t)

(let ((default-directory (expand-file-name "packages" doom-user-dir)))
  (normal-top-level-add-subdirs-to-load-path))

;; start:sort
(use-package! gpt)
(use-package! setup-system)
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

;; Suppress `Package cl is deprecated` warnings
(setq byte-compile-warnings '(not obsolete))

(setq doom-scratch-dir doom-user-dir)

;; start:sort
(load!! "config-align")
(load!! "config-appearance")
(load!! "config-doom")
(load!! "config-modeline")
(load!! "config-packages")
(load!! "config-random")
(load!! "lisp/plotting")
(load!! "lisp/regulator")
(load!! "lisp/tracking")
(load!! "passwords")
(load!! "psiq")
;; end:sort

;; Load setup files
(dolist (file (file-expand-wildcards (concat doom-user-dir "lisp/setup*.el")))
        (load!! (file-name-sans-extension file)))

;; load keybinds after everything else
(load!! "config-keybinds")

;; Local Variables:
;; eval: (make-variable-buffer-local 'kill-buffer-hook)
;; eval: (add-hook 'kill-buffer-hook (lambda () (sort-elisp-block) (save-buffer)) nil t)
;; End:
