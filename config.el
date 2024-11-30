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

;; Fish (and possibly other non-POSIX shells) is known to inject garbage
;; output into some of the child processes that Emacs spawns. Many Emacs
;; packages/utilities will choke on this output, causing unpredictable
;; issues. To get around this, either:
(setq shell-file-name (executable-find "bash"))
(setq-default vterm-shell (executable-find "fish"))
(setq-default explicit-shell-file-name (executable-find "fish"))

;; start:sort
(load!! "config-align")
(load!! "config-appearance")
(load!! "config-doom")
(load!! "config-keybinds")
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

(defun byte-compile-config ()
  (interactive)
  (byte-recompile-directory (expand-file-name "~/.doom.d") 0))

;;------------------------------------------------------------------------------
;; Mode aliases
;;------------------------------------------------------------------------------

;; enable syntax highlighting for vimrc files
(add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode))   ; vimrc
(add-to-list 'auto-mode-alist '("\\.xdc\\'"          . vivado-mode))  ; tcl mode for xdc files
(add-to-list 'auto-mode-alist '("\\.ltx\\'"          . json-mode))    ; json mode for ltx files
(add-to-list 'auto-mode-alist '("\\.ino\\'"          . cpp-mode))     ; cpp mode for arduino files
(add-to-list 'auto-mode-alist '("\\.cheby\\'"        . yaml-mode))    ; yaml mode for cheby
(add-to-list 'auto-mode-alist '("\\.bb\\'"           . clojure-mode)) ; babashka
(add-to-list 'auto-mode-alist '("\\.drawio\\'"       . image-mode))
(add-to-list 'auto-mode-alist '("\\.excalidraw\\'"   . image-mode))

;; Local Variables:
;; eval: (make-variable-buffer-local 'kill-buffer-hook)
;; eval: (add-hook 'kill-buffer-hook (lambda () (sort-elisp-block) (save-buffer)) nil t)
;; End:
