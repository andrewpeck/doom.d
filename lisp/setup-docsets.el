;;------------------------------------------------------------------------------
;; Dash Docs
;;------------------------------------------------------------------------------

(use-package! dash-docs
  :defer-incrementally t
  :config
  (defvar dash-docs-my-docsets
    '("Tcl" "Python_3" "C" "C++" "Bash" "Clojure")
    "Docsets to be installed via `dash-docs-install-my-docsets'")

  (defun my/dash-docs-install-docsets ()

    "Install docsets specified by the list `dash-docs-my-docsets'"

    (interactive)

    (require 'dash-docs)

    (mkdir dash-docs-docsets-path t)

    (let ((dash-docs-lib-path (locate-library "dash-docs"))
          (dest dash-docs-docsets-path))

      (dolist (docset dash-docs-my-docsets)
        (when (not (file-exists-p (concat  dash-docs-docsets-path
                                           (string-replace "_" " " docset) ".docset")))
          (message (concat "Installing " docset))
          (async-start (lambda () (progn (load dash-docs-lib-path)
                                         (require 'dash-docs)
                                         (message (concat "Installing " docset))
                                         (setq dash-docs-docsets-path dest)
                                         (dash-docs-install-docset docset)
                                         docset))
                       (lambda (result) (message (format  "Docset %s installed" result)))))))))
