;;------------------------------------------------------------------------------
;; Awk
;;------------------------------------------------------------------------------

(add-hook! 'awk-mode-hook
  (setq-local  comment-start "# "))

;;------------------------------------------------------------------------------
;; OpenSCAD
;;------------------------------------------------------------------------------

(use-package! scad-mode

  :init

  (add-hook! 'scad-mode-hook
    (defun scad--reindent-buffer-hook ()
      "Reindent buffer on save."
      (add-hook 'write-contents-functions
                #'re-indent-buffer nil t)))

  :config

  (defun open-in-openscad ()
    "Open the current buffer in openscad"
    (interactive)
    (call-process
     "openscad" nil 0 nil
     (buffer-file-name)))

  (define-key scad-mode-map (kbd "C-c C-p")
              'open-in-openscad)

  (defun scad-cheatshet ()
    "Open the SCAD Cheatsheet in a web browser"
    (interactive)
    (browse-url  "https://openscad.org/cheatsheet/")))

;;------------------------------------------------------------------------------
;; Tcl
;;------------------------------------------------------------------------------

(use-package! tcl

  :defer-incrementally t

  :config
  ;; (dolist (key vivado-builtin-list)
  ;;   (add-to-list 'tcl-builtin-list key))
  ;; (dolist (key vivado-keyword-list)
  ;;   (add-to-list 'tcl-keyword-list key))
  ;; (dolist (key vivado-constant-list)
  ;;   (add-to-list 'tcl-constant-list key))

  ;; make $ not part of a symbol in tcl-mode

  (setq-local smartparens-mode t
              auto-fill-mode nil)

  (setq tcl-help-directory-list '("/usr/share/doc/tclx"))

  (modify-syntax-entry ?$ "'" tcl-mode-syntax-table))

;;------------------------------------------------------------------------------
;; Markdown
;;------------------------------------------------------------------------------

(use-package! markdown

  :defer-incrementally t

  :config

  (defun markdown->pdf ()
    "Export markdown to PDF with Pandoc and open."
    (interactive)
    (let* ((base (file-name-base (buffer-file-name)))
           (md (concat base ".md"))
           (pdf (concat base ".pdf")))

      (if (not (executable-find "pandoc"))
          (message "Pandoc not installed!")
        (progn
          (message (shell-command-to-string (format "pandoc %s -o %s" md pdf)))
          (if (f-file-p pdf)
              (async-shell-command (format "xdg-open %s" pdf))))))))

;;------------------------------------------------------------------------------
;; C mode
;;------------------------------------------------------------------------------

(use-package! c

  :defer-incrementally t

  :init
  ;; double slashes // instead of slash-stars /* ... */
  (add-hook! 'c-mode-common-hook
             ;; Preferred comment style
             (defun hook/set-c-comment-start ()
               (setq comment-start "// " comment-end ""))))

;;------------------------------------------------------------------------------
;; XML
;;------------------------------------------------------------------------------

(use-package! nxml

  :defer-incrementally t

  :init

  (add-hook! 'nxml-mode-hook
    (defun hook/disable-visual-fill-column-mode ()
      (visual-fill-column-mode -1)))

  :config

  (setq nxml-child-indent 2
        nxml-attribute-indent 2)

  (defun nxml-pretty-format ()
    (interactive)
    (save-excursion
      (shell-command-on-region (point-min) (point-max)
                               "xmllint --format -" (buffer-name) t)
      (nxml-mode)
      (indent-region begin end))))

;;------------------------------------------------------------------------------
;; Elisp
;;------------------------------------------------------------------------------

(use-package! elisp-mode
  :defer-incrementally t

  :init
  (remove-hook! 'emacs-lisp-mode-hook #'outline-minor-mode)
  (remove-hook! 'emacs-lisp-mode-hook #'highlight-quoted-mode)
  (remove-hook! 'emacs-lisp-mode-hook #'embrace-emacs-lisp-mode-hook)
  (remove-hook! 'emacs-lisp-mode-hook #'doom--setq-tab-width-for-emacs-lisp-mode-h)
  (remove-hook! 'emacs-lisp-mode-hook #'doom--setq-outline-level-for-emacs-lisp-mode-h)
  (remove-hook! 'emacs-lisp-mode-hook #'doom--setq-outline-regexp-for-emacs-lisp-mode-h)

  ;; set the tab width for emacs lisp mode to 4 for compatibility with emacs libs
  (add-hook! 'emacs-lisp-mode-hook
    (defun hook/set-elisp-tab-width ()
      (setq-local tab-width 4))))

;;------------------------------------------------------------------------------
;; Common Lisp
;;------------------------------------------------------------------------------

(use-package! slime
  :defer-incrementally t
  :config
  (setq inferior-lisp-program "sbcl"
        org-babel-lisp-eval-fn 'slime-eval))

;;------------------------------------------------------------------------------
;; Clojure
;;------------------------------------------------------------------------------

(use-package! clojure-mode

  :defer-incrementally t

  :config

  ;; cider-edit-jack-in-command
  (setq org-babel-clojure-backend "cider")
  (setq cider-save-file-on-load t))

(use-package! flycheck-clj-kondo
  :defer-incrementally t
  :after clojure-mode
  :defer-incrementally t)

;;------------------------------------------------------------------------------
;; Graphviz
;;------------------------------------------------------------------------------

(use-package! graphviz-dot-mode
  :defer-incrementally t

  :config

  ;; png seems to have a bug right now
  (setq graphviz-dot-preview-extension "jpg")

  (defun graphviz--display-preview-buffer (stdout-buffer)
    "Display STDOUT-BUFFER as the dot preview."
    (save-excursion
      (with-current-buffer stdout-buffer
        (goto-char (point-min))
        (image-mode)
        (display-buffer stdout-buffer)))))

;;------------------------------------------------------------------------------
;; Hog
;;------------------------------------------------------------------------------

(use-package! hog
  :config
  (pcase (system-name)
    ("strange" (setq hog-vivado-path "~/Xilinx/Vivado/2021.1"
                     hog-number-of-jobs 16))
    ("larry" (setq hog-vivado-path "/storage/Xilinx/Vivado/2021.1"
                   hog-number-of-jobs 4))
    ("pepper" (setq hog-vivado-path "/opt/Xilinx/Vivado/2021.1"))
    ("apeck-len01" (setq hog-vivado-path "/opt/Xilinx/Vivado/2022.2"))
    )

  (setq hog-ieee-library
        '("ieee" ("/usr/local/lib/ghdl/src/synopsys/*.vhdl"
                  "/usr/local/lib/ghdl/src/std/v08/*.vhdl"
                  "/usr/local/lib/ghdl/src/ieee2008/*.vhdl"
                  "/usr/lib/ghdl/src/synopsys/*.vhdl"
                  "/usr/lib/ghdl/src/std/v08/*.vhdl"
                  "/usr/lib/ghdl/src/ieee2008/*.vhdl"))))
