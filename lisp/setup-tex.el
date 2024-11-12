;;------------------------------------------------------------------------------
;; LaTex
;; LaTeX-mode-hook is used by AUCTeX's LaTeX mode.
;; latex-mode-hook is used by Emacs' built-in latex mode.
;;------------------------------------------------------------------------------

(use-package! tex-fold
  :config

  (setq TeX-fold-auto-reveal t)

  ;; https://emacs.stackexchange.com/questions/33663/in-auctex-how-could-i-fold-acronyms
  (setq TeX-fold-macro-spec-list
        (append TeX-fold-macro-spec-list
                '(
                  ;; glossary
                  ((lambda (x) (capitalize x)) ("Gls"))
                  ("{1}" ("gls"))

                  ;; ((lambda (x y) (propertize y 'font-lock-face '(:background "red")))  ("textcolor"))
                  ("{2}" ("textcolor"))

                  ("~" ("textasciitilde"))
                  ("^" ("textasciicircum" "xor"))
                  ("×" ("texttimes"))

                  ;; just put backticks around typewriter font
                  ("`{1}`" ("texttt"))

                  ;; shorten references
                  ("[r:{1}]" ("autoref"))
                  ("[l:{1}]" ("label"))
                  ("[r:{1}]" ("ref"))
                  ("[c:{1}]" ("cite"))

                  ;; used in resume
                  ((lambda (x) (concat (number-to-string (- (string-to-number (format-time-string "%Y"))
                                                            (string-to-number x))) " years")) ("yearsago"))

                  ;; used in resume
                  ("{1}" ("heading"))))))

;;------------------------------------------------------------------------------
;; auctex + tex
;;------------------------------------------------------------------------------

(use-package! latex
  :config

  (require 'expand-region)

  (defun tex-follow-link-at-point ()
    (interactive)

    (let ((f (thing-at-point 'filename t)))
      (string-match "\\(.*\\)\{\\(.*\\)}" f)
      (let ((f (concat (vc-root-dir) (match-string 2 f))))
        (when (and (not (string= f (vc-root-dir)))
                   (file-exists-p f))
          (find-file f)))))

  (evil-define-key '(motion normal) TeX-mode-map
    (kbd "RET") #'tex-follow-link-at-point)


  (setq-default TeX-master nil)         ; Query for master file.

  (evil-define-key '(motion normal visual) reftex-toc-mode-map
    (kbd "<") #'reftex-toc-promote)
  (evil-define-key '(motion normal visual) reftex-toc-mode-map
    (kbd ">") #'reftex-toc-demote)
  (evil-define-key '(motion normal visual) reftex-toc-mode-map
    (kbd "r") #'reftex-toc-Rescan)
  (evil-define-key '(motion normal visual) reftex-toc-mode-map
    (kbd "L") #'reftex-toc-set-max-level)


  (setq  reftex-cite-format nil)
  (setq  reftex-find-reference-format nil)
  ;; (setq  reftex-ref-style-alist '(("\\ref" t)))
  (setq  reftex-ref-macro-prompt nil)

  (defvar default-tex-master nil)
  (defun hook/set-default-tex-master ()
    (when (not TeX-master)
      (setq-local TeX-master default-tex-master)))

  (defun latex/set-default-tex-master ()
    (interactive)
    (let ((master-file
           (completing-read "Master File: "
                            (cl-remove-if-not (lambda (f) (string= "tex" (file-name-extension f)))
                                              (project-files (project-current))))))
      (setq default-tex-master master-file)
      (hook/set-default-tex-master) ; execute now to take effect immediately
      )) ; add a hook for future files

  (add-hook 'LaTeX-mode-hook #'hook/set-default-tex-master)

  (define-key LaTeX-mode-map (kbd "C-c C-s") #'LaTeX-section)

  (add-hook 'LaTeX-mode-hook #'outline-minor-mode)

  (define-key global-map (kbd "M-<right>") nil) ; bound by drag-stuff

  (define-key TeX-mode-map (kbd "M-<right>") #'outline-demote)
  (define-key TeX-mode-map (kbd "M-<left>") #'outline-promote)
  (define-key TeX-mode-map (kbd "TAB") #'nil)

  (defun TeX-toggle-folding ()
    (interactive)
    (call-interactively #'TeX-fold-mode)
    (if TeX-fold-mode
        (TeX-fold-buffer)
      (TeX-fold-clearout-buffer)))

  (map! :map TeX-mode-map
        :localleader

        ;; olivetti
        :desc "Olivetti Mode" "o" #'olivetti-mode

        ;; formatting macros
        :desc "TeX Format Bold"      "tb" #'tex-bold
        :desc "TeX Format Underline" "tu" #'tex-underline
        :desc "TeX Format Folding"   "ti" #'tex-italic
        :desc "TeX Format Folding"   "tt" #'tex-tt
        :desc "Tex glossarify"       "tg" #'tex-glossarify
        :desc "Tex Glossarify"       "tG" #'tex-Glossarify

        ;; folding
        :desc "Toggle TeX Folding" "b" #'TeX-toggle-folding
        )

  (defun tex-link-insert ()
    "Insert TeX href link"
    (interactive)
    (let ((url-at-point (thing-at-point 'url)))
      (let ((url (read-string "URL: " url-at-point))
            (text (read-string (format "Text: "))))
        (when (and url text)

          (when url-at-point
            (let ((bounds (bounds-of-thing-at-point 'url)))
              (delete-region (car bounds)
                             (cdr bounds))))

          (insert (format "\\href{%s}{%s}" url text))))))

  (define-key TeX-mode-map (kbd "C-c C-l") #'tex-link-insert)

  (define-key TeX-mode-map (kbd "C-c C-s") #'LaTeX-section)

  (defun reftex-toc-set-max-level ()
    (interactive)
    (let ((level
           (read-number "Level: " reftex-toc-max-level)))
      (setq reftex-toc-max-level level))
    (reftex-toc-Rescan))

  (setq pdf-sync-backward-display-action nil)
  (setq pdf-sync-forward-display-action t)

  (setq TeX-view-program-selection
        '((output-pdf "PDF Tools")
          (output-dvi "xdvi")))

  (setq TeX-view-program-list
        '(("PDF Tools" TeX-pdf-tools-sync-view)))

  (setq reftex-toc-max-level 2
        reftex-toc-split-windows-horizontally t
        reftex-toc-split-windows-fraction 0.2
        TeX-master nil
        +latex-viewers '(okular atril evince zathura))

  (setq-default TeX-command-extra-options " -shell-escape -synctex=1")

  ;; this isn't working very well and is creating
  ;;      paragraphs which are
  ;;      awkwardly indented
  (remove-hook! 'LaTeX-mode-hook
    #'adaptive-wrap-prefix-mode)

  (let ((name "*toc*"))
    (fit-window-to-buffer
     (car (seq-filter
           (lambda (window)
             (equal name (buffer-name (window-buffer window))))
           (window-list-1 nil 0 t))) nil nil "10"))

  (add-hook! 'LaTeX-mode-hook

             (olivetti-mode)
             (reftex-mode 1)
             (jinx-mode t)
             ;; (variable-pitch-mode 1)

             ;; (make-variable-buffer-local 'font-lock-type-face)
             ;; (set-face-attribute 'font-lock-type-face nil
             ;;                     :inherit 'default
             ;;                     :family "Courier New"
             ;;                     :height 120)

             ;; https://www.flannaghan.com/2013/01/11/tex-fold-mode
             ;; (add-hook! 'find-file-hook :local (TeX-fold-region (point-min) (point-max)))
             ;; (add-hook! 'write-contents-functions :local (TeX-fold-region (point-min) (point-max)))
             ;; (add-hook! 'after-change-functions :local 'TeX-fold-paragraph)

             ;; doom has some annoying hooks after macro insertion that cause obnoxious folding
             ;; (setq TeX-after-insert-macro-hook nil)

             (flycheck-add-next-checker 'proselint 'tex-chktex))

  ;; Semantic Linefeeds
  ;;------------------------------------------------------------------------------
  ;; https://abizjak.github.io/emacs/2016/03/06/latex-fill-paragraph.html
  ;; TODO: check for common things at end of line:
  ;; c.f. e.g. i.e.

  (defun ap/line-fill-paragraph (&optional P)
    "When called with prefix argument call `fill-paragraph'.
   Otherwise split the current paragraph into one sentence per line."
    (interactive "P")
    (if (not P)
        (save-excursion
          (let ((fill-column 12345678)) ;; relies on dynamic binding
            (fill-paragraph) ;; this will not work correctly if the paragraph is
            ;; longer than 12345678 characters (in which case the
            ;; file must be at least 12MB long. This is unlikely.)
            (let ((end (save-excursion
                         (forward-paragraph 1)
                         (backward-sentence)
                         (point-marker)))) ;; remember where to stop
              (beginning-of-line)
              (while (progn (forward-sentence)
                            (<= (point) (marker-position end)))
                (just-one-space) ;; leaves only one space, point is after it
                (delete-char -1) ;; delete the space
                (newline)        ;; and insert a newline
                (evil-indent-line (line-beginning-position) (line-end-position))))))

      ;; otherwise do ordinary fill paragraph
      (fill-paragraph P)))

  ;; treat hyphenated words as one
  (add-hook 'latex-mode-hook #'(lambda () (modify-syntax-entry ?- "w")))

  (defun tex-expand-and-insert (macro)
    (interactive)
    (when (not (region-active-p))
      (er/mark-word))
    (TeX-insert-macro macro))

  (defun tex-underline ()
    "Make the current TeX selection bold."
    (interactive)
    (tex-expand-and-insert "underline"))

  (defun tex-bold ()
    "Make the current TeX selection bold."
    (interactive)
    (tex-expand-and-insert "textbf"))

  (defun tex-italic ()
    "Make the current TeX selection italic."
    (interactive)
    (tex-expand-and-insert "textit"))

  (defun tex-tt ()
    "Make the current TeX selection typewriter."
    (interactive)
    (tex-expand-and-insert "texttt"))

  (defun tex-glossarify ()
    "Make the current TeX selection a glossary entry."
    (interactive)
    (tex-expand-and-insert "gls"))

  (defun tex-Glossarify ()
    "Make the current TeX selection a Glossary entry."
    (interactive)
    (tex-expand-and-insert "Gls"))

  ;; Electric Space
  ;;------------------------------------------------------------------------------

  (defun electric-space ()        ; Trying to get Emacs to do semantic linefeeds
    (interactive)
    (if (looking-back (sentence-end) nil)
        (insert "\n")
      (self-insert-command 1)))

  (defvar electric-space-on-p nil)

  (defun toggle-electric-space ()
    (interactive)
    (global-set-key
     " "
     (if (setq electric-space-on-p
               (not electric-space-on-p))
         'electric-space
       'self-insert-command))))
