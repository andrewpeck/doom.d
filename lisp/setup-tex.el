;; -*- lexical-binding: t; -*-
;;------------------------------------------------------------------------------
;; LaTex
;; LaTeX-mode-hook is used by AUCTeX's LaTeX mode.
;; latex-mode-hook is used by Emacs' built-in latex mode.
;;------------------------------------------------------------------------------

(use-package! tex-fold
  :after latex
  :custom
  (TeX-fold-auto-reveal t)
  :config
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

                  ;; just show the short link text
                  ("{2}" ("href"))

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

(use-package tex
  :config

  (setq-default TeX-command-extra-options " -shell-escape -synctex=1")

  (setq-default TeX-master nil)        ; Query for master file.

  (advice-add 'TeX-view :before
              (lambda ()
                (when TeX-master
                  (setq TeX-current-process-region-p nil)))))

(use-package reftex
  :after latex
  :custom
  (reftex-cite-format nil)
  (reftex-find-reference-format nil)
  (reftex-toc-max-level 2)
  (reftex-toc-split-windows-horizontally t)
  (reftex-toc-split-windows-fraction 0.2)
  (reftex-ref-macro-prompt nil))

(use-package pdf-sync
  :config
  (setq pdf-sync-backward-display-action nil
        pdf-sync-forward-display-action t))

(use-package latex

  :custom

  (+latex-viewers '(pdf-tools okular atril evince zathura))

  :hook

  (LaTeX-mode-hook . outline-minor-mode)
  (LaTeX-mode-hook . jinx-mode)
  (LaTeX-mode-hook . reftex-mode)
  (LaTeX-mode-hook . hook/modify-latex-hyphen-syntax)

  :config

  (require 'auctex)

  ;;------------------------------------------------------------------------------
  ;; Keybinds
  ;;------------------------------------------------------------------------------

  (defun tex-file-at-point ()
    (let ((f (thing-at-point 'filename t)))
      (string-match "\\(.*\\)\{\\(.*\\)}" f)
      (let ((f (concat (vc-root-dir) (match-string 2 f))))
        (and (not (string= f (vc-root-dir)))
             (file-exists-p f)
             f))))

  (defun tex-follow-link-at-point ()
    ;; TODO this doesn't work for urls
    (interactive)
    (let ((f (thing-at-point 'filename t)))
      (string-match "\\(.*\\)\{\\(.*\\)}" f)
      (let ((f (concat (vc-root-dir) (match-string 2 f))))
        (when (and (not (string= f (vc-root-dir)))
                   (file-exists-p f))
          (find-file f)))))

  (defun tex-m-ret-dwim ()
    (interactive)
    (cond
     ((thing-at-point 'url) (browse-url (thing-at-point 'url)))
     ((tex-file-at-point) (find-file (tex-file-at-point)))
     (t (LaTeX-insert-item))))

  (map! (:map LaTeX-mode-map
              "M-q" 'ap/line-fill-paragraph
              "C-c C-l" 'tex-link-insert
              "M-<right>" 'outline-demote
              "M-<left>" 'outline-promote
              "C-c C-l" 'tex-link-insert
              "C-c C-s" 'LaTeX-section
              "M-RET" 'tex-m-ret-dwim)

        (:map reftex-toc-mode-map
         :after reftex-toc-mode
         "<return>" 'reftex-toc-goto-line
         "<"        'reftex-toc-promote
         ">"        'reftex-toc-demote
         "r"        'reftex-toc-Rescan
         "L"        'reftex-toc-set-max-level)

        (:map TeX-mode-map
         :localleader

         ;; formatting macros
         :desc "TeX Format Bold" "tb" 'tex-bold
         :desc "TeX Format Underline" "tu" 'tex-underline
         :desc "TeX Format Folding" "ti" 'tex-italic
         :desc "TeX Format Folding" "tt" 'tex-tt
         :desc "Tex glossarify" "tg" 'tex-glossarify
         :desc "Tex Glossarify" "tG" 'tex-Glossarify

         ;; folding
         :desc "Toggle TeX Folding" "b" 'TeX-toggle-folding))

  ;;------------------------------------------------------------------------------
  ;; Config
  ;;------------------------------------------------------------------------------

  (setq TeX-outline-extra
        '(("%chapter" 1)
          ("%section" 2)
          ("%subsection" 3)
          ("%subsubsection" 4)
          ("%paragraph" 5)))

  ;; add font locking to the headers
  (font-lock-add-keywords
   'latex-mode
   '(("^%\\(chapter\\|\\(sub\\|subsub\\)?section\\|paragraph\\)"
      0 'font-lock-keyword-face t)
     ("^%chapter{\\(.*\\)}"       1 'font-latex-sectioning-1-face t)
     ("^%section{\\(.*\\)}"       1 'font-latex-sectioning-2-face t)
     ("^%subsection{\\(.*\\)}"    1 'font-latex-sectioning-3-face t)
     ("^%subsubsection{\\(.*\\)}" 1 'font-latex-sectioning-4-face t)
     ("^%paragraph{\\(.*\\)}"     1 'font-latex-sectioning-5-face t)))

  (after! flycheck
    (flycheck-add-next-checker 'proselint 'tex-chktex))

  ;; this isn't working very well and is creating
  ;;      paragraphs which are
  ;;      awkwardly indented
  (remove-hook! 'LaTeX-mode-hook #'adaptive-wrap-prefix-mode))
