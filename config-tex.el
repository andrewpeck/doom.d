;; -*- lexical-binding: t; -*-

;; https://github.com/hlissner/doom-emacs/blob/master/modules/lang/latex/README.org
(setq +latex-viewers
      '(okular atril evince zathura))

;; https://emacs.stackexchange.com/questions/33663/in-auctex-how-could-i-fold-acronyms
(after! tex-fold
  (add-to-list 'TeX-fold-macro-spec-list '("{1}" ("gls")))
  (add-to-list 'TeX-fold-macro-spec-list '("{1}" ("cite")))
;;(add-to-list 'TeX-fold-macro-spec-list '("{1}s" ("gls")))
;;(add-to-list 'TeX-fold-macro-spec-list '("{1}" ("ac" "acf")))
;;(add-to-list 'TeX-fold-macro-spec-list '("{1}s" ("acp" "acpf")))
)

(add-hook
 'LaTex-mode-hook
 (lambda () (flycheck-add-next-checker 'proselint 'tex-chktex)))

;; this is some e.g. text that has an example in it. And some other sentence that is actually a sentence. Something else.

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
                       (point-marker))))  ;; remember where to stop
            (beginning-of-line)
            (while (progn (forward-sentence)
                          (<= (point) (marker-position end)))
              (just-one-space) ;; leaves only one space, point is after it
              (delete-char -1) ;; delete the space
              (newline)        ;; and insert a newline
              (evil-indent-line (line-beginning-position) (line-end-position))))))

    ;; otherwise do ordinary fill paragraph
    (fill-paragraph P)))

  (evil-define-key 'normal latex-mode-map (kbd "M-q") #'ap/line-fill-paragraph)
  (evil-define-key 'normal latex-mode-map (kbd "M-q") nil)

;; TeX
;;------------------------------------------------------------------------------

(after! tex
  (setq reftex-toc-split-windows-horizontally t)
  (setq reftex-toc-split-windows-fraction 0.15)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook (lambda () (reftex-mode 1)))
  (add-hook 'reftex-toc-mode-hook (lambda ()
                                    (define-key reftex-toc-mode-map (kbd "<return>") 'reftex-toc-goto-line)))
  )

;; Electric Space
;;------------------------------------------------------------------------------

(defun electric-space () ; Trying to get Emacs to do semantic linefeeds
  (interactive)
  (if (looking-back (sentence-end) nil)
      (insert "\n")
    (self-insert-command 1))
  )

(defvar electric-space-on-p nil)

(defun toggle-electric-space ()
  (interactive)
  (global-set-key
   " "
   (if (setq electric-space-on-p
             (not electric-space-on-p))
       'electric-space
     'self-insert-command)))
