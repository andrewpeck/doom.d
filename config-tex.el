;; -*- lexical-binding: t; -*-

;; https://emacs.stackexchange.com/questions/33663/in-auctex-how-could-i-fold-acronyms
(after! tex-fold
  (add-to-list 'TeX-fold-macro-spec-list '("{1}" ("gls")))
  (add-to-list 'TeX-fold-macro-spec-list '("{1}" ("cite")))
)
;; Semantic Linefeeds
;;------------------------------------------------------------------------------
;; https://abizjak.github.io/emacs/2016/03/06/latex-fill-paragraph.html
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

(evil-define-minor-mode-key 'normal 'latex-mode-map (kbd "M-q") #'ap/line-fill-paragraph)

;;(add-hook 'LaTex-mode-hook
;;          (lambda () (define-key evil-normal-state-local-map (kbd "M-q") 'ap/line-fill-paragraph))
;;          )

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
