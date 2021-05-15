

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
              (evil-indent-line (line-beginning-position) (line-end-position))
              ))))

    ;; otherwise do ordinary fill paragraph
    (fill-paragraph P)))

;;(add-hook 'LaTex-mode-hook
;;          (lambda () (define-key evil-normal-state-local-map (kbd "M-q") 'ap/line-fill-paragraph))
;;          )

;;; TeX
;;------------------------------------------------------------------------------

(after! tex
  (setq reftex-toc-split-windows-horizontally t)
  (setq reftex-toc-split-windows-fraction 0.15)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook (lambda () (reftex-mode 1)))
  (add-hook 'reftex-toc-mode-hook (lambda ()
                                    (define-key reftex-toc-mode-map (kbd "<return>") 'reftex-toc-goto-line)))
  )
