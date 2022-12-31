;; -*- lexical-binding: t; -*-

;; https://github.com/hlissner/doom-emacs/blob/master/modules/lang/latex/README.org
(setq +latex-viewers
      '(okular atril evince zathura))

;; https://emacs.stackexchange.com/questions/33663/in-auctex-how-could-i-fold-acronyms
(after! tex-fold
  (setq TeX-fold-macro-spec-list
        (append TeX-fold-macro-spec-list
                '(("{1}" ("gls"))         ; used in l0mdt
                  ("{1}" ("cite"))        ; used in l0mdt
                  ("{1}" ("yearsago"))    ; used in resume
                  ("{1}" ("heading")))))) ; used in resume

(defun tex-bold ()
  "Make the current TeX selection bold."
  (interactive)
  (TeX-font nil 2))

(defun tex-italic ()
  "Make the current TeX selection italic."
  (interactive)
  (TeX-font nil 9))

(defun tex-tt ()
  "Make the current TeX selection italic."
  (interactive)
  (TeX-font nil 20))

(setq TeX-fold-auto t)

;; LaTeX-mode-hook is used by AUCTeX's LaTeX mode.
;; latex-mode-hook is used by Emacs' built-in latex mode.

;; https://www.flannaghan.com/2013/01/11/tex-fold-mode
(add-hook 'TeX-mode-hook
          (lambda ()
            (TeX-fold-mode 1)
            ;; (add-hook 'after-change-functions 'TeX-fold-paragraph t t)
            (add-hook 'find-file-hook 'TeX-fold-buffer t t)
            (add-hook 'after-save-hook 'TeX-fold-buffer t t)))

;; (define-key evil-visual-state-map (kbd "C-b") 'tex-bold)
;; (define-key vil-tex-mode-map (kbd "C-S-b") 'tex-bold)

(add-hook
 'LaTex-mode-hook
 (lambda () (flycheck-add-next-checker 'proselint 'tex-chktex)))

;; this is some e.g. text that has an example in it. And some other sentence that is actually a sentence. Something else.

;; Semantic Linefeeds
;;------------------------------------------------------------------------------
;; https://abizjak.github.io/emacs/2016/03/06/latex-fill-paragraph.html
;; TODO: check for common things at end of line:
;; c.f. e.g. i.e.

(after! evil
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
  (evil-define-key 'normal latex-mode-map (kbd "M-q") nil))

;; TeX
;;------------------------------------------------------------------------------

(after! tex
  (setq reftex-toc-split-windows-horizontally t)
  (setq reftex-toc-split-windows-fraction 0.15)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook (lambda () (reftex-mode 1)))
  (add-hook 'reftex-toc-mode-hook
            (lambda ()
              (define-key reftex-toc-mode-map (kbd "<return>") 'reftex-toc-goto-line)))

  ;; Electric Space
  ;;------------------------------------------------------------------------------

  (defun electric-space ()              ; Trying to get Emacs to do semantic linefeeds
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
