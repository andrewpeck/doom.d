;;------------------------------------------------------------------------------
;; LaTeX
;;------------------------------------------------------------------------------

;;;###autoload
(defun texify-quotation-marks ()
  (interactive)
  ;; TODO: should add some feature to skip forward?

  (save-excursion
    (goto-char (point-min))

    "find a quote mark"
    (while (re-search-forward "\"" nil t)

      (let* ((quote-pos (- (point) 1))
             (next-quote-pos nil))

        (save-excursion
          (when (re-search-forward "\"" nil t)
            (setq next-quote-pos (point))))

        (when (and quote-pos next-quote-pos)

          (lazy-highlight-cleanup)
          (let ((overlay (make-overlay quote-pos next-quote-pos)))
            (overlay-put overlay 'face '(ffap bold)))

          (unwind-protect
              (progn

                ;; Opening quote
                (goto-char quote-pos)
                (overlay-put (make-overlay (point) (1+ (point)))'face '(isearch))
                (when (yes-or-no-p "Replace opening quote?")
                  (delete-char 1)
                  (insert "``")

                  (goto-char next-quote-pos)
                  (overlay-put (make-overlay (point) (1+ (point)))'face '(isearch))
                  (when (yes-or-no-p "Replace closing quote?")
                    (delete-char 1)
                    (insert "''"))))

            (remove-overlays quote-pos next-quote-pos)))))))

;;;###autoload
(defvar default-tex-master nil)

;;;###autoload
(defun my/set-default-tex-master ()
  "Set the master tex file for the current project."
  (interactive)
  ;; get master file from user
  (let ((master-file
         (completing-read "Master File: "
                          (cl-remove-if-not (lambda (f) (string= "tex" (file-name-extension f)))
                                            (project-files (project-current))))))
    (setq default-tex-master master-file)
    (setq TeX-master master-file)))   ; execute now to take effect immediately

;;;###autoload
(defun TeX-toggle-folding ()
  (interactive)
  (call-interactively #'TeX-fold-mode)
  (if TeX-fold-mode
      (TeX-fold-buffer)
    (TeX-fold-clearout-buffer)))

;;;###autoload
(defun tex-link-insert ()
  "Insert TeX href link"
  (interactive)
  (let* ((url-at-point (thing-at-point 'url))
         (text-at-point (when (and (region-active-p)
                                   (not url-at-point))
                          (buffer-substring-no-properties
                           (region-beginning)
                           (region-end)))))
    (let ((url (read-string "URL: " url-at-point))
          (text (read-string (format "Text: ") text-at-point)))
      (when (and url text)

        (when (region-active-p)
          (delete-region (region-beginning)
                         (region-end)))

        (when url-at-point
          (let ((bounds (bounds-of-thing-at-point 'url)))
            (delete-region (car bounds)
                           (cdr bounds))))

        (insert (format "\\href{%s}{%s}" url text))))))

;;;###autoload
(defun reftex-toc-set-max-level ()
  (interactive)
  (let ((level
         (read-number "Level: " reftex-toc-max-level)))
    (setq reftex-toc-max-level level))
  (reftex-toc-Rescan))

;;;###autoload
(defun tex-expand-and-insert (macro)
  (interactive)
  (when (not (region-active-p))
    (er/mark-word))
  (TeX-insert-macro macro))

;;;###autoload
(defun tex-underline ()
  "Make the current TeX selection bold."
  (interactive)
  (tex-expand-and-insert "underline"))

;;;###autoload
(defun tex-bold ()
  "Make the current TeX selection bold."
  (interactive)
  (tex-expand-and-insert "textbf"))

;;;###autoload
(defun tex-italic ()
  "Make the current TeX selection italic."
  (interactive)
  (tex-expand-and-insert "textit"))

;;;###autoload
(defun tex-tt ()
  "Make the current TeX selection typewriter."
  (interactive)
  (tex-expand-and-insert "texttt"))

;;;###autoload
(defun tex-glossarify ()
  "Make the current TeX selection a glossary entry."
  (interactive)
  (tex-expand-and-insert "gls"))

;;;###autoload
(defun tex-Glossarify ()
  "Make the current TeX selection a Glossary entry."
  (interactive)
  (tex-expand-and-insert "Gls"))

;;;###autoload
(defun hook/modify-latex-hyphen-syntax ()
  "Treat hyphenated words as one."
  (modify-syntax-entry ?- "w"))

;;;###autoload
(defun electric-space ()        
  "Trying to get Emacs to do semantic linefeeds."
  (interactive)
  (if (looking-back (sentence-end) nil)
      (insert "\n")
    (self-insert-command 1)))

(defvar electric-space-on-p nil)

;;;###autoload
(defun toggle-electric-space ()
  (interactive)
  (global-set-key
   " "
   (if (setopt electric-space-on-p
               (not electric-space-on-p))
       'electric-space
     'self-insert-command)))

(provide 'my-tex)
;; Local Variables:
;; eval: (make-variable-buffer-local 'write-contents-functions)
;; eval: (add-hook 'write-contents-functions (lambda () (loaddefs-generate "~/.doom.d/my-autoloads/" "~/.doom.d/loaddefs.el")))
;; End:
