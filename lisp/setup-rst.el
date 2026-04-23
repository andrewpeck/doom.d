;; Restructured Text -*- lexical-binding: t; -*-

(use-package rst
  :config

  (defvar-local my/rst-preview--pending-heading nil
    "Heading to jump to after eww finishes rendering.")

  (defun my/rst-preview--current-heading ()
    "Return the text of the nearest rst section heading at or before point."
    (save-mark-and-excursion
      (rst-mark-section)
      (string-trim (buffer-substring-no-properties
                    (line-beginning-position)
                    (line-end-position)))))


  (defun my/rst-preview--restore-hook ()
    "Jump to the pending heading after eww finishes rendering."
    (when my/rst-preview--pending-heading
      (my/rst-preview--jump-to-anchor my/rst-preview--pending-heading)
      (setq my/rst-preview--pending-heading nil)))

  (defun my/rst-preview ()
    "Convert the current rst buffer to HTML using rst2html and preview it in eww.
If the eww preview buffer is already open, reload it (making it visible if needed).
Scrolls the eww buffer to the current section heading. Focus stays in the rst buffer."
    (interactive)
    (unless (buffer-file-name)
      (user-error "Buffer is not visiting a file"))
    (unless (string-suffix-p ".rst" (buffer-file-name))
      (user-error "Buffer does not appear to be an RST file"))
    (save-buffer)
    (let* ((rst-file     (buffer-file-name))
           (html-file    (concat (file-name-sans-extension rst-file) ".html"))
           (eww-buf-name (format "*eww: %s*" (file-name-nondirectory html-file)))
           (existing-buf (get-buffer eww-buf-name))
           (origin-win   (selected-window))
           (heading      (my/rst-preview--current-heading)))
      (let ((exit-code (call-process "rst2html" nil nil nil rst-file html-file)))
        (if (not (zerop exit-code))
            (user-error "rst2html failed with exit code %d" exit-code)
          (let* ((buf     (or existing-buf (generate-new-buffer eww-buf-name)))
                 (eww-win (display-buffer buf)))
            (with-selected-window eww-win
              (setq my/rst-preview--pending-heading heading)
              (add-hook 'eww-after-render-hook #'my/rst-preview--restore-hook nil t)
              (if existing-buf
                  (eww-reload)
                (eww-mode)
                (eww (concat "file://" html-file)))))
          (select-window origin-win)))))

  (defun my/rst-preview--clean-heading (heading)
    "Strip rst inline markup from HEADING to match eww's rendered text."
    (let* ((clean (replace-regexp-in-string "[*`]" "" heading))
           (clean (string-trim clean)))
      clean))
  (defun my/rst-preview--jump-to-anchor (heading)
    "Jump to HEADING in the current eww buffer."
    (let ((case-fold-search t)
          (clean (my/rst-preview--clean-heading heading)))
      (goto-char (point-min))
      (when clean
        (re-search-forward (concat "^" (regexp-quote clean) "$") nil t)))
    (beginning-of-line)
    (recenter 10)
    (pulse-momentary-highlight-region (line-beginning-position)
                                      (line-end-position)))



  (defun rst/mark-rst-symbol ()
    "Mark a reStructuredText symbol (including underscores)."
    (interactive)
    (let ((symbol-chars "[:alnum:]_"))
      (skip-chars-backward symbol-chars)
      (set-mark (point))
      (skip-chars-forward symbol-chars)))

  (defun rst/surround-region (str)
    "Surround the active region with STR on both sides."
    (interactive "sSurround with: ")
    (when (use-region-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (save-excursion
          (goto-char end)
          (insert str)
          (goto-char beg)
          (insert str)))))

  (defun rst-tt ()
    "Make the current TeX selection bold."
    (interactive)
    (unless (region-active-p)
      (rst/mark-rst-symbol))
    (rst/surround-region "``"))

  (require 'line-fill)
  (map! :map rst-mode-map
        "C-c C-p" #'my/rst-preview
        "M-q" #'line-fill-paragraph
        :localleader (
                      :desc "Code"  "tt" #'rst-tt)))
