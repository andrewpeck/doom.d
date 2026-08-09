;; Restructured Text -*- lexical-binding: t; -*-

(use-package rst
  :config

  (require 'ucs-normalize)

  (defun my/rst-preview--slugify (text)
    "Compute the docutils section anchor for TEXT.
Mirrors `docutils.nodes.make_id': lowercase, NFKD-strip non-ASCII,
replace runs of non `[a-z0-9]' with `-', and strip leading
hyphens/digits and trailing hyphens."
    (let* ((s (downcase text))
           (s (with-temp-buffer
                (insert s)
                (ucs-normalize-NFKD-region (point-min) (point-max))
                (buffer-string)))
           (s (replace-regexp-in-string "[^[:ascii:]]" "" s))
           (s (replace-regexp-in-string "[^a-z0-9]+" "-" s))
           (s (replace-regexp-in-string "\\`[-0-9]+" "" s))
           (s (replace-regexp-in-string "-+\\'" "" s)))
      s))

  (defun my/rst-preview--adornment-line-p ()
    "Non-nil if the current line is an rst section adornment line."
    (save-excursion
      (beginning-of-line)
      (looking-at-p "\\([^[:alnum:][:space:]]\\)\\1+[ \t]*$")))

  (defun my/rst-preview--title-at-point ()
    "Return title text if the current line begins a section title, else nil.
Handles `title + underline' (point on title line) and
`overline + title + underline' (point on overline)."
    (save-excursion
      (beginning-of-line)
      (cond
       ((my/rst-preview--adornment-line-p)
        (when (and (zerop (forward-line 1))
                   (not (my/rst-preview--adornment-line-p))
                   (not (looking-at-p "[ \t]*$")))
          (let ((title (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position))))
            (when (and (zerop (forward-line 1))
                       (my/rst-preview--adornment-line-p))
              (string-trim title)))))
       ((not (looking-at-p "[ \t]*$"))
        (let ((title (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position))))
          (when (and (zerop (forward-line 1))
                     (my/rst-preview--adornment-line-p))
            (string-trim title)))))))

  (defun my/rst-preview--current-title ()
    "Return the rst section title at or before point, or nil."
    (let ((target (line-number-at-pos))
          (last nil))
      (save-excursion
        (goto-char (point-min))
        (while (and (not (eobp))
                    (<= (line-number-at-pos) target))
          (let ((title (my/rst-preview--title-at-point)))
            (cond
             (title (setq last title) (forward-line 2))
             (t     (forward-line 1))))))
      last))

  (defun my/rst-preview ()
    "Convert the current rst buffer to HTML via rst2html and preview in eww.
Jumps to the section containing point using the docutils-generated
HTML anchor. If the eww preview buffer already exists, reuse it.
Focus stays in the rst buffer."
    (interactive)
    (unless (and buffer-file-name (string-suffix-p ".rst" buffer-file-name))
      (user-error "Buffer does not appear to be an RST file"))
    (save-buffer)
    (let* ((rst-file     (buffer-file-name))
           (html-file    (concat (file-name-sans-extension rst-file) ".html"))
           (title        (my/rst-preview--current-title))
           (anchor       (and title (my/rst-preview--slugify title)))
           (url          (concat "file://" html-file
                                 (if (and anchor (not (string-empty-p anchor)))
                                     (concat "#" anchor)
                                   "")))
           (eww-buf-name (format "*eww: %s*" (file-name-nondirectory html-file)))
           (existing-buf (get-buffer eww-buf-name))
           (origin-win   (selected-window))
           (exit-code    (call-process "rst2html" nil nil nil rst-file html-file)))
      (unless (zerop exit-code)
        (user-error "rst2html failed with exit code %d" exit-code))
      (let* ((buf     (or existing-buf (generate-new-buffer eww-buf-name)))
             (eww-win (display-buffer buf)))
        (with-selected-window eww-win
          (unless existing-buf (eww-mode))
          (eww url)))
      (select-window origin-win)))



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
