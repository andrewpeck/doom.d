;; -*- lexical-binding: t; -*-

(map! :map verilog-mode-map
      :after verilog-mode
      ;; unbind electric tab insertion
      "<TAB>" nil
      :i "<return>" #'electric-verilog-terminate-and-indent
      ;; unbind verilog electric backspace
      :nm "<backspace>" #'buffer-flip)

(use-package verilog-port-copy
  :after verilog)

(use-package verilog-mode

  :mode ("\\.v\\'" "\\.sv\\'" "\\.svh\\'")

  :init
  
  (add-hook 'verilog-mode-hook
            (lambda ()
              (progn
                (require 'rainbow-delimiters)
                (setq rainbow-delimiters--font-lock-keywords
                      '(rainbow-delimiters--propertize-verilog))
                (rainbow-delimiters-mode-enable))))

  (add-hook 'verilog-mode-hook
            (defun hook/verilog-beautify-symbols-hook ()
              "Beautify Verilog Symbols"
              (setq prettify-symbols-alist
                    '(("begin" . "《")
                      ("end"   . "⟫")
                      ("function" . "󰡱")))
              (prettify-symbols-mode)))

  :config

  (require 'verilog-ts-mode)
  (add-hook 'verilog-mode-hook (lambda () (when (eq major-mode 'verilog-mode)
                                            (verilog-ts-mode))))

  (defun rainbow-delimiters--apply-color-range (start end depth match)
    "Highlight a single delimiter at LOC according to DEPTH.

LOC is the location of the character to add text properties to.
DEPTH is the nested depth at LOC, which determines the face to use.
MATCH is nil iff it's a mismatched closing delimiter."
    (let ((face (funcall rainbow-delimiters-pick-face-function depth match start)))
      (when face
        (font-lock-prepend-text-property start end 'font-lock-face face))))

  (defun rainbow-delimiters--propertize-verilog (_)
    "Highlight delimiters in region between point and END.

Used by font-lock for dynamic highlighting."

    (interactive)
    (when (eq major-mode 'verilog-mode)
      (goto-char (point-min))
      (let* ((depth 0)
             (_ (point-max)))

        ;; find begin-end pairs in the whole doc
        ;; should probably operate on a range in case the document is large :(
        ;; but for now this works...
        (while (re-search-forward "\\<begin\\>\\|\\<end\\>" nil t)

          ;; skip if we are in a comment
          (unless (nth 4 (syntax-ppss))

            (let* ((delim-start (match-beginning 0))
                   (delim-end (match-end 0))
                   (delim-word (match-string-no-properties 0)))

              (when (string= delim-word "begin")
                (setq depth (1+ depth)))

              (rainbow-delimiters--apply-color-range delim-start delim-end depth (> depth 0))

              (when (string= delim-word "end")
                (setq depth (1- depth)))

              (when (= depth -1)
                (setq depth 0)))))))
    ;; We already fontified the delimiters, tell font-lock there's nothing more
    ;; to do.
    nil)


  (setopt verilog-align-ifelse t
          verilog-tab-always-indent nil
          ;; Regexp that matches user typedefs for declaration alignment.
          verilog-align-typedef-regexp (concat "\\<" verilog-identifier-re "_\\(t\\)\\>")
          verilog-auto-delete-trailing-whitespace t
          verilog-auto-inst-param-value t
          verilog-indent-lists nil ;; Fix the dumb indentation inside of port lists
          verilog-auto-inst-vector nil
          verilog-auto-lineup (lambda () nil)
          verilog-auto-newline nil
          verilog-auto-save-policy nil
          verilog-auto-template-warn-unused t
          verilog-highlight-grouping-keywords t
          verilog-highlight-modules t
          verilog-case-indent 2
          verilog-cexp-indent 2
          verilog-indent-level 2
          verilog-indent-level-behavioral 2
          verilog-indent-level-declaration 2
          verilog-indent-level-module 2
          verilog-tab-to-comment nil)

  (defun advice/restore-verilog-position (orig &rest args)
    "This saves the column before indenting, and if the cursor was at or
  past the indentation (i.e., in the actual content, not in the leading
  whitespace), it restores the column after."
    (let ((col (current-column))
          (boi (save-excursion (back-to-indentation) (current-column))))
      (apply orig args)
      (when (>= col boi)
        (move-to-column col))))

  (advice-add 'verilog-indent-line-relative :around #'advice/restore-verilog-position)

  ;; copy in an old version of verilog-pretty-expr before things got broken
  (defun verilog-pretty-expr (&optional quiet)
    "Line up expressions around point.
If QUIET is non-nil, do not print messages showing the progress of line-up."
    (interactive)
    (let* ((basic-complete-pretty-expr-re (if verilog-align-assign-expr
                                              verilog-basic-complete-expr-no-assign-re
                                            verilog-basic-complete-expr-re))
           (complete-pretty-expr-re (concat verilog-extended-complete-re "\\|\\(" basic-complete-pretty-expr-re "\\)"))
           (discard-re (concat "^\\s-*\\(" complete-pretty-expr-re "\\)"))
           rstart rend)
      (save-excursion
        (when (region-active-p)
          (setq rstart (region-beginning))
          (setq rend (region-end))
          (goto-char rstart))
        (unless (verilog-in-comment-or-string-p)
          (beginning-of-line)
          (when (and (verilog--pretty-expr-assignment-found discard-re)
                     (save-excursion
                       (goto-char (match-end 2))
                       (and (not (verilog-in-attribute-p))
                            (not (verilog-in-comment-or-string-p)))))
            (let* ((start (cond (;; Using region
                                 (region-active-p)
                                 rstart)
                                (;; Parameter list
                                 (verilog-in-parenthesis-p)
                                 (progn
                                   (verilog-backward-up-list 1)
                                   (forward-char)
                                   (verilog-re-search-forward verilog-assignment-operation-re-2 nil 'move)
                                   (goto-char (match-beginning 0))
                                   (point)))
                                (t ;; Declarations
                                 (save-excursion ; BOL of the first line of the assignment block
                                   (beginning-of-line)
                                   (let ((pt (point)))
                                     (verilog-backward-syntactic-ws)
                                     (beginning-of-line)
                                     (while (and (verilog--pretty-expr-assignment-found discard-re)
                                                 (not (bobp)))
                                       (setq pt (point))
                                       (verilog-backward-syntactic-ws)
                                       (beginning-of-line)) ; Ack, need to grok `define
                                     pt)))))
                   (startpos (set-marker (make-marker) start))
                   (end (cond (;; Using region
                               (region-active-p)
                               (verilog--pretty-expr-find-end discard-re rend))
                              (;; Parameter list
                               (verilog-in-parenthesis-p)
                               (verilog--pretty-expr-find-end))
                              (t ;; Declarations
                               (verilog--pretty-expr-find-end discard-re))))
                   (endpos (set-marker (make-marker) end))
                   (contains-2-char-operator (string-match "<=" (buffer-substring-no-properties start end))))
              ;; Start with alignment
              (goto-char startpos)
              (unless (save-excursion
                        (beginning-of-line)
                        (looking-at discard-re))
                (verilog-do-indent (verilog-calculate-indent)))
              (when (and (not quiet)
                         (> (- (marker-position endpos) (marker-position startpos)) 100))
                (message "Lining up expressions.. (please stand by)"))
              ;; Set indent to minimum throughout region
              ;; Rely on mark rather than on point as the indentation changes can
              ;; make the older point reference obsolete
              (while (< (point) (marker-position endpos))
                (beginning-of-line)
                (save-excursion
                  (if (looking-at verilog-complete-re)
                      (progn (goto-char (marker-position startpos))
                             (verilog-just-one-space verilog-assignment-operation-re-2))
                    (verilog-just-one-space verilog-assignment-operation-re)))
                (verilog-do-indent (verilog-calculate-indent))
                (end-of-line)
                (verilog-forward-syntactic-ws))

              (let ((ind (verilog-get-lineup-indent-2 verilog-assignment-operation-re (marker-position startpos) (marker-position endpos))) ; Find the biggest prefix
                    e)
                ;; Now indent each line.
                (goto-char (marker-position startpos))
                (while (progn
                         (setq e (marker-position endpos))
                         (> e (point)))
                  (unless quiet
                    (message " verilog-pretty-expr: %d" (- e (point))))
                  (setq e (point))
                  (cond
                   ((or (looking-at verilog-assignment-operation-re)
                        (and (verilog-in-parenthesis-p)
                             (looking-at verilog-assignment-operation-re-2)))
                    (goto-char (match-beginning 2))
                    (unless (or (and (verilog-in-parenthesis-p) ; Leave attributes and comparisons alone
                                     (save-excursion ; Allow alignment of some expressions inside param/port list
                                       (verilog-backward-up-list 1)
                                       (verilog-beg-of-statement-1)
                                       (not (looking-at verilog-defun-level-re))))
                                (verilog-in-coverage-p))
                      (if (and contains-2-char-operator
                               (eq (char-after) ?=))
                          (indent-to (1+ ind)) ; Line up the = of the <= with surrounding =
                        (indent-to ind)))
                    (forward-line 1))
                   ((and (save-excursion

                           (verilog-forward-syntactic-ws)
                           (not (looking-at verilog-complete-re)))

                         ;; AP:: added this condition to not indent non-trailing comments when lining up declaration
                         ;; previously something like this:
                         ;;
                         ;;    int A = 6,
                         ;;    // some comment
                         ;;    int B = 2,
                         ;;
                         ;; would get aligned to
                         ;;
                         ;;    int A = 6,
                         ;;          // some comment
                         ;;    int B = 2,
                         ;;
                         ;; consider filing a bug-- reproduce w/ vanilla emacs first
                         ;; it seems like from the comment below (the t condition) that this is not intended behavior

                         (save-excursion
                           (beginning-of-line)
                           (not (looking-at "^[[:space:]]*\/\/.*")))

                         (verilog-continued-line-1 (marker-position startpos)))
                    (goto-char e)
                    (indent-line-to ind)

                    (forward-line 1))
                   (t             ; Must be comment, white space or syntax error
                    (goto-char e)
                    (forward-line 1))))
                ;; Align comments if enabled
                (when verilog-align-decl-expr-comments
                  (verilog-align-comments startpos endpos))
                (unless quiet
                  (message ""))))))))))
