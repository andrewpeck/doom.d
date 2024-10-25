;;------------------------------------------------------------------------------
;; Verilog
;;------------------------------------------------------------------------------

(use-package! verilog-mode

  :mode ("\\.v\\'" "\\.sv\\'" "\\.svh\\'")

  :init

  (add-hook 'verilog-mode-hook
            (defun hook/verilog-beautify-symbols-hook ()
              "Beautify Verilog Symbols"
              (setq prettify-symbols-alist
                    '(("begin" . "《")
                      ("end"   . "⟫")
                      ("function" . "󰡱")))
              (prettify-symbols-mode)))

  ;; (font-lock-add-keywords
  ;;  'verilog-mode
  ;;  '(("\\(always_ff @(posedge \\)"
  ;;     1 '(face nil display  "󰁥(pos "))))

  ;; (font-lock-add-keywords
  ;;  'verilog-mode
  ;;  '(("\\(always_ff @(negedge \\)"
  ;;     1 '(face nil display  "󰁥(neg "))))


  (add-hook 'verilog-mode-hook
            (defun hook/verilog-configure-indent-and-comment ()
              "Wrap verilog-do-indent in a save excursion so it doesn't jump around.... uhg"
              (setq-local indent-line-function
                          (lambda ()
                            (if (eq evil-state 'normal)
                                (verilog-indent-line)
                              (save-excursion (verilog-indent-line-relative)))))
              ;; (setq-local indent-line-function #'verilog-indent-line)
              (setq-local comment-multi-line t)))

  ;; (add-hook 'verilog-mode-hook
  ;;           (defun hook/set-fill-prefix ()
  ;;             "Set the verilog fill prefix."
  ;;             (setq-local fill-prefix "// ")))

  :config

  (evil-define-key '(motion normal) 'verilog-mode-map
    (kbd "=") #'indent-for-tab-command)

  (defun verilog-indent-line ()
    "Indent for special part of code."
    (interactive)

    (let ((pt (point))
          (at-beginning (equal (current-column) 0)))
      (when (not at-beginning)
        (beginning-of-line))
      (verilog-do-indent (verilog-calculate-indent))
      (when (not at-beginning)
        (goto-char pt)))

    nil)

  (define-key verilog-mode-map (kbd  "<return>") #'electric-verilog-terminate-and-indent)
  (define-key verilog-mode-map (kbd "TAB") nil)
  (define-key verilog-mode-map (kbd "TAB") (lambda () nil))
  (define-key verilog-mode-map (kbd "<backspace>") nil)

  (evil-define-key '(motion normal insert visual) verilog-mode-map
    (kbd "TAB") (lambda () (interactive) nil))

  (require 'verilog-port-copy)

  (defun yosys-make-schematic ()
    "Make a yosys schematic from the module at point.

TODO: need to process dependencies.
TODO: use something less intrusive and focus stealing than shell-command (e.g. comint)
TODO: catch errors, e.g. if read-module-name fails

"

    (interactive)
    (let* ((module (verilog-read-module-name))
           (sources (buffer-file-name))
           (cmd
            (concat "yosys -p " "\"plugin -i systemverilog; read_systemverilog "
                    sources " ; "
                    "proc; stat; synth -top "
                    module ";"
                    " ; write_json -compat-int netlist.json; ltp; stat\""
                    (format
                     "&& netlistsvg netlist.json -o %s.svg && xdg-open %s.svg" module module))))
      (shell-command cmd)))


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
                  (message "")))))))))

  (define-key verilog-mode-map (kbd "RET") nil)
  (define-key verilog-mode-map (kbd "TAB") nil)
  (define-key verilog-mode-map (kbd "<backspace>") nil)

  (defun verilog-name-to-port-inst ()
    "Convert symbol at point into a verilog port instantiation.

e.g. if you place the point at `outcome_cycle_idx_0' in the
following line and execute this function:

output reg [TBINB-1:0]       outcome_cycle_idx_0,

it will be transformed into:

.outcome_cycle_idx_0 (outcome_cycle_idx_0)


This makes for easy conversion of some port list or wire list
into Verilog ports."

    (interactive)
    (let ((name (symbol-at-point)))
      (beginning-of-line)
      (kill-line)
      (insert (format ".%s (%s)," name name))
      (verilog-indent-line)
      (re-search-forward (format "%s" name))
      (re-search-forward (format "%s" name))))

  (setq verilog-align-ifelse t
        verilog-tab-always-indent nil
        ;; Regexp that matches user typedefs for declaration alignment.
        verilog-align-typedef-regexp (concat "\\<" verilog-identifier-re "_\\(t\\)\\>")
        verilog-auto-delete-trailing-whitespace t
        verilog-auto-inst-param-value t
        verilog-indent-lists nil ;; Fix the dumb indentation inside of port lists
        verilog-auto-inst-vector nil
        verilog-auto-lineup (quote all)
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
        verilog-tab-to-comment nil))

;;------------------------------------------------------------------------------
;; VHDL Mode
;;------------------------------------------------------------------------------

(use-package! vhdl-mode

  :config

  (require 'er-basic-expansions)
  (require 'flycheck)

  ;; vhdl mode will wrap comments after some # of characters
  (setq vhdl-end-comment-column 200
        vhdl-standard '(08)
        vhdl-platform-spec nil
        vhdl-prompt-for-comments nil
        flycheck-ghdl-language-standard "08")

  (defun vhdl-unsigned->slv ()
    "Convert a VHDL unsigned to standard logic vector."
    (interactive)
    (when (not (region-active-p))
      (er/mark-symbol))
    (let ((sig (buffer-substring-no-properties (mark) (point))))
      (delete-region (mark) (point))
      (insert (format  "std_logic_vector(%s)" sig))
      (backward-char 1))
    (when (functionp 'evil-insert)
      (evil-insert 0)))

  (defun vhdl-int->slv ()
    "Convert a VHDL integer to standard logic vector."
    (interactive)
    (when (not (region-active-p))
      (er/mark-symbol))
    (let ((sig (buffer-substring-no-properties (mark) (point))))
      (delete-region (mark) (point))
      (insert (format  "std_logic_vector(to_unsigned(%s, ))" sig))
      (backward-char 2))
    (when (functionp 'evil-insert)
      (evil-insert 0)))

  (defun vhdl-slv->int ()
    "Convert a VHDL standard logic vector to integer."
    (interactive)
    (when (not (region-active-p))
      (er/mark-symbol))
    (let ((sig (buffer-substring-no-properties (mark) (point))))
      (delete-region (mark) (point))
      (insert (format  "to_integer(unsigned(%s))" sig))))

  (defun vhdl-slv->unsigned ()
    "Convert a VHDL standard logic vector to unsigned."
    (interactive)
    (when (not (region-active-p))
      (er/mark-symbol))
    (let ((sig (buffer-substring-no-properties (mark) (point))))
      (delete-region (mark) (point))
      (insert (format  "unsigned(%s)" sig))
      (backward-char 1))))
