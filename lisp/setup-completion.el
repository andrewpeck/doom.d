;;------------------------------------------------------------------------------
;; Cape
;;------------------------------------------------------------------------------

(use-package! corfu

  ;; cape-dabbrev: Complete word from current buffers. See also dabbrev-capf on Emacs 29.
  ;; cape-elisp-block: Complete Elisp in Org or Markdown code block.
  ;; cape-file: Complete file name.
  ;; cape-history: Complete from Eshell, Comint or minibuffer history.
  ;; cape-keyword: Complete programming language keyword.
  ;; cape-symbol: Complete Elisp symbol.
  ;; cape-abbrev: Complete abbreviation (add-global-abbrev, add-mode-abbrev).
  ;; cape-dict: Complete word from dictionary file.
  ;; cape-line: Complete entire line from current buffer.
  ;; cape-tex: Complete Unicode char from TeX command, e.g. \hbar.
  ;; cape-sgml: Complete Unicode char from SGML entity, e.g., &alpha.
  ;; cape-rfc1345: Complete Unicode char using RFC 1345 mnemonics.

  :config

  (define-key global-map (kbd "TAB") #'corfu-complete)
  (define-key corfu-map (kbd "TAB") #'corfu-complete)

  (setopt +corfu-want-tab-prefer-navigating-org-tables nil)

  (add-hook! 'TeX-mode-hook
    (setq-local cape-file-directory (vc-root-dir)
                cape-file-prefix "file:"))

  (add-hook! 'hog-src-mode-hook
    (setq-local cape-file-prefix nil)
    (setq-local cape-file-directory (vc-root-dir))
    (setq-local completion-at-point-functions (list 'cape-file 'cape-dabbrev)))

  (setq corfu-auto-delay 0.3
        corfu-auto-prefix 5
        ;; corfu-on-exact-match 'show
        ;; corfu-preselect 'prompt         ; prompt first valid directory

        ;; corfu-preview-current nil
        ;; No preview vs Non-inserting preview
        ;;
        ;; If t, check all other buffers (subject to dabbrev ignore rules).
        ;; Any other non-nil value only checks some other buffers, as per
        ;; dabbrev-select-buffers-function.
        cape-dabbrev-check-other-buffers nil)

  :init

  (add-hook! 'verilog-mode-hook
    (defun hook/add-verilog-keywords ()
      (with-eval-after-load 'cape-keyword
        (add-to-list 'cape-keyword-list
                     (append '(verilog-mode) verilog-keywords)))))

  (add-hook! 'vhdl-mode-hook
    (defun hook/add-vhdl-keywords ()
      (with-eval-after-load 'cape-keyword
        (add-to-list 'cape-keyword-list
                     (append '(vhdl-mode)
                             vhdl-keywords
                             vhdl-types
                             vhdl-attributes
                             vhdl-enum-values
                             vhdl-constants
                             vhdl-functions
                             vhdl-packages
                             vhdl-directives)))))

  (add-hook! 'LaTeX-mode-hook
    (defun hook/set-latex-capf-functions ()
      (setq-local completion-at-point-functions
                  (list
                   ;; 'lsp-completion-at-point
                   'citar-capf
                   ;; 'TeX--completion-at-point
                   ;; 'LaTeX--arguments-completion-at-point
                   'cape-tex
                   'yasnippet-capf
                   'cape-dabbrev
                   'cape-file))))

  (add-hook! 'emacs-lisp-mode-hook
    (defun hook/set-elisp-capf-functions ()
      (setq-local completion-at-point-functions
                  (list
                   ;; (cape-company-to-capf #'company-yasnippet)
                   'yasnippet-capf
                   'cape-elisp-symbol
                   'cape-keyword
                   'cape-dabbrev
                   'cape-history
                   'cape-file))))

  (add-hook! 'verilog-mode-hook
    (defun hook/set-verilog-capf ()
      (setq-local completion-at-point-functions
                  (list (cape-capf-super
                         'cape-dabbrev
                         'cape-keyword
                         'yasnippet-capf)))))

  (add-hook! 'vhdl-mode-hook
    (defun hook/set-vhdl-capf ()
      (setq-local completion-at-point-functions
                  (list (cape-capf-super
                         'cape-dabbrev
                         'cape-keyword
                         'yasnippet-capf)))))

  (dolist (mode '(python-ts-mode-hook python-mode-hook))
    (add-hook mode
              (defun hook/set-capf ()
                (setq-local completion-at-point-functions
                            (list
                             (cape-capf-super
                              'eglot-completion-at-point
                              'cape-keyword
                              'cape-file
                              'cape-dabbrev
                              'yasnippet-capf))))))

  (add-hook! 'tcl-mode-hook
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       'cape-dabbrev
                       'cape-keyword
                       'cape-file
                       'yasnippet-capf))))

  (add-hook! 'tcl-mode-hook
    (with-eval-after-load 'cape-keyword
      (add-to-list 'cape-keyword-list
                   (append '(tcl-mode)

                           ;; vivado
                           '("set_property" "add_files" "generate_target"
                             "report_utilization"
                             "report_timing_summary"
                             "import_ip" "create_project"
                             "get_files" "get_clocks" "get_cells" "get_pins" "get_ports"
                             "get_nets" "font-lock-builtin-face" "create_generated_clock"
                             "create_clock" "set_input_jitter" "set_input_delay" "set_output_delay"
                             "set_property" "set_clock_groups" "set_multicycle_path" "set_false_path"
                             "set_max_delay" "create_pblock" "add_cells_to_pblock" "resize_pblock")

                           tcl-keyword-list
                           tcl-typeword-list
                           tcl-builtin-list)))))
