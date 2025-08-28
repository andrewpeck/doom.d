;; -*- lexical-binding: t; -*-

;;------------------------------------------------------------------------------
;; Completion Preview
;;------------------------------------------------------------------------------

(use-package completion-preview-mode
  :init
  (global-completion-preview-mode)
  :config
  (unbind-key (kbd "TAB") completion-preview-active-mode-map)
  ;; (evil-define-key '(insert) completion-preview-active-mode-map (kbd "TAB")
  ;;   'completion-preview-insert)
  ;; Org mode has a custom `self-insert-command'
  (push 'org-self-insert-command completion-preview-commands))

(use-package! corfu

  :commands (corfu-complete)

  :config

  ;; for reasons I don't understand, the exit function gets called with no
  ;; candidates resulting in a args out of range 0,0 error when eglot completion
  ;; is performed and there are no candidates
  ;; avoid the error with some small advice
  (advice-add 'corfu--exit-function :before-while
              (lambda (_ _ cands) cands))

  (setq-default
   corfu-preselect 'first

   corfu-auto-delay 0.5
   corfu-auto-prefix 3
   corfu-auto t
   corfu-preview-current t              ; No preview vs Non-inserting preview

   ;; +corfu-want-minibuffer-completion nil
   ;; +corfu-want-tab-prefer-navigating-org-tables t
   ;; +corfu-want-tab-prefer-expand-snippets nil
   ;; +corfu-want-ret-to-confirm t
   )

  :init

  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  (map! (:map corfu-map "SPC" #'corfu-insert-separator))

  ;; (unbind-key (kbd "C-<SPC>") global-map)
  (define-key corfu-mode-map (kbd "C-<SPC>") #'completion-at-point))

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package! cape

  :defer t

  :after corfu

  :custom

  ;; If t, check all other buffers (subject to dabbrev ignore rules).
  ;; Any other non-nil value only checks some other buffers, as per
  ;; dabbrev-select-buffers-function.
  (cape-dabbrev-check-other-buffers nil)

  :config

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

  ;;------------------------------------------------------------------------------
  ;; Hog
  ;;------------------------------------------------------------------------------

  (add-hook! 'hog-src-mode-hook
    (setq-local cape-file-prefix nil)
    (setq-local cape-file-directory (vc-root-dir))
    (setq-local completion-at-point-functions (list #'cape-file #'cape-dabbrev)))

  ;;------------------------------------------------------------------------------
  ;; Python
  ;;------------------------------------------------------------------------------

  (add-hook 'python-mode-hook
            (defun hook/set-python-base-capf ()
              (setq-local completion-at-point-functions
                          (list (cape-capf-super
                                 'eglot-completion-at-point
                                 'python-completion-at-point
                                 'yasnippet-capf
                                 'cape-dabbrev)))))

  ;;------------------------------------------------------------------------------
  ;; Verilog
  ;;------------------------------------------------------------------------------

  (after! verilog-mode
    (add-hook 'verilog-mode-hook
              (defun hook/add-verilog-keywords ()
                (with-eval-after-load 'cape-keyword
                  (add-to-list 'cape-keyword-list
                               (append '(verilog-mode) verilog-keywords))))))

  (add-hook 'verilog-mode-hook
            (defun hook/set-verilog-capf ()
              (setq-local completion-at-point-functions
                          (list (cape-capf-super
                                 #'cape-dabbrev
                                 #'cape-keyword
                                 #'yasnippet-capf)))))

  ;;------------------------------------------------------------------------------
  ;; VHDL
  ;;------------------------------------------------------------------------------

  (add-hook 'vhdl-mode-hook
            (defun hook/add-vhdl-keywords ()
              (require 'vhdl-mode)
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

  (add-hook 'vhdl-mode-hook
            (defun hook/set-vhdl-capf ()
              (setq-local completion-at-point-functions
                          (list (cape-capf-super
                                 #'cape-dabbrev
                                 #'cape-keyword
                                 #'yasnippet-capf)))))

  (add-hook 'makefile-mode-hook
            (lambda () (call-interactively 'makefile-pickup-everything)))
  ;;------------------------------------------------------------------------------
  ;; Tex
  ;;------------------------------------------------------------------------------

  (defun hook/setup-tex-with-corfu ()

    (require 'corfu)
    (require 'cape)

    (setq-local cape-file-directory (locate-dominating-file (directory-file-name (buffer-file-name)) ".git"))
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       ;; 'TeX--completion-at-point
                       ;; 'LaTeX--arguments-completion-at-point
                       #'citar-capf
                       #'yasnippet-capf
                       #'cape-tex
                       #'yasnippet-capf
                       #'cape-dabbrev
                       #'cape-file))))

  (add-hook 'LaTeX-mode-hook 'hook/setup-tex-with-corfu)

  ;; HACK: eglot screws with completion-at-point-functions... usually might not
  ;; care but with latex the eglot completion at point errors so NONE of the
  ;; capfs work when eglot is active. So just remove the eglot capf then re-configure corfu.
  ;;
  ;; This seemingly can't be in the LaTeX mode hook since eglot mode hook is run
  ;; *after* latex mode, so anything in the latex mode hook just get overwritten
  ;; by what happens in eglot setup
  (add-hook 'eglot-managed-mode-hook
            (defun hook/remove-tex-eglot-completion ()
              (when (eq major-mode 'LaTeX-mode)
                (remove-hook 'completion-at-point-functions 'eglot-completion-at-point t)
                (hook/setup-tex-with-corfu))))

  ;;------------------------------------------------------------------------------
  ;; Elisp
  ;;------------------------------------------------------------------------------

  (add-hook 'emacs-lisp-mode-hook
            (defun hook/set-elisp-capf-functions ()
              (setq-local completion-at-point-functions
                          (list
                           ;; (cape-company-to-capf #'company-yasnippet)
                           #'yasnippet-capf
                           #'cape-elisp-symbol
                           #'cape-keyword
                           #'cape-dabbrev
                           #'cape-history
                           #'cape-file))))

  ;;------------------------------------------------------------------------------
  ;; TCL
  ;;------------------------------------------------------------------------------

  (add-hook 'tcl-mode-hook
            (defun hook/set-tcl-cape-capfs ()
              (setq-local completion-at-point-functions
                          (list (cape-capf-super
                                 #'cape-dabbrev
                                 #'cape-keyword
                                 #'cape-file
                                 #'yasnippet-capf)))))

  (add-hook 'tcl-mode-hook
            (defun hook/set-tcl-cape-keywords ()
              (require 'cape-keyword)
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
