;; -*- lexical-binding: t; -*-

;;------------------------------------------------------------------------------
;; Completion Preview
;;------------------------------------------------------------------------------

(use-package! completion-preview
  :hook (doom-first-input . global-completion-preview-mode)
  :config
  (setq completion-preview-idle-delay 0.1)
  ;; (bind-key "TAB" completion-preview-active-mode-map)
  ;; (map! :after corfu (:map completion-preview-active-mode-map "TAB" #'completion-preview-insert))
  ;; Org mode has a custom `self-insert-command'
  (push 'org-self-insert-command completion-preview-commands))

(use-package! corfu


  :commands (corfu-insert-separator corfu-complete)

  :config

  (defun my/completion-preview-hide-when-corfu ()
    "Inhibit the inline completion preview while the Corfu popup is up.
For `completion-preview-inhibit-functions', which only needs a boolean --
`completion-preview--update' hides the preview itself when this wins."
    ;; NOTE: must test visibility, not just the variable: corfu creates
    ;; `corfu--frame' once and only ever makes it invisible, so a bare
    ;; `corfu--frame' check stays true forever after the first popup and kills
    ;; the preview for the rest of the session.
    (and (frame-live-p corfu--frame)
         (frame-visible-p corfu--frame)))

  (add-hook 'completion-preview-inhibit-functions
            #'my/completion-preview-hide-when-corfu)

  (after! eglot
    (defun corfu-debug-eglot ()
      (interactive)
      (setq-local completion-at-point-functions
                  (list (cape-capf-buster (cape-capf-debug #'eglot-completion-at-point))))))

  (map! (:map corfu-map "SPC" #'corfu-insert-separator)
        (:map corfu-map "C-SPC" #'corfu-insert-separator)
        (:map corfu-mode-map "C-c f" #'cape-file)
        (:map corfu-mode-map "C-SPC" #'completion-at-point))

  ;; +corfu-want-minibuffer-completion nil
  ;; +corfu-want-tab-prefer-navigating-org-tables t
  ;; +corfu-want-tab-prefer-expand-snippets nil
  ;; +corfu-want-ret-to-confirm t
  (setopt corfu-preselect 'first
          ;; `corfu-auto-delay'/`corfu-auto-prefix' are deliberately left at
          ;; Doom's values -- they do nothing while `corfu-auto' is nil.
          corfu-auto nil
          corfu-preview-current t)) ; No preview vs Non-inserting preview

(after! orderless
  ;; (setopt orderless-style-dispatchers '(orderless-affix-dispatch))
  ;; (setopt orderless-component-separator #'orderless-escapable-split-on-space)
  (setopt completion-styles '(basic orderless)
          completion-category-defaults nil ;; Disable defaults, use our settings
          completion-pcm-leading-wildcard t) ;; Emacs 31: partial-completion behaves like substring
  ;; Amend rather than replace: Doom's corfu module adds an `lsp-capf' entry
  ;; here that a wholesale setq would drop.
  (setf (alist-get 'file completion-category-overrides)
        '((styles partial-completion))))

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

(use-package cape

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
    (setq-local cape-file-directory (vc-root-dir))
    (setq-local completion-at-point-functions (list #'cape-file #'cape-dabbrev)))

  ;;------------------------------------------------------------------------------
  ;; Python
  ;;------------------------------------------------------------------------------

  ;; after! python-ts-mode
  (defun hook/set-python-base-capf ()
    (setq-local cape-file-prefix '("\"" "'"))
    (setq-local completion-at-point-functions
                (list #'python-completion-at-point
                      #'yasnippet-capf
                      #'cape-dabbrev)))

  (add-hook! '(python-mode-hook python-ts-mode-hook) #'hook/set-python-base-capf)

  ;;------------------------------------------------------------------------------
  ;; Verilog
  ;;------------------------------------------------------------------------------

  (defun hook/add-verilog-keywords ()
    (require 'cape-keyword)
    (require 'verilog-mode)
    (dolist (mode '(verilog-mode verilog-ts-mode))
      (add-to-list 'cape-keyword-list
                   (append (list mode) verilog-keywords))))

  (defun hook/set-verilog-capf ()
    (setq-local completion-at-point-functions
                (list #'yasnippet-capf
                      #'cape-keyword
                      #'cape-dabbrev)))

  (add-hook! '(verilog-mode-hook verilog-ts-mode-hook)
             'hook/add-verilog-keywords
             'hook/set-verilog-capf)

  ;;------------------------------------------------------------------------------
  ;; VHDL
  ;;------------------------------------------------------------------------------

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
                           vhdl-directives))))

  (defun hook/set-vhdl-capf ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'cape-dabbrev
                       #'cape-keyword
                       #'yasnippet-capf))))

  (add-hook! 'vhdl-mode-hook 'hook/add-vhdl-keywords 'hook/set-vhdl-capf)

  ;;------------------------------------------------------------------------------
  ;; Make
  ;;------------------------------------------------------------------------------

  (add-hook! 'makefile-mode-hook
    (defun hook/makefile-pickup-everything ()
      (makefile-pickup-everything nil)))

  ;;------------------------------------------------------------------------------
  ;; RST
  ;;------------------------------------------------------------------------------

  (defun my/project-root-of-buffer ()
    "Nearest ancestor of the current buffer's file containing .git, if any."
    (when-let* ((file (buffer-file-name)))
      (locate-dominating-file (directory-file-name file) ".git")))

  (defun hook/setup-rst-with-corfu ()

    (require 'corfu)
    (require 'cape)

    (setq-local cape-file-directory (my/project-root-of-buffer))
    (setq-local completion-at-point-functions
                (list #'cape-file
                      #'cape-dabbrev
                      #'yasnippet-capf
                      #'cape-dict)))

  (add-hook 'rst-mode-hook 'hook/setup-rst-with-corfu)

  ;;------------------------------------------------------------------------------
  ;; Tex
  ;;------------------------------------------------------------------------------

  (defun hook/setup-tex-with-corfu ()

    (require 'corfu)
    (require 'cape)

    (setq-local cape-file-prefix
                '("{"
                  "\\input{"
                  "\\includegraphics{"))

    (setq-local cape-file-directory (my/project-root-of-buffer))
    (setq-local completion-at-point-functions
                (list
                 ;; 'TeX--completion-at-point
                 ;; 'LaTeX--arguments-completion-at-point
                 #'cape-file
                 #'cape-dabbrev
                 #'yasnippet-capf
                 #'cape-tex
                 #'cape-dict
                 #'citar-capf)))

  (add-hook 'LaTeX-mode-hook #'hook/setup-tex-with-corfu)

  (add-hook 'LaTeX-mode-local-vars-hook
            (defun hook/tex-restore-cape-file-prefix ()
              (setq-local cape-file-prefix
                          '("{" "\\input{" "\\includegraphics{")))
            90)

  ;; HACK: eglot screws with completion-at-point-functions... usually might not
  ;; care but with latex the eglot completion at point errors so NONE of the
  ;; capfs work when eglot is active. So just remove the eglot capf then re-configure corfu.
  ;;
  ;; This seemingly can't be in the LaTeX mode hook since eglot mode hook is run
  ;; *after* latex mode, so anything in the latex mode hook just get overwritten
  ;; by what happens in eglot setup
  ;;
  (add-hook 'eglot-managed-mode-hook
            (defun hook/remove-tex-eglot-completion ()
              (when (and (eglot-managed-p)
                         (eq major-mode 'LaTeX-mode))
                (remove-hook 'completion-at-point-functions #'eglot-completion-at-point t)
                (hook/setup-tex-with-corfu))))

  ;;------------------------------------------------------------------------------
  ;; Elisp
  ;;------------------------------------------------------------------------------

  (defun hook/set-elisp-capf-functions ()
    (setq-local completion-at-point-functions
                (list
                 #'yasnippet-capf
                 #'cape-elisp-symbol
                 #'cape-keyword
                 #'cape-dabbrev
                 #'cape-history
                 #'cape-file)))

  (add-hook 'emacs-lisp-mode-hook 'hook/set-elisp-capf-functions)

  ;;------------------------------------------------------------------------------
  ;; TCL
  ;;------------------------------------------------------------------------------

  (defun hook/set-tcl-capfs ()
    (setq-local completion-at-point-functions
                (list
                 (cape-capf-super #'cape-dabbrev
                                  #'cape-keyword
                                  #'yasnippet-capf))))

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
                         tcl-builtin-list)))

  (add-hook! 'tcl-mode-hook 'hook/set-tcl-capfs 'hook/set-tcl-cape-keywords))
