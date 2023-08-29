;; -*- lexical-binding: t; -*-

;;------------------------------------------------------------------------------
;; Cape
;;------------------------------------------------------------------------------
;;
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

(defun cape-add-yasnippet ()
  (add-to-list 'completion-at-point-functions
               (cape-company-to-capf #'company-yasnippet)))

(setq corfu-auto-delay 0.5)

(add-hook! 'emacs-lisp-mode-hook
  (setq-local completion-at-point-functions
              (list
               (cape-company-to-capf #'company-yasnippet)
               'cape-dabbrev
               'cape-keyword
               'cape-elisp-block
               'cape-files
               'lsp-completion-at-point)))

(add-hook! 'vhdl-mode-hook
  (setq-local completion-at-point-functions
              (list (cape-super-capf
                     'cape-dabbrev
                     'cape-keyword
                     (cape-company-to-capf #'company-yasnippet)))))

(add-hook! 'python-mode-hook
  (setq-local completion-at-point-functions
              (list
               (cape-super-capf
                'cape-keyword
                'cape-file
                ;; #'lsp-completion-at-point
                ;; #'eglot-completion-at-point
                'cape-capf-buster
                'cape-dabbrev
                (cape-company-to-capf #'company-yasnippet)))))

(add-hook 'tcl-mode-hook
          (lambda ()
            (setq-local completion-at-point-functions
                        (list (cape-super-capf
                               'cape-dabbrev
                               'cape-keyword
                               'cape-file
                               (cape-company-to-capf #'company-yasnippet))))))

(after! tcl
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
                         tcl-builtin-list))))

(after! vhdl-mode
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

;;------------------------------------------------------------------------------
;; Company
;;------------------------------------------------------------------------------


(after! company

  (setq company-idle-delay 1.0
        company-minimum-prefix-length 2
        company-icon-size '(auto-scale . 24)
        company-backends
        '(company-bbdb company-semantic company-cmake company-capf company-clang company-files
          (company-dabbrev-code company-gtags company-etags company-keywords)
          company-oddmuse company-dabbrev))

  ;; (set-company-backend! 'text-mode nil)

  (after! org
    (set-company-backend! 'org-mode nil)
    (set-company-backend! 'org-mode '(company-capf company-files company-yasnippet company-dabbrev)))

  (add-hook! 'tcl-mode-hook
    (setq company-backends
          '((:separate company-dabbrev-code company-capf company-keywords
             :with company-yasnippet company-files))))

  (after! vhdl-mode
    (set-company-backend! 'vhdl-mode nil)
    (set-company-backend! 'vhdl-mode
                          '(company-capf company-keywords company-dabbrev-code company-yasnippet)))

  (after! hog-src-mode
    (set-company-backend! 'hog-src-mode nil)
    (set-company-backend! 'hog-src-mode 'company-files))

  (after! clojure-mode

    (add-hook! 'cider-repl-mode-hook #'company-mode)
    (add-hook! 'cider-mode-hook #'company-mode)
    (add-hook! 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
    (add-hook! 'cider-mode-hook #'cider-company-enable-fuzzy-completion)

    (defun my-clojure-mode-hook ()
      (setq-local company-backends
                  '((:separate company-capf company-keywords company-dabbrev-code company-yasnippet company-files))))
    (add-hook 'clojure-mode-hook #'my-clojure-mode-hook))

  (set-company-backend! 'clojure-mode
                        '(:separate company-capf company-keywords company-dabbrev-code company-yasnippet)))
