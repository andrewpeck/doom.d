;; -*- lexical-binding: t; -*-

;;; Company Completion
;;------------------------------------------------------------------------------

(after! company

  ;; turn on company
  (add-hook 'after-init-hook 'global-company-mode)

  (define-key company-active-map (kbd "<tab>") nil)
  (define-key company-active-map (kbd "<return>") #'company-complete-selection)

  ;; company settings
  (setq company-minimum-prefix-length 4)
  (setq company-auto-commit nil)
  (setq company-idle-delay 0.4)
  (setq company-require-match 'never)
  (setq company-frontends '(company-box-frontend company-echo-metadata-frontend))
  (setq-default company-box-show-single-candidate 'always)
  (add-hook 'company-mode-hook 'company-box-mode)
  (setq-default company-box-backends-colors 'nil)

  (add-hook 'python-mode-hook (lambda () (add-to-list 'company-backends 'company-jedi)))

  (add-hook 'Latex-mode-hook
            (lambda () (set-company-backend! 'LaTeX-mode-hook
                         '(company-yasnippet company-reftex company-auctex
                                             company-math company-files
                                             company-keywords company-capf company-dabbrev-code
                                             company-etags company-dabbrev))))

  ;; (set-company-backend! 'org-mode '(company-yasnippet
  ;;                                   company-capf
  ;;                                   company-files
  ;;                                   company-keywords
  ;;                                   ))

  (set-company-backend!
    '(text-mode
      markdown-mode
      org-mode
      gfm-mode)
    '(:seperate
      company-files
      company-yasnippet
      company-ispell
      ))

  (set-company-backend! '(prog-mode tcl-mode python-mode vhdl-mode)
    '(company-yasnippet company-keywords company-capf company-files
                        company-dabbrev-code company-etags company-dabbrev ))

  )
