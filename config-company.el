;; -*- lexical-binding: t; -*-

(after! company

  (add-hook 'after-init-hook 'global-company-mode) ; turn on company

  (setq company-idle-delay 0.25
        company-minimum-prefix-length 2
        company-icon-size '(auto-scale . 24))

  ;; (setq company-frontends '(company-box-frontend company-echo-metadata-frontend))

  ;; (define-key company-active-map (kbd "<tab>") nil)
  ;; (define-key company-active-map (kbd "<return>") #'company-complete-selection)

  ;; ;; company settings
  ;; (setq company-auto-commit nil)
  ;; (setq company-require-match 'never)
  ;; (setq-default company-box-show-single-candidate 'always)
  ;; (add-hook 'company-mode-hook 'company-box-mode)
  ;; (setq-default company-box-backends-colors 'nil)

  ;; (add-hook 'Latex-mode-hook
  ;;           (lambda ()
  ;;             (set-company-backend! 'LaTeX-mode-hook
  ;;               '(company-yasnippet company-reftex company-auctex
  ;;                                   company-math company-files
  ;;                                   company-keywords company-capf company-dabbrev-code
  ;;                                   company-etags company-dabbrev))))

  ;; company-files
  ;; company-yasnippet
  ;; company-capf

  ;; (set-company-backend!
  ;;   '(markdown-mode org-mode)
  ;;   nil)

  (set-company-backend! 'text-mode nil)

  (after! org
    (set-company-backend! 'org-mode nil)
    (set-company-backend! 'org-mode '(company-capf company-files company-yasnippet company-dabbrev)))

  (after! vhdl-mode
    (set-company-backend! 'vhdl-mode nil)
    (set-company-backend! 'vhdl-mode
      '(company-capf company-keywords company-dabbrev-code company-yasnippet)))

  (after! hog-src-mode
    (set-company-backend! 'hog-src-mode nil)
    (set-company-backend! 'hog-src-mode 'company-files))
  )
