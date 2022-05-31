;; -*- lexical-binding: t; -*-

;;; Company Completion
;;------------------------------------------------------------------------------

(after! company

  (setq company-idle-delay 0.5)
  (add-hook 'after-init-hook 'global-company-mode) ; turn on company

  ;; (define-key company-active-map (kbd "<tab>") nil)
  ;; (define-key company-active-map (kbd "<return>") #'company-complete-selection)

  ;; ;; company settings
  ;; (setq company-minimum-prefix-length 4)
  ;; (setq company-auto-commit nil)
  ;; (setq company-idle-delay 0.4)
  ;; (setq company-require-match 'never)
  ;; (setq company-frontends '(company-box-frontend company-echo-metadata-frontend))
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

  ;; (set-company-backend! 'org-mode nil)
  (set-company-backend!
    '(markdown-mode org-mode)
    '(company-files company-ispell))

  ;; (set-company-backend! 'vhdl-mode nil)
  ;; (set-company-backend!
  ;;   '(vhdl-mode)
  ;;   '(company-capf company-keywords company-dabbrev company-yasnippet))

  (set-company-backend! 'hog-src-mode nil)
  (set-company-backend!
    '(hog-src-mode)
    '(company-files company-dabbrev))
  )
