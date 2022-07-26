;; -*- lexical-binding: t; -*-

;;; Company Completion
;;------------------------------------------------------------------------------

(after! company

  (add-hook 'after-init-hook 'global-company-mode) ; turn on company

  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2)

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

  (set-company-backend! '(org-mode) nil)
  (set-company-backend!
    '(markdown-mode org-mode)
    '(company-files))

  (set-company-backend! '(vhdl-mode) nil)
  (set-company-backend!
    '(vhdl-mode)
    '(company-capf company-keywords company-dabbrev-code company-yasnippet))

  (set-company-backend! 'hog-src-mode nil)
  (set-company-backend!
    '(hog-src-mode)
    '(company-files )))

;; (use-package corfu-doc
;;   :config
;;   (setq corfu-doc-delay 0.2
;;         corfu-doc-max-width 80
;;         corfu-doc-max-height 40))

;; (use-package corfu
;;   :config
;;   (setq corfu-cycle t
;;         corfu-auto t
;;         corfu-auto-prefix 1
;;         corfu-auto-delay 0.01
;;         corfu-separator ?\s
;;         corfu-quit-at-boundary nil
;;         corfu-quit-no-match t
;;         corfu-preview-current nil
;;         corfu-preselect-first t
;;         corfu-on-exact-match nil
;;         corfu-echo-documentation t
;;         corfu-scroll-margin 10)
;;   (map! :map global-map
;;         :nvi "C-SPC" #'completion-at-point)
;;   (map! :map corfu-map
;;         "C-j" #'corfu-next
;;         "C-k" #'corfu-previous
;;         "C-l" #'corfu-insert
;;         "C-;" #'corfu-doc-toggle
;;         "TAB" #'corfu-insert
;;         "<tab>" #'corfu-insert
;;         "ESC" #'corfu-reset)
;;   (add-hook! '(prog-mode-hook
;;                 text-mode-hook)
;;     (corfu-mode +1)
;;     (corfu-doc-mode +1)
;;     (unless (display-graphic-p)
;;       (corfu-terminal-mode +1)
;;       (corfu-doc-terminal-mode +1))
;;     (company-mode -1))
;;   (add-hook! '(clojure-mode-hook
;;                 clojurescript-mode-hook
;;                 clojurec-mode-hook
;;                 css-mode-hook
;;                 scss-mode-hook
;;                 sass-mode-hook
;;                 less-css-mode-hook)
;;     (corfu-mode -1)
;;     (company-mode +1)))

;; (use-package kind-icon
;;   :ensure t
;;   :after corfu
;;   :custom
;;   (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; ;; Optionally use the `orderless' completion style. See `+orderless-dispatch'
;; ;; in the Consult wiki for an advanced Orderless style dispatcher.
;; ;; Enable `partial-completion' for files to allow path expansion.
;; ;; You may prefer to use `initials' instead of `partial-completion'.
;; (use-package orderless
;;   :init
;;   ;; Tune the global completion style settings to your liking!
;;   ;; This affects the minibuffer and non-lsp completion at point.
;;   (setq completion-styles '(orderless)
;;         completion-category-defaults nil
;;         completion-category-overrides '((file (styles . (partial-completion))))))

;; (use-package lsp-mode
;;   :init
;;   (defun my/lsp-mode-setup-completion ()
;;     (setq-local completion-styles '(flex)))
;;   :config
;;   ;; We use Corfu!
;;   (setq lsp-completion-provider :none)
;;   (add-hook 'lsp-completion-mode-hook #'my/lsp-mode-setup-completion)
;;   (add-hook 'lsp-mode-hook (lambda () (lsp-completion-mode +1))))

;; ;; Add extensions
;; (use-package cape
;;   :init
;;   ;; Add `completion-at-point-functions', used by `completion-at-point'.
;;   (add-to-list 'completion-at-point-functions #'cape-file)
;;   (add-to-list 'completion-at-point-functions #'cape-tex)
;;   (add-to-list 'completion-at-point-functions #'cape-dabbrev)
;;   (add-to-list 'completion-at-point-functions #'cape-keyword)
;;   (add-to-list 'completion-at-point-functions #'cape-symbol)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-dict)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-line)
;;   )
