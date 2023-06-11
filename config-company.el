;; -*- lexical-binding: t; -*-
;;

(defun cape-add-yasnippet ()
  (add-to-list 'completion-at-point-functions
               (cape-company-to-capf #'company-yasnippet)))

(add-hook! vhdl-mode-hook
  (setq-local completion-at-point-functions
        (list (cape-keyword
               cape-dabbrev
               (cape-company-to-capf #'company-yasnippet)
               corfu--ispell-in-comments-and-strings))))

(setq +corfu-auto-delay 1.0
      corfu-auto-delay 1.0)

(add-hook! python-mode-hook
  (setq-local completion-at-point-functions
              (list (cape-keyword
                     cape-file
                     #'lsp-completion-at-point
                     ;; #'eglot-completion-at-point
                     cape-capf-buster
                     cape-dabbrev
                     (cape-company-to-capf #'company-yasnippet)
                     corfu--ispell-in-comments-and-strings))))

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

  (add-hook! tcl-mode
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

    (add-hook! cider-repl-mode-hook #'company-mode)
    (add-hook! cider-mode-hook #'company-mode)
    (add-hook! cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
    (add-hook! cider-mode-hook #'cider-company-enable-fuzzy-completion)

    (defun my-clojure-mode-hook ()
      (setq-local company-backends
                  '((:separate company-capf company-keywords company-dabbrev-code company-yasnippet company-files))))
    (add-hook 'clojure-mode-hook #'my-clojure-mode-hook))

  (set-company-backend! 'clojure-mode
    '(:separate company-capf company-keywords company-dabbrev-code company-yasnippet)))
