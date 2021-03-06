;; -*- no-byte-compile: t; -*-

;; start:sort -- keep the stuff below sorted alphabetically
(package! all-the-icons)
(package! all-the-icons-dired)
(package! all-the-icons-ibuffer)
(package! all-the-icons-ivy)
(package! backup-each-save)
(package! crontab-mode)
(package! doom-todo-ivy :recipe (:host github :repo "jsmestad/doom-todo-ivy"))
(package! emojify)
(package! evil-leader)
(package! evil-matchit)
(package! flycheck-clang-tidy)
(package! flymake-json)
(package! fzf :recipe (:host github :repo "bling/fzf.el"))
(package! graphviz-dot-mode)
(package! irony)
(package! logview)
(package! lorem-ipsum)
(package! lsp-mode :recipe (:host github :repo "emacs-lsp/lsp-mode"))
(package! lsp-pyright)
(package! magit-todos)
(package! mixed-pitch)
(package! org-download :recipe (:host github :repo "abo-abo/org-download"))
(package! org-web-tools)
(package! ox-clip :recipe (:host github :repo "jkitchin/ox-clip"))
(package! ox-gfm)
(package! pdf-tools)
(package! scad-mode)
(package! screenshot :recipe (:host github :repo "tecosaur/screenshot"))
(package! toc-org)
(package! tree-sitter)
(package! tree-sitter-langs)
(package! vimrc-mode)
(package! xr)                           ; xr is rx in reverse
(package! yaml-mode)
;; (package! ample-regexps)
;; (package! base16-theme)
;; (package! bespoke-modeline :recipe (:host github :repo "mclear-tools/bespoke-modeline"))
;; (package! bespoke-themes :recipe (:host github :repo "mclear-tools/bespoke-themes"))
;; (package! browse-at-remote)
;; (package! company-jedi)
;; (package! counsel-etags)
;; (package! dumb-jump)
;; (package! eaf :recipe (:host github :repo "manateelazycat/emacs-application-framework"))
;; (package! emacs-tree-sitter :recipe  (:host github :repo   "emacs-tree-sitter/elisp-tree-sitter"))
;; (package! etags-table)
;; (package! evil-anzu)
;; (package! flycheck-projectile :pin "ce6e9e8" :recipe (:host github :repo "nbfalcon/flycheck-projectile"))
;; (package! hl-line :disable t)
;; (package! htmlize :recipe (:host github :repo "hniksic/emacs-htmlize"))
;; (package! infix.el :recipe (:host github :repo "rspeele/infix.el"))
;; (package! mmm-mode)
;; (package! nano-modeline)
;; (package! nano-theme)
;; (package! olivetti)
;; (package! org-attach-screenshot)
;; (package! org-roam :recipe (:host github :repo "org-roam/org-roam"))
;; (package! org-roam-server)
;; (package! pcre2el)
;; (package! popup)
;; (package! prism)
;; (package! svg-tag-mode :recipe (:host github :repo "rougier/svg-tag-mode"))
;; (package! undo-hl :recipe (:host github :repo "casouri/undo-hl"))
;; (package! vlf)                          ; view large files
;; end:sort

;; start:sort -- keep the stuff below sorted alphabetically
;;(package! auctex)
;;(package! clang-format)
;;(package! clang-format+)
;;(package! company)
;;(package! company-box)
;;(package! company-tabnine)
;;(package! elfeed)
;;(package! elpy)
;;(package! esup)
;;(package! fira-code-mode)
;;(package! fzf :recipe (:host github :repo "seenaburns/fzf.el"))
;;(package! good-scroll :recipe (:host github :repo "io12/good-scroll.el"))
;;(package! leuven-theme)
;;(package! org-bullets)
;;(package! org-gcal :recipe (:host github :repo "kidd/org-gcal.el"))
;;(package! org-sync)
;;(package! org-table-comment)
;;(package! rainbow-mode) ;; included in rgb package
;;(package! theme-changer :recipe (:host github :repo "hadronzoo/theme-changer"))
;;(package! visual-fill-column)
;;(package! vterm)
;; end:sort
;;
;;
;;

;; https://www.emacswiki.org/emacs/UntabifyUponSave
;; Local Variables:
;; eval: (make-variable-buffer-local 'write-contents-functions)
;; eval: (add-hook 'write-contents-functions 'sort-elisp-block nil t)
;; End:
