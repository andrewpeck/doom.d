;; -*- lexical-binding: t; no-byte-compile: t;-*-

;; packages to disable
(package! hl-line :disable t)

;; org mode
(package! toc-org)
(package! evil-org)
(package! org-download :recipe (:host github :repo "abo-abo/org-download"))
(package! org-web-tools)
(package! ox-gfm)
(package! ox-pandoc)
(package! ox-clip :recipe (:host github :repo "jkitchin/ox-clip"))
;; (package! org-protocol-capture-html :recipe (:host github :repo "alphapapa/org-protocol-capture-html"))

;; -- keep the stuff below sorted alphabetically
;; start:sort
(package! affe)
(package! all-the-icons)
(package! all-the-icons-dired)
(package! all-the-icons-ibuffer)
(package! backup-each-save)
(package! clojure-emacs :recipe (:host github :repo "clojure-emacs/squiggly-clojure"))
(package! crontab-mode)
(package! delight)
(package! dwim-shell-command :recipe (:host github :repo "xenodium/dwim-shell-command"))
(package! emojify)
(package! evil-leader)
(package! evil-matchit)
(package! flycheck-clj-kondo :recipe (:host github :repo "borkdude/flycheck-clj-kondo"))
(package! fzf :recipe (:host github :repo "bling/fzf.el"))
(package! graphviz-dot-mode)
(package! logview)
(package! lorem-ipsum)
(package! magit-todos)
(package! org-web-tools)
(package! pdf-tools)
(package! plz)
(package! rainbow-mode)
(package! scad-mode)
(package! screenshot :recipe (:host github :repo "tecosaur/screenshot"))
(package! solarized-emacs :recipe (:host github :repo "bbatsov/solarized-emacs"))
(package! straight :pin "3eca39d") ; https://github.com/doomemacs/doomemacs/issues/6960
(package! systemd :pin "b6ae63a236605b1c5e1069f7d3afe06ae32a7bae" :recipe (:host github :repo "holomorph/systemd-mode"))
(package! vimrc-mode)
(package! visual-fill-column)
(package! web-mode)
(package! xr)                           ; xr is rx in reverse
(package! yaml-mode)
(package! zenburn-emacs :recipe (:host github :repo "bbatsov/zenburn-emacs"))
;; end:sort

;; start:sort -- keep the stuff below sorted alphabetically
;; (package! minibuffer-line)
;;(package! all-the-icons-ivy)
;;(package! ample-regexps)
;;(package! auctex)
;;(package! base16-theme)
;;(package! bespoke-modeline :recipe (:host github :repo "mclear-tools/bespoke-modeline"))
;;(package! bespoke-themes :recipe (:host github :repo "mclear-tools/bespoke-themes"))
;;(package! browse-at-remote)
;;(package! clang-format)
;;(package! clang-format+)
;;(package! company)
;;(package! company-box)
;;(package! company-jedi)
;;(package! company-tabnine)
;;(package! counsel-etags)
;;(package! doom-todo-ivy :recipe (:host github :repo "jsmestad/doom-todo-ivy"))
;;(package! dumb-jump)
;;(package! eaf :recipe (:host github :repo "manateelazycat/emacs-application-framework"))
;;(package! elfeed)
;;(package! elpy)
;;(package! emacs-tree-sitter :recipe  (:host github :repo   "emacs-tree-sitter/elisp-tree-sitter"))
;;(package! esup)
;;(package! etags-table)
;;(package! evil-anzu)
;;(package! ewp :recipe (:host github :repo "larsmagne/ewp"))
;;(package! fira-code-mode)
;;(package! flycheck-clang-tidy)
;;(package! flycheck-projectile :pin "ce6e9e8" :recipe (:host github :repo "nbfalcon/flycheck-projectile"))
;;(package! flymake-json)
;;(package! fzf :recipe (:host github :repo "seenaburns/fzf.el"))
;;(package! good-scroll :recipe (:host github :repo "io12/good-scroll.el"))
;;(package! hl-line :disable t)
;;(package! htmlize :recipe (:host github :repo "hniksic/emacs-htmlize"))
;;(package! infix.el :recipe (:host github :repo "rspeele/infix.el"))
;;(package! irony)
;;(package! leuven-theme)
;;(package! lsp-mode :recipe (:host github :repo "emacs-lsp/lsp-mode"))
;;(package! lsp-pyright)
;;(package! mermaid-mode :recipe (:host github :repo "abrochard/mermaid-mode"))
;;(package! mixed-pitch)
;;(package! mmm-mode)
;;(package! nano-modeline)
;;(package! nano-theme)
;;(package! ob-mermaid :recipe (:host github :repo "arnm/ob-mermaid"))
;;(package! olivetti)
;;(package! org-attach-screenshot)
;;(package! org-bullets)
;;(package! org-gcal :recipe (:host github :repo "kidd/org-gcal.el"))
;;(package! org-roam :recipe (:host github :repo "org-roam/org-roam"))
;;(package! org-roam-server)
;;(package! org-sync)
;;(package! org-table-comment)
;;(package! pcre2el)
;;(package! popup)
;;(package! prism)
;;(package! svg-tag-mode :recipe (:host github :repo "rougier/svg-tag-mode"))
;;(package! theme-changer :recipe (:host github :repo "hadronzoo/theme-changer"))
;;(package! tree-sitter)
;;(package! tree-sitter-langs)
;;(package! undo-hl :recipe (:host github :repo "casouri/undo-hl"))
;;(package! vlf)                          ; view large files
;;(package! vterm)
;; end:sort

;; Local Variables:
;; eval: (make-variable-buffer-local 'write-contents-functions)
;; eval: (add-hook 'write-contents-functions 'sort-elisp-block nil t)
;; End:
