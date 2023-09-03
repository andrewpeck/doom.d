;; -*- lexical-binding: t; no-byte-compile: t;-*-

;; Disabled packages
(package! hl-line :disable t)
(package! powerline :disable t)
(package! popwin :disable t)
(package! popwin :disable t)
(package! ivy :disable t)
(package! swiper :disable t)

(package! company)

;; Org mode
(package! evil-org)
(package! org-download :recipe (:host github :repo "abo-abo/org-download"))
(package! org-web-tools)
(package! ox-clip :recipe (:host github :repo "jkitchin/ox-clip"))
(package! ox-gfm)
(package! ox-pandoc)
(package! toc-org)
;; (package! org-protocol-capture-html :recipe (:host github :repo "alphapapa/org-protocol-capture-html"))

;; Clojure
(package! clojure-emacs :recipe (:host github :repo "clojure-emacs/squiggly-clojure"))
(package! flycheck-clj-kondo :recipe (:host github :repo "borkdude/flycheck-clj-kondo"))

;; Themes
;; (package! solarized-emacs :recipe (:host github :repo "bbatsov/solarized-emacs"))
;; (package! zenburn-emacs :recipe (:host github :repo "bbatsov/zenburn-emacs"))

;; Misc packages
;; start:sort
(package! affe)
(package! flycheck :pin "e56e30d" :recipe (:host github :repo "flycheck/flycheck"))
(package! all-the-icons)
(package! all-the-icons-dired)
(package! all-the-icons-ibuffer)
(package! benchmark-init)
(package! verilog-mode :recipe (:host github :repo "veripool/verilog-mode"))
(package! crontab-mode)
(package! delight)
(package! dwim-shell-command :recipe (:host github :repo "xenodium/dwim-shell-command"))
(package! emojify)
(package! evil-collection :pin "1f34f5e" :recipe (:host github :repo "emacs-evil/evil-collection"))
(package! evil-leader)
(package! fzf :recipe (:host github :repo "bling/fzf.el"))
(package! graphviz-dot-mode)
(package! logview)
(package! lsp-grammarly)
(package! magit-todos)
(package! org-modern :recipe (:host github :repo "minad/org-modern"))
(package! plz)
(package! rainbow-mode)
(package! scad-mode)
(package! screenshot :recipe (:host github :repo "tecosaur/screenshot"))
(package! standard-themes :recipe (:host github :repo "protesilaos/standard-themes"))
(package! systemd :pin "b6ae63a236605b1c5e1069f7d3afe06ae32a7bae" :recipe (:host github :repo "holomorph/systemd-mode"))
(package! vimrc-mode)
(package! visual-fill-column)
(package! xr)                           ; xr is rx in reverse
;; end:sort

;; Local Variables:
;; eval: (make-variable-buffer-local 'kill-buffer-hook)
;; eval: (add-hook 'kill-buffer-hook (lambda () (sort-elisp-block) (save-buffer)) nil t)
;; End:
