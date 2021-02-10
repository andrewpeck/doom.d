;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here, run 'doom sync' on
;; the command line, then restart Emacs for the changes to take effect.
;; Alternatively, use M-x doom/reload.
;;
;; WARNING: Disabling core packages listed in ~/.emacs.d/core/packages.el may
;; have nasty side-effects and is not recommended.


;; All of Doom's packages are pinned to a specific commit, and updated from
;; release to release. To un-pin all packages and live on the edge, do:
;(unpin! t)

;; ...but to unpin a single package:
;(unpin! pinned-package)
;; Use it to unpin multiple packages
;(unpin! pinned-package another-pinned-package)


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a particular repo, you'll need to specify
;; a `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, for whatever reason,
;; you can do so here with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

(package! all-the-icons)
(package! all-the-icons-dired)
(package! all-the-icons-ibuffer)
(package! all-the-icons-ivy)
(package! auctex)
(package! base16-theme)
(package! browse-at-remote)
(package! clang-format)
(package! clang-format+)
(package! company)
(package! company-box)
(package! company-jedi)
(package! company-tabnine)
(package! counsel-etags)
(package! doom-todo-ivy :recipe (:host github :repo "jsmestad/doom-todo-ivy"))
(package! dumb-jump)
(package! elfeed)
(package! emojify)
(package! etags-table)
(package! evil-leader)
(package! evil-magit)
(package! evil-numbers)
(package! flycheck-clang-tidy)
(package! flymake-json)
(package! forge)
(package! graphviz-dot-mode)
(package! hl-line :disable t)
(package! htmlize :recipe (:host github :repo "hniksic/emacs-htmlize"))
(package! irony)
(package! json-mode)
(package! logview)
(package! lsp-mode :recipe (:host github :repo "emacs-lsp/lsp-mode"))
(package! magit-todos)
(package! mixed-pitch)
(package! org-attach-screenshot)
(package! org-bullets)
(package! org-download :recipe (:host github :repo "abo-abo/org-download"))
(package! org-gcal :recipe (:host github :repo "kidd/org-gcal.el"))
(package! org-sync)
(package! org-table-comment)
(package! ox-clip :recipe (:host github :repo "jkitchin/ox-clip"))
(package! ox-gfm)
(package! pcre2el)
(package! pdf-tools)
(package! rainbow-mode)
(package! slime)
(package! solarized-theme)
(package! screenshot :recipe (:host github :repo "tecosaur/screenshot"))
(package! toc-org)
(package! vimrc-mode)
(package! visual-fill-column)
(package! vivado-mode :recipe (:host github :repo "ashtonchase/vivado_mode"))
(package! yaml-mode)
(package! zaiste-theme :recipe (:host github :repo "zaiste/zaiste-emacs-theme"))
;;(package! vterm)
;(package! fzf :recipe (:host github :repo "seenaburns/fzf.el"))
;(package! leuven-theme)
;;(package! fira-code-mode)
;;(package! theme-changer :recipe (:host github :repo "hadronzoo/theme-changer"))
