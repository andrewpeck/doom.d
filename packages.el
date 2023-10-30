;; -*- lexical-binding: t; no-byte-compile: t;-*-

;; Disabled packages
(package! hl-line :disable t)
(package! diredfl :disable t)
(package! lsp :disable t)
(package! powerline :disable t)
(package! popwin :disable t)
(package! popwin :disable t)
(package! ivy :disable t)
(package! swiper :disable t)

(package! company :pin "66201465a962ac003f320a1df612641b2b276ab5")

;; Org mode
(package! evil-org :pin "a9706da260c45b98601bcd72b1d2c0a24a017700")
(package! org-download :recipe (:host github :repo "abo-abo/org-download") :pin "19e166f0a8c539b4144cfbc614309d47a9b2a9b7")
(package! ox-clip :recipe (:host github :repo "jkitchin/ox-clip") :pin "ff117cf3c619eef12eccc0ccbfa3f11adb73ea68")
(package! ox-gfm :pin "46faa67dbb3fb0cd7a76c3fe518f16e4195c22c7")
(package! ox-pandoc :pin "0f758517f512e375825679541b5d905be40342eb")
(package! toc-org :pin "6d3ae0fc47ce79b1ea06cabe21a3c596395409cd")
;; (package! org-protocol-capture-html :recipe (:host github :repo "alphapapa/org-protocol-capture-html"))

;; Clojure
(package! clojure-emacs :recipe (:host github :repo "clojure-emacs/squiggly-clojure") :pin "592c4f89efb5112784cbf94c9ea6fdd045771b62")
(package! flycheck-clj-kondo :recipe (:host github :repo "borkdude/flycheck-clj-kondo") :pin "ff7bed2315755cfe02ef471edf522e27b78cd5ca")

;; Themes
;; (package! solarized-emacs :recipe (:host github :repo "bbatsov/solarized-emacs"))
;; (package! zenburn-emacs :recipe (:host github :repo "bbatsov/zenburn-emacs"))

;; Misc packages
;; start:sort
(package! affe :pin "ae3169ac4bbd64520d165b4ce4806b7a34b972dc")
(package! ascii-art-to-unicode :pin "79a73bd4f623f10135b6b65030f6d5e3070466db")
(package! benchmark-init :pin "02435560415bbadbcf5051fb7042880549170e7e")
(package! browse-at-remote :recipe (:host github :repo "rmuslimov/browse-at-remote") :pin "76aa27dfd469fcae75ed7031bb73830831aaccbf")
(package! cape :recipe (:host github :repo "minad/cape") :pin "abacb231157e0c90e29bdda6d15b4b448e48ffbd")
(package! crontab-mode :pin "7412f3df0958812bfcacd5875a409fa795fa8ecc")
(package! delight :pin "46932bdc4da69072329aec8df052da3e04444899")
(package! dwim-shell-command :recipe (:host github :repo "xenodium/dwim-shell-command") :pin "853318d58a9060c17538eaeacc00e4e6eb4fd642")
(package! elfeed :recipe (:host github :repo "skeeto/elfeed") :pin "55fb162fa27e71b88effa59a83c57842e262b00f" )
(package! emojify :pin "1b726412f19896abf5e4857d4c32220e33400b55")
(package! evil-collection :recipe (:host github :repo "emacs-evil/evil-collection") :pin "18304d9d9e4243687a7bf915dc7e591a216873c6")
(package! evil-leader :pin "39f7014bcf8b36463e0c7512c638bda4bac6c2cf")
(package! flycheck :recipe (:host github :repo "flycheck/flycheck") :pin "e56e30d8c66ffc9776d07740658d3b542c1a8e21")
(package! fzf :recipe (:host github :repo "bling/fzf.el") :pin "3a55b983921c620fb5a2cc811f42aa4daaad8266")
(package! graphviz-dot-mode :pin "8ff793b13707cb511875f56e167ff7f980a31136")
(package! logview :pin "9140067afdc2f0d1eb493dc4dfdb53645289dd2b")
(package! magit-todos :pin "d0646dbbf46d75d08e3d7b4c665d7d763a468af1")
(package! markdown-preview-mode :recipe (:host github :repo "ancane/markdown-preview-mode") :pin "68242b3907dc065aa35412bfd928b43d8052d321")
(package! org-modern :recipe (:host github :repo "minad/org-modern") :pin "240026f0bef6dabff1e86ab092a678beddd301c3")
(package! org-web-tools :pin "821e6f032f5823e5e3344ea4d1a36870e236aba1")
(package! plz :pin "545ed3c5179dc2432ba25ea06a402a083c0f871b")
(package! rainbow-mode :pin "41f4af40964cdb9d0cc3552eb450540fdc820455")
(package! scad-mode :pin "e1af74735ad6113448c99b3ab128a665e6adaaca")
(package! screenshot :recipe (:host github :repo "tecosaur/screenshot") :pin "ddb092d88d61330a3f562f67f051677b8de9ac07")
(package! standard-themes :recipe (:host github :repo "protesilaos/standard-themes") :pin "b7c89d4f2b36d0a7fcfdac8a7c5b13c132abeddb")
(package! systemd :recipe (:host github :repo "holomorph/systemd-mode") :pin "8742607120fbc440821acbc351fda1e8e68a8806" )
(package! treesit-auto :recipe (:host github :repo "renzmann/treesit-auto") :pin "f5ed138a4179058bf436f18953ed6dc493d9a124")
(package! verilog-mode :recipe (:host github :repo "veripool/verilog-mode") :pin "9395f121e0214d3ae385a5500e2c5c9edeb11ad6")
(package! vimrc-mode :pin "13bc150a870d5d4a95f1111e4740e2b22813c30e")
(package! visual-fill-column :pin "695a59789209c42fa08a5bce92963ee32f4455be")
(package! xr :pin "3e38048521f56a1414cb2d9adcd55ef5b270b557")                           ; xr is rx in reverse
;; end:sort

;; (package! jinx :recipe (:host github :repo "minad/jinx") :pin "b96d4c27e2f7472e923f7c4446c2061f1259819f") problems in snap?

;; (package! all-the-icons :pin "be9d5dcda9c892e8ca1535e288620eec075eb0be") ; supplanted by nerd icons?
;; (package! all-the-icons-dired :pin "83821b5cf3e221c2b00df2cee253a295983d1a28") ; supplanted by nerd icons?
;; (package! all-the-icons-ibuffer :pin "280c99bb32dae3f0c43e97756d495beaf4554460") ; supplanted by nerd icons?

;; (package! verilog-ext :recipe (:host github :repo "gmlarumbe/verilog-ext")) ;; brings in too many dependencies... ag aphelia lsp outorg outshine rg :(

;; Local Variables:
;; eval: (make-variable-buffer-local 'kill-buffer-hook)
;; eval: (add-hook 'kill-buffer-hook (lambda () (sort-elisp-block) (save-buffer)) nil t)
;; End:
