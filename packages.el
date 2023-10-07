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

(package! company :pin "b59662293cc9bd52bf6c7a4c3f70393351b52d71")

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
(package! benchmark-init :pin "02435560415bbadbcf5051fb7042880549170e7e")
(package! cape :recipe (:host github :repo "minad/cape") :pin "116063b9ee912cbaa7318dbe6597ade4a62b3f59")
(package! crontab-mode :pin "7412f3df0958812bfcacd5875a409fa795fa8ecc")
(package! delight :pin "46932bdc4da69072329aec8df052da3e04444899")
(package! dwim-shell-command :recipe (:host github :repo "xenodium/dwim-shell-command") :pin "3ad3db9155bdde0dc050690b2d50a57a3ac99aa4")
(package! elfeed :recipe (:host github :repo "skeeto/elfeed") :pin "55fb162fa27e71b88effa59a83c57842e262b00f" )
(package! emojify :pin "1b726412f19896abf5e4857d4c32220e33400b55")
(package! evil-collection :recipe (:host github :repo "emacs-evil/evil-collection") :pin "499415799e982c03b2557150df0c33293cbe41c7")
(package! browse-at-remote :recipe (:host github :repo "rmuslimov/browse-at-remote") :pin "76aa27dfd469fcae75ed7031bb73830831aaccbf")
(package! evil-leader :pin "39f7014bcf8b36463e0c7512c638bda4bac6c2cf")
(package! flycheck :recipe (:host github :repo "flycheck/flycheck") :pin "e56e30d8c66ffc9776d07740658d3b542c1a8e21")
(package! fzf :recipe (:host github :repo "bling/fzf.el") :pin "3a55b983921c620fb5a2cc811f42aa4daaad8266")
(package! graphviz-dot-mode :pin "8ff793b13707cb511875f56e167ff7f980a31136")
(package! logview :pin "00b3b01dfc99d1b3fb6d255e1ba9c3b4581cf065")
(package! magit-todos :pin "d85518d45d329cc0b465cc3b84910b7c66b3fc42")
(package! markdown-preview-mode :recipe (:host github :repo "ancane/markdown-preview-mode") :pin "68242b3907dc065aa35412bfd928b43d8052d321")
(package! org-modern :recipe (:host github :repo "minad/org-modern") :pin "afa7d44282d62dbba84afec2a1a6c2a3ee41e7b9")
(package! org-web-tools :pin "e79ca5039068f275c23d5501a7a0a182561a7b7f")
(package! plz :pin "545ed3c5179dc2432ba25ea06a402a083c0f871b")
(package! rainbow-mode :pin "41f4af40964cdb9d0cc3552eb450540fdc820455")
(package! scad-mode :pin "e1af74735ad6113448c99b3ab128a665e6adaaca")
(package! screenshot :recipe (:host github :repo "tecosaur/screenshot") :pin "ddb092d88d61330a3f562f67f051677b8de9ac07")
(package! standard-themes :recipe (:host github :repo "protesilaos/standard-themes") :pin "b7c89d4f2b36d0a7fcfdac8a7c5b13c132abeddb")
(package! systemd :recipe (:host github :repo "holomorph/systemd-mode") :pin "8742607120fbc440821acbc351fda1e8e68a8806" )
(package! treesit-auto :recipe (:host github :repo "renzmann/treesit-auto") :pin "bac3b9d1d61a4d759f87c80de7be3b808d19cbf6")
(package! verilog-mode :recipe (:host github :repo "veripool/verilog-mode") :pin "08237590afc44db3e19b62acfe7d8af16bcf05e1")
(package! vimrc-mode :pin "13bc150a870d5d4a95f1111e4740e2b22813c30e")
(package! visual-fill-column :pin "695a59789209c42fa08a5bce92963ee32f4455be")
(package! xr :pin "73803c5653ee491e4ff52b038aabf065e72ceadd")                           ; xr is rx in reverse
;; end:sort

;; (package! jinx :recipe (:host github :repo "minad/jinx")) ;; problems in snap?
;; (package! all-the-icons :pin "be9d5dcda9c892e8ca1535e288620eec075eb0be") ; supplanted by nerd icons?
;; (package! all-the-icons-dired :pin "83821b5cf3e221c2b00df2cee253a295983d1a28") ; supplanted by nerd icons?
;; (package! all-the-icons-ibuffer :pin "280c99bb32dae3f0c43e97756d495beaf4554460") ; supplanted by nerd icons?

;; (package! verilog-ext :recipe (:host github :repo "gmlarumbe/verilog-ext")) ;; brings in too many dependencies... ag aphelia lsp outorg outshine rg :(

;; Local Variables:
;; eval: (make-variable-buffer-local 'kill-buffer-hook)
;; eval: (add-hook 'kill-buffer-hook (lambda () (sort-elisp-block) (save-buffer)) nil t)
;; End:
