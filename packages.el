;; -*- lexical-binding: t; no-byte-compile: t;-*-

;; Disabled packages
(package! hl-line   :disable t)
(package! diredfl   :disable t)
(package! lsp       :disable t)
(package! powerline :disable t)
(package! popwin    :disable t)
(package! ivy       :disable t)
(package! swiper    :disable t)

;; Misc packages
;; start:sort
(package! affe                  :pin "1fd5732afa5d68b120fd2e949702b1abde0466d7")
(package! apheleia              :pin "96a9805ecb75aac2adde7568d26b3e3b3ffc19af")
(package! ascii-art-to-unicode  :pin "79a73bd4f623f10135b6b65030f6d5e3070466db")
(package! backup-each-save      :pin "282766018efbeb4c4185506e0dd1f0f5a3c0b346" :recipe (:host github :repo "andrewpeck/backup-each-save"))
(package! benchmark-init        :pin "02435560415bbadbcf5051fb7042880549170e7e")
(package! browse-at-remote      :pin "76aa27dfd469fcae75ed7031bb73830831aaccbf" :recipe (:host github :repo "rmuslimov/browse-at-remote"))
(package! cape                  :pin "f021305d20a40021058fdd31f9acddc86e463eaf" :recipe (:host github :repo "minad/cape"))
(package! clojure-emacs         :pin "592c4f89efb5112784cbf94c9ea6fdd045771b62" :recipe (:host github :repo "clojure-emacs/squiggly-clojure"))
(package! company               :pin "0cf923231702b8fb7e494af1ae06666a7aa5851d")
(package! crontab-mode          :pin "7412f3df0958812bfcacd5875a409fa795fa8ecc")
(package! delight               :pin "46932bdc4da69072329aec8df052da3e04444899")
(package! dired-filter          :pin "874449d6fc98aee565e1715ec18acec3c1c2cafb" :recipe (:host github :repo "Fuco1/dired-hacks"))
(package! dired-git-info        :pin "1bfcdf139181afc9034a40dd0640e53d95b12f5f" :recipe (:host github :repo "tuh8888/dired-git-info"))
(package! dwim-shell-command    :pin "7a2c298424466d2bff7c050e01fb85b5f882dbc3" :recipe (:host github :repo "xenodium/dwim-shell-command"))
(package! elfeed                :pin "55fb162fa27e71b88effa59a83c57842e262b00f" :recipe (:host github :repo "skeeto/elfeed"))
(package! emacs-ucf-mode        :pin "6345afdbd373e6738e9c9d93694d4a5fb39e4a13" :recipe (:host github :repo "andrewpeck/emacs-ucf-mode"))
(package! emojify               :pin "1b726412f19896abf5e4857d4c32220e33400b55")
(package! evil-collection       :pin "8c84f9bc89fe56e71b56519f886085ddcbc671cf" :recipe (:host github :repo "emacs-evil/evil-collection"))
(package! evil-leader           :pin "39f7014bcf8b36463e0c7512c638bda4bac6c2cf")
(package! evil-org              :pin "a9706da260c45b98601bcd72b1d2c0a24a017700")
(package! expand-region         :pin "af2916b89d8b5e520ff4ee7214bab62a0fdb3cec" :recipe (:host github :repo "magnars/expand-region.el"))
(package! flycheck              :pin "e56e30d8c66ffc9776d07740658d3b542c1a8e21" :recipe (:host github :repo "flycheck/flycheck"))
(package! flycheck-clj-kondo    :pin "ff7bed2315755cfe02ef471edf522e27b78cd5ca" :recipe (:host github :repo "borkdude/flycheck-clj-kondo"))
(package! fzf                   :pin "3a55b983921c620fb5a2cc811f42aa4daaad8266" :recipe (:host github :repo "bling/fzf.el"))
(package! graphviz-dot-mode     :pin "8ff793b13707cb511875f56e167ff7f980a31136")
(package! hdl_deps              :pin "6adb437647edc9e17ce1ddf6e88f2571f8945179" :recipe (:host github :repo "andrewpeck/hdl_deps"))
(package! hog-emacs             :pin "6d2184bc6404abe1c96ac36498f57217e8ed2274" :recipe (:host github :repo "andrewpeck/hog-emacs"))
(package! logview               :pin "9140067afdc2f0d1eb493dc4dfdb53645289dd2b")
(package! magit-todos           :pin "debb77b3589f2d83c8b43706edc1f8f90bf1ad91")
(package! markdown-preview-mode :pin "68242b3907dc065aa35412bfd928b43d8052d321" :recipe (:host github :repo "ancane/markdown-preview-mode"))
(package! ob-wavedrom           :pin "85b12dba1a5ba2496e6bc377c25ce365fa221fbf" :recipe (:host github :repo "andrewpeck/ob-wavedrom"))
(package! org-download          :pin "19e166f0a8c539b4144cfbc614309d47a9b2a9b7" :recipe (:host github :repo "abo-abo/org-download"))
(package! org-web-tools         :pin "7a6498f442fc7f29504745649948635c7165d847")
(package! ox-clip               :pin "ff117cf3c619eef12eccc0ccbfa3f11adb73ea68" :recipe (:host github :repo "jkitchin/ox-clip"))
(package! ox-gfm                :pin "4f774f13d34b3db9ea4ddb0b1edc070b1526ccbb")
(package! ox-pandoc             :pin "399d787b6e2124bd782615338b845c3724a47718")
(package! plz                   :pin "8f42fe1c28fb8eacdc760847a43e1e3118f7b9f7")
(package! poporg                :pin "2c58d68c81ecca4140bf179f19ed153ec804b65a" :recipe (:host github :repo "QBobWatson/poporg"))
(package! rainbow-mode          :pin "70ed10d410ef00c82c49b2ba41647930626d6218")
(package! scad-mode             :pin "cd7070569f4ada5469087773e1b542514ed19974")
(package! screenshot            :pin "ddb092d88d61330a3f562f67f051677b8de9ac07" :recipe (:host github :repo "tecosaur/screenshot"))
(package! standard-themes       :pin "52d5f0b3bde6b25063fff34b604ab95784630cff" :recipe (:host github :repo "protesilaos/standard-themes"))
(package! svg-tag-mode          :pin "03989f66b45944271faa6dd8eb977d623561de86" :recipe (:host github :repo "rougier/svg-tag-mode"))
(package! system-install.el     :pin "df93485695659c7b827d64270ba70d096682b33d" :recipe (:host github :repo "andrewpeck/system-install.el"))
(package! systemd               :pin "8742607120fbc440821acbc351fda1e8e68a8806" :recipe (:host github :repo "holomorph/systemd-mode"))
(package! toc-org               :pin "6d3ae0fc47ce79b1ea06cabe21a3c596395409cd")
(package! treesit-auto          :pin "3686c6bf12da93c1f62869dc1000818763b63310" :recipe (:host github :repo "renzmann/treesit-auto"))
(package! verilog-mode          :pin "9105afa401c040f6dd8f76039ef3f410136a6e74" :recipe (:host github :repo "veripool/verilog-mode"))
(package! verilog-port-copy     :pin "4166c8f51b196408516348f9985bb9ba39b9e193" :recipe (:host github :repo "andrewpeck/verilog-port-copy"))
(package! vimrc-mode            :pin "13bc150a870d5d4a95f1111e4740e2b22813c30e")
(package! visual-fill-column    :pin "db7c7c236555c9c684e1294a277efefdc25fa5c4")
(package! vivado_mode           :pin "33bfb1f8eb991f76d3420ecf5ad87d4cd7526392" :recipe (:host github :repo "andrewpeck/vivado_mode"))
(package! wavedrom-mode         :pin "15718ab8eba8b0fce4ab5387b13421059f1b128f" :recipe (:host github :repo "gmlarumbe/wavedrom-mode"))
(package! xr                    :pin "2a225260a747dd3c091f6b02108277ce0b97c566")                           ; xr is rx in reverse
(package! yasnippet             :pin "3e14a8534f930e7116d576992c93d33bf7ee38c9" :recipe (:host github :repo "joaotavora/yasnippet") )
(package! yasnippet-capf        :pin "db12b55cd08b614cbba134008566e48d7faf660e" :recipe (:host github :repo "elken/yasnippet-capf") )
;; end:sort

;; (package! tabular :recipe (:host github :repo "andrewpeck/tabular"))
;; (package! org-modern            :pin "240026f0bef6dabff1e86ab092a678beddd301c3" :recipe (:host github :repo "minad/org-modern"))
;; (package! jinx :recipe (:host github :repo "minad/jinx") :pin "b96d4c27e2f7472e923f7c4446c2061f1259819f") problems in snap?
;; (package! verilog-ext :recipe (:host github :repo "gmlarumbe/verilog-ext")) ;; brings in too many dependencies... ag aphelia lsp outorg outshine rg :(

;; Local Variables:
;; eval: (make-variable-buffer-local 'kill-buffer-hook)
;; eval: (add-hook 'kill-buffer-hook (lambda () (sort-elisp-block) (save-buffer)) nil t)
;; End:
