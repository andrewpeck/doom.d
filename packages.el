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
(package! affe                  :pin "2f2423effdcec2731ab865f513fd92bb96d0eb03")
(package! ascii-art-to-unicode  :pin "79a73bd4f623f10135b6b65030f6d5e3070466db")
(package! benchmark-init        :pin "02435560415bbadbcf5051fb7042880549170e7e")
(package! browse-at-remote      :pin "76aa27dfd469fcae75ed7031bb73830831aaccbf" :recipe (:host github :repo "rmuslimov/browse-at-remote"))
(package! cape                  :pin "f9f8e4a1e36b0ee7972c35bc0a635cbed9959aa0" :recipe (:host github :repo "minad/cape"))
(package! clojure-emacs         :pin "592c4f89efb5112784cbf94c9ea6fdd045771b62" :recipe (:host github :repo "clojure-emacs/squiggly-clojure"))
(package! company               :pin "9b21604d19696de2c79ee28931620839b3a908b4")
(package! crontab-mode          :pin "7412f3df0958812bfcacd5875a409fa795fa8ecc")
(package! delight               :pin "46932bdc4da69072329aec8df052da3e04444899")
(package! dwim-shell-command    :pin "e4a139fe181ed1b576302fb36f3761fbf9914580" :recipe (:host github :repo "xenodium/dwim-shell-command"))
(package! elfeed                :pin "55fb162fa27e71b88effa59a83c57842e262b00f"  :recipe (:host github :repo "skeeto/elfeed"))
(package! emojify               :pin "1b726412f19896abf5e4857d4c32220e33400b55")
(package! evil-collection       :pin "18304d9d9e4243687a7bf915dc7e591a216873c6" :recipe (:host github :repo "emacs-evil/evil-collection"))
(package! evil-leader           :pin "39f7014bcf8b36463e0c7512c638bda4bac6c2cf")
(package! evil-org              :pin "a9706da260c45b98601bcd72b1d2c0a24a017700")
(package! expand-region         :pin "9e3f86c02c5e2ab6f0d95da8a34045b54f6166d1" :recipe (:host github :repo "magnars/expand-region.el"))
(package! flycheck              :pin "e56e30d8c66ffc9776d07740658d3b542c1a8e21" :recipe (:host github :repo "flycheck/flycheck"))
(package! flycheck-clj-kondo    :pin "ff7bed2315755cfe02ef471edf522e27b78cd5ca" :recipe (:host github :repo "borkdude/flycheck-clj-kondo"))
(package! fzf                   :pin "3a55b983921c620fb5a2cc811f42aa4daaad8266" :recipe (:host github :repo "bling/fzf.el"))
(package! graphviz-dot-mode     :pin "8ff793b13707cb511875f56e167ff7f980a31136")
(package! logview               :pin "9140067afdc2f0d1eb493dc4dfdb53645289dd2b")
(package! magit-todos           :pin "d0646dbbf46d75d08e3d7b4c665d7d763a468af1")
(package! markdown-preview-mode :pin "68242b3907dc065aa35412bfd928b43d8052d321" :recipe (:host github :repo "ancane/markdown-preview-mode"))
(package! org-download          :pin "19e166f0a8c539b4144cfbc614309d47a9b2a9b7" :recipe (:host github :repo "abo-abo/org-download"))
(package! org-modern            :pin "240026f0bef6dabff1e86ab092a678beddd301c3" :recipe (:host github :repo "minad/org-modern"))
(package! org-web-tools         :pin "821e6f032f5823e5e3344ea4d1a36870e236aba1")
(package! ox-clip               :pin "ff117cf3c619eef12eccc0ccbfa3f11adb73ea68" :recipe (:host github :repo "jkitchin/ox-clip"))
(package! ox-gfm                :pin "46faa67dbb3fb0cd7a76c3fe518f16e4195c22c7")
(package! ox-pandoc             :pin "0f758517f512e375825679541b5d905be40342eb")
(package! plz                   :pin "545ed3c5179dc2432ba25ea06a402a083c0f871b")
(package! rainbow-mode          :pin "41f4af40964cdb9d0cc3552eb450540fdc820455")
(package! scad-mode             :pin "e1af74735ad6113448c99b3ab128a665e6adaaca")
(package! screenshot            :pin "ddb092d88d61330a3f562f67f051677b8de9ac07" :recipe (:host github :repo "tecosaur/screenshot"))
(package! standard-themes       :pin "fba33e81789e33c81e8434aeb4ddddd813a940d7" :recipe (:host github :repo "protesilaos/standard-themes"))
(package! systemd               :pin "8742607120fbc440821acbc351fda1e8e68a8806" :recipe (:host github :repo "holomorph/systemd-mode"))
(package! toc-org               :pin "6d3ae0fc47ce79b1ea06cabe21a3c596395409cd")
(package! treesit-auto          :pin "78b38c4ded525311d6fb1130bfa7ef03273033f5" :recipe (:host github :repo "renzmann/treesit-auto"))
(package! verilog-mode          :pin "9395f121e0214d3ae385a5500e2c5c9edeb11ad6" :recipe (:host github :repo "veripool/verilog-mode"))
(package! vimrc-mode            :pin "13bc150a870d5d4a95f1111e4740e2b22813c30e")
(package! visual-fill-column    :pin "db7c7c236555c9c684e1294a277efefdc25fa5c4")
(package! xr                    :pin "3e38048521f56a1414cb2d9adcd55ef5b270b557")                           ; xr is rx in reverse
;; (package! jinx :recipe (:host github :repo "minad/jinx") :pin "b96d4c27e2f7472e923f7c4446c2061f1259819f") problems in snap?
;; (package! verilog-ext :recipe (:host github :repo "gmlarumbe/verilog-ext")) ;; brings in too many dependencies... ag aphelia lsp outorg outshine rg :(
;; end:sort

;; Local Variables:
;; eval: (make-variable-buffer-local 'kill-buffer-hook)
;; eval: (add-hook 'kill-buffer-hook (lambda () (sort-elisp-block) (save-buffer)) nil t)
;; End:
