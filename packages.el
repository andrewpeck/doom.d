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
(package! affe                    :pin "a3d094e05e2d1461029920abb971ccd47ff8e5d0")
(package! apheleia                :pin "66bf5195b4e922f23a9d573f2823daeb63e7ed5b")
(package! ascii-art-to-unicode    :pin "79a73bd4f623f10135b6b65030f6d5e3070466db")
(package! backup-each-save        :pin "282766018efbeb4c4185506e0dd1f0f5a3c0b346" :recipe (:host github :repo "andrewpeck/backup-each-save"))
(package! benchmark-init          :pin "2b34432d79fa0aae8abc3db72db1cb79a28c00b2")
(package! browse-at-remote        :pin "76aa27dfd469fcae75ed7031bb73830831aaccbf" :recipe (:host github :repo "rmuslimov/browse-at-remote"))
(package! clojure-emacs           :pin "592c4f89efb5112784cbf94c9ea6fdd045771b62" :recipe (:host github :repo "clojure-emacs/squiggly-clojure"))
(package! code-cells.el           :pin "44546ca256f3da29e3ac884e3d699c8455acbd6e" :recipe (:host github :repo "astoff/code-cells.el"))
(package! comint-scroll-to-bottom :pin "1cd1471cfca72ac62d330630afe2f2fad44abcf5" :recipe (:host github :repo "jorgenschaefer/comint-scroll-to-bottom"))
(package! crontab-mode            :pin "7412f3df0958812bfcacd5875a409fa795fa8ecc")
(package! delight                 :pin "46932bdc4da69072329aec8df052da3e04444899")
(package! dired-filter            :pin "a01c126c3b1068655509487c76971895f5459d09" :recipe (:host github :repo "Fuco1/dired-hacks"))
(package! dired-git-info          :pin "1bfcdf139181afc9034a40dd0640e53d95b12f5f" :recipe (:host github :repo "tuh8888/dired-git-info"))
(package! dwim-shell-command      :pin "3dc879fa4c473d1f7c0929ebb0cebcbe729553cc" :recipe (:host github :repo "xenodium/dwim-shell-command"))
(package! elfeed                  :pin "5c05a1eab37bc113ecb158a4d57fe05352fa2c6a" :recipe (:host github :repo "skeeto/elfeed"))
(package! emacs-memoize           :pin "51b075935ca7070f62fae1d69fe0ff7d8fa56fdd" :recipe (:host github :repo "skeeto/emacs-memoize"))
(package! emacs-ucf-mode          :pin "6345afdbd373e6738e9c9d93694d4a5fb39e4a13" :recipe (:host github :repo "andrewpeck/emacs-ucf-mode"))
(package! emojify                 :pin "1b726412f19896abf5e4857d4c32220e33400b55")
(package! expand-region           :pin "e8f4e0fe9c9a80a6a26e2b438502aba9a799d580" :recipe (:host github :repo "magnars/expand-region.el"))
(package! flycheck                :pin "bec59eeb58f86b10f8235bf94c873f3b0352ac41" :recipe (:host github :repo "flycheck/flycheck"))
(package! flycheck-clj-kondo      :pin "e38c67ba9db1ea1cbe1b61ab39b506c05efdcdbf" :recipe (:host github :repo "borkdude/flycheck-clj-kondo"))
(package! fzf                     :pin "3a55b983921c620fb5a2cc811f42aa4daaad8266" :recipe (:host github :repo "bling/fzf.el"))
(package! graphviz-dot-mode       :pin "8ff793b13707cb511875f56e167ff7f980a31136")
(package! hdl_deps                :pin "6adb437647edc9e17ce1ddf6e88f2571f8945179" :recipe (:host github :repo "andrewpeck/hdl_deps"))
(package! hog-emacs               :pin "da58d254eb1f45a6ab35544448c93da2e3d7d1ed" :recipe (:host github :repo "andrewpeck/hog-emacs"))
(package! jinx                    :pin "f4abad42cd0bdcff62ac325a644cb8faad186a4b" :recipe (:host github :repo "minad/jinx")) ;problems in snap?
(package! logview                 :pin "8eb158ef7af412b0336a10d0273a292adddd4eb0")
(package! magit-todos             :pin "332ce763f7336ea356964b92723678aa1ed4640f")
(package! markdown-preview-mode   :pin "68242b3907dc065aa35412bfd928b43d8052d321" :recipe (:host github :repo "ancane/markdown-preview-mode"))
(package! nerd-icons.el           :pin "fb395120e9de33b276d16caaccaefd98d4340b92" :recipe (:host github :repo "rainstormstudio/nerd-icons.el"))
(package! ob-wavedrom             :pin "85b12dba1a5ba2496e6bc377c25ce365fa221fbf" :recipe (:host github :repo "andrewpeck/ob-wavedrom"))
(package! org-download            :pin "19e166f0a8c539b4144cfbc614309d47a9b2a9b7" :recipe (:host github :repo "abo-abo/org-download"))
(package! org-web-tools           :pin "7a6498f442fc7f29504745649948635c7165d847")
(package! plz                     :pin "f9601afac886a4409cf389cbf0e4beb6471bb9d8")
(package! poporg                  :pin "2c58d68c81ecca4140bf179f19ed153ec804b65a" :recipe (:host github :repo "QBobWatson/poporg"))
(package! rainbow-mode            :pin "70ed10d410ef00c82c49b2ba41647930626d6218")
(package! scad-mode               :pin "1bae3dab16adf1f0d47befcc6d2b63a50af87231")
(package! screenshot              :pin "2770c0cfefe1cc09d55585f4f2f336a1b26e610e" :recipe (:host github :repo "tecosaur/screenshot"))
(package! standard-themes         :pin "b76092042307f445dcc1a16d780ff05f7978070d" :recipe (:host github :repo "protesilaos/standard-themes"))
(package! svg-tag-mode            :pin "f01307dd7720ed952355db0a552ae555c13ced13" :recipe (:host github :repo "rougier/svg-tag-mode"))
(package! system-install.el       :pin "df93485695659c7b827d64270ba70d096682b33d" :recipe (:host github :repo "andrewpeck/system-install.el"))
(package! systemd                 :pin "8742607120fbc440821acbc351fda1e8e68a8806" :recipe (:host github :repo "holomorph/systemd-mode"))
(package! toc-org                 :pin "6d3ae0fc47ce79b1ea06cabe21a3c596395409cd")
(package! verilog-port-copy       :pin "4bdb806e87c992eefc877a2a0a2083414920187b" :recipe (:host github :repo "andrewpeck/verilog-port-copy"))
(package! vimrc-mode              :pin "13bc150a870d5d4a95f1111e4740e2b22813c30e")
(package! visual-fill-column      :pin "5b9f9309bdf040b72bb2c3d99d1d8a0f0d98c308")
(package! vivado_mode             :pin "33bfb1f8eb991f76d3420ecf5ad87d4cd7526392" :recipe (:host github :repo "andrewpeck/vivado_mode"))
(package! wavedrom-mode           :pin "02e07787791d0ffa0f919cd16dfc1352390beee6" :recipe (:host github :repo "gmlarumbe/wavedrom-mode"))
(package! xr                      :pin "2a225260a747dd3c091f6b02108277ce0b97c566")                           ; xr is rx in reverse
;; end:sort

;; (package! eglot-booster           :pin "e19dd7ea81bada84c66e8bdd121408d9c0761fe6" :recipe (:host github :repo "jdtsmith/eglot-booster"))
;; (package! tabular :recipe (:host github :repo "andrewpeck/tabular"))
;; (package! org-modern            :pin "240026f0bef6dabff1e86ab092a678beddd301c3" :recipe (:host github :repo "minad/org-modern"))
;; (package! verilog-ext :recipe (:host github :repo "gmlarumbe/verilog-ext")) ;; brings in too many dependencies... ag aphelia lsp outorg outshine rg :(

;; Local Variables:
;; eval: (make-variable-buffer-local 'kill-buffer-hook)
;; eval: (add-hook 'kill-buffer-hook (lambda () (sort-elisp-block) (save-buffer)) nil t)
;; End:
