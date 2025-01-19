;; Disabled packages
(package! pyenv-mode :disable t)
(package! pyenv      :disable t)
(package! hl-line   :disable t)
(package! diredfl   :disable t)
(package! haml-mode  :disable t)
(package! lsp       :disable t)
(package! powerline :disable t)
(package! popwin    :disable t)
(package! ivy       :disable t)
(package! swiper    :disable t)
(package! org-yt    :disable t)
(package! orgit-forge :disable t)
(package! org-pdftools :disable t)

;; Misc packages
;; start:sort
(package! affe                    :pin "649daaa9087446d14f11af9d52268cfb930e4a08")
(package! almost-mono-themes      :pin "0641bf565c113caef8d5c2a93f38cff32ebb62b7" :recipe (:host github :repo "cryon/almost-mono-themes"))
(package! apheleia                :pin "fa50cb3a3c7cb8f2b4216760c05f8a885a3ce906")
(package! ascii-art-to-unicode    :pin "fa3d82ddb531855ad7b8ef871871907861327c9f")
(package! backup-each-save        :pin "9d9714be7bf99b2a192587c1454822c69c61915e" :recipe (:host github :repo "andrewpeck/backup-each-save"))
(package! benchmark-init          :pin "2b34432d79fa0aae8abc3db72db1cb79a28c00b2")
(package! browse-at-remote        :pin "76aa27dfd469fcae75ed7031bb73830831aaccbf" :recipe (:host github :repo "rmuslimov/browse-at-remote"))
(package! casual                  :pin "2d1cfd6fe4b16393770a7826026b48948eccb02a" :recipe (:host github :repo "kickingvegas/casual"))
(package! clojure-emacs           :pin "592c4f89efb5112784cbf94c9ea6fdd045771b62" :recipe (:host github :repo "clojure-emacs/squiggly-clojure"))
(package! code-cells.el           :pin "caffb420be106cebbdfe4474ed0507a601603f83" :recipe (:host github :repo "astoff/code-cells.el"))
(package! comint-scroll-to-bottom :pin "1cd1471cfca72ac62d330630afe2f2fad44abcf5" :recipe (:host github :repo "jorgenschaefer/comint-scroll-to-bottom"))
(package! crontab-mode            :pin "7412f3df0958812bfcacd5875a409fa795fa8ecc")
(package! delight                 :pin "15acb0f0ba400c470e378f9984b315f9e02c1122")
(package! dired-filter            :pin "e9e408e8571aee5574ca0a431ef15cac5a3585d4" :recipe (:host github :repo "Fuco1/dired-hacks"))
(package! dired-git-info          :pin "1bfcdf139181afc9034a40dd0640e53d95b12f5f" :recipe (:host github :repo "tuh8888/dired-git-info"))
(package! drag-stuff              :pin "6d06d846cd37c052d79acd0f372c13006aa7e7c8")
(package! dwim-shell-command      :pin "1fa8b9d361f01618bf65111ac0b253adb0599a09" :recipe (:host github :repo "xenodium/dwim-shell-command"))
(package! ebib                    :pin "c1100497e8b23a00190d65b8a5ed5f974d2c9f8d" :recipe (:host github :repo "joostkremers/ebib"))
(package! eldoc-box               :pin "1a2f604ef290bd50f46ad7b324b058c4650d272f" :recipe (:host github :repo "casouri/eldoc-box"))
(package! elfeed                  :pin "a39fb78e34ee25dc8baea83376f929d7c128344f" :recipe (:host github :repo "skeeto/elfeed"))
(package! emacs-memoize           :pin "51b075935ca7070f62fae1d69fe0ff7d8fa56fdd" :recipe (:host github :repo "skeeto/emacs-memoize"))
(package! emacs-pet               :pin "d9aa6a4dd2d7ce52febeafc31ac84d861ac1e222" :recipe (:host github :repo "wyuenho/emacs-pet"))
(package! emacs-ucf-mode          :pin "6345afdbd373e6738e9c9d93694d4a5fb39e4a13" :recipe (:host github :repo "andrewpeck/emacs-ucf-mode"))
(package! emojify                 :pin "1b726412f19896abf5e4857d4c32220e33400b55")
(package! expand-region           :pin "351279272330cae6cecea941b0033a8dd8bcc4e8" :recipe (:host github :repo "magnars/expand-region.el"))
(package! flycheck-clj-kondo      :pin "e38c67ba9db1ea1cbe1b61ab39b506c05efdcdbf" :recipe (:host github :repo "borkdude/flycheck-clj-kondo"))
(package! flycheck-pycheckers     :pin "1bd9b7a7d4009a81ebd34515a72a3a94c313ad76" :recipe (:host github :repo "msherry/flycheck-pycheckers"))
(package! fzf                     :pin "641aef33c88df3733f13d559bcb2acc548a4a0c3" :recipe (:host github :repo "bling/fzf.el"))
(package! gptel                   :pin "447a37e715323bc447bbb37e045e0dbacd8e7bad")
(package! graphviz-dot-mode       :pin "8ff793b13707cb511875f56e167ff7f980a31136")
(package! hdl_deps                :pin "6adb437647edc9e17ce1ddf6e88f2571f8945179" :recipe (:host github :repo "andrewpeck/hdl_deps"))
(package! hog-emacs               :pin "16900610b7517c03537d7c2f2eab0b26cfac984a" :recipe (:host github :repo "andrewpeck/hog-emacs"))
(package! hydra                   :pin "317e1de33086637579a7aeb60f77ed0405bf359b")
(package! jinx                    :pin "8407ed958001f6a4718f225737665e50e6666dbf" :recipe (:host github :repo "minad/jinx"))
(package! logview                 :pin "034c240c816188bf8be7441c9b0925abb92e861c")
(package! lorem-ipsum)
(package! lte.el :recipe (:host github :repo "fredericgiquel/lte.el"))
(package! magit-todos             :pin "bd27c57eada0fda1cc0a813db04731a9bcc51b7b")
(package! markdown-preview-mode   :pin "68242b3907dc065aa35412bfd928b43d8052d321" :recipe (:host github :repo "ancane/markdown-preview-mode"))
(package! nerd-icons.el           :pin "546ee20caf825e65da4c5435d31f13d269ed2a81" :recipe (:host github :repo "rainstormstudio/nerd-icons.el"))
(package! ob-wavedrom             :pin "85b12dba1a5ba2496e6bc377c25ce365fa221fbf" :recipe (:host github :repo "andrewpeck/ob-wavedrom"))
(package! olivetti                :pin "845eb7a95a3ca3325f1120c654d761b91683f598")
(package! org-appear              :pin "32ee50f8fdfa449bbc235617549c1bccb503cb09" :recipe (:host github :repo "awth13/org-appear"))
(package! org-download            :pin "c8be2611786d1d8d666b7b4f73582de1093f25ac" :recipe (:host github :repo "abo-abo/org-download"))
(package! org-excalidraw          :pin "a6e57489a5a171869ca0ec57c481f19d4e957f6e" :recipe (:host github :repo "andrewpeck/org-excalidraw") )
(package! org-mode                :pin "abbc024aef5ce86f9c181b7f8f6048b04b57c4c1" :recipe (:host github :repo "emacs-straight/org-mode"))
(package! org-web-tools           :pin "7a6498f442fc7f29504745649948635c7165d847")
(package! p-search                :pin "9ab409b4cf44817eb93e9c248e8c1b592ee28e40" :recipe (:host github :repo "zkry/p-search"))
(package! plz                     :pin "4eeff1323bc3d9d5cb67d922341d3b2f68fa075a")
(package! poporg                  :pin "2c58d68c81ecca4140bf179f19ed153ec804b65a" :recipe (:host github :repo "QBobWatson/poporg"))
(package! rainbow-mode            :pin "2e6b18609c2fdd1a2dc513937a64d276fd6cf24c")
(package! scad-mode               :pin "7f74580a34fa80b44412dc2e6b40a0ff332a20e5")
(package! screenshot              :pin "2770c0cfefe1cc09d55585f4f2f336a1b26e610e" :recipe (:host github :repo "tecosaur/screenshot"))
(package! standard-themes         :pin "8380916d589c90e44dac198b09f5e02273cd357a" :recipe (:host github :repo "protesilaos/standard-themes"))
(package! svg-tag-mode            :pin "13e888b8bd9a0664d060149a44a751b2113331b6" :recipe (:host github :repo "rougier/svg-tag-mode"))
(package! system-install.el       :pin "df93485695659c7b827d64270ba70d096682b33d" :recipe (:host github :repo "andrewpeck/system-install.el"))
(package! systemd                 :pin "8742607120fbc440821acbc351fda1e8e68a8806" :recipe (:host github :repo "holomorph/systemd-mode"))
(package! toc-org                 :pin "6d3ae0fc47ce79b1ea06cabe21a3c596395409cd")
(package! treesit-auto            :pin "016bd286a1ba4628f833a626f8b9d497882ecdf3" :recipe (:host github :repo "renzmann/treesit-auto"))
(package! verilog-mode            :pin "5f866524d29cc82072c0d91ebe585a4ba8fac453" :recipe (:host github :repo "veripool/verilog-mode"))
(package! verilog-port-copy       :pin "a103f8e2209dde19d26d2151f35fe8e53ab3d336" :recipe (:host github :repo "andrewpeck/verilog-port-copy"))
(package! vimrc-mode              :pin "13bc150a870d5d4a95f1111e4740e2b22813c30e")
(package! visual-fill-column      :pin "11575df4e042f3263db62de0e8ee4f2331fbb1b4")
(package! vivado_mode             :pin "d0192c2919855bbc96f269f75b49a5a95242eeb1" :recipe (:host github :repo "andrewpeck/vivado_mode"))
(package! wavedrom-mode           :pin "93c463a5650482a3072742a13156b284b01e49b6" :recipe (:host github :repo "gmlarumbe/wavedrom-mode"))
(package! xr                      :pin "7cbd4bfde777606c99a5f20fada8195e589fb240")                           ; xr is rx in reverse
;; end:sort

;; (package! verilog-ext             :pin "d504f59eac2fa04f1d627d7acab639f79a2356fe" :recipe (:host github :repo "gmlarumbe/verilog-ext"))
;; (package! eglot-booster           :pin "e19dd7ea81bada84c66e8bdd121408d9c0761fe6" :recipe (:host github :repo "jdtsmith/eglot-booster"))
;; (package! tabular :recipe (:host github :repo "andrewpeck/tabular"))
;; (package! org-modern            :pin "240026f0bef6dabff1e86ab092a678beddd301c3" :recipe (:host github :repo "minad/org-modern"))
;; (package! verilog-ext :recipe (:host github :repo "gmlarumbe/verilog-ext")) ;; brings in too many dependencies... ag aphelia lsp outorg outshine rg :(

;; Local Variables:
;; eval: (make-variable-buffer-local 'kill-buffer-hook)
;; eval: (add-hook 'kill-buffer-hook (lambda () (sort-elisp-block) (save-buffer)) nil t)
;; End:
