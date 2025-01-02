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
(package! affe                    :pin "569c661aa2333502759aee2d4c7c87a046b09d33")
(package! almost-mono-themes      :pin "0641bf565c113caef8d5c2a93f38cff32ebb62b7" :recipe (:host github :repo "cryon/almost-mono-themes"))
(package! apheleia                :pin "d6f520752a77923a420f2ef894a6f2d26d29d7d0")
(package! ascii-art-to-unicode    :pin "fa3d82ddb531855ad7b8ef871871907861327c9f")
(package! backup-each-save        :pin "9d9714be7bf99b2a192587c1454822c69c61915e" :recipe (:host github :repo "andrewpeck/backup-each-save"))
(package! benchmark-init          :pin "2b34432d79fa0aae8abc3db72db1cb79a28c00b2")
(package! browse-at-remote        :pin "76aa27dfd469fcae75ed7031bb73830831aaccbf" :recipe (:host github :repo "rmuslimov/browse-at-remote"))
(package! casual                  :pin "72126c3fae2584f8f542ce409f46fa92a9b8b456" :recipe (:host github :repo "kickingvegas/casual"))
(package! clojure-emacs           :pin "592c4f89efb5112784cbf94c9ea6fdd045771b62" :recipe (:host github :repo "clojure-emacs/squiggly-clojure"))
(package! code-cells.el           :pin "6511793ce9092c3f68f7cd5340267472a4b1b7dc" :recipe (:host github :repo "astoff/code-cells.el"))
(package! comint-scroll-to-bottom :pin "1cd1471cfca72ac62d330630afe2f2fad44abcf5" :recipe (:host github :repo "jorgenschaefer/comint-scroll-to-bottom"))
(package! crontab-mode            :pin "7412f3df0958812bfcacd5875a409fa795fa8ecc")
(package! delight                 :pin "15acb0f0ba400c470e378f9984b315f9e02c1122")
(package! dired-filter            :pin "e9e408e8571aee5574ca0a431ef15cac5a3585d4" :recipe (:host github :repo "Fuco1/dired-hacks"))
(package! dired-git-info          :pin "1bfcdf139181afc9034a40dd0640e53d95b12f5f" :recipe (:host github :repo "tuh8888/dired-git-info"))
(package! drag-stuff              :pin "6d06d846cd37c052d79acd0f372c13006aa7e7c8")
(package! dwim-shell-command      :pin "9ee831bf05eb54f04baeb3d2e57d70752661e286" :recipe (:host github :repo "xenodium/dwim-shell-command"))
(package! ebib                    :pin "315a8b067059f7d2c4e19220586dfbb042161fc4" :recipe (:host github :repo "joostkremers/ebib"))
(package! elfeed                  :pin "904b6d4feca78e7e5336d7dbb7b8ba53b8c4dac1" :recipe (:host github :repo "skeeto/elfeed"))
(package! emacs-memoize           :pin "51b075935ca7070f62fae1d69fe0ff7d8fa56fdd" :recipe (:host github :repo "skeeto/emacs-memoize"))
(package! emacs-pet               :pin "75b371ac4638cb8c6d7190f21a9c3ff257798d61" :recipe (:host github :repo "wyuenho/emacs-pet"))
(package! emacs-ucf-mode          :pin "6345afdbd373e6738e9c9d93694d4a5fb39e4a13" :recipe (:host github :repo "andrewpeck/emacs-ucf-mode"))
(package! emojify                 :pin "1b726412f19896abf5e4857d4c32220e33400b55")
(package! expand-region           :pin "541d971f7c77ca5c0f66c88bcbfeb0d165883023" :recipe (:host github :repo "magnars/expand-region.el"))
(package! flycheck-clj-kondo      :pin "e38c67ba9db1ea1cbe1b61ab39b506c05efdcdbf" :recipe (:host github :repo "borkdude/flycheck-clj-kondo"))
(package! flycheck-pycheckers     :pin "1bd9b7a7d4009a81ebd34515a72a3a94c313ad76" :recipe (:host github :repo "msherry/flycheck-pycheckers"))
(package! fzf                     :pin "641aef33c88df3733f13d559bcb2acc548a4a0c3" :recipe (:host github :repo "bling/fzf.el"))
(package! gptel)
(package! graphviz-dot-mode       :pin "8ff793b13707cb511875f56e167ff7f980a31136")
(package! hdl_deps                :pin "6adb437647edc9e17ce1ddf6e88f2571f8945179" :recipe (:host github :repo "andrewpeck/hdl_deps"))
(package! hog-emacs               :pin "16900610b7517c03537d7c2f2eab0b26cfac984a" :recipe (:host github :repo "andrewpeck/hog-emacs"))
(package! hydra                   :pin "317e1de33086637579a7aeb60f77ed0405bf359b")
(package! jinx                    :pin "316e470633d9bbe365079cf237e3ddef1f37a80e" :recipe (:host github :repo "minad/jinx"))
(package! logview                 :pin "8e020b9296b8e1adc810ebc9d36985f64d93dbc2")
(package! magit-todos             :pin "bd27c57eada0fda1cc0a813db04731a9bcc51b7b")
(package! markdown-preview-mode   :pin "68242b3907dc065aa35412bfd928b43d8052d321" :recipe (:host github :repo "ancane/markdown-preview-mode"))
(package! nerd-icons.el           :pin "c3d641d8e14bd11b5f98372da34ee5313636e363" :recipe (:host github :repo "rainstormstudio/nerd-icons.el"))
(package! ob-wavedrom             :pin "85b12dba1a5ba2496e6bc377c25ce365fa221fbf" :recipe (:host github :repo "andrewpeck/ob-wavedrom"))
(package! olivetti                :pin "1a6a521273e70173af6dcf34e3c9f8bb97dd7afc")
(package! org-appear              :pin "32ee50f8fdfa449bbc235617549c1bccb503cb09" :recipe (:host github :repo "awth13/org-appear"))
(package! org-download            :pin "19e166f0a8c539b4144cfbc614309d47a9b2a9b7" :recipe (:host github :repo "abo-abo/org-download"))
(package! org-excalidraw          :pin "edbae1b1d002e7d93a45a741c2b55b3b0d0b79d3" :recipe (:host github :repo "andrewpeck/org-excalidraw") )
(package! org-web-tools           :pin "7a6498f442fc7f29504745649948635c7165d847")
(package! p-search                :pin "50adec02415af2626928663b41b0de708707c3ce" :recipe (:host github :repo "zkry/p-search"))
(package! plz                     :pin "4eeff1323bc3d9d5cb67d922341d3b2f68fa075a")
(package! poporg                  :pin "2c58d68c81ecca4140bf179f19ed153ec804b65a" :recipe (:host github :repo "QBobWatson/poporg"))
(package! rainbow-mode            :pin "2e6b18609c2fdd1a2dc513937a64d276fd6cf24c")
(package! scad-mode               :pin "2dc2457650d56d7b495a3db1926acdfb57ad430a")
(package! screenshot              :pin "2770c0cfefe1cc09d55585f4f2f336a1b26e610e" :recipe (:host github :repo "tecosaur/screenshot"))
(package! standard-themes         :pin "8f9b5c6ff8b697d822655431cfcfc034eaea5132" :recipe (:host github :repo "protesilaos/standard-themes"))
(package! svg-tag-mode            :pin "91179d9576850312b00583910a55c798ba615382" :recipe (:host github :repo "rougier/svg-tag-mode"))
(package! system-install.el       :pin "df93485695659c7b827d64270ba70d096682b33d" :recipe (:host github :repo "andrewpeck/system-install.el"))
(package! systemd                 :pin "8742607120fbc440821acbc351fda1e8e68a8806" :recipe (:host github :repo "holomorph/systemd-mode"))
(package! toc-org                 :pin "6d3ae0fc47ce79b1ea06cabe21a3c596395409cd")
(package! treesit-auto            :pin "016bd286a1ba4628f833a626f8b9d497882ecdf3" :recipe (:host github :repo "renzmann/treesit-auto"))
(package! verilog-mode            :pin "85d8429b6b662760b252f8ffbfe21683816032fb" :recipe (:host github :repo "veripool/verilog-mode"))
(package! verilog-port-copy       :pin "a103f8e2209dde19d26d2151f35fe8e53ab3d336" :recipe (:host github :repo "andrewpeck/verilog-port-copy"))
(package! vimrc-mode              :pin "13bc150a870d5d4a95f1111e4740e2b22813c30e")
(package! visual-fill-column      :pin "e04d3521b6dc2435de4c4a4b9cac5feb194f0d5b")
(package! vivado_mode             :pin "d0192c2919855bbc96f269f75b49a5a95242eeb1" :recipe (:host github :repo "andrewpeck/vivado_mode"))
(package! wavedrom-mode           :pin "1c78db33dba69166bea74cf20ea873a9d6b83089" :recipe (:host github :repo "gmlarumbe/wavedrom-mode"))
(package! xr                      :pin "a7adeb24d807f86abbe67a0e90b48d433c282ddd")                           ; xr is rx in reverse
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
