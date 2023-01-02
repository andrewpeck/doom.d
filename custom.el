;; -*- mode: emacs-lisp-mode -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3d7d25d60e3b3bb6b76921a00715ca77014c8dd2584dce147965937ca96e2da1" "28a34dd458a554d34de989e251dc965e3dc72bace7d096cdc29249d60f395a82" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "636b135e4b7c86ac41375da39ade929e2bd6439de8901f53f88fde7dd5ac3561" "60ada0ff6b91687f1a04cc17ad04119e59a7542644c7c59fc135909499400ab8" "443e2c3c4dd44510f0ea8247b438e834188dc1c6fb80785d83ad3628eadf9294" "dd8b509fd5eeeba4bca555e42899dacc90f37b818676b40305b6c8c718ee16d9" "2721b06afaf1769ef63f942bf3e977f208f517b187f2526f0e57c1bd4a000350" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "5784d048e5a985627520beb8a101561b502a191b52fa401139f4dd20acb07607" "e2c926ced58e48afc87f4415af9b7f7b58e62ec792659fcb626e8cba674d2065" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "c4063322b5011829f7fdd7509979b5823e8eea2abf1fe5572ec4b7af1dd78519" "f91395598d4cb3e2ae6a2db8527ceb83fed79dbaf007f435de3e91e5bda485fb" "745d03d647c4b118f671c49214420639cb3af7152e81f132478ed1c649d4597d" "4f1d2476c290eaa5d9ab9d13b60f2c0f1c8fa7703596fa91b235db7f99a9441b" "246a9596178bb806c5f41e5b571546bb6e0f4bd41a9da0df5dfbca7ec6e2250c" "613aedadd3b9e2554f39afe760708fc3285bf594f6447822dd29f947f0775d6c" "333958c446e920f5c350c4b4016908c130c3b46d590af91e1e7e2a0611f1e8c5" "dac1ac5702660acdb024e894e6185575b937b15b63ef569541fece79554218ef" "748a4497168680eb25b5e9d5bd4ba4c7042a16074c8857b559e2f0b655e0fb4a" "59a1f5f15954181cb29ebc4332825dd95925db9a01f056f051253a25ebda8dff" "df8909067680a63af002aafc380f03d9ce6afe352a329b8ba343393e96ba5d00" "39dd7106e6387e0c45dfce8ed44351078f6acd29a345d8b22e7b8e54ac25bac4" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" default))
 '(safe-local-variable-values
   '((hog-vivado-path . "/storage/Xilinx/Vivado/2020.1")
     (eval remove-hook 'write-contents-functions #'re-indent-buffer)
     (eval add-hook 'write-contents-functions #'re-indent-buffer nil t)
     (eval when
      (functionp '+word-wrap-mode)
      (+word-wrap-mode 0))
     (eval add-hook 'write-file-functions
      (lambda nil
        (progn
          (org-sbe "generate_invoices")
          (save-buffer))))
     (eval add-hook 'write-contents-functions
      (lambda nil
        (when
            (fboundp 'sort-elisp-block)
          (sort-elisp-block)))
      nil t)
     (eval auto-fill-mode t)
     (eval add-hook 'write-contents-functions
      (lambda nil
        (org-sbe "generate_invoices")))
     (eval add-hook 'after-save-hook
      (lambda nil
        (shell-command
         (format "pandoc %s.org -o %s.md -t gfm"
                 (file-name-base)
                 (file-name-base))))
      nil 'local)
     (eval add-hook 'write-contents-functions
      (lambda nil
        (when
            (boundp 'sort-elisp-block)
          (sort-elisp-block)))
      nil t)
     (hog-vivado-path . "/storage/Xilinx/Vivado/2020.1/settings64.sh")
     (hog-vivado-path "/storage/Xilinx/Vivado/2020.1/settings64.sh")
     (hog-vivado-path "/storage/Xilinx/Vivado/2021.1/settings64.sh")
     (eval add-hook 'write-contents-functions
      (lambda nil
        (and nil
             (org-sbe "write_contents_hooks")))
      nil t)
     (eval add-hook 'write-contents-functions
      (lambda nil
        (org-sbe "write_contents_hooks"))
      nil t)
     (eval add-hook 'write-contents-functions
      (lambda nil
        (org-sbe write_contents_hooks))
      nil t)
     (eval add-hook 'write-contents-functions
      (lambda nil
        (org-sbe update_all_histograms))
      nil t)
     (eval add-hook 'after-save-hook #'copy-html-to-ohm nil 'local)
     (eval add-hook 'write-contents-functions 'sort-elisp-block nil t)
     (eval add-hook 'after-save-hook
      (lambda nil
        (start-process "copy-to-ohm" nil "scp" "index.html" "ohm:~/public_html/notes/index.php")))
     (eval add-hook 'after-save-hook
      (lambda nil
        (shell-command
         (concat "scp "
                 (format "%s.css"
                         (file-name-base))
                 " ohm:~/public_html/notes/")))
      nil 'local)
     (eval add-hook 'after-save-hook
      (lambda nil
        (shell-command
         (concat "scp "
                 (format "%s.html"
                         (file-name-base))
                 " ohm:~/public_html/notes/")))
      nil 'local)
     (eval add-hook 'write-contents-functions
      (lambda nil
        (save-excursion
          (progn
            (org-babel-goto-named-src-block "sorting")
            (org-babel-execute-src-block))))
      nil t)
     (eval load-file
      (concat
       (file-name-directory
        (buffer-file-name))
       "org-setup.el"))
     (eval add-hook 'after-save-hook
      (lambda nil
        (progn
          (unpackaged/org-export-html-with-useful-ids-mode)
          (org-html-export-to-html)))
      nil 'local)
     (eval add-hook 'after-save-hook
      (lambda nil
        (shell-command
         (concat "rsync" " -av "
                 (format "%s.html"
                         (file-name-base))
                 " ohm:~/public_html/notes/"))))
     (eval add-hook 'after-save-hook
      (lambda nil
        (start-process "copy-to-ohm" nil "scp"
                       (format "%s.html"
                               (file-name-base))
                       "ohm:~/public_html/notes/"))
      nil 'local)
     (eval add-hook 'write-contents-functions
      (lambda nil
        (when
            (boundp 'sort-elisp-block)
          sort-elisp-block))
      nil t)
     (eval make-variable-buffer-local 'write-contents-functions)
     (eval add-hook 'after-save-hook
      (lambda nil
        (mapcar
         (lambda
           (file)
           (start-process "copy-to-ohm" nil "rsync" "-av" file "ohm:~/public_html/notes/"))
         (file-expand-wildcards "*.svg"))
        nil 'local))
     (eval add-hook 'after-save-hook
      (lambda nil
        (start-process "copy-to-ohm" nil "scp"
                       (format "%s.html"
                               (file-name-base))
                       "ohm:~/public_html/notes/")))
     (eval add-hook 'after-save-hook
      (lambda nil
        (start-process "copy-to-ohm" nil "rsync" "-av"
                       (format "%s.html"
                               (file-name-base))
                       "ohm:~/public_html/notes/")))
     (eval add-hook 'after-save-hook #'org-html-export-to-html nil 'local)
     (eval load-file
      (concat
       (file-name-directory
        (buffer-file-name))
       "latex-class.el"))
     (eval progn
      (find-file "spec-header.org")
      (org-babel-goto-named-src-block "startup")
      (org-babel-execute-src-block))
     (eval add-hook 'after-save-hook
      (lambda nil
        (org-export-to-file 'latex
            (concat
             (file-name-base)
             ".tex")))
      nil 'local)
     (eval add-hook 'after-save-hook
      (lambda nil
        (org-export-to-file 'md
            (concat
             (file-name-base)
             ".md")))
      nil 'local)
     (eval make-variable-buffer-local 'after-save-hook)
     (eval if
      (functionp '+word-wrap-mode)
      (+word-wrap-mode 0))))
 '(warning-suppress-types '((lsp-on-idle-hook) (iedit))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ts-fold-replacement-face ((t (:foreground nil :box nil :inherit font-lock-comment-face :weight light))))
 '(writegood-weasels-face ((t (:underline (:color "#888888" :style wave))))))

(put 'narrow-to-region 'disabled nil)
