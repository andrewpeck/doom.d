;; -*- lexical-binding: t; -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-safe-themes
   '("75b8719c741c6d7afa290e0bb394d809f0cc62045b93e1d66cd646907f8e6d43" "8f5a7a9a3c510ef9cbb88e600c0b4c53cdcdb502cfe3eb50040b7e13c6f4e78e" "d0731d8e34d33aac6ec6fe83b78cf42b4e0cac030e6b382e9f67e6336b8701fa" "3cc32a1cdba9485fa0787274749dcc5f5c04bb1cc684587d274be1f8df0d4026" "951eea60c02a173a7a5f374946522d2cdb3ad17c5fbe96334db5436108d727de" "0c3b1358ea01895e56d1c0193f72559449462e5952bded28c81a8e09b53f103f" "9efb2d10bfb38fe7cd4586afb3e644d082cbcdb7435f3d1e8dd9413cbe5e61fc" "6b80b5b0762a814c62ce858e9d72745a05dd5fc66f821a1c5023b4f2a76bc910" "01cf34eca93938925143f402c2e6141f03abb341f27d1c2dba3d50af9357ce70" "711efe8b1233f2cf52f338fd7f15ce11c836d0b6240a18fffffc2cbd5bfe61b0" "990e24b406787568c592db2b853aa65ecc2dcd08146c0d22293259d400174e37" "2cdc13ef8c76a22daa0f46370011f54e79bae00d5736340a5ddfe656a767fddf" "74ba9ed7161a26bfe04580279b8cad163c00b802f54c574bfa5d924b99daa4b9" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "3c2f28c6ba2ad7373ea4c43f28fcf2eed14818ec9f0659b1c97d4e89c99e091e" "bf387180109d222aee6bb089db48ed38403a1e330c9ec69fe1f52460a8936b66" "79278310dd6cacf2d2f491063c4ab8b129fee2a498e4c25912ddaa6c3c5b621e" "d6603a129c32b716b3d3541fc0b6bfe83d0e07f1954ee64517aa62c9405a3441" "e6ff132edb1bfa0645e2ba032c44ce94a3bd3c15e3929cdf6c049802cf059a2a" "4bca89c1004e24981c840d3a32755bf859a6910c65b829d9441814000cf6c3d0" "8d7684de9abb5a770fbfd72a14506d6b4add9a7d30942c6285f020d41d76e0fa" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "cf3d5d77679f7daed6a2c863e4f2e30427d5e375b254252127be9359957502ec" default))
 '(debug-on-error t)
 '(elfeed-feeds '("https://hackaday.com/blog/feed/") t)
 '(hl-sexp-background-color "#efebe9")
 '(org-agenda-files
   '("~/Dropbox/notes/gaps_timesheet.org" "/home/andy/Dropbox/org/todo.org" "/home/andy/Dropbox/org/gcal-hazen.org" "/home/andy/Dropbox/org/gcal-medical.org" "/home/andy/Dropbox/org/gcal-peck.org" "/home/andy/Dropbox/org/gcal-work.org"))
 '(package-selected-packages '(org-plus-contrib))
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook
           (lambda nil
             (progn
               (sort-all-org-entries)
               (save-buffer)))
           nil 'local)
     (eval add-hook 'after-save-hook
           (lambda nil
             (shell-command
              (format "pandoc %s.org -o %s.md -t gfm"
                      (file-name-base)
                      (file-name-base))))
           nil 'local)
     (eval add-hook 'after-save-hook
           (lambda nil
             (shell-command
              (format "pandoc ~A.org -o ~A.md -t gfm"
                      (file-name-base)
                      (file-name-base))))
           nil 'local)
     (eval add-hook 'after-save-hook
           (lambda nil
             (shell-command
              (format "pandoc ~A.org -o ~A.md -t gfm")
              (file-name-base)
              (file-name-base)))
           nil 'local)
     (eval add-hook 'after-save-hook
           (lambda nil
             (shell-command "pandoc csm-interface.org -o csm-interface.md -t gfm"))
           nil 'local)
     (eval make-variable-buffer-local 'after-save-hook)))
 '(warning-suppress-types '((comp) (comp) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
