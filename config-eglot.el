;;------------------------------------------------------------------------------
;; Eglot
;;------------------------------------------------------------------------------

(after! eglot

  (dolist (provider '(:hoverProvider :documentHighlightProvider))
    (add-to-list 'eglot-ignored-server-capabilities provider))

  (add-to-list 'eglot-server-programs
               '(vhdl-mode . ("ghdl-ls"))))
