;;; ../Sync/dotfiles/doom.d/config-eglot.el -*- lexical-binding: t; -*-


(after! eglot

  (dolist (provider '(:hoverProvider :documentHighlightProvider))
    (add-to-list 'eglot-ignored-server-capabilities provider)))
