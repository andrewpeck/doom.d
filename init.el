;;; init.el -*- lexical-binding: t; tab-width: 4; -*-

;; This file controls what Doom modules are enabled and what order they load in.
;; Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find information about all of Doom's modules
;;      and what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c g k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c g d') on a module to browse its
;;      directory (for easy access to its source code).

(setenv "LSP_USE_PLISTS" "true")

;; Start emacs in full screen by default
(add-to-list 'default-frame-alist
             '(fullscreen . maximized))

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist
             '(inhibit-double-buffering . t))

(menu-bar-mode 0)                   ; Turn off menu bar

(doom!

 :completion

 (corfu +icons +orderless +dbbrev)
 (vertico +icons)     ; the search engine of the future

 :ui

 doom                 ; what makes DOOM look the way it does
 doom-dashboard
 ;;emoji              ;
 hl-todo              ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
 ;;indent-guides      ; highlighted indent columns
 nav-flash            ; blink the current line after jumping
 hydra                ;
 ophints              ; highlight the region an operation acts on
 (popup               ; tame sudden yet inevitable temporary windows
  +all                ; catch all popups that start with an asterix
  +defaults)          ; default popup rules
 ;;pretty-code        ; replace bits of code with pretty symbols
 ;;tabs               ; an tab bar for Emacs
 ;;treemacs             ; a project drawer, like neotree but cooler
 ;;unicode            ; extended unicode support for various languages
 (vc-gutter +pretty
            +diff-hl) ; vcs diff in the fringe
 ;;vi-tilde-fringe    ; fringe tildes to mark beyond EOB
 ;;window-select      ; visually switch windows
 workspaces           ; tab emulation, persistence & separate workspaces
 ;;zen                ; distraction-free coding or writing

 :editor

 lispy
 (evil +everywhere)  ; come to the dark side, we have cookies
 ;;file-templates    ; auto-snippets for empty files
 fold                ; (nigh) universal code folding
 ;; (format)            ; automated prettiness
 ;;multiple-cursors  ; editing in many places at once
 ;;objed             ; text object editing for the innocent
 ;;parinfer          ; turn lisp into python, sort of
 ;;rotate-text       ; cycle region at point between text candidates
 snippets            ; my elves. They type so I don't have to
 word-wrap           ; soft wrapping with language-aware indent

 :emacs

 ;; dired          ; making dired pretty [functional]
 ;; electric       ; smarter, keyword-based electric-indent
 ibuffer           ; interactive buffer management
 vc                ; version-control and Emacs, sitting in a tree
 (undo)            ; persistent, smarter undo for your inevitable mistakes

 :term

 ;;eshell          ; a consistent, cross-platform shell (WIP)
 ;;shell           ; a terminal REPL for Emacs
 ;;term            ; terminals in Emacs
 vterm             ; another terminals in Emacs

 :checkers

 syntax               ; tasing you for every semicolon you forget
 ;;(spell +flyspell)  ; tasing you for misspelling mispelling
 ;;grammar            ; tasing grammar mistake every you make

 :tools

 ;;debugger          ; FIXME stepping through code, to help you add bugs
 (docker +lsp)
 ;; ein
 ;; tree-sitter
 (eval +overlay)     ; run code, run (also, repls)

 ;; helps you navigate your code and documentation
 (lookup +dictionary +docsets)

 (lsp)
 (magit +forge)      ; a git porcelain for Emacs
 make                ; run make tasks from Emacs
 pdf                 ; pdf enhancements
 tmux                ; an API for interacting with tmux

 ;;pass              ; password manager for nerds
 ;;upload            ; map local to remote projects via ssh/ftp

 :os

 (:if IS-MAC macos)  ; improve compatibility with macOS
 tty               ; improve the terminal Emacs experience

 :lang

 ;;assembly           ; assembly for fun or debugging

 ;; C/C++/Obj-C madness
 (cc +lsp ;; +tree-sitter
     )

 ;; java with a lisp
 (clojure +lsp ;; +tree-sitter
          )

 ;; json, uhg
 (json +lsp ;; +tree-sitter
       )

 ;; if you've seen one lisp, you've seen them all
 ;; common-lisp

 ;;coq                ; proofs-as-programs
 ;;crystal            ; ruby at the speed of c
 ;;csharp             ; unity, .NET, and mono shenanigans

 ;; A dumping ground for data formats
 data                 ; config/data formats (csv + xml)

 ;;elixir             ; erlang done right
 ;;elm                ; care for a cup of TEA?

 emacs-lisp           ; drown in parentheses

 ;;erlang             ; an elegant language for a more civilized age
 ;;ess                ; emacs speaks statistics
 ;;faust              ; dsp, but you get to keep your soul
 ;;fsharp             ; ML stands for Microsoft's Language
 ;;fstar              ; (dependent) types and (monadic) effects and Z3

 ;; the hipster dialect
 ;;(go +lsp +tree-sitter)

 ;; a language that's lazier than I am
 ;; (haskell +lsp +tree-sitter)

 ;;hy                 ; readability of scheme w/ speed of python
 ;;idris              ; A language you can depend on
 ;;(java +meghanada)  ; the poster child for carpal tunnel syndrome
 ;;javascript         ; all(hope(abandon(ye(who(enter(here))))))

 ;; a better, faster MATLAB
 ;; (julia +lsp +tree-sitter)

 ;; writing papers in Emacs has never been so fun
 (latex +fold +lsp +latexmk)

 ;;lean               ; For folks with too much to prove
 ;;factor             ;
 ;;ledger             ; an accounting system in Emacs
 ;;lua                ; one-based indices? one-based indices
 (markdown +grip)     ; writing docs for people to ignore
 ;;nim                ; python + lisp at the speed of c
 ;;nix                ; I hereby declare "nix geht mehr!"
 ;;ocaml              ; an objective camel

 ;; organize your plain life in plain text
 (org +dragndrop +gnuplot +pandoc
      ;; +brain
      ;; +contacts
      ;;+hugo         ; use Emacs for hugo blogging
      ;;+journal
      ;;+jupyter      ; ipython/jupyter support for babel
      ;;+noter
      ;;+passwords
      ;;+pomodoro     ; be fruitful with the tomato technique
      ;;+present      ; using org-mode for presentations
      ;;+pretty       ; Enables pretty unicode symbols for bullets and priorities
      ;;+roam
      ;;+roam2
      )

 ;; perl's insecure younger brother
 (php +lsp ;; +tree-sitter
      )

 ;;plantuml          ; diagrams for confusing people more
 ;;purescript        ; javascript, but functional

 ;; beautiful is better than ugly
 (python +pyright +lsp +pyenv ;; +tree-sitter
         )

 ;; a DSL for DSLs
 ;; (racket +lsp +xp)

 ;; Fe2O3.unwrap().unwrap().unwrap().unwrap()
 (rust ;; +tree-sitter
  )

 ;; a fully conniving family of lisps
 ;;scheme

 ;; she sells {ba,z,fi}sh shells on the C xor
 (sh +lsp +treesitter +fish)

 ;; the tubes
 (web +lsp ;; +tree-sitter
      )

 ;; JSON, but readable
 (yaml +lsp ;; +tree-sitter
       )

 :email

 (mu4e +gmail +org)

 :app
 everywhere    ; *leave* Emacs!? You must be joking
 (rss)         ; emacs as an RSS reader
 ;;calendar
 ;;emms        ; This module enables Emacs to be used as a music player
 ;;irc         ; how neckbeards socialize
 ;; (rss +org) ; emacs as an RSS reader
 ;;twitter     ; twitter client https://twitter.com/vnought

 :config

 (default +bindings +smartparens))

(when init-file-debug
  (load "~/.emacs.d/.local/straight/repos/benchmark-init-el/benchmark-init")
  (require 'benchmark-init)
  (add-hook 'doom-first-input-hook #'benchmark-init/deactivate))
