;;; init.el -*- lexical-binding: t; no-byte-compile: t; -*-

(defmacro comment (&rest _) nil)

;; Start emacs in full screen by default
(add-to-list 'default-frame-alist
             '(fullscreen . maximized))

(add-to-list 'default-frame-alist
             '(background-color . "#17191a"))

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist
             '(inhibit-double-buffering . nil))

(menu-bar-mode 0)                   ; Turn off menu bar

(setq doom-cache-dir (concat doom-user-dir ".cache/"))

(doom!

 :completion

 ;; (corfu +icons +orderless +dbbrev)
 (vertico +icons)     ; the search engine of the future

 :ui

 doom                ; what makes DOOM look the way it does
 doom-dashboard      ;
 hl-todo             ; highlight TODO / FIXME/NOTE/DEPRECATED/HACK/REVIEW
 nav-flash           ; blink the current line after jumping
 ophints             ; highlight the region an operation acts on
 (popup              ; tame sudden yet inevitable temporary windows
  +all               ; catch all popups that start with an asterix
  +defaults)         ; default popup rules
 (vc-gutter +pretty) ; vcs diff in the fringe
 ;; workspaces          ; tab emulation, persistence & separate workspaces

 ;;emoji              ;
 ;;indent-guides      ; highlighted indent columns
 ;;vi-tilde-fringe    ; fringe tildes to mark beyond EOB
 ;;window-select      ; visually switch windows
 ;;pretty-code        ; replace bits of code with pretty symbols
 ;;tabs               ; an tab bar for Emacs
 ;;treemacs             ; a project drawer, like neotree but cooler
 ;;unicode            ; extended unicode support for various languages
 ;;zen                ; distraction-free coding or writing

 :editor

 whitespace
 lispy
 (evil +everywhere)  ; come to the dark side, we have cookies
 fold                ; (nigh) universal code folding
 ;; snippets            ; my elves. They type so I don't have to
 ;; word-wrap           ; soft wrapping with language-aware indent

 ;;file-templates    ; auto-snippets for empty files
 ;; (format)            ; automated prettiness
 ;;multiple-cursors  ; editing in many places at once
 ;;objed             ; text object editing for the innocent
 ;;parinfer          ; turn lisp into python, sort of
 ;;rotate-text       ; cycle region at point between text candidates

 :emacs

 tramp
 ibuffer           ; interactive buffer management
 vc                ; version-control and Emacs, sitting in a tree
 (undo)            ; persistent, smarter undo for your inevitable mistakes

 ;; (dired +dirvish +icons) ; making dired pretty [functional]

 ;; electric       ; smarter, keyword-based electric-indent

 :term

 vterm             ; another terminals in Emacs

 ;;eshell          ; a consistent, cross-platform shell (WIP)
 ;;shell           ; a terminal REPL for Emacs
 ;;term            ; terminals in Emacs

 :checkers

 syntax               ; tasing you for every semicolon you forget
 ;;(spell +flyspell)  ; tasing you for misspelling mispelling
 ;;grammar            ; tasing grammar mistake every you make

 :tools

 llm
 biblio                                 ; Writes a PhD for you (citation needed)
 (docker +lsp +tree-sitter)
 (eval +overlay)                        ; run code, run (also, repls)
 (lookup +dictionary +docsets +offline) ; helps you navigate your code and documentation
 (lsp +eglot)                           ;
 (magit +forge)                         ; a git porcelain for Emacs
 make                                   ; run make tasks from Emacs
 pdf                                    ; pdf enhancements
 ;; tmux                                   ; an API for interacting with tmux

 ;;debugger          ; FIXME stepping through code, to help you add bugs
 ;; ein
 tree-sitter
 ;;pass              ; password manager for nerds
 ;;upload            ; map local to remote projects via ssh/ftp

 :os

 (:if IS-MAC macos)  ; improve compatibility with macOS
 tty               ; improve the terminal Emacs experience

 :lang

 common-lisp
 (cc +lsp +tree-sitter)              ; C/C++/Obj-C madness
 (clojure +lsp +tree-sitter)         ; java with a lisp
 (json +tree-sitter)                 ; json, uhg
 (latex +fold +lsp +tree-sitter)     ; writing papers in Emacs has never been so fun
 (markdown +grip +tree-sitter)       ; writing docs for people to ignore
 ;; data                             ; A dumping ground for data formats (csv, xml)
 emacs-lisp                          ; drown in parentheses
 (php +tree-sitter +lsp)             ; perl's insecure younger brother
 (python +pyright +lsp +tree-sitter) ; beautiful is better than ugly
 (rust +lsp +tree-sitter)            ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
 (sh +fish +tree-sitter)             ; she sells {ba,z,fi}sh shells on the C xor
 (web +tree-sitter)                  ; the tubes
 (yaml +tree-sitter)                 ; JSON, but readable

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

 ;;assembly                      ; assembly for fun or debugging
 ;;common-lisp                   ; if you've seen one lisp, you've seen them all
 ;;coq                           ; proofs-as-programs
 ;;crystal                       ; ruby at the speed of c
 ;;csharp                        ; unity, .NET, and mono shenanigans
 ;;elixir                        ; erlang done right
 ;;elm                           ; care for a cup of TEA?
 ;;erlang                        ; an elegant language for a more civilized age
 ;;ess                           ; emacs speaks statistics
 ;;faust                         ; dsp, but you get to keep your soul
 ;;fsharp                        ; ML stands for Microsoft's Language
 ;;fstar                         ; (dependent) types and (monadic) effects and Z3
 ;;(go +lsp)                     ; the hipster dialect
 ;;(haskell +lsp)                ; a language that's lazier than I am
 ;;hy                            ; readability of scheme w/ speed of python
 ;;idris                         ; A language you can depend on
 ;;(java +meghanada)             ; the poster child for carpal tunnel syndrome
 (javascript +lsp +tree-sitter)  ; all(hope(abandon(ye(who(enter(here))))))
 ;;(julia +lsp)                  ; a better, faster MATLAB
 ;;lean                          ; For folks with too much to prove
 ;;factor                        ;
 ;;ledger                        ; an accounting system in Emacs
 (lua +fennel +lsp +tree-sitter) ; one-based indices? one-based indices
 ;;nim                           ; python + lisp at the speed of c
 (nix +lsp +tree-sitter)         ; I hereby declare "nix geht mehr!"
 ;;ocaml                         ; an objective camel
 ;;plantuml                      ; diagrams for confusing people more
 ;;purescript                    ; javascript, but functional
 ;;(racket +lsp +xp)             ; a DSL for DSLs
 ;;scheme                        ; a fully conniving family of lisps

 :email

 (mu4e +gmail +org)

 :app

 everywhere    ; *leave* Emacs!? You must be joking
 rss           ; emacs as an RSS reader
 ;;calendar
 ;;emms        ; This module enables Emacs to be used as a music player
 ;;irc         ; how neckbeards socialize
 ;; (rss +org) ; emacs as an RSS reader

 :config

 (default +bindings +smartparens))

;; start emacs with `emacs --debug-init`
;; load with `benchmark-init/show-durations-tree'
(when init-file-debug
  (load (concat doom-emacs-dir ".local/straight/repos/benchmark-init-el/benchmark-init"))
  (require 'benchmark-init)
  (add-hook 'doom-first-input-hook #'benchmark-init/deactivate))
