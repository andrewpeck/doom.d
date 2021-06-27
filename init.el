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

;; https://www.reddit.com/r/emacs/comments/kqb9s9/cannot_recompile_packagess_error_wrong_number_of/
;;
(defun make-obsolete (obsolete-name current-name &optional when)
    "Make the byte-compiler warn that function OBSOLETE-NAME is obsolete.
    OBSOLETE-NAME should be a function name or macro name (a symbol).

    The warning will say that CURRENT-NAME should be used instead.
    If CURRENT-NAME is a string, that is the `use instead' message
    \(it should end with a period, and not start with a capital).
    WHEN should be a string indicating when the function
    was first made obsolete, for example a date or a release number."
  (declare (advertised-calling-convention
            ;; New code should always provide the `when' argument.
            (obsolete-name current-name when) "23.1"))
  (put obsolete-name 'byte-obsolete-info
       ;; The second entry used to hold the `byte-compile' handler, but
       ;; is not used any more nowadays.
       (purecopy (list current-name nil when)))
  obsolete-name)

(defmacro define-obsolete-function-alias (obsolete-name current-name
                                                        &optional when docstring)
  "Set OBSOLETE-NAME's function definition to CURRENT-NAME and mark it obsolete.

    \(define-obsolete-function-alias \\='old-fun \\='new-fun \"22.1\" \"old-fun's doc.\")

    is equivalent to the following two lines of code:

    \(defalias \\='old-fun \\='new-fun \"old-fun's doc.\")
    \(make-obsolete \\='old-fun \\='new-fun \"22.1\")

    WHEN should be a string indicating when the function was first
    made obsolete, for example a date or a release number.

    See the docstrings of `defalias' and `make-obsolete' for more details."
  (declare (doc-string 4)
           (advertised-calling-convention
            ;; New code should always provide the `when' argument.
            (obsolete-name current-name when &optional docstring) "23.1"))
  `(progn
     (defalias ,obsolete-name ,current-name ,docstring)
     (make-obsolete ,obsolete-name ,current-name ,when)))

(defun make-obsolete-variable (obsolete-name current-name &optional when access-type)
    "Make the byte-compiler warn that OBSOLETE-NAME is obsolete.
    The warning will say that CURRENT-NAME should be used instead.
    If CURRENT-NAME is a string, that is the `use instead' message.
    WHEN should be a string indicating when the variable
    was first made obsolete, for example a date or a release number.
    ACCESS-TYPE if non-nil should specify the kind of access that will trigger
    obsolescence warnings; it can be either `get' or `set'."
  (declare (advertised-calling-convention
            ;; New code should always provide the `when' argument.
            (obsolete-name current-name when &optional access-type) "23.1"))
  (put obsolete-name 'byte-obsolete-variable
       (purecopy (list current-name access-type when)))
  obsolete-name)

(defmacro define-obsolete-variable-alias (obsolete-name current-name
                                                        &optional when docstring)
    "Make OBSOLETE-NAME a variable alias for CURRENT-NAME and mark it obsolete.
    This uses `defvaralias' and `make-obsolete-variable' (which see).
    See the Info node `(elisp)Variable Aliases' for more details.

    If CURRENT-NAME is a defcustom or a defvar (more generally, any variable
    where OBSOLETE-NAME may be set, e.g. in an init file, before the
    alias is defined), then the define-obsolete-variable-alias
    statement should be evaluated before the defcustom, if user
    customizations are to be respected.  The simplest way to achieve
    this is to place the alias statement before the defcustom (this
    is not necessary for aliases that are autoloaded, or in files
    dumped with Emacs).  This is so that any user customizations are
    applied before the defcustom tries to initialize the
    variable (this is due to the way `defvaralias' works).

    WHEN should be a string indicating when the variable was first
    made obsolete, for example a date or a release number.

    For the benefit of Customize, if OBSOLETE-NAME has
    any of the following properties, they are copied to
    CURRENT-NAME, if it does not already have them:
    `saved-value', `saved-variable-comment'."
  (declare (doc-string 4)
           (advertised-calling-convention
            ;; New code should always provide the `when' argument.
            (obsolete-name current-name when &optional docstring) "23.1"))
  `(progn
     (defvaralias ,obsolete-name ,current-name ,docstring)
     ;; See Bug#4706.
     (dolist (prop '(saved-value saved-variable-comment))
       (and (get ,obsolete-name prop)
            (null (get ,current-name prop))
            (put ,current-name prop (get ,obsolete-name prop))))
     (make-obsolete-variable ,obsolete-name ,current-name ,when)))

(setq comp-deferred-compilation t)
(setq evil-want-C-i-jump t)

(doom!
    :completion
        (company            ; the ultimate code completion backend
        +childframe)
        ;;helm              ; the *other* search engine for love and life
        ;;ido               ; the other *other* search engine...
        (ivy +fuzzy)       ; a search engine for love and life

    :ui
        ;;deft           ; notational velocity for Emacs
        doom             ; what makes DOOM look the way it does
        doom-dashboard   ; a nifty splash screen for Emacs
        ;;doom-quit      ; DOOM quit-message prompts when you quit Emacs
        hl-todo          ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
        ;;hydra
        indent-guides    ; highlighted indent columns
        modeline         ; snazzy, Atom-inspired modeline, plus API
        nav-flash        ; blink the current line after jumping
        ;;neotree        ; a project drawer, like NERDTree for vim
        ophints          ; highlight the region an operation acts on
        (popup           ; tame sudden yet inevitable temporary windows
        ;;+all           ; catch all popups that start with an asterix
        +defaults)       ; default popup rules
        ;;pretty-code    ; replace bits of code with pretty symbols
        tabs             ; an tab bar for Emacs
        ;;treemacs       ; a project drawer, like neotree but cooler
        ;;unicode        ; extended unicode support for various languages
        vc-gutter        ; vcs diff in the fringe
        vi-tilde-fringe  ; fringe tildes to mark beyond EOB
        ;;window-select  ; visually switch windows
        workspaces       ; tab emulation, persistence & separate workspaces
        ;;zen            ; distraction-free coding or writing

    :editor
        (evil +everywhere)  ; come to the dark side, we have cookies
        file-templates      ; auto-snippets for empty files
        fold                ; (nigh) universal code folding
        format              ; automated prettiness
        ;;multiple-cursors  ; editing in many places at once
        ;;objed             ; text object editing for the innocent
        ;;parinfer          ; turn lisp into python, sort of
        ;;rotate-text       ; cycle region at point between text candidates
        snippets            ; my elves. They type so I don't have to
        word-wrap           ; soft wrapping with language-aware indent

    :emacs
        dired             ; making dired pretty [functional]
        electric          ; smarter, keyword-based electric-indent
        ibuffer           ; interactive buffer management
        vc                ; version-control and Emacs, sitting in a tree
        undo              ; persistent, smarter undo for your inevitable mistakes

    :term
        ;;eshell          ; a consistent, cross-platform shell (WIP)
        shell           ; a terminal REPL for Emacs
        ;;term            ; terminals in Emacs
        ;;vterm             ; another terminals in Emacs

    :checkers
        syntax             ; tasing you for every semicolon you forget
        (spell +flyspell)  ; tasing you for misspelling mispelling
        ;;grammar            ; tasing grammar mistake every you make

    :tools
        ;;ansible
        ;;debugger          ; FIXME stepping through code, to help you add bugs
        ;;direnv
        ;;docker
        ;;editorconfig      ; let someone else argue about tabs vs spaces
        ;;ein               ; tame Jupyter notebooks with emacs
        (eval +overlay)     ; run code, run (also, repls)
        ;;gist              ; interacting with github gists
        (lookup             ; helps you navigate your code and documentation
        +docsets)           ; ...or in Dash docsets locally
        lsp
        (magit +forge)      ; a git porcelain for Emacs
        make                ; run make tasks from Emacs
        ;;pass              ; password manager for nerds
        pdf                 ; pdf enhancements
        ;;prodigy           ; FIXME managing external services & code builders
        rgb                 ; creating color strings
        ;;terraform         ; infrastructure as code
        ;;tmux              ; an API for interacting with tmux
        ;;upload            ; map local to remote projects via ssh/ftp

    :os
        (:if IS-MAC macos)  ; improve compatibility with macOS
        ;;tty               ; improve the terminal Emacs experience

    :lang
        ;;agda               ; types of types of types of types...
        ;;assembly           ; assembly for fun or debugging
        (cc +lsp)            ; C/C++/Obj-C madness
        clojure              ; java with a lisp
        json
        common-lisp          ; if you've seen one lisp, you've seen them all
        ;;coq                ; proofs-as-programs
        ;;crystal            ; ruby at the speed of c
        ;;csharp             ; unity, .NET, and mono shenanigans
        data                 ; config/data formats
        ;;elixir             ; erlang done right
        ;;elm                ; care for a cup of TEA?
        (emacs-lisp  +lsp)   ; drown in parentheses
        ;;erlang             ; an elegant language for a more civilized age
        ;;ess                ; emacs speaks statistics
        ;;faust              ; dsp, but you get to keep your soul
        ;;fsharp             ; ML stands for Microsoft's Language
        ;;fstar              ; (dependent) types and (monadic) effects and Z3
        ;;go                 ; the hipster dialect
        (haskell +dante)     ; a language that's lazier than I am
        ;;hy                 ; readability of scheme w/ speed of python
        ;;idris              ;
        ;;(java +meghanada)  ; the poster child for carpal tunnel syndrome
        ;;javascript         ; all(hope(abandon(ye(who(enter(here))))))
        (julia + lsp)        ; a better, faster MATLAB
        ;;kotlin             ; a better, slicker Java(Script)
        (latex             ; writing papers in Emacs has never been so fun
         +fold
         +lsp)
        ;;lean
        ;;factor
        ;;ledger             ; an accounting system in Emacs
        ;;lua                ; one-based indices? one-based indices
        markdown             ; writing docs for people to ignore
        nim                  ; python + lisp at the speed of c
        ;;nix                ; I hereby declare "nix geht mehr!"
        ;;ocaml              ; an objective camel
        (org                 ; organize your plain life in plain text
            ;;+roam
            +gnuplot
          ;;+pretty         ; Enables pretty unicode symbols for bullets and priorities
            +dragndrop      ; drag & drop files/images into org buffers
            +pandoc         ; export-with-pandoc support
            ;;+hugo         ; use Emacs for hugo blogging
            ;;+jupyter      ; ipython/jupyter support for babel
            ;;+pomodoro     ; be fruitful with the tomato technique
            +present        ; using org-mode for presentations
        )
        ;;perl              ; write code no one else can comprehend
        ;;php               ; perl's insecure younger brother
        ;;plantuml          ; diagrams for confusing people more
        ;;purescript        ; javascript, but functional
        (python +lsp)       ; beautiful is better than ugly
        ;;qt                ; the 'cutest' gui framework ever
        ;;racket            ; a DSL for DSLs
        ;;rest              ; Emacs as a REST client
        ;;rst               ; ReST in peace
        ;;ruby              ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
        ;;rust              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
        ;;scala             ; java, but good
        ;;scheme            ; a fully conniving family of lisps
        (sh +lsp)           ; she sells {ba,z,fi}sh shells on the C xor
        ;;solidity          ; do you need a blockchain? No.
        ;;swift             ; who asked for emoji variables?
        ;;terra             ; Earth and Moon in alignment for performance.
        ;;web               ; the tubes

    ;;email
        ;;(mu4e +gmail)
        ;;notmuch
        ;;(wanderlust +gmail)

    :app
        ;;calendar
        ;;emms              ; This module enables Emacs to be used as a music player
        ;;everywhere        ; *leave* Emacs!? You must be joking
        ;;irc               ; how neckbeards socialize
        ;;(rss +org)        ; emacs as an RSS reader
        ;;twitter           ; twitter client https://twitter.com/vnought

    :config
        ;;literate
        (default +bindings +smartparens)
)
