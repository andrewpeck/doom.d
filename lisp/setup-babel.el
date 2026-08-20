;;; setup-babel.el --- Transient menu for Org Babel header arguments -*- lexical-binding: t; -*-

;; A two-level transient for the header arguments of a source block:
;;
;;   #+begin_src python :results output :exports both :session foo
;;
;; The first menu lists the header arguments themselves, grouped by what they
;; affect and annotated with a one-line description plus the value currently in
;; effect.  Picking one opens a second menu listing that argument's documented
;; values, again with descriptions, along with `!' to type a value by hand and
;; `-' to remove the argument.
;;
;; `=' cycles where the value is written: the `#+begin_src' line at point, or
;; the enclosing subtree's `header-args' / `header-args:LANG' property.
;;
;; Reference: (info "(org) Using Header Arguments") and the value tables in
;; `org-babel-common-header-args-w-values'.

(require 'transient)
(require 'cl-lib)
(require 'seq)

;;------------------------------------------------------------------------------
;; The header argument table
;;------------------------------------------------------------------------------

;; Each entry is (NAME . PLIST) where PLIST understands:
;;
;;   :key    -- key in the top level menu
;;   :doc    -- one line description, kept short enough to sit in a column
;;   :values -- list of (VALUE DOC . PLIST); the value PLIST understands
;;              :class (column heading, for mutually exclusive value groups),
;;              :label (shown instead of VALUE) and :key (fixed key)
;;   :reader -- (PROMPT INITIAL) function used for `!' custom values
;;
;; An argument with no :values prompts with :reader directly instead of opening
;; a second menu -- there would be nothing to show.

(defconst +org-babel-header-args
  '(("Evaluation"
     (session
      :key "s" :doc "persistent REPL for the block"
      :reader +org-babel--read-string
      :values (("none" "run in a fresh, one-off process")))
     (eval
      :key "e" :doc "when evaluation is allowed"
      :values (("yes"          "evaluate, subject to `org-confirm-babel-evaluate'")
               ("query"        "always ask first")
               ("no"           "never evaluate")
               ("never"        "never evaluate")
               ("no-export"    "evaluate interactively, but not on export")
               ("never-export" "evaluate interactively, but not on export")
               ("query-export" "ask before evaluating during export")))
     (cache
      :key "c" :doc "reuse unchanged results"
      :values (("no"  "always re-run the block (default)")
               ("yes" "skip when body, args and result are unchanged")))
     (dir
      :key "d" :doc "working directory"
      :reader +org-babel--read-directory
      :values (("." "the directory of the Org file")))
     (var
      :key "v" :doc "bind NAME=VALUE for the block"
      :reader +org-babel--read-var)
     (cmdline
      :key "x" :doc "extra interpreter arguments"
      :reader +org-babel--read-string)
     (prologue
      :key "<" :doc "text prepended to the body"
      :reader +org-babel--read-string)
     (epilogue
      :key ">" :doc "text appended to the body"
      :reader +org-babel--read-string))

    ("Results"
     (results
      :key "r" :doc "what to collect, how to insert"
      :values (("output"   "text printed to stdout" :class "Collect")
               ("value"    "value of the last expression" :class "Collect")

               ("file"     "a file name; link to it" :class "Type")
               ("list"     "insert as an Org list" :class "Type")
               ("vector"   "insert as an Org table" :class "Type")
               ("table"    "insert as an Org table" :class "Type")
               ("scalar"   "insert literally, no table" :class "Type")
               ("verbatim" "insert literally, no table" :class "Type")

               ("raw"      "insert as raw Org markup" :class "Format")
               ("html"     "wrap in #+begin_export html" :class "Format")
               ("latex"    "wrap in #+begin_export latex" :class "Format")
               ("org"      "wrap in #+begin_src org" :class "Format")
               ("code"     "wrap in a code block" :class "Format")
               ("pp"       "pretty print, then a code block" :class "Format")
               ("drawer"   "wrap in a :RESULTS: drawer" :class "Format")
               ("link"     "link to :file, do not write it" :class "Format")
               ("graphics" "link to :file, do not write it" :class "Format")

               ("replace"  "replace previous results" :class "Handle")
               ("silent"   "echo only, do not insert" :class "Handle")
               ("none"     "neither insert nor echo" :class "Handle")
               ("discard"  "ignore the results entirely" :class "Handle")
               ("append"   "add below previous results" :class "Handle")
               ("prepend"  "add above previous results" :class "Handle")))
     (wrap
      :key "w" :doc "wrap results in a named block"
      :reader +org-babel--read-string
      :values (("results"         "#+begin_results ... #+end_results")
               ("example"         "#+begin_example ... #+end_example")
               ("quote"           "#+begin_quote ... #+end_quote")
               ("src org"         "#+begin_src org ... #+end_src")
               ("EXPORT html"     "#+begin_export html ... #+end_export")
               ("EXPORT latex"    "#+begin_export latex ... #+end_export")
               ("EXPORT markdown" "#+begin_export markdown ... #+end_export")
               ("no"              "do not wrap the results")))
     (post
      :key "p" :doc "post-process results (*this*)"
      :reader +org-babel--read-string)
     (file
      :key "f" :doc "write results to this file"
      :reader +org-babel--read-file)
     (file-ext
      :key "F" :doc "extension for a generated name"
      :reader +org-babel--read-string
      :values (("png" "") ("svg" "") ("pdf" "") ("txt" "") ("csv" "")))
     (file-desc
      :key "D" :doc "description of the file link"
      :reader +org-babel--read-string
      :values (("[]" "present, but with an empty description")))
     (file-mode
      :key "M" :doc "permissions of the results file"
      :reader +org-babel--read-string
      :values (("(identity #o755)" "rwxr-xr-x, executable")
               ("(identity #o644)" "rw-r--r--")
               ("(identity #o444)" "r--r--r--, read only")))
     (output-dir
      :key "O" :doc "directory for the results file"
      :reader +org-babel--read-directory)
     (sep
      :key "S" :doc "separator for a table in :file"
      :reader +org-babel--read-string
      :values ((","  "comma separated")
               (";"  "semicolon separated")
               ("|"  "pipe separated"))))

    ("Tables and export"
     (hlines
      :key "H" :doc "keep hlines in input tables"
      :values (("no"  "strip horizontal rules before the block sees them")
               ("yes" "pass horizontal rules through as `hline'")))
     (colnames
      :key "C" :doc "handle table column names"
      :values (("nil" "strip names only when an hline marks them (default)")
               ("yes" "always treat the first row as column names")
               ("no"  "do not touch column names")))
     (rownames
      :key "R" :doc "handle table row names"
      :values (("no"  "do not touch the first column (default)")
               ("yes" "strip the first column, then put it back")))
     (exports
      :key "X" :doc "what export keeps"
      :values (("code"    "only the code (default)")
               ("results" "only the results")
               ("both"    "the code and the results")
               ("none"    "neither"))))

    ("Tangling"
     (tangle
      :key "t" :doc "tangle to a file"
      :reader +org-babel--read-file
      :values (("no"  "do not tangle (default)")
               ("yes" "tangle to the Org file's name plus the language extension")))
     (tangle-mode
      :key "T" :doc "permissions of tangled file"
      :reader +org-babel--read-string
      :values (("o755" "rwxr-xr-x, executable")
               ("o644" "rw-r--r--")
               ("o444" "r--r--r--, read only")))
     (mkdirp
      :key "m" :doc "create missing directories"
      :values (("yes" "create parent directories when tangling")
               ("no"  "fail instead of creating them")))
     (comments
      :key "#" :doc "comments around tangled code"
      :values (("no"    "no extra comments (default)")
               ("link"  "comments linking back to the Org file")
               ("yes"   "same as link, kept for compatibility")
               ("org"   "the surrounding Org text as a comment")
               ("both"  "both link and org")
               ("noweb" "link, with expanded noweb references in comments")))
     (padline
      :key "P" :doc "blank lines around the block"
      :values (("yes" "a newline before and after each block (default)")
               ("no"  "no padding between tangled blocks")))
     (shebang
      :key "b" :doc "first line; makes the file exec"
      :reader +org-babel--read-string
      :values (("\"#!/bin/sh\"" "")
               ("\"#!/bin/bash\"" "")
               ("\"#!/usr/bin/env python3\"" "")))
     (no-expand
      :key "N" :doc "do not expand when tangling"
      :values (("" "set the flag: tangle the body unexpanded" :label "(flag)" :key "y"))))

    ("Noweb"
     (noweb
      :key "n" :doc "expand <<references>>"
      :values (("no"           "never expand (default)")
               ("yes"          "expand when evaluating, tangling and exporting")
               ("tangle"       "expand when tangling only")
               ("eval"         "expand when evaluating only")
               ("no-export"    "expand when evaluating and tangling, not on export")
               ("strip-export" "expand for eval and tangle, drop the references on export")
               ("strip-tangle" "expand for eval and export, drop the references when tangling")))
     (noweb-ref
      :key "W" :doc "name this block contributes to"
      :reader +org-babel--read-string)
     (noweb-sep
      :key "," :doc "separator when concatenating"
      :reader +org-babel--read-string)
     (noweb-prefix
      :key "%" :doc "repeat indentation on each line"
      :values (("yes" "indent every expanded line like the reference (default)")
               ("no"  "indent only the first expanded line")))))
  "Org Babel header arguments, grouped for `+org-babel-header-args-menu'.")

(defun +org-babel--spec (arg)
  "Return the plist describing header argument ARG, or nil."
  (cl-loop for (_group . entries) in +org-babel-header-args
           thereis (cdr (assq arg entries))))

;;------------------------------------------------------------------------------
;; Reading and writing header argument strings
;;------------------------------------------------------------------------------

;; A header argument string -- ":results output :session foo" -- is edited as a
;; list of raw chunks rather than parsed and re-printed, so anything we do not
;; touch survives verbatim.

(defun +org-babel--chunks (string)
  "Split STRING into a list of raw \":arg value\" chunks."
  (let ((raw (org-babel-balanced-split (string-trim (or string "")) '((32 9) . 58))))
    (seq-filter (lambda (chunk) (string-prefix-p ":" chunk))
                (cons (car raw) (mapcar (lambda (r) (concat ":" r)) (cdr raw))))))

(defun +org-babel--chunk-arg (chunk)
  "Return the argument name in CHUNK, without its leading colon."
  (and (string-match "\\`:\\([^ \t]+\\)" chunk)
       (match-string 1 chunk)))

(defun +org-babel--get-in (string arg)
  "Return the value ARG has in header STRING, or nil when absent.
A flag present without a value yields the empty string."
  (let ((name (symbol-name arg)))
    (cl-loop for chunk in (+org-babel--chunks string)
             when (equal name (+org-babel--chunk-arg chunk))
             return (string-trim (substring chunk (1+ (length name)))))))

(defun +org-babel--set-in (string arg value)
  "Return header STRING with ARG set to VALUE.
A nil VALUE removes ARG; an empty VALUE leaves it as a bare flag."
  (let* ((name (symbol-name arg))
         (new (and value (string-trim (concat name " " value))))
         (found nil)
         (chunks (delq nil
                       (mapcar
                        (lambda (chunk)
                          (cond
                           ((not (equal name (+org-babel--chunk-arg chunk))) chunk)
                           (found nil) ; drop any duplicate
                           (t (setq found t)
                              (and new (concat ":" new)))))
                        (+org-babel--chunks string)))))
    (when (and new (not found))
      (setq chunks (append chunks (list (concat ":" new)))))
    (mapconcat #'string-trim chunks " ")))

;; `:results' takes up to one value from each of four orthogonal groups, so
;; setting one of them replaces only its own group.

(defconst +org-babel--results-classes '("Collect" "Type" "Format" "Handle")
  "The `:class' names used by the `results' entry of `+org-babel-header-args'.
One value from each may appear in a `:results' header argument at a time.")

(defun +org-babel--results-class (token)
  "Return the value group TOKEN belongs to, or TOKEN when it is unknown."
  (or (plist-get (cddr (assoc token (plist-get (+org-babel--spec 'results) :values)))
                 :class)
      token))

(defun +org-babel--merge-results (old new)
  "Return the `:results' value that adds NEW to OLD, replacing its group."
  (let* ((class (+org-babel--results-class new))
         (kept (seq-remove (lambda (token) (equal class (+org-babel--results-class token)))
                           (split-string (or old "") "[ \t]+" t)))
         (rank (lambda (token)
                 (or (seq-position +org-babel--results-classes
                                   (+org-babel--results-class token))
                     most-positive-fixnum))))
    (mapconcat #'identity
               (sort (append kept (list new))
                     (lambda (a b) (< (funcall rank a) (funcall rank b))))
               " ")))

;;------------------------------------------------------------------------------
;; Where the value is written
;;------------------------------------------------------------------------------

(defvar +org-babel-header-arg-scope 'block
  "Where `+org-babel-header-args-menu' writes.
One of `block', `subtree' or `subtree-any'.")

(defvar +org-babel-header-args-return-to-menu t
  "Whether setting a header argument reopens the menu.")

(defun +org-babel--lang ()
  "Return the language of the source block at point, if any."
  (when (derived-mode-p 'org-mode)
    (let ((element (org-element-at-point)))
      (and (org-element-type-p element 'src-block)
           (org-element-property :language element)))))

(defun +org-babel--property-name ()
  "Return the subtree property name the current scope writes to."
  (let ((lang (+org-babel--lang)))
    (if (and lang (eq +org-babel-header-arg-scope 'subtree))
        (concat "header-args:" lang)
      "header-args")))

(defun +org-babel--scope-label ()
  "Return a human readable name for the current scope."
  (if (eq +org-babel-header-arg-scope 'block)
      "#+begin_src line"
    (format "subtree :%s:" (+org-babel--property-name))))

(defun +org-babel--header-string ()
  "Return the header argument string of the current scope."
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (if (eq +org-babel-header-arg-scope 'block)
      (save-excursion
        (unless (org-babel-where-is-src-block-head)
          (user-error "Point is not inside a source block"))
        (match-string-no-properties 4))
    (save-excursion
      (when (org-before-first-heading-p)
        (user-error "Point is not inside a subtree"))
      (org-entry-get nil (+org-babel--property-name)))))

(defun +org-babel--write-header-string (string)
  "Write STRING as the header arguments of the current scope."
  (if (eq +org-babel-header-arg-scope 'block)
      (save-excursion
        (unless (org-babel-where-is-src-block-head)
          (user-error "Point is not inside a source block"))
        (let ((beg (match-beginning 4))
              (end (match-end 4)))
          (goto-char end)
          (delete-region beg end)
          ;; Group 3 of `org-babel-src-block-regexp' swallows the whitespace in
          ;; front of the arguments, so normalise it rather than doubling it up.
          (skip-chars-backward " \t")
          (delete-region (point) beg)
          (unless (string-empty-p string)
            (insert " " string))))
    (save-excursion
      (when (org-before-first-heading-p)
        (user-error "Point is not inside a subtree"))
      (if (string-empty-p string)
          (org-entry-delete nil (+org-babel--property-name))
        (org-entry-put nil (+org-babel--property-name) string)))))

(defun +org-babel--current-value (arg)
  "Return the value ARG has in the current scope, or nil."
  (ignore-errors (+org-babel--get-in (+org-babel--header-string) arg)))

;;;###autoload
(defun +org-babel-set-header-arg (arg value)
  "Set header argument ARG to VALUE in the current scope.
A nil VALUE removes ARG.  For `results' the VALUE replaces only the
value group -- collection, type, format or handling -- that it belongs
to, leaving the others in place."
  (interactive (list (+org-babel-read-header-arg) nil))
  (let* ((string (or (+org-babel--header-string) ""))
         (value (if (and (eq arg 'results) value)
                    (+org-babel--merge-results (+org-babel--get-in string 'results) value)
                  value)))
    (+org-babel--write-header-string (+org-babel--set-in string arg value))
    (message "%s %s" (+org-babel--scope-label)
             (if value (format ":%s %s" arg value) (format "removed :%s" arg)))))

;;------------------------------------------------------------------------------
;; Value readers
;;------------------------------------------------------------------------------

(defun +org-babel--read-string (prompt initial)
  (read-string prompt initial))

(defun +org-babel--read-var (prompt _initial)
  (read-string (concat prompt "NAME=VALUE: ")))

(defun +org-babel--relative (path)
  "Return PATH relative to the Org file when it lives below it."
  (let ((dir (and buffer-file-name (file-name-directory buffer-file-name))))
    (if (and dir (string-prefix-p (expand-file-name dir) (expand-file-name path)))
        (file-relative-name path dir)
      (abbreviate-file-name path))))

(defun +org-babel--read-directory (prompt initial)
  (+org-babel--relative (read-directory-name prompt nil nil nil initial)))

(defun +org-babel--read-file (prompt initial)
  (+org-babel--relative (read-file-name prompt nil nil nil initial)))

;;------------------------------------------------------------------------------
;; Value tables
;;------------------------------------------------------------------------------

(defun +org-babel--known-args ()
  "Return an alist of every header argument Org knows about here."
  (require 'ob-core)
  (let* ((lang (+org-babel--lang))
         (symbol (and lang (intern (concat "org-babel-header-args:" lang)))))
    (org-babel-combine-header-arg-lists
     org-babel-common-header-args-w-values
     (and symbol (boundp symbol) (symbol-value symbol)))))

(defun +org-babel--org-values (arg)
  "Return value entries for ARG taken from Org's own tables."
  (let ((values (cdr (assq arg (+org-babel--known-args)))))
    (when (and values (listp values))
      (cl-loop for group in values
               for i from 1
               append (cl-loop for value in group
                               unless (eq value :any)
                               collect (list (format "%s" value) ""
                                             :class (if (= (length values) 1)
                                                        "Values"
                                                      (format "Group %d" i))))))))

(defun +org-babel--values (arg)
  "Return the value entries offered for header argument ARG."
  (or (plist-get (+org-babel--spec arg) :values)
      (+org-babel--org-values arg)))

(defun +org-babel--reader (arg)
  "Return the custom value reader for header argument ARG."
  (or (plist-get (+org-babel--spec arg) :reader) #'+org-babel--read-string))

(defun +org-babel-read-header-arg ()
  "Prompt for a header argument, annotated with its description."
  (let ((completion-extra-properties
         (list :annotation-function
               (lambda (candidate)
                 (when-let* ((doc (plist-get (+org-babel--spec (intern candidate)) :doc)))
                   (concat "  " doc))))))
    (intern (completing-read "Header argument: "
                             (mapcar (lambda (entry) (symbol-name (car entry)))
                                     (+org-babel--known-args))
                             nil nil))))

;;------------------------------------------------------------------------------
;; Keys
;;------------------------------------------------------------------------------

(defconst +org-babel--key-pool
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  "Characters `+org-babel--assign-keys' falls back on.")

(defun +org-babel--assign-keys (names reserved)
  "Return an alist mapping each of NAMES to a free menu key.
Keys are taken from the name itself where possible, avoiding RESERVED."
  (let ((taken (copy-sequence reserved))
        (result nil))
    (dolist (name names (nreverse result))
      (let ((key (cl-loop for char across (concat name +org-babel--key-pool)
                          for candidate = (char-to-string char)
                          unless (or (member candidate taken)
                                     (not (string-match-p "[[:alnum:]]" candidate)))
                          return candidate)))
        (push key taken)
        (push (cons name key) result)))))

;;------------------------------------------------------------------------------
;; The value menu
;;------------------------------------------------------------------------------

(defun +org-babel--value-heading ()
  (transient-with-shadowed-buffer
    (let* ((arg (transient-scope))
           (current (+org-babel--current-value arg)))
      (concat (propertize (format ":%s" arg) 'face 'transient-heading)
              (when-let* ((doc (plist-get (+org-babel--spec arg) :doc)))
                (format " -- %s" doc))
              (if current
                  (format "  [currently %s]"
                          (if (string-empty-p current) "set" current))
                "  [unset]")))))

(defun +org-babel--value-children (_children)
  (let* ((arg (transient-scope))
         (entries (+org-babel--values arg))
         (fixed (seq-keep (lambda (entry) (plist-get (cddr entry) :key)) entries))
         (keys (+org-babel--assign-keys
                (mapcar #'car (seq-remove (lambda (e) (plist-get (cddr e) :key)) entries))
                (append '("!" "-") fixed)))
         (classes (delete-dups
                   (mapcar (lambda (entry) (or (plist-get (cddr entry) :class) "Values"))
                           entries)))
         (width (apply #'max 4 (mapcar (lambda (e) (length (or (plist-get (cddr e) :label)
                                                          (car e))))
                                       entries))))
    (transient-parse-suffixes
     '+org-babel-value-menu
     (mapcar
      (lambda (class)
        (vconcat
         (list class)
         (mapcar
          (lambda (entry)
            (let* ((value (car entry))
                   (label (or (plist-get (cddr entry) :label) value))
                   (doc (cadr entry)))
              (list (or (plist-get (cddr entry) :key) (cdr (assoc value keys)))
                    (string-trim-right (format "%s %s" (string-pad label width) doc))
                    `(lambda ()
                       (interactive)
                       (+org-babel-set-header-arg ',arg ,value)
                       (+org-babel--return-to-menu)))))
          (seq-filter (lambda (entry)
                        (equal class (or (plist-get (cddr entry) :class) "Values")))
                      entries))))
      classes))))

(defun +org-babel--set-custom-value ()
  "Prompt for a value for the header argument being edited."
  (interactive)
  (let ((arg (transient-scope)))
    (+org-babel-set-header-arg
     arg (funcall (+org-babel--reader arg)
                  (format ":%s " arg)
                  (+org-babel--current-value arg)))
    (+org-babel--return-to-menu)))

(defun +org-babel--unset-value ()
  "Remove the header argument being edited."
  (interactive)
  (+org-babel-set-header-arg (transient-scope) nil)
  (+org-babel--return-to-menu))

(transient-define-prefix +org-babel-value-menu (arg)
  "Choose a value for the Org Babel header argument ARG."
  [:description +org-babel--value-heading
   :class transient-columns
   :setup-children +org-babel--value-children]
  ["Value"
   ("!" "Type a value..." +org-babel--set-custom-value)
   ("-" "Unset this header argument" +org-babel--unset-value)]
  (interactive (list (+org-babel-read-header-arg)))
  (if (+org-babel--values arg)
      (transient-setup '+org-babel-value-menu nil nil :scope arg)
    ;; Nothing to choose from -- go straight to the minibuffer.
    (let ((value (funcall (+org-babel--reader arg)
                          (format ":%s (empty to unset) " arg)
                          (+org-babel--current-value arg))))
      (+org-babel-set-header-arg arg (if (string-empty-p value) nil value))
      (+org-babel--return-to-menu))))

;;------------------------------------------------------------------------------
;; The header argument menu
;;------------------------------------------------------------------------------

(defun +org-babel--return-to-menu ()
  "Reopen the header argument menu, if that is wanted."
  (when +org-babel-header-args-return-to-menu
    (run-at-time 0 nil #'+org-babel-header-args-menu)))

(defun +org-babel-cycle-scope ()
  "Cycle where header arguments are written."
  (interactive)
  (setq +org-babel-header-arg-scope
        (pcase +org-babel-header-arg-scope
          ('block 'subtree)
          ('subtree 'subtree-any)
          (_ 'block))))

(defun +org-babel--scope-description ()
  (transient-with-shadowed-buffer
    (format "Write to %s"
            (propertize (+org-babel--scope-label) 'face 'transient-value))))

(defun +org-babel--menu-heading ()
  (transient-with-shadowed-buffer
    (let ((string (ignore-errors (+org-babel--header-string))))
      (concat (propertize "Header arguments" 'face 'transient-heading)
              (when-let* ((lang (+org-babel--lang))) (format " for %s" lang))
              (format " -- writing to the %s" (+org-babel--scope-label))
              (when (org-string-nw-p string)
                (concat "\n" (propertize (string-trim string) 'face 'transient-value)))))))

(defun +org-babel--arg-description (arg doc width)
  (transient-with-shadowed-buffer
    (let ((current (+org-babel--current-value arg)))
      (concat (string-pad (symbol-name arg) width) " " doc
              (when current
                (propertize (if (string-empty-p current)
                                "  [set]"
                              (format "  [%s]" current))
                            'face 'transient-value))))))

(defun +org-babel--menu-columns (groups)
  "Parse GROUPS of `+org-babel-header-args' into transient columns."
  (transient-parse-suffixes
   '+org-babel-header-args-menu
   (mapcar
    (lambda (group)
      (let* ((entries (cdr group))
             (width (apply #'max (mapcar (lambda (e) (length (symbol-name (car e))))
                                         entries))))
        (vconcat
         (list (car group))
         (mapcar
          (lambda (entry)
            (let ((arg (car entry))
                  (doc (plist-get (cdr entry) :doc)))
              (list (plist-get (cdr entry) :key)
                    `(lambda () (+org-babel--arg-description ',arg ,doc ,width))
                    `(lambda () (interactive) (+org-babel-value-menu ',arg)))))
          entries))))
    groups)))

(defun +org-babel--menu-row-1 (_children)
  (+org-babel--menu-columns (seq-take +org-babel-header-args 3)))

(defun +org-babel--menu-row-2 (_children)
  (+org-babel--menu-columns (seq-drop +org-babel-header-args 3)))

;;;###autoload (autoload '+org-babel-header-args-menu "setup-babel" nil t)
(transient-define-prefix +org-babel-header-args-menu ()
  "Set the Org Babel header arguments of the source block at point."
  [:description +org-babel--menu-heading
   :class transient-columns
   :setup-children +org-babel--menu-row-1]
  [:class transient-columns
   :setup-children +org-babel--menu-row-2]
  [["Menu"
    ("=" +org-babel-cycle-scope
     :description +org-babel--scope-description :transient t)
    (":" "Any other header argument..." +org-babel-value-menu)]])

;;------------------------------------------------------------------------------
;; Keybindings
;;------------------------------------------------------------------------------

(map! :after org
      :map org-babel-map
      "H" #'+org-babel-header-args-menu)

(map! :after org
      :localleader
      :map org-mode-map
      :desc "Babel header args" "H" #'+org-babel-header-args-menu)
