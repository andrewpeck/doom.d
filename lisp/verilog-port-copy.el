(require 'vhdl-mode)

(defun verilog-in-component-p ()
  ;; FIXME: update to verilog
  (when (or (not (re-search-backward
                  "^\\s-*\\(component\\|entity\\|end\\)\\>" nil t))
            (equal "END" (upcase (match-string 1))))
    (throw 'parse "ERROR:  Not within an entity or component declaration")))

(defun verilog-parse-ports ()
  (when (vhdl-parse-string "port[ \t\n\r\f]*(" t)
    ;; parse group comment and spacing
    (setq group-comment (vhdl-parse-group-comment))
    (setq end-of-list (vhdl-parse-string ")[ \t\n\r\f]*;[ \t\n\r\f]*" t))
    (while (not end-of-list)
      ;; parse object
      (setq object
            (and (vhdl-parse-string "\\<\\(signal\\|quantity\\|terminal\\)\\>[ \t\n\r\f]*" t)
                 (match-string-no-properties 1)))
      ;; parse names (accept extended identifiers)
      (vhdl-parse-string "\\(\\\\[^\\]+\\\\\\|\\w+\\)[ \t\n\r\f]*")
      (setq names (list (match-string-no-properties 1)))
      (while (vhdl-parse-string ",[ \t\n\r\f]*\\(\\\\[^\\]+\\\\\\|\\w+\\)[ \t\n\r\f]*" t)
        (setq names (append names (list (match-string-no-properties 1)))))
      ;; parse direction
      (vhdl-parse-string ":[ \t\n\r\f]*")
      (setq direct
            (and (vhdl-parse-string "\\<\\(in\\|out\\|inout\\|buffer\\|linkage\\)\\>[ \t\n\r\f]+" t)
                 (match-string-no-properties 1)))
      ;; parse type
      (vhdl-parse-string "\\([^();\n]+\\)")
      (setq type (match-string-no-properties 1))
      (when (vhdl-in-comment-p) ; if stuck in comment
        (setq type (concat type (and (vhdl-parse-string ".*")
                                     (match-string-no-properties 0)))))
      (setq comment nil)
      (while (looking-at "(")
        (setq type (concat type
                           (buffer-substring-no-properties
                            (point) (progn (forward-sexp) (point)))
                           (and (vhdl-parse-string "\\([^();\n]*\\)" t)
                                (match-string-no-properties 1)))))
      ;; special case: closing parenthesis is on separate line
      (when (and type (string-match "\\(\\s-*--\\s-*\\)\\(.*\\)" type))
        (setq comment (substring type (match-beginning 2)))
        (setq type (substring type 0 (match-beginning 1))))
      ;; strip of trailing group-comment
      (string-match "\\(\\(\\s-*\\S-+\\)+\\)\\s-*" type)
      (setq type (substring type 0 (match-end 1)))
      (vhdl-forward-syntactic-ws)
      (setq end-of-list (vhdl-parse-string ")" t))
      (vhdl-parse-string "\\s-*;\\s-*")
      ;; parse inline comment
      (unless comment
        (setq comment (and (vhdl-parse-string "--\\s-*\\([^\n]*\\)" t)
                           (match-string-no-properties 1))))
      ;; save everything in list
      (setq port-list (append port-list
                              (list (list names object direct type
                                          comment group-comment))))
      ;; parse group comment and spacing
      (setq group-comment (vhdl-parse-group-comment)))))

(defun verilog-parse-generics ()
  (when (vhdl-parse-string "generic[ \t\n\r\f]*(" t)
    ;; parse group comment and spacing
    (setq group-comment (vhdl-parse-group-comment))
    (setq end-of-list (vhdl-parse-string ")[ \t\n\r\f]*;[ \t\n\r\f]*" t))
    (while (not end-of-list)
      ;; parse names (accept extended identifiers)
      (vhdl-parse-string "\\(\\\\[^\\]+\\\\\\|\\w+\\)[ \t\n\r\f]*")
      (setq names (list (match-string-no-properties 1)))
      (while (vhdl-parse-string ",[ \t\n\r\f]*\\(\\\\[^\\]+\\\\\\|\\w+\\)[ \t\n\r\f]*" t)
        (setq names
              (append names (list (match-string-no-properties 1)))))
      ;; parse type
      (vhdl-parse-string ":[ \t\n\r\f]*\\([^():;\n]+\\)")
      (setq type (match-string-no-properties 1))
      (when (vhdl-in-comment-p) ; if stuck in comment
        (setq type (concat type (and (vhdl-parse-string ".*")
                                     (match-string-no-properties 0)))))
      (setq comment nil)
      (while (looking-at "(")
        (setq type
              (concat type
                      (buffer-substring-no-properties
                       (point) (progn (forward-sexp) (point)))
                      (and (vhdl-parse-string "\\([^():;\n]*\\)" t)
                           (match-string-no-properties 1)))))
      ;; special case: closing parenthesis is on separate line
      (when (and type (string-match "\\(\\s-*--\\s-*\\)\\(.*\\)" type))
        (setq comment (substring type (match-beginning 2)))
        (setq type (substring type 0 (match-beginning 1))))
      ;; strip of trailing group-comment
      (string-match "\\(\\(\\s-*\\S-+\\)+\\)\\s-*" type)
      (setq type (substring type 0 (match-end 1)))
      ;; parse initialization expression
      (setq init nil)
      (when (vhdl-parse-string ":=[ \t\n\r\f]*" t)
        (vhdl-parse-string "\\([^();\n]*\\)")
        (setq init (match-string-no-properties 1))
        (while (looking-at "(")
          (setq init
                (concat init
                        (buffer-substring-no-properties
                         (point) (progn (forward-sexp) (point)))
                        (and (vhdl-parse-string "\\([^();\n]*\\)" t)
                             (match-string-no-properties 1))))))
      ;; special case: closing parenthesis is on separate line
      (when (and init (string-match "\\(\\s-*--\\s-*\\)\\(.*\\)" init))
        (setq comment (substring init (match-beginning 2)))
        (setq init (substring init 0 (match-beginning 1)))
        (vhdl-forward-syntactic-ws))
      (skip-chars-forward " \t")
      ;; parse inline comment, special case: as above, no initial.
      (unless comment
        (setq comment (and (vhdl-parse-string "--\\s-*\\([^\n]*\\)" t)
                           (match-string-no-properties 1))))
      (vhdl-forward-syntactic-ws)
      (setq end-of-list (vhdl-parse-string ")" t))
      (vhdl-parse-string "\\s-*;\\s-*")
      ;; parse inline comment
      (unless comment
        (setq comment (and (vhdl-parse-string "--\\s-*\\([^\n]*\\)" t)
                           (match-string-no-properties 1))))
      ;; save everything in list
      (setq generic-list (append generic-list
                                 (list (list names type init
                                             comment group-comment))))
      ;; parse group comment and spacing
      (setq group-comment (vhdl-parse-group-comment)))))

(defun verilog-parse-context ()
  ;; parse context clause
  (setq context-clause (vhdl-scan-context-clause))
  (message "Reading port of %s \"%s\"...done" decl-type name)
  )

(defun verilog-port-copy ()
  ""
  (interactive)
  (save-excursion)
  (let (parse-error
        end-of-list
        decl-type
        name
        generic-list
        port-list
        context-clause
        object
        names
        direct
        type
        init
        comment
        group-comment)


    ;; Enable case insensitive search, switch to syntax table that includes _,
    ;; arrange to ignore intangible overlays, then execute BODY, and finally restore
    ;; the old environment.  Used for consistent searching.
    (vhdl-prepare-search-2
     (setq parse-error
           (catch 'parse
             ;; check if within module declaration
             (end-of-line)

             (verilog-in-component-p)

             (setq decl-type (downcase (match-string-no-properties 1)))
             (forward-word-strictly 1)

             ;; FIXME:
             (vhdl-parse-string "\\s-+\\(\\w+\\)\\(\\s-+is\\>\\)?")
             (setq name (match-string-no-properties 1))
             (message "Reading port of %s \"%s\"..." decl-type name)
             (vhdl-forward-syntactic-ws)

             ;; parse parameters clause
             (verilog-parse-generics)

             ;; parse port clause
             (verilog-parse-ports)

             ;; parse context clause
             (verilog-parse-context)

             nil)))

    (if parse-error
        (error parse-error)
      (setq vhdl-port-list (list name generic-list port-list context-clause)
            vhdl-port-reversed-direction nil
            vhdl-port-flattened nil))))

(defun vhdl-port-copy-original ()
  "Get generic and port information from an entity or component declaration."
  (interactive)
  (save-excursion
    (let (parse-error end-of-list
                      decl-type name generic-list port-list context-clause
                      object names direct type init comment group-comment)
      (vhdl-prepare-search-2
       (setq
        parse-error
        (catch 'parse
          ;; check if within entity or component declaration
          (end-of-line)
          (when (or (not (re-search-backward
                          "^\\s-*\\(component\\|entity\\|end\\)\\>" nil t))
                    (equal "END" (upcase (match-string 1))))
            (throw 'parse "ERROR:  Not within an entity or component declaration"))
          (setq decl-type (downcase (match-string-no-properties 1)))
          (forward-word-strictly 1)
          (vhdl-parse-string "\\s-+\\(\\w+\\)\\(\\s-+is\\>\\)?")
          (setq name (match-string-no-properties 1))
          (message "Reading port of %s \"%s\"..." decl-type name)
          (vhdl-forward-syntactic-ws)
          ;; parse generic clause
          (when (vhdl-parse-string "generic[ \t\n\r\f]*(" t)
            ;; parse group comment and spacing
            (setq group-comment (vhdl-parse-group-comment))
            (setq end-of-list (vhdl-parse-string ")[ \t\n\r\f]*;[ \t\n\r\f]*" t))
            (while (not end-of-list)
              ;; parse names (accept extended identifiers)
              (vhdl-parse-string "\\(\\\\[^\\]+\\\\\\|\\w+\\)[ \t\n\r\f]*")
              (setq names (list (match-string-no-properties 1)))
              (while (vhdl-parse-string ",[ \t\n\r\f]*\\(\\\\[^\\]+\\\\\\|\\w+\\)[ \t\n\r\f]*" t)
                (setq names
                      (append names (list (match-string-no-properties 1)))))
              ;; parse type
              (vhdl-parse-string ":[ \t\n\r\f]*\\([^():;\n]+\\)")
              (setq type (match-string-no-properties 1))
              (when (vhdl-in-comment-p) ; if stuck in comment
                (setq type (concat type (and (vhdl-parse-string ".*")
                                             (match-string-no-properties 0)))))
              (setq comment nil)
              (while (looking-at "(")
                (setq type
                      (concat type
                              (buffer-substring-no-properties
                               (point) (progn (forward-sexp) (point)))
                              (and (vhdl-parse-string "\\([^():;\n]*\\)" t)
                                   (match-string-no-properties 1)))))
              ;; special case: closing parenthesis is on separate line
              (when (and type (string-match "\\(\\s-*--\\s-*\\)\\(.*\\)" type))
                (setq comment (substring type (match-beginning 2)))
                (setq type (substring type 0 (match-beginning 1))))
              ;; strip of trailing group-comment
              (string-match "\\(\\(\\s-*\\S-+\\)+\\)\\s-*" type)
              (setq type (substring type 0 (match-end 1)))
              ;; parse initialization expression
              (setq init nil)
              (when (vhdl-parse-string ":=[ \t\n\r\f]*" t)
                (vhdl-parse-string "\\([^();\n]*\\)")
                (setq init (match-string-no-properties 1))
                (while (looking-at "(")
                  (setq init
                        (concat init
                                (buffer-substring-no-properties
                                 (point) (progn (forward-sexp) (point)))
                                (and (vhdl-parse-string "\\([^();\n]*\\)" t)
                                     (match-string-no-properties 1))))))
              ;; special case: closing parenthesis is on separate line
              (when (and init (string-match "\\(\\s-*--\\s-*\\)\\(.*\\)" init))
                (setq comment (substring init (match-beginning 2)))
                (setq init (substring init 0 (match-beginning 1)))
                (vhdl-forward-syntactic-ws))
              (skip-chars-forward " \t")
              ;; parse inline comment, special case: as above, no initial.
              (unless comment
                (setq comment (and (vhdl-parse-string "--\\s-*\\([^\n]*\\)" t)
                                   (match-string-no-properties 1))))
              (vhdl-forward-syntactic-ws)
              (setq end-of-list (vhdl-parse-string ")" t))
              (vhdl-parse-string "\\s-*;\\s-*")
              ;; parse inline comment
              (unless comment
                (setq comment (and (vhdl-parse-string "--\\s-*\\([^\n]*\\)" t)
                                   (match-string-no-properties 1))))
              ;; save everything in list
              (setq generic-list (append generic-list
                                         (list (list names type init
                                                     comment group-comment))))
              ;; parse group comment and spacing
              (setq group-comment (vhdl-parse-group-comment))))
          ;; parse port clause
          (when (vhdl-parse-string "port[ \t\n\r\f]*(" t)
            ;; parse group comment and spacing
            (setq group-comment (vhdl-parse-group-comment))
            (setq end-of-list (vhdl-parse-string ")[ \t\n\r\f]*;[ \t\n\r\f]*" t))
            (while (not end-of-list)
              ;; parse object
              (setq object
                    (and (vhdl-parse-string "\\<\\(signal\\|quantity\\|terminal\\)\\>[ \t\n\r\f]*" t)
                         (match-string-no-properties 1)))
              ;; parse names (accept extended identifiers)
              (vhdl-parse-string "\\(\\\\[^\\]+\\\\\\|\\w+\\)[ \t\n\r\f]*")
              (setq names (list (match-string-no-properties 1)))
              (while (vhdl-parse-string ",[ \t\n\r\f]*\\(\\\\[^\\]+\\\\\\|\\w+\\)[ \t\n\r\f]*" t)
                (setq names (append names (list (match-string-no-properties 1)))))
              ;; parse direction
              (vhdl-parse-string ":[ \t\n\r\f]*")
              (setq direct
                    (and (vhdl-parse-string "\\<\\(in\\|out\\|inout\\|buffer\\|linkage\\)\\>[ \t\n\r\f]+" t)
                         (match-string-no-properties 1)))
              ;; parse type
              (vhdl-parse-string "\\([^();\n]+\\)")
              (setq type (match-string-no-properties 1))
              (when (vhdl-in-comment-p) ; if stuck in comment
                (setq type (concat type (and (vhdl-parse-string ".*")
                                             (match-string-no-properties 0)))))
              (setq comment nil)
              (while (looking-at "(")
                (setq type (concat type
                                   (buffer-substring-no-properties
                                    (point) (progn (forward-sexp) (point)))
                                   (and (vhdl-parse-string "\\([^();\n]*\\)" t)
                                        (match-string-no-properties 1)))))
              ;; special case: closing parenthesis is on separate line
              (when (and type (string-match "\\(\\s-*--\\s-*\\)\\(.*\\)" type))
                (setq comment (substring type (match-beginning 2)))
                (setq type (substring type 0 (match-beginning 1))))
              ;; strip of trailing group-comment
              (string-match "\\(\\(\\s-*\\S-+\\)+\\)\\s-*" type)
              (setq type (substring type 0 (match-end 1)))
              (vhdl-forward-syntactic-ws)
              (setq end-of-list (vhdl-parse-string ")" t))
              (vhdl-parse-string "\\s-*;\\s-*")
              ;; parse inline comment
              (unless comment
                (setq comment (and (vhdl-parse-string "--\\s-*\\([^\n]*\\)" t)
                                   (match-string-no-properties 1))))
              ;; save everything in list
              (setq port-list (append port-list
                                      (list (list names object direct type
                                                  comment group-comment))))
              ;; parse group comment and spacing
              (setq group-comment (vhdl-parse-group-comment))))
          ;; parse context clause
          (setq context-clause (vhdl-scan-context-clause))
          (message "Reading port of %s \"%s\"...done" decl-type name)
          nil)))
      ;; finish parsing
      (if parse-error
          (error parse-error)
        (setq vhdl-port-list (list name generic-list port-list context-clause)
              vhdl-port-reversed-direction nil
              vhdl-port-flattened nil)))))
