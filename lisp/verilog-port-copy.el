(require 'vhdl-mode)

(setq example
      '("optohybrid_fw"

        ((("GEN_TRIG_PHY")
          "boolean" "true" nil "\n-- turn off to disable the MGTs (for simulation and such)\n")
         (("GLOBAL_DATE")
          "std_logic_vector (31 downto 0)" "x\"00000000\"" nil "\n-- "))

        ((("clock_p")
          nil "in" "std_logic" nil "\n---")
         (("clock_n")
          nil "in" "std_logic" nil "")
         (("elink_i_p")))

        (("unisim" . "vcomponents"))))

(defun verilog-flatten-buffer ()
  "Flatten a Verilog buffer.
removes all comments and newlines for
easier processing as a stream."
  (goto-char (point-min))
  ;; remove all comments
  (while (re-search-forward "\/\/.*\n" nil t)
    (replace-match ""))
  (goto-char (point-min))

  ;; with comments removed, safe to remove all newlines
  (while (re-search-forward "\n" nil t)
    (replace-match " "))
  (goto-char (point-min)))

(defun verilog-get-module-name ()
  "Get the name of the Verilog module in the currently opened
buffer. if you have multiple modules in one file it will just
choose the first one, sorry."
  (let ((parent-buffer (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring-no-properties parent-buffer)
      (verilog-flatten-buffer)
      (while (re-search-forward
              (concat "module\s+"        ; instance name
                      "\\([A-z,0-9]+\\)" ; library
                      "\s*("
                      ) nil t 1))
      (let ((name (match-string 1)))
        ;; if there is a match, intern it, otherwise return nil
        (if name name nil)))))

(defun verilog-parse-ansi-parameters ()
  ""
  ;; parse names (accept extended identifiers)
  nil
  )

(defun verilog-parse-nonansi-parameters ()
  ""
  (interactive)
  (let ((old-buffer (current-buffer)))
    (with-temp-buffer

      (insert-buffer-substring-no-properties old-buffer)
      (verilog-flatten-buffer)

      ;; get uninitialized params, e.g. "parameter MXCNT;"
      (goto-char (point-min))
      (let ((parameters nil))

        (cl-flet ((push-to-params
                   (name val)
                   (when name (push (cons name val) parameters))))

          (while (re-search-forward
                  (concat
                   "parameter\s+" ;;
                   "\\([A-z,0-9]+\\)" ;; name
                   "\s*;"
                   ) nil t)
            (let ((name (match-string 1))
                  (val nil))
              (push-to-params name val)))

          ;; get initialized params, e.g. "parameter MXCNT = 12;"
          (goto-char (point-min))
          (while (re-search-forward
                  (concat
                   "parameter\s+" ;;
                   "\\([A-z,0-9]+\\)" ;; name
                   "\s*=\s*"
                   "\\([0-9]+\\)" ;; val
                   "\s*;"
                   ) nil t)
            (let ((name (match-string 1))
                  (val  (match-string 2)))
              (push-to-params name val)))

          ) (print parameters) parameters))))

(defun verilog-parse-generics ()

  ;; save everything in list

  ;; (setq names (verilog-parse-ansi-parameters))
  ;; (setq generic-list
  ;;       (append generic-list
  ;;               (list (list names "integer" "" "" ""))))

  (setq names (verilog-parse-nonansi-parameters))
  (setq generic-list
        (append generic-list
                (list (list names "integer" "" "" "")))))

(defun verilog-parse-ports ()
  ""
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

(defun verilog-port-copy ()
  ""
  (interactive)
  (save-excursion)

  (let (parse-error name generic-list port-list context-clause)
    ;; Enable case insensitive search, switch to syntax table that includes _,
    ;; arrange to ignore intangible overlays, then execute BODY, and finally restore
    ;; the old environment.  Used for consistent searching.
    (setq parse-error
          (catch 'parse

            (setq name verilog-get-module-name)
            (message "Reading port of \"%s\"..."  name)

            ;; parse parameters clause
            (setq generic-list (verilog-parse-generics))

            ;; parse port clause
            (setq port-list (verilog-parse-ports))

            (setq context-clause nil)

            nil))

    (if parse-error (error parse-error)
      (setq vhdl-port-list (list name generic-list port-list context-clause)
            vhdl-port-reversed-direction nil
            vhdl-port-flattened nil))))
