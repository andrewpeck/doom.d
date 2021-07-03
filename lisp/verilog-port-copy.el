(require 'vhdl-mode)

(setq example
      '("distrip"
        ((("A_GENERIC")
          "integer" "0" nil ""))
        ((("qn_m2")
          nil "in" "std_logic_vector (9 downto 0)" nil "\n")
         (("qn_m1")
          nil "in" "std_logic_vector (9 downto 0)" nil "")
         (("qn")
          nil "in" "std_logic_vector (9 downto 0)" nil "")
         (("qn_p1")
          nil "in" "std_logic_vector (9 downto 0)" nil "")
         (("qn_p2")
          nil "in" "std_logic_vector (9 downto 0)" nil "")
         (("qn_p3")
          nil "in" "std_logic_vector (9 downto 0)" nil "")
         (("vth")
          nil "in" "std_logic_vector (9 downto 0)" nil "")
         (("bypass_t5")
          nil "in" "std_logic" nil "\n")
         (("bypass_t4")
          nil "in" "std_logic" nil "")
         (("bypass_t3")
          nil "in" "std_logic" nil "")
         (("distrip_out")
          nil "out" "std_logic" nil "\n")
         (("t0")
          nil "out" "std_logic" nil "")
         (("t1")
          nil "out" "std_logic" nil "")
         (("t2")
          nil "out" "std_logic" nil ""))
        (("ieee" . "std_logic_1164")
         ("ieee" . "std_logic_misc")
         ("ieee" . "numeric_std")))
      )

(defun verilog-flatten-buffer ()
  "Flatten a Verilog buffer.
removes all comments and newlines for easier processing as a
stream."
  (goto-char (point-min)) ; move to beginning of buffer

  ;; remove all comments
  (while (re-search-forward "\/\/.*\n" nil t)
    (replace-match ""))

  (goto-char (point-min)) ; move to beginning of buffer

  ;; with comments removed, safe to remove all newlines
  (while (re-search-forward "\n" nil t)
    (replace-match " "))

  (goto-char (point-min)) ; move to beginning of buffer
  )

;;;-----------------------------------------------------------------------------
;;; Top Level Port Copy Function
;;;-----------------------------------------------------------------------------

(defun verilog-port-copy ()
  ""
  (interactive)
  (save-excursion) ; Save point, and current buffer; execute BODY; restore those things.

  (let (parse-error name generic-list port-list context-clause)

    ;; Enable case insensitive search, switch to syntax table that includes _,
    ;; arrange to ignore intangible overlays, then execute BODY, and finally restore
    ;; the old environment.  Used for consistent searching.
    (setq parse-error

          (catch 'parse

            (setq name (verilog-get-module-name))
            (setq generic-list (verilog-parse-generics)) ; parse parameters clause
            (setq port-list (verilog-parse-ports))       ; parse port clause
            (setq context-clause nil)                    ; not useful for now

            ;; printouts
            (message "Reading port of \"%s\"..."  name)
            (print generic-list)
            (print port-list)

            nil))

    (if parse-error
        (error parse-error)
      (setq vhdl-port-list (list name generic-list port-list context-clause)
            vhdl-port-reversed-direction nil
            vhdl-port-flattened nil))))

;;;------------------------------------------------------------------------------
;;; Module name
;;;------------------------------------------------------------------------------

(defun verilog-get-module-name ()
  "Get the name of the Verilog module in the currently opened
buffer. if you have multiple modules in one file it will just
choose the first one, sorry. Eventually it should choose the
module at-point but for now that is not supported"

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

;;;-----------------------------------------------------------------------------
;;; Parameters
;;;-----------------------------------------------------------------------------

(defun verilog-parse-ansi-parameters ()
  "Placeholder for ANSI style parameter parsing"
  ;; parse names (accept extended identifiers)
  nil
  )

(defun verilog-parse-nonansi-parameters ()
  "Gathers up the non-ansi parameters in a Verilog module."

  (interactive)

  (let ((old-buffer (current-buffer)))

    ;; create a temp buffer as a copy of the current one
    (with-temp-buffer
      (insert-buffer-substring-no-properties old-buffer)

      (verilog-flatten-buffer) ; flatten the current buffer into a easy to parse stream

      (goto-char (point-min))
      (let ((parameters nil))

        (cl-flet ((push-to-params
                   (name val)
                   (when name (push (cons name val) parameters))))

          ;; get uninitialized params, e.g. "parameter MXCNT;"
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
                  ;; FIXME: need to account for different radixes, 'h3 / 7'h3 /
                  ;; 3'b10001 etc... right now this only works with integer
                  ;; parameters
                  (concat
                   "parameter\s+" ;;
                   "\\([A-z,0-9]+\\)" ;; name
                   "\s*=\s*"
                   "\\([0-9]+\\)" ;; val
                   "\s*;"
                   ) nil t)
            (let ((name (match-string 1))
                  (val  (match-string 2)))
              (push-to-params name val))))

        (print parameters) ; print the found parameters
        parameters)))) ; return the found parameters

(defun verilog-parse-generics ()
  "Wrapper to gather up both ANSI and non-ANSI parameters into a list"

  ;; save everything in list

  ;; (setq names (verilog-parse-ansi-parameters))
  ;; (setq generic-list
  ;;       (append generic-list
  ;;               (list (list names "integer" "" "" ""))))

  (setq names (verilog-parse-nonansi-parameters))
  (setq generic-list
        (append generic-list
                (list (list names "integer" "" "" "")))))


;;;-----------------------------------------------------------------------------
;;; Ports
;;;-----------------------------------------------------------------------------

(defun verilog-parse-ansi-ports ()
  ""
  (interactive)
  (let ((old-buffer (current-buffer)))
    (with-temp-buffer

      (insert-buffer-substring-no-properties old-buffer)
      (verilog-flatten-buffer)

      (let ((ports nil))

        (cl-flet ((push-to-ports
                   (name dir type)

                   ;; match to specified format, e.g.
                   ;; ((("qn_m2") nil "in" "std_logic_vector (9 downto 0)" nil "\n")
                   (when name (push (list (list name) nil dir type nil "") ports))))

          ;; get std_logics
          (goto-char (point-min))
          (while (re-search-forward
                  (concat
                   "\\(input\\|output\\)\s+" ; direction
                   "\\([0-9,A-z]+\\)"        ; name
                   "\s*\\(,\\|)\\)"          ; trailing comma or paren
                   ) nil t 1)
            (let ((dir (match-string 1))
                  (name (match-string 2)))
              (push-to-ports name dir "std_logic")
              (message (format "%s : %s" name dir)))
            )

          ;; get buses
          (goto-char (point-min))
          (while (re-search-forward
                  (concat
                   "\\(input\\|output\\)\s+" ; direction
                   "\\[\\([0-9]*\\)\s*:"     ; bit high
                   "\\([0-9]*\\)]\s*"        ; bit low
                   "\\([0-9,A-z]+\\)"        ; name
                   "\s*\\(,\\|)\\)"          ; trailing comma or paren
                   ) nil t 1)
            (let ((dir (match-string 1))
                  (bithi (match-string 2))
                  (bitlo (match-string 3))
                  (name (match-string 4)))
              ;;(push-to-ports name dir (format "std_logic_vector (%s downto %s)" bithi bitlo))
              (message (format "%s : %s [%s:%s]" name dir bithi bitlo))

              ))) ports))))

(defun verilog-parse-ports ()
  ""
  (verilog-parse-ansi-ports))

