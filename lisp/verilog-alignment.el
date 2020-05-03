;;; verilog.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Andrew
;;
;; Author: Andrew <http://github/andy>
;; Maintainer: Andrew <andy@pepper>
;; Created: May 02, 2020
;; Modified: May 02, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/andy/verilog
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:



(provide 'verilog)
;;; verilog.el ends here

(defun verilog-fixup-whitespace-buffer ()
  "Fixup whitespace in buffer.  Surround operator symbols by one space,
eliminate multiple spaces (except at beginning of line), eliminate spaces at
end of line, do nothing in comments."
  (interactive)
  (vhdl-fixup-whitespace-region (point-min) (point-max)))

(defun vhdl-fixup-whitespace-region (beg end &optional no-message)
  "Fixup whitespace in region.  Surround operator symbols by one space,
eliminate multiple spaces (except at beginning of line), eliminate spaces at
end of line, do nothing in comments and strings."
  (interactive "r")
  (unless no-message (message "Fixing up whitespace..."))
  (save-excursion
    (goto-char end)
    (setq end (point-marker))
    ;; have no space before and one space after `,' and ';'
    (goto-char beg)
    ;(while (re-search-forward "\\(--.*\n\\|\"[^\"\n]*[\"\n]\\|'.'\\|\\\\[^\\\n]*[\\\n]\\)\\|\\(\\s-*\\([,;]\\)\\)" end t)
    (while
        (re-search-forward
         (rx
          (or
            ;; match 1 = strings & quotes
            ;; match 2 = commas and semicolons
            (submatch (or
                       (seq "//" (* not-newline) "") ;; comments //xxx
                       (seq "\"" (* (not (any ?\n ?\"))) (any ?\n ?\")) ;; strings "xxxx"
                       (seq "'" not-newline "'") ;; 1 digit things in single quotes '0'
                       (seq "\\" (* (not (any ?\n ?\\))) (any ?\n ?\\)) ;; things between backslashes /xxxxx/
                       ))
            ;; commas or semicolons + preceeding whitespace
            (submatch (* (syntax whitespace)) (submatch (any ?\, ?\;)))
            )) end t)
    (if (match-string 1)
        (goto-char (match-end 1))
      (replace-match "\\3 " nil nil nil 2)))

    ;; have no space after `('
    (goto-char beg)
    ;(while (re-search-forward "\\(--.*\n\\|\"[^\"\n]*[\"\n]\\|'.'\\|\\\\[^\\\n]*[\\\n]\\)\\|\\((\\)\\s-+" end t)
    (while (re-search-forward
            (rx (or (submatch
                     (or
                      (seq "//" (* nonl) "") ;; comments
                      (seq "\"" (* (not (any ?\n ?\"))) (any ?\n ?\"))
                      (seq "'" nonl "'")
                      (seq "\\" (* (not (any ?\n ?\\))) (any ?\n ?\\))
                      )
                     )
                    ;; whitespace after a )
              (seq (submatch (any ?\()) (+ (syntax whitespace)))
              )) end t)
      (if (match-string 1)
          (goto-char (match-end 1))
        (replace-match "\\2")))

    ;; have no space before `)'
    (goto-char beg)
    (while (re-search-forward
            (rx
             (or
              (submatch (or
                         (seq "//" (* nonl) "")
                         (seq "\"" (* (not (any ?\n ?\"))) (any ?\n ?\"))
                         (seq "'" nonl "'")
                         (seq "\\" (* (not (any ?\n ?\\))) (any ?\n ?\\))
                         (seq bol (+ (syntax whitespace)))
                         ))
              ;; whitespace before a )
              (seq (+ (syntax whitespace)) (submatch (any ?\))))
              )) end t)
      (if (match-string 1)
          (goto-char (match-end 1))
        (replace-match "\\2")))

    ;; surround operator symbols by one space
    (goto-char beg)
    (while
        (re-search-forward
          ;"\\(--.*\n\\|\"[^\"\n]*[\"\n]\\|'.'\\|\\\\[^\\\n]*[\\\n]\\)\\|\\(\\([^/:<>=\n]\\)\\(:\\|\\??=\\|\\??<<\\|\\??>>\\|\\??<\\|\\??>\\|:=\\|\\??<=\\|\\??>=\\|=>\\|\\??/=\\|\\?\\?\\)\\([^=>\n]\\|$\\)\\)"
         (rx
          (or
           (submatch (or
                      (seq "//" (* nonl) "")
                      (seq "\"" (* (not (any ?\n ?\"))) (any ?\n ?\"))
                      (seq "'" nonl "'")
                      (seq "\\" (* (not (any ?\n ?\\))) (any ?\n ?\\))
                      ))
           (submatch
            (submatch (not (any ?\n ?/ ?: (?< . ?>))))
            (submatch
             (or
              ;(any ?:)
              "=="
              "="
              "<<"
              ">>"
              "<"
              ">"
              "<="
              ">="
              "+:"
              "-:"
              "<="
              "!="
              "?"))
            (submatch (or (not (any ?\n ?= ?>)) eol))
            )
           )
          )
         end t)
      (if (or (match-string 1)
              (<= (match-beginning 0)  ; not if at boi
     (save-excursion (back-to-indentation) (point))))
          (goto-char (match-end 0))
        (replace-match "\\3 \\4 \\5")
        (goto-char (match-end 2))))

    ;; eliminate multiple spaces and spaces at end of line
    (goto-char beg)
    (while (or
            (and (looking-at "--.*\n") (re-search-forward "--.*\n" end t))
            (and (looking-at "--.*") (re-search-forward "--.*" end t))
            (and (looking-at "\"") (re-search-forward "\"[^\"\n]*[\"\n]" end t))
            (and (looking-at "\\s-+$") (re-search-forward "\\s-+$" end t)
                 (progn (replace-match "" nil nil) t))
            (and (looking-at "\\s-+;") (re-search-forward "\\s-+;" end t)
                 (progn (replace-match ";" nil nil) t))
            (and (looking-at "^\\s-+") (re-search-forward "^\\s-+" end t))
            (and (looking-at "\\s-+--") (re-search-forward "\\s-+" end t)
                 (progn (replace-match "  " nil nil) t))
            (and (looking-at "\\s-+") (re-search-forward "\\s-+" end t)
                 (progn (replace-match " " nil nil) t))
            (and (looking-at "-") (re-search-forward "-" end t))
            (re-search-forward "[^ \t\"-]+" end t))))
  (unless no-message (message "Fixing up whitespace...done")))

(defconst verilog-align-alist
  '(
    ;; after some keywords
    (verilog-mode "^\\s-*\\(across\\|constant\\|quantity\\|signal\\|subtype\\|terminal\\|through\\|type\\|variable\\)[ \t]"
	       "^\\s-*\\(across\\|constant\\|quantity\\|signal\\|subtype\\|terminal\\|through\\|type\\|variable\\)\\([ \t]+\\)" 2)
    ;; before ':'
    (verilog-mode ":[^=]" "\\([ \t]*\\):[^=]")
    ;; after direction specifications
    (verilog-mode ":[ \t]*\\(in\\|out\\|inout\\|buffer\\|\\)\\>"
	       ":[ \t]*\\(in\\|out\\|inout\\|buffer\\|\\)\\([ \t]+\\)" 2)
    ;; before "==", ":=", "=>", and "<="
    (verilog-mode "[<:=]=" "\\([ \t]*\\)\\??[<:=]=" 1) ; since "<= ... =>" can occur
    (verilog-mode "=>" "\\([ \t]*\\)=>" 1)
    (verilog-mode "[<:=]=" "\\([ \t]*\\)\\??[<:=]=" 1) ; since "=> ... <=" can occur
    ;; before some keywords
    (verilog-mode "[ \t]after\\>" "[^ \t]\\([ \t]+\\)after\\>" 1)
    (verilog-mode "[ \t]when\\>" "[^ \t]\\([ \t]+\\)when\\>" 1)
    (verilog-mode "[ \t]else\\>" "[^ \t]\\([ \t]+\\)else\\>" 1)
    (verilog-mode "[ \t]across\\>" "[^ \t]\\([ \t]+\\)across\\>" 1)
    (verilog-mode "[ \t]through\\>" "[^ \t]\\([ \t]+\\)through\\>" 1)
    ;; before "=>" since "when/else ... =>" can occur
    (verilog-mode "=>" "\\([ \t]*\\)=>" 1)
    )
  "The format of this alist is (MODES [or MODE] REGEXP ALIGN-PATTERN SUBEXP).
It is searched in order.  If REGEXP is found anywhere in the first
line of a region to be aligned, ALIGN-PATTERN will be used for that
region.  ALIGN-PATTERN must include the whitespace to be expanded or
contracted.  It may also provide regexps for the text surrounding the
whitespace.  SUBEXP specifies which sub-expression of
ALIGN-PATTERN matches the white space to be expanded/contracted.")
