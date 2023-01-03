#!/usr/bin/env sh
:; ( echo "$EMACS" | grep -q "term" ) && EMACS=emacs || EMACS=${EMACS:-emacs} # -*-emacs-lisp-*-
:; command -v $EMACS >/dev/null || { >&2 echo "Can't find emacs in your PATH"; exit 1; }
:; exec emacs -Q --script "$0" -- "$@"
:; exit 0
;; -*- lexical-binding: t -*-

(when (not (equal "~/emacs-backups"
                  (string-trim (abbreviate-file-name
                                (shell-command-to-string "pwd")))))
  (error "Not in the emacs backup dir!"))

(let* ((match-re (rx (seq (submatch (* nonl))
                          "-"
                          (submatch
                           (= 4 digit) "_" ; year
                           (= 2 digit) "_" ; month
                           (= 2 digit)) "_" ; day
                          (= 2 digit) "_" ; hours
                          (= 2 digit) "_" ; min
                          (= 2 digit)))) ; sec
       (all-files
        (seq-filter
         (lambda (x)
           (string-match-p match-re x))
         (split-string
          (shell-command-to-string "find . -type f") "\n")))

       (file-names-sans-seconds
        (delete-dups
         (mapcar (lambda (x) ()
                   (string-match match-re x)
                   (concat
                    (match-string 1 x)
                    "-"
                    (match-string 2 x))) all-files))))

(defun bytes-to-megabytes (bytes)
  "Converts the given BYTES to megabytes and prints the result."
  (let ((megabytes (/ bytes 1048576.0)))
    (message "%.2f megabytes" megabytes)))

  (let ((size 0)
        (cnt 0))
    (dolist (file file-names-sans-seconds)
      ;; (princ (format "%s\n" file))
      (let* ((path (file-name-directory file))
             (matches (split-string
                       (shell-command-to-string
                        (format "find \"%s\" -wholename \"%s*\"" path file)) "\n" t)))

        (dolist (match (reverse (cdr (reverse matches))))
          (setq cnt (+ 1 cnt))
          (let ((file-size  (file-attribute-size (file-attributes match))))
            (when file-size
              (setq size (+ size file-size))))
          (princ (format " > rm %s\n" match))
          (delete-file match))))
    (princ (format "Total removed: %s (%d files)\n"
                   (file-size-human-readable size) cnt))))
