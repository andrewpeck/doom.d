;;; timesheet.el --- Helpers to turn org mode into a timetracking software -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020-2025 Andrew Peck

;; Author: Andrew Peck <git@andrewepeck.xyz>
;; URL: https://github.com/andrewpeck/timesheet.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>
;; 
;;; Commentary:
;; 
;; Helpers to turn org mode into a timetracking software.
;;
;;; Code:

(require 'calendar)
(require 'cal-iso)
(require 'ob-ref)
(require 'org-table)
(require 'dash)

(defvar timesheet-substitutions nil
  "Alist of name substitutions to be made., e.g.

\\='((\"^ME0SF$\" . \"ME0\")
  (\"^ME0OH$\" . \"ME0\")
  (\"^ME0BE$\" . \"ME0\")
  (\"^VAC$\"   . \"VACATION\"))")

(defun timesheet--ymd-to-weekday (C Y m d)
  "Convert a date (C Y M D) into day of week.

For date 2025/12/07:

C = Century, e.g. 20
Y = year, e.g. 25
m = month, e.g. 12
d = day, e.g. 7

W = ( d + floor (2.6m - 0.2) - 2C + Y + floor(Y/4) + floor (C/4) ) mod 7
https://cs.uwaterloo.ca/~alopez-o/math-faq/node73.html
k is day (1 to 31)
m is month (1 = March, ..., 10 = December, 11 = Jan, 12 = Feb)
  Treat Jan & Feb as months of the preceding year
C is century (1987 has C = 19)
Y is year (1987 has Y = 87 except Y = 86 for Jan & Feb)
W is week day (0 = Sunday, ..., 6 = Saturday)"

  (declare (pure t) (side-effect-free error-free))

  ;; (1 = March ,\... ,10 = December ,11 = Jan ,12 = Feb)
  ;; Treat Jan & Feb as months of the preceding year
  (if (< m 3)
      (progn (setf Y (- Y 1))
             (setf m (+ m 10)))
    (setf m (- m 2)))

  ;; Return the day of the week
  (mod (+ d
          (floor (- (* m 2.6) 0.2))
          (- (* 2 C))
          Y
          (floor (/ Y 4.0))
          (floor (/ C 4.0))) 7))

(defun timesheet--weekday-to-abbr (d)
  "Return an abbreviated day of the week from an integer D.
0=SUN, 1=MON, etc"
  (declare (pure t) (side-effect-free error-free))
  (aref ["SUN" "MON" "TUE" "WED" "THU" "FRI" "SAT"] d))

(defun timesheet--month-to-number (month)
  "Convert a text MONTH to an integer."
  (declare (pure t) (side-effect-free error-free))
  (pcase month
    ("January"   1)
    ("February"  2)
    ("March"     3)
    ("April"     4)
    ("May"       5)
    ("June"      6)
    ("July"      7)
    ("August"    8)
    ("September" 9)
    ("October"   10)
    ("November"  11)
    ("December"  12)
    (_ (user-error (format "Invalid date %s" month)))))

;;;###autoload
(defun timesheet-get-day-of-week (year month day)
  "Return day of week as a str from a TITLE and DAY-OF-MONTH."
  (declare (pure t) (side-effect-free error-free))
  (let* ((y (mod year 100))
         (c (/ year 100)))
    (if (string= day "") " "
      (timesheet--weekday-to-abbr
       (timesheet--ymd-to-weekday c
                                  y
                                  month
                                  (string-to-number day))))))

(defun timesheet--clock-to-float (time)
  "Convert a clock TIME (e.g. 12:30) to a float (e.g. 12.5)."
  (declare (pure t) (side-effect-free error-free))
  (let* ((split (split-string time ":" t))
         (hours (string-to-number (car split)))
         (minutes 0))
    (when (cadr split)
      (setf minutes (string-to-number (cadr split))))
    (print (+ hours (/ minutes 60.0)))))

;;;###autoload
(defun timesheet-range-to-time (range)
  "Convert a 12hr clock time RANGE time (e.g. 1-2:30)
to a float amount of time (1.5)."
  (declare (pure t) (side-effect-free t))
  (if (string-empty-p range) ""
    (let ((start (timesheet--clock-to-float (car (split-string range "-" t))))
          (end (timesheet--clock-to-float (cadr (split-string range "-" t)))))
      (when (> start end)
        (setf end (+ 12 end)))
      (- end start))))

;;;###autoload
(defun timesheet-update-all-histograms ()
  "Update all timesheet histograms blocks."
  (save-excursion
    (goto-char (point-min))
    (while
        (re-search-forward "#\\+begin_src emacs-lisp.*data=[0-9]\\{4\\}-[0-9]\\{2\\}$" nil t)
      (forward-line)
      (org-babel-execute-src-block))))

(cl-defun timesheet--bin-data (data &key normalize sort)
  ""
  (let ((totals ())
        (ht (make-hash-table :test 'equal))
        (max-val 0))

    (debug)

    (dolist (row data 't)
      (let ((key (nth 0 row))
            (val (nth 1 row)))
        (when (and (numberp val)
                   (> val 0))
          (puthash key (+ val (gethash key ht 0)) ht))))

    (maphash (lambda (key value)
               (setq totals (cons (list key value) totals))) ht)

    ;; sort in increasing date order
    (when sort
      (setq totals (sort totals (lambda (a b) (funcall sort (cadr a) (cadr b))))))

    ;; find max value for normalization
    (setq max-val (apply #'max (mapcar (lambda (a) (float (car (cdr a)))) totals)))

    (when normalize
      (setq totals (mapcar  (lambda (x) (list (nth 0 x) (/ (nth 1 x) max-val))) totals)))

    totals))

;;;###autoload
(cl-defun timesheet-plot-chart (data keyword valword &key normalize sort uplot title)
  "Simple function to plot an ascii bar chart from DATA.

It accepts DATA in the form of an alist of the form
\='((KEY . VAL) (KEY . VAL) (KEY . VAL)) and will
produce a bar chart where for each key val is summed.

NORMALIZE will normalize the bar chart to some number of ASCII symbols.

SORT to non-nill will sort the list. "

  (let ((totals ())
        (ht (make-hash-table :test 'equal))
        (sum 0)
        (max-length 0)
        (max-val 0))

    (dolist (row data 't)
      (let ((key (nth 0 row))
            (val (nth 1 row)))
        (when (and (numberp val)
                   (not (equal key "--"))
                   (> val 0))
          (puthash key (+ val (gethash key ht 0)) ht))))

    ;; Call FUNCTION for all entries in hash table TABLE.
    (maphash (lambda (k v) (setq totals (cons (list k v) totals))) ht)

    ;; sort in increasing date order
    (when sort
      (setq totals (sort totals (lambda (a b) (< (cadr a) (cadr b))))))

    ;; find max value for normalization
    (setq max-val (apply #'max (mapcar (lambda (a) (float (car (cdr a)))) totals)))

    ;; Get the total for percentages
    (setq sum (apply #'+ (mapcar (lambda (a) (float (car (cdr a)))) totals)))

    ;; Get the longest key so we can zero pad accordingly
    (setq max-length
          (max (length keyword)
               (apply #'max (mapcar
                             (lambda (a) (length (car a))) totals))))

    ;; Get the number of decimal digits needed, if not specified
    (if (< max-val normalize) (setq normalize max-val))

    (when (not uplot)
      (princ (format "%s  %4s    %s%s\n" valword "%Tot"
                     (make-string (- max-length (length keyword)) ? ) keyword))

      (princ (format "%s\n" (make-string 32 ?-)))
      (dolist (item totals)
        (let* ((key (car item))
               (count (cadr item))
               (count-normal (if normalize (* normalize (/ count max-val)) count))
               (percent (* 100 (/ count sum))))


          (princ (format "%5.2f   %2d%%     %s%s %s\n"
                         count        ; count
                         percent      ; percent
                         (make-string (- max-length (length key)) ? ) ; white paddng
                         key                                          ; key
                         (make-string (round count-normal) ?+)))))    ; +++++
      (princ (format "%s\n" (make-string 32 ?-)))
      (princ (format "%6.2f\n" sum)))

    (when uplot
      (princ
       (concat
        (shell-command-to-string
         (concat
          "echo \""
          (apply #'concat
                 (mapcar (lambda (x) (format "%s, %f\n" (car x) (cadr x)) ) totals))
          ;; HACK until nix youplot is fixed
          (format
           "\" | uplot bar -d, -t \"%s\" 2>&1 | tail -n +4" title))) "\n")))))

;;;###autoload
(defun timesheet-filter-work-chart (data)
  ""
  ;; remove the header
  (let* ((data (cdr data))

        ;;  remove empty lines (e.g. ("" 0))
        (data
         (cl-remove-if
          (lambda (a) (string= "" (car a)))
          data))

        ;; Filter the data from an entire table to just a list of
        ;; ( (PROJECT + HOURS) x N )
        ;; e.g. (("ETL" 6.5) ("ADMIN" 0.5) ("" 0))
        (data-filtered
         (mapcar
          (lambda (x)
            (let* ((hours (nth 6 x))
                   (hours (if (stringp hours)
                              (string-to-number hours) hours))
                   (project (upcase (nth 3 x)))
                   (project (seq-reduce
                             (lambda (str subs)
                               (replace-regexp-in-string
                                (car subs)
                                (cdr subs) str))
                             timesheet-substitutions project)))
              (list project hours))) data)))
    data-filtered))

(defun timesheet--week-number (month day year)
  "Convert MONTH/DAY/YEAR to week number."
  (car (calendar-iso-from-absolute
        (calendar-absolute-from-gregorian
         (list month day year)))))

(defun timesheet--iso-week-to-timestamp (year week day)
  "Convert YEAR WEEK DAY to a timestamp."
  (pcase-let ((`(,m ,d ,y)
               (calendar-gregorian-from-absolute
                (calendar-iso-to-absolute (list week day year)))))
    (encode-time 0 0 0 d m y)))

;;;###autoload
(defun timesheet-plot-weekly-work-goals (data)
  ""
  (let ((sums (make-hash-table :test 'equal))
        (keys '()))

    (dolist (datum data)

      (let* ((timesheet (nth 0 datum))
             (year (nth 1 datum))
             (month (nth 2 datum)))

        (dolist (row timesheet 't)
          (let* ((day (nth 1 row))
                 (prj (nth 3 row))
                 (hours (nth 6 row))
                 (week (if (numberp day) (timesheet--week-number month day year) -1)))

            (when (and (numberp day)
                       (numberp hours)
                       (not (equal prj "--"))
                       (> hours 0))
              ;; (princ (format "%04d-%02d-%02d\n" year month day))
              (let ((key (format "%04d-%02d" year week)))
                (puthash key (+ hours (gethash key sums 0)) sums)
                (push key keys)))))))

    (dolist (key (reverse keys))
      (let* ((annual-goal 1200)
             (holidays 15)
             (sickdays 15)
             (vacdays 20)
             (weekly-goal (/ (+ annual-goal
                                (* 8 sickdays)
                                (* 8 holidays)
                                (* 8 vacdays)) 52))
             (sum (gethash key sums 0))
             (year (string-to-number (nth 0 (split-string key "-"))))
             (week (string-to-number (nth 1 (split-string key "-"))))
             (goal (if (or (>= year 2023)
                           (and (>= year 2022)
                                (>= week 48)))
                       24 40))
             (percentage (* 100 (/ sum goal)))
             (indicator-count-total 20)
             (indicator-count (round (* indicator-count-total
                                        (/ percentage 100.0))))
             ;; (indicator-count 4)
             (indicator (make-string indicator-count ?+))
             (pad-count (- indicator-count-total indicator-count 1))
             (pad (if (< pad-count 0) ""
                    (concat  (make-string pad-count ? ) "|")))
             (difference (- sum goal))
             (time (timesheet--iso-week-to-timestamp year week 1))
             (month (string-to-number (format-time-string "%m" time))))

        (princ (concat
                (format "%-65s"
                        (format "Week %04d-%02d-%02d: %5.1f%% (%2d hours) %s%s"
                                year month week percentage sum indicator pad))
                (format "(%d)\n" difference)))))))

;;;###autoload
(defun timesheet-plot-weekly-goals-for-range (start-year start-month end-year end-month)
  ""
  (let* ((year start-year)
         (month start-month)
         (timesheets nil))

    (while (not (and (= month end-month)
                     (= year end-year)))
      (push (list
             (cl-remove-if
              (lambda (x) (equal x 'hline))
              (cdr (org-babel-ref-resolve
                    (format "%04d-%02d" year month)))) year month)
            timesheets)

      (if (= month 12)
          (setq month 1
                year (1+ year))
        (setq month (1+ month))))

    (timesheet-plot-weekly-work-goals (reverse timesheets))))

;;;###autoload
(cl-defun timesheet-plot-monthly (data &key uplot title)
  ""
  (timesheet-plot-chart (timesheet-filter-work-chart data) "Project" "Hours"
              :normalize 50 :sort t :uplot uplot :title (if title title "Monthly Work Distribution")))

;;;###autoload
(defun timesheet-range-to-year-month (start-year start-month end-year end-month)
  ""
  (let* ((end-tag (+ end-month (* 12 end-year)))
         (start-tag (+ start-month (* 12 start-year)))
         (difference (- end-tag start-tag))
         (month-sequence (number-sequence 0 difference)))
    (mapcar (lambda (x) (list (+ start-year (floor (+ x (- start-month 1)) 12))
                              (1+ (mod (+ x (- start-month 1)) 12))))
            month-sequence)))

;;;###autoload
(defun timesheet-add-yyyy-mm-to-table (data year month)
  ""
  (mapcar
   (lambda (x)
     (if (listp x)
         (progn
           (let ((day (nth 1 x)))
             (setf (nth 1 x)
                   (cond ((integerp day)
                          (format "%4d-%02d-%02d" year month day))
                         ((and (stringp day)
                               (> (string-to-number day) 0))
                          (format "%4d-%02d-%02d"
                                  (string-to-number year)
                                  (string-to-number month)
                                  (string-to-number day)))
                         (t day))))
           x)
       x))
   data))


;;;###autoload
(cl-defun timesheet-get-work-data-in-date-range (title &key
start-year start-month end-year end-month
short meetings meetings-detailed hlines projects)
  ""
  (message title)
  (let ((org-table-data ()))

    ;; defaults for start/end time to min/max
    (when (and (not start-year) (not start-month))
      (setq start-year 2021)
      (setq start-month 01))

    (when (and (not end-year) (not end-month))
      (setq end-year (nth 5 (decode-time (current-time)))) ;; current year
      (setq end-month (nth 4 (decode-time (current-time))))) ;; current month

    ;; gather all of the tables into one list
    (save-excursion

      ;;  make a list of all year-months and iterate over it
      (dolist (table
               (mapcar (lambda (x) (format "%04d-%02d" (car x) (cadr x)))
                       (timesheet-range-to-year-month start-year start-month end-year end-month)))
        (goto-char (point-min))
        (search-forward (concat "#+" "TBLNAME: " table))

        ;;  gather an org table
        (let ((month-table-data
               (org-table-to-lisp
                (buffer-substring-no-properties (org-table-begin)
                                                (org-table-end)))))

          ;;  concat all the tables together
          (setq org-table-data
                ;;  append the yyyy-mm to the day
                ;; month-table-data
                (append org-table-data
                        (timesheet-add-yyyy-mm-to-table month-table-data
                                              (car (split-string table "-"))
                                              (cadr (split-string table "-"))))))))


    ;; remove hlines and the first line (heading)
    (when (not hlines)
      (setq org-table-data
            (cl-remove-if (lambda (a) (equal 'hline a))
                          org-table-data)))

    ;; remove empty items and non-entered -- items
    (setq org-table-data
          (cl-remove-if
           (lambda (a)
             (and (listp a)
                  (or (string= "--" (nth 3 a))
                      (string= "" (nth 3 a))
                      (string= "TIME" (upcase (nth 2 a))))))
           org-table-data))

    ;; if projects, filter out all non-matching entries
    (when projects
      (setq org-table-data
            (cl-remove-if-not
             (lambda (a)
               (member (upcase (nth 3 a)) (mapcar #'upcase projects)))
             org-table-data)))

    ;; filter out all of the header lines except the first; convert those to hlines
    (when hlines
      (let ((header (cadr org-table-data)))
        (print header)
        (setq org-table-data
              (mapcar
               (lambda (x)
                 (if (listp x)
                     (if (string= (nth 1 x) "D") nil x)
                   x))
               (cdr org-table-data)))
        (push header org-table-data)
        (push 'hline org-table-data)))


    ;; if short, filter out non-billable items
    (when short
      (princ "\n\n")
      (setq org-table-data
            (cl-remove-if
             (lambda (a)
               (member (upcase (nth 3 a)) '("VACATION" "HOLIDAY" "SICK" "ADMIN" "DEVEL" "VAC")))
             org-table-data)))

    ;; if meetings, filter into meeting vs. real work categories
    (when meetings
      (let ((meetings-keywords
             '("MEET" "ACCRUALS" "INDARA" "EMAIL" "E-MAIL"
               "CALL" "OZGUR" "SLIDES" "CHLOE"
               "MOCKUP" "CHAT" "PDR" "REVIEW" "CHRIS +"
               "MANAGER" "REQUIREMENTS DOC" "TALK")))
        (setq org-table-data
              (mapcar
               (lambda (a)

                 (let ((cmd (nth 0 a))
                       (day (nth 1 a))
                       (hours (nth 2 a))
                       (project (nth 3 a))
                       (task (nth 4 a))
                       (weekday (nth 5 a))
                       (hours (nth 6 a)))
                   (cond

                    ;; is a meeting
                    ((cl-some (lambda (x) x)
                              (mapcar
                               (lambda (x)
                                 (string-match-p x (upcase task))) meetings-keywords))
                     (list cmd day hours
                           (if meetings-detailed (concat project " - Meeting") "Meeting")
                           task weekday hours))

                    ;; is not a meeting
                    (t
                     (list cmd day hours
                           (if meetings-detailed (concat task " - Real Work") "Real Work")
                           task weekday hours)))))

               org-table-data))))

    ;; (pp org-table-data)
    org-table-data))


;;;###autoload
(cl-defun timesheet-plot-work-chart-in-date-range
    (title &key
           start-year start-month
           end-year end-month
           short
           meetings meetings-detailed)
  (timesheet-plot-monthly
   (timesheet-get-work-data-in-date-range
    title
    :start-year start-year :start-month start-month :end-year end-year :end-month end-month
    :short short :meetings meetings :meetings-detailed meetings-detailed) :title title :uplot t))

;;;###autoload
(defun timesheet-plot-weekly-summary-for-month (year month)
  ""

  (let* ((start-year (if (= month 1) (- year 1) year))
         (end-year (if (= month 12) (+ year 1) year))
         (start-month (if (= month 1) 12 (- month 1)))
         (end-month (cond
                     ;; december should wrap to jan
                     ((= month 12) 1)
                     ;;  current month
                     ((= month (nth 4 (decode-time (current-time)))) (+ 1 month))
                     ;; any other month
                     (t (+ month 2)))))

    (-as-> (with-output-to-string
             (timesheet-plot-weekly-goals-for-range start-year start-month end-year end-month)) data
             (split-string data "\n" t)
             (cl-remove-if-not (lambda (x) (string-search (format "%04d-%02d" year month) x)) data)
             (string-join data "\n")
             (format "\n%s\n\n" data)
             (princ data))))

;;;###autoload
(defun timesheet-filter-timesheet-for-hours (data)
  ""
  (let ((plot-data
         (cdr (mapcar
               (lambda (x)
                 (let* ((hours (string-to-number (nth 6 x)))
                        (yyyy-mm-dd (split-string (nth 1 x) "-"))
                        (yyyy (nth 0 yyyy-mm-dd))
                        (mm   (nth 1 yyyy-mm-dd))
                        (datestr (concat yyyy "-" mm)))
                   (list datestr hours)))
               data))))

    (setq plot-data
          (cl-remove-if
           (lambda (x) (string= "-TOTAL" (car x))) plot-data))
    plot-data))

;;;###autoload
(defun timesheet-plot-monthly-histogram (title data)
  ""
  (timesheet-plot-chart (timesheet-filter-timesheet-for-hours data) "Month" title
              :normalize 50 :uplot t :title title)
  (princ "\n"))

;;;###autoload
(defun timesheet-insert-monthly ()
  "Insert a new timesheet for the current month."
  (interactive)
  (let* ((time (current-time))
         (month (format-time-string "%B" time))
         (mm (format-time-string "%m" time))
         (year (format-time-string "%Y" time)))

    (insert
     (concat
      ;; (s-lex-format)
      (format "*** %s %s\n" month year)
      ":PROPERTIES:\n"
      ":VISIBILITY: showall\n"
      ":END:\n"
      "#+ATTR_HTML: :border 2 :frame none\n"
      "\n"
      (format  "#+TBLNAME: %s-%s\n" year mm)
      "|---+---+----------+---------+--------------------+-----+-------|\n"
      "|   | D |     Time | Project | Task               | Day | Hours |\n"
      "|---+---+----------+---------+--------------------+-----+-------|\n"
      "| # |   |          |         |                    |     |       |\n"
      "|---+---+----------+---------+--------------------+-----+-------|\n"
      (format  "#+TBLFM: $6='(timesheet-get-day-of-week %s %s $2)::$7='(timesheet-range-to-time $3)\n" year mm)
      "\n"
      (format  "#+begin_src emacs-lisp :exports results :results output :var data=%s-%s\n" year mm)
      "(timesheet-plot-monthly data :uplot t)\n"
      (format "(timesheet-plot-weekly-summary-for-month %s %s)\n" year mm)
      "#+end_src\n"))))

(provide 'timesheet)
;;; timesheet.el ends here

;; Local Variables:
;; eval: (make-variable-buffer-local 'write-contents-functions)
;; eval: (add-hook 'write-contents-functions (lambda () (loaddefs-generate "~/.doom.d/my-autoloads/" "~/.doom.d/loaddefs.el")))
;; End:
