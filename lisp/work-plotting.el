;;; -plotting.el -*- lexical-binding: t; -*-

;;------------------------------------------------------------------------------
;; Plotting
;;------------------------------------------------------------------------------

(cl-defun bin-data (data &key normalize sort)
  (let ((totals ())
        (ht (make-hash-table :test 'equal)))

    (dolist (row data 't)
      (let ((key (nth 0 row))
            (val (nth 1 row)))
        (when (and (> val 0) (numberp val))
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

(defun plot-chart (data keyword valword &optional normalize sort)
  "Simple function to plot an ascii bar chart.

It accepts DATA in the form of an alist of the form '((KEY . VAL) (KEY . VAL) (KEY . VAL)) and will
produce a bar chart where for each key val is summed.

NORMALIZE will normalize the bar chart to some number of ASCII symbols.

SORT to non-nill will sort the list. "

  (let ((totals ())
        (ht (make-hash-table :test 'equal)))

    (dolist (row data 't)
      (let ((key (nth 0 row))
            (val (nth 1 row)))
        (when (and (> val 0) (numberp val))
          (puthash key (+ val (gethash key ht 0)) ht))))

    (maphash (lambda (key value)
               (setq totals (cons (list key value) totals))) ht)

    ;; sort in increasing date order
    (when sort
      (setq totals (sort totals (lambda (a b) (< (cadr a) (cadr b))))))

    ;; find max value for normalization
    (setq max-val (apply #'max (mapcar (lambda (a) (float (car (cdr a)))) totals)))

    ;; Get the total for percentages
    (setq sum (apply #'+ (mapcar (lambda (a) (float (car (cdr a)))) totals)))

    ;; Get the longest key so we can zero pad accordingly
    (setq max-length (max (length keyword)
                          (apply #'max (mapcar
                                        (lambda (a) (length (car a))) totals))))

    ;; Get the number of decimal digits needed, if not specified

    ;;

    (if (< max-val normalize) (setq normalize max-val))

    (princ (format "%5s   %4s    %s%s\n" valword "%Tot"
                   (make-string (- max-length (length keyword)) ? ) keyword))

    (princ (format "%s\n" (make-string 32 ?-)))
    (dolist (item totals)
      (let* ((key (car item))
             (count (cadr item))
             (count-normal (if normalize (* normalize (/ count max-val)) count))
             (percent (* 100 (/ count sum))))
        (when (not (equal key "--"))
          (princ (format "%7.2f   %2d%%     %s%s %s\n"
                         count                                        ; count
                         percent                                      ; percent
                         (make-string (- max-length (length key)) ? ) ; white paddng
                         key                                          ; key
                         (make-string (round count-normal) ?+))))))   ; +++++
    (princ (format "%s\n" (make-string 32 ?-)))
    (princ (format "%6.2f\n" sum))))

(defun filter-work-chart (data)
  (cl-remove-if
   (lambda (a) (string= "" (car a)))
   (cdr (mapcar
         (lambda (x)
           (list
            (replace-regexp-in-string
             "^VAC$" "VACATION"
             (upcase (nth 3 x )))         ; project
            (if (stringp (nth 6 x))
                (string-to-number (nth 6 x)) (nth 6 x)))) data))))

(defun plot-monthly-work-chart (data)
  (plot-chart
   (filter-work-chart data) "Project" "Hours" 50 t) )

(defun date-range-to-year-month-list (start-year start-month end-year end-month)
  (let* ((end-tag (+ end-month (* 12 end-year)))
         (start-tag (+ start-month (* 12 start-year)))
         (difference (- end-tag start-tag))
         (month-sequence (number-sequence 0 difference)))
    (mapcar (lambda (x) (list (+ start-year (floor (+ x (- start-month 1)) 12))
                              (1+ (mod (+ x (- start-month 1)) 12))
                              )) month-sequence)))

(cl-defun plot-work-chart-in-date-range
    (title &key start-year start-month end-year end-month short meetings meetings-detailed)
  (plot-monthly-work-chart
   (get-work-data-in-date-range
    title
    :start-year start-year :start-month start-month :end-year end-year :end-month end-month
    :short short :meetings meetings :meetings-detailed meetings-detailed)))

(defun add-yyyy-mm-to-table (data year month)
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
                                  (string-to-number day)
                                  ))
                         (t day)))) x)
       x)) data))

(cl-defun get-work-data-in-date-range
    (title &key
           start-year start-month end-year end-month
           short meetings meetings-detailed hlines projects)
  (let ((org-table-data ()))

    ;; defaults for start/end time to min/max
    (when (and (not start-year) (not start-month))
      (setq start-year 2021)
      (setq start-month 01))

    (when (and (not end-year) (not end-month))
      (setq end-year (nth 5 (decode-time (current-time)))) ;; current year
      (setq end-month (nth 4 (decode-time (current-time))))) ;; current month

    ;; (princ "---------------------------------------------------------------------------\n")
    ;; (princ (format "%s\n" title))
    ;; (princ "---------------------------------------------------------------------------\n")

    ;; gather all of the tables into one list
    (save-excursion

      ;;  make a list of all year-months and iterate over it
      (dolist (table
               (mapcar (lambda (x) (format "%04d-%02d" (car x) (cadr x)))
                       (date-range-to-year-month-list start-year start-month end-year end-month)))
        (goto-char (point-min))
        (search-forward (concat "#+" "TBLNAME: " table))

        ;;  gather an org table
        (let ((month-table-data
               (org-table-to-lisp
                (buffer-substring-no-properties (org-table-begin) (org-table-end)))))

          ;;  concat all the tables together
          (setq org-table-data
                ;;  append the yyyy-mm to the day
                ;; month-table-data
                (append org-table-data
                        (add-yyyy-mm-to-table month-table-data
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
                      (string= "TIME" (upcase (nth 2 a)))
                      )))
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
      (setq header (cadr org-table-data))
      (print header)
      (setq org-table-data
            (mapcar
             (lambda (x)
               (if (listp x)
                   (if (string= (nth 1 x) "D") nil x)
                 x))
             (cdr org-table-data)))
      (push header org-table-data)
      (push 'hline org-table-data))


    ;; if short, filter out non-billable items
    (when short
      (princ "\n\n")
      (setq org-table-data
            (cl-remove-if
             (lambda (a)
               (member (upcase (nth 3 a)) '("VACATION" "HOLIDAY" "SICK" "ADMIN" "DEVEL")))
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

(cl-defun filter-timesheet-for-hours (data)
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

(cl-defun plot-monthly-histogram (title data)
  (plot-chart (filter-timesheet-for-hours data) "Month" title 50 nil))

;; (plot-gaps-timesheet "Hours")
;; (princ "\n\n")
;; (plot-gaps-timesheet "Dollars" 45)
;; #+end_src

;; ((or (string-match-p (regexp-quote "PDR") (upcase (nth 4 a)))
;;      (string-match-p (regexp-quote "REVIEW") (upcase (nth 4 a))))
;;  (list (nth 0 a)  ; #
;;        (nth 1 a)  ; day
;;        (nth 2 a)  ; hours
;;        (if meetings-detailed
;;            (concat (nth 3 a)
;;                    " - Review")
;;            "Review"
;;          )  ; project
;;        (nth 4 a)  ; task
;;        (nth 5 a)  ; day
;;        (nth 6 a)))
