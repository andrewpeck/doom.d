;;; -plotting.el -*- lexical-binding: t; -*-

;;------------------------------------------------------------------------------
;; Plotting
;;------------------------------------------------------------------------------

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
    (title start-year start-month end-year end-month
           &key short meetings meetings-detailed)
  (plot-monthly-work-chart
   (get-work-data-in-date-range
    title start-year start-month end-year end-month
    :short short :meetings meetings :meetings-detailed meetings-detailed)))

(cl-defun get-work-data-in-date-range
    (title start-year start-month end-year end-month
           &key short meetings meetings-detailed)
  (let ((org-table-data ()))

    (princ "---------------------------------------------------------------------------\n")
    (princ (format "%s\n" title))
    (princ "---------------------------------------------------------------------------\n")

    ;; gather all of the tables into one list
    (save-excursion
      (dolist (table
               (mapcar (lambda (x) (format "%04d-%02d" (car x) (cadr x)))
                       (date-range-to-year-month-list start-year start-month end-year end-month)))
        (goto-char (point-min))
        (search-forward (concat "#+" "TBLNAME: " table))
        (setq org-table-data
              (append org-table-data
                      (org-table-to-lisp
                       (buffer-substring-no-properties
                        (org-table-begin) (org-table-end)))))))

    ;; remove hlines and the first line (heading)
    (setq org-table-data
          (cl-remove-if (lambda (a) (equal 'hline a))
                        org-table-data))

    ;; (print org-table-data)

    (setq org-table-data
          (cl-remove-if
           (lambda (a)
             (or
              (string= "--" (upcase (nth 3 a)))
              (string= "" (upcase (nth 3 a))))
             )
           org-table-data))

    ;; if short, filter out non-billable items
    (when short
      (princ "\n\n")
      (setq org-table-data
            (cl-remove-if
             (lambda (a)
               (member (upcase (nth 3 a)) '("VACATION" "HOLIDAY" "SICK" "ADMIN" "DEVEL")))
             org-table-data)))

    (when meetings
      (setq org-table-data
            (mapcar
             (lambda (a)
               (cond  ((or (string-match-p (regexp-quote "MEET") (upcase (nth 4 a)))
                           (string-match-p (regexp-quote "ACCRUALS") (upcase (nth 4 a)))
                           (string-match-p (regexp-quote "INDARA") (upcase (nth 4 a)))
                           (string-match-p (regexp-quote "EMAIL") (upcase (nth 4 a)))
                           (string-match-p (regexp-quote "E-MAIL") (upcase (nth 4 a)))
                           (string-match-p (regexp-quote "CALL") (upcase (nth 4 a)))
                           (string-match-p (regexp-quote "OZGUR") (upcase (nth 4 a)))
                           (string-match-p (regexp-quote "SLIDES") (upcase (nth 4 a)))
                           ;; (string-match-p (regexp-quote "CHLOE") (upcase (nth 4 a)))
                           (string-match-p (regexp-quote "MOCKUP") (upcase (nth 4 a)))
                           (string-match-p (regexp-quote "CHAT") (upcase (nth 4 a)))
                           (string-match-p (regexp-quote "PDR") (upcase (nth 4 a)))
                           (string-match-p (regexp-quote "REVIEW") (upcase (nth 4 a)))
                           (string-match-p (regexp-quote "CHRIS +") (upcase (nth 4 a)))
                           (string-match-p (regexp-quote "MANAGER") (upcase (nth 4 a)))
                           (string-match-p (regexp-quote "REQUIREMENTS DOC") (upcase (nth 4 a)))
                           (string-match-p (regexp-quote "TALK") (upcase (nth 4 a)))
                           )
                       (list (nth 0 a)  ; #
                             (nth 1 a)  ; day
                             (nth 2 a)  ; hours
                             (if meetings-detailed
                                 (concat (nth 3 a) " - Meeting") ; project
                               "Meeting")                        ; project
                             (nth 4 a)                           ; task
                             (nth 5 a)                           ; day
                             (nth 6 a)))                         ; hours
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

                      (t
                       (list (nth 0 a)  ; #
                             (nth 1 a)  ; day
                             (nth 2 a)  ; hours
                             (if meetings-detailed
                                 (concat (nth 3 a)
                                         " - Real Work") ; project
                               "Real Work")
                             (nth 4 a)     ; task
                             (nth 5 a)     ; day
                             (nth 6 a))))) ; hours

             org-table-data)))
    ;; (pp org-table-data)

    org-table-data))
