;;; regulator.el --- -*- lexical-binding: t; -*-
(require 'cl-lib)

(defvar e24
  "E24 5% Tolerance Resistors"
  (list 1.0 1.1 1.2 1.3 1.5 1.6 1.8 2.0 2.2 2.4 2.7 3.0
        3.3 3.6 3.9 4.3 4.7 5.1 5.6 6.2 6.8 7.5 8.2 9.1)
  )

(defvar e48
  "E48 2% Tolerance Resistors"
  (list 1.00 1.05 1.10 1.15 1.21 1.27 1.33 1.40 1.47 1.54 1.62 1.69 1.78 1.87 1.96 2.05 2.15 2.26
        2.37 2.49 2.61 2.74 2.87 3.01 3.16 3.32 3.48 3.65 3.83 4.02 4.22 4.42 4.64 4.87 5.11 5.36 5.62
        5.90 6.19 6.49 6.81 7.15 7.50 7.87 8.25 8.66 9.09 9.53)
  )

(defvar e96
  "E96 1% tolerance resistors"
  (list 1.00 1.02 1.05 1.07 1.10 1.13 1.15 1.18 1.21 1.24 1.27 1.30 1.33 1.37 1.40 1.43 1.47 1.50
        1.54 1.58 1.62 1.65 1.69 1.74 1.78 1.82 1.87 1.91 1.96 2.00 2.05 2.10 2.15 2.21 2.26 2.32 2.37
        2.43 2.49 2.55 2.61 2.67 2.74 2.80 2.87 2.94 3.01 3.09 3.16 3.24 3.32 3.40 3.48 3.57 3.65 3.74
        3.83 3.92 4.02 4.12 4.22 4.32 4.42 4.53 4.64 4.75 4.87 4.99 5.11 5.23 5.36 5.49 5.62 5.76 5.90
        6.04 6.19 6.34 6.49 6.65 6.81 6.98 7.15 7.32 7.50 7.68 7.87 8.06 8.25 8.45 8.66 8.87 9.09 9.31
        9.53 9.76)
  )

(defvar e192
  "E192 <0.5% tolerance resistors"
  (list 1.00 1.01 1.02 1.04 1.05 1.06 1.07 1.09 1.10 1.11 1.13 1.14 1.15 1.17 1.18 1.20 1.21 1.23
        1.24 1.26 1.27 1.29 1.30 1.32 1.33 1.35 1.37 1.38 1.40 1.42 1.43 1.45 1.47 1.49 1.50 1.52 1.54
        1.56 1.58 1.60 1.62 1.64 1.65 1.67 1.69 1.72 1.74 1.76 1.78 1.80 1.82 1.84 1.87 1.89 1.91 1.93
        1.96 1.98 2.00 2.03 2.05 2.08 2.10 2.13 2.15 2.18 2.21 2.23 2.26 2.29 2.32 2.34 2.37 2.40 2.43
        2.46 2.49 2.52 2.55 2.58 2.61 2.64 2.67 2.71 2.74 2.77 2.80 2.84 2.87 2.91 2.94 2.98 3.01 3.05
        3.09 3.12 3.16 3.20 3.24 3.28 3.32 3.36 3.40 3.44 3.48 3.52 3.57 3.61 3.65 3.70 3.74 3.79 3.83
        3.88 3.92 3.97 4.02 4.07 4.12 4.17 4.22 4.27 4.32 4.37 4.42 4.48 4.53 4.59 4.64 4.70 4.75 4.81
        4.87 4.93 4.99 5.05 5.11 5.17 5.23 5.30 5.36 5.42 5.49 5.56 5.62 5.69 5.76 5.83 5.90 5.97 6.04
        6.12 6.19 6.26 6.34 6.42 6.49 6.57 6.65 6.73 6.81 6.90 6.98 7.06 7.15 7.23 7.32 7.41 7.50 7.59
        7.68 7.77 7.87 7.96 8.06 8.16 8.25 8.35 8.45 8.56 8.66 8.76 8.87 8.98 9.09 9.20 9.31 9.42 9.53
        9.65 9.76 9.88)
  )

(defun regulator-format-resistor (value)
  (cond ((>= value 1000000) (format "%gM" (/ value 1000000.0)))
        ((>= value 1000) (format "%gk" (/ value 1000.0)))
        (t (format "%d" value))
        ))

(defun find-regulator-divider ()
  "Find resistors for a voltage regulator"
  (interactive)
  (let ((series (symbol-value (intern (completing-read "series: " '("e24" "e48" "e96" "e192")))))
        (vref (string-to-number (read-string "vref: " "0.5")))
        (v-goal (string-to-number (read-string "voltage: " "3.33")))
        (divider "|-----+------+---------+----------|\n"))
    ;; function to calculate the output voltage of a regulator
    (with-output-to-temp-buffer "regulator"
      (princ divider)
      (princ (format "| r1 | r2 | voltage | %% error |\n"))
      (princ divider)
      (cl-flet
          ((output-voltage (r1 r2 vref) (* vref (+ 1 (/ r2 r1))))
           (percent-error (truth measured) (* 100 (/ (abs (- truth measured)) truth))))
        ;; create multiples of the E* series
        (let ((r (append (mapcar (lambda (x) (* x 1000)) series)
                         (mapcar (lambda (x) (* x 10000)) series)))
              (min-err 100.0))
          ;; loop over all values of the resistors and minimize
          (dolist (r1 r)
            (dolist (r2 r)
              (let* (
                     (v-out (output-voltage r1 r2 vref))
                     (err (percent-error v-goal v-out)))
                (when (<= err min-err)
                  (setf min-err err)
                  (when (<= err 5)
                    (princ (format "| %s | %s | %g | %2.3f%% |\n"
                                   (regulator-format-resistor r1)
                                   (regulator-format-resistor r2)
                                   v-out
                                   err)))))))))
    (princ divider))

  (with-current-buffer "regulator"
    (read-only-mode -1)
    (org-mode)
    (org-table-align)))
t)
