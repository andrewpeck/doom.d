;;; ../.dotfiles/doom.d/lisp/tracking.el -*- lexical-binding: t; -*-

;; https://parceltracking.info/tracking-number-formats-by-carrier/
;; http://gummydev.com/regex/
;; https://stackoverflow.com/questions/619977/regular-expression-patterns-for-tracking-numbers
(require 'cl-lib)

(defun track-package ()
  "Track Fedex/UPS/USPS package number at point"
  (interactive)
  (track-package-by-number (current-word)) t)

(defun track-package-is-usps (tracking-number)
  (or (string-match "\\b\\(9[1234]\\)[0-9]\\{2\\} ?[0-9]\\{4\\} ?[0-9]\\{4\\} ?[0-9]\\{4\\} ?[0-9]\\{4\\} ?[0-9]\\{2\\}\\b" tracking-number)))

(defun track-package-is-ups (tracking-number)
  (or (string-match "\\b1[A-z][A-Z,0-9]\\{16\\}\\b" tracking-number) ;   1Z9999999999999999
      (string-match "\\bT[0-9]\\{10\\}\\b" tracking-number) ;   T9999999999
      (string-match "\\b[0-9]\\{9\\}\\b" tracking-number);   999999999
      (string-match "\\b[0-9]\\{12\\}\\b" tracking-number))) ;   999999999999

(defun track-package-is-fedex (tracking-number)
  (or (string-match "\\b\\(81\\|7[789]\\)[0-9]\\{10\\}\\b" tracking-number)))

(defun track-package-by-number (tracking-number)
  (cond
   ;; FEDEX
   ((track-package-is-fedex tracking-number)
    (track-fedex tracking-number))
   ;; UPS
   ((track-package-is-fedex tracking-number)
    (track-ups tracking-number))
   ;; USPS
   ((track-package-is-usps tracking-number)
    (track-usps tracking-number))))

(defun track-fedex (number)
  (let ((url (format "https://www.fedex.com/fedextrack/?tracknumbers=%s&action=track" number)))
    (browse-url url)))

(defun track-ups (number)
  (let ((url (format "https://wwwapps.ups.com/WebTracking/processInputRequest?TypeOfInquiryNumber=T&InquiryNumber1=%s" number)))
    (browse-url url)))

(defun track-usps (number)
  (let ((url (format "https://tools.usps.com/go/TrackConfirmAction_input?qtc_tLabels1=%s" number)))
    (browse-url url)))

;; TODO: convert to ERT?
(eval-when-compile
  ;; USPS:
  (cl-assert (track-package-is-usps "9374889692090270407075")) ; regular
  ;;(cl-assert (track-package-is-usps "70160910000108310009"))   ; certified
  ;;(cl-assert (track-package-is-usps "23153630000057728970"))   ; signature confirmation
  ;;(cl-assert (track-package-is-usps "RE360192014US"))          ; registered mail
  ;;(cl-assert (track-package-is-usps "EL595811950US"))          ; priority express

  ;; FEDEX:
  (cl-assert (track-package-is-fedex "810132562702")) ;  (all seem to follow same pattern regardless)
  (cl-assert (track-package-is-fedex "795223646324"))
  (cl-assert (track-package-is-fedex "785037759224"))
  (cl-assert (track-package-is-fedex "770826842738"))

  ;; UPS:
  ;;(cl-assert (track-package-is-ups "K2479825491"))      ; (UPS ground)
  ;;(cl-assert (track-package-is-ups "J4603636537"))      ; (UPS next day express)
  (cl-assert (track-package-is-ups "1Z87585E4391018698")) ; (regular)
  (cl-assert (track-package-is-ups "1Z0159190392069793"))
  (cl-assert (track-package-is-ups "1Z739R590322540520"))
  )

(provide 'tracking)
