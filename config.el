;; config.el -*- lexical-binding: t; -*-
;;
;; https://github.com/alphapapa/taxy.el
;; https://svn.red-bean.com/repos/kfogel/trunk/.emacs
;;
;; TODO tabular-like alignment
;; TODO magit.sh like functionality
;; TODO outline folding
;;
;; Tecosaur: https://github.com/tecosaur/emacs-config/blob/master/config.org
;; Steve Purcell: https://github.com/purcell/emacs.d/
;; Henrik: https://github.com/hlissner/doom-emacs-private/blob/master/config.el

;; Bookmarks

(setq bookmark-default-file "~/.doom.d/bookmarks")

(setq so-long-threshold 800)

;; start:sort
(load "~/.doom.d/config-align.el")
(load "~/.doom.d/config-appearance.el")
(load "~/.doom.d/config-company.el")
(load "~/.doom.d/config-doom.el")
(load "~/.doom.d/config-evil.el")
(load "~/.doom.d/config-git.el")
(load "~/.doom.d/config-lsp.el")
(load "~/.doom.d/config-modeline.el")
(load "~/.doom.d/config-org.el")
(load "~/.doom.d/config-random.el")
(load "~/.doom.d/config-scad.el")
(load "~/.doom.d/config-tex.el")
(load "~/.doom.d/lisp/doctor.el")
(load "~/.doom.d/lisp/hdl_deps/hdl_deps.el")
(load "~/.doom.d/lisp/hog-emacs/hog-emacs.el")
(load "~/.doom.d/lisp/regulator.el")
(load "~/.doom.d/lisp/system-install.el")
(load "~/.doom.d/lisp/tracking.el")
(load "~/.doom.d/lisp/ucf-mode.el")
(load "~/.doom.d/lisp/verilog-port-copy.el")
(load "~/.doom.d/lisp/vivado-mode.el")
;; end:sort

;;------------------------------------------------------------------------------
;; Prose lint
;;------------------------------------------------------------------------------

;; https://unconj.ca/blog/linting-prose-in-emacs.html
(flycheck-define-checker proselint
  "A linter for prose."
  :command ("proselint"
            ;;
            ;;            (option-flag "--external-sources" flycheck-shellcheck-follow-sources)
            "--config" (eval (expand-file-name "~/.doom.d/proselint.rc"))
            source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": "
            (id (one-or-more (not (any " "))))
            (message) line-end))
  :modes (text-mode latex-mode markdown-mode gfm-mode org-mode))

(add-to-list 'flycheck-checkers 'proselint)


;;------------------------------------------------------------------------------
;;; SLIME
;;------------------------------------------------------------------------------

;; slime
;;(after! slime
;;  (setq inferior-lisp-program "sbcl")
;;  (setq org-babel-lisp-eval-fn 'slime-eval))

;;------------------------------------------------------------------------------
;;; Spell-Checking
;;------------------------------------------------------------------------------

(after! writegood
  (writegood-passive-voice-turn-off))

;;; Snippets
;;------------------------------------------------------------------------------

;; Don't add newlines to snippet endings
(after! yasnippet
  (setq-default yas-also-auto-indent-first-line t)
  (add-hook 'snippet-mode-hook
            (lambda () (setq require-final-newline nil))))

;;------------------------------------------------------------------------------
;;; Mixed Pitch Mode
;;------------------------------------------------------------------------------

;; (add-hook 'org-mode-hook      #'mixed-pitch-mode)
;; (add-hook 'markdown-mode-hook #'mixed-pitch-mode)
;; (add-hook 'latex-mode-hook    #'mixed-pitch-mode)

;;------------------------------------------------------------------------------
;;; Line wrapping
;;------------------------------------------------------------------------------

(defun ap/no-wrap ()
  (interactive)
  (visual-line-mode 0)
  (toggle-truncate-lines 1)
  (visual-fill-column-mode 0))

;; Disable auto fill mode in text modes
(remove-hook 'text-mode-hook #'auto-fill-mode)

;; Don't wrap text modes unless we really want it
(remove-hook 'text-mode-hook #'+word-wrap-mode)

(defun fix-visual-fill-column-mode (&optional ARG)
  (setq visual-fill-column-mode visual-line-mode))

;; toggle visual-fill column mode when chaing word wrap settings
(advice-add '+word-wrap-mode
            :after 'fix-visual-fill-column-mode)
;;
(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)

;;------------------------------------------------------------------------------
;;; Hog
;;------------------------------------------------------------------------------

(after! hog-emacs
  (cond

   ((string= (system-name) "strange")
    (progn (setq hog-vivado-path "~/Xilinx/Vivado/2020.2/settings64.sh")
           (setq hog-number-of-jobs 16)))

   ((string= (system-name) "larry")
    (progn (setq hog-vivado-path "~/Xilinx/Vivado/2020.2/settings64.sh")
           (setq hog-number-of-jobs 4)))))

;;------------------------------------------------------------------------------
;;; VHDL Mode
;;------------------------------------------------------------------------------

;; vhdl mode will wrap comments after some # of characters
(setq vhdl-end-comment-column 200
      vhdl-prompt-for-comments nil
      auto-fill-mode nil)

;;-----------------------------------------------------------------------------------------
;;; Elfeed
;;------------------------------------------------------------------------------

(after! elfeed
  (setq
   elfeed-feeds
   '("https://hackaday.com/blog/feed/")))

;;-----------------------------------------------------------------------------------------
;;; IELM
;;------------------------------------------------------------------------------

;; remember ielm history
;; global copy of the buffer-local variable
(after! ielm
  (defvar ielm-comint-input-ring nil)

  (defun set-ielm-comint-input-ring ()
    ;; create a buffer-local binding of kill-buffer-hook
    (make-local-variable 'kill-buffer-hook)
    ;; save the value of comint-input-ring when this buffer is killed
    (add-hook 'kill-buffer-hook #'save-ielm-comint-input-ring)
    ;; restore saved value (if available)
    (when ielm-comint-input-ring
      (message "Restoring comint-input-ring...")
      (setq comint-input-ring ielm-comint-input-ring)))

  (defun save-ielm-comint-input-ring ()
    (message "Saving comint-input-ring...")
    (setq ielm-comint-input-ring comint-input-ring))

  (require 'ielm)
  (add-hook 'inferior-emacs-lisp-mode-hook #'set-ielm-comint-input-ring))

;;-----------------------------------------------------------------------------------------
;;; XML
;;------------------------------------------------------------------------------

(after! nxml
  (add-hook
   'nxml-mode-hook
   (setq nxml-child-indent 2 nxml-attribute-indent 2)))

;; (add-hook 'nxml-mode-hook (lambda () (visual-fill-column-mode -1)))
;; (defun nxml-pretty-format ()
;;   (interactive)
;;   (save-excursion
;;     (shell-command-on-region (point-min) (point-max)
;;     "xmllint --format -" (buffer-name) t)
;;     (nxml-mode)
;;     (indent-region begin end)))

;;-----------------------------------------------------------------------------------------
;;; Markdown
;;------------------------------------------------------------------------------

;; Make Fundamental Mode GFM by default
(after! gfm
  (setq initial-major-mode 'gfm-mode))

;;-----------------------------------------------------------------------------------------
;; C mode
;;-----------------------------------------------------------------------------------------

;; For example, if you prefer double slashes // instead of slash-stars /* ... */
;; in c-mode, insert below code into your ~/.emacs:
(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Preferred comment style
            (setq comment-start "// " comment-end "")))

;;-----------------------------------------------------------------------------------------
;; Clojure
;;-----------------------------------------------------------------------------------------

(setq org-babel-clojure-backend "cider")
;; http://blogs.fluidinfo.com/terry/2011/11/10/emacs-buffer-mode-histogram/

;;-----------------------------------------------------------------------------------------
;; Buffer Mode Histogram
;;-----------------------------------------------------------------------------------------

(defun buffer-mode-histogram ()
  "Display a histogram of emacs buffer modes."
  (interactive)
  (let* ((totals ())
         (buffers (buffer-list()))
         (total-buffers (length buffers))
         (ht (make-hash-table :test 'equal)))

    ;;
    (save-excursion
      (dolist (buffer buffers)
        (set-buffer buffer)
        (let
            ((mode-name (symbol-name major-mode)))
          (puthash mode-name (1+ (gethash mode-name ht 0)) ht))))

    ;;
    (maphash (lambda (key value)
               (setq totals (cons (list key value) totals))) ht)

    ;;
    (setq totals (sort totals (lambda (x y) (> (cadr x) (cadr y)))))

    ;; printout
    (with-output-to-temp-buffer "Buffer mode histogram"
      (princ (format "%d buffers open, in %d distinct modes\n\n"
                     total-buffers (length totals)))
      (dolist (item totals)
        (let ((key (car item))
              (count (cadr item)))
          (if (equal (substring key -5) "-mode")
              (setq key (substring key 0 -5)))
          (princ (format "%2d %20s %s\n" count key
                         (make-string count ?+))))))))

;;------------------------------------------------------------------------------
;; Functions for alphabetically sorting items
;;------------------------------------------------------------------------------

(defun sort-code-block (comment-char)
  "Alphabetically sorts code blocks in a file, starting with #
start:sort and ending with # end:sort, where # is the comment
char of the language you are editing"
  (let ((home (point))
        (start-search (concat "^\s*" comment-char " start:sort"))
        (end-search (concat "^\s*" comment-char " end:sort")))
    (goto-char (point-min))
    (while (re-search-forward start-search nil t)

      (previous-line)
      (let ((start (re-search-forward start-search nil t))
            (end
             ;; want to get the match *before* end:sort
             (- (progn (re-search-forward end-search nil t)
                       (match-beginning 0)) 1)))
        (sort-lines 'nil start end)))
    (goto-char home))
  ;; make sure to return nil here for write-contents-hooks
  nil)

(defun sort-elisp-block ()
  (interactive)
  (sort-code-block ";;"))



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

    (princ (format "%5s   %4s    %s%s\n"
                   valword "%Tot"

                   (make-string (- max-length (length keyword)) ? )
                   keyword))
    (princ (format "%s\n" (make-string 32 ?-)))
    (dolist (item totals)
      (let* ((key (car item))
             (count (cadr item))
             (count-normal (if normalize (* normalize (/ count max-val)) count))
             (percent (* 100 (/ count sum))))
        (when (not (equal key "--"))
          (princ (format "%6.2f   %2d%%     %s%s %s\n"
                         count                                        ; count
                         percent                                      ; percent
                         (make-string (- max-length (length key)) ? ) ; white paddng
                         key                                          ; key
                         (make-string (round count-normal) ?+))))))   ; +++++
    (princ (format "%s\n" (make-string 32 ?-)))
    (princ (format "%6.2f" sum))))

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

(defun print-work-chart-in-date-range (title start-year start-month end-year end-month &optional short)
  (let ((org-table-data ()))

    (princ "---------------------------------------------------------------------------\n")
    (princ (format "%s\n" title))
    (princ "---------------------------------------------------------------------------\n")

    ;; gather all of the tables into one list
    (save-excursion
      (dolist (table
               (mapcar (lambda (x) (format "%04d-%02d" (car x) (cadr x)))
                       (date-range-to-year-month-list start-year start-month end-year end-month))
               )
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

    ;; plot once with everything included
    (plot-monthly-work-chart org-table-data)


    ;; plot again with only billable items
    (when (not short)
      (princ "\n\n")
      (setq org-table-data
            (cl-remove-if
             (lambda (a)
               (member (upcase (nth 3 a)) '("VACATION" "HOLIDAY" "SICK" "ADMIN" "DEVEL")))
             org-table-data))
      (plot-monthly-work-chart org-table-data))))

;;
;; Local Variables:
;; eval: (make-variable-buffer-local 'after-save-hook)
;; eval: (add-hook 'write-contents-hooks 'sort-elisp-block nil t)
;; End:
