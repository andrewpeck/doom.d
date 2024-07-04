;; -*- lexical-binding: t; -*-

;;; Doom
;;------------------------------------------------------------------------------

(defun doom/package-under-review () (interactive)
       "Open the doom packages under review Github page"
       (browse-url "https://github.com/orgs/doomemacs/projects/5/views/1"))

;; Dashboard
(defun ap/dashboard-widget-banner ()
  (let ((point (point)))
    (mapc (lambda (line)
            (insert (propertize (+doom-dashboard--center +doom-dashboard--width line)
                                'face 'doom-dashboard-banner) " ")
            (insert "\n"))
          '(""
            "    ↑      "
            "← h j k l →"
            "      ↓    "
            )
          )

    (when (and (display-graphic-p)
               (stringp fancy-splash-image)
               (file-readable-p fancy-splash-image))
      (let ((image (create-image (fancy-splash-image-file))))
        (add-text-properties
         point (point) `(display ,image rear-nonsticky (display)))
        (save-excursion
          (goto-char point)
          (insert (make-string
                   (truncate
                    (max 0 (+ 1 (/ (- +doom-dashboard--width
                                      (car (image-size image nil)))
                                   2))))
                   ? ))))
      (insert (make-string (or (cdr +doom-dashboard-banner-padding) 0)
                           ?\n)))))

(defun doom-display-benchmark-h (&optional return-p)
  "Display a benchmark including number of packages and modules loaded.

If RETURN-P, return the message as a string instead of displaying it."
  (funcall (if return-p #'format #'message)
           "Loaded %d packages across %d modules in %.03fs"
           (- (length load-path) (length (get 'load-path 'initial-value)))
           (hash-table-count doom-modules)
           doom-init-time))

(setq +doom-dashboard-functions
      '(ap/dashboard-widget-banner
        ;; doom-dashboard-widget-shortmenu
        doom-dashboard-widget-loaded
        ;; doom-dashboard-widget-footer
        ))


;; (remove-hook
;;  '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
