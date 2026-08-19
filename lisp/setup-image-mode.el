;; -*- lexical-binding: t; -*-

(use-package image
  :custom
  (image-use-external-converter t))

(use-package image-converter
  :init
  (add-to-list 'auto-mode-alist '("\\.drawio\\'"       . image-mode))
  (add-to-list 'auto-mode-alist '("\\.excalidraw\\'"   . image-mode))
  (add-to-list 'auto-mode-alist '("\\.gbr\\'"          . image-mode))
  (add-to-list 'auto-mode-alist '("\\.art\\'"          . image-mode))
  (add-to-list 'auto-mode-alist '("\\.kra\\'"          . image-mode))

  (declare-function gbr-to-png "setup-image-mode" (file data-p))
  (declare-function drawio-to-png "setup-image-mode" (file data-p))
  (declare-function excalidraw-to-png "setup-image-mode" (file data-p))
  (declare-function krita-to-png "setup-image-mode" (file data-p))

  :config

  (defun gbr-to-png (file data-p)
    (if data-p
        (error "Can't decode non-files")
      (call-process "gerbv" nil t nil
                    "--dpi=600"  "--border=0"  "--export=png" (concat  "--output=" "/dev/stdout") file)))

  (defun drawio-to-png (file data-p)
    (if data-p
        (error "Can't decode non-files")
      (let ((ofile (make-temp-file "drawio-" nil ".png")))
        (unwind-protect
            (progn
              ;; NB: every argument here is passed straight to drawio; a shell
              ;; redirection like "2>/dev/null" is taken as an input file and
              ;; makes the export fail.
              (unless (zerop (call-process "drawio" nil nil nil
                                           "-x" "-f" "png" "-o" ofile file))
                (error "drawio failed to export %s" file))
              (when (zerop (file-attribute-size (file-attributes ofile)))
                (error "drawio produced no output for %s" file))
              (insert-file-contents-literally ofile))
          (delete-file ofile)))))

  (defun excalidraw-to-png (file data-p)
    (if data-p
        (error "Can't decode non-files")
      (let ((svg (concat file ".svg"))
            (png (concat file ".png")))
        (call-process "excalidraw_export" nil nil nil file)
        (call-process "inkscape" nil nil nil svg "--export-area-drawing" "--export-type=png" (concat "--export-filename=" png))
        (call-process "cat" nil t nil png)
        (delete-file png))))

  ;; https://ayatakesi.github.io/emacs/29.1/html/Image-Mode.html
  (defun krita-to-png (file data-p)
    (if data-p
        (error "Can't decode non-files")
      (call-process "unzip" nil t nil
                    "-qq" "-c" "-x" file "mergedimage.png")))

  (run-with-timer 3 nil
                  (lambda ()
                    (image-converter-initialize)
                    (image-converter-add-handler "art" 'gbr-to-png)
                    (image-converter-add-handler "gbr" 'gbr-to-png)
                    (image-converter-add-handler "drawio" 'drawio-to-png)
                    (image-converter-add-handler "excalidraw" 'excalidraw-to-png)
                    (image-converter-add-handler "kra" 'krita-to-png))))
