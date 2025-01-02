;; -*- lexical-binding: t; -*-
;;------------------------------------------------------------------------------ 
;; Image Handlers
;;------------------------------------------------------------------------------

(use-package image-mode
  :init

  (add-to-list 'auto-mode-alist '("\\.drawio\\'"       . image-mode))
  (add-to-list 'auto-mode-alist '("\\.excalidraw\\'"   . image-mode))
  (add-to-list 'auto-mode-alist '("\\.gbr\\'"          . image-mode))
  (add-to-list 'auto-mode-alist '("\\.art\\'"          . image-mode))

  :config

  (defun gbr-to-png (file data-p)
    (if data-p
        (error "Can't decode non-files")
      (call-process "gerbv" nil t nil
                    "--dpi=600"  "--border=0"  "--export=png" (concat  "--output=" "/dev/stdout") file)))

  (defun drawio-to-png (file data-p)
    (if data-p
        (error "Can't decode non-files")
      (let ((ofile (concat file ".drawio.png")))
        (call-process "drawio" nil nil nil "-x" file "-f" "png" "-o" ofile "2>/dev/null")
        (call-process "cat" nil t nil ofile)
        (delete-file ofile))))

  (defun excalidraw-to-png (file data-p)
    (if data-p
        (error "Can't decode non-files")
      (let ((svg (concat file ".svg"))
            (png (concat file ".png")))
        (call-process "excalidraw_export" nil nil nil file)
        (call-process "inkscape" nil nil nil svg "--export-area-drawing" "--export-type=png" (concat "--export-filename=" png))
        (call-process "cat" nil t nil png)
        (delete-file png))))

  (setq image-use-external-converter t)
  (image-converter-add-handler "art" 'gbr-to-png)
  (image-converter-add-handler "gbr" 'gbr-to-png)
  (image-converter-add-handler "drawio" 'drawio-to-png)
  (image-converter-add-handler "excalidraw" 'excalidraw-to-png))
