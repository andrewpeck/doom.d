;;; ../.dotfiles/doom.d/lisp/teros-hdl.el -*- lexical-binding: t; -*-



;; (shell-command-to-string "~/work/colibri/bin/teroshdl-hdl-documenter --self_contained --dep --fsm -i
;; --outpath
(defvar teros-hdl-documenter-path
  "~/work/colibri/bin/teroshdl-hdl-documenter")

(defun teros-hdl-sm-diagram-from-buffer ()
  (interactive)
  (teros-hdl-sm-diagram-from-file (buffer-file-name)))

(defun teros-hdl-sm-diagram-from-file (file)
  (let ((tmp (format "/tmp/%s" (md5 file)))
        (entity-name "tdc_packet_processor"))
    (make-directory tmp t)
    (message (shell-command-to-string
              (format "%s -o markdown --fsm -i %s --outpath %s"
                      teros-hdl-documenter-path file tmp)))

    ;;(split-window-horizontally)
    (let ((fname (format "%s/stm_%s_00.svg" tmp entity-name)))
      (with-selected-window (selected-window)
        (switch-to-buffer-other-window (find-file-noselect fname t))
        (revert-buffer t t))
      )))

(teros-hdl-sm-diagram-from-file "/home/andrew/work/l0mdt-hdl-design/HAL/tdc/src/tdc_decoder/tdc_packet_processor.vhd")
