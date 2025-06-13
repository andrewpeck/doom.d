;; -*- lexical-binding: t; -*-

(use-package fennel
  :config

  (set-popup-rule! "*fennel .*.fnl*"
    :side 'bottom
    :size 0.25
    :select nil
    :quit t)

  (defun fennel-view-compilation ()
    "Compile the current buffer contents and view the output.
Arguments for the fennel executable are taken from the `fennel-program'
variable."
    (interactive)
    (let* ((file (or (buffer-file-name) (make-temp-file "fennel-compile")))
           (tmp? (not (buffer-file-name)))
           (fennel-program (replace-regexp-in-string "--repl" "" fennel-program))
           (command (format "%s --compile %S" fennel-program file))
           (buffer-name (format "*fennel %s*" (buffer-name))))
      (when tmp?
        (let ((inhibit-message t))
          (write-region (point-min) (point-max) file)))
      (pop-to-buffer buffer-name)
      (save-mark-and-excursion
        (fennel-view-compilation-minor-mode -1)
        (delete-region (point-min) (point-max))
        (insert (shell-command-to-string command))
        (fennel-view-compilation-minor-mode 1)
        (when tmp?
          (delete-file file))))))
