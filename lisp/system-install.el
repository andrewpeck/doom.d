;;; -*- lexical-binding: t; -*-

;;; Code:

;; package specific commands and flags
;;
(require 'ansi-color)

(defun system-install-get-package-cmd ()
  (cond ((executable-find "dnf")    "dnf")
        ((executable-find "pacman") "pacman")
        ((executable-find "apt")    "apt")))

(defun system-install-get-package-info-flag ()
  (cond ((executable-find "dnf")    "info")
        ((executable-find "pacman") "-Si")
        ((executable-find "apt")    "show")))

(defun system-install-get-package-install-flag ()
  (cond ((executable-find "dnf")    "install")
        ((executable-find "pacman") "-S")
        ((executable-find "apt")    "install")))

(defun system-install-get-package-update-flag ()
  (cond ((executable-find "dnf")    "update")
        ((executable-find "pacman") "-Sy")
        ((executable-find "apt")    "install")))

(defun system-install-get-system-upgrade-flag ()
  (cond ((executable-find "dnf")    "update")
        ((executable-find "pacman") "-Syu")
        ((executable-find "apt")    "upgrade")))

(defun system-install-get-package-list-cmd ()
  (cond ((executable-find "dnf")    "dnf -C list available | awk -F. '{print $1}' | uniq | tail -n +2")
        ((executable-find "pacman") "pacman -Sl | awk '{print $2}'")
        ((executable-find "apt")    "apt-cache search . | awk '{print $1}'")))

(defun system-install-get-installed-package-list-cmd ()
  (cond ((executable-find "dnf") "dnf -C list installed | awk -F. '{print $1}' | uniq | tail -n +2")
        ((executable-find "pacman") "pacman -Q | awk '{print $2}'")
        ((executable-find "apt") "apt list --installed 2> /dev/null | awk -F\/ '{print $1}' | grep -v \"Listing...\"")))

(defun system-install-get-clean-cache-cmd ()
  (cond ((executable-find "pacman") "sudo pacman -Sc")
        ((executable-find "apt") "sudo apt-get clean")))

;;;###autoload
(defun system-install-clean-cache ()
  "Clean system package cache"
  (interactive)
  (with-editor-async-shell-command (system-install-get-clean-cache-cmd)))

;; generic functions

(defun system-install-get-package-list ()
  (s-split "\n" (shell-command-to-string (system-install-get-package-list-cmd)) t))

(defun system-install-get-installed-package-list ()
  (s-split "\n" (shell-command-to-string (system-install-get-installed-package-list-cmd)) t))

(define-minor-mode system-install-run-minor-mode
  "Minor mode for buffers running brew commands"
  :keymap '(("q" .  bury-buffer)))

(with-eval-after-load 'evil
  (evil-define-key 'normal system-install-run-minor-mode-map "q" #'bury-buffer))

;; (ignore-errors
;;   (defun my-colorize-compilation-buffer ()
;;     (when (eq major-mode 'compilation-mode)
;;       (ansi-color-apply-on-region compilation-filter-start (point-max))))
;;   (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

(defun system-install-run (subcmd &rest args)
  (let* ((name (format "%s" subcmd))
         (buf (format "*%s*" name)))


    (with-editor-async-shell-command (format "sudo %s %s%s%s"
                                             (system-install-get-package-cmd)
                                             subcmd
                                             (if args " " "")
                                             (string-join args " "))
                                     buf)

    (with-current-buffer buf
      (ansi-color-apply-on-region (point-min) (point-max))
      (system-install-run-minor-mode))))

;;;###autoload
(defun system-install (package)
  "Install `package' via system installer"
  (interactive (list (completing-read "Formula: "
                                      (system-install-get-package-list)
                                      nil
                                      t)))
  (system-install-run (system-install-get-package-install-flag) package))

;;;###autoload
(defun system-upgrade-package (package)
  "Upgrade `package' to the latest version"
  (interactive (list (completing-read "Formula: "
                                      (system-install-get-installed-package-list)
                                      nil
                                      t)))
  (system-install-run (system-install-get-package-update-flag) package))

;;;###autoload
(defun system-upgrade ()
  "Upgrade all system packages"
  (interactive)
  (system-install-run (system-install-get-system-upgrade-flag)))

;;;###autoload
(defun system-update ()
  "Update the package database"
  (interactive)
  (system-install-run (system-install-get-package-update-flag)))

;;;###autoload
(defun system-package-info (package)
  "Display `info' output for `package'"
  (interactive (list (completing-read "Formula: "
                                      (system-install-get-package-list)
                                      nil
                                      t)))
  (system-install-run (system-install-get-package-info-flag) package))

(provide 'system-install)
;;; system-install.el ends here
