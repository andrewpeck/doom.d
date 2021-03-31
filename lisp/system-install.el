;;; pacman.el --- An Emacs interface to Pacman   -*- lexical-binding: t; -*-

;;; Code:

(defun system-install-commandp (program)
  (eq 0 (shell-command (format "command -v %s 2> /dev/null" program))))

;; package specific commands and flags

(defun system-install-get-package-cmd ()
  (cond
   ((system-install-commandp "dnf")
    "dnf")
   ((system-install-commandp "pacman")
    "pacman")
   ((system-install-commandp "apt")
    "apt")))

(defun system-install-get-package-info-flag ()
  (cond
   ((system-install-commandp "dnf")
    "info")
   ((system-install-commandp "pacman")
    "-Si")
   ((system-install-commandp "apt")
    "show")))

(defun system-install-get-package-install-flag ()
  (cond
   ((system-install-commandp "dnf")
    "install")
   ((system-install-commandp "pacman")
    "-S")
   ((system-install-commandp "apt")
    "install")))

(defun system-install-get-package-update-flag ()
  (cond
   ((system-install-commandp "dnf")
    "makecache")
   ((system-install-commandp "pacman")
    "-Sy")
   ((system-install-commandp "apt")
    "update")))

(defun system-install-get-system-upgrade-flag ()
  (cond
   ((system-install-commandp "dnf")
    "update")
   ((system-install-commandp "pacman")
    "-Syu")
   ((system-install-commandp "apt")
    "upgrade")))

(defun system-install-get-package-list-cmd ()
  (cond
   ((system-install-commandp "dnf")
    "dnf -C list available | awk '{print $1}' | tail -n +2")
   ((system-install-commandp "pacman")
    "pacman -Sl | awk '{print $2}'")
   ((system-install-commandp "apt")
    "apt-cache search . | awk '{print $1}'")))

(defun system-install-get-installed-package-list-cmd ()
  (cond
   ((system-install-commandp "dnf")
    "dnf -C list installed | awk '{print $1}' | tail -n +2")
   ((system-install-commandp "pacman")
    "pacman -Q | awk '{print $2}'")
   ((system-install-commandp "apt list --installed 2> /dev/null | awk -F\/ '{print $1}' | grep -v "Listing..." ")
    "")))

(defun system-install-get-clean-cache-cmd ()
  (cond
   ((system-install-commandp "pacman")
    "sudo pacman -Sc")
   ((system-install-commandp "apt")
    "sudo apt-get clean")))

;;;###autoload
(defun system-install-clean-cache ()
  "Clean system package cache"
  (interactive)
  (with-editor-async-shell-command (system-install-get-clean-cache-cmd)))

;; generic functions

(defun system-install-get-package-list ()
  (s-split "\n" (shell-command-to-string (system-install-get-package-list-cmd)) t)
  )

(defun system-install-get-installed-package-list ()
  (s-split "\n" (shell-command-to-string (system-install-get-installed-package-list-cmd)) t))

(define-minor-mode system-install-run-minor-mode
  "Minor mode for buffers running brew commands"
  :keymap '(("q" .  bury-buffer)))

(with-eval-after-load 'evil
  (evil-define-key 'normal system-install-run-minor-mode-map "q" #'bury-buffer))

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
