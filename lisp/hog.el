;;; ../.dotfiles/doom.d/lisp/hog.el -*- lexical-binding: t; -*-

(defvar hog-vivado-path
  "~/Xilinx/Vivado/2019.2/settings64.sh")

(defun hog-get-projects ()
  (split-string (shell-command-to-string (format "ls -d %sTop/* | sed 's#.*/##'" (projectile-project-root)))))

;;;###autoload
(defun hog-create-project (project)
  "Create project in Hog"
  (interactive (list (completing-read "Formula: "
                                      (hog-get-projects)
                                      nil
                                      t)))
  (hog-run-command "Hog/CreateProject.sh" project))

;;;###autoload
(defun hog-launch-synthesis (project)
  "Launch Hog Sythesis Only"
  (interactive (list (completing-read "Formula: "
                                      (hog-get-projects)
                                      nil
                                      t)))
  (hog-run-command "Hog/LaunchWorkflow.sh -synth_only" project))

;;;###autoload
(defun hog-launch-workflow (project)
  "Launch Hog Sythesis Only"
  (interactive (list (completing-read "Formula: "
                                      (hog-get-projects)
                                      nil
                                      t)))
  (hog-run-command "Hog/LaunchWorkflow.sh" project))

;;;###autoload
(defun hog-launch-impl (project)
  "Launch Hog Sythesis Only"
  (interactive (list (completing-read "Formula: "
                                      (hog-get-projects)
                                      nil
                                      t)))
  (hog-run-command "Hog/LaunchWorkflow.sh -impl_only" project))

(defun hog-run-command (subcmd project &rest args)
  (let* ((name (format "%s" subcmd))
         (buf (format "*%s*" name)))
    (async-shell-command (format "source %s && %s %s %s%s | tee hog.log | ccze -A"
                                             hog-vivado-path
                                             subcmd
                                             project
                                             (if args " " "")
                                             (string-join args " "))
                                     buf)
    (with-current-buffer buf
      (evil-normal-state)
      (view-mode)
      )))
