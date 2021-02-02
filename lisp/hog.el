;; ../.dotfiles/doom.d/lisp/hog.el -*- lexical-binding: t; -*-

(defvar hog-vivado-path "~/Xilinx/Vivado/2019.2/settings64.sh")
(defvar hog-number-of-jobs 4)

(defun hog-get-projects ()
  "Get a list of available Hog projects"
  (split-string (shell-command-to-string
                 (format "ls -d %sTop/* | sed 's#.*/##'"
                         (projectile-project-root)))))

;;;###autoload
(defun hog-open-project (project)
  (interactive (list (completing-read "Project: "
                                      (hog-get-projects)
                                      nil
                                      t)))
  (let ((command (format "cd %s && source %s && vivado %sProjects/%s/%s.xpr &"
                         (projectile-project-root)
                         hog-vivado-path
                         (projectile-project-root)
                         project
                         project
                         )))
    (message (format "Opening Hog Project %s" project))
    (async-shell-command command)))

;;;###autoload
(defun hog-create-project (project)
  "Create project in Hog"
  (interactive (list (completing-read "Project: "
                                      (hog-get-projects)
                                      nil
                                      t)))
  (hog-run-command "Hog/CreateProject.sh" project))

;;;###autoload
(defun hog-launch-synthesis (project)
  "Launch Hog Sythesis Only"
  (interactive (list (completing-read "Project: "
                                      (hog-get-projects)
                                      nil
                                      t)))
  (hog-run-command
   (format "Hog/LaunchWorkflow.sh -synth_only -j%d" hog-number-of-jobs)
   project))

;;;###autoload
(defun hog-launch-workflow (project)
  "Launch Hog Full Workflow"
  (interactive (list (completing-read "Project: "
                                      (hog-get-projects)
                                      nil
                                      t)))
  (hog-run-command
   (format "Hog/LaunchWorkflow.sh -j%d" hog-number-of-jobs)
   project))

;;;###autoload
(defun hog-launch-impl (project)
  "Launch Hog Implementation Only"
  (interactive (list (completing-read "Project: "
                                      (hog-get-projects)
                                      nil
                                      t)))
  (hog-run-command
   (format "Hog/LaunchWorkflow.sh -impl_only -j%d" hog-number-of-jobs)
   project))

(defun hog-run-command (subcmd project &rest args)
  "Run a Hog command (and colorize it)"
  (let* ((name (format "%s" subcmd))
         (buf (format "*%s*" name)))
    (async-shell-command (format "cd %s && source %s && %s%s %s %s%s | tee hog.log | ccze -A"
                                 (projectile-project-root)
                                 hog-vivado-path
                                 (projectile-project-root)
                                 subcmd
                                 project
                                 (if args " " "")
                                 (string-join args " "))
                         buf)
    (with-current-buffer buf
      (evil-normal-state)
      (view-mode)
      )))

;;--------------------------------------------------------------------------------
;; Intelligence for reading source files...
;; I have plans for this... but it does nothing right now
;;--------------------------------------------------------------------------------

(defun hog-read-lines-from-file (file-path)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents file-path)
    (split-string (buffer-string) "\n" t)))

;; TODO: add lib= etc filtering to this
(defun hog-read-line (line)
  (file-expand-wildcards line))

(defun hog-get-src-files (project)
  "Return a list of src files for a given project"
  (split-string (shell-command-to-string
                 (format "ls -d %sTop/%s/list/*"
                         (projectile-project-root) project))))

;; (defvar structure-example
;;   '(
;;     ("lib" .
;;      (("a file" "another file" "another file"))
;;      )))
;; (find 5 '( (5 . ((0 1 9)) ) (4 . ((2 3)) )))
;; (print structure-example)
;; (print (find "lib" structure-example))

(defun hog-src-is-comment (line)
  "Check if Hog src line is a comment"
  (string-match "^#.*" line)
  ;; FIXME: should check also for whitespace then comment
  )

(defun hog-src-strip-props (line)
  line
  ;;(replace-regexp-in-string "\s+.*" "" line)
  )

(defun hog-read-src-file (file)
  "Read a Hog source file"
  ;; expand glob patterns into a list of files
  (mapcar #'file-expand-wildcards
          ;; tack the project root onto the Hog (relative) path
          (mapcar (lambda (x) (format "%s" (projectile-project-root) x))
                  ;;(mapcar #'hog-src-strip-props
                  ;; remove comment lines and read the others from the .src file
                  (remove-if #'hog-src-is-comment
                             (hog-read-lines-from-file file)
                             ))))

(defun hog-read-src-files (project)
  "Reads all source files into a nested list"
  (let ((files (hog-get-src-files project)))
    (loop for file in files do
          ;; read only .src files... we don't care about .prop, .xdc, etc
          (if (equal "src" (file-name-extension file))
              (print (list
                      (file-name-base file)
                      (hog-read-src-file file)
                      ))))))
