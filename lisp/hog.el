;; ../.dotfiles/doom.d/lisp/hog.el -*- lexical-binding: t; -*-

(defvar hog-vivado-path "~/Xilinx/Vivado/2019.2/settings64.sh")
(defvar hog-number-of-jobs 4)

(defun hog-get-projects ()
  "Get a list of available Hog projects"
  ;; convert the full directory into the path, e.g.
  ;; /home/topham/project/Top/myproject --> myproject
  (mapcar (lambda (file) (file-name-nondirectory (directory-file-name file)))
          ;; list all directories in the Top/ folder
          (split-string (shell-command-to-string (format "ls -d %sTop/*" (projectile-project-root))))
          ))

(defun hog-get-project-xml (project)
  "Return the XML (XPR) file for a given Hog project"
  (format "%sProjects/%s/%s.xpr" (projectile-project-root) project project)
  )

;;;###autoload
(defun hog-open-project (project)
  (interactive (list (completing-read "Project: "
                                      (hog-get-projects)
                                      nil
                                      t)))
  (let ((command (format "cd %s && source %s && vivado %s &"
                         (projectile-project-root)
                         hog-vivado-path
                         (hog-get-project-xml project)
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
    (let ((cmd (format "cd %s && source %s && %s%s %s %s%s | tee hog.log | ccze -A"
                      (projectile-project-root)
                      hog-vivado-path
                      (projectile-project-root)
                      subcmd
                      project
                      (if args " " "")
                      (string-join args " "))))
      (async-shell-command cmd buf)
      )
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

;;(defun hog-get-src-files (project)
;;  "Return a list of src files for a given project"
;;  (split-string (shell-command-to-string
;;                 (format "ls -d %sTop/%s/list/*"
;;                         (projectile-project-root) project))))

(defun hog-parse-vivado-xml (project-file)
  ;; https://stackoverflow.com/questions/43806637/parsing-xml-file-with-elisp
  (require 'xml)
  (setq lib-list (list))
  (dolist (file-node
        ;; get a list of all the Project -> FileSets -> FileSet --> File nodes
        (xml-get-children (assq 'FileSet (assq 'FileSets (assq 'Project (xml-parse-file project-file)))) 'File))

        ;; for each node, extract the path to the .src file
        (setq src-file
          ;; strip off the vivado relative path; make it relative to the repo root instead
          (replace-regexp-in-string "$PPRDIR\/\.\.\/\.\.\/" "" (xml-get-attribute file-node 'Path )))

        ;; for each node, extract the library property (only applies to vhdl sources)
        (dolist (attr (xml-get-children (assq 'FileInfo (cdr file-node)) 'Attr))

          (when (equal (xml-get-attribute attr 'Name) "Library")

                (setq lib  (xml-get-attribute attr 'Val))

                (setf lib-list (hog-append-to-library lib-list lib src-file))
                )))
  lib-list
  )

(defun hog-vhdl-tool-lib-declaration (lib-list)
  (message (car lib-list))
  )

(defun hog-append-to-library (src-list lib-name file-name)
  (let ((lib (assoc lib-name src-list)))
    (when (eq lib nil)
      (setf src-list (append src-list (list (list lib-name (list)))))
      ;;(print src-list)
      (setq lib (assoc lib-name src-list))
      )
    (setf (cadr lib) (append (cadr lib) (list file-name) ))
    )
  src-list
  )
