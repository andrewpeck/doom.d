;; -*- lexical-binding: t; -*-
;;
;; this file holds some functions that I want to init early; keep it minimal
;; with things i want at startup that have minimal package depdencies. keep
;; heavier weight functions in my-defuns.el

(defun project-root-dir (&rest _)
  "Return root directory of current project."
  (when-let ((proj (project-current)))
    (project-root proj)))

(defun projectile-project-root (&rest _)
  "Get the root of the current VCS project."
  (project-root-dir))

(defun doom-project-root (&rest _)
  "Get the root of the current VCS project."
  (project-root-dir))

(defun remote-host? (path)
  "Return t if path is a remote host."
  ;; this is just tramp-remote-file-name-spec-regexp
  ;; have a copy here so we don't need
  (let ((remote-name-regexp "\\(-\\|[[:alnum:]]\\{2,\\}\\)\\(?::\\)\\(?:\\([^/:|[:blank:]]+\\)\\(?:@\\)\\)?\\(\\(?:[%._[:alnum:]-]+\\|\\(?:\\[\\)\\(?:\\(?:[[:alnum:]]*:\\)+[.[:alnum:]]*\\)?\\(?:]\\)\\)\\(?:\\(?:#\\)\\(?:[[:digit:]]+\\)\\)?\\)?"))
    (or (string-match-p remote-name-regexp path)
        (file-remote-p path 'host))))

(defun advise-inhibit-messages (fn)
  "Pass in a function name, that function will be advised to supress its output.
Useful if you have an noisy command you want to keep quiet.

Use as e.g. (advice-inhibit-messages \='recentf-cleanup)"

  (advice-add fn :around
              (lambda (orig-fun &rest args)
                (let ((inhibit-message t))
                  (apply orig-fun args)))))

(defun load!! (path) (load! path doom-user-dir t))

(defun load-timer (pkg &optional timer)
  "Load package on a timer."
  (let ((timer (if timer timer 1.5)))
    (run-with-timer timer nil #'load!! pkg)))

(defun load-idle (pkg &optional timer)
  "Load package on idle."
  (unless timer (setq timer 0.1))
  (run-with-idle-timer timer nil #'load!! pkg))

(defmacro run-when-idle (seconds &rest body)
  "Execute BODY after SECONDS of idle."
  (run-with-idle-timer seconds nil (lambda (_) (progn body)) nil))

(defun user-load-idle (path) (load-idle (concat doom-user-dir path)))
(defun user-load-timer (path) (load-timer (concat doom-user-dir path)))

(defun consult--recent-files-sort (file-list)
 "Sort the FILE-LIST by modification time, from most recent to least recent."
 (thread-last
   file-list
   ;; Use modification time, since getting file access time seems to count as
   ;; accessing the file, ruining future uses.
   (mapcar (lambda (f)
             (cons f (file-attribute-modification-time (file-attributes f)))))
   (seq-sort (pcase-lambda (`(,f1 . ,t1) `(,f2 . ,t2))
               ;; Want existing, most recent, local files first.
               (cond ((or (not (file-exists-p f1))
                          (file-remote-p f1))
                      nil)
                     ((or (not (file-exists-p f2))
                          (file-remote-p f2))
                      t)
                     (t (time-less-p t2 t1)))))
   (mapcar #'car)))

;;;###autoload
(defun consult-recent-file ()
  "Find recent using `completing-read'.

This version is modified to sort by recent files"
  (interactive)
  (require 'consult)
  (require 'recentf)
  (find-file
   (consult--read
    (or (mapcar #'abbreviate-file-name recentf-list)
        (user-error "No recent files"))
    :prompt "Find recent file: "
    :sort 'consult--recent-files-sort
    :require-match t
    :category 'file
    :state (consult--file-preview)
    :history 'file-name-history)))

;; like project-find-file, but sorted by date
;;;###autoload
(defun consult-project-find-file ()
  "Find project files using `completing-read'."
  (interactive)
  (require 'consult)
  (find-file
   (consult--read
    (or (project-files (project-current))
        (user-error "No recent files"))
    :prompt "Find project file: "
    :sort 'consult--recent-files-sort
    :require-match t
    :category 'file
    :state (consult--file-preview)
    :history 'file-name-history)))

;;;###autoload
(defun consult-open-buffers ()
  "Find project files using `completing-read'."
  (interactive)
  (require 'consult)
  (require 'dash)
  (find-file
   (consult--read
    (or (delq nil (mapcar #'buffer-file-name (buffer-list)))
        (user-error "No recent files"))
    :prompt "Find buffer file: "
    :sort 'consult--recent-files-sort
    :require-match t
    :category 'file
    :state (consult--file-preview)
    :history 'file-name-history)))
