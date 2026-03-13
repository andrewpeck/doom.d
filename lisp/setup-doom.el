;; -*- lexical-binding: t; -*-

(defun doom/package-under-review ()
  "Open the doom packages under review Github page"
  (interactive)
  (browse-url "https://github.com/orgs/doomemacs/projects/5/views/1"))

;; Dashboard
(setq +dashboard-ascii-banner-fn (lambda () "Emacs"))

(setq +dashboard-functions
  `(+dashboard-widget-banner
    +dashboard-widget-loaded))

(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

;; add an extra newline to center it, maybe should report
(defun +dashboard-widget-loaded ()
  (when doom-init-time
    (+dashboard-insert-centered
     (concat "\n"
             (propertize (doom-display-benchmark-h 'return)
                         'face '+dashboard-loaded)))))

(setq doom-scratch-dir doom-user-dir)
