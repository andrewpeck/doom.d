;; -*- lexical-binding: t; -*-

;;------------------------------------------------------------------------------
;; Keybindings
;;------------------------------------------------------------------------------

(map! :after dired-aux
      :map dired-mode-map
      :n "E" #'dired-do-open)

;;------------------------------------------------------------------------------
;; Dired Preview
;;------------------------------------------------------------------------------

(use-package dired-preview
  :after dired
  :custom
  ;; upstream default; anything much lower previews on every line you cursor
  ;; past (the package treats 0 as 0.1 internally for that reason)
  (dired-preview-delay 0.7)
  (dired-preview-ignored-extensions-regexp
   (concat "\\."
           "\\(mkv\\|webm\\|mp4\\|mp3\\|ogg\\|m4a\\|flac\\|wav"
           "\\|gz\\|zst\\|tar\\|xz\\|rar\\|zip"
           "\\|iso\\|epub\\|pdf\\)\\'"))
  :config

  ;; Don't preview over tramp: building a preview reads the file's contents
  ;; (up to `dired-preview-max-size') across the connection. Guards only the
  ;; automatic path -- `M-x dired-preview-mode' still works if you want it.
  ;; Named rather than a lambda so re-loading this file doesn't stack advice.
  (advice-add 'dired-preview--on :before-while
              (defun dired-preview-local-only-p ()
                (not (file-remote-p default-directory))))

  (dired-preview-global-mode 1))

;;------------------------------------------------------------------------------
;; Dired
;;------------------------------------------------------------------------------

(use-package! diredfl
  :after dired
  :config
  (add-to-list 'diredfl-compressed-extensions ".zst"))

(use-package! dired-aux
  :after dired
  :config

  (setopt dired-compress-file-default-suffix ".zst"
          dired-compress-directory-default-suffix ".tar.zst")

  (dolist (element (list 'dired-do-rename 'dired-create-directory))
    (advice-add element
                :after
                (defun dired-unmark-after-rename-advice (&optional _)
                  (dired-unmark 1 t)
                  (forward-line -1)))))

(use-package! dired-x
  :after dired
  :config

  (setopt dired-omit-extensions '())

  ;; replace the dired-mode-hook dired-omit-mode additions
  (add-hook 'dired-initial-position-hook #'dired-omit-mode))

(use-package! dired

  :config

  ;; https://github.com/jwiegley/emacs-async
  ;; dired async mode somehow seems even slower...
  ;; (dired-async-mode 1)

  ;; better dired soring
  (setopt dired-listing-switches "-a1vBhl  --group-directories-first"
          dired-mouse-drag-files t
          dired-do-revert-buffer t                        ; Automatically revert Dired buffers after dired-do operations.
          dired-dwim-target t                             ; suggest a target for moving/copying intelligently
          dired-hide-details-hide-symlink-targets nil
          dired-auto-revert-buffer #'dired-buffer-stale-p ; don't prompt to revert, just do it
          dired-recursive-copies  'always                 ; Always copy/delete recursively
          dired-recursive-deletes 'top                    ; Always copy/delete recursively
        )

  (defun +dired/quit-all ()
    "Kill all `dired-mode' buffers."
    (interactive)
    (mapc #'kill-buffer (doom-buffers-in-mode 'dired-mode))
    (message "Killed all dired buffers"))

  (map! :map dired-mode-map
        ;; Kill all dired buffers on q
        :ng "q" #'+dired/quit-all
        ;; To be consistent with ivy/helm+wgrep integration
        "C-c C-e" #'wdired-change-to-wdired-mode)

  :init

  ;; drop the unconditional variant; diff-hl in dired is enabled by doom's
  ;; `+vc-gutter-enable-maybe-h', which skips remote directories (see
  ;; setup-git.el)
  (remove-hook! 'dired-mode-hook #'diff-hl-dired-mode)

  (add-hook! 'dired-mode-hook
    (defun hook/dired-hide-details ()
      "Hide details by default in dired to reduce line noise."
      (dired-hide-details-mode 1)))

  ;; (add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode 1)))

  ;; (add-hook! 'dired-mode-hook
  ;;   (defun hook/enable-dired-git-filter ()
  ;;     ""
  ;;     (unless (remote-host? default-directory)
  ;;       (when (locate-dominating-file "." ".git")
  ;;         (dired-filter-mode)
  ;;         (dired-filter-by-git-ignored)))))

  ;; (add-hook! 'dired-after-readin-hook
  ;;   (defun hook/dired-git-info-mode ()
  ;;     "Enable dired git info on local files."
  ;;     (unless (remote-host? default-directory)
  ;;       (when (locate-dominating-file "." ".git")
  ;;         (dired-git-info-auto-enable)))))

  ;; Stolen from doom: Disable the prompt about whether I want to kill the Dired
  ;; buffer for a deleted directory. Of course I do!
  (setopt dired-clean-confirm-killing-deleted-buffers nil)

  (defun my/dired-convert-marked-image-files-to-pdf ()
    "Combine the marked image files into a single PDF."
    (interactive)
    (unless (executable-find "convert")
      (user-error "ImageMagick's `convert' not found"))
    (let ((ofile (read-string "Output File: ")))

      (when (string-empty-p ofile)
        (user-error "No output file specified!"))

      ;; stick on pdf extension if it isn't there
      (when (not (string= "pdf" (file-name-extension ofile)))
        (setq ofile (concat ofile ".pdf")))

      ;; NOTE: DESTINATION must not be t here -- that means "the current
      ;; buffer", which would splice convert's output into the dired listing.
      (let ((args (append (list "convert" nil (get-buffer-create "*convert*") nil)
                          (dired-get-marked-files)
                          (list ofile))))

        (if (zerop (apply #'call-process args))
            (progn (revert-buffer) (message "Wrote %s" ofile))
          (pop-to-buffer "*convert*"))))))

;;------------------------------------------------------------------------------
;; DWIM Shell
;;------------------------------------------------------------------------------

(use-package! dwim-shell-command

  :commands (dwim-shell-command-on-marked-files)
  :after dired
  :init
  (defun my/dwim-shell-command-archive-zstd ()
    "Tar marked files as zst"
    (interactive)
    (dwim-shell-command-on-marked-files
     "Archive as zstd"
     "tar -cavf '<<fne>>.tar.zst' '<<f>>'"
     :utils "tar"))

  (defun my/dwim-svg-to-pdf ()
    "Convert SVG to PDF"
    (interactive)
    (dwim-shell-command-on-marked-files
     "Convert SVG to PDF"
     "inkscape '<<f>>' --export-area-drawing --batch-process --export-type=pdf --export-filename='<<fne>>.pdf'"
     :utils "inkscape"))

  (defun my/gerber-to-svg ()
    "Convert Gerber to SVG"
    (interactive)
    (dwim-shell-command-on-marked-files
     "Convert Gerber to SVG"
     "gerbv --dpi=600 --border=0 --export=svg --output='<<fne>>'.svg '<<f>>'"
     :utils "gerbv"))

  (defun my/dwim-shell-command-archive-gz ()
    "Tar marked files as gz"
    (interactive)
    (dwim-shell-command-on-marked-files
     "Archive as zstd"
     "tar -cavf '<<fne>>.tar.gz' '<<f>>'"
     :utils "tar"))

  (defun my/dwim-shell-command-extract ()
    "Extract tar archive at point."
    (interactive)
    (dwim-shell-command-on-marked-files
     "Unarchive."
     "tar -xavf '<<f>>'"
     :utils "tar")))
