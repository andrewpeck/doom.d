;;------------------------------------------------------------------------------
;; Casual Dired
;;------------------------------------------------------------------------------

(use-package! casual-dired
  :config
  (define-key dired-mode-map (kbd "C-o") #'casual-dired-tmenu)
  (map! :map dired-mode-map :localleader :desc "Casual Dired" "h" #'casual-dired-tmenu))

(sp-local-pair 'verilog-mode "begin" "end")

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

  (setq dired-compress-file-default-suffix ".zst"
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

  (add-hook 'dired-mode-hook #'dired-omit-mode)
  (setq dired-omit-extensions (remove ".bin" dired-omit-extensions))
  (setq dired-omit-extensions (remove ".bit" dired-omit-extensions))
  )

(use-package! dired

  :config

  ;; https://github.com/jwiegley/emacs-async
  (dired-async-mode 1)

  (evil-define-key 'normal dired-mode-map
    (kbd "E") #'dired-do-open)

  ;; better dired soring
  (setq dired-listing-switches "-a1vBhl  --group-directories-first"
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

  ;; this breaks sensible dired sorting on remote hosts;
  ;; it is there for DOS compatibility which I don't care about
  (remove-hook! 'dired-mode-hook #'+dired-disable-gnu-ls-flags-maybe-h)

  (remove-hook! 'dired-mode-hook #'diff-hl-dired-mode)

  (add-hook 'dired-mode-hook #'diff-hl-dired-mode-unless-remote)

  (add-hook 'dired-mode-hook #'dired-omit-mode)

  (add-hook! 'dired-mode-hook
    (defun hook/dired-hide-details ()
      "Hide details by default in dired to reduce line noise."
      (dired-hide-details-mode 1)))

  ;; (add-hook! 'dired-mode-hook
  ;;   (defun hook/enable-dired-git-filter ()
  ;;     ""
  ;;     (unless (remote-host? default-directory)
  ;;       (when (locate-dominating-file "." ".git")
  ;;         (dired-filter-mode)
  ;;         (dired-filter-by-git-ignored)))))

  (add-hook! 'dired-after-readin-hook
    (defun hook/dired-git-info-mode ()
      "Enable dired git info on local files."
      (unless (remote-host? default-directory)
        (when (locate-dominating-file "." ".git")
          (dired-git-info-auto-enable)))))

  ;; Stolen from doom: Disable the prompt about whether I want to kill the Dired
  ;; buffer for a deleted directory. Of course I do!
  (setq dired-clean-confirm-killing-deleted-buffers nil)

  (advice-add 'dired-find-file
              :before-until
              (lambda () (if (and (member (file-name-extension (dired-get-file-for-visit))
                                          auto-external-handle-extensions)
                                  (not (remote-host? (dired-get-file-for-visit))))
                             (ap/dired-external-open)
                           nil)))

  (defun ap/external-open (file)
    (let* ((ext-handler (cdr (assoc (file-name-extension file) external-program-handlers)))
           (program (or ext-handler "xdg-open")))
      (message (format  "Opening %s in %s" file program))
      (call-process program nil 0 nil file)))

  (defun ap/dired-external-open()
    (interactive)
    (ap/external-open (dired-get-file-for-visit))
    t)

  (defvar auto-external-handle-extensions
    '("drawio")
    "List of extensions to automatically open via external program.")

  (defvar external-program-handlers
    '(("drawio" . "drawio"))
    "alist of extensions and the program which should be used to open them, e.g.
\\='((\".mp3\" .\"mplayer\")
   (\".avi\" .\"vlc\")).

If not specified it will default to xdg-open.")
  )

(defun drawio ()
  (interactive)
  (let* ((file (buffer-file-name))
         (ext-handler (cdr (assoc (file-name-extension file) external-program-handlers)))
         (program (or ext-handler "xdg-open")))
    (message (format  "Opening %s in %s" file program))
    (call-process program nil 0 nil file)))


;;------------------------------------------------------------------------------
;; DWIM Shell
;;------------------------------------------------------------------------------

(use-package! dwim-shell-command
  :after dired
  :config
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
