;; -*- lexical-binding: t; -*-

;; Evil
;;------------------------------------------------------------------------------

(after! evil

  ;;(define-key evil-normal-state-map "zg" 'spell-fu-word-add)
  ;;(define-key evil-normal-state-map "zg" 'spell-fu-word-add)
  (define-key evil-normal-state-map "zg" #'my-save-word )
  (define-key evil-normal-state-map "z=" 'ispell-word)

  ;; Evil Bindings

  (global-evil-leader-mode)

  ;; Make evil-mode up/down operate in screen lines instead of logical lines
  (define-key evil-motion-state-map "j" 'evil-next-visual-line)
  (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
  (define-key evil-visual-state-map "j" 'evil-next-visual-line)
  (define-key evil-visual-state-map "k" 'evil-previous-visual-line)

  ;; unbind annoying emacs bindings
  (define-key evil-normal-state-map "\C-p" nil)
  (define-key evil-normal-state-map "\C-d" nil)

  (global-set-key (kbd "M-<left>") nil)
  (global-set-key (kbd "M-<right>") nil)
  (global-set-key (kbd "M-`") nil)

  ;; only substitute the 1st match by default (reverse vim behavior)
  (setq evil-ex-substitute-global t)

  ;; Switch to the new window after splitting
  (setq evil-split-window-below t
        evil-vsplit-window-right t)
  )

(after! evil-maps

  ;; Ctrl-o to search the home with fzf
  (define-key evil-normal-state-map (kbd "C-o")
    (lambda () (interactive)
      (counsel-fzf "" "~/")))

  ;; Ctrl-p to search the project root with fzf
  (define-key evil-normal-state-map (kbd "C-p")
    (lambda () (interactive)
      (counsel-fzf "" (projectile-project-root))))

  ;; Ctrl-n to search Dropbox notes
  (define-key evil-normal-state-map (kbd "C-n")
    (lambda () (interactive)
      (counsel-fzf "" "~/Dropbox/notes")))

  ;; C-t to open TODO file
  ;;  • Needed to unbind some conflicts first
  (define-key evil-motion-state-map (kbd "C-t")   'nil)
  (define-key evil-normal-state-map (kbd "C-t")   'nil)
  (define-key evil-normal-state-map (kbd "C-t")
    (lambda () (interactive) (find-file "~/Dropbox/org/todo.org")))

  ;; Ctrl + Alt + equal to re-indent buffer
  (defun re-indent-buffer ()
    (interactive)
    (evil-indent (point-min) (point-max)))
  (after! evil
    (define-key evil-normal-state-map (kbd "C-M-=")
      #'re-indent-buffer))

  (defun ap/tab-fallthrough ()
    (interactive)
    (or ; short circuiting OR
     ;; evil jump item throws a user-error if a jumpable item is not
     ;; found, so we have to catch the error with a condition case
     ;; and turn it into a nil
     (condition-case nil (evil-jump-item) (user-error nil))
     (progn
       (print "Outline Cycle")
       (outline-cycle))
     ))

  (define-key evil-normal-state-map (kbd "<C-tab>")
    (lambda () (interactive)
      (outline-cycle)))

  (define-key evil-normal-state-map (kbd "C-<iso-lefttab>")
    (lambda () (interactive)
      (outline-hide-body)))

  ;; Evil leader keys

  (defun open-buffer-in-vim ()
    (interactive)
    (start-process "gvim" (format "*gvim-%s" (buffer-file-name)) "gvim" (buffer-file-name)))

  (evil-leader/set-key "vv" 'open-buffer-in-vim)
  (evil-leader/set-key "tt" 'doom/ivy-tasks)
  (evil-leader/set-key "pp" '+ivy/project-search)
  (evil-leader/set-key "rr" 'projectile-recentf)
  (evil-leader/set-key "rtw" 'delete-trailing-whitespace) ;; delete trailing whitespace
  (evil-leader/set-key "E" 'dired-jump)
  (evil-leader/set-key "pp" 'org-publish-current-project)
  ;;(evil-leader/set-key "rr" 'fzf-recentf)
  ;;(evil-leader/set-key "S" 'magit-stage-file)
  ;;(evil-leader/set-key "er" 'eval-region)
  ;;(evil-leader/set-key "f" 'helm-locate)
  ;;(evil-leader/set-key "b" 'helm-mini) ;; map helm mini to leader
  ;;(evil-leader/set-key "tag" 'projectile-regenerate-tags)

  ;; Let enter open org mode links

  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "RET") 'org-open-at-point)

  ;; Tab in normal mode shouldn't indent

  (global-set-key (kbd "TAB") nil)
  (define-key evil-insert-state-map (kbd "TAB") 'indent-for-tab-command)

  ;; Backspace to switch to last buffer
  (defun er-switch-to-previous-buffer ()
    "Switch to previously open buffer. Repeated invocations toggle between the two most recently open buffers."
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) 1)))

  (define-key evil-normal-state-map (kbd "DEL") 'er-switch-to-previous-buffer)
  (add-hook 'verilog-mode-hook (lambda() (local-unset-key [backspace])))

  ;; Evil numbers

  (define-key evil-normal-state-map (kbd "C-a")   'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-S-a") 'evil-numbers/dec-at-pt)
  (define-key evil-visual-state-map (kbd "C-a")   'evil-numbers/inc-at-pt-incremental)
  (define-key evil-visual-state-map (kbd "C-S-a") 'evil-numbers/dec-at-pt-incremental)

  ;; Jump back and forth through files, time, and space with arrow keys

  (define-key evil-normal-state-map (kbd "<C-M-left>")   'better-jumper-jump-backward)
  (define-key evil-normal-state-map (kbd "<C-M-right>")   'better-jumper-jump-forward)

  ;; FIXME: this opens dired now...
  (defun browse-file-directory ()
    "Open the current file's directory however the OS would."
    (interactive)
    (if default-directory
        (browse-url-of-file (expand-file-name default-directory))
      (error "No `default-directory' to open")))
  (evil-leader/set-key "bf" 'browse-file-directory)

  )

;; Evil Surround
;;------------------------------------------------------------------------------

;;(after! evil
;;  (add-hook 'org-mode-hook (lambda ()
;;                             (push '(?< . ("~" . "~")) evil-surround-pairs-alist)
;;                             (push '(?< . ("=" . "=")) evil-surround-pairs-alist)
;;                             (push '(?< . ("*" . "*")) evil-surround-pairs-alist)
;;                             (push '(?< . ("/" . "/")) evil-surround-pairs-alist)
;;                             )))
