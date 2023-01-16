;; -*- lexical-binding: t; -*-

;;------------------------------------------------------------------------------
;; Evil Config
;;------------------------------------------------------------------------------

(after! evil
  (add-hook! debugger-mode-hook #'turn-on-evil-mode)

  (setq evil-want-C-i-jump t)

  ;; https://github.com/doomemacs/doomemacs/issues/6478
  (evil-select-search-module 'evil-search-module 'isearch)

  ;; for some reason this makes org tables really really slow
  (advice-remove 'set-window-buffer #'ad-Advice-set-window-buffer)

  ;; only substitute the 1st match by default (reverse vim behavior)
  (setq evil-ex-substitute-global t)

  ;; Switch to the new window after splitting
  (setq evil-split-window-below t
        evil-vsplit-window-right t))

;;------------------------------------------------------------------------------
;; Utility Functions
;;------------------------------------------------------------------------------

(defun open-link-or (fn)
  (cond
   ((thing-at-point 'url) #'link-hint-open-link-at-point)
   (t fn)) t)

;; (defmacro affe-find-x! (name path bind)
;;   (defun )
;;   )

(defun affe-find-home () (interactive) (affe-find "~/"))
(define-key evil-normal-state-map (kbd "C-o") #'affe-find-home)

(defun affe-find-work () (interactive) (affe-find "~/work"))
(define-key evil-normal-state-map (kbd "C-y") #'affe-find-work)

(defun affe-find-project () (interactive) (affe-find (projectile-project-root)))
(define-key evil-normal-state-map (kbd "C-p") #'affe-find-project)

(defun affe-grep-project () (interactive) (affe-grep (projectile-project-root)))
(define-key evil-normal-state-map (kbd "C-S-p") #'affe-grep-project)

(defun affe-find-notes () (interactive) (affe-find "~/Sync/notes"))
(define-key evil-normal-state-map (kbd "C-n") #'affe-find-notes)

(defun affe-find-dotfile () (interactive) (affe-find "~/.dotfiles"))
(define-key evil-normal-state-map (kbd "C-n") #'affe-find-notes)

(defun re-indent-buffer ()
  (interactive)
  (evil-indent (point-min) (point-max)))

(defun ap/tab-fallthrough ()
  (interactive)
  (or
   ;; evil jump item throws a user-error if a jumpable item is not
   ;; found, so we have to catch the error with a condition case
   ;; and turn it into a nil
   (condition-case nil (evil-jump-item) (user-error nil))
   (progn
     (print "Outline Cycle")
     (outline-cycle))))

(cond
 ((string= (system-name) "strange")   (setq preferred-terminal '("terminator" "--working-directory")))
 ((string= (system-name) "pepper")    (setq preferred-terminal '("kitty" "--directory")))
 ((string= (system-name) "larry")     (setq preferred-terminal '("terminator" "--working-directory")))
 ((string= (system-name) "cobweb")    (setq preferred-terminal '("kitty" "--directory")))
 (t '("terminator" "--working-directory") ))

(defun open-pwd-in-terminal ()
  "Opens the present working directory in terminal"
  (interactive)
  (let ((pwd (cl-case major-mode
               ;; magit mode
               ('magit-status-mode (projectile-project-root))
               ;; dired
               ('dired-mode (file-name-directory (dired-get-filename)))
               ;; default
               (t (file-name-directory (buffer-file-name))))))
    (start-process "*terminal*" nil
                   "setsid"
                   (executable-find (car preferred-terminal)) (cadr preferred-terminal) pwd)))

(defun open-buffer-in-vim ()
  "Opens the current buffer in gvim :)"
  (interactive)
  (start-process "*gvim*" nil "setsid"
                 (executable-find "gvim") (buffer-file-name)))

(defun org-make-tables-pretty ()
  "Makes org mode tables pretty in the current buffer"
  (interactive)
  (org-table-map-tables 'org-table-align))

(defun xdg-do (x)
  (call-process (executable-find "xdg-open") nil nil nil x))

(defun xdg-browse-directory ()
  "Open the current file's directory however the OS would."
  (interactive)
  (xdg-do (file-name-directory (buffer-file-name))))

(defun xdg-open-file ()
  "Open the current file however the OS would."
  (interactive)
  (xdg-do (buffer-file-name)))

(defun org-fill-paragraph-t ()
  (interactive)
  (org-fill-paragraph t))

(defun py-black () (interactive)
       (save-buffer)
       (print (shell-command-to-string (concat "black " (buffer-file-name))))
       (revert-buffer))

(defun verible-format () (interactive)
       (save-buffer)
       (print (shell-command-to-string (concat "verible-verilog-format --inplace " (buffer-file-name))))
       (revert-buffer))

(defun pyment () (interactive)
       (save-buffer)
       (print (shell-command-to-string (concat "pyment -o google -w " (buffer-file-name))))
       (revert-buffer))

;; Backspace to switch to last buffer
(defun er-switch-to-previous-buffer ()
  "Switch to previously open buffer. Repeated invocations toggle
between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;;------------------------------------------------------------------------------
;; Keybindings
;;------------------------------------------------------------------------------

(define-key evil-motion-state-map (kbd "C-t")   'nil)
(define-key evil-normal-state-map (kbd "C-t")   'nil)
(define-key evil-normal-state-map (kbd "C-t")
  (lambda () (interactive) (find-file "~/Sync/notes/todo.org")))

;;  Open the end of my billing log
(define-key evil-normal-state-map (kbd "C-S-b")
  (lambda () (interactive)
    (progn
      (find-file "~/Sync/billing/billing.org")
      (goto-char (point-max))
      (re-search-backward "TBLFM")
      (org-reveal)
      (forward-line -1)
      (forward-line -1)
      (recenter-top-bottom))))

(define-key minibuffer-mode-map
  (kbd "C-p") 'evil-paste-after)

(define-key evil-insert-state-map  (kbd "<mouse-2>") 'evil-paste-after)
(define-key evil-normal-state-map  (kbd "<mouse-2>") 'evil-paste-after)

;; Jump back and forth through files, time, and space with arrow keys
(define-key evil-normal-state-map (kbd "<mouse-8>")   'evil-jump-backward)
(define-key evil-normal-state-map (kbd "<mouse-9>")   'evil-jump-forward)

;; (define-key evil-motion-state-map "\C-b" nil)
(define-key evil-visual-state-map "\C-i" nil)


;; middle click to paste
(global-set-key (kbd "<mouse-2>") 'evil-paste-after)
(global-set-key (kbd "M-p") 'evil-paste-after)

;; Make evil-mode up/down operate in screen lines instead of logical lines
(define-key evil-motion-state-map "j" 'evil-next-visual-line)
(define-key evil-motion-state-map "k" 'evil-previous-visual-line)

;; unbind annoying emacs bindings
(define-key evil-normal-state-map "\C-p" nil)
(define-key evil-normal-state-map "\C-d" nil)
(global-set-key (kbd "M-<left>") nil)
(global-set-key (kbd "M-<right>") nil)
(global-set-key (kbd "M-`") nil)

;; Ctrl + Alt + equal to re-indent buffer
(define-key evil-normal-state-map (kbd "C-M-=")
  #'re-indent-buffer)

(define-key evil-normal-state-map (kbd "<C-tab>")
  (lambda () (interactive)
    (outline-cycle)))

(define-key evil-normal-state-map (kbd "C-<iso-lefttab>")
  (lambda () (interactive)
    (outline-hide-body)))

;; Evil numbers increment/decrement
(global-set-key (kbd "C-x") nil)
(define-key evil-normal-state-map (kbd "C-a")   'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-x")   'evil-numbers/dec-at-pt)
(define-key evil-visual-state-map (kbd "C-a")   'evil-numbers/inc-at-pt-incremental)
(define-key evil-visual-state-map (kbd "C-x")   'evil-numbers/dec-at-pt-incremental)

(define-key org-mode-map (kbd "M-q") #'org-fill-paragraph-t)
(define-key text-mode-map (kbd "M-q") #'fill-paragraph)


(define-key python-mode-map (kbd "C-c C-b") #'py-black)


(define-key evil-motion-state-map (kbd "SPC") nil)

;; Tab in normal mode shouldn't indent

(global-set-key (kbd "TAB") nil)
(define-key evil-insert-state-map (kbd "TAB") 'indent-for-tab-command)

(define-key evil-normal-state-map (kbd "DEL") 'er-switch-to-previous-buffer)
(add-hook 'verilog-mode-hook (lambda () (local-unset-key [backspace])))

;;------------------------------------------------------------------------------
;; Evil Bindings
;;------------------------------------------------------------------------------

(after! evil

  (evil-define-key 'normal system-install-run-minor-mode-map "q" #'bury-buffer)

  ;; (define-key evil-visual-state-map "\C-b" nil)
  ;; (define-key evil-normal-state-map "\C-b" nil)
  ;; (define-key evil-insert-state-map "\C-b" nil)
  ;; (evil-define-key 'motion
  ;;   latex-mode-map  "\C-b" nil)
  ;; (evil-define-key nil
  ;;   'latex-mode  "\C-b" 'tex-bold)
  ;; (evil-define-key 'visual 'latex-mode  "\C-b" 'tex-bold)
  ;; (evil-define-key 'visual 'latex-mode  "\C-i" 'tex-italic)

  ;; bindings to eval buffers

  (evil-define-key 'motion emacs-lisp-mode-map
    (kbd "RET") (lambda () (interactive) (open-link-or #'eval-buffer)))

  (evil-define-key 'motion clojure-mode-map
    (kbd "RET") (lambda () (interactive) (open-link-or #'cider-eval-buffer)))

  (evil-define-key 'motion python-mode-map
    (kbd "RET") (lambda () (interactive) (open-link-or #'python-shell-send-buffer)))

  (evil-define-key 'normal elfeed-search-mode-map
    "q" #'elfeed-kill-buffer
    "r" #'elfeed-search-update--force
    (kbd "M-RET") #'elfeed-search-browse-url))

;;------------------------------------------------------------------------------
;; Evil Leader Keys
;;------------------------------------------------------------------------------

(after! evil-leader

  (evil-leader/set-key "bt" 'org-make-tables-pretty)
  (evil-leader/set-key "bf" 'xdg-browse-directory)
  (evil-leader/set-key "ot" 'open-pwd-in-terminal)
  (evil-leader/set-key "vv" 'open-buffer-in-vim)
  (evil-leader/set-key "tt" 'doom/ivy-tasks)
  (evil-leader/set-key "pp" '+ivy/project-search)
  (evil-leader/set-key "rr" 'projectile-recentf)

  (evil-leader/set-key "rtw" 'delete-trailing-whitespace) ;; delete trailing whitespace
  (evil-leader/set-key "E" 'dired-jump)
  (evil-leader/set-key "pp" 'org-publish-current-project))

;;------------------------------------------------------------------------------
;; Doom Binds
;;------------------------------------------------------------------------------

(after! doom-keybinds

  (map! :localleader
        :map python-mode-map
        :desc "Expand macro" "m" #'macrostep-expand
        (:prefix ("e" . "eval")
                 "R" #'run-python
                 "b" #'python-shell-send-buffer
                 "d" #'python-shell-send-defun
                 "r" #'python-shell-send-region))

  (map! :leader             :desc "Open Dired"           "E"  #'dired-jump)
  (map! :leader :prefix "g" :desc "Magit Amend"          "A"  #'magit-commit-amend)
  (map! :leader :prefix "o" :desc "Open Elfeed"          "e"  #'elfeed)
  (map! :leader :prefix "g" :desc "Open Elfeed"          "pt" #'gpt-prompt)
  (map! :leader :prefix "c" :desc "Make"                 "m"  #'+make/run)
  (map! :leader :prefix "o" :desc "List flycheck errors" "l"  #'flycheck-list-errors)
  (map! :leader :prefix "f" :desc "Open dotfile"         "."  #'ap/find-file-in-dotfiles))
