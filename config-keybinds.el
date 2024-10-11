;; -*- lexical-binding: t; -*-

;;------------------------------------------------------------------------------
;; Evil Config
;;------------------------------------------------------------------------------

(after! evil
  (add-hook! 'debugger-mode-hook #'turn-on-evil-mode)

  ;; Whether C-i jumps forward in the jump list (like Vim).
  (setq evil-want-C-i-jump t)

  ;; https://github.com/doomemacs/doomemacs/issues/6478
  ;; Configuration A
  (setq org-fold-core-style 'overlays)
  (evil-select-search-module 'evil-search-module 'evil-search)

  ;; Configuration B
  ;; (setq org-fold-core-style 'text-properties)
  ;; (evil-select-search-module 'evil-search-module 'isearch)

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
   ((thing-at-point 'url) (link-hint-open-link-at-point))
   (t (call-interactively fn))))

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

(setq preferred-terminal
      (let* ((suffixes '((terminator . "--working-directory")
                         (konsole . "--workdir")))
             (pref-shell (pcase (system-name)
                           ("pepper" 'terminator)
                           ("larry" 'terminator)
                           ("cobweb" 'terminator)
                           ("strange" 'terminator)
                           (_ 'konsole)))
             (assl (assoc pref-shell suffixes)))
        (list (symbol-name (car assl)) (cdr assl))))

(defun get-pwd ()
  (cl-case major-mode
    ;; magit mode
    (magit-status-mode (projectile-project-root))
    ;; dired
    (dired-mode (file-name-directory (dired-get-filename)))
    ;; default
    (t (file-name-directory (buffer-file-name)))))

(defun open-pwd-in-terminal ()
  "Opens the present working directory in terminal"
  (interactive)
  (call-process (executable-find (car preferred-terminal)) nil 0 nil
                (cadr preferred-terminal) (get-pwd)))

(defun open-buffer-in-vim ()
  "Opens the current buffer in gvim."
  (interactive)
  (call-process (executable-find "gvim") nil 0 nil (buffer-file-name)))

(defun org-make-tables-pretty ()
  "Makes org mode tables pretty in the current buffer."
  (interactive)
  (org-table-map-tables 'org-table-align))

(defun xdg-do (x)
  (call-process (executable-find "xdg-open") nil nil nil x))

(defun xdg-browse-directory ()
  "Open the current file's directory however the OS would."
  (interactive)
  (xdg-do (get-pwd)))

(defun xdg-open-file ()
  "Open the current file however the OS would."
  (interactive)
  (xdg-do (buffer-file-name)))

(defun org-fill-paragraph-t ()
  (interactive)
  (org-fill-paragraph t))

(defun py-black ()
  "Format python file with black."
  (interactive)
  (save-buffer)
  (let ((pt (point)))
    (save-excursion
      (if (not (executable-find "black"))
          (error "Python Black not found. Please install (pip install black)")
        (shell-command-on-region
         (point-min) (point-max)        ; beginning and end of buffer
         "black -l 100 -"               ; command and parameters
         (current-buffer)               ; output buffer
         t                              ; replace?
         "*Python Black Error Buffer*"  ; name of the error buffer
         nil))) ; show error buffer?
    (goto-char pt)))

(defun autopep ()
  "Format python file with autopep."
  (interactive)
  (save-buffer)
  (let ((pt (point)))
    (if (not (executable-find "black"))
        (error "Autopep not found. Please install (pip install autopep8)")
      (shell-command-on-region
       (point-min) (point-max)         ; beginning and end of buffer
       "autopep8 -"                    ; command and parameters
       (current-buffer)                ; output buffer
       t                               ; replace?
       "*Python Autopep Error Buffer*" ; name of the error buffer
       nil))
    (goto-char pt)))                   ; show error buffer?

(defun verible-format ()
  "Format verilog file with verible."
  (interactive)
  (save-buffer)
  (if (not (executable-find "verible-verilog-format"))
      (message "verible-verilog-format not found")
    (print (shell-command-to-string
            (string-join `("verible-verilog-format"
                           "--inplace "
                           "--port_declarations_alignment    align"
                           "--port_declarations_indentation  wrap"
                           "--named_port_alignment           align"
                           "--assignment_statement_alignment align"
                           "--formal_parameters_alignment"   "align"
                           "--try_wrap_long_lines"           "false"
                           "--port_declarations_right_align_unpacked_dimensions true"
                           "--struct_union_members_alignment align"
                           "--formal_parameters_indentation  indent"
                           "--named_parameter_alignment      align"
                           "--named_parameter_indentation    indent"
                           ,(buffer-file-name)) " "))))
  (revert-buffer))

(defun pyment ()
  "Format python file with pyment"
  (interactive)
  (save-buffer)
  (if (not (executable-find "pyment"))
      (message "Python pyment not found. Please install (pip install pyment)")
    (shell-command-on-region
     (point-min) (point-max)            ; beginning and end of buffer
     "pyment -o google -w -"            ; command and parameters
     (current-buffer)                   ; output buffer
     t                                  ; replace?
     "*Python Pyment Error Buffer*"     ; name of the error buffer
     t)))                               ; show error buffer?

;; Backspace to switch to last buffer
(defun switch-to-previous-buffer ()
  "Switch to previously open buffer. Repeated invocations toggle
between the two most recently open buffers."
  (interactive)
  ;; (evil-switch-to-windows-last-buffer)
  (let* ((current (current-buffer))
         (other (other-buffer current 1)))
    (when (and  (buffer-file-name other)
                (buffer-file-name current))
      (switch-to-buffer other))))

(defun open-todo ()
  "Open my todo file"
  (interactive)
  (find-file +org-capture-todo-file))

(defun org-agenda-and-todo ()
  "Open the full org agenda + todo"
  (interactive)
  (org-agenda nil "n"))

(defun open-timesheet ()
  "Open my EDF timesheet"
  (interactive)
  (find-file "~/work/billing/billing.org")
  (goto-char (point-max))
  (re-search-backward "TBLFM")
  (org-reveal)
  (forward-line -1)
  (forward-line -1)
  (recenter-top-bottom))

;;------------------------------------------------------------------------------
;; Keybindings
;;------------------------------------------------------------------------------

(after! lispy
  (define-key lispy-mode-map        (kbd  "M-<return>") nil)
  (define-key lispy-mode-map-evilcp (kbd  "M-<return>") nil)
  (define-key lispy-mode-map-lispy  (kbd  "M-<return>") nil)
  (define-key lispy-mode-map        (kbd  "M-RET") nil)
  (define-key lispy-mode-map-evilcp (kbd  "M-RET") nil)
  (define-key lispy-mode-map-lispy  (kbd  "M-RET") nil))

(after! evil-maps

  (evil-define-key '(motion normal insert) 'global
    (kbd "C-s") #'save-buffer)

  (evil-define-key '(motion normal insert) 'global
    (kbd "C-c C-o")
    (defun dwim-open-at-point ()
      (interactive)
      (cl-case major-mode
        (org-mode      (org-open-at-point))
        (hog-mode      (hog-follow-link-at-point))
        (markdown-mode (markdown-follow-thing-at-point))
        (t             (browse-url-at-point)))))

  ;; Dired
  (evil-define-key '(motion normal) dired-mode-map
    (kbd ")") #'dired-git-info-mode)

  ;; Org
  (evil-define-key '(insert) org-mode-map
    (kbd "<tab>") #'org-cycle)

  ;; normal evil-org-end-of-line is broken
  ;; https://github.com/Somelauw/evil-org-mode/issues/50
  ;; just use the regular evil mode.. there doesn't seem to be any downside
  (evil-define-key 'visual org-mode-map
    (kbd "$") #'evil-end-of-line)
  ;; (add-hook 'org-mode-hook (lambda () (define-key evil-visual-state-map "$" #'evil-end-of-line)))

  (evil-define-key 'normal org-mode-map
    (kbd "zs") #'org-toggle-link-display)

  (evil-define-key 'normal org-mode-map
    (kbd "o") #'evil-org-open-below)
  (evil-define-key 'normal org-mode-map
    (kbd "O") #'evil-org-open-above)

  ;; (evil-define-key nil org-mode-map
  ;;   (kbd "M-RET")
  ;;   (lambda ()
  ;;     (interactive)
  ;;     (org-meta-return t)))

  ;; Hog
  (evil-define-key '(motion normal) hog-src-mode-map
    (kbd "M-RET") #'hog-follow-link-at-point)

  ;; Ctrl + Alt + equal to re-indent buffer
  (evil-define-key '(motion normal) 'global
    (kbd "C-M-=") #'re-indent-buffer)

  ;; ???
  (evil-define-key '(motion normal) 'global
    (kbd "<C-tab>") (lambda () (interactive) (outline-cycle)))
  (evil-define-key '(motion normal) 'global
    (kbd "C-<iso-lefttab>") (lambda () (interactive) (outline-hide-body)))

  ;; Bindings to open files
  (evil-define-key '(normal motion) 'global
    (kbd "C-S-t") #'open-todo
    (kbd "C-S-b") #'open-timesheet)

  ;; Jump back and forth through files, time, and space with arrow keys

  (evil-define-key '(normal insert motion) 'global
    (kbd "C-t") 'evil-jump-backward)

  ;; unbind these from the global map
  (evil-define-key '(motion normal visual insert) 'global
    (kbd "<mouse-8>") nil
    (kbd "<mouse-9>") nil)

  ;; Jump back and forth through files, time, and space with arrow keys
  (evil-define-key nil 'global
    (kbd "<mouse-8>") 'evil-jump-backward
    (kbd "<mouse-9>") 'evil-jump-forward)

  (evil-define-key nil pdf-history-minor-mode-map
    (kbd "<mouse-8>") #'evil-collection-pdf-jump-backward
    (kbd "<mouse-9>") #'evil-collection-pdf-jump-forward)

  (evil-define-key nil 'global
    (kbd "<mouse-3>") 'context-menu-open)

  (evil-define-key 'visual 'global
    (kbd "C-i") nil)

  ;; middle click to paste
  (evil-define-key nil 'global
    (kbd "<mouse-2>") 'evil-paste-after
    (kbd "M-p")       'evil-paste-after)

  ;; C-p to paste in the minibuffer
  (evil-define-key nil minibuffer-mode-map
    (kbd "C-p") #'evil-paste-after)

  ;; Make evil-mode up/down operate in screen lines instead of logical lines
  (evil-define-key '(motion normal) 'global
    "j" 'evil-next-visual-line
    "k" 'evil-previous-visual-line)

  ;; Addition / Subtraction
  (evil-define-key '(motion normal) 'global
    (kbd "C-a") 'evil-numbers/inc-at-pt
    (kbd "C-x") 'evil-numbers/dec-at-pt)
  (evil-define-key '(visual) 'global
    (kbd "C-a") 'evil-numbers/inc-at-pt-incremental
    (kbd "C-x") 'evil-numbers/dec-at-pt-incremental)

  ;; Fill Paragraph
  (evil-define-key nil text-mode-map
    (kbd "M-q") #'fill-paragraph)
  (evil-define-key nil org-mode-map
    (kbd "M-q") #'org-fill-paragraph-t)
  (evil-define-key nil latex-mode-map
    (kbd "M-q") #'ap/line-fill-paragraph)

  ;; Backspace to jump to previous buffer
  (evil-define-key '(normal motion) 'global
    (kbd "DEL") 'switch-to-previous-buffer)

  (evil-define-key '(normal motion) python-mode-map
    (kbd "C-c C-b") #'py-black)

  (evil-define-key '(normal motion) emacs-lisp-mode-map
    (kbd "RET") (lambda () (interactive) (open-link-or #'eval-defun))
    (kbd "M-RET") #'eval-buffer)

  (evil-define-key 'motion clojure-mode-map
    (kbd "RET") (lambda () (interactive) (open-link-or #'cider-eval-defun-at-point))
    (kbd "M-RET") #'cider-eval-buffer)

  (evil-define-key 'motion python-mode-map
    (kbd "RET") (lambda () (interactive) (open-link-or #'python-shell-send-defun))
    (kbd "M-RET") #'python-shell-send-buffer)

  (evil-define-key 'normal system-install-run-minor-mode-map
    "q" #'bury-buffer)

  (evil-define-key 'normal elfeed-search-mode-map
    (kbd  "q")    #'elfeed-kill-buffer
    (kbd  "r")    #'elfeed-search-update--force
    (kbd "M-RET") #'elfeed-search-browse-url)

  ;; (define-key evil-visual-state-map "\C-b" nil)
  ;; (define-key evil-normal-state-map "\C-b" nil)
  ;; (define-key evil-insert-state-map "\C-b" nil)
  ;; (evil-define-key 'motion
  ;;   latex-mode-map  "\C-b" nil)
  ;; (evil-define-key nil
  ;;   'latex-mode  "\C-b" 'tex-bold)
  ;; (evil-define-key 'visual 'latex-mode  "\C-b" 'tex-bold)
  ;; (evil-define-key 'visual 'latex-mode  "\C-i" 'tex-italic)

  (evil-define-key nil reftex-toc-mode-map
    (kbd "<return>") #'reftex-toc-goto-line))

(evil-define-key nil archive-mode-map
  (kbd "-")   #'+popup/quit-window)

;;------------------------------------------------------------------------------
;; Doom Binds
;;------------------------------------------------------------------------------

(after! doom-keybinds

  (map! :localleader
        :map python-mode-map
        (:prefix ("e" . "eval")
                 "R" #'run-python
                 "b" #'python-shell-send-buffer
                 "d" #'python-shell-send-defun
                 "r" #'python-shell-send-region))

  (map! :localleader
        :map vhdl-mode-map
        :desc "Beautify"
        (:prefix ("b" . "Beautify")
                 "b" #'vhdl-beautify-buffer
                 "r" #'vhdl-beautify-region))

  (map! :localleader
        :map verilog-mode-map
        :desc "Align Ports"
        (:prefix ("a" . "align")
                 "p" #'verilog--align-ports
                 "d" #'verilog-pretty-declarations
                 "=" #'verilog-pretty-expr))


  (map! :leader :prefix "o" :desc "Vterm Here"  "T" (lambda () (interactive) (+vterm/here t)))
  (map! :leader :prefix "n" :desc "Narrow to Region"  "r" #'narrow-to-region)
  (map! :leader :prefix "n" :desc "Widen (Unnarrow)"  "w" #'widen)

  (map! :leader :prefix "s" :desc "SVG Tag Mode"         "vg" #'svg-tag-mode)
  (map! :localleader :map org-mode-map :prefix "m" :desc "Latexify"             "lp" #'org-latex-preview-all :map org-mode-map)
  (map! :localleader :map org-mode-map :prefix "m" :desc "De-latexify"          "lP" #'org-latex-preview-clear :map org-mode-map)

  (map! :localleader :map dired-mode-map :prefix "m" :desc "Dired Filter Mode"    "f"  #'dired-filter-mode :map dired-mode-map)

  (map! :leader :prefix "i" :desc "Insert Date"          "d"  #'insert-current-date)

  (map! :leader             :desc "Open Dired"           "E"  #'dired-jump)
  (map! :leader :prefix "g" :desc "Magit Amend"          "A"  #'magit-commit-amend)
  (map! :leader :prefix "o" :desc "Open Elfeed"          "e"  #'elfeed)
  (map! :leader :prefix "o" :desc "Open Terminal Here"   "K"  #'open-pwd-in-terminal)

  (map! :leader :prefix "r" :desc "Replace Symbol"       "s"  #'query-replace-symbol)
  (map! :leader :prefix "r" :desc "Replace Query"        "q"  #'query-replace)
  (map! :leader :prefix "r" :desc "Replace Globally"     "g"  #'replace-string)

  (map! :leader :prefix "g" :desc "GPT Prompt"           "pt" #'gpt-prompt)
  (map! :leader :prefix "c" :desc "Make"                 "m"  #'+make/run)
  (map! :leader :prefix "o" :desc "List flycheck errors" "l"  #'flycheck-list-errors)
  (map! :leader :prefix "o" :desc "Open org agenda"      "x"  #'org-agenda-and-todo)
  (map! :leader :prefix "y" :desc "Org Link Copy"        "y"  #'org-link-copy)
  (map! :leader :prefix "v" :desc "Toggle Visual Wrap"   "w"  #'ap/toggle-wrap)
  (map! :leader :prefix "t" :desc "Toggle Dark Mode"     "d"  #'toggle-dark-mode))

  (map! :leader :prefix "b" :desc "Format Buffer" "f"
        (lambda ()
          (interactive)
          (call-interactively #'apheleia-format-buffer)
          (message "Formatted buffer...")))

(map! :localleader
      :map org-mode-map
      :prefix "a"
      :desc "Download Screenshot" "c" #'org-download-screenshot
      :desc "Download Clipboard" "p" #'org-download-clipboard
      :desc "Download Yank" "P" #'org-download-yank
      :desc "Edit Image" "e" #'org-download-edit
      :desc "Delete Image" "d" #'org-download-delete
      :desc "Move Image" "m" #'org-download-rename)


(map! :leader
      :prefix "o"
      :desc "Mu4e Inbox"  "i"
      (lambda () (interactive) (mu4e~headers-jump-to-maildir "/INBOX")))


;; (map! :leader
;;       :prefix "n"
;;       :desc "Org-Roam-Insert"     "i" #'org-roam-node-insert
;;       :desc "Org-Roam-Find"       "/" #'org-roam-find-file
;;       :desc "Org-Roam-Buffer"     "r" #'org-roam
;;       :desc "Org-Roam-Show-Graph" "g" #'org-roam-graph)
