;; -*- lexical-binding: t; -*-
;;
;;------------------------------------------------------------------------------
;; Keybindings
;;------------------------------------------------------------------------------

;; use mouse forward/backward to jump between buffers
(map! :n [mouse-8] #'previous-buffer
      :n [mouse-9] #'next-buffer)

(after! lispy
  (define-key lispy-mode-map        (kbd  "C-e") nil)
  (define-key lispy-mode-map-base   (kbd  "C-e") nil)
  (define-key lispy-mode-map-evilcp   (kbd  "C-e") nil)
  (define-key lispy-mode-map-lispy   (kbd  "C-e") nil)
  (define-key lispy-mode-map-paredit   (kbd  "C-e") nil)
  (define-key lispy-mode-map-parinfer   (kbd  "C-e") nil)
  (define-key lispy-mode-map        (kbd  "M-<return>") nil)
  (define-key lispy-mode-map-evilcp (kbd  "M-<return>") nil)
  (define-key lispy-mode-map-lispy  (kbd  "M-<return>") nil)
  (define-key lispy-mode-map        (kbd  "M-RET") nil)
  (define-key lispy-mode-map-evilcp (kbd  "M-RET") nil)
  (define-key lispy-mode-map-lispy  (kbd  "M-RET") nil))

(after! evil-maps

  (define-key global-map (kbd "C-/") #'embrace-commander)

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

  (map! :leader
        :desc "Org Capture TODO"
        "T" (lambda () (interactive) (org-capture nil "t")))

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

  (define-key evil-motion-state-map (kbd "C-e") nil)
  (define-key emacs-lisp-mode-map (kbd "C-e") #'eval-print-last-sexp)

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

  ;; Tab in normal mode shouldn't indent
  (evil-define-key '(insert motion normal visual) 'global
    (kbd "TAB") 'nil)
  (define-key global-map (kbd "TAB") nil)

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
  (map! :leader :prefix "o" :desc "XDG Open File"        "o"  #'xdg-open-file)

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
