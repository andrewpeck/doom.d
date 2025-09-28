;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'markdown-mode))

(defun dwim-open-at-point ()
  (interactive)
  (cl-case major-mode
    (org-mode      (org-open-at-point))
    (hog-src-mode  (hog-follow-link-at-point))
    (markdown-mode (call-interactively #'markdown-follow-thing-at-point))
    (t             (browse-url-at-point))))

;; modify +vterm/toggle to open in pwd instead of project root
(defun my/vterm-here ()
  (interactive)
  (cl-letf (((symbol-function 'doom-project-root)
             (lambda () nil)))
    (vterm)))

(defun my/mu4e-open-inbox ()
  (interactive)
  (mu4e~headers-jump-to-maildir "/INBOX"))

(after! doom-keybinds

  ;;------------------------------------------------------------------------------
  ;; Tabs
  ;;------------------------------------------------------------------------------

  (map! "C-<tab>"   #'tab-next
        "C-S-<tab>" #'tab-previous
        "C-M-<tab>" #'tab-new
        "C-S-t"     #'tab-new
        "C-S-k"     #'tab-close
        "C-<next>"  #'tab-next
        "C-<prior>" #'tab-previous)

  ;;------------------------------------------------------------------------------
  ;; CUA
  ;;------------------------------------------------------------------------------

  (map! :nmi "C-s" #'save-buffer)
  (map! :map 'minibuffer-mode-map :i "C-v" #'yank)

  ;;------------------------------------------------------------------------------
  ;;
  ;;------------------------------------------------------------------------------

  (map! "C-c C-o" #'dwim-open-at-point)

  ;; Dired
  (map! :map dired-mode-map :nm ")" #'dired-git-info-mode)

  ;; Org
  (map! :map org-mode-map
        :i "<tab>" #'org-cycle
        :n "o" #'evil-org-open-below
        :n "O" #'evil-org-open-above
        ;; normal evil-org-end-of-line is broken
        ;; https://github.com/Somelauw/evil-org-mode/issues/50
        ;; just use the regular evil mode.. there doesn't seem to be any downside
        :v "$" #'evil-end-of-line
        :n "zs" #'org-toggle-link-display)

  ;; Hog
  (map! :mode hog-src-mode-map
        :nm "M-RET" #'hog-follow-link-at-point)

  ;; Ctrl + Alt + equal to re-indent buffer
  (map! :nm "C-M-=" #'re-indent-buffer)

  ;; Jump back and forth through files, time, and space
  (map! "C-t" 'evil-jump-backward

        "<mouse-8>" 'evil-jump-backward
        "<mouse-9>" 'evil-jump-forward

        (:map pdf-history-minor-mode-map
         :after pdf-view-mode
         "<mouse-8>" #'evil-collection-pdf-jump-backward
         "<mouse-9>" #'evil-collection-pdf-jump-forward))

  (map! "<mouse-3>" 'context-menu-open)

  (map! :v "C-i" nil)

  (map! :after lispy
        :map lispy-mode-map "C-e" nil
        :map lispy-mode-map-base "C-e" nil
        :map lispy-mode-map-evilcp "C-e" nil
        :map lispy-mode-map-lispy "C-e" nil
        :map lispy-mode-map-paredit "C-e" nil
        :map lispy-mode-map-parinfer "C-e" nil
        :map lispy-mode-map "M-<return>" nil
        :map lispy-mode-map-evilcp "M-<return>" nil
        :map lispy-mode-map-lispy "M-<return>" nil
        :map lispy-mode-map "M-RET" nil
        :map lispy-mode-map-evilcp "M-RET" nil
        :map lispy-mode-map-lispy "M-RET" nil)

  (map! :map evil-motion-state-map "C-e" nil)
  (map! :map emacs-lisp-mode-map   "C-e" #'eval-print-last-sexp)

  ;; middle click to paste
  (map! "<mouse-2>" 'evil-paste-after
        "M-p"       'evil-paste-after

        ;; C-p to paste in the minibuffer
        :map minibuffer-mode-map "C-p" #'evil-paste-after)

  (map!
   ;; Make evil-mode up/down operate in screen lines instead of logical lines
   :nm "j" 'evil-next-visual-line
   :nm "k" 'evil-previous-visual-line

   ;; Addition / Subtraction
   :v  "C-a" 'evil-numbers/inc-at-pt-incremental
   :v  "C-x" 'evil-numbers/dec-at-pt-incremental
   :nm "C-a" 'evil-numbers/inc-at-pt
   :nm "C-x" 'evil-numbers/dec-at-pt)

  ;; Fill Paragraph
  (map! :map text-mode-map "M-q" #'fill-paragraph)
  (map! :after org :map org-mode-map "M-q" #'org-fill-paragraph-t)
  (map! :after latex :map latex-mode-map "M-q" #'ap/line-fill-paragraph)

  ;; Tab in normal mode shouldn't indent
  (map! :nmv "TAB" 'nil)

  ;; Backspace to jump to previous buffer
  (map! :nm "DEL" 'evil-switch-to-windows-last-buffer)

  (map! :map emacs-lisp-mode-map
        :nm "RET"  (lambda () (interactive) (open-link-or #'eval-defun))
        :nm "M-RET" #'eval-buffer)

  (map! :map clojure-mode-map
        :after cider-mode
        :m "RET" (lambda () (interactive) (open-link-or #'cider-eval-defun-at-point))
        :m "M-RET" #'cider-eval-buffer)

  (map! :map python-mode-map
        :m "RET" (lambda () (interactive) (open-link-or #'python-shell-send-defun))
        :m "M-RET" #'python-shell-send-buffer)

  (map! :map system-install-run-minor-mode-map
        :after system-install
        :n "q" #'bury-buffer)

  (declare-function popup/quit-window "+hacks" (args))
  (map! :after archive-mode :map archive-mode-map "-" #'popup/quit-window)

  (map! :localleader
        :map python-mode-map
        :after python
        (:prefix ("e" . "eval")
                 "R" #'run-python
                 "b" #'python-shell-send-buffer
                 "d" #'python-shell-send-defun
                 "r" #'python-shell-send-region))

  (map! :localleader
        :map vhdl-mode-map
        :after vhdl-mode
        :desc "Beautify"
        (:prefix ("b" . "Beautify")
                 "b" #'vhdl-beautify-buffer
                 "r" #'vhdl-beautify-region))

  (map! :localleader
        :map verilog-mode-map
        :after verilog-mode
        :desc "Align Ports"
        (:prefix ("a" . "align")
                 "p" #'verilog-align-ports
                 "d" #'verilog-pretty-declarations
                 "=" #'verilog-pretty-expr))


  (map! :leader :prefix "n" :desc "Narrow to Region"  "r" #'narrow-to-region)
  (map! :leader :prefix "n" :desc "Narrow to Defun"   "f" #'narrow-to-defun)
  (map! :leader :prefix "n" :desc "Widen (Unnarrow)"  "w" #'widen)
  (map! :leader :prefix "n" :desc "Narrow to Lines"   "l" #'consult-focus-lines)

  (map! :leader :prefix "s" :desc "SVG Tag Mode"         "vg" #'svg-tag-mode)

  (map! :localleader :map dired-mode-map :prefix "m" :desc "Dired Filter Mode"    "f"  #'dired-filter-mode :map dired-mode-map)

  (map! :leader :prefix "i" :desc "Insert Date"          "d"  #'insert-current-date)

  (map! :leader :desc "Open Dired"           "E"  #'dired-jump)

  (map! :leader (:prefix "o"
                 :desc "Open Elfeed"          "e"  #'elfeed
                 :desc "Open Terminal Here"   "K"  #'open-pwd-in-terminal
                 :desc "Open File"            "o"  #'xdg-open-file
                 :desc "Open Directory"       "O"  #'xdg-browse-directory
                 :desc "Open org agenda"      "x"  #'org-agenda-and-todo
                 :desc "GPT Prompt"           "ai" #'gpt-prompt
                 :desc "Calculator"           "c"  #'calc
                 :desc "Vterm Toggle"         "t"  #'my/vterm-here
                 :desc "Vterm Toggle Root"    "T"  #'+vterm/here
                 :desc "Mu4e Inbox"           "i"  #'my/mu4e-open-inbox
                 :desc "Quick Calc"           "C"  #'quick-calc))

  (map! :leader
        (:prefix-map ("r" . "replace")
         :desc "Replace Symbol"   "s" #'query-replace-symbol
         :desc "Replace Query"    "q" #'query-replace
         :desc "Replace Globally" "g" #'replace-string))

  (map! :leader
        (:prefix-map ("d" . "do")
                     (:prefix-map ("p" . "package")
                      :desc "Package Install" "i" #'system-install
                      :desc "Package Remove"  "r" #'system-install-remove-package)))

  (map! :leader :prefix "c" :desc "Make"                 "m"  #'+make/run)
  (map! :leader :prefix "v" :desc "Toggle Visual Wrap"   "w"  #'ap/toggle-wrap)
  (map! :leader :prefix "t" :desc "Toggle Dark Mode"     "d"  #'toggle-dark-mode)

  (map! :leader :prefix "b" :desc "Format Buffer" "f"
        (lambda ()
          (interactive)
          (call-interactively #'apheleia-format-buffer))))
