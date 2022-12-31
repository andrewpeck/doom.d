;; config.el -*- lexical-binding: t; -*-
;;
;; https://github.com/alphapapa/taxy.el
;;
;; TODO tabular-like alignment
;; TODO magit.sh like functionality
;; TODO outline folding
;;
;; Tecosaur: https://github.com/tecosaur/emacs-config/blob/master/config.org
;; https://svn.red-bean.com/repos/kfogel/trunk/.emacs
;; Steve Purcell: https://github.com/purcell/emacs.d/
;; Henrik: https://github.com/hlissner/doom-emacs-private/blob/master/config.el
;; https://github.com/caisah/emacs.dz
;; https://github.com/danilevy1212/doom

(load! "~/.doom.d/custom.el")
(load! "~/.doom.d/lisp/monochrome-theme.el")

(setq lsp-julia-default-environment "~/.julia/environments/v1.6")
(require 'delight)

(delight '+org-pretty-mode         " org-pretty " "org")
(delight 'better-jumper-local-mode ""             "better-jumper")
(delight 'company-mode             ""             "company")
(delight 'dtrt-indent-mode         ""             "dtrt-indent")
(delight 'eldoc-mode               ""             "eldoc")
(delight 'evil-escape-mode         ""             "evil-escape")
(delight 'evil-goggles-mode        ""             "evil-goggles")
(delight 'evil-org-mode            ""             "evil-org")
(delight 'evil-snipe-local-mode    ""             "evil-snipe")
(delight 'evil-traces-mode         ""             "evil-traces")
(delight 'evil-traces-mode         ""             "evil-traces")
(delight 'gcmh-mode                ""             "gcmh")
(delight 'hi-lock-mode             ""             "hi-lock")
(delight 'lispy-mode               ""             "lispy")
(delight 'lispyville-mode          ""             "lispyville")
(delight 'org-indent-mode          ""             "org-indent")
(delight 'outline-minor-mode       ""             "outline")
(delight 'persp-mode               "view"         "perspective-mode")
(delight 'projectile-mode          ""             "projectile")
(delight 'subword-mode             ""             "subword")
(delight 'undo-tree-mode           ""             "undo-tree")
(delight 'which-key-mode           ""             "which-key")
(delight 'whitespace-mode          ""             "whitespace")
(delight 'ws-butler-mode           ""             "ws-butler")
(delight 'yas-minor-mode           ""             "yasnippet")
(delight 'evil-markdown-mode       ""             "evil-markdown")
(delight 'markdown-mode            "Md"           "markdown")
(delight 'poly-markdown-mode       " Pm"          "poly-markdown")

(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length.
Containing LEFT, and RIGHT aligned respectively."
  (let ((available-width
         (- (window-total-width)
            (+ (length (format-mode-line left))
               (length (format-mode-line right))))))
    (append left
            (list (format (format "%%%ds" available-width) ""))
            right)))

(setq mode-line-buffer-identification
      (propertized-buffer-identification "%b"))

(setq flycheck-mode-line '(:eval (replace-regexp-in-string
                                  "FlyC" ""
                                  (flycheck-mode-line-status-text))))

'(:eval (propertize (concat "\t[" mode-name "] %l:%i\t") 'face '(:foreground "black" :height 0.9 :weight normal)
                    'help-echo (buffer-file-name)))

(defun flycheck-mode-line-status-text (&optional status)
  "Get a text describing STATUS for use in the mode line.
STATUS defaults to `flycheck-last-status-change' if omitted or
nil."
  (pcase (or status flycheck-last-status-change)

    (`not-checked "∄")
    (`no-checker "⛌")
    (`running "⟳")
    (`errored "‼")
    (`finished
     (let-alist (flycheck-count-errors flycheck-current-errors)
       (if (or .error .warning)
           (concat
            "" (propertize (format "%s" (or .error 0) ) 'face '(:inherit error))
            "|" (propertize (format "%s" (or .warning 0)) 'face '(:inherit warning)))
         " ✓")))
    (`interrupted ".")
    (`suspicious "?")))

(setq-default mode-line-format
              '((:eval (simple-mode-line-render
                        ;; Left.
                        `("%e "
                          mode-line-mule-info
                          evil-mode-line-tag
                          mode-line-modified " "
                          mode-line-buffer-identification)

                        ;; Right.
                        `("L%l:C%c:%p"
                          (vc-mode vc-mode)
                          mode-line-frame-identification
                          mode-line-modes
                          flycheck-mode-line
                          " ")))))

;;------------------------------------------------------------------------------
;; to file
;;------------------------------------------------------------------------------
;; (setq langtool-java-classpath "/usr/share/languagetool:/usr/share/java/languagetool/*")
;;
(after! langtool
  (setq langtool-java-classpath "/snap/languagetool/current/usr/share/java")

  (setq langtool-language-tool-server-jar "/snap/languagetool/current/usr/bin/languagetool.jar"))

(setq  flycheck-check-syntax-automatically
       '(save idle-buffer-switch mode-enabled))

;; disable confusing undo-fu behavior
;; https://gitlab.com/ideasman42/emacs-undo-fu/-/issues/6
(after! undo-fu
  (setq undo-fu-ignore-keyboard-quit t))

(add-to-list 'warning-suppress-types '(iedit))
(remove-hook
 '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)

;;------------------------------------------------------------------------------
;; Loads
;;------------------------------------------------------------------------------

(add-to-list 'load-path (expand-file-name "~/.doom.d/"))
(add-to-list 'load-path (expand-file-name "~/.doom.d/lisp/"))
(add-to-list 'load-path (expand-file-name "~/.doom.d/lisp/doctor/"))
(add-to-list 'load-path (expand-file-name "~/.doom.d/lisp/hdl_deps/"))

(let ((default-directory (expand-file-name "lisp" doom-user-dir)))
  (normal-top-level-add-subdirs-to-load-path))

;; (add-to-list 'load-path (expand-file-name "~/.doom.d/scad-preview/"))
;; https://github.com/hlissner/doom-emacs/issues/1213

(if (string= (system-name) "larry")
    (load! "~/.doom.d/config-mail.el"))

;; start:sort
(use-package! doctor :load-path "~/.doom.d/lisp/doctor/doctor.el")
(use-package! hog :load-path "~/.doom.d/lisp/hog-emacs/hog.el")
(use-package! regulator :load-path "~/.doom.d/lisp/regulator.el")
(use-package! system-install :load-path "~/.doom.d/lisp/system-install/system-install.el")
(use-package! tracking :load-path "~/.doom.d/lisp/tracking.el")
(use-package! ucf-mode :load-path "~/.doom.d/lisp/ucf-mode.el")
(use-package! vivado-mode :load-path "~/.doom.d/lisp/vivado-mode.el")
;; end:sort

;; start:sort
(load! "~/.doom.d/config-align.el")
(load! "~/.doom.d/config-appearance.el")
(load! "~/.doom.d/config-company.el")
(load! "~/.doom.d/config-doom.el")
(load! "~/.doom.d/config-evil.el")
(load! "~/.doom.d/config-flycheck.el")
(load! "~/.doom.d/config-git.el")
(load! "~/.doom.d/config-langs.el")
(load! "~/.doom.d/config-lsp.el")
(load! "~/.doom.d/config-org.el")
(load! "~/.doom.d/config-random.el")
(load! "~/.doom.d/config-scad.el")
(load! "~/.doom.d/config-tex.el")
(load! "~/.doom.d/config-vhdl.el")
(load! "~/.doom.d/gerb-view.el")
(load! "~/.doom.d/lisp/work-plotting.el")
;; (load! "~/.doom.d/config-modeline.el")
;; (load! "~/.doom.d/lisp/regulator.el")
;; (load! "~/.doom.d/lisp/scimax-org-return.el")
;; (load! "~/.doom.d/lisp/verilog-port-copy.el")
;; end:sort

;;------------------------------------------------------------------------------
;; Functions for alphabetically sorting items
;;------------------------------------------------------------------------------

(defun sort-code-block (comment-char)
  "Alphabetically sorts code blocks in a file, starting with #
start:sort and ending with # end:sort, where # is the comment
char of the language you are editing"
  (save-excursion
    (let ((home (point))
          (start-search (concat "^\s*" comment-char " start:sort"))
          (end-search (concat "^\s*" comment-char " end:sort")))
      (goto-char (point-min))
      (while (re-search-forward start-search nil t)

        (forward-line -1)
        (let ((start (re-search-forward start-search nil t))
              (end
               ;; want to get the match *before* end:sort
               (- (progn (re-search-forward end-search nil t)
                         (match-beginning 0)) 1)))
          (sort-lines 'nil start end)))
      (goto-char home)))
  ;; make sure to return nil here for write-contents-functions
  nil)

(defun sort-elisp-block ()
  (interactive)
  (save-excursion
    (sort-code-block ";;")))

;; Local Variables:
;; eval: (make-variable-buffer-local 'write-contents-functions)
;; eval: (add-hook 'write-contents-functions (lambda () (when (fboundp 'sort-elisp-block) (sort-elisp-block))) nil t)
;; End:
