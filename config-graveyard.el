;;; mu4e
;;------------------------------------------------------------------------------

;;;;;(after! mu4e
;;;;;  (setq mail-user-agent 'mu4e-user-agent)
;;;;;  (setq mu4e-drafts-folder "/[Gmail].Drafts")
;;;;;  (setq mu4e-sent-folder   "/[Gmail].Sent Mail")
;;;;;  (setq mu4e-trash-folder  "/[Gmail].Trash")
;;;;;  (setq mu4e-sent-messages-behavior 'delete)
;;;;;  (setq mu4e-maildir-shortcuts
;;;;;        '( ("/INBOX"               . ?i)
;;;;;           ("/[Gmail].Sent Mail"   . ?s)
;;;;;           ("/[Gmail].Trash"       . ?t)
;;;;;           ("/[Gmail].All Mail"    . ?a)))
;;;;;  (setq mu4e-get-mail-command "offlineimap")
;;;;;  (setq mu4e-use-fancy-chars t)
;;;;;  (setq mu4e-view-show-addresses t)
;;;;;  (setq mu4e-view-show-images t)
;;;;;  (add-hook 'mu4e-view-mode-hook #'visual-line-mode)
;;;;;  (add-hook 'mu4e-compose-mode-hook 'flyspell-mode)
;;;;;)
;;;;;


;;(defvar +company-backend-alist
;;  '((text-mode company-yasnippet company-dabbrev  company-ispell)
;;    (prog-mode company-yasnippet company-capf )
;;    (conf-mode company-yasnippet company-capf company-dabbrev-code )))

;;(set-company-backend! 'python-mode-hook '(company-yasnippet company-jedi
;;                                       company-files
;;                                      company-keywords company-capf company-dabbrev-code
;;                                      company-etags company-dabbrev))

;;(setq company-frontends '(company-preview-if-just-one-frontend company-box-frontend company-echo-metadata-frontend))
;;(setq company-frontends '(company-preview-if-just-one-frontend  company-echo-metadata-frontend))
;;(setq company-frontends '(company-box-frontend company-echo-metadata-frontend))
;;(add-to-list 'company-backends 'company-irony)

;;(define-key company-active-map (kbd "<return>") nil)
;;(define-key company-active-map (kbd "<tab>") #'company-complete-selection )

;;   (set-company-backend! '(company-tabnine company-yasnippet  company-files))
;;                                         ;(set-company-backend! 'org-mode '(company-roam company-files company-dabbrev))
;;   ;;(set-company-backend! '(tcl-mode) '(company-tabnine company-yasnippet))
;;   (set-company-backend! '(prog-mode) '(company-tabnine company-yasnippet))
;;   )

;;; "Recycle Bin"

;; Scrolling.
;; Good speed and allow scrolling through large images (pixel-scroll).
;; Note: Scroll lags when point must be moved but increasing the number
;;       of lines that point moves in pixel-scroll.el ruins large image
;;       scrolling. So unfortunately I think we'll just have to live with
;;       this.
;; (pixel-scroll-mode)
;; (setq pixel-dead-time 0) ; Never go back to the old scrolling behaviour.
;; (setq pixel-resolution-fine-flag t) ; Scroll by number of pixels instead of lines (t = frame-char-height pixels).
;; (setq mouse-wheel-scroll-amount '(2)) ; Distance in pixel-resolution to scroll each mouse wheel event.
;; (setq mouse-wheel-progressive-speed nil) ; Progressive speed is too fast for me.




;;(add-to-list 'default-frame-alist '(drag-internal-border . 1))
;;(add-to-list 'default-frame-alist '(right-divider-width . 5))
;;(add-to-list 'default-frame-alist '(bottom-divider-width . 5))
;;(add-to-list 'default-frame-alist '(left-fringe . 5))
;;(add-to-list 'default-frame-alist '(right-fringe . 5))
;;(add-to-list 'default-frame-alist '(internal-border-width . 5))



;; (setq speedbar-show-unknown-files t) ; show all files
;; (setq speedbar-use-images nil) ; use text for buttons
;;                                         ;(setq sr-speedbar-right-side nil) ; put on left side


;; ;; active Babel languages
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((gnuplot . t)))
;; ;; add additional languages with '((language . t)))

;; port this to xclip....
;; (defun formatted-copy ()
;;   "Export region to HTML, and copy it to the clipboard."
;;   (interactive)
;;   (save-window-excursion
;;     (let* ((buf (org-export-to-buffer 'html "*Formatted Copy*" nil nil t t))
;;            (html (with-current-buffer buf (buffer-string))))
;;       (with-current-buffer buf
;;         (shell-command-on-region
;;          (point-min)
;;          (point-max)
;;          "textutil -stdin -format html -convert rtf -stdout | pbcopy"))
;;       (kill-buffer buf))))


;;(add-hook 'org-mode-hook (lambda ()
;;                           "Beautify Org Checkbox Symbol"
;;                           (push '("[ ]" . "☐") prettify-symbols-alist)
;;                           (push '("[X]" . "☑") prettify-symbols-alist)
;;                           (push '("[-]" . "❍") prettify-symbols-alist)
;;                           (prettify-symbols-mode)))
;;(add-hook 'vhdl-mode-hook (lambda ()
;;                            "Beautify VHDL Symbols"
;;                            ;;(push '("/=" . "≠") prettify-symbols-alist)
;;                            ;;(push '("=>" . "⇒") prettify-symbols-alist)
;;                            ;;(push '("<=" . "⇐") prettify-symbols-alist)
;;                            (prettify-symbols-mode)))
