;; -*- lexical-binding: t; -*-

(setq +notmuch-sync-backend 'offlineimap)

;; (use-package smtpmail-async
;;   :commands (async-smtpmail-send-it))

(use-package mu4e

  :load-path "/usr/share/emacs/site-lisp/mu4e"

  :config
  
  ;; mu4e
  ;;------------------------------------------------------------------------------

  ;; https://github.com/doomemacs/doomemacs/issues/6906
  ;; (defconst mu4e-headers-buffer-name "*mu4e-headers*"
  ;;   "Name of the buffer for message headers.")

  ;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")

  (require 'smtpmail-async)

  (defun ap/mu4e-index-mail ()
    (interactive)
    (async-shell-command "mu index"))

  (setq mu4e-drafts-folder "/fastmail/Drafts"
        mu4e-sent-folder   "/fastmail/Sent"
        mu4e-refile-folder "/fastmail/Archive"
        mu4e-trash-folder  "/fastmail/Trash")

  (setq mu4e-search-include-related nil)

  (setq mu4e-bookmarks
        '(("maildir:/inbox AND flag:unread AND NOT flag:trashed" "Unread messages" ?u)
          ("date:today..now AND NOT flag:trashed" "Today's messages" ?t)
          ("maildir:/inbox AND date:today..now AND NOT flag:trashed" "Today's unread messages" ?r)
          ("date:7d..now AND NOT flag:trashed" "Last 7 days" ?w)
          ("mime:image/* AND NOT flag:trashed" "Messages with images" ?p)
          ("flag:flagged AND NOT flag:trashed" "Flagged messages" ?f)))

  (setq mail-user-agent 'mu4e-user-agent)

  (setq send-mail-function #'smtpmail-send-it)
  (setq message-send-mail-function #'smtpmail-send-it)

  (setq user-full-name "Andrew Peck")

  (setq mu4e-sent-messages-behavior 'delete)
  (setq mu4e-use-fancy-chars t)
  (setq +mu4e-personal-addresses '("peckandrew@gmail.com"
                                   "andrew.peck@cern.ch"
                                   "apeck@fastmail.com"
                                   "peck@bu.edu"))

  ;; C-c C-c for 
  (map! :map mu4e-thread-mode-map :n "C-c C-c" #'mu4e-mark-execute-all)
  (unbind-key (kbd "x") mu4e-thread-mode-map)

  (add-hook 'mu4e-view-mode-hook #'visual-line-mode)
  (add-hook 'mu4e-compose-mode-hook 'flyspell-mode))

;; (setq user-mail-address "peckandrew@gmail.com")
(setq mu4e-maildir-shortcuts
      '(("/fastmail/inbox"              . ?i)
        ("/fastmail/Sent"               . ?s)
        ("/fastmail/Archive"            . ?a)
        ("/fastmail/Trash"              . ?t)))

;; https://www.djcbsoftware.nl/code/mu/mu4e/Bookmarks.html
;; (setq mu4e-alert-style nil
;;       mu4e-alert-email-notification-types nil)

;; (setq mu4e-drafts-folder "/[Gmail]/Drafts")
;; (setq mu4e-sent-folder   "/[Gmail]/Sent Mail")
;; ;; https://www.djcbsoftware.nl/code/mu/mu4e/Refiling-messages.html
;; (setq mu4e-refile-folder "/[Gmail].All Mail")
;; (setq mu4e-trash-folder  "/[Gmail]/Trash")

;; (setq mu4e-get-mail-command "offlineimap")
;; (setq mu4e-view-show-addresses t)
;; (setq mu4e-view-show-images t)
