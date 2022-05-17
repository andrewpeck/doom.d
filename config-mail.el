;; -*- lexical-binding: t; -*-

;; mu4e
;;------------------------------------------------------------------------------

(when (string= (system-name) "larry")
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e"))

(defun mu4e-reindex () (interactive)
       (async-shell-command "mu index -m ~/.mail"))

(after! mu4e
  ;; https://www.djcbsoftware.nl/code/mu/mu4e/Bookmarks.html
  (setq mu4e-alert-style nil
        mu4e-alert-email-notification-types nil)

  (setq mu4e-bookmarks
        '(("maildir:/inbox AND flag:unread AND NOT flag:trashed" "Unread messages" ?u)
          ("date:today..now" "Today's messages" ?t)
          ("maildir:/inbox AND date:today..now" "Today's unread messages" ?r)
          ("date:7d..now" "Last 7 days" ?w)
          ("mime:image/*" "Messages with images" ?p)
          ("flag:flagged" "Flagged messages" ?f)))

  (setq mail-user-agent 'mu4e-user-agent)
  (setq mu4e-drafts-folder "/[Gmail]/Drafts")
  (setq mu4e-sent-folder   "/[Gmail]/Sent Mail")
  ;; https://www.djcbsoftware.nl/code/mu/mu4e/Refiling-messages.html
  (setq mu4e-refile-folder "/[Gmail].All Mail")
  (setq mu4e-trash-folder  "/[Gmail]/Trash")

  (setq smtpmail-smtp-server "smtp.gmail.com")
  (setq smtpmail-smtp-service 587 )

  (setq user-mail-address "peckandrew@gmail.com")
  (setq user-full-name "Andrew Peck")

  (setq mu4e-sent-messages-behavior 'delete)
  (setq mu4e-user-mail-address-list '("peckandrew@gmail.com" "andrew.peck@cern.ch" "peck@bu.edu"))
  (setq mu4e-maildir-shortcuts
        '( ("/INBOX"               . ?i)
           ("/[Gmail]/Sent Mail"   . ?s)
           ("/[Gmail]/Trash"       . ?t)
           ("/[Gmail]/All Mail"    . ?a)))
  ;; (setq mu4e-get-mail-command "offlineimap")
  (setq mu4e-use-fancy-chars t)
  (setq mu4e-view-show-addresses t)
  (setq +mu4e-personal-addresses '("peckandrew@gmail.com"))
  (setq mu4e-view-show-images t)
  (add-hook 'mu4e-view-mode-hook #'visual-line-mode)
  (add-hook 'mu4e-compose-mode-hook 'flyspell-mode))
