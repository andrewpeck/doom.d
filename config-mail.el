;; -*- lexical-binding: t; -*-

(when (string= (system-name) "larry")

  ;; mu4e
  ;;------------------------------------------------------------------------------

  ;; https://github.com/doomemacs/doomemacs/issues/6906
  ;; (defconst mu4e-headers-buffer-name "*mu4e-headers*"
  ;;   "Name of the buffer for message headers.")

  (when (string= (system-name) "larry")
    (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e"))

  (defun ap/mu4e-index-mail ()
    (interactive)
    (async-shell-command "mu index"))

  (after! mu4e

    ;; https://www.djcbsoftware.nl/code/mu/mu4e/Bookmarks.html
    (setq mu4e-alert-style nil
          mu4e-alert-email-notification-types nil)

    (setq mu4e-search-include-related nil)

    (setq mu4e-bookmarks
          '(("maildir:/inbox AND flag:unread AND NOT flag:trashed" "Unread messages" ?u)
            ("date:today..now AND NOT flag:trashed" "Today's messages" ?t)
            ("maildir:/inbox AND date:today..now AND NOT flag:trashed" "Today's unread messages" ?r)
            ("date:7d..now AND NOT flag:trashed" "Last 7 days" ?w)
            ("mime:image/* AND NOT flag:trashed" "Messages with images" ?p)
            ("flag:flagged AND NOT flag:trashed" "Flagged messages" ?f)))

    (setq mail-user-agent 'mu4e-user-agent)
    (setq mu4e-drafts-folder "/[Gmail]/Drafts")
    (setq mu4e-sent-folder   "/[Gmail]/Sent Mail")
    ;; https://www.djcbsoftware.nl/code/mu/mu4e/Refiling-messages.html
    (setq mu4e-refile-folder "/[Gmail].All Mail")
    (setq mu4e-trash-folder  "/[Gmail]/Trash")

    (setq message-send-mail-function 'smtpmail-send-it
          starttls-use-gnutls t
          smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
          smtpmail-auth-credentials
          '(("smtp.gmail.com" 587 "peckandrew@gmail.com" nil))
          smtpmail-default-smtp-server "smtp.gmail.com"
          smtpmail-smtp-server "smtp.gmail.com"
          smtpmail-smtp-service 587)

    (setq user-mail-address "peckandrew@gmail.com")
    (setq user-full-name "Andrew Peck")

    (setq mu4e-sent-messages-behavior 'delete)
    (setq mu4e-maildir-shortcuts
          '( ("/INBOX"               . ?i)
             ("/[Gmail]/Sent Mail"   . ?s)
             ("/[Gmail]/Trash"       . ?t)
             ("/[Gmail]/All Mail"    . ?a)))

    ;; (setq mu4e-get-mail-command "offlineimap")
    ;; (setq mu4e-view-show-addresses t)
    ;; (setq mu4e-view-show-images t)

    (setq mu4e-use-fancy-chars t)
    (setq +mu4e-personal-addresses '("peckandrew@gmail.com"
                                     "andrew.peck@cern.ch"
                                     "peck@bu.edu"))
    (add-hook 'mu4e-view-mode-hook #'visual-line-mode)
    (add-hook 'mu4e-compose-mode-hook 'flyspell-mode)))
