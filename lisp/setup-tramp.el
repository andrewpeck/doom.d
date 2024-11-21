;;------------------------------------------------------------------------------
;; Tramp
;;------------------------------------------------------------------------------

(use-package! tramp

  :load-path "~/.emacs.d/.local/straight/repos/tramp"
  :config


  (setq tramp-ssh-controlmaster-options "-o ControlMaster=auto"
        tramp-use-ssh-controlmaster-options t
        tramp-histfile-override "~/.tramp_history"
        tramp-inline-compress-start-size 1000
        tramp-copy-size-limit 10000
        vc-handled-backends '(Git)
        tramp-verbose 1
        tramp-default-method "ssh")

  ;; https://www.gnu.org/software/tramp/#Remote-shell-setup
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "/sshx:user@host:")
                     "remote-shell" "/usr/bin/sh"))

  ;; Another way to find the remote path is to use the path assigned to the remote user by the
  ;; remote host. TRAMP does not normally retain this remote path after login. However,
  ;; tramp-own-remote-path preserves the path value, which can be used to update tramp-remote-path.
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))
