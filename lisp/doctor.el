;;; ../.dotfiles/doom.d/lisp/doctor.el -*- lexical-binding: t; -*-

(defun check-for-exe (exe &optional url cmd)
  (if (executable-find exe)
      (progn (princ (format "- [X] Found %s\n" exe)))
    (progn
      (princ (format "- [ ] Couldn't find %s\n" exe url))
      (when url
        (princ (format "  -  %s\n"  url)))
      (when cmd
        (when (y-or-n-p (format "%s not found. Try to install??" exe))
          (if (compile cmd t)
              (princ (format "  - %s installed\n"  exe))
            (princ (format "  - Failed to install %s\n"  exe))))))))


(defun check-for-path (path)
  (if (not (f-directory-p path))
      (progn
        (princ (format "- [ ] path %s was not found\n" path)))
    (progn
      (princ (format "- [X] path %s found\n" path)))))

(defun my-doctor ()
  "Keep a list of useful programs and other things, make sure they are installed and the computer is set up ok"
  (interactive)

  ;; (letf system-install-commandp (program)
  ;;   (eq 0 (shell-command (format "command -v %s 2> /dev/null" program))))

  (let ((buffer "*doctor*"))
    (with-output-to-temp-buffer buffer
      (with-current-buffer buffer

        (read-only-mode -1)
        (org-mode)

        (princ "* Doctor\n")
        (princ "** Checking for required programs\n")
        ;; external programs wanted by emacs
        ;; markdown
        (check-for-exe "markdownlint" "https://github.com/igorshubovych/markdownlint-cli" "sudo npm install -g markdownlint-cli")
        ;; c/c++
        (check-for-exe "bear" "https://github.com/rizsotto/Bear" "sudo dnf install bear")
        (check-for-exe "ccls" )
        ;; vhdl
        (check-for-exe "vhdl-tool" "https://www.vhdltool.com/")
        (check-for-exe "ghdl" )
        (check-for-exe "ghdl-ls" )
        ;; python
        (check-for-exe "pyright" "https://github.com/microsoft/pyright")
        ;; bash
        (check-for-exe "shellcheck" "sudo dnf install ShellCheck")
        ;; utilities
        (check-for-exe "aspell" "sudo dnf install aspell")
        (check-for-exe "rg" "sudo dnf install rg")
        (check-for-exe "fzf" "git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf && ~/.fzf/install")
        (check-for-exe "gvim")
        (check-for-exe "xfce4-screenshooter")
        ;;(check-for-exe "some-exe" nil "sudo dnf install some-exe")

        ;; make sure ssh permissions are right
        ;; TODO: should just check here and prompt if its wrong?
        (princ "** Fixing ssh permissions\n")
        (when (fix-ssh-permissions)
          (princ (format "- [X] ssh permissions corrected\n")))

        ;;
        (princ "** Checking for required paths\n")
        (check-for-path "~/Dropbox/org")
        (check-for-path "~/Dropbox/notes")
        ))))
