;;; ../.dotfiles/doom.d/lisp/doctor.el -*- lexical-binding: t; -*-

(cl-defun check-for-exe (exe &key url cmd)
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
        ;; external programs wanted by my emacs
        (check-for-exe "terminator")
        (check-for-exe "bat")
        (check-for-exe "rg")
        (check-for-exe "ag" :cmd "sudo apt install silversearcher-ag")

        ;; proselint
        (check-for-exe "proselint")

        ;; markdown
        (check-for-exe "markdownlint"
                       :url "https://github.com/igorshubovych/markdownlint-cli"
                       :cmd "sudo npm install -g markdownlint-cli")
        ;; c/c++
        (check-for-exe "bear"
                       :url "https://github.com/rizsotto/Bear"
                       :cmd "sudo dnf install bear")
        (check-for-exe "ccls" )

        ;; vhdl
        (check-for-exe "vhdl-tool" :url "https://www.vhdltool.com/")
        (check-for-exe "ghdl")
        (check-for-exe "ghdl-ls")
        (check-for-exe "vhdl_ls")

        ;; python
        (check-for-exe "pyright"
                       :url "https://github.com/microsoft/pyright"
                       :cmd "npm install -g pyright")

        ;; bash
        (check-for-exe "shellcheck" :cmd "sudo dnf install ShellCheck")

        ;; graph-easy
        (check-for-exe "graph-easy" :cmd "cpan install Graph::Easy")
        (check-for-exe "dot")
        (check-for-exe "ditaa" :cmd "sudo apt install ditaa")

        ;; utilities
        (check-for-exe "aspell" :cmd "sudo dnf install aspell")
        (check-for-exe "rg" :cmd "sudo dnf install rg")
        (check-for-exe "fzf" :cmd "git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf && ~/.fzf/install")
        (check-for-exe "gvim")
        (check-for-exe "xfce4-screenshooter")

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
