;;; ../.dotfiles/doom.d/lisp/doctor.el -*- lexical-binding: t; -*-

(defun check-for-exe (exe url)
  (when (not (executable-find exe))
    (error (format "Oh no, I couldn't find %s %s" exe url))))

(defun check-for-path (path)
  (when (not (f-directory-p path))
    (error "path %s was not found" path)))

(defun my-doctor ()
  "Keep a list of useful programs and other things, make sure they are installed and the computer is set up ok"
  (interactive)

  ;; external programs wanted by emacs
  ;; markdown
  (check-for-exe "markdownlint" "https://github.com/igorshubovych/markdownlint-cli")
  ;; c/c++
  (check-for-exe "bear" "https://github.com/rizsotto/Bear")
  (check-for-exe "ccls" "")
  ;; vhdl
  (check-for-exe "vhdl-tool" "")
  (check-for-exe "ghdl" "")
  ;; python
  (check-for-exe "pyright" "https://github.com/microsoft/pyright")
  ;; bash
  (check-for-exe "shellcheck" "")
  ;; utilities
  (check-for-exe "aspell" "")
  (check-for-exe "rg" "")
  (check-for-exe "fzf" "")
  (check-for-exe "gvim" "")
  (check-for-exe "xfce4-screenshooter" "")

  ;; make sure ssh permissions are right
  (fix-ssh-permissions)

  ;;
  (check-for-path "~/Dropbox/org")
  (check-for-path "~/Dropbox/notes")

  ;;
  )
