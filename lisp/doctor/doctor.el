;;; ../.dotfiles/doom.d/lisp/doctor.el -*- lexical-binding: t; -*-
;;;

(cl-defun check-for-exe (exe &key url cmd ubuntu dnf)
  (if  (executable-find exe)
      (progn (princ (format "- [X] Found %s\n" exe)))
    (progn
      (princ (format "- [ ] Couldn't find %s\n" exe url))
      (when url
        (princ (format "  -  %s\n"  url)))

      (when (not (executable-find "dnf"))
        (setq dnf nil))
      (when (not (executable-find "apt"))
        (setq ubuntu nil))
      (when (not (executable-find "pacman"))
        (setq pacman nil))

      (when (or cmd ubuntu dnf)
        (let ((pad "      "))
          (princ (concat pad "#+begin_src bash  :tangle no :dir /sudo::~/ :results output\n"))
          (when ubuntu (princ (concat pad (concat "sudo apt --yes install " ubuntu "\n"))))
          (when dnf (princ (concat pad (concat "sudo dnf install " dnf "\n"))))
          (when pacman (princ (concat pad (concat "sudo pacman -Syu " dnf "\n"))))
          (when cmd    (princ (concat pad (concat cmd "\n"))))
          (princ (concat pad "#+end_src\n")))))))

(defun make-symlink (a b &rest create)
  (setq a  (concat (expand-file-name "~/Sync/dotfiles/") a))

  (shell-command (format "mkdir -p %s" (file-name-directory b)))
  (shell-command (format "ln -sn %s %s"  a b))

  (let ((check
         (if  (string=
               (shell-command-to-string (concat  "printf %s \"$(readlink " b ")\""))
               a)
             "X" " ")))
    (princ (format "- [%s] ~%s~ → ~%s~\n"  check a b))))

(defun check-for-path (path)
  (if (not (f-directory-p path))
      (progn (princ (format "- [ ] path %s was not found\n" path)))
    (progn (princ (format "- [X] path %s found\n" path)))))

(defun fix-ssh-permissions ()
  "Fix the ssh permissions on host computer"
  (interactive)
  (shell-command "chmod o-w ~/")
  (shell-command "chmod 700 ~/.ssh > /dev/null 2>&1")
  (shell-command "chmod 644 ~/.ssh/id_rsa.pub > /dev/null 2>&1")
  (shell-command "chmod 600 ~/.ssh/id_rsa > /dev/null 2>&1")
  (shell-command "chmod 600 ~/.ssh/authorized_keys > /dev/null 2>&1")
  t)

(defun my-doctor ()
  "Keep a list of useful programs and other things, make sure
they are installed and the computer is set up ok"
  (interactive)

  (let ((buffer "*doctor*"))
    (with-output-to-temp-buffer buffer
      (with-current-buffer buffer

        (read-only-mode -1)
        (org-mode)

        (princ "* Doctor\n")
        (princ "** Checking for required programs\n")
        ;; external programs wanted by my emacs
        (check-for-exe "terminator" :ubuntu "terminator")
        (check-for-exe "bat" :ubuntu "bat")
        (check-for-exe "rg" :dnf "rg")
        ;; $ curl -LO https://github.com/BurntSushi/ripgrep/releases/download/13.0.0/ripgrep_13.0.0_amd64.deb
        ;; $ sudo dpkg -i ripgrep_13.0.0_amd64.deb
        (check-for-exe "ag" :ubuntu "silversearcher-ag")

        ;; proselint
        (check-for-exe "pip3" :ubuntu "python3-pip")

        ;; proselint
        (check-for-exe "npm" :ubuntu "npm" :dnf "npm")

        ;; proselint
        (check-for-exe "proselint" :cmd "sudo pip3 install proselint")

        ;; markdown
        (check-for-exe "markdownlint"
                       :url "https://github.com/igorshubovych/markdownlint-cli"
                       :cmd "sudo npm install -g markdownlint-cli")
        ;; c/c++
        (check-for-exe "bear"
                       :url "https://github.com/rizsotto/Bear"
                       :dnf "bear")
        (check-for-exe "ccls" )

        ;; vhdl
        (check-for-exe "vhdl-tool" :url "https://www.vhdltool.com/")
        (check-for-exe "ghdl")
        (check-for-exe "ghdl-ls")
        (check-for-exe "vhdl_ls")

        ;; python
        (check-for-exe "pyright"
                       :url "https://github.com/microsoft/pyright"
                       :cmd "sudo npm install -g pyright")

        ;; bash
        (check-for-exe "shellcheck" :dnf "ShellCheck" :ubuntu "shellcheck")

        ;; graph-easy
        (check-for-exe "graph-easy" :cmd "sudo" :ubuntu "sudo cpan install Graph::Easy")

        ;; markdown
        (check-for-exe "markdownlint"
                       :url "https://github.com/igorshubovych/markdownlint-cli"
                       :cmd "sudo npm install -g markdownlint-cli")

        ;; utilities
        (check-for-exe "aspell" :dnf "aspell")
        (check-for-exe "rg" :dnf "rg")
        (check-for-exe "fzf" :cmd "git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf && ~/.fzf/install")
        (check-for-exe "gvim" :ubuntu "vim-gtk3")
        (check-for-exe "mpd" :ubuntu "mpd" :dnf "mpd")
        (check-for-exe "mpc" :ubuntu "mpc" :dnf "mpc")
        (check-for-exe "ncmpcpp" :ubuntu "ncmpcpp" :dnf "ncmpcpp")
        (check-for-exe "xfce4-screenshooter" :ubuntu "xfce4-screenshooter")

        ;; make sure ssh permissions are right
        ;;
        ;; TODO: should just check here and prompt if its wrong?
        (princ "** Fixing ssh permissions\n")
        (when (fix-ssh-permissions)
          (princ (format "- [X] ssh permissions corrected\n")))

        ;;
        (princ "** Checking for required paths\n")
        (check-for-path "~/Sync/org")
        (check-for-path "~/Sync/notes")

        (princ "** Setting git settings\n")
        (shell-command "git config --global user.name \"Andrew Peck\"")
        (shell-command "git config --global user.email \"andrew.peck@cern.ch\"")

        ;; symlinks
        (princ "** Creating symlinks\n")

        (make-symlink "org-protocol.desktop" "~/.local/share/applications/org-protocol.desktop")
        (make-symlink "aspell.en.pws" "~/.aspell.en.pws")
        (make-symlink "mimeapps.list" "~/.config/mimeapps.list")
        (make-symlink "Xresources" "~/.Xresources")
        (make-symlink "profile" "~/.profile")
        (make-symlink "bash_profile" "~/.bash_profile")
        (make-symlink "mbsyncrc" "~/.mbsyncrc")
        (make-symlink "bashrc" "~/.bashrc")
        (make-symlink "bash_logout" "~/.bash_logout")

        (make-symlink "xinitrc" "~/.xinitrc")
        (make-symlink "xmobarrc" "~/.xmobarrc")
        (make-symlink "vim/vimrc" "~/.vimrc")
        (make-symlink "ssh/config" "~/.ssh/config")
        (make-symlink "ncmpcpp/config" "~/.ncmpcpp/config")
        (make-symlink "mpd/mpd.conf" "~/.mpd/mpd.conf")
        (make-symlink "xbindkeysrc" "~/.xbindkeysrc")
        (make-symlink "local/share/applications/emacsclient.desktop" "~/.local/share/applications/emacsclient.desktop")
        (make-symlink "config/autostart/xbindkeys.desktop" "~/.config/autostart/xbindkeys.desktop")

        (make-symlink "nvim" "~/.config/nvim")
        (make-symlink "doom.d" "~/.doom.d")
        (make-symlink "xmonad" "~/.xmonad")
        (make-symlink "vim/vim" "~/.vim")
        (make-symlink "Fonts" "~/.fonts")
        (make-symlink "bin" "~/bin")

        (princ "** Setting custom mimetypes\n")
        (shell-command "cp mime/* ~/.local/share/mime/packages/ && update-mime-database ~/.local/share/mime")

        (princ "** Setting up org desktop protocol\n")
        (shell-command "xdg-mime default org-protocol.desktop x-scheme-handler/org-protocol")

        ;; (princ "** Setting up CERN certificate\n")
        ;; FIXME: sudo doesn't work here .. put an org block? or use `compile`
        ;; (shell-command "sudo mkdir -p /usr/local/share/ca-certificates/ && sudo cp ~/.dotfiles/CERN\ Root\ Certification\ Authority\ 2.crt /usr/local/share/ca-certificates/CERN\ Root\ Certification\ Authority\ 2.crt && sudo update-ca-trust")

        (princ "** Updating font cache\n")
        (start-process "*fc-cache*" nil "fc-cache" "-f" "-v")

        ;; - command: if [ $(hostname) = pepper ]; then ln -sf ~/.dotfiles/equalizerrc_mb42x ~/.config/pulseaudio/equalizerrc
        ;;   description: Setting up MB42X Equalizer for pepper
        ;; - command: if [ $(hostname) = larry  ]; then sudo ln -sf ~/.rsnapshot.conf /etc/rsnapshot.conf; fi;
        ;;   description: Linking rsnapshot configuration to /etc
        ;; - command: if [ $(hostname) = pepper  ]; then sudo ln -sf ~/.rsnapshot.conf /etc/rsnapshot.conf; fi;
        ;;   description: Linking rsnapshot configuration to /etc
        ;; - command: if [ $(hostname) = pepper ]; then sudo ln -sf ~/.dotfiles/UPower.conf /etc/UPower/UPower.conf; sudo chmod a+rw /etc/UPower/UPower.conf; fi;
        ;;   description: Setting up UPower config for pepper
        ;; - command: if [ $(hostname) = pepper ]; then sudo ln -sf ~/.dotfiles/logind.conf /etc/systemd/logind.conf; fi;
        ;;   description: Setting up Logind config for pepper

        ))))

(provide 'doctor)
