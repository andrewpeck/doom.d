;;; ../.dotfiles/doom.d/lisp/doctor.el -*- lexical-binding: t; -*-
;;;

(cl-defun check-for-exe (exe &key url noroot cmd ubuntu dnf)
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
          (if noroot
              (princ (concat pad "#+begin_src bash  :tangle no :results output\n"))
              (princ (concat pad "#+begin_src bash  :tangle no :dir /sudo::~/ :results output\n")))
          (when ubuntu (princ (concat pad (concat "sudo apt --yes install " ubuntu "\n"))))
          (when dnf (princ (concat pad (concat "sudo dnf install " dnf "\n"))))
          (when pacman (princ (concat pad (concat "sudo pacman -Syu " dnf "\n"))))
          (when cmd    (princ (concat pad (concat cmd "\n"))))
          (princ (concat pad "#+end_src\n")))))))

(defun dotfiles (file)
  (concat (expand-file-name "~/Sync/dotfiles/") file))

(defun make-symlink (a b &rest create)

  (setq a (expand-file-name a))
  (setq b (expand-file-name b))

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
        (check-for-exe "fd" :ubuntu "fd-find")
        ;; $ curl -LO https://github.com/BurntSushi/ripgrep/releases/download/13.0.0/ripgrep_13.0.0_amd64.deb
        ;; $ sudo dpkg -i ripgrep_13.0.0_amd64.deb
        (check-for-exe "ag" :ubuntu "silversearcher-ag")

        ;; python
        (check-for-exe "pip3" :ubuntu "python3-pip")
        (check-for-exe "pyflakes" :cmd "sudo pip install pyflakes")
        (check-for-exe "isort" :cmd "sudo pip install isort")
        (check-for-exe "pytest" :cmd "sudo pip install pytest")

        ;; sbcl
        (check-for-exe "sbcl" :ubuntu "sbcl" :dnf "sbcl")

        ;; gnuplot
        (check-for-exe "gnuplot" :ubuntu "gnuplot" :dnf "gnuplot")

        ;; npm
        (check-for-exe "npm" :ubuntu "npm" :dnf "npm")

        ;; proselint
        (check-for-exe "proselint" :cmd "sudo pip3 install proselint")

        ;; yamllint
        (check-for-exe "yamllint" :dnf "yamllint" :ubuntu "yamllint")

        ;;  cask
        (check-for-exe "cask" :cmd "cd ~/ && git clone https://github.com/cask/cask && make -C cask install")

        ;; markdown
        (check-for-exe "markdownlint"
                       :url "https://github.com/igorshubovych/markdownlint-cli"
                       :cmd "sudo npm install -g markdownlint-cli")
        (check-for-exe "grip" :cmd "sudo pip3 install grip")

        ;; c/c++
        (check-for-exe "bear"
                       :url "https://github.com/rizsotto/Bear"
                       :ubuntu "bear"
                       :dnf "bear")
        (check-for-exe "ccls" )
        (check-for-exe "libtool" :ubuntu "libtool-bin")

        ;; vhdl
        (check-for-exe "vhdl-tool" :url "https://www.vhdltool.com/")
        (check-for-exe "ghdl")
        (check-for-exe "ghdl-ls")
        (check-for-exe "vhdl_ls")

        ;; node
        (check-for-exe "node"
                       :noroot t
                       :cmd "curl -fsSL https://deb.nodesource.com/setup_current.x | sudo -E bash - && sudo apt-get install -y nodejs")

        ;; python
        (check-for-exe "pyright"
                       :url "https://github.com/microsoft/pyright"
                       :cmd "sudo npm install -g pyright")
        (check-for-exe "black"
                       :cmd "pip install black")

        ;; bash
        (check-for-exe "shellcheck" :dnf "ShellCheck" :ubuntu "shellcheck")

        ;; graph-easy
        (check-for-exe "graph-easy" :cmd "sudo" :ubuntu "sudo cpan install Graph::Easy")

        ;; utilities
        ;;
        (check-for-exe "kitty" :cmd "cd ~/ && curl -L https://sw.kovidgoyal.net/kitty/installer.sh | sh /dev/stdin")
        (check-for-exe "act" :cmd "cd ~/ && curl https://raw.githubusercontent.com/nektos/act/master/install.sh | sudo bash")
        (check-for-exe "aspell" :dnf "aspell" :ubuntu "aspell")
        (check-for-exe "pandoc" :dnf "pandoc" :ubuntu "pandoc")
        (check-for-exe "cmake" :ubuntu "cmake" :dnf "cmake")
        (check-for-exe "cloc" :ubuntu "cloc" :dnf "cloc")
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

        (make-symlink (dotfiles "org-protocol.desktop") "~/.local/share/applications/org-protocol.desktop")
        (make-symlink (dotfiles "aspell.en.pws") "~/.aspell.en.pws")
        (make-symlink (dotfiles "mimeapps.list") "~/.config/mimeapps.list")
        (make-symlink (dotfiles "Xresources") "~/.Xresources")
        (make-symlink (dotfiles "profile") "~/.profile")
        (make-symlink (dotfiles "bash_profile") "~/.bash_profile")
        (make-symlink (dotfiles "mbsyncrc") "~/.mbsyncrc")
        (make-symlink (dotfiles "bashrc") "~/.bashrc")
        (make-symlink (dotfiles "bash_logout") "~/.bash_logout")

        (make-symlink "~/Sync/emacs-backups" "~/emacs-backups")

        (make-symlink "~/.local/kitty.app/bin/kitty" "~/bin/kitty")
        (make-symlink (dotfiles "kitty") "~/.config/kitty")
        (make-symlink (dotfiles "xinitrc") "~/.xinitrc")
        (make-symlink (dotfiles "xmobarrc") "~/.xmobarrc")
        (make-symlink (dotfiles "vim/vimrc") "~/.vimrc")
        (make-symlink (dotfiles "ssh/config") "~/.ssh/config")
        (make-symlink (dotfiles "ncmpcpp/config") "~/.ncmpcpp/config")
        (make-symlink (dotfiles "mpd/mpd.conf") "~/.mpd/mpd.conf")
        (make-symlink (dotfiles "xbindkeysrc") "~/.xbindkeysrc")
        (make-symlink (dotfiles "local/share/applications/emacsclient.desktop") "~/.local/share/applications/emacsclient.desktop")
        (make-symlink (dotfiles "config/autostart/xbindkeys.desktop") "~/.config/autostart/xbindkeys.desktop")

        (when (string-match ".*ubuntu.*" (shell-command-to-string "uname -a"))
          (when (executable-find "fdfind")
            (make-symlink (executable-find "fdfind") "~/.local/bin/fd")))

        (make-symlink (dotfiles "nvim") "~/.config/nvim")
        (make-symlink (dotfiles "doom.d") "~/.doom.d")
        (make-symlink (dotfiles "xmonad") "~/.xmonad")
        ;; (make-symlink (dotfiles "vim/vim") "~/.vim")
        (make-symlink (dotfiles "Fonts") "~/.fonts")
        (make-symlink (dotfiles "bin") "~/bin")

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
        ;;
        ;;


        ;; curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
        ;;     https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

        ))))

(provide 'doctor)
