;;; -*- lexical-binding: t; -*-
;;; package --- Summary

;;; package --- Commentary

;;; package --- Code

(require 'cl-lib)

(cl-defun setup--check-for-exe (exe &key url noroot cmd ubuntu dnf pacman)
  ""
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

      (when (or cmd ubuntu dnf pacman)
        (let ((pad "      "))

          (if noroot
              (princ (concat pad "#+begin_src bash :async :tangle no :results output\n"))
            (princ (concat pad "#+begin_src bash :async :tangle no :dir /sudo::~/ :results output\n")))

          (when ubuntu (princ (format "%ssudo apt install --yes %s\n" pad ubuntu)))
          (when dnf    (princ (format "%ssudo dnf install -y %s\n" pad dnf)))
          (when pacman (princ (format "%ssudo pacman -Syu %s\n" pad dnf)))
          (when cmd    (princ (format "%s%s\n" pad cmd)))

          (princ (concat pad "#+end_src\n")))))))

(defun setup--shell-command-nil (command)
  "Wraps `shell-command` and return nil instead of 0 from the output of COMMAND."
  (let ((ret (shell-command command)))
    (if (= 0 ret) t nil)))

;;;###autoload
(defun setup--fix-ssh-permissions ()
  "Fix the ssh permissions on host computer."
  (interactive)
  (and (setup--shell-command-nil "chmod o-w ~/")
       (setup--shell-command-nil "chmod 700 ~/.ssh > /dev/null 2>&1")
       (setup--shell-command-nil "chmod 644 ~/.ssh/id_rsa.pub > /dev/null 2>&1")
       (setup--shell-command-nil "chmod 600 ~/.ssh/id_rsa > /dev/null 2>&1")
       (setup--shell-command-nil "chmod 600 ~/.ssh/authorized_keys > /dev/null 2>&1")))

(defun setup--check-for-path (path)
  ""
  (if (not (file-directory-p path))
      (progn (princ (format "- [ ] path %s was not found\n" path)))
    (progn (princ (format "- [X] path %s found\n" path)))))

(defvar setup--dotfile-path
  "~/Sync/dotfiles/"
  "Path to dotfile dir.")

(defun setup--dotfiles (file)
  (concat (expand-file-name setup--dotfile-path) file))

(defun setup--make-symlink (a b)

  (setq a (expand-file-name a))
  (setq b (expand-file-name b))

  ;; make sure that the target file exists
  (if (not (file-exists-p a))
      (princ (format "- [ ] %s not found\n"  a ))
    (progn
      (shell-command (format "mkdir -p %s" (file-name-directory b)))

      (when (not (file-exists-p b))
        (shell-command (format "ln -sn %s %s"  a b)))

      (let* ((command (concat  "printf %s \"$(readlink " b ")\""))
             (check (if  (string= (shell-command-to-string command) a)
                        "X" " ")))
        (princ (format "- [%s] ~%s~ → ~%s~\n"  check a b))))))

(defun setup--make-dotfile-symlink (a b)
  ""
  (setup--make-symlink (setup--dotfiles a) b))

(defun setup--create-org-buffer (payload)
  ""

  (let ((buffer "Setup System"))
    (with-output-to-temp-buffer buffer
      (with-current-buffer buffer
        (read-only-mode -1)
        (org-mode)
        (funcall payload)))))

(defun setup--check-functions ()
  ""

  (princ "* Setup System\n")
  (princ "** Checking for required programs\n")

  ;; external programs wanted by my emacs
  (setup--check-for-exe "terminator" :ubuntu "terminator")
  (setup--check-for-exe "bat" :ubuntu "bat")
  (setup--check-for-exe "fd" :dnf "fd-find" :ubuntu "fd-find")
  ;; $ curl -LO https://github.com/BurntSushi/ripgrep/releases/download/13.0.0/ripgrep_13.0.0_amd64.deb
  ;; $ sudo dpkg -i ripgrep_13.0.0_amd64.deb
  (setup--check-for-exe "ag" :ubuntu "silversearcher-ag" :dnf "the_silver_searcher")

  ;; https://github.com/blahgeek/emacs-lsp-booster
  (setup--check-for-exe "emacs-lsp-booster" :cmd "cargo install --git https://github.com/blahgeek/emacs-lsp-booster" :noroot t)

  ;; python
  (setup--check-for-exe "autopep8" :cmd "pipx install autopep8" :noroot t)
  (setup--check-for-exe "isort" :cmd "pipx install isort" :noroot t)
  (setup--check-for-exe "pip3" :ubuntu "python3-pip" :dnf "python3-pip")
  (setup--check-for-exe "pyflakes" :cmd "pipx install pyflakes" :noroot t)
  (setup--check-for-exe "pyment" :cmd "pipx install pyment" :noroot t)
  (setup--check-for-exe "pyright" :url "https://github.com/microsoft/pyright" :cmd "pipx install pyright" :noroot t)
  (setup--check-for-exe "pytest" :cmd "pipx install pytest" :noroot t)
  (setup--check-for-exe "wordcloud_cli" :cmd "pipx install wordcloud" :noroot t)

  ;; clojure
  (setup--check-for-exe "clj-kondo" :noroot t :cmd "cd /tmp && curl -sLO https://raw.githubusercontent.com/clj-kondo/clj-kondo/master/script/install-clj-kondo && chmod +x install-clj-kondo && ./install-clj-kondo --dir ~/.local/bin")
  (setup--check-for-exe "clojure-lsp" :cmd "sudo bash < <(curl -s https://raw.githubusercontent.com/clojure-lsp/clojure-lsp/master/install)")

  ;; sbcl
  (setup--check-for-exe "sbcl" :ubuntu "sbcl" :dnf "sbcl")

  ;; gnuplot
  (setup--check-for-exe "gnuplot" :ubuntu "gnuplot" :dnf "gnuplot")

  ;; npm
  (setup--check-for-exe "npm" :ubuntu "npm" :dnf "npm")

  ;; proselint
  (setup--check-for-exe "proselint" :cmd "pipx install proselint" :noroot t)

  ;; yamllint
  (setup--check-for-exe "yamllint" :dnf "yamllint" :ubuntu "yamllint")

  ;;  cask
  ;; (setup--check-for-exe "cask" :cmd "cd ~/ && git clone https://github.com/cask/cask && make -C cask install")

  ;; markdown
  (setup--check-for-exe "markdownlint"
                        :url "https://github.com/igorshubovych/markdownlint-cli"
                        :cmd "sudo npm install -g markdownlint-cli")
  (setup--check-for-exe "grip" :cmd "pipx install grip" :noroot t)

  ;; c/c++
  (setup--check-for-exe "bear"
                        :url "https://github.com/rizsotto/Bear"
                        :ubuntu "bear"
                        :dnf "bear")
  (setup--check-for-exe "ccls" )
  (setup--check-for-exe "libtool" :ubuntu "libtool-bin")

  ;; vhdl
  (setup--check-for-exe "vhdl-tool" :url "https://www.vhdltool.com/")
  (setup--check-for-exe "ghdl")
  (setup--check-for-exe "ghdl-ls")
  (setup--check-for-exe "vhdl_ls")

  ;; Lesspipe
  (setup--check-for-exe "xpdf")
  (setup--check-for-exe "lesspipe.sh")

  ;; latex lsp
  (setup--check-for-exe "digestif" :noroot t :cmd "wget https://raw.githubusercontent.com/astoff/digestif/master/scripts/digestif -O ~/.local/bin/digestif && chmod +x ~/.local/bin/digestif")

  ;; Mail
  (setup--check-for-exe "mu4e" :dnf "maildir-utils")
  (setup--check-for-exe "mbsync" :dnf "isync")

  ;; node
  (setup--check-for-exe "node" :noroot t
                        :ubuntu "curl -fsSL https://deb.nodesource.com/setup_current.x | sudo -E bash - && sudo apt-get install -y nodejs")
  (setup--check-for-exe "vl2svg" :cmd "sudo npm install vega-lite vega-cli canvas")

  ;; bash
  (setup--check-for-exe "shellcheck" :dnf "ShellCheck" :ubuntu "shellcheck")

  ;; tree-sitter
  (setup--check-for-exe "tree-sitter" :dnf "tree-sitter-cli libtree-sitter-devel")

  ;; graph-easy
  (setup--check-for-exe "graph-easy" :cmd "sudo" :ubuntu "sudo cpan install Graph::Easy")

  ;; rust
  (setup--check-for-exe "rustc" :noroot t :cmd "curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh")
  (setup--check-for-exe "rust-analyzer" :noroot t :cmd "rustup component add rust-src && mkdir -p ~/.local/bin && curl -L https://github.com/rust-lang/rust-analyzer/releases/latest/download/rust-analyzer-x86_64-unknown-linux-gnu.gz | gunzip -c - > ~/.local/bin/rust-analyzer && chmod +x ~/.local/bin/rust-analyzer")

  ;; utilities
  (setup--check-for-exe "pushover-cli" :noroot t :cmd "curl -o ~/.local/bin/pushover-cli https://raw.githubusercontent.com/markus-perl/pushover-cli/master/pushover-cli && chmod +x ~/.local/bin/pushover-cli")
  (setup--check-for-exe "act" :cmd "cd ~/ && curl https://raw.githubusercontent.com/nektos/act/master/install.sh | sudo bash")
  (setup--check-for-exe "htop" :dnf "htop" :ubuntu "htop")
  (setup--check-for-exe "aspell" :dnf "aspell" :ubuntu "aspell")
  (setup--check-for-exe "unoconv" :dnf "unoconv" :ubuntu "unoconv") ; for emacs doc conversions
  (setup--check-for-exe "pandoc" :dnf "pandoc" :ubuntu "pandoc")
  (setup--check-for-exe "cmake" :ubuntu "cmake" :dnf "cmake")
  (setup--check-for-exe "cloc" :ubuntu "cloc" :dnf "cloc")
  (setup--check-for-exe "rg" :dnf "ripgrep" :ubuntu "rg")
  (setup--check-for-exe "fzf" :noroot t :cmd "git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf && ~/.fzf/install")
  (setup--check-for-exe "gvim" :ubuntu "vim-gtk3" :dnf "vim-X11")
  (setup--check-for-exe "mpd" :ubuntu "mpd" :dnf "mpd")
  (setup--check-for-exe "mpc" :ubuntu "mpc" :dnf "mpc")
  (setup--check-for-exe "ncmpcpp" :ubuntu "ncmpcpp" :dnf "ncmpcpp")
  (setup--check-for-exe "xfce4-screenshooter" :ubuntu "xfce4-screenshooter")
  (setup--check-for-exe "uplot" :noroot t :cmd "gem install youplot")

  ;; make sure ssh permissions are right
  ;;
  ;; TODO: should just check here and prompt if its wrong?
  (princ "** Fixing ssh permissions\n")
  (when (setup--fix-ssh-permissions)
    (princ "- [X] ssh permissions corrected\n"))

  ;;
  (princ "** Checking for required paths\n")
  (setup--check-for-path "~/org")
  (setup--check-for-path "~/notes")
  (setup--check-for-path "~/todo")

  (when (and (setup--shell-command-nil  "git config pull.rebase true")
             (setup--shell-command-nil  "git config rebase.autoStash true"))
    (princ "- [X] git autostash configured\n"))

  ;; symlinks
  (princ "** Creating symlinks\n")

  (shell-command "cd ~/Sync/dotfiles/scripts && find ~+ -exec ln -sf {} ~/.local/bin/ \;")

  (setup--make-dotfile-symlink "bin" "~/bin")

  (setup--make-dotfile-symlink "org-protocol.desktop" "~/.local/share/applications/org-protocol.desktop")
  (setup--make-dotfile-symlink "aspell.en.pws" "~/.aspell.en.pws")
  (setup--make-dotfile-symlink "mimeapps.list" "~/.config/mimeapps.list")
  (setup--make-dotfile-symlink "Xresources" "~/.Xresources")
  (setup--make-dotfile-symlink "profile" "~/.profile")
  (setup--make-dotfile-symlink "bash_profile" "~/.bash_profile")
  (setup--make-dotfile-symlink "mbsyncrc" "~/.mbsyncrc")
  (setup--make-dotfile-symlink "bashrc" "~/.bashrc")
  (setup--make-dotfile-symlink "bash_logout" "~/.bash_logout")
  (setup--make-dotfile-symlink "conkyrc" "~/.conkyrc")
  (setup--make-dotfile-symlink "fdignore" "~/.fdignore")
  (setup--make-dotfile-symlink "mutt" "~/.mutt")
  (setup--make-dotfile-symlink "mailcap" "~/.mailcap")
  (setup--make-dotfile-symlink "pycodestyle" "~/.config/pycodestyle")

  (setup--make-symlink "~/Sync/emacs-backups" "~/emacs-backups")

  ;; systemctl  start --user emacs.service
  (setup--make-dotfile-symlink "emacs.service" "~/.config/systemd/user/emacs.service")

  (setup--make-dotfile-symlink "kitty" "~/.config/kitty")
  (setup--make-dotfile-symlink "xinitrc" "~/.xinitrc")
  (setup--make-dotfile-symlink ".xmobarrc" "~/.xmobarrc")
  (setup--make-dotfile-symlink "vim/vimrc" "~/.vimrc")
  (setup--make-dotfile-symlink "ssh/config" "~/.ssh/config")
  (setup--make-dotfile-symlink "ncmpcpp/config" "~/.ncmpcpp/config")
  (setup--make-dotfile-symlink "mpd/mpd.conf" "~/.mpd/mpd.conf")
  (setup--make-dotfile-symlink "xbindkeysrc" "~/.xbindkeysrc")
  (setup--make-dotfile-symlink "local/share/applications/emacsclient.desktop" "~/.local/share/applications/emacsclient.desktop")
  (setup--make-dotfile-symlink "config/autostart/xbindkeys.desktop" "~/.config/autostart/xbindkeys.desktop")

  (when (string-match ".*ubuntu.*" (shell-command-to-string "uname -a"))
    (when (executable-find "fdfind")
      (setup--make-symlink (executable-find "fdfind") "~/.local/bin/fd")))

  (setup--make-dotfile-symlink "nvim" "~/.config/nvim")
  (setup--make-dotfile-symlink "tmux.conf" "~/.tmux.conf")
  (setup--make-dotfile-symlink "doom.d" "~/.doom.d")
  (setup--make-dotfile-symlink "xmonad" "~/.xmonad")
  ;; (setup--make-dotfile-symlink "vim/vim" "~/.vim")
  (setup--make-dotfile-symlink "../Fonts" "~/.fonts")

  (princ "** Setting custom mimetypes\n")
  (shell-command "cp mime/* ~/.local/share/mime/packages/ && update-mime-database ~/.local/share/mime")

  (princ "** Setting up org desktop protocol\n")
  (shell-command "xdg-mime default org-protocol.desktop x-scheme-handler/org-protocol")

  ;; (princ "** Setting up CERN certificate\n")
  ;; FIXME: sudo doesn't work here .. put an org block? or use `compile`
  ;; (shell-command "sudo mkdir -p /usr/local/share/ca-certificates/ && sudo cp ~/.dotfiles/CERN\ Root\ Certification\ Authority\ 2.crt /usr/local/share/ca-certificates/CERN\ Root\ Certification\ Authority\ 2.crt && sudo update-ca-trust")

  (princ "** Updating font cache\n")
  (call-process "fc-cache" nil 0 nil "-f" "-v")

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

  )

;;;###autoload
(defun setup-system ()

  "Keep a list of useful programs and other things.
Make sure they are installed and the computer is set up ok"

  (interactive)
  (setup--create-org-buffer #'setup--check-functions))

(provide 'setup-system)
;;; setup-system.el ends here
