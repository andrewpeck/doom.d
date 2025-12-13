;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((nil . (
         ;; this is needed if doom.d is a submodule which is symlinked to ~/.doom.d
         ;; project-try-vc--search is pretty dumb; if it determines that the repo is a submodule
         ;; it just looks in the parent path and assumes that is the parent module
         ;; with a symlink the parent path is ~/ which is of course not the git repo this belongs to
         ;; so we get an error
         (project-vc-merge-submodules . nil)
         )))
