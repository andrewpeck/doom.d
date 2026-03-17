;; -*- lexical-binding: t; -*-
;;------------------------------------------------------------------------------
;; Evil Config
;;------------------------------------------------------------------------------

(after! evil

  (setopt evil-indent-convert-tabs nil

          ;; Whether C-i jumps forward in the jump list (like Vim).
          evil-want-C-i-jump t

          ;; only substitute the 1st match by default (reverse vim behavior)
          evil-ex-substitute-global t

          ;; Switch to the new window after splitting
          evil-split-window-below t
          evil-vsplit-window-right t)

  (evil-select-search-module 'evil-search-module 'evil-search)
  (add-hook! 'debugger-mode-hook #'turn-on-evil-mode))

;; try to make sure that fundamental mode buffers use evil
;; https://emacs.stackexchange.com/questions/16693/auto-enable-minor-modes-in-fundamental-mode
(add-hook 'after-change-major-mode-hook
          (defun hook/turn-on-evil-mode ()
            "Turn on evil mode in fundamental mode"
            (when (eq major-mode 'fundamental-mode)
              (evil-local-mode))))

(after! evil-escape
  (setq evil-escape-key-sequence "jk"))

;; don't make escape annoyingly close popups UHG
(advice-remove 'evil-force-normal-state
               '+evil-escape-a)
