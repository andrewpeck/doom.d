;; -*- lexical-binding: t; -*-
;;------------------------------------------------------------------------------
;; Evil Config
;;------------------------------------------------------------------------------

(after! evil

  (setq evil-indent-convert-tabs nil)

  (add-hook! 'debugger-mode-hook #'turn-on-evil-mode)

  ;; Whether C-i jumps forward in the jump list (like Vim).
  (setq evil-want-C-i-jump t)

  ;; https://github.com/doomemacs/doomemacs/issues/6478
  ;; Configuration A
  (setq org-fold-core-style 'overlays)
  (evil-select-search-module 'evil-search-module 'evil-search)

  ;; Configuration B
  ;; (setq org-fold-core-style 'text-properties)
  ;; (evil-select-search-module 'evil-search-module 'isearch)

  ;; for some reason this makes org tables really really slow
  ;; but for some reason now if I am removing this advice then git time machine breaks!!
  ;; (advice-remove 'set-window-buffer #'ad-Advice-set-window-buffer)

  ;; only substitute the 1st match by default (reverse vim behavior)
  (setq evil-ex-substitute-global t)

  ;; Switch to the new window after splitting
  (setq evil-split-window-below t
        evil-vsplit-window-right t))

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
