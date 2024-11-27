;;---------------------------------------------------------------------------------
;; Package Configs
;;---------------------------------------------------------------------------------

(after! +popup
  ;; Completely disable management of the mode-line in popups:
  (remove-hook '+popup-buffer-mode-hook #'+popup-set-modeline-on-enable-h)
  ;; Make sure evil is on in popups
  (add-hook '+popup-buffer-mode-hook #'turn-on-evil-mode)

  (evil-define-key
    'motion +popup-buffer-mode-map "q" #'bury-buffer)
  (evil-define-key
    'motion +popup-buffer-mode-map (kbd "C-<up>") #'+popup/raise)

  ;; (set-popup-rules!
  ;;  '(("^ \\*" :slot -1) ; fallback rule for special buffers
  ;;    ("^\\*" :select t)
  ;;    ("^\\*Completions" :slot -1 :ttl 0)
  ;;    ("^\\*\\(?:scratch\\|Messages\\)" :ttl t)
  ;;    ("^\\*Help" :slot -1 :size 0.2 :select t)
  ;;    ("^\\*doom:"
  ;;     :size 0.35 :select t :modeline t :quit t :ttl t)))

  ;; display-buffer-alist
  (set-popup-rule! ".*eww.*"
    :ignore t
    :modeline t
    :side 'right
    :quit nil
    :size 1.0
    ;; :vslot -4
    :select nil
    :ttl 0)

  (set-popup-rule! ".*poporg.*"
    :ignore t
    :modeline t
    :side 'right
    :quit nil
    :size 1.0
    ;; :vslot -4
    :select nil
    :ttl 0)

  (set-popup-rule! ".*cider-repl.*"
    :modeline t
    :side 'right
    :quit nil
    :size 0.5
    ;; :vslot -4
    :select nil
    :ttl 0)

  (set-popup-rule! ".*mu4e-main.*"
    :modeline t
    :side 'left
    :quit nil
    :size 0.5
    :slot -4
    :select t
    :ttl 0)

  (set-popup-rule! ".*toc.*"
    :modeline nil
    :side 'left
    :quit nil
    :size 0.6
    :slot -4
    :select t
    :ttl 0)

  (set-popup-rule! ".*mu4e-headers.*"
    :modeline t
    :side 'right
    :quit nil
    :size 0.5
    :slot -4
    :select t
    :ttl 0)

  (set-popup-rule! ".*compilation.*specs.*"
    :modeline nil
    :side 'bottom
    :quit t
    :size 0.1
    :select nil
    :ttl 0)

  ;; ;; synctex view
  ;; (set-popup-rule! ".*\.tex"
  ;;   :quit nil
  ;;   :actions nil
  ;;   :side 'right
  ;;   :slot -10
  ;;   :select t
  ;;   :ttl 0)


  )
