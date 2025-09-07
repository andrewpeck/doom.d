;; -*- lexical-binding: t; -*-

;;---------------------------------------------------------------------------------
;; Package Configs
;;---------------------------------------------------------------------------------

;; Completely disable management of the mode-line in popups:
(remove-hook '+popup-buffer-mode-hook #'+popup-set-modeline-on-enable-h)
;; Make sure evil is on in popups
(add-hook '+popup-buffer-mode-hook #'turn-on-evil-mode)

(map! :map +popup-buffer-mode-map
      :m "C-<up>" #'+popup/raise)

(set-popup-rule! ".*eww.*" :ignore t)
(set-popup-rule! ".*poporg.*" :ignore t)
(set-popup-rule! ".*notmuch.*" :ignore t)

(set-popup-rule! "*devdocs*"
  :side 'right
  :size 0.5
  :select t
  :quit nil)

(set-popup-rule! ".*HN.*"
  :side 'right
  :size 0.5
  :select t
  :quit nil)

(set-popup-rule! "*info*"
  :modeline t
  :side 'right
  :quit nil
  :size 0.5
  :select t
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

;; (set-popup-rule! ".*toc.*"
;;   :modeline nil
;;   :side 'left
;;   :quit nil
;;   :size 0.6
;;   :slot -4
;;   :select t
;;   :ttl 0)

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
  :quit nil
  :size 0.1
  :select nil
  :ttl 0)

(set-popup-rule! "*Backtrace*"
  :modeline nil
  :side 'bottom
  :quit nil
  :size 0.3
  :select t
  :ttl nil)

;; ;; synctex view
;; (set-popup-rule! ".*\.tex"
;;   :quit nil
;;   :actions nil
;;   :side 'right
;;   :slot -10
;;   :select t
;;   :ttl 0)
