;;------------------------------------------------------------------------------
;; P-Search
;;------------------------------------------------------------------------------

(use-package p-search
  :demand t
  :config

  (evil-define-key '(normal) p-search-mode-map "a" #'p-search-add-dwim)
  (evil-define-key '(normal) p-search-mode-map "e" #'p-search-edit-dwim)
  (evil-define-key '(normal) p-search-mode-map "C" #'p-search-add-candidate-generator)
  (evil-define-key '(normal) p-search-mode-map "r" #'p-search-refresh-buffer)
  (evil-define-key '(normal) p-search-mode-map "R" #'p-search-hard-refresh-buffer)
  (evil-define-key '(normal) p-search-mode-map "D" #'p-search-kill-entity-at-point)
  (evil-define-key '(normal) p-search-mode-map "j" #'p-search-next-item)
  (evil-define-key '(normal) p-search-mode-map "k" #'p-search-prev-item)
  (evil-define-key '(normal) p-search-mode-map "o" #'p-search-observe)
  (evil-define-key '(normal) p-search-mode-map "P" #'p-search-add-prior)
  (evil-define-key '(normal) p-search-mode-map "+" #'p-search-increase-preview-size)
  (evil-define-key '(normal) p-search-mode-map "-" #'p-search-decrease-preview-size)
  (evil-define-key '(normal) p-search-mode-map "<tab>" #'p-search-toggle-section)
  (evil-define-key '(normal) p-search-mode-map "<return>" #'p-search-find-document)
  (evil-define-key '(normal) p-search-mode-map "v" #'p-search-view-document)
  (evil-define-key '(normal) p-search-mode-map "q" #'p-search-quit)
  (evil-define-key '(normal) p-search-mode-map "\C-o" #'p-search-display-document)
  ;; (evil-define-key '(normal) p-search-mode-map "i" #'p-search-importance)
  ;; (evil-define-key '(normal) p-search-mode-map "r" #'p-search-reinstantiate-prior)
  ;; (evil-define-key '(normal) p-search-mode-map "C-o" #'p-search-display-file)
  ;; (evil-define-key '(normal) p-search-mode-map "1" #'p-search-show-level-1)
  ;; (evil-define-key '(normal) p-search-mode-map "2" #'p-search-show-level-2)
  ;; (evil-define-key '(normal) p-search-mode-map "3" #'p-search-show-level-3)
  ;; (evil-define-key '(normal) p-search-mode-map "j g"  #'p-search-jump-candidate-generators)
  ;; (evil-define-key '(normal) p-search-mode-map "j p"  #'p-search-jump-priors)
  ;; (evil-define-key '(normal) p-search-mode-map "j r"  #'p-search-jump-results)
  )
