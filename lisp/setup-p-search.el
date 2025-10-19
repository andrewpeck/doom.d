;; -*- lexical-binding: t; -*-

(use-package p-search
  :defer t
  :commands (p-search)
  :config

  (load (concat (file-name-directory (file-truename (find-library-name "p-search" ))) "extensions/p-search-x-info.el"))

  (require 'p-search-x-info)

  (defun my/p-search-emacs (search-query)
    (interactive "sSearchs Term: ")
    (p-search-setup-buffer
     `(:group ((:prior-template p-search-prior-query
                :args ((query-string . ,search-query) (importance . medium)))
               (:candidate-generator psx-info-candidate-generator :args ((info-node . emacs)))
               (:candidate-generator psx-info-candidate-generator :args ((info-node . elisp)))))))

  (map! :after p-search
        :map p-search-mode-map
        :n "a"        #'p-search-add-dwim
        :n "e"        #'p-search-edit-dwim
        :n "C"        #'p-search-add-candidate-generator
        :n "r"        #'p-search-refresh-buffer
        :n "R"        #'p-search-hard-refresh-buffer
        :n "D"        #'p-search-kill-entity-at-point
        :n "j"        #'p-search-next-item
        :n "k"        #'p-search-prev-item
        :n "o"        #'p-search-observe
        :n "P"        #'p-search-add-prior
        :n "+"        #'p-search-increase-preview-size
        :n "-"        #'p-search-decrease-preview-size
        :n "<tab>"    #'p-search-toggle-section
        :n "<return>" #'p-search-find-document
        :n "v"        #'p-search-view-document
        :n "q"        #'p-search-quit
        :n "\C-o"     #'p-search-display-document))
