;; -*- lexical-binding: t; -*-
;;
(require 'bookmark-url)

(defun make-link-fn (name link)
  "Macro to create a Open Link Command"
  (let ((fnname (intern (string-replace " " "-" (eval name)))))
    (defalias fnname `(lambda () (interactive) (browse-url ,link)) name)))

(after! bookmark-url
  (bookmark-url-setup 'xilinx/open-doc
                      :bookmarks-file (concat doom-user-dir "xilinx-docs.json")
                      :prompt "Xilinx Datasheets")

  (map! :leader :prefix "o" (:desc "Xilinx Documentation" "X" #'xilinx/open-doc)))
