;; -*- lexical-binding: t; -*-

(defun make-link-fn (name link)
 "Macro to create a Open Link Command"
 (let ((fnname (intern (string-replace " " "-" (eval name)))))
   (defalias fnname `(lambda () (interactive) (browse-url ,link)) name)))

(defvar xilinx-docs-list nil "alist of Xilinx doc links with names")

(setq xilinx-docs-list
        '(("Xilinx ⟶ 7 Series ⟶ Clocking" . "https://docs.xilinx.com/v/u/en-US/ug472_7Series_Clocking")
          ("Xilinx ⟶ 7 Series ⟶ Product Table" . "https://docs.xilinx.com/v/u/en-US/cost-optimized-product-selection-guide")
          ("Xilinx ⟶ 7 Series ⟶ Configuration Guide" . "https://docs.amd.com/v/u/en-US/ug470_7Series_Config")
          ("Xilinx ⟶ 7 Series ⟶ FPGAs SelectIO Resources" . "https://docs.amd.com/v/u/en-US/ug471_7Series_SelectIO")
          ("Xilinx ⟶ 7 Series ⟶ Artix 7/Data Sheet: DC/Switching" . "https://docs.xilinx.com/v/u/en-US/ds181_Artix_7_Data_Sheet")
          ("Xilinx ⟶ 7 Series ⟶ Kintex 7/Data Sheet: DC/Switching" . "https://docs.xilinx.com/v/u/en-US/ds182_Kintex_7_Data_Sheet")
          ("Xilinx ⟶ Ultrascale ⟶ Kintex UltraScale FPGAs Data Sheet: DC/Switching" . "https://docs.xilinx.com/v/u/en-US/ds892-kintex-ultrascale-data-sheet")
          ("Xilinx ⟶ Ultrascale ⟶ Kintex UltraScale Plus FPGAs Data Sheet: DC/Switching" . "https://docs.xilinx.com/v/u/en-US/ds922-kintex-ultrascale-plus")
          ("Xilinx ⟶ Ultrascale+ ⟶ Product Table" . "https://docs.xilinx.com/v/u/en-US/ultrascale-plus-fpga-product-selection-guide")
          ("Xilinx ⟶ Ultrascale ⟶ Product Table" . "https://docs.amd.com/v/u/en-US/ultrascale-fpga-product-selection-guide")))

(dolist (doc xilinx-docs-list)
  (make-link-fn (car doc) (cdr doc)))

(defun alist-keys (alist)
  (mapcar 'car alist))

(defun xilinx/open-doc () ""
       (interactive)
       (browse-url (cdr (assoc  (completing-read "Document: " (alist-keys xilinx-docs-list)) xilinx-docs-list))))

(map! :leader :prefix "o"
      ;; (:prefix ("p" . "PsiQuantum")
      ;;          "l" #'psi/sysarch-logic
      ;;          "m" #'psi/sysarch-models)
      (:prefix ("X" . "Xilinx")
               (:desc "Documentation" "X" #'xilinx/open-doc)))
