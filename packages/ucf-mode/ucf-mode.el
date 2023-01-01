;;; ../.dotfiles/doom.d/lisp/ucf-mode.el -*- lexical-binding: t; -*-

;;  Major mode for editing Xilinx UCF files
;;
;; This program is free software.  It is released under the terms of the GNU
;; GPL version 3 or later.  See the file LICENSE for full text.
;;
;; commentary:
;; To enable, add the following lines to your .emacs:
;;
;;    (autoload 'ucf-mode "ucf-mode" "Xilinx UCF mode" t)
;;    (add-to-list 'auto-mode-alist '("\\.ucf\\'" . ucf-mode))


(require 'generic-x)

;; "Keywords" introduce a new constraint. Almost every line should
;; start with one of these, I think.  Exceptions seem to be some
;; TIMESPEC constraints where attributes (like OFFSET) implicitly
;; refer to the most recent TIMESPEC?
(setq ucf-constraint-keywords
      '("NET" "INST" "PIN" "TIMESPEC" "TIMEGRP" "CONFIG" "AREA_GROUP"))

;; "Attributes" are the FOO in "FOO=BAR" statements.  This list is not
;; exhaustive!  I'm also including other "joining" keywords that say
;; what the constraint means, like FROM-THRU-TO, and PERIOD.
(setq ucf-constraint-attributes-etc
      '("TNM_NET" "TPTHRU" "PERIOD" "OFFSET" "OUT" "VALID" "BEFORE" "AFTER" "LOC"
        "IOSTANDARD" "FROM" "THRU" "TO" "TNM" "LOC" "RLOC" "BEL"
        "BUFG" "CLOCK_DEDICATED_ROUTE" "DIFF_TERM" "FAST" "FLOAT"
        "IODELAY_GROUP" "PART" "IDELAY_VALUE" "SIGNAL_PATTERN"
        "CONFIG_MODE" "DCI_CASCADE" "ENABLE_SUSPEND" "PROHIBIT"
        "MCB_PERFORMANCE" "POST_CRC" "POST_CRC_ACTION" "POST_CRC_FREQ"
        "POST_CRC_INIT_FLAG" "POST_CRC_SIGNAL" "POST_CRC_SOURCE"
        "STEPPING" "VCCAUX" "VREF" "RANGE" "GROUP" "PLACE"))

;; "Constants" are pre-defined values (the "BAR" in "FOO=BAR") as well
;; as "tags" like TIG, and defined units like "MHz".
(setq ucf-constraint-constants
      ;; Miscelaneous values -- not exhaustive
      '("HIGH" "RISING" "FALLING" "IN" "DATAPATHONLY" "TIG" "FALSE"
        "TRUE" "YES" "NO" "PULLUP" "UPPER" "LOWER" "CLK" "OE" "SR"
        "DATA_GATE" "N/A" "FILTERED" "UNFILTERED" "STANDARD"
        "EXTENDED" "ENABLE" "DISABLE" "ONESHOT" "HALT" "CONTINUE"
        "OFF" "ALWAYSACTIVE" "FREEZE" "CLOSED"
        ;; Units
        "MHz" "GHz" "kHz" "ps" "ns" "micro" "ms" "\%"
        ;; IOSTANDARDs -- believed to be exhaustive for 6-series parts
        ;; LVCMOS
        "LVCMOS12" "LVCMOS15" "LVCMOS18" "LVCMOS25"
        ;; LVDCI
        "LVDCI_15""LVDCI_18" "LVDCI_25" "LVDCI_DV2_15" "LVDCI_DV2_18"
        "LVDCI_DV2_25" "HSLVDCI_15" "HSLVDCI_18" "HSLVDCI_25"
        ;; HSTL
        "HSTL_I" "HSTL_III" "HSTL_I_18" "HSTL_ III_18" "HSTL_I_12"
        "HSTL_ I_DCI" "HSTL_ III_DCI" "HSTL_ I_DCI_18" "HSTL_ III_DCI_18"
        "HSTL_II" "HSTL_II_18" "HSTL_II_DCI" "HSTL_II_DCI_18"
        "HSTL_II_T_DCI" "HSTL_II_T_DCI_18" "DIFF_HSTL_ II"
        "DIFF_HSTL_II_18" "DIFF_HSTL_II_DCI" "DIFF_HSTL_II_DCI_18"
        "DIFF_HSTL_II_T_DCI" "DIFF_HSTL_II_T_DCI_18" "DIFF_HSTL_I"
        "DIFF_HSTL_I_18" "DIFF_HSTL_I_DCI" "DIFF_HSTL_I_DCI_18"
        ;; SSTL
        "SSTL2_I" "SSTL18_I" "SSTL2_I_DCI" "SSTL18_I_DCI" "SSTL2_II"
        "SSTL18_II" "SSTL15" "SSTL2_II_DCI" "SSTL18_II_DCI" "SSTL15_DCI"
        "DIFF_SSTL2_I" "DIFF_SSTL18_I" "DIFF_SSTL2_I_DCI"
        "DIFF_SSTL18_I_DCI" "DIFF_SSTL2_II" "DIFF_SSTL18_II" "DIFF_SSTL15"
        "DIFF_SSTL2_II_DCI" "DIFF_SSTL18_II_DCI" "DIFF_SSTL15_DCI"
        "SSTL2_II_T_DCI" "SSTL18_II_T_DCI" "SSTL15_T_DCI"
        ;; LVDS
        "LVDS_25""LVDSEXT_25"  "BLVDS_25"
        ;; Misc
        "HT_25" "RSDS_25" "LVPECL_25"))

(setq things-with-xy-locations
      ;; From UG625 v.13.4 pp47-49
      '("BSCAN" "BUFDS" "BUFGCTRL" "BUFGMUX" "BUFHCE" "BUFH" "BUFIO2FB"
        "BUFIO2" "BUFIODQS" "BUFIO""BUFO" "BUFPLL_MCB" "BUFPLL" "BUFR"
        "CAPTURE" "CFG_IO_ACCESS" "CRC32" "CRC64" "DCIRESET" "DCI" "DCM_ADV"
        "DCM" "DNA_PORT" "DPM" "DSP48" "EFUSE_USR" "EMAC" "FIFO16"
        "GLOBALSIG" "GT11CLK" "GT11" "GTPA1_DUAL" "GTP_DUAL" "GTXE1"
        "GTX_DUAL" "IBUFDS_GTXE1" "ICAP" "IDELAYCTRL" "ILOGIC" "IOB"
        "IODELAY" "IPAD" "MCB" "MMCM_ADV" "MONITOR" "OCT_CAL" "OLOGIC"
        "OPAD" "PCIE" "PCILOGIC" "PLL_ADV" "PMCD" "PMVBRAM" "PMVIOB" "PMV"
        "PPC405_AV" "PPC440" "PPR_FRAME" "RAMB16" "RAMB18" "RAMB36" "RAMB8"
        "SLICE" "STARTUP" "SYSMON" "TEMAC" "TIEOFF" "USR_ACCESS"))

(setq located-things-re
      (concat "\\<"
              (regexp-opt things-with-xy-locations nil)
              "_X[0-9]+Y[0-9]+"
              "\\>"))

(setq things-with-numbers
      '("INTERNAL_VREF_BANK" "VCCOSENSEMODE"))

(setq numbered-things-re
      (concat "\\<"
              (regexp-opt things-with-numbers nil)
              "[0-9]+"
              "\\>"))

(setq ucf-numbers-re
      (concat "\\<"
              "[0-9]+\.?[0-9]+"
              "\\>"))

(setq ucf-string-re "\"[^\s]+\"")

;; name-introducers" imply that the next symbol is a new name in the
;; constraint file. That is in a like "FOO BAR BAZ ...", if "FOO" is a
;; name-introducer, then "BAR" is a name.
(setq ucf-name-introducers
      '("NET" "INST" "TIMESPEC" "TIMEGRP" "AREA_GROUP" "AFTER"))

(define-generic-mode 'ucf-mode
  ;; comment list
  '("#")

  ;; keyword list
  nil

  ;; font lock list:
  ;;    a list of additional expressions to highlight.  Each
  ;;    element of this list should have the same form as an element of
  ;;    font-lock-keywords.

  `((,(regexp-opt ucf-constraint-keywords 'symbols) . 'font-lock-keyword-face)
    (,ucf-numbers-re . 'highlight-numbers-number)
    (,(regexp-opt ucf-constraint-attributes-etc 'symbols) .  'font-lock-type-face)
    (,numbered-things-re . 'font-lock-type-face)
    (,(regexp-opt ucf-constraint-constants 'symbols) .  'font-lock-constant-face)
    (,located-things-re . 'font-lock-constant-face)
    ;; Anchored regexp should highlight the BAR in FOO BAR or FOO "BAR".
    (,(regexp-opt ucf-name-introducers 'symbols) "\\_<\"?\\(.*?\\)\"?\\_>\\(.+\\)"
     nil nil
     (1 'font-lock-string-face))
    (,ucf-string-re . 'font-lock-string-face)
    )

  ;; auto-mode-list
  '(".ucf\\'")

  ;; function list
  (list
   (lambda ()

     ;; The syntax is changed only for table SYNTAX-TABLE, which defaults to
     ;; the current buffer's syntax table.
     ;; CHAR may be a cons (MIN . MAX), in which case, syntaxes of all characters
     ;; in the range MIN to MAX are changed.
     ;;
     ;;  Space or -  whitespace syntax.    w   word constituent.
     ;;  _           symbol constituent.   .   punctuation.
     ;;  (           open-parenthesis.     )   close-parenthesis.
     ;;  "           string quote.         \   escape.
     ;;  $           paired delimiter.     '   expression quote or prefix operator.
     ;;  <           comment starter.      >   comment ender.
     ;;  /           character-quote.      @   inherit from parent table.
     ;;  |           generic string fence. !   generic comment fence.

     ;; Treat these characters as punctuation, meaning that
     ;; e.g. "|KEYWORD" is treated similarly to "KEYWORD".
     (modify-syntax-entry ?| ".")
     (modify-syntax-entry ?= ".")
     (modify-syntax-entry ?\; ".")
     ;; Quotes can be part of symbols.  That is, we expect
     ;; 'NET "FOO"' to define a net called '"FOO"'.
     (modify-syntax-entry ?\" "_")
     ;; Also, allow ? to be part of a symbol so that heierarchical
     ;; names are OK.
     (modify-syntax-entry ?\? "_")
     (modify-syntax-entry ?\] "_")
     (modify-syntax-entry ?\[ "_")
     (modify-syntax-entry ?\. "_")
     ))

  ;; docstring
  "Major mode for editing Xilinx User Constraints Files")

(provide 'ucf-mode)
