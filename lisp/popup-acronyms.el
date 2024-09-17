;;;  -*- lexical-binding: t; -*-

(defvar acronym-alist
  '(
    ;; GAPS
    ("FMC"   . "Fusion Measurement Circuit")
    ("DRS4"  . "Domino Ring Sampler 4")
    ("MT"    . "Master Trigger")
    ("MTB"   . "Master Trigger Board")
    ("RB"    . "Readout Board")
    ("LT"    . "Local Trigger")
    ("LTB"   . "Local Trigger Board")

    ;; CSC
    ("AFEB"  . "Anode Front-end Board")
    ("ALCT"  . "Anode Local Charge Track")
    ("CCB"   . "Crate Regulator Board")
    ("CFEB"  . "Cathode Front-end Board")
    ("CRB"   . "Crate Regulator Board")
    ("CSC"   . "Cathode Strip Chamber")
    ("DCFEB" . "Digital Cathode Front-end Board")
    ("DMB"   . "Data Mother Board")
    ("LVDB"  . "Low Voltage Distribution Board")
    ("LVMB"  . "Low Voltage Mother Board")
    ("MPC"   . "Muon Port Card")
    ("OALCT" . "Optical ALCT")
    ("ODMB"  . "Optical DMB")
    ("OTMB"  . "Optical Trigger Mother Board")
    ("TMB"   . "Trigger Mother Board")

    ;; GEM
    ("GEM"   . "GAS Electron Multiplier")
    ("GEB"   . "GEM Electronics Board")
    ("OH"    . "OptoHybrid")
    ("VFAT"  . "Very Forward ATLAS Totem")
    ("ROB"   . "Readout Board")
    ("TOTEM" . "")

    ;; ETL
    ("ETL"     . "Endcap Timing Layer")
    ("ETROC"   . "Endcap Timing Readout Chip")
    ("ALTIROC" . "??? Readout Chip")
    ("PB"      . "Power Board")

    ;; ATLAS
    ("ATLAS" . "")
    ("CTP"   . "Central Trigger Processor. The place where the L0 is generated")
    ("FEX"   . "Feature EXtractor processors. FEX are the L1 calorimeter trigger processors that will be installed in ATLAS during the LS2 shutdown and will become the basis for the L0Calo trigger system at HL-LHC")
    ("EFEX"  . "FE processors that identify electron/photon/tau candidates")
    ("GFEX"  . "Global FEX")
    ("JFEX"  . "Jet FEX")

    ;;  L0MDT
    ("MDT"   . "Monitored Drift Tube")
    ("VAMC"  . "VHDL Advanced Memory Controller (Guillermo)")
    ("ULL"   . "User Logic Layer")
    ("HAL"   . "Hardware Abstraction Layer")
    ("ULT"   . "User Logic Top; the top level of the ULL")
    ("UPT"   . "The Uc irvine PT calculator")
    ("MPT"   . "The Max Planck PT calculator")
    ("LSF"   . "Legendre Segment Finder (UC Irvine)")
    ("CSF"   . "Compact Segment Finder (Max Planck)")
    ("MTC"   . "Muon Track Candidate Manager (Priya)")
    ("MPL"   . "Muon Pipeline (Guillermo)")
    ("CSM"   . "Chamber Service Mezzanine (U Michigan)")
    ("HP"    . "Hit Processor (Guillermo)")
    ("TAR"   . "Tube Address Remapping (Guillermo)")
    ("SLC"   . "Sector Logic Candidate")
    ("HEG"   . "Hit Extraction Group (Guillermo);\n  - filters MDT hits in each station based on the information from the SLC")
    ("H2S"   . "Hits to Segments; H2S is a wrapper for several HPS;")
    ("MCM"   . "See UCM; this acronym is used in the PDR")
    ("UCM"   . "Interfaces with the incoming SLCs and selects which candidates are processed depending on their priority and the availability of resources.")
    ("HPS"   . "Hit Processing Station; transforms the MDT hit data, filters the data and calculates the segments for each station")
    ("FM"    . "Fast Monitoring (Priya)")
    ("APBUS" . "Apollo Platform Bus (Guillermo Bus)")
    ("ASD"   . "Amplifier, Shaper, Discriminator ASIC that reads out the MDT chambers"))

  "Alist of acronyms to be expanded.

Set a user dictionary with e.g.

(setq acronym-alist \\='(\"TLA\" . \"Three letter acronym\"))")

(save-excursion
  (dolist (acronym acronym-alist)
    (goto-char (point-min))
    (while (re-search-forward (concat "\\<" (car acronym) "\\>") nil t)
      (let ((start (match-beginning 0))
            (end (match-end 0)))
        (setq my-overlay
              (make-overlay start end)))
      (overlay-put my-overlay 'help-echo (concat (car acronym) ": " (cdr acronym)))
      (overlay-put my-overlay 'face '(bold underline)))))

(defun expand-acronym ()
  (interactive)
  (let ((description
         (cdr (assoc (upcase (symbol-name (symbol-at-point)))
                     acronym-alist))))
    (if description
        (popup-tip description
                   :point (point)
                   :around t
                   :height 30
                   :scroll-bar t
                   :margin t))))

(after! evil
  (evil-leader/set-key "pa" #'expand-acronym))
