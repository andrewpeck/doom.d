;;; orcad-theme.el --- Awesome Emacs color theme on white background

;;; Commentary:


;;; Code:

;;; Options.

(defgroup orcad nil
  "Orcad theme options.
The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom orcad-scale-outline-headlines t
  "Scale `outline' (and `org') level-1 headlines.
This can be nil for unscaled, t for using the theme default, or a scaling number."
  :type '(choice
          (const :tag "Unscaled" nil)
          (const :tag "Default provided by theme" t)
          (number :tag "Set scaling"))
  :group 'orcad)

(defcustom orcad-scale-org-agenda-structure t
  "Scale Org agenda structure lines, like dates.
This can be nil for unscaled, t for using the theme default, or a scaling number."
  :type '(choice
          (const :tag "Unscaled" nil)
          (const :tag "Default provided by theme" t)
          (number :tag "Set scaling"))
  :group 'orcad)

(defun orcad-scale-font (control default-height)
  "Function for splicing optional font heights into face descriptions.
CONTROL can be a number, nil, or t.  When t, use DEFAULT-HEIGHT."
  (cond
   ((numberp control) (list :height control))
   ((eq t control) (list :height default-height))
   (t nil)))

;;; Theme Faces.

(deftheme orcad
  "Face colors with a light background.
   Basic, Font Lock, Isearch, Gnus, Message, Org mode, Diff, Ediff,
   Flyspell, Semantic, and Ansi-Color faces are included -- and much
   more...")

;; define colors
(let (
      (orcad-black          '"#222")
      (orcad-white          '"#ccc")
      (orcad-red            '"#f00")
      (orcad-blue           '"#00f")
      (orcad-green          '"#080")
      (orcad-pink           '"#f08")
      (orcad-purple         '"#804")
      (orcad-blue-green     '"#048")
      (orcad-green-blue     '"#084")
      )

  ;; create classes
  (let (
        ;; Orcad generic colors.
        (cancel                        `(:foreground ,orcad-black :slant italic :strike-through t))
        (clock-line                    `(:box (:line-width 1 :color ,orcad-blue) :foreground ,orcad-black :background "#eec900"))

        ;; code block
        (code-block                    `(:foreground ,orcad-black :background "#efefef" :extend t))
        ;;(code-inline                   `(:foreground ,orcad-black :background "#efefef" :extend t))
        (code-inline                   `(:foreground ,orcad-red :background "#ffffff" :extend t))

        ;;
        (column                        `(:height 1.0 :weight normal :slant normal :underline nil :strike-through nil :foreground "#e6ad4f" :background "#fff2de"))

        ;; Completion
        (completion-inline             `(:foreground ,orcad-black  :weight normal :inherit hl-line)) ; Like Google.
        (completion-selected-candidate `(:foreground ,orcad-white :weight bold   :background ,orcad-blue-green))
        (completion-other-candidates   `(:foreground ,orcad-blue-green :weight bold   :background ,orcad-green-blue))

        ;; diff
        (diff-added                    `(:background ,orcad-green))
        (diff-changed                  `(:foreground "#0000ff" :background "#ddddff"))
        (diff-header                   `(:weight bold :foreground "#800000" :background "#ffffaf"))
        (diff-hunk-header              `(:foreground "#990099" :background "#ffeeff"))
        (diff-none                     `(:foreground "#888888"))
        (diff-refine-added             `(:background "#97f295"))
        (diff-refine-removed           `(:background "#ffb6ba"))
        (diff-removed                  `(:background "#fee8e9"))

        (directory                     `(:foreground ,orcad-blue-green :weight bold :background "white"))
        (file                          `(:foreground ,orcad-black :background "white"))
        (function-param                `(:foreground ,orcad-blue-green))
        (grep-file-name                `(:foreground ,orcad-blue :weight bold )) ; Used for grep hits.
        (grep-line-number              `(:foreground ,orcad-purple :weight bold ))

        ;;
        (highlight-blue                `(:background "#e6ecff" :extend t))
        (highlight-blue2               `(:background "#e4f1f9" :extend t))
        (highlight-gray                `(:background "#e4e4e3" :extend t))
        (highlight-green               `(:background "#d5f1cf" :extend t))
        (highlight-red                 `(:background "#ffc8c8" :extend t))
        (highlight-yellow              `(:background "#f6fecd" :extend t))

        ;; hyperlinks
        (link                          `(:family "Numbus Mono Ps" :weight bold   :underline t :foreground ,orcad-blue))
        (link-no-underline             `(:family "Numbus Mono Ps" :weight bold                :foreground ,orcad-blue))

        ;; mail
        (mail-header-name              `(:family "Sans Serif" :weight normal :foreground "#a3a3a2"))
        (mail-header-other             `(:family "Sans Serif" :slant normal :foreground "#666666"))
        (mail-read                     `(:foreground "#8c8c8c"))
        (mail-read-high                `(:foreground "#808080"))
        (mail-ticked                   `(:foreground ,orcad-red))
        (mail-to                       `(:family "Sans Serif" :underline nil :foreground ,orcad-blue))
        (mail-unread                   `(:weight bold :foreground ,orcad-black))
        (mail-unread-high              `(:weight bold :foreground "#135985"))

        (marked-line                   `(:foreground "#aa0000" :background "#ffaaaa"))
        (match                         `(:weight bold :background "#ffff00")) ; occur patterns + match in helm for files + match in Org files.

        ;; Org mode levels
        (ol1 `(:height 1.3 :weight bold :slant normal :foreground ,orcad-red :extend t))
        (ol2 `(:height 1.2 :weight bold :slant normal :foreground ,orcad-green  :extend t))
        (ol3 `(:height 1.1 :weight bold :slant normal :foreground ,orcad-blue  :extend t))
        (ol4 `(:height 1.0 :weight bold :slant normal :foreground ,orcad-red :extend t))
        (ol5 `(:height 1.0 :weight bold :slant normal :foreground ,orcad-pink   :extend t))
        (ol6 `(:height 1.0 :weight bold :slant italic :foreground ,orcad-blue-green  :extend t))
        (ol7 `(:height 1.0 :weight bold :slant italic :foreground ,orcad-green  :extend t))
        (ol8 `(:height 1.0 :weight bold :slant italic :foreground ,orcad-red :extend t))

        (paren-matched                `(:background "#c0e8c3")) ; Or take that green for region?
        (paren-unmatched              `(:weight bold :underline ,orcad-red :foreground ,orcad-black :background "#ffa5a5"))
        (region                       `(:background ,orcad-blue :extend t))
        (shadow                       `(:foreground ,orcad-black))
        (string                       `(:foreground ,orcad-blue-green)) ; affects the color in the mode bar of the project as well as other strings
        (subject                      `(:foreground ,orcad-black :family "Sans Serif" :weight bold ))
        (symlink                      `(:foreground ,orcad-blue))
        (tab                          `(:foreground "#e8e8e8" :background ,orcad-white))
        (trailing                     `(:foreground "#e8e8e8" :background "#ffffab"))
        (volatile-highlight           `(:underline nil :foreground ,orcad-white :background "#9e3699"))
        (volatile-highlight-supersize `(:height 1.1 :underline nil :foreground ,orcad-white :background "#9e3699")) ; flash-region
        (vc-branch                    `(:box (:line-width 1 :color ,orcad-red) :foreground ,orcad-black :background "#aaffaa"))
        (xml-attribute                `(:foreground ,orcad-red))
        (xml-tag                      `(:foreground "#ae1b9a"))
        (highlight-current-tag        `(:background "#e8e8ff")) ; #EEF3F6 or #FFEB26
        )

    (custom-theme-set-faces
     'orcad

                                        ;(set-background-color "white")

     ;; Git gutter fringe
     `(git-gutter-fr:modified           ((t (:foreground ,orcad-red))))
     `(git-gutter-fr:added              ((t (:foreground ,orcad-green))))
     `(git-gutter-fr:deleted            ((t (:foreground ,orcad-red))))

     ;; Standard fonts
     `(default     ((t (:foreground ,orcad-black :background "white"))))
     `(bold        ((t (:foreground ,orcad-black :background "white" :weight bold))))
     `(bold-italic ((t (:foreground ,orcad-black :background "white" :weight bold :slant italic))))
     `(italic      ((t (:foreground ,orcad-black :background "white"             :slant italic))))
     `(underline   ((t (:underline t :background "white"))))
     `(cursor      ((t (:background ,orcad-blue))))

     ;; Highlighting faces.
     `(fringe                   ((t (:foreground "black" :background "white"))))

     `(highlight                ((t ,highlight-blue)))
     `(region                   ((t ,region)))
     `(secondary-selection      ((t ,match))) ; Used by Org-mode for highlighting matched entries and keywords.

                                        ; search
     `(isearch                  ((t (:foreground ,orcad-red :background ,orcad-red :underline ,orcad-black))))
     `(isearch-fail             ((t (:foreground ,orcad-black :background "#ffcccc" :weight bold))))
     `(lazy-highlight           ((t (:foreground ,orcad-black :background ,orcad-green)))) ; Isearch others (see `match').

                                        ;
     `(trailing-whitespace      ((t ,trailing)))
     `(query-replace            ((t (:inherit isearch))))
     `(whitespace-hspace        ((t (:foreground "#d2d2d2")))) ; see also `nobreak-space'
     `(whitespace-indentation   ((t ,tab)))
     `(whitespace-line          ((t (:foreground ,orcad-red :background "#ffff88"))))
     `(whitespace-tab           ((t ,tab)))
     `(whitespace-trailing      ((t ,trailing)))

     ;;
     ;; centaur-tabs
     `(centaur-tabs-default ((t (:foreground ,orcad-blue :background ,orcad-white))))
     `(centaur-tabs-unselected ((t (:foreground ,orcad-blue :background ,orcad-white))))
     `(centaur-tabs-selected ((t (:foreground ,orcad-blue :background ,orcad-white))))
     `(centaur-active-bar-face ((t (:foreground ,orcad-blue :background ,orcad-white))))
     `(centaur-modified-marker-selected ((t (:foreground ,orcad-blue :background ,orcad-white))))
     `(centaur-modified-marker-unselected ((t (:foreground ,orcad-blue :background ,orcad-white))))

     ;; ((centaur-tabs-unselected &inherit tab-bar-tab-inactive))
     ;; (centaur-tabs-selected-modified   :background bg :foreground teal)
     ;; (centaur-tabs-unselected-modified :background bg-alt :foreground teal)
     ;; (centaur-tabs-active-bar-face :background (if (bound-and-true-p -modeline-bright) modeline-bg highlight))
     ;; (centaur-tabs-modified-marker-selected
     ;;  :foreground (if (bound-and-true-p -modeline-bright) modeline-bg highlight)
     ;;  :inherit 'centaur-tabs-selected)
     ;; (centaur-tabs-modified-marker-unselected
     ;;  :foreground (if (bound-and-true-p -modeline-bright) modeline-bg highlight)
     ;;  :inherit 'centaur-tabs-unselected)

     ;; Mode line faces.
     `(mode-line           ((t (:box (:line-width 1 :color "#1a2f54") :foreground ,orcad-white :background ,orcad-blue-green))))
     `(mode-line-inactive  ((t (:box (:line-width 1 :color "#4e4e4c") :foreground ,orcad-white :background ,orcad-green-blue))))

     `(mode-line-buffer-id ((t (:weight bold :foreground ,orcad-white))))
     `(mode-line-emphasis  ((t (:weight bold :foreground ,orcad-white))))
     `(mode-line-highlight ((t (:weight bold :foreground ,orcad-white))))

     `(doom-modeline-bar                ((t (:weight bold :foreground ,orcad-pink)))) ;;
     `(doom-modeline-buffer-file        ((t (:weight bold :foreground ,orcad-white    )))) ;;
     `(doom-modeline-buffer-major-mode  ((t (             :foreground ,orcad-white    )))) ;;
     `(doom-modeline-buffer-minor-mode  ((t (             :foreground ,orcad-white    )))) ;;
     `(doom-modeline-buffer-modified    ((t (:weight bold :foreground ,orcad-red      )))) ;;
     `(doom-modeline-buffer-path        ((t (:weight bold :foreground ,orcad-white    )))) ;;
     `(doom-modeline-debug              ((t (:weight bold :foreground ,orcad-white    )))) ;;
     `(doom-modeline-evil-emacs-state   ((t (:weight bold :foreground ,orcad-red   )))) ;;
     `(doom-modeline-evil-insert-state  ((t (:weight bold :foreground ,orcad-red   )))) ;;
     `(doom-modeline-evil-motion-state  ((t (:weight bold :foreground ,orcad-green    )))) ;;
     `(doom-modeline-evil-normal-state  ((t (:weight bold :foreground ,orcad-green    )))) ;;
     `(doom-modeline-evil-replace-state ((t (:weight bold :foreground ,orcad-red      )))) ;;
     `(doom-modeline-evil-visual-state  ((t (:weight bold :foreground ,orcad-red   )))) ;;
     `(doom-modeline-highlight          ((t (:weight bold :foreground ,orcad-green    )))) ;;
     `(doom-modeline-info               ((t (             :foreground ,orcad-white    )))) ;;
     `(doom-modeline-project-dir        ((t (:weight bold :foreground ,orcad-white    )))) ;;
     `(doom-modeline-lsp-error          ((t (:weight bold :foreground ,orcad-red     :background ,orcad-blue-green)))) ;;
     `(doom-modeline-lsp-running        ((t (:weight bold :foreground ,orcad-red   :background ,orcad-blue-green)))) ;;
     `(doom-modeline-lsp-warning        ((t (:weight bold :foreground ,orcad-red   :background ,orcad-blue-green)))) ;;
     `(doom-modeline-lsp-success        ((t (:weight bold :foreground ,orcad-green    :background ,orcad-blue-green)))) ;;
     `(doom-modeline-urgent             ((t (:weight bold :foreground ,orcad-red     :background ,orcad-blue-green)))) ;;
     `(doom-modeline-warning            ((t (:weight bold :foreground ,orcad-red   :background ,orcad-blue-green)))) ;;

     ;; Escape and prompt faces.
     `(minibuffer-prompt            ((t (:weight bold :foreground ,orcad-black :background "gold"))))
     `(minibuffer-noticeable-prompt ((t (:weight bold :foreground ,orcad-black :background "gold"))))
     `(escape-glyph                 ((t (             :foreground ,orcad-blue))))
     `(error                        ((t (:weight bold :foreground ,orcad-red))))
     `(note                         ((t (:weight bold :foreground ,orcad-green))))
     `(warning                      ((t (:weight bold :foreground ,orcad-red))))
     `(success                      ((t (             :foreground ,orcad-green))))

     ;; Font lock faces.
     `(font-lock-builtin-face              ((t (:foreground ,orcad-red :bold t       ))))
     `(font-lock-comment-delimiter-face    ((t (:foreground ,orcad-green                ))))
     `(font-lock-comment-face              ((t (:foreground ,orcad-green :slant normal  ))))
     `(font-lock-constant-face             ((t (:foreground ,orcad-pink                 ))))
     `(font-lock-doc-face                  ((t (:foreground ,orcad-green                ))))
     ;;`(font-lock-doc-string-face           ((t (:foreground ,orcad-green                )))) ; XEmacs only, but is used for HTML exports from org2html (and not interactively)
     `(font-lock-function-name-face        ((t (:foreground ,orcad-pink                 ))))
     `(font-lock-keyword-face              ((t (:foreground ,orcad-red :bold t       ))))
     `(font-lock-preprocessor-face         ((t (:foreground ,orcad-black                 ))))
     `(font-lock-regexp-grouping-backslash ((t (:weight bold :inherit nil            ))))
     `(font-lock-regexp-grouping-construct ((t (:weight bold :inherit nil            ))))
     `(font-lock-string-face               ((t ,string)))
     `(font-lock-type-face                 ((t (:foreground ,orcad-black :weight bold    ))))
     `(font-lock-variable-name-face        ((t (:foreground ,orcad-black :weight normal  ))))
     `(font-lock-warning-face              ((t (:weight bold :foreground ,orcad-red     ))))
     `(vhdl-font-lock-attribute-face       ((t (:foreground ,orcad-pink                 ))))
     `(vhdl-font-lock-function-face        ((t (:foreground ,orcad-black :weight bold    ))))

     ;; Button and link faces.
     `(link         ((t ,link)))
     `(link-visited ((t (:underline t :foreground ,orcad-red))))
     `(button       ((t (:underline t :foreground ,orcad-blue))))
     `(header-line  ((t (:box (:line-width 1 :color ,orcad-black) :foreground ,orcad-black :background "#f0f0f0"))))

     ;; indent

     `(indent-guide-face ((t (:foreground "black" :background "black"))))

     `(highlight-indent-guides-character-face       ((t (:background "white" :foreground ,orcad-white))))
     `(highlight-indent-guides-even-face            ((t (:background "white" :foreground "lightgray"))))
     `(highlight-indent-guides-odd-face             ((t (:background "white" :foreground "lightgray"))))
     `(highlight-indent-guides-stack-character-face ((t (:background "white" :foreground "lightgray"))))
     `(highlight-indent-guides-stack-even-face      ((t (:background "white" :foreground ,orcad-green))))
     `(highlight-indent-guides-stack-odd-face       ((t (:background "white" :foreground ,orcad-green))))
     `(highlight-indent-guides-top-character-face   ((t (:background "white" :foreground "lightgray"))))
     `(highlight-indent-guides-top-even-face        ((t (:background "white" :foreground "lightgray"))))
     `(highlight-indent-guides-top-odd-face         ((t (:background "white" :foreground "lightgray"))))

     ;; Gnus faces.
     `(gnus-button                   ((t (:weight normal))))
     `(gnus-cite-attribution-face    ((t (:foreground "#5050b0"))))
     `(gnus-cite-1                   ((t (:foreground "#5050b0" :background "#f6f6f6"))))
     `(gnus-cite-2                   ((t (:foreground "#660066" :background "#f6f6f6"))))
     `(gnus-cite-3                   ((t (:foreground "#007777" :background "#f6f6f6"))))
     `(gnus-cite-4                   ((t (:foreground "#990000" :background "#f6f6f6"))))
     `(gnus-cite-5                   ((t (:foreground "#000099" :background "#f6f6f6"))))
     `(gnus-cite-6                   ((t (:foreground "#bb6600" :background "#f6f6f6"))))
     `(gnus-cite-7                   ((t (:foreground "#5050b0" :background "#f6f6f6"))))
     `(gnus-cite-8                   ((t (:foreground "#660066" :background "#f6f6f6"))))
     `(gnus-cite-9                   ((t (:foreground "#007777" :background "#f6f6f6"))))
     `(gnus-cite-10                  ((t (:foreground "#990000" :background "#f6f6f6"))))
     `(gnus-emphasis-bold            ((t (:weight bold))))
     `(gnus-emphasis-highlight-words ((t (:foreground "yellow" :background ,orcad-black))))
     `(gnus-group-mail-1             ((t (:weight bold :foreground "#ff50b0"))))
     `(gnus-group-mail-1-empty       ((t (:foreground "#5050b0"))))
     `(gnus-group-mail-2             ((t (:weight bold :foreground "#ff0066"))))
     `(gnus-group-mail-2-empty       ((t (:foreground "#660066"))))
     `(gnus-group-mail-3             ((t ,mail-unread)))
     `(gnus-group-mail-3-empty       ((t ,mail-read)))
     `(gnus-group-mail-low           ((t ,cancel)))
     `(gnus-group-mail-low-empty     ((t ,cancel)))
     `(gnus-group-news-1             ((t (:weight bold :foreground "#ff50b0"))))
     `(gnus-group-news-1-empty       ((t (:foreground "#5050b0"))))
     `(gnus-group-news-2             ((t (:weight bold :foreground "#ff0066"))))
     `(gnus-group-news-2-empty       ((t (:foreground "#660066"))))
     `(gnus-group-news-3             ((t ,mail-unread)))
     `(gnus-group-news-3-empty       ((t ,mail-read)))
     `(gnus-group-news-4             ((t (:weight bold :foreground "#ff0000"))))
     `(gnus-group-news-4-empty       ((t (:foreground "#990000"))))
     `(gnus-group-news-5             ((t (:weight bold :foreground "#ff0099"))))
     `(gnus-group-news-5-empty       ((t (:foreground "#000099"))))
     `(gnus-group-news-6             ((t (:weight bold :foreground "gray50"))))
     `(gnus-group-news-6-empty       ((t (:foreground "#808080"))))
     `(gnus-header-content           ((t ,mail-header-other)))
     `(gnus-header-from              ((t (:family "Sans Serif" :foreground ,orcad-black))))
     `(gnus-header-name              ((t ,mail-header-name)))
     `(gnus-header-newsgroups        ((t (:family "Sans Serif" :foreground "#3399cc"))))
     `(gnus-header-subject           ((t ,subject)))
     `(gnus-picon                    ((t (:foreground "yellow" :background ,orcad-white))))
     `(gnus-picon-xbm                ((t (:foreground "yellow" :background ,orcad-white))))
     `(gnus-server-closed            ((t (:slant italic :foreground "blue" :background ,orcad-white))))
     `(gnus-server-denied            ((t (:weight bold :foreground orcad-red :background ,orcad-white))))
     `(gnus-server-opened            ((t (:family "Sans Serif" :foreground ,orcad-white :foreground "#466bd7"))))
     `(gnus-signature                ((t (:slant italic :foreground "#8b8d8e"))))
     `(gnus-splash                   ((t (:foreground "#ff8c00"))))
     `(gnus-summary-cancelled        ((t ,cancel)))
     `(gnus-summary-high-ancient     ((t ,mail-unread-high)))
     `(gnus-summary-high-read        ((t ,mail-read-high)))
     `(gnus-summary-high-ticked      ((t ,mail-ticked)))
     `(gnus-summary-high-unread      ((t ,mail-unread-high)))
     `(gnus-summary-low-ancient      ((t (:slant italic :foreground ,orcad-black))))
     `(gnus-summary-low-read         ((t (:slant italic :foreground "#999999" :background "#e0e0e0"))))
     `(gnus-summary-low-ticked       ((t ,mail-ticked)))
     `(gnus-summary-low-unread       ((t (:slant italic :foreground ,orcad-black))))
     `(gnus-summary-normal-ancient   ((t ,mail-read)))
     `(gnus-summary-normal-read      ((t ,mail-read)))
     `(gnus-summary-normal-ticked    ((t ,mail-ticked)))
     `(gnus-summary-normal-unread    ((t ,mail-unread)))
     `(gnus-summary-selected         ((t (:foreground ,orcad-white :background "#008cd7"))))
     `(gnus-x-face                   ((t (:foreground ,orcad-black :background ,orcad-white))))

     ;; Message faces.
     `(message-header-name       ((t ,mail-header-name)))
     `(message-header-cc         ((t ,mail-to)))
     `(message-header-other      ((t ,mail-header-other)))
     `(message-header-subject    ((t ,subject)))
     `(message-header-to         ((t ,mail-to)))
     `(message-cited-text        ((t (:foreground "#5050b0" :background "#f6f6f6"))))
     `(message-separator         ((t (:family "Sans Serif" :weight normal :foreground "#bdc2c6"))))
     `(message-header-newsgroups ((t (:family "Sans Serif" :foreground "#3399cc"))))
     `(message-header-xheader    ((t ,mail-header-other)))
     `(message-mml               ((t (:foreground "forest green"))))

     ;; Diff.
     `(diff-added             ((t ,diff-added)))
     `(diff-changed           ((t ,diff-changed)))
     `(diff-context           ((t ,diff-none)))
     `(diff-file-header       ((t ,diff-header)))
     `(diff-file1-hunk-header ((t (:foreground "dark magenta" :background "#eaf2f5"))))
     `(diff-file2-hunk-header ((t (:foreground "#2b7e2a" :background "#eaf2f5"))))
     `(diff-function          ((t (:foreground "#cc99cc"))))
     `(diff-header            ((t ,diff-header)))
     `(diff-hunk-header       ((t ,diff-hunk-header)))
     `(diff-index             ((t ,diff-header)))
     `(diff-indicator-added   ((t (:foreground "#3a993a" :background "#cdffd8"))))
     `(diff-indicator-changed ((t (:background "#dbedff"))))
     `(diff-indicator-removed ((t (:foreground "#cc3333" :background "#ffdce0"))))
     `(diff-refine-added      ((t ,diff-refine-added)))
     `(diff-refine-change     ((t (:background "#ddddff"))))
     `(diff-refine-removed    ((t ,diff-refine-removed)))
     `(diff-removed           ((t ,diff-removed)))

     ;; SMerge.
     `(smerge-mine           ((t ,diff-changed)))
     `(smerge-other          ((t ,diff-added)))
     `(smerge-base           ((t ,diff-removed)))
     `(smerge-markers        ((t (:background "#ffe5cc"))))
     `(smerge-refined-change ((t (:background "#aaaaff"))))

     ;; Ediff.
     `(ediff-current-diff-A ((t (:background ,orcad-red   :extend t))))
     `(ediff-current-diff-B ((t (:background ,orcad-green :extend t))))
     `(ediff-current-diff-C ((t (:background ,orcad-green-blue  :extend t))))
     `(ediff-even-diff-A    ((t (:background ,orcad-pink  :extend t))))
     `(ediff-even-diff-B    ((t (:background ,orcad-pink  :extend t))))
     `(ediff-fine-diff-A    ((t (:background ,orcad-pink  :extend t))))
     `(ediff-fine-diff-B    ((t (:background ,orcad-green      :extend t))))
     `(ediff-odd-diff-A     ((t (:background ,orcad-pink  :extend t))))
     `(ediff-odd-diff-B     ((t (:background ,orcad-pink  :extend t))))

     ;; ;; Semantic faces.
     ;; `(semantic-decoration-on-includes ((t (:underline ,cham-4))))
     ;; `(semantic-decoration-on-private-members-face ((t (:background ,alum-2))))
     ;; `(semantic-decoration-on-protected-members-face ((t (:background ,alum-2))))
     `(semantic-decoration-on-unknown-includes ((t (:background "#fff8f8"))))
     ;; `(semantic-decoration-on-unparsed-includes ((t (:underline ,orange-3))))
     `(semantic-highlight-func-current-tag-face ((t ,highlight-current-tag)))
     `(semantic-tag-boundary-face ((t (:overline "#777777")))) ; Method separator.
     ;; `(semantic-unmatched-syntax-face ((t (:underline ,red-1))))

     `(Info-title-1-face               ((t ,ol1)))
     `(Info-title-2-face               ((t ,ol2)))
     `(Info-title-3-face               ((t ,ol3)))
     `(Info-title-4-face               ((t ,ol4)))
     `(ace-jump-face-foreground        ((t (:weight bold :foreground ,orcad-black :background "#fea500"))))
     `(ahs-face                        ((t (:background "#e4e4ff"))))
     `(ahs-definition-face             ((t (:background "#ffb6c6"))))
     `(ahs-plugin-defalt-face          ((t (:background "#ffe4ff")))) ; Current.
     `(anzu-match-1                    ((t (:foreground ,orcad-black :background "aquamarine"))))
     `(anzu-match-2                    ((t (:foreground ,orcad-black :background "springgreen"))))
     `(anzu-match-3                    ((t (:foreground ,orcad-black :background "red"))))
     `(anzu-mode-line                  ((t (:foreground ,orcad-black :background "#80ff80"))))
     `(anzu-mode-line-no-match         ((t (:foreground ,orcad-black :background "#ff8080"))))
     `(anzu-replace-highlight          ((t (:inherit query-replace))))
     `(anzu-replace-to                 ((t (:weight bold :foreground "#bd33fd" :background "#fdbd33"))))
     `(auto-dim-other-buffers-face     ((t (:background "#f7f7f7"))))
     `(avy-background-face             ((t (:background "#a9a9a9"))))
     `(avy-lead-face                   ((t (:weight bold :foreground ,orcad-black :background "#fea500"))))
     `(bbdb-company                    ((t (:slant italic :foreground "steel blue"))))
     `(bbdb-field-name                 ((t (:weight bold :foreground "steel blue"))))
     `(bbdb-field-value                ((t (:foreground "steel blue"))))
     `(bbdb-name                       ((t (:underline t :foreground "#ff6633"))))
     `(bmkp-light-autonamed            ((t (:background "#f0f0f0"))))
     `(bmkp-light-fringe-autonamed     ((t (:foreground "#5a5a5a" :background "#d4d4d4"))))
     `(bmkp-light-fringe-non-autonamed ((t (:foreground "#ffffcc" :background "#01fffb")))) ; default
     `(bmkp-light-non-autonamed        ((t (:background "#bffffe"))))
     `(bmkp-no-local                   ((t (:background "pink"))))
     `(browse-kill-ring-separator-face ((t (:foreground "red"))))
     `(calendar-month-header           ((t (:weight bold :foreground "#4f4a3d" :background "#ffffcc"))))
     `(calendar-today                  ((t (:weight bold :foreground "#4f4a3d" :background "#ffffcc"))))
     `(calendar-weekday-header         ((t (:weight bold :foreground "#1662af"))))
     `(calendar-weekend-header         ((t (:weight bold :foreground "#4e4e4e"))))
     `(cfw:face-annotation             ((t (:foreground "green" :background "red"))))
     `(cfw:face-day-title              ((t (:foreground "#c9c9c9"))))
     `(cfw:face-default-content        ((t (:foreground "#2952a3"))))
     `(cfw:face-default-day            ((t (:weight bold))))
     `(cfw:face-disable                ((t (:foreground "DarkGray"))))
     `(cfw:face-grid                   ((t (:foreground "#dddddd"))))
     `(cfw:face-header                 ((t (:foreground "#1662af" :background ,orcad-white :weight bold))))
     `(cfw:face-holiday                ((t (:foreground "#777777" :background "#e4ebfe"))))
     `(cfw:face-periods                ((t (:foreground ,orcad-white :background "#668cd9" :slant italic))))
     `(cfw:face-saturday               ((t (:foreground "#4e4e4e" :background ,orcad-white :weight bold))))
     `(cfw:face-select                 ((t (:foreground "#4a95eb" :background "#edf1fa"))))
     `(cfw:face-sunday                 ((t (:foreground "#4e4e4e" :background ,orcad-white :weight bold))))
     `(cfw:face-title                  ((t (:height 1.0 :foreground "#676767" :weight bold :inherit variable-pitch))))
     `(cfw:face-today                  ((t (:foreground "#4f4a3d" :background "#ffffcc"))))
     `(cfw:face-today-title            ((t (:foreground ,orcad-white :background "#1766b1"))))
     `(cfw:face-toolbar                ((t (:background ,orcad-white))))
     `(cfw:face-toolbar-button-off     ((t (:foreground "#cfcfcf" :background ,orcad-white))))
     `(cfw:face-toolbar-button-on      ((t (:foreground "#5e5e5e" :background "#f6f6f6"))))
     `(change-log-date                 ((t (:foreground "purple"))))
     `(change-log-file                 ((t (:weight bold :foreground "#4183c4"))))
     `(change-log-list                 ((t (:foreground ,orcad-black :background "#75eec7"))))
     `(change-log-name                 ((t (:foreground "#008000"))))
     `(circe-highlight-all-nicks-face  ((t (:foreground "blue" :background "#f0f0f0")))) ; other nick names
     `(circe-highlight-nick-face       ((t (:foreground "#009300" :background "#f0f0f0")))) ; messages with my nick cited
     `(circe-my-message-face           ((t (:foreground "#8b8b8b" :background "#f0f0f0"))))
     `(circe-originator-face           ((t (:foreground "blue"))))
     `(circe-prompt-face               ((t (:foreground "red"))))
     `(circe-server-face               ((t (:foreground "#99cae5"))))
     `(comint-highlight-input          ((t (:weight bold :foreground "#0000ff" :inherit nil))))
     ;;`(comint-highlight-prompt         ((t (:weight bold :foreground ,orcad-black :background "gold"))))
     `(comint-highlight-prompt         ((t (:weight bold :foreground "#0000ff" :inherit nil))))

     ;; `(ac-selection-face ((t ,completion-selected-candidate)))
     `(ac-selection-face ((t (:weight bold :foreground ,orcad-white :background "orange")))) ; TEMP For diff'ing AC from Comp.
     `(ac-candidate-face ((t ,completion-other-candidates)))
     `(ac-completion-face ((t ,completion-inline)))
     `(ac-candidate-mouse-face ((t (:inherit highlight))))
     `(popup-scroll-bar-background-face ((t (:background "#ebf4fe"))))
     `(popup-scroll-bar-foreground-face ((t (:background "#d1dae4")))) ; Scrollbar (visible).

     `(company-tooltip-common-selection     ((t (:weight normal :foreground "#f9eccc" :inherit company-tooltip-selection)))) ; Prefix + common part in tooltip (for selection).
     `(company-tooltip-selection            ((t ,completion-selected-candidate))) ; Suffix in tooltip (for selection).
     `(company-tooltip-annotation-selection ((t (:weight normal :foreground "#f9eccc")))) ; Annotation (for selection).
     `(company-tooltip-common               ((t (:weight normal :foreground "#b000b0" :inherit company-tooltip)))) ; Prefix + common part in tooltip.
     `(company-tooltip                      ((t ,completion-other-candidates))) ; Suffix in tooltip.
     `(company-tooltip-annotation           ((t (:weight normal :foreground "#2415ff")))) ; Annotation.
     `(company-preview-common               ((t ,completion-inline)))
     `(company-scrollbar-bg                 ((t (:background "#ebf4fe"))))
     `(company-scrollbar-fg                 ((t (:background "#d1dae4")))) ; Scrollbar (visible).

     `(compare-windows ((t (:background "#ffff00"))))
     ;; `(completions-common-part ((t (:foreground "red" :weight bold))))
     ;; `(completions-first-difference ((t (:foreground "green" :weight bold))))

     `(compilation-error ((t (:weight bold :foreground "red")))) ; Used for grep error messages.
     `(compilation-info ((t ,grep-file-name)))
     `(compilation-line-number ((t ,grep-line-number)))
     `(compilation-warning ((t (:weight bold :foreground "orange"))))
     `(compilation-mode-line-exit ((t (:weight bold :foreground "green")))) ; :exit[matched]
     `(compilation-mode-line-fail ((t (:weight bold :foreground "violet")))) ; :exit[no match]
     `(compilation-mode-line-run ((t (:weight bold :foreground "orange")))) ; :run

     `(css-property ((t (:foreground "#00aa00"))))
     `(css-selector ((t (:weight bold :foreground "blue"))))

     `(custom-button ((t (:box (:line-width 2 :style released-button) :foreground ,orcad-black :background "lightgrey"))))
     `(custom-button-mouse ((t (:box (:line-width 2 :style released-button) :foreground ,orcad-black :background "grey90"))))
     `(custom-button-pressed ((t (:box (:line-width 2 :style pressed-button) :foreground ,orcad-black :background "light grey"))))
     `(custom-button-pressed-unraised ((t (:underline t :foreground "magenta4"))))
     `(custom-button-unraised ((t (:underline t))))
     `(custom-changed ((t (:foreground ,orcad-white :background "blue"))))
     `(custom-comment ((t (:background "gray85"))))
     `(custom-comment-tag ((t (:foreground "blue"))))
     `(custom-documentation ((t (nil))))
     `(custom-face-tag ((t (:family "Sans Serif" :height 1.2 :weight bold))))
     `(custom-group-tag ((t (:height 1.2 :weight bold :foreground "blue1"))))
     `(custom-group-tag-1 ((t (:family "Sans Serif" :height 1.2 :weight bold :foreground "red1"))))
     `(custom-invalid ((t (:foreground "yellow" :background "red"))))
     `(custom-link ((t (:underline t :foreground "blue1"))))
     `(custom-modified ((t (:foreground ,orcad-white :background "blue"))))
     `(custom-rogue ((t (:foreground "pink" :background ,orcad-black))))
     `(custom-saved ((t (:underline t))))
     `(custom-set ((t (:foreground "blue" :background ,orcad-white))))
     `(custom-state ((t (:foreground "green4"))))
     `(custom-themed ((t (:foreground ,orcad-white :background "blue1"))))
     `(custom-variable-button ((t (:weight bold :underline t))))
     `(custom-variable-tag ((t (:family "Sans Serif" :height 1.0 :weight bold :foreground "blue1"))))
     `(custom-visibility ((t ,link)))

     ;; diff
     `(diff-hl-change        ((t (             :foreground "blue3"   :background "#dbedff" ))))
     `(diff-hl-delete        ((t (             :foreground ,orcad-red   :background "#ffdce0" ))))
     `(diff-hl-dired-change  ((t (:weight bold :foreground ,orcad-black :background "#ffa335" ))))
     `(diff-hl-dired-delete  ((t (:weight bold :foreground ,orcad-red                         ))))
     `(diff-hl-dired-ignored ((t (:weight bold :foreground ,orcad-white :background "#c0bbab" ))))
     `(diff-hl-dired-insert  ((t (:weight bold :foreground "#b9b9ba"                       ))))
     `(diff-hl-dired-unknown ((t (             :foreground ,orcad-white :background "#3f3bb4" ))))
     `(diff-hl-insert        ((t (             :foreground ,orcad-green :background "#cdffd8" ))))
     `(diff-hl-unknown       ((t (             :foreground ,orcad-white :background "#3f3bb4" ))))

     `(diary-face ((t (:foreground "#87c9fc"))))

     `(dircolors-face-asm            ((t (:foreground ,orcad-black))))
     `(dircolors-face-backup         ((t (:foreground ,orcad-black))))
     `(dircolors-face-compress       ((t (:foreground ,orcad-red))))
     `(dircolors-face-dir            ((t ,directory)))
     `(dircolors-face-doc            ((t (:foreground ,orcad-black))))
     `(dircolors-face-dos            ((t (:foreground ,orcad-green))))
     `(dircolors-face-emacs          ((t (:foreground ,orcad-black))))
     `(dircolors-face-exec           ((t (:foreground ,orcad-green))))
     `(dircolors-face-html           ((t (:foreground ,orcad-black))))
     `(dircolors-face-img            ((t (:foreground "magenta3"))))
     `(dircolors-face-lang           ((t (:foreground ,orcad-black))))
     `(dircolors-face-lang-interface ((t (:foreground ,orcad-black))))
     `(dircolors-face-make           ((t (:foreground ,orcad-black))))
     `(dircolors-face-objet          ((t (:foreground ,orcad-black))))
     `(dircolors-face-package        ((t (:foreground ,orcad-black))))
     `(dircolors-face-paddb          ((t (:foreground ,orcad-black))))
     `(dircolors-face-ps             ((t (:foreground ,orcad-black))))
     `(dircolors-face-sound          ((t (:foreground "DeepSkyBlue"))))
     `(dircolors-face-tar            ((t (:foreground "red"))))
     `(dircolors-face-text           ((t (:foreground ,orcad-black))))
     `(dircolors-face-yacc           ((t (:foreground ,orcad-black))))

     ;; DIRED
     `(dired-directory               ((t ,directory)))
     `(dired-header                  ((t ,directory)))
     `(dired-ignored                 ((t (:strike-through t :foreground ,orcad-red))))
     `(dired-mark                    ((t ,marked-line)))
     `(dired-marked                  ((t ,marked-line)))
     `(dired-symlink                 ((t ,symlink)))

     ;; DIREDP
     `(diredfl-file-name              ((t ,file)))
     `(diredfl-dir-name               ((t ,directory)))
     `(diredfl-ignored-file-name      ((t ,shadow)))
     `(diredfl-compressed-file-suffix ((t (:foreground ,orcad-red))))
     `(diredfl-symlink                ((t (:foreground ,orcad-green))))
     `(diredfl-dir-heading            ((t ,directory)))
     `(diredfl-file-suffix            ((t (:foreground ,orcad-black))))
     `(diredfl-read-priv              ((t (:backgrount ,orcad-green-blue))))
     `(diredfl-exec-priv              ((t (:background ,orcad-red))))
     `(diredfl-write-priv             ((t (:background ,orcad-green))))
     `(diredfl-rare-priv              ((t (:background ,orcad-red))))
     `(diredfl-dir-priv               ((t (:fireground ,orcad-black))))
     `(diredfl-no-priv                ((t (:background "white"))))
     `(diredfl-number                 ((t (:foreground ,orcad-pink))))
     `(diredfl-date-time              ((t (:foreground ,orcad-black))))
     `(diredfl-executable-tag         ((t (:foreground "ForestGreen" :background ,orcad-white))))
     `(diredfl-flag-mark-line         ((t ,marked-line)))

     ;;
     `(eldoc-highlight-function-argument ((t (:weight bold :foreground "red" :background "#ffe4ff"))))
     `(elfeed-search-filter-face         ((t (:foreground "gray"))))
     ;;`(eww-form-checkbox                 ((t ())))
     ;;`(eww-form-select                   ((t ())))
     ;;`(eww-form-submit                   ((t ())))
     `(eww-form-text                     ((t (:weight bold :foreground "#40586f" :background "#a7cdf1"))))
     ;;`(eww-form-textarea                 ((t ())))
     `(file-name-shadow                  ((t ,shadow)))


     ;; Flyspell.
     `(flyspell-duplicate ((t (:underline (:style wave :color ,orcad-red) :inherit nil))))
     `(flyspell-incorrect ((t (:underline (:style wave :color ,orcad-red) :inherit nil))))

     ;; Flycheck
     `(flycheck-error                        ((t (:underline (:color ,orcad-red   :style wave) :weight bold :background ,orcad-red))))
     `(flycheck-info                         ((t (:underline (:color ,orcad-green :style wave) :weight bold))))
     `(flycheck-warning                      ((t (:underline (:color ,orcad-green :style wave)))))
     `(flycheck-error-list-line-number       ((t (:foreground ,orcad-purple))))
     `(flycheck-fringe-error                 ((t (:foreground ,orcad-red))))
     `(flycheck-fringe-info                  ((t (:foreground ,orcad-green))))
     `(flycheck-fringe-warning               ((t (:foreground "#f4a939"))))
     `(flycheck-color-mode-line-error-face   ((t (:background ,orcad-red))))
     `(flycheck-color-mode-line-warning-face ((t (:background "#ebc700"))))
     `(flycheck-color-mode-line-info-face    ((t (:background "yellow"))))

     ;; writegood
     `(writegood-passive-voice-face       ((t (:underline (:color ,orcad-green    :style wave) :background ,orcad-white))))
     `(writegood-duplicates-face          ((t (:underline (:color ,orcad-red :style wave) :background ,orcad-white))))
     `(writegood-weasels-face             ((t (:underline (:color ,orcad-green    :style wave) :background ,orcad-white))))

     ;;
     `(fancy-narrow-blocked-face             ((t (:foreground "#9998a4"))))

     ;;
     `(font-latex-bold-face         ((t (:foreground ,orcad-black :weight bold))))
     `(font-latex-italic-face       ((t (:foreground ,orcad-black :slant italic))))
     `(font-latex-math-face         ((t (:foreground ,orcad-blue-green))))
     `(font-latex-sectioning-1-face ((t ,ol1)))
     `(font-latex-sectioning-2-face ((t ,ol2)))
     `(font-latex-sectioning-3-face ((t ,ol3)))
     `(font-latex-sectioning-4-face ((t ,ol4)))
     `(font-latex-sectioning-5-face ((t ,ol5)))
     `(font-latex-sedate-face       ((t (:foreground ,orcad-red))))
     `(font-latex-string-face       ((t (:weight bold :foreground ,orcad-blue-green))))
     `(font-latex-script-char-face  ((t (:foreground ,orcad-red))))
     `(font-latex-verbatim-face     ((t ,code-block)))

     ;;
     `(git-commit-summary-face                ((t (:foreground ,orcad-black))))
     `(git-commit-comment-face                ((t (:slant italic :foreground "#696969"))))
     `(git-timemachine-commit                 ((t ,diff-removed)))
     `(git-timemachine-minibuffer-author-face ((t ,diff-added)))
     `(git-timemachine-minibuffer-detail-face ((t ,diff-header)))

     ;; Helm
     `(helm-action                          ((t (:foreground ,orcad-black))))
     `(helm-bookmark-file                   ((t ,file)))
     `(helm-bookmarks-su-face               ((t (:foreground "red"))))
     `(helm-buffer-directory                ((t ,directory)))
     ;;`(helm-non-file-buffer                 ((t (:slant italic :foreground "blue"))))
     ;;`(helm-buffer-file                     ((t (:foreground ,orcad-black))))
     `(helm-buffer-modified                 ((t (:slant italic :foreground ,orcad-purple))))
     `(helm-buffer-process                  ((t (:foreground "#008200"))))
     `(helm-candidate-number                ((t (:foreground ,orcad-black :background "#ffff66"))))
     `(helm-dir-heading                     ((t (:foreground "blue" :background "pink"))))
     `(helm-dir-priv                        ((t (:foreground "dark red" :background "light grey"))))
     `(helm-ff-directory                    ((t ,directory)))
     `(helm-ff-dotted-directory             ((t ,directory)))
     `(helm-ff-executable                   ((t (:foreground "green3" :background ,orcad-white))))
     `(helm-ff-file                         ((t (:foreground ,orcad-black))))
     `(helm-ff-invalid-symlink              ((t (:foreground "yellow" :background "red"))))
     `(helm-ff-symlink                      ((t ,symlink)))
     `(helm-file-name                       ((t (:foreground "blue"))))
     `(helm-gentoo-match-face               ((t (:foreground "red"))))
     `(helm-grep-file                       ((t ,grep-file-name)))
     `(helm-grep-lineno                     ((t ,grep-line-number)))
     `(helm-grep-match                      ((t ,match)))
     `(helm-grep-running                    ((t (:weight bold :foreground ,orcad-white))))
     `(helm-isearch-match                   ((t (:background "#ccffcc"))))
     `(helm-lisp-show-completion            ((t ,volatile-highlight-supersize))) ; See `helm-dabbrev'.
     ;;`(helm-ls-git-added-copied-face        ((t (:foreground ""))))
     ;;`(helm-ls-git-added-modified-face      ((t (:foreground ""))))
     ;;`(helm-ls-git-conflict-face            ((t (:foreground ""))))
     ;;`(helm-ls-git-deleted-and-staged-face  ((t (:foreground ""))))
     ;;`(helm-ls-git-deleted-not-staged-face  ((t (:foreground ""))))
     ;;`(helm-ls-git-modified-and-staged-face ((t (:foreground ""))))
     `(helm-ls-git-modified-not-staged-face ((t (:foreground "#ba36a5"))))
     ;;`(helm-ls-git-renamed-modified-face    ((t (:foreground ""))))
     ;;`(helm-ls-git-untracked-face           ((t (:foreground ""))))
     `(helm-match                           ((t ,match)))
     `(helm-moccur-buffer                   ((t (:foreground "#0066cc"))))
     `(helm-selection                       ((t (:background "#3875d6" :foreground ,orcad-white))))
     `(helm-selection-line                  ((t ,highlight-gray))) ; ???
     `(helm-separator                       ((t (:foreground "red"))))
     `(helm-source-header                   ((t (:weight bold :box (:line-width 1 :color "#c7c7c7") :background "#dedede" :foreground ,orcad-black))))
     `(helm-swoop-target-line-block-face    ((t (:background "#cccc00" :foreground "#222222"))))
     `(helm-swoop-target-line-face          ((t (:background "#ccccff"))))
     `(helm-swoop-target-word-face          ((t (:weight bold :foreground nil :background "#fdbd33"))))
     `(helm-visible-mark                    ((t ,marked-line)))
     `(helm-w3m-bookmarks-face              ((t (:underline t :foreground "cyan1"))))

     `(highlight-changes        ((t (:foreground nil)))) ;; blue "#2e08b5"
     `(highlight-changes-delete ((t (:strike-through nil :foreground nil)))) ;; red "#b5082e"
     `(highlight-symbol-face    ((t (:background "#ffffa0"))))

     `(hl-line ((t ,highlight-yellow))) ; Highlight current line.

     `(hl-tags-face ((t ,highlight-current-tag))) ; ~ Pair highlighting (matching tags).

     `(holiday-face ((t (:foreground "#777777" :background "#e4ebfe"))))

     `(html-helper-bold-face      ((t (:weight bold :foreground ,orcad-black))))
     `(html-helper-italic-face    ((t (:slant italic :foreground ,orcad-black))))
     `(html-helper-underline-face ((t (:underline t :foreground ,orcad-black))))
     `(html-tag-face              ((t (:foreground "blue"))))

     `(ilog-non-change-face ((t (:height 2.0 :foreground "#6434a3"))))
     `(ilog-change-face     ((t (:height 2.0 :foreground "#008200"))))
     `(ilog-echo-face       ((t (:height 2.0 :foreground "#006fe0"))))
     `(ilog-load-face       ((t (:foreground "#ba36a5"))))
     `(ilog-message-face    ((t (:foreground "#808080"))))

     `(info-file         ((t (:family "Sans Serif" :height 1.8 :weight bold :box (:line-width 1 :color "#0000cc") :foreground "cornflower blue" :background "LightSteelBlue1"))))
     `(info-header-node  ((t (:underline t :foreground "orange")))) ; nodes in header
     `(info-header-xref  ((t (:underline t :foreground "dodger blue")))) ; cross references in header
     `(info-index-match  ((t (:weight bold :foreground nil :background "#fdbd33")))) ; when using `i'
     `(info-menu-header  ((t ,ol2))) ; menu titles (headers) -- major topics
     `(info-menu-star    ((t (:foreground ,orcad-black)))) ; every 3rd menu item
     `(info-node         ((t (:underline t :foreground "blue")))) ; node names
     `(info-quoted-name  ((t ,code-inline)))
     `(info-string       ((t ,string)))
     `(info-title-1      ((t ,ol1)))
     `(info-xref         ((t (:underline t :foreground "#006daf")))) ; unvisited cross-references
     `(info-xref-visited ((t (:underline t :foreground "magenta4")))) ; previously visited cross-references

     `(js2-error                    ((t (:box (:line-width 1 :color ,orcad-red) :background ,orcad-red))))
     `(js2-external-variable        ((t (:foreground "#ff0000" :background "#fff8f8"))))
     `(js2-function-param           ((t ,function-param)))
     `(js2-instance-member          ((t (:foreground "DarkOrchid"))))
     `(js2-jsdoc-html-tag-delimiter ((t (:foreground ,orcad-red))))
     `(js2-jsdoc-html-tag-name      ((t (:foreground ,orcad-red))))
     `(js2-jsdoc-tag                ((t (:weight normal :foreground "#6434a3"))))
     `(js2-jsdoc-type               ((t (:foreground "SteelBlue"))))
     `(js2-jsdoc-value              ((t (:weight normal :foreground "#ba36a5")))) ; #800080
     `(js2-magic-paren              ((t (:underline t))))
     `(js2-private-function-call    ((t (:foreground "goldenrod"))))
     `(js2-private-member           ((t (:foreground "PeachPuff3"))))
     `(js2-warning                  ((t (:underline "orange"))))

     ;; Org non-standard faces.
     `(orcad-org-deadline-overdue ((t (:foreground "#f22659"))))
     `(orcad-org-deadline-today ((t (:weight bold :foreground "#4f4a3d" :background "#ffffcc"))))
     `(orcad-org-deadline-tomorrow ((t (:foreground "#40a80b"))))
     `(orcad-org-deadline-future ((t (:foreground "#40a80b"))))
     `(orcad-gnus-unseen ((t (:weight bold :foreground "#fc7202"))))
     `(orcad-gnus-date ((t (:foreground "#ff80bf"))))
     `(orcad-gnus-size ((t (:foreground "#8fbf60"))))
     `(orcad-todo-items-face ((t (:weight bold :foreground "#ff3125" :background "#ffff88"))))

     `(light-symbol-face ((t (:background "#ffffa0"))))

     ;;`(linum ((t (:foreground ,orcad-white :background ,orcad-red :weight bold))))
     `(line-number ((t (:foreground ,orcad-white :background ,orcad-blue-green :weight bold))))

     `(log-view-file ((t (:foreground "#0000cc" :background "#eaf2f5"))))
     `(log-view-message ((t (:foreground ,orcad-black :background "#edea74"))))
     `(lui-button-face ((t ,link)))
     `(lui-highlight-face ((t (:box '(:line-width 1 :color "#cc0000") :foreground "#cc0000" :background "#ffff88")))) ; my nickname
     `(lui-time-stamp-face ((t (:foreground "purple"))))

     `(lsp-ui-doc-header ((t (:background ,orcad-blue-green :foreground ,orcad-white))))
     `(lsp-ui-doc-background ((t (:background ,orcad-green :foreground "black"))))

     ;; Magit
                                        ;`(magit-blame-header      ((t (:inherit magit-diff-file-header))))
                                        ;`(magit-blame-heading     ((t (:overline ,orcad-pink :foreground ,orcad-red   :background ,orcad-white))))
                                        ;`(magit-blame-hash        ((t (:overline ,orcad-pink :foreground ,orcad-red   :background ,orcad-white))))
                                        ;`(magit-blame-name        ((t (:overline ,orcad-pink :foreground ,orcad-green :background ,orcad-white))))
                                        ;`(magit-blame-date        ((t (:overline ,orcad-pink :foreground ,orcad-blue-green :background ,orcad-white))))
                                        ;`(magit-blame-summary     ((t (:overline ,orcad-pink :foreground ,orcad-black  :background ,orcad-white :weight bold))))
                                        ;`(magit-branch            ((t ,vc-branch)))
                                        ;`(magit-diff-file-header  ((t (:height 1.1 :weight bold :foreground "#4183c4"))))
                                        ;`(magit-diff-hunk-header  ((t ,diff-hunk-header)))
                                        ;`(magit-diff-none         ((t ,diff-none)))
                                        ;`(magit-header            ((t (:foreground ,orcad-white :background ,orcad-red))))
                                        ;`(magit-item-highlight    ((t (:background "#eaf2f5"))))
                                        ;`(magit-item-mark         ((t ,marked-line)))
                                        ;`(magit-log-head-label    ((t (:box (:line-width 1 :color "blue" :style nil)))))
                                        ;`(magit-log-tag-label     ((t (:box (:line-width 1 :color "#00cc00" :style nil)))))
                                        ;`(magit-section-title     ((t (:family "Sans Serif" :height 1.8 :weight bold :foreground "cornflower blue" :inherit nil))))
                                        ;`(magit-header-line   ((t (:foreground ,orcad-red :background "#e6e6e6"))))
                                        ;`(magit-section-highlight ((t (:weight bold :foreground ,orcad-blue ))))
     ;;`(magit-section-highlight ((t (:box (:line-width 1 :color "#00cc00" :style nil)))))
     `(magit-section-heading ((t (:weight bold :foreground ,orcad-red))))
     `(magit-section-highlight ((t (:background ,orcad-green-blue))))
     `(magit-branch-remote ((t (:foreground ,orcad-green :weight bold))))
     `(magit-branch-local  ((t (:foreground ,orcad-blue-green :weight bold))))
     ;;`(magit-diff-added        ((t ,diff-added)))
                                        ;`(magit-diff-added-highlight ((t (:foreground ,orcad-green))))
     ;;`(magit-diff-deleted      ((t ,diff-removed)))

     `(makefile-space-face ((t (:background "hot pink"))))
     `(makefile-targets ((t (:weight bold :foreground "blue"))))

     ;; Markdown
     ;;`(markdown-blockquote-face       ((t ())))
     `(markdown-bold-face             ((t (:inherit bold))))
     ;;`(markdown-comment-face          ((t ())))
     ;;`(markdown-footnote-face         ((t ())))
     ;;`(markdown-header-delimiter-face ((t ())))
     ;;`(markdown-header-face           ((t ())))
     `(markdown-header-face-1         ((t ,ol1)))
     `(markdown-header-face-2         ((t ,ol2)))
     `(markdown-header-face-3         ((t ,ol3)))
     `(markdown-header-face-4         ((t ,ol4)))
     `(markdown-header-face-5         ((t ,ol5)))
     `(markdown-header-face-6         ((t ,ol6)))
     ;;`(markdown-header-rule-face      ((t ())))
     `(markdown-inline-code-face      ((t ,code-inline)))
     `(markdown-italic-face           ((t (:inherit italic))))
     `(markdown-language-keyword-face ((t (:inherit org-block-begin-line))))
     ;;`(markdown-line-break-face       ((t ())))
     `(markdown-link-face             ((t ,link-no-underline)))
     ;;`(markdown-link-title-face       ((t ())))
     ;;`(markdown-list-face             ((t ())))
     ;;`(markdown-math-face             ((t ())))
     ;;`(markdown-metadata-key-face     ((t ())))
     ;;`(markdown-metadata-value-face   ((t ())))
     ;;`(markdown-missing-link-face     ((t ())))
     `(markdown-pre-face              ((t (:inherit org-block-background))))
     ;;`(markdown-reference-face        ((t ())))
     ;;`(markdown-strike-through-face   ((t ())))
     `(markdown-url-face              ((t ,link)))


     `(match ((t ,match)))           ; Used for grep matches.
     `(mc/cursor-bar-face ((t (:height 1.0 :foreground "#1664c4" :background "#1664c4"))))
     `(mc/cursor-face ((t (:inverse-video t))))
     `(mc/region-face ((t (:inherit region))))

     `(mm-uu-extract ((t ,code-block)))
     `(moccur-current-line-face ((t (:foreground ,orcad-black :background "#ffffcc"))))
     `(moccur-face ((t (:foreground ,orcad-black :background "#ffff99"))))
     `(next-error ((t ,volatile-highlight-supersize)))
     `(nobreak-space ((t (:background "#cce8f6"))))

     ;; NXML
     `(nxml-attribute-local-name-face          ((t ,xml-attribute)))
     `(nxml-attribute-value-delimiter-face     ((t (:foreground "green4"))))
     `(nxml-attribute-value-face               ((t (:foreground "green4"))))
     `(nxml-comment-content-face               ((t (:slant italic :foreground "red"))))
     `(nxml-comment-delimiter-face             ((t (:foreground "red"))))
     `(nxml-element-local-name                 ((t ,xml-tag)))
     `(nxml-element-local-name-face            ((t (:foreground "blue"))))
     `(nxml-processing-instruction-target-face ((t (:foreground "purple1"))))
     `(nxml-tag-delimiter-face                 ((t (:foreground "blue"))))
     `(nxml-tag-slash-face                     ((t (:foreground "blue"))))

     ;; Org
     `(org-agenda-block-count      ((t (:weight bold :foreground "#a5a5a5"))))
     `(org-agenda-calendar-event   ((t (:weight bold :foreground "#3774cc" :background "#e4ebfe"))))
     `(org-agenda-calendar-sexp    ((t (:foreground "#327acd" :background "#f3f7fc"))))
     `(org-agenda-clocking         ((t (:foreground ,orcad-black :background "#eec900"))))
     `(org-agenda-column-dateline  ((t ,column)))
     `(org-agenda-current-time     ((t (:underline t :foreground "#1662af"))))
     `(org-agenda-date             ((t (,@(orcad-scale-font orcad-scale-org-agenda-structure 1.6) :weight bold :foreground "#1662af"))))
     `(org-agenda-date-today       ((t (,@(orcad-scale-font orcad-scale-org-agenda-structure 1.6) :weight bold :foreground "#4f4a3d" :background "#ffffcc"))))
     `(org-agenda-date-weekend     ((t (,@(orcad-scale-font orcad-scale-org-agenda-structure 1.6) :weight bold :foreground "#4e4e4e"))))
     `(org-agenda-diary            ((t (:weight bold :foreground "green4" :background "light blue"))))
     `(org-agenda-dimmed-todo-face ((t (:foreground "gold2"))))
     `(org-agenda-done             ((t (:foreground "#555555"))))
     `(org-agenda-filter-category  ((t (:weight bold :foreground "orange"))))
     `(org-agenda-filter-effort    ((t (:weight bold :foreground "orange"))))
     `(org-agenda-filter-regexp    ((t (:weight bold :foreground "orange"))))
     `(org-agenda-filter-tags      ((t (:weight bold :foreground "orange"))))
     `(org-agenda-restriction-lock ((t (:background "#e77d63"))))
     `(org-agenda-structure        ((t (,@(orcad-scale-font orcad-scale-org-agenda-structure 1.6) :weight bold :foreground ,orcad-blue))))
     `(org-archived                ((t (:foreground "gray70"))))
     `(org-beamer-tag              ((t (:box (:line-width 1 :color "#fabc18") :foreground "#2c2c2c" :background "#fff8d0"))))
     `(org-block                   ((t ,code-block)))
     `(org-block-background        ((t (:background "#fafafa" :extend t)))) ;; :inherit fixed-pitch))))
     `(org-block-begin-line        ((t (:height 0.7 :foreground "#555555" :background "#efefef" :extend t))))
     `(org-block-end-line          ((t (:height 0.7 :foreground "#555555" :background "#efefef" :extend t))))
     `(org-checkbox                ((t (:weight bold :box (:line-width 1 :style pressed-button) :foreground "#123555" :background ,orcad-white))))
     `(org-headline-done           ((t (:height 1.0 :weight normal :strike-through t :foreground ,orcad-green))))
     `(org-clock-overlay           ((t (:foreground ,orcad-white :background "SkyBlue"))))
     `(org-code                    ((t ,code-inline)))
     `(org-column                  ((t ,column)))
     `(org-column-title            ((t ,column)))
     `(org-date                    ((t (:underline t :foreground "#00459e"))))
     `(org-default                 ((t (:foreground ,orcad-black :background "#ffffff"))))
     `(org-dim                     ((t (:foreground "#aaaaaa"))))
     `(org-document-info           ((t (:foreground ,orcad-black))))
     `(org-document-info-keyword   ((t (:foreground ,orcad-black))))
     `(org-document-title          ((t (:foreground ,orcad-black))))
     `(org-done                    ((t (:weight bold :box (:line-width 1 :color "#bbbbbb") :foreground "#bbbbbb" :background "#f0f0f0"))))
     `(org-drawer                  ((t (:foreground "light sky blue"))))
     `(org-ellipsis                ((t (:underline nil :foreground "#999999")))) ; #FFEE62
     `(org-example                 ((t (:foreground "blue" :background "#eaffea"))))
     `(org-footnote                ((t (:underline t :foreground ,orcad-blue))))
     `(org-formula                 ((t (:foreground "chocolate1"))))
     `(org-hide                    ((t (:foreground "#e2e2e2"))))
     `(org-inlinetask              ((t (:box (:line-width 1 :color "#ebebeb") :foreground "#777777" :background "#ffffd6"))))
     `(org-latex-and-related       ((t (:foreground ,orcad-blue :background ,orcad-white))))
     `(org-level-1                 ((t ,ol1)))
     `(org-level-2                 ((t ,ol2)))
     `(org-level-3                 ((t ,ol3)))
     `(org-level-4                 ((t ,ol4)))
     `(org-level-5                 ((t ,ol5)))
     `(org-level-6                 ((t ,ol6)))
     `(org-level-7                 ((t ,ol7)))
     `(org-level-8                 ((t ,ol8)))
     `(org-link                    ((t ,link)))
     `(org-list-dt                 ((t (:weight bold :foreground "#335ea8"))))
     `(org-macro                   ((t (:weight bold :foreground "#edb802"))))
     `(org-meta-line               ((t (:slant italic :foreground "#aaaaaa"))))
     `(org-mode-line-clock         ((t (:box (:line-width 1 :color "#335ea8") :foreground ,orcad-black :background "#ffa335"))))
     `(org-mode-line-clock-overrun ((t (:weight bold :box (:line-width 1 :color "#335ea8") :foreground ,orcad-white :background "#ff4040"))))
     `(org-number-of-items         ((t (:weight bold :foreground ,orcad-white :background "#79ba79"))))
     `(org-property-value          ((t (:foreground "#00a000"))))
     `(org-quote                   ((t (:slant italic :extend t :foreground ,orcad-black :background "#fefefe"))))
     `(org-scheduled               ((t (:foreground ,orcad-black))))
     `(org-scheduled-previously    ((t (:foreground "#1466c6"))))
     `(org-scheduled-today         ((t (:weight bold :foreground "#4f4a3d" :background "#ffffcc"))))
     `(org-sexp-date               ((t (:foreground "#3774cc"))))
     `(org-special-keyword         ((t (:weight bold :foreground "#00bb00" :background "#eaffea"))))
     `(org-table                   ((t (:foreground "dark green" :background "#eaffea")))) ;; :inherit fixed-pitch))))
     `(org-tag                     ((t (:weight normal :slant italic :foreground "#9a9fa4" :background ,orcad-white))))
     `(org-target                  ((t (:foreground "#ff6daf"))))
     `(org-time-grid               ((t (:foreground "#cfcfcf"))))
     `(org-todo                    ((t (:weight bold :box (:line-width 1 :color "#d8aba7") :foreground "#d8aba7" :background "#ffe6e4"))))
     `(org-upcoming-deadline       ((t (:foreground "#ff5555"))))
     `(org-verbatim                ((t ,code-inline)))
     `(org-verse                   ((t (:slant italic :foreground "dim gray" :background "#eeeeee"))))
     `(org-warning                 ((t (:weight bold :foreground ,orcad-black :background "#cce7ff"))))

     ;; Outline
     `(outline-1 ((t ,ol1)))
     `(outline-2 ((t ,ol2)))
     `(outline-3 ((t ,ol3)))
     `(outline-4 ((t ,ol4)))
     `(outline-5 ((t ,ol5)))
     `(outline-6 ((t ,ol6)))
     `(outline-7 ((t ,ol7)))
     `(outline-8 ((t ,ol8)))

     ;;
     `(pabbrev-debug-display-label-face ((t (:foreground ,orcad-white :background "#a62154"))))
     `(pabbrev-suggestions-face ((t (:weight bold :foreground ,orcad-white :background "red"))))
     `(pabbrev-suggestions-label-face ((t (:weight bold :foreground ,orcad-white :background "purple"))))

     `(paren-face-match ((t ,paren-matched)))
     `(paren-face-mismatch ((t ,paren-unmatched)))
     `(paren-face-no-match ((t ,paren-unmatched)))

     `(persp-selected-face ((t (:weight bold :foreground "#eef5fe"))))

     `(powerline-active1 ((t (:foreground "#85ceeb" :background "#383838" :inherit mode-line))))
     `(powerline-active2 ((t (:foreground "#85ceeb" :background "#4070b6" :inherit mode-line))))
     `(powerline-inactive1 ((t (:foreground "#f0f0ef" :background "#686868" :inherit mode-line-inactive))))
     `(powerline-inactive2 ((t (:foreground "#f0f0ef" :background "#a9a9a9" :inherit mode-line-inactive))))

     ;; Rainbow delimiters
     `(rainbow-delimiters-depth-1-face    ((t (:foreground ,orcad-red    :weight bold   ))))
     `(rainbow-delimiters-depth-2-face    ((t (:foreground "deep pink"     ))))
     `(rainbow-delimiters-depth-3-face    ((t (:foreground "#036a07"       ))))
     `(rainbow-delimiters-depth-4-face    ((t (:foreground "deep sky blue" ))))
     `(rainbow-delimiters-depth-4-face    ((t (:foreground ,orcad-red        ))))
     `(rainbow-delimiters-depth-5-face    ((t (:foreground "orchid"        ))))
     `(rainbow-delimiters-depth-6-face    ((t (:foreground "steel blue"    ))))
     `(rainbow-delimiters-depth-7-face    ((t (:foreground ,orcad-red       ))))
     `(rainbow-delimiters-depth-8-face    ((t (:foreground "#cc3333"       ))))
     `(rainbow-delimiters-depth-9-face    ((t (:foreground "#edb802"       ))))
     `(rainbow-delimiters-mismatched-face ((t ,paren-unmatched)))
     `(rainbow-delimiters-unmatched-face  ((t ,paren-unmatched)))

     ;;
     `(recover-this-file ((t (:weight bold :background "#ff3f3f"))))
     `(rng-error ((t (:weight bold :foreground "red" :background "#fbe3e4"))))
     `(sh-heredoc ((t (:foreground "blue" :background "#eef5fe"))))
     `(sh-quoted-exec ((t (:foreground "#ff1493"))))
     `(shadow ((t ,shadow)))         ; Used for grep context lines.
     `(shell-option-face ((t (:foreground "forest green"))))
     `(shell-output-2-face ((t (:foreground "blue"))))
     `(shell-output-3-face ((t (:foreground "purple"))))
     `(shell-output-face ((t (:foreground ,orcad-black))))
     ;; `(shell-prompt-face ((t (:weight bold :foreground "yellow"))))
     `(shm-current-face ((t (:background "#eee8d5"))))
     `(shm-quarantine-face ((t (:background "lemonchiffon"))))
     `(show-paren-match ((t ,paren-matched)))
     `(show-paren-mismatch ((t ,paren-unmatched)))
     `(sml-modeline-end-face ((t (:background "#6badf6")))) ; #335EA8
     `(sml-modeline-vis-face ((t (:background "#1979ca"))))
     `(term ((t (:foreground ,orcad-black :background "#ffffff"))))

     ;; `(sp-pair-overlay-face ((t ())))
     ;; `(sp-show-pair-enclosing ((t ())))
     ;; `(sp-show-pair-match-face ((t ()))) ; ~ Pair highlighting (matching tags).
     ;; `(sp-show-pair-mismatch-face ((t ())))
     ;; `(sp-wrap-overlay-closing-pair ((t ())))
     ;; `(sp-wrap-overlay-face ((t ())))
     ;; `(sp-wrap-overlay-opening-pair ((t ())))
     ;; `(sp-wrap-tag-overlay-face ((t ())))

     `(speedbar-button-face ((t (:foreground "green4"))))
     `(speedbar-directory-face ((t (:foreground "blue"))))
     `(speedbar-file-face ((t (:foreground "cyan4"))))
     `(speedbar-highlight-face ((t ,volatile-highlight)))
     `(speedbar-selected-face ((t (:underline t :foreground "red"))))
     `(speedbar-tag-face ((t (:foreground "brown"))))
     `(svn-status-directory-face ((t ,directory)))
     `(svn-status-filename-face ((t (:weight bold :foreground "#4183c4"))))
     `(svn-status-locked-face ((t (:weight bold :foreground "red"))))
     `(svn-status-marked-face ((t ,marked-line)))
     `(svn-status-marked-popup-face ((t (:weight bold :foreground "green3"))))
     `(svn-status-switched-face ((t (:slant italic :foreground "gray55"))))
     `(svn-status-symlink-face ((t ,symlink)))
     `(svn-status-update-available-face ((t (:foreground "orange"))))
     `(tex-verbatim ((t (:foreground "blue"))))
     `(tool-bar ((t (:box (:line-width 1 :style released-button) :foreground ,orcad-black :background "gray75"))))
     `(tooltip ((t (:foreground ,orcad-black :background "light yellow"))))
     `(traverse-match-face ((t (:weight bold :foreground "blue violet"))))

     ;;
     `(vc-annotate-face-3F3FFF ((t (:foreground "#3f3fff" :background ,orcad-black))))
     `(vc-annotate-face-3F6CFF ((t (:foreground "#3f3fff" :background ,orcad-black))))
     `(vc-annotate-face-3F99FF ((t (:foreground "#3f99ff" :background ,orcad-black))))
     `(vc-annotate-face-3FC6FF ((t (:foreground "#3f99ff" :background ,orcad-black))))
     `(vc-annotate-face-3FF3FF ((t (:foreground "#3ff3ff" :background ,orcad-black))))
     `(vc-annotate-face-3FFF56 ((t (:foreground "#4bff4b" :background ,orcad-black))))
     `(vc-annotate-face-3FFF83 ((t (:foreground "#3fffb0" :background ,orcad-black))))
     `(vc-annotate-face-3FFFB0 ((t (:foreground "#3fffb0" :background ,orcad-black))))
     `(vc-annotate-face-3FFFDD ((t (:foreground "#3ff3ff" :background ,orcad-black))))
     `(vc-annotate-face-56FF3F ((t (:foreground "#4bff4b" :background ,orcad-black))))
     `(vc-annotate-face-83FF3F ((t (:foreground "#b0ff3f" :background ,orcad-black))))
     `(vc-annotate-face-B0FF3F ((t (:foreground "#b0ff3f" :background ,orcad-black))))
     `(vc-annotate-face-DDFF3F ((t (:foreground "#fff33f" :background ,orcad-black))))
     `(vc-annotate-face-F6FFCC ((t (:foreground ,orcad-black :background "#ffffc0"))))
     `(vc-annotate-face-FF3F3F ((t (:foreground "#ff3f3f" :background ,orcad-black))))
     `(vc-annotate-face-FF6C3F ((t (:foreground "#ff3f3f" :background ,orcad-black))))
     `(vc-annotate-face-FF993F ((t (:foreground "#ff993f" :background ,orcad-black))))
     `(vc-annotate-face-FFC63F ((t (:foreground "#ff993f" :background ,orcad-black))))
     `(vc-annotate-face-FFF33F ((t (:foreground "#fff33f" :background ,orcad-black))))

     ;; ;; vc
     ;; (vc-up-to-date-state    ((,c :foreground ,(gc 'green-1))))
     ;; (vc-edited-state        ((,c :foreground ,(gc 'yellow+1))))
     ;; (vc-missing-state       ((,c :foreground ,(gc 'red))))
     ;; (vc-conflict-state      ((,c :foreground ,(gc 'red+2) :weight bold)))
     ;; (vc-locked-state        ((,c :foreground ,(gc 'cyan-1))))
     ;; (vc-locally-added-state ((,c :foreground ,(gc 'blue))))
     ;; (vc-needs-update-state  ((,c :foreground ,(gc 'magenta))))
     ;; (vc-removed-state       ((,c :foreground ,(gc 'red-1))))

     `(vhl/default-face ((t ,volatile-highlight))) ; `volatile-highlights.el' (for undo, yank).

     ;;
     `(w3m-anchor                            ((t ,link)))
     `(w3m-arrived-anchor                    ((t (:foreground "purple1"))))
     `(w3m-bitmap-image-face                 ((t (:foreground "gray4" :background "green"))))
     `(w3m-bold                              ((t (:weight bold :foreground ,orcad-black))))
     `(w3m-current-anchor                    ((t (:weight bold :underline t :foreground "blue"))))
     `(w3m-form                              ((t (:underline t :foreground "tan1"))))
     `(w3m-form-button-face                  ((t (:weight bold :underline t :foreground "gray4" :background "light grey"))))
     `(w3m-form-button-mouse-face            ((t (:underline t :foreground "light grey" :background "#2b7e2a"))))
     `(w3m-form-button-pressed-face          ((t (:weight bold :underline t :foreground "gray4" :background "light grey"))))
     `(w3m-header-line-location-content-face ((t (:foreground "#7f7f7f":background "#f7f7f7"))))
     `(w3m-header-line-location-title-face   ((t (:foreground "#2c55b1" :background "#f7f7f7"))))
     `(w3m-history-current-url-face          ((t (:foreground "lemon chiffon"))))
     `(w3m-image-face                        ((t (:weight bold :foreground "DarkSeaGreen2"))))
     `(w3m-link-numbering                    ((t (:foreground "#b4c7eb")))) ; mouseless browsing
     `(w3m-strike-through-face               ((t (:strike-through t))))
     `(w3m-underline-face                    ((t (:underline t))))

     ;; Web mode
     ;;`(web-mode-block-attr-name-face           ((t ())))
     ;;`(web-mode-block-attr-value-face          ((t ())))
     ;;`(web-mode-block-comment-face             ((t ())))
     ;;`(web-mode-block-control-face             ((t ())))
     ;;`(web-mode-block-delimiter-face           ((t ())))
     ;;`(web-mode-block-face                     ((t ())))
     ;;`(web-mode-block-string-face              ((t ())))
     ;;`(web-mode-bold-face                      ((t ())))
     ;;`(web-mode-builtin-face                   ((t ())))
     ;;`(web-mode-comment-face                   ((t ())))
     ;;`(web-mode-comment-keyword-face           ((t ())))
     ;;`(web-mode-constant-face                  ((t ())))
     ;;`(web-mode-css-at-rule-face               ((t ())))
     ;;`(web-mode-css-color-face                 ((t ())))
     ;;`(web-mode-css-comment-face               ((t ())))
     ;;`(web-mode-css-function-face              ((t ())))
     ;;`(web-mode-css-priority-face              ((t ())))
     ;;`(web-mode-css-property-name-face         ((t ())))
     ;;`(web-mode-css-pseudo-class-face          ((t ())))
     ;;`(web-mode-css-selector-face              ((t ())))
     ;;`(web-mode-css-string-face                ((t ())))
     ;;`(web-mode-css-variable-face              ((t ())))
     ;;`(web-mode-current-column-highlight-face  ((t ())))
     `(web-mode-current-element-highlight-face ((t (:background "#99ccff")))) ; #FFEE80
     ;;`(web-mode-doctype-face                   ((t ())))
     ;;`(web-mode-error-face                     ((t ())))
     ;;`(web-mode-filter-face                    ((t ())))
     `(web-mode-folded-face                    ((t (:box (:line-width 1 :color "#777777") :foreground "#9a9a6a" :background "#f3f349"))))
     ;;`(web-mode-function-call-face             ((t ())))
     ;;`(web-mode-function-name-face             ((t ())))
     ;;`(web-mode-html-attr-custom-face          ((t ())))
     ;;`(web-mode-html-attr-engine-face          ((t ())))
     ;;`(web-mode-html-attr-equal-face           ((t ())))
     `(web-mode-html-attr-name-face            ((t ,xml-attribute)))
     ;;`(web-mode-html-attr-value-face           ((t ())))
     ;;`(web-mode-html-entity-face               ((t ())))
     `(web-mode-html-tag-bracket-face          ((t ,xml-tag)))
     ;;`(web-mode-html-tag-custom-face           ((t ())))
     `(web-mode-html-tag-face                  ((t ,xml-tag)))
     ;;`(web-mode-html-tag-namespaced-face       ((t ())))
     ;;`(web-mode-inlay-face                     ((t ())))
     ;;`(web-mode-italic-face                    ((t ())))
     ;;`(web-mode-javascript-comment-face        ((t ())))
     ;;`(web-mode-javascript-string-face         ((t ())))
     ;;`(web-mode-json-comment-face              ((t ())))
     ;;`(web-mode-json-context-face              ((t ())))
     ;;`(web-mode-json-key-face                  ((t ())))
     ;;`(web-mode-json-string-face               ((t ())))
     ;;`(web-mode-jsx-depth-1-face               ((t ())))
     ;;`(web-mode-jsx-depth-2-face               ((t ())))
     ;;`(web-mode-jsx-depth-3-face               ((t ())))
     ;;`(web-mode-jsx-depth-4-face               ((t ())))
     ;;`(web-mode-keyword-face                   ((t ())))
     ;;`(web-mode-param-name-face                ((t ())))
     ;;`(web-mode-part-comment-face              ((t ())))
     `(web-mode-part-face                      ((t (:background "#ffffe0"))))
     ;;`(web-mode-part-string-face               ((t ())))
     ;;`(web-mode-preprocessor-face              ((t ())))
     `(web-mode-script-face                    ((t (:background "#eff0f1"))))
     ;;`(web-mode-sql-keyword-face               ((t ())))
     ;;`(web-mode-string-face                    ((t ())))
     ;;`(web-mode-style-face                     ((t ())))
     ;;`(web-mode-symbol-face                    ((t ())))
     ;;`(web-mode-type-face                      ((t ())))
     ;;`(web-mode-underline-face                 ((t ())))
     ;;`(web-mode-variable-name-face             ((t ())))
     ;;`(web-mode-warning-face                   ((t ())))
     ;;`(web-mode-whitespace-face                ((t ())))

     `(which-func ((t (:weight bold :slant italic :foreground ,orcad-white))))
     ;; `(which-key-command-description-face)
     ;; `(which-key-group-description-face)
     ;; `(which-key-highlighted-command-face)
     ;; `(which-key-key-face)
     `(which-key-local-map-description-face ((t (:weight bold :background "#f3f7fc" :inherit which-key-command-description-face))))
     ;; `(which-key-note-face)
     ;; `(which-key-separator-face)
     ;; `(which-key-special-key-face)
     `(widget-button ((t ,link)))
     `(widget-button-pressed ((t (:foreground "red"))))
     `(widget-documentation ((t (:foreground "green4"))))
     `(widget-field ((t (:background "gray85"))))
     `(widget-inactive ((t (:foreground "dim gray"))))
     `(widget-single-line-field ((t (:background "gray85"))))
     `(woman-bold ((t (:weight bold :foreground "#f13d3d"))))
     `(woman-italic ((t (:weight bold :slant italic :foreground "#46be1b"))))
     `(woman-symbol ((t (:weight bold :foreground "purple"))))
     `(yas-field-debug-face ((t (:foreground ,orcad-white :background "#a62154"))))
     `(yas-field-highlight-face ((t (:box (:line-width 1 :color "#838383") :foreground ,orcad-black :background "#d4dcd8"))))

     ;; `(ztreep-arrow-face ((t ())))
     ;; `(ztreep-diff-header-face ((t ())))
     ;; `(ztreep-diff-header-small-face ((t ())))
     `(ztreep-diff-model-add-face ((t (:weight bold :foreground "#008800"))))
     `(ztreep-diff-model-diff-face ((t (:weight bold :foreground "#0044dd"))))
     `(ztreep-diff-model-ignored-face ((t (:strike-through t :foreground "#9e9e9e"))))
     `(ztreep-diff-model-normal-face ((t (:foreground ,orcad-black))))
     ;; `(ztreep-expand-sign-face ((t ())))
     ;; `(ztreep-header-face ((t ())))
     ;; `(ztreep-leaf-face ((t ())))
     ;; `(ztreep-node-face ((t ())))

     )))

(custom-theme-set-variables 'orcad

                            ;; highlight-sexp-mode.
                            '(hl-sexp-background-color "#efebe9")

                            '(ansi-color-faces-vector
                              [default default default italic underline success warning error])

                            ;; Colors used in Shell mode.
                            '(ansi-color-names-vector
                              ["black" "red" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
                            )

;;;###autoload
(when (and (boundp 'custom-theme-load-path)
           load-file-name)
  ;; Add theme folder to `custom-theme-load-path' when installing over MELPA.
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'orcad)

;; This is for the sake of Emacs.
;; Local Variables:
;; no-byte-compile: t
;; time-stamp-end: "$"
;; time-stamp-format: "%:y%02m%02d.%02H%02M"
;; time-stamp-start: "Version: "
;; End:
