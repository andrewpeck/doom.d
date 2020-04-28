;;; leuven-theme.el --- Awesome Emacs color theme on white background

;; Copyright (C) 2003-2020 Free Software Foundation, Inc.

;; Author: Fabrice Niessen <(concat "fniessen" at-sign "pirilampo.org")>
;; URL: https://github.com/fniessen/emacs-leuven-theme
;; Version: 20200102.2050
;; Keywords: color theme

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This elegant Org-enhancing color theme "leuven" ROCKS!
;; ... and not just for Org mode.
;;
;; To use it, put the following in your Emacs configuration file:
;;
;;   (load-theme 'leuven t)
;;
;; Requirements: Emacs 24+.
;;
;; NOTE -- Would you like implement a version of this for dark backgrounds,
;; please do so!  I'm willing to integrate it...

;;; Code:

;;; Options.

(defgroup leuven-summerfruit nil
  "Leuven theme options.
The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom leuven-summerfruit-scale-outline-headlines t
  "Scale `outline' (and `org') level-1 headlines.
This can be nil for unscaled, t for using the theme default, or a scaling number."
  :type '(choice
          (const :tag "Unscaled" nil)
          (const :tag "Default provided by theme" t)
          (number :tag "Set scaling"))
  :group 'leuven-summerfruit)

(defcustom leuven-summerfruit-scale-org-agenda-structure t
  "Scale Org agenda structure lines, like dates.
This can be nil for unscaled, t for using the theme default, or a scaling number."
  :type '(choice
          (const :tag "Unscaled" nil)
          (const :tag "Default provided by theme" t)
          (number :tag "Set scaling"))
  :group 'leuven-summerfruit)

(defun leuven-summerfruit-scale-font (control default-height)
  "Function for splicing optional font heights into face descriptions.
CONTROL can be a number, nil, or t.  When t, use DEFAULT-HEIGHT."
  (cond
   ((numberp control) (list :height control))
   ((eq t control) (list :height default-height))
   (t nil)))

;;; Theme Faces.

(deftheme leuven-summerfruit
  "Face colors with a light background.
   Basic, Font Lock, Isearch, Gnus, Message, Org mode, Diff, Ediff,
   Flyspell, Semantic, and Ansi-Color faces are included -- and much
   more...")

  ;; define colors
  (let (
        (sf-black          '"#333333")
        (sf-gray           '"#6b6b6b")
        (sf-lightgray      '"#bbbbbb")
        (sf-white          '"#ffffff")
        (sf-offwhite       '"#e9e9e9")
        (sf-lightyellow    '"#eeffdd")
        (sf-lightgreen     '"#ddffee")
        (sf-lightlightgray '"#dddddd")
        (sf-lightred       '"#ffdddd")
        (sf-lightblue      '"#bad6ff")
        (sf-red            '"#ff0000")
        (sf-red2           '"#e5786d")
        (sf-orange         '"#fb660a")
        (sf-pink           '"#ff0088")
        (sf-green          '"#22a21f")
        (sf-blue1          '"#0077cc")
        (sf-blue2          '"#006daf")
        (sf-blue3          '"#3366a9")
        (sf-blue4          '"#008ed1")
        (sf-blue5          '"#aad5ff")
        (sf-blue6          '"#438ec3")
        (sf-yellow         '"#ffffab")
        )

    ;; create classes
    (let ((class '((class color) (min-colors 89)))

          ;; Leuven-Summerfruit generic colors.
          (cancel                        `(:foreground ,sf-gray :slant italic :strike-through t))
          (clock-line                    `(:box (:line-width 1 :color ,sf-blue3) :foreground ,sf-black :background "#eec900"))

          ;; code block
          (code-block                    `(:foreground ,sf-black :background ,sf-offwhite :extend t))
          (code-inline                   `(:foreground ,sf-green :background ,sf-offwhite :extend t))

          ;;
          (column                        `(:height 1.0 :weight normal :slant normal :underline nil :strike-through nil :foreground "#e6ad4f" :background "#fff2de"))

          ;; Completion
          (completion-inline             `(:foreground ,sf-gray  :weight normal :inherit hl-line)) ; Like Google.
          (completion-selected-candidate `(:foreground ,sf-white :weight bold   :background ,sf-blue1))
          (completion-other-candidates   `(:foreground ,sf-blue1 :weight bold   :background ,sf-lightblue))

          ;; diff
          (diff-added                    `(:background "#ddffdd"))
          (diff-changed                  `(:foreground "#0000ff" :background "#ddddff"))
          (diff-header                   `(:weight bold :foreground "#800000" :background "#ffffaf"))
          (diff-hunk-header              `(:foreground "#990099" :background "#ffeeff"))
          (diff-none                     `(:foreground "#888888"))
          (diff-refine-added             `(:background "#97f295"))
          (diff-refine-removed           `(:background "#ffb6ba"))
          (diff-removed                  `(:background "#fee8e9"))

          (directory                     `(:weight bold :foreground ,sf-blue1 :background ,sf-white))
          (file                          `(:foreground ,sf-black))
          (function-param                `(:foreground "#247284"))
          (grep-file-name                `(:weight bold :foreground "#2a489e")) ; Used for grep hits.
          (grep-line-number              `(:weight bold :foreground "#a535ae"))

          ;;
          (highlight-blue                `(:background "#e6ecff" :extend t))
          (highlight-blue2               `(:background "#e4f1f9" :extend t))
          (highlight-gray                `(:background "#e4e4e3" :extend t))
          (highlight-green               `(:background "#d5f1cf" :extend t))
          (highlight-red                 `(:background "#ffc8c8" :extend t))
          (highlight-yellow              `(:background "#f6fecd" :extend t))

          ;; hyperlinks
          (link                          `(:weight normal :underline t :foreground ,sf-blue2))
          (link-no-underline             `(:weight normal              :foreground ,sf-blue2))

          ;; mail
          (mail-header-name              `(:family "Sans Serif" :weight normal :foreground "#a3a3a2"))
          (mail-header-other             `(:family "Sans Serif" :slant normal :foreground "#666666"))
          (mail-read                     `(:foreground "#8c8c8c"))
          (mail-read-high                `(:foreground "#808080"))
          (mail-ticked                   `(:foreground ,sf-red))
          (mail-to                       `(:family "Sans Serif" :underline nil :foreground ,sf-blue3))
          (mail-unread                   `(:weight bold :foreground ,sf-black))
          (mail-unread-high              `(:weight bold :foreground "#135985"))

          (marked-line                   `(:foreground "#aa0000" :background "#ffaaaa"))
          (match                         `(:weight bold :background "#ffff00")) ; occur patterns + match in helm for files + match in Org files.

          ;; Org mode levels
          (ol1 `(:height 1.3 :weight bold :slant normal :foreground ,sf-orange :extend t))
          (ol2 `(:height 1.2 :weight bold :slant normal :foreground ,sf-blue3 :extend t))
          (ol3 `(:height 1.1 :weight bold :slant normal :foreground ,sf-green :extend t))
          (ol4 `(:height 1.0 :weight bold :slant normal :foreground ,sf-orange :extend t))
          (ol5 `(:height 1.0 :weight bold :slant normal :foreground ,sf-pink  :extend t))
          (ol6 `(:height 1.0 :weight bold :slant italic :foreground ,sf-blue1 :extend t))
          (ol7 `(:height 1.0 :weight bold :slant italic :foreground ,sf-green :extend t))
          (ol8 `(:height 1.0 :weight bold :slant italic :foreground ,sf-orange :extend t))

          (paren-matched                `(:background "#c0e8c3")) ; Or take that green for region?
          (paren-unmatched              `(:weight bold :underline ,sf-red :foreground ,sf-black :background "#ffa5a5"))
          (region                       `(:background ,sf-blue5 :extend t))
          (shadow                       `(:foreground ,sf-gray))
          (string                       `(:foreground ,sf-blue1)) ; affects the color in the mode bar of the project as well as other strings
          (subject                      `(:foreground ,sf-black :family "Sans Serif" :weight bold ))
          (symlink                      `(:foreground ,sf-blue4))
          (tab                          `(:foreground "#e8e8e8" :background ,sf-white))
          (trailing                     `(:foreground "#e8e8e8" :background "#ffffab"))
          (volatile-highlight           `(:underline nil :foreground ,sf-white :background "#9e3699"))
          (volatile-highlight-supersize `(:height 1.1 :underline nil :foreground ,sf-white :background "#9e3699")) ; flash-region
          (vc-branch                    `(:box (:line-width 1 :color ,sf-red) :foreground ,sf-black :background "#aaffaa"))
          (xml-attribute                `(:foreground ,sf-orange))
          (xml-tag                      `(:foreground "#ae1b9a"))
          (highlight-current-tag        `(:background "#e8e8ff")) ; #EEF3F6 or #FFEB26
          )

      (custom-theme-set-faces
       'leuven-summerfruit

       ;; Git gutter fringe
       `(git-gutter-fr:modified           ((,class (:foreground ,sf-orange))))
       `(git-gutter-fr:added              ((,class (:foreground ,sf-green))))
       `(git-gutter-fr:deleted            ((,class (:foreground ,sf-red))))

       ;; Standard fonts
       `(default     ((,class (:foreground ,sf-black))))
       `(bold        ((,class (:foreground ,sf-black :weight bold))))
       `(bold-italic ((,class (:foreground ,sf-black :weight bold :slant italic))))
       `(italic      ((,class (:foreground ,sf-black :slant italic))))
       `(underline   ((,class (:underline t))))
       `(cursor      ((,class (:background "#21bdff"))))

       ;; Lucid toolkit emacs menus.
       `(menu ((,class (:foreground "#ffffff" :background ,sf-black))))

       ;; Highlighting faces.
       ;`(fringe                   ((,class (:foreground "#4c9ed9" :background ,sf-white)))) ;; does not seem to change anything??

       ; `(highlight-numbers-number ((,class (:foreground ,sf-red)))) ;; does not seem to change anything

       `(highlight                ((,class ,highlight-blue)))
       `(region                   ((,class ,region)))
       `(secondary-selection      ((,class ,match))) ; Used by Org-mode for highlighting matched entries and keywords.

       ; search
       `(isearch                  ((,class (:foreground ,sf-black :background ,sf-yellow :underline ,sf-black))))
       `(isearch-fail             ((,class (:foreground ,sf-black :background "#ffcccc" :weight bold))))
       `(lazy-highlight           ((,class (:foreground ,sf-black :background ,sf-yellow)))) ; Isearch others (see `match').

       ;
       `(trailing-whitespace      ((,class ,trailing)))
       `(query-replace            ((,class (:inherit isearch))))
       `(whitespace-hspace        ((,class (:foreground "#d2d2d2")))) ; see also `nobreak-space'
       `(whitespace-indentation   ((,class ,tab)))
       `(whitespace-line          ((,class (:foreground ,sf-red :background "#ffff88"))))
       `(whitespace-tab           ((,class ,tab)))
       `(whitespace-trailing      ((,class ,trailing)))

       ;; Mode line faces.
       `(mode-line           ((,class (:box (:line-width 1 :color "#1a2f54") :foreground ,sf-white :background ,sf-blue3))))
       `(mode-line-inactive  ((,class (:box (:line-width 1 :color "#4e4e4c") :foreground ,sf-white :background ,sf-lightgray))))
       `(mode-line-buffer-id ((,class (:weight bold :foreground ,sf-white))))
       `(mode-line-emphasis  ((,class (:weight bold :foreground ,sf-white))))
       `(mode-line-highlight ((,class (:weight bold :foreground ,sf-white))))

       ;; Escape and prompt faces.
       `(minibuffer-prompt            ((,class (:weight bold :foreground ,sf-black :background "gold"))))
       `(minibuffer-noticeable-prompt ((,class (:weight bold :foreground ,sf-black :background "gold"))))
       `(escape-glyph                 ((,class (             :foreground ,sf-blue4))))
       `(error                        ((,class (:weight bold :foreground ,sf-red))))
       `(warning                      ((,class (:weight bold :foreground ,sf-orange))))
       `(success                      ((,class (             :foreground ,sf-green))))

       ;; Font lock faces.
       `(font-lock-builtin-face              ((,class (:foreground ,sf-orange :bold t       ))))
       `(font-lock-comment-delimiter-face    ((,class (:foreground ,sf-green                ))))
       `(font-lock-comment-face              ((,class (:foreground ,sf-green :slant normal  ))))
       `(font-lock-constant-face             ((,class (:foreground ,sf-pink                 ))))
       `(font-lock-doc-face                  ((,class (:foreground ,sf-green                ))))
     ;;`(font-lock-doc-string-face           ((,class (:foreground ,sf-green                )))) ; XEmacs only, but is used for HTML exports from org2html (and not interactively)
       `(font-lock-function-name-face        ((,class (:foreground ,sf-pink                 ))))
       `(font-lock-keyword-face              ((,class (:foreground ,sf-orange :bold t       ))))
       `(font-lock-preprocessor-face         ((,class (:foreground ,sf-gray                 ))))
       `(font-lock-regexp-grouping-backslash ((,class (:weight bold :inherit nil            ))))
       `(font-lock-regexp-grouping-construct ((,class (:weight bold :inherit nil            ))))
       `(font-lock-string-face               ((,class ,string)))
       `(font-lock-type-face                 ((,class (:foreground ,sf-gray :weight bold    ))))
       `(font-lock-variable-name-face        ((,class (:foreground ,sf-gray :weight normal  ))))
       `(font-lock-warning-face              ((,class (:weight bold :foreground ,sf-red     ))))
       `(vhdl-font-lock-attribute-face       ((,class (:foreground ,sf-pink                 ))))
       `(vhdl-font-lock-function-face        ((,class (:foreground ,sf-gray :weight bold    ))))

       ;; Button and link faces.
       `(link         ((,class ,link)))
       `(link-visited ((,class (:underline t :foreground ,sf-red2))))
       `(button       ((,class (:underline t :foreground ,sf-blue2))))
       `(header-line  ((,class (:box (:line-width 1 :color ,sf-black) :foreground ,sf-black :background "#f0f0f0"))))

       ;; Gnus faces.
       `(gnus-button                   ((,class (:weight normal))))
       `(gnus-cite-attribution-face    ((,class (:foreground "#5050b0"))))
       `(gnus-cite-1                   ((,class (:foreground "#5050b0" :background "#f6f6f6"))))
       `(gnus-cite-2                   ((,class (:foreground "#660066" :background "#f6f6f6"))))
       `(gnus-cite-3                   ((,class (:foreground "#007777" :background "#f6f6f6"))))
       `(gnus-cite-4                   ((,class (:foreground "#990000" :background "#f6f6f6"))))
       `(gnus-cite-5                   ((,class (:foreground "#000099" :background "#f6f6f6"))))
       `(gnus-cite-6                   ((,class (:foreground "#bb6600" :background "#f6f6f6"))))
       `(gnus-cite-7                   ((,class (:foreground "#5050b0" :background "#f6f6f6"))))
       `(gnus-cite-8                   ((,class (:foreground "#660066" :background "#f6f6f6"))))
       `(gnus-cite-9                   ((,class (:foreground "#007777" :background "#f6f6f6"))))
       `(gnus-cite-10                  ((,class (:foreground "#990000" :background "#f6f6f6"))))
       `(gnus-emphasis-bold            ((,class (:weight bold))))
       `(gnus-emphasis-highlight-words ((,class (:foreground "yellow" :background ,sf-black))))
       `(gnus-group-mail-1             ((,class (:weight bold :foreground "#ff50b0"))))
       `(gnus-group-mail-1-empty       ((,class (:foreground "#5050b0"))))
       `(gnus-group-mail-2             ((,class (:weight bold :foreground "#ff0066"))))
       `(gnus-group-mail-2-empty       ((,class (:foreground "#660066"))))
       `(gnus-group-mail-3             ((,class ,mail-unread)))
       `(gnus-group-mail-3-empty       ((,class ,mail-read)))
       `(gnus-group-mail-low           ((,class ,cancel)))
       `(gnus-group-mail-low-empty     ((,class ,cancel)))
       `(gnus-group-news-1             ((,class (:weight bold :foreground "#ff50b0"))))
       `(gnus-group-news-1-empty       ((,class (:foreground "#5050b0"))))
       `(gnus-group-news-2             ((,class (:weight bold :foreground "#ff0066"))))
       `(gnus-group-news-2-empty       ((,class (:foreground "#660066"))))
       `(gnus-group-news-3             ((,class ,mail-unread)))
       `(gnus-group-news-3-empty       ((,class ,mail-read)))
       `(gnus-group-news-4             ((,class (:weight bold :foreground "#ff0000"))))
       `(gnus-group-news-4-empty       ((,class (:foreground "#990000"))))
       `(gnus-group-news-5             ((,class (:weight bold :foreground "#ff0099"))))
       `(gnus-group-news-5-empty       ((,class (:foreground "#000099"))))
       `(gnus-group-news-6             ((,class (:weight bold :foreground "gray50"))))
       `(gnus-group-news-6-empty       ((,class (:foreground "#808080"))))
       `(gnus-header-content           ((,class ,mail-header-other)))
       `(gnus-header-from              ((,class (:family "Sans Serif" :foreground ,sf-black))))
       `(gnus-header-name              ((,class ,mail-header-name)))
       `(gnus-header-newsgroups        ((,class (:family "Sans Serif" :foreground "#3399cc"))))
       `(gnus-header-subject           ((,class ,subject)))
       `(gnus-picon                    ((,class (:foreground "yellow" :background ,sf-white))))
       `(gnus-picon-xbm                ((,class (:foreground "yellow" :background ,sf-white))))
       `(gnus-server-closed            ((,class (:slant italic :foreground "blue" :background ,sf-white))))
       `(gnus-server-denied            ((,class (:weight bold :foreground sf-red :background ,sf-white))))
       `(gnus-server-opened            ((,class (:family "Sans Serif" :foreground ,sf-white :foreground "#466bd7"))))
       `(gnus-signature                ((,class (:slant italic :foreground "#8b8d8e"))))
       `(gnus-splash                   ((,class (:foreground "#ff8c00"))))
       `(gnus-summary-cancelled        ((,class ,cancel)))
       `(gnus-summary-high-ancient     ((,class ,mail-unread-high)))
       `(gnus-summary-high-read        ((,class ,mail-read-high)))
       `(gnus-summary-high-ticked      ((,class ,mail-ticked)))
       `(gnus-summary-high-unread      ((,class ,mail-unread-high)))
       `(gnus-summary-low-ancient      ((,class (:slant italic :foreground ,sf-black))))
       `(gnus-summary-low-read         ((,class (:slant italic :foreground "#999999" :background "#e0e0e0"))))
       `(gnus-summary-low-ticked       ((,class ,mail-ticked)))
       `(gnus-summary-low-unread       ((,class (:slant italic :foreground ,sf-black))))
       `(gnus-summary-normal-ancient   ((,class ,mail-read)))
       `(gnus-summary-normal-read      ((,class ,mail-read)))
       `(gnus-summary-normal-ticked    ((,class ,mail-ticked)))
       `(gnus-summary-normal-unread    ((,class ,mail-unread)))
       `(gnus-summary-selected         ((,class (:foreground ,sf-white :background "#008cd7"))))
       `(gnus-x-face                   ((,class (:foreground ,sf-black :background ,sf-white))))

       ;; Message faces.
       `(message-header-name       ((,class ,mail-header-name)))
       `(message-header-cc         ((,class ,mail-to)))
       `(message-header-other      ((,class ,mail-header-other)))
       `(message-header-subject    ((,class ,subject)))
       `(message-header-to         ((,class ,mail-to)))
       `(message-cited-text        ((,class (:foreground "#5050b0" :background "#f6f6f6"))))
       `(message-separator         ((,class (:family "Sans Serif" :weight normal :foreground "#bdc2c6"))))
       `(message-header-newsgroups ((,class (:family "Sans Serif" :foreground "#3399cc"))))
       `(message-header-xheader    ((,class ,mail-header-other)))
       `(message-mml               ((,class (:foreground "forest green"))))

       ;; Diff.
       `(diff-added             ((,class ,diff-added)))
       `(diff-changed           ((,class ,diff-changed)))
       `(diff-context           ((,class ,diff-none)))
       `(diff-file-header       ((,class ,diff-header)))
       `(diff-file1-hunk-header ((,class (:foreground "dark magenta" :background "#eaf2f5"))))
       `(diff-file2-hunk-header ((,class (:foreground "#2b7e2a" :background "#eaf2f5"))))
       `(diff-function          ((,class (:foreground "#cc99cc"))))
       `(diff-header            ((,class ,diff-header)))
       `(diff-hunk-header       ((,class ,diff-hunk-header)))
       `(diff-index             ((,class ,diff-header)))
       `(diff-indicator-added   ((,class (:foreground "#3a993a" :background "#cdffd8"))))
       `(diff-indicator-changed ((,class (:background "#dbedff"))))
       `(diff-indicator-removed ((,class (:foreground "#cc3333" :background "#ffdce0"))))
       `(diff-refine-added      ((,class ,diff-refine-added)))
       `(diff-refine-change     ((,class (:background "#ddddff"))))
       `(diff-refine-removed    ((,class ,diff-refine-removed)))
       `(diff-removed           ((,class ,diff-removed)))

       ;; SMerge.
       `(smerge-mine           ((,class ,diff-changed)))
       `(smerge-other          ((,class ,diff-added)))
       `(smerge-base           ((,class ,diff-removed)))
       `(smerge-markers        ((,class (:background "#ffe5cc"))))
       `(smerge-refined-change ((,class (:background "#aaaaff"))))

       ;; Ediff.
       `(ediff-current-diff-A ((,class (:background "#ffdddd" :extend t))))
       `(ediff-current-diff-B ((,class (:background "#ddffdd" :extend t))))
       `(ediff-current-diff-C ((,class (:background "cyan" :extend t))))
       `(ediff-even-diff-A    ((,class (:background "light grey" :extend t))))
       `(ediff-even-diff-B    ((,class (:background "light grey" :extend t))))
       `(ediff-fine-diff-A    ((,class (:background "#ffaaaa" :extend t))))
       `(ediff-fine-diff-B    ((,class (:background "#55ff55" :extend t))))
       `(ediff-odd-diff-A     ((,class (:background "light grey" :extend t))))
       `(ediff-odd-diff-B     ((,class (:background "light grey" :extend t))))

       ;; Flyspell.
       (if (version< emacs-version "24.4")
           `(flyspell-duplicate ((,class (:underline "#f4eb80" :inherit nil))))
         `(flyspell-duplicate ((,class (:underline (:style wave :color "#f4eb80") :background "#faf7cc" :inherit nil)))))
       (if (version< emacs-version "24.4")
           `(flyspell-incorrect ((,class (:underline "#faa7a5" :inherit nil))))
         `(flyspell-incorrect ((,class (:underline (:style wave :color "#faa7a5") :background "#f4d7da":inherit nil)))))

       ;; ;; Semantic faces.
       ;; `(semantic-decoration-on-includes ((,class (:underline ,cham-4))))
       ;; `(semantic-decoration-on-private-members-face ((,class (:background ,alum-2))))
       ;; `(semantic-decoration-on-protected-members-face ((,class (:background ,alum-2))))
       `(semantic-decoration-on-unknown-includes ((,class (:background "#fff8f8"))))
       ;; `(semantic-decoration-on-unparsed-includes ((,class (:underline ,orange-3))))
       `(semantic-highlight-func-current-tag-face ((,class ,highlight-current-tag)))
       `(semantic-tag-boundary-face ((,class (:overline "#777777")))) ; Method separator.
       ;; `(semantic-unmatched-syntax-face ((,class (:underline ,red-1))))

       `(Info-title-1-face               ((,class ,ol1)))
       `(Info-title-2-face               ((,class ,ol2)))
       `(Info-title-3-face               ((,class ,ol3)))
       `(Info-title-4-face               ((,class ,ol4)))
       `(ace-jump-face-foreground        ((,class (:weight bold :foreground ,sf-black :background "#fea500"))))
       `(ahs-face                        ((,class (:background "#e4e4ff"))))
       `(ahs-definition-face             ((,class (:background "#ffb6c6"))))
       `(ahs-plugin-defalt-face          ((,class (:background "#ffe4ff")))) ; Current.
       `(anzu-match-1                    ((,class (:foreground ,sf-black :background "aquamarine"))))
       `(anzu-match-2                    ((,class (:foreground ,sf-black :background "springgreen"))))
       `(anzu-match-3                    ((,class (:foreground ,sf-black :background "red"))))
       `(anzu-mode-line                  ((,class (:foreground ,sf-black :background "#80ff80"))))
       `(anzu-mode-line-no-match         ((,class (:foreground ,sf-black :background "#ff8080"))))
       `(anzu-replace-highlight          ((,class (:inherit query-replace))))
       `(anzu-replace-to                 ((,class (:weight bold :foreground "#bd33fd" :background "#fdbd33"))))
       `(auto-dim-other-buffers-face     ((,class (:background "#f7f7f7"))))
       `(avy-background-face             ((,class (:background "#a9a9a9"))))
       `(avy-lead-face                   ((,class (:weight bold :foreground ,sf-black :background "#fea500"))))
       `(bbdb-company                    ((,class (:slant italic :foreground "steel blue"))))
       `(bbdb-field-name                 ((,class (:weight bold :foreground "steel blue"))))
       `(bbdb-field-value                ((,class (:foreground "steel blue"))))
       `(bbdb-name                       ((,class (:underline t :foreground "#ff6633"))))
       `(bmkp-light-autonamed            ((,class (:background "#f0f0f0"))))
       `(bmkp-light-fringe-autonamed     ((,class (:foreground "#5a5a5a" :background "#d4d4d4"))))
       `(bmkp-light-fringe-non-autonamed ((,class (:foreground "#ffffcc" :background "#01fffb")))) ; default
       `(bmkp-light-non-autonamed        ((,class (:background "#bffffe"))))
       `(bmkp-no-local                   ((,class (:background "pink"))))
       `(browse-kill-ring-separator-face ((,class (:foreground "red"))))
       `(calendar-month-header           ((,class (:weight bold :foreground "#4f4a3d" :background "#ffffcc"))))
       `(calendar-today                  ((,class (:weight bold :foreground "#4f4a3d" :background "#ffffcc"))))
       `(calendar-weekday-header         ((,class (:weight bold :foreground "#1662af"))))
       `(calendar-weekend-header         ((,class (:weight bold :foreground "#4e4e4e"))))
       `(cfw:face-annotation             ((,class (:foreground "green" :background "red"))))
       `(cfw:face-day-title              ((,class (:foreground "#c9c9c9"))))
       `(cfw:face-default-content        ((,class (:foreground "#2952a3"))))
       `(cfw:face-default-day            ((,class (:weight bold))))
       `(cfw:face-disable                ((,class (:foreground "DarkGray"))))
       `(cfw:face-grid                   ((,class (:foreground "#dddddd"))))
       `(cfw:face-header                 ((,class (:foreground "#1662af" :background ,sf-white :weight bold))))
       `(cfw:face-holiday                ((,class (:foreground "#777777" :background "#e4ebfe"))))
       `(cfw:face-periods                ((,class (:foreground ,sf-white :background "#668cd9" :slant italic))))
       `(cfw:face-saturday               ((,class (:foreground "#4e4e4e" :background ,sf-white :weight bold))))
       `(cfw:face-select                 ((,class (:foreground "#4a95eb" :background "#edf1fa"))))
       `(cfw:face-sunday                 ((,class (:foreground "#4e4e4e" :background ,sf-white :weight bold))))
       `(cfw:face-title                  ((,class (:height 2.0 :foreground "#676767" :weight bold :inherit variable-pitch))))
       `(cfw:face-today                  ((,class (:foreground "#4f4a3d" :background "#ffffcc"))))
       `(cfw:face-today-title            ((,class (:foreground ,sf-white :background "#1766b1"))))
       `(cfw:face-toolbar                ((,class (:background ,sf-white))))
       `(cfw:face-toolbar-button-off     ((,class (:foreground "#cfcfcf" :background ,sf-white))))
       `(cfw:face-toolbar-button-on      ((,class (:foreground "#5e5e5e" :background "#f6f6f6"))))
       `(change-log-date                 ((,class (:foreground "purple"))))
       `(change-log-file                 ((,class (:weight bold :foreground "#4183c4"))))
       `(change-log-list                 ((,class (:foreground ,sf-black :background "#75eec7"))))
       `(change-log-name                 ((,class (:foreground "#008000"))))
       `(circe-highlight-all-nicks-face  ((,class (:foreground "blue" :background "#f0f0f0")))) ; other nick names
       `(circe-highlight-nick-face       ((,class (:foreground "#009300" :background "#f0f0f0")))) ; messages with my nick cited
       `(circe-my-message-face           ((,class (:foreground "#8b8b8b" :background "#f0f0f0"))))
       `(circe-originator-face           ((,class (:foreground "blue"))))
       `(circe-prompt-face               ((,class (:foreground "red"))))
       `(circe-server-face               ((,class (:foreground "#99cae5"))))
       `(comint-highlight-input          ((,class (:weight bold :foreground "#0000ff" :inherit nil))))
       ;;`(comint-highlight-prompt         ((,class (:weight bold :foreground ,sf-black :background "gold"))))
       `(comint-highlight-prompt         ((,class (:weight bold :foreground "#0000ff" :inherit nil))))

       ;; `(ac-selection-face ((,class ,completion-selected-candidate)))
       `(ac-selection-face ((,class (:weight bold :foreground ,sf-white :background "orange")))) ; TEMP For diff'ing AC from Comp.
       `(ac-candidate-face ((,class ,completion-other-candidates)))
       `(ac-completion-face ((,class ,completion-inline)))
       `(ac-candidate-mouse-face ((,class (:inherit highlight))))
       `(popup-scroll-bar-background-face ((,class (:background "#ebf4fe"))))
       `(popup-scroll-bar-foreground-face ((,class (:background "#d1dae4")))) ; Scrollbar (visible).

       `(company-tooltip-common-selection     ((,class (:weight normal :foreground "#f9eccc" :inherit company-tooltip-selection)))) ; Prefix + common part in tooltip (for selection).
       `(company-tooltip-selection            ((,class ,completion-selected-candidate))) ; Suffix in tooltip (for selection).
       `(company-tooltip-annotation-selection ((,class (:weight normal :foreground "#f9eccc")))) ; Annotation (for selection).
       `(company-tooltip-common               ((,class (:weight normal :foreground "#b000b0" :inherit company-tooltip)))) ; Prefix + common part in tooltip.
       `(company-tooltip                      ((,class ,completion-other-candidates))) ; Suffix in tooltip.
       `(company-tooltip-annotation           ((,class (:weight normal :foreground "#2415ff")))) ; Annotation.
       `(company-preview-common               ((,class ,completion-inline)))
       `(company-scrollbar-bg                 ((,class (:background "#ebf4fe"))))
       `(company-scrollbar-fg                 ((,class (:background "#d1dae4")))) ; Scrollbar (visible).

       `(compare-windows ((,class (:background "#ffff00"))))
       ;; `(completions-common-part ((,class (:foreground "red" :weight bold))))
       ;; `(completions-first-difference ((,class (:foreground "green" :weight bold))))

       `(compilation-error ((,class (:weight bold :foreground "red")))) ; Used for grep error messages.
       `(compilation-info ((,class ,grep-file-name)))
       `(compilation-line-number ((,class ,grep-line-number)))
       `(compilation-warning ((,class (:weight bold :foreground "orange"))))
       `(compilation-mode-line-exit ((,class (:weight bold :foreground "green")))) ; :exit[matched]
       `(compilation-mode-line-fail ((,class (:weight bold :foreground "violet")))) ; :exit[no match]
       `(compilation-mode-line-run ((,class (:weight bold :foreground "orange")))) ; :run

       `(css-property ((,class (:foreground "#00aa00"))))
       `(css-selector ((,class (:weight bold :foreground "blue"))))

       `(custom-button ((,class (:box (:line-width 2 :style released-button) :foreground ,sf-black :background "lightgrey"))))
       `(custom-button-mouse ((,class (:box (:line-width 2 :style released-button) :foreground ,sf-black :background "grey90"))))
       `(custom-button-pressed ((,class (:box (:line-width 2 :style pressed-button) :foreground ,sf-black :background "light grey"))))
       `(custom-button-pressed-unraised ((,class (:underline t :foreground "magenta4"))))
       `(custom-button-unraised ((,class (:underline t))))
       `(custom-changed ((,class (:foreground ,sf-white :background "blue"))))
       `(custom-comment ((,class (:background "gray85"))))
       `(custom-comment-tag ((,class (:foreground "blue4"))))
       `(custom-documentation ((,class (nil))))
       `(custom-face-tag ((,class (:family "Sans Serif" :height 1.2 :weight bold))))
       `(custom-group-tag ((,class (:height 1.2 :weight bold :foreground "blue1"))))
       `(custom-group-tag-1 ((,class (:family "Sans Serif" :height 1.2 :weight bold :foreground "red1"))))
       `(custom-invalid ((,class (:foreground "yellow" :background "red"))))
       `(custom-link ((,class (:underline t :foreground "blue1"))))
       `(custom-modified ((,class (:foreground ,sf-white :background "blue"))))
       `(custom-rogue ((,class (:foreground "pink" :background ,sf-black))))
       `(custom-saved ((,class (:underline t))))
       `(custom-set ((,class (:foreground "blue" :background ,sf-white))))
       `(custom-state ((,class (:foreground "green4"))))
       `(custom-themed ((,class (:foreground ,sf-white :background "blue1"))))
       `(custom-variable-button ((,class (:weight bold :underline t))))
       `(custom-variable-tag ((,class (:family "Sans Serif" :height 1.2 :weight bold :foreground "blue1"))))
       `(custom-visibility ((,class ,link)))

       ;; diff
       `(diff-hl-change        ((,class (             :foreground "blue3"   :background "#dbedff" ))))
       `(diff-hl-delete        ((,class (             :foreground ,sf-red   :background "#ffdce0" ))))
       `(diff-hl-dired-change  ((,class (:weight bold :foreground ,sf-black :background "#ffa335" ))))
       `(diff-hl-dired-delete  ((,class (:weight bold :foreground ,sf-red                         ))))
       `(diff-hl-dired-ignored ((,class (:weight bold :foreground ,sf-white :background "#c0bbab" ))))
       `(diff-hl-dired-insert  ((,class (:weight bold :foreground "#b9b9ba"                       ))))
       `(diff-hl-dired-unknown ((,class (             :foreground ,sf-white :background "#3f3bb4" ))))
       `(diff-hl-insert        ((,class (             :foreground ,sf-green :background "#cdffd8" ))))
       `(diff-hl-unknown       ((,class (             :foreground ,sf-white :background "#3f3bb4" ))))

       `(diary-face ((,class (:foreground "#87c9fc"))))

       `(dircolors-face-asm            ((,class (:foreground ,sf-black))))
       `(dircolors-face-backup         ((,class (:foreground ,sf-black))))
       `(dircolors-face-compress       ((,class (:foreground ,sf-red))))
       `(dircolors-face-dir            ((,class ,directory)))
       `(dircolors-face-doc            ((,class (:foreground ,sf-black))))
       `(dircolors-face-dos            ((,class (:foreground ,sf-green))))
       `(dircolors-face-emacs          ((,class (:foreground ,sf-black))))
       `(dircolors-face-exec           ((,class (:foreground ,sf-green))))
       `(dircolors-face-html           ((,class (:foreground ,sf-black))))
       `(dircolors-face-img            ((,class (:foreground "magenta3"))))
       `(dircolors-face-lang           ((,class (:foreground ,sf-black))))
       `(dircolors-face-lang-interface ((,class (:foreground ,sf-black))))
       `(dircolors-face-make           ((,class (:foreground ,sf-black))))
       `(dircolors-face-objet          ((,class (:foreground ,sf-black))))
       `(dircolors-face-package        ((,class (:foreground ,sf-black))))
       `(dircolors-face-paddb          ((,class (:foreground ,sf-black))))
       `(dircolors-face-ps             ((,class (:foreground ,sf-black))))
       `(dircolors-face-sound          ((,class (:foreground "DeepSkyBlue"))))
       `(dircolors-face-tar            ((,class (:foreground "red"))))
       `(dircolors-face-text           ((,class (:foreground ,sf-black))))
       `(dircolors-face-yacc           ((,class (:foreground ,sf-black))))

       ;; DIRED
       `(dired-directory               ((,class ,directory)))
       `(dired-header                  ((,class ,directory)))
       `(dired-ignored                 ((,class (:strike-through t :foreground ,sf-red))))
       `(dired-mark                    ((,class ,marked-line)))
       `(dired-marked                  ((,class ,marked-line)))
       `(dired-symlink                 ((,class ,symlink)))
       `(diredp-compressed-file-suffix ((,class (:foreground ,sf-red))))
       `(diredp-date-time              ((,class (:foreground "purple"))))
       `(diredp-dir-heading            ((,class ,directory)))
       `(diredp-dir-name               ((,class ,directory)))
       `(diredp-dir-priv               ((,class ,directory)))
       `(diredp-exec-priv              ((,class (:background "#03c03c"))))
       `(diredp-executable-tag         ((,class (:foreground "ForestGreen" :background ,sf-white))))
       `(diredp-file-name              ((,class ,file)))
       `(diredp-file-suffix            ((,class (:foreground "#c0c0c0"))))
       `(diredp-flag-mark-line         ((,class ,marked-line)))
       `(diredp-ignored-file-name      ((,class ,shadow)))
       `(diredp-read-priv              ((,class (:background "#0a99ff"))))
       `(diredp-write-priv             ((,class (:foreground ,sf-white :background ,sf-red))))

       ;;
       `(eldoc-highlight-function-argument ((,class (:weight bold :foreground "red" :background "#ffe4ff"))))
       `(elfeed-search-filter-face         ((,class (:foreground "gray"))))
     ;;`(eww-form-checkbox                 ((,class ())))
     ;;`(eww-form-select                   ((,class ())))
     ;;`(eww-form-submit                   ((,class ())))
       `(eww-form-text                     ((,class (:weight bold :foreground "#40586f" :background "#a7cdf1"))))
     ;;`(eww-form-textarea                 ((,class ())))
       `(file-name-shadow                  ((,class ,shadow)))

       ;; Flycheck
       `(flycheck-error                        ((,class (:underline (:color "#fe251e" :style wave) :weight bold :background "#ffe1e1"))))
       `(flycheck-error-list-line-number       ((,class (:foreground "#a535ae"))))
       `(flycheck-fringe-error                 ((,class (:foreground "#fe251e"))))
       `(flycheck-fringe-info                  ((,class (:foreground "#158a15"))))
       `(flycheck-fringe-warning               ((,class (:foreground "#f4a939"))))
       `(flycheck-info                         ((,class (:underline (:color "#158a15" :style wave) :weight bold))))
       `(flycheck-warning                      ((,class (:underline (:color "#f4a939" :style wave) :weight bold :background "#ffffbe"))))
       `(fancy-narrow-blocked-face             ((,class (:foreground "#9998a4"))))
       `(flycheck-color-mode-line-error-face   ((,class (:background "#cf5b56"))))
       `(flycheck-color-mode-line-warning-face ((,class (:background "#ebc700"))))
       `(flycheck-color-mode-line-info-face    ((,class (:background "yellow"))))

       ;;
       `(font-latex-bold-face         ((,class (:weight bold :foreground ,sf-black))))
       `(font-latex-italic-face       ((,class (:slant italic :foreground ,sf-black))))
       `(font-latex-math-face         ((,class (:foreground ,sf-blue1))))
       `(font-latex-sectioning-1-face ((,class ,ol1)))
       `(font-latex-sectioning-2-face ((,class ,ol2)))
       `(font-latex-sectioning-3-face ((,class ,ol3)))
       `(font-latex-sectioning-4-face ((,class ,ol4)))
       `(font-latex-sectioning-5-face ((,class ,ol5)))
       `(font-latex-sedate-face       ((,class (:foreground ,sf-orange))))
       `(font-latex-string-face       ((,class (:weight bold :foreground ,sf-blue1))))
       `(font-latex-verbatim-face     ((,class ,link)))

       ;;
       `(git-commit-summary-face                ((,class (:foreground ,sf-black))))
       `(git-commit-comment-face                ((,class (:slant italic :foreground "#696969"))))
       `(git-timemachine-commit                 ((,class ,diff-removed)))
       `(git-timemachine-minibuffer-author-face ((,class ,diff-added)))
       `(git-timemachine-minibuffer-detail-face ((,class ,diff-header)))

       ;; Helm
       `(helm-action                          ((,class (:foreground ,sf-black))))
       `(helm-bookmark-file                   ((,class ,file)))
       `(helm-bookmarks-su-face               ((,class (:foreground "red"))))
       `(helm-buffer-directory                ((,class ,directory)))
     ;;`(helm-non-file-buffer                 ((,class (:slant italic :foreground "blue"))))
     ;;`(helm-buffer-file                     ((,class (:foreground ,sf-black))))
       `(helm-buffer-modified                 ((,class (:slant italic :foreground "#ba36a5"))))
       `(helm-buffer-process                  ((,class (:foreground "#008200"))))
       `(helm-candidate-number                ((,class (:foreground ,sf-black :background "#ffff66"))))
       `(helm-dir-heading                     ((,class (:foreground "blue" :background "pink"))))
       `(helm-dir-priv                        ((,class (:foreground "dark red" :background "light grey"))))
       `(helm-ff-directory                    ((,class ,directory)))
       `(helm-ff-dotted-directory             ((,class ,directory)))
       `(helm-ff-executable                   ((,class (:foreground "green3" :background ,sf-white))))
       `(helm-ff-file                         ((,class (:foreground ,sf-black))))
       `(helm-ff-invalid-symlink              ((,class (:foreground "yellow" :background "red"))))
       `(helm-ff-symlink                      ((,class ,symlink)))
       `(helm-file-name                       ((,class (:foreground "blue"))))
       `(helm-gentoo-match-face               ((,class (:foreground "red"))))
       `(helm-grep-file                       ((,class ,grep-file-name)))
       `(helm-grep-lineno                     ((,class ,grep-line-number)))
       `(helm-grep-match                      ((,class ,match)))
       `(helm-grep-running                    ((,class (:weight bold :foreground ,sf-white))))
       `(helm-isearch-match                   ((,class (:background "#ccffcc"))))
       `(helm-lisp-show-completion            ((,class ,volatile-highlight-supersize))) ; See `helm-dabbrev'.
     ;;`(helm-ls-git-added-copied-face        ((,class (:foreground ""))))
     ;;`(helm-ls-git-added-modified-face      ((,class (:foreground ""))))
     ;;`(helm-ls-git-conflict-face            ((,class (:foreground ""))))
     ;;`(helm-ls-git-deleted-and-staged-face  ((,class (:foreground ""))))
     ;;`(helm-ls-git-deleted-not-staged-face  ((,class (:foreground ""))))
     ;;`(helm-ls-git-modified-and-staged-face ((,class (:foreground ""))))
       `(helm-ls-git-modified-not-staged-face ((,class (:foreground "#ba36a5"))))
     ;;`(helm-ls-git-renamed-modified-face    ((,class (:foreground ""))))
     ;;`(helm-ls-git-untracked-face           ((,class (:foreground ""))))
       `(helm-match                           ((,class ,match)))
       `(helm-moccur-buffer                   ((,class (:foreground "#0066cc"))))
       `(helm-selection                       ((,class (:background "#3875d6" :foreground ,sf-white))))
       `(helm-selection-line                  ((,class ,highlight-gray))) ; ???
       `(helm-separator                       ((,class (:foreground "red"))))
       `(helm-source-header                   ((,class (:weight bold :box (:line-width 1 :color "#c7c7c7") :background "#dedede" :foreground ,sf-black))))
       `(helm-swoop-target-line-block-face    ((,class (:background "#cccc00" :foreground "#222222"))))
       `(helm-swoop-target-line-face          ((,class (:background "#ccccff"))))
       `(helm-swoop-target-word-face          ((,class (:weight bold :foreground nil :background "#fdbd33"))))
       `(helm-visible-mark                    ((,class ,marked-line)))
       `(helm-w3m-bookmarks-face              ((,class (:underline t :foreground "cyan1"))))

       `(highlight-changes        ((,class (:foreground nil)))) ;; blue "#2e08b5"
       `(highlight-changes-delete ((,class (:strike-through nil :foreground nil)))) ;; red "#b5082e"
       `(highlight-symbol-face    ((,class (:background "#ffffa0"))))

       `(hl-line ((,class ,highlight-yellow))) ; Highlight current line.

       `(hl-tags-face ((,class ,highlight-current-tag))) ; ~ Pair highlighting (matching tags).

       `(holiday-face ((,class (:foreground "#777777" :background "#e4ebfe"))))

       `(html-helper-bold-face      ((,class (:weight bold :foreground ,sf-black))))
       `(html-helper-italic-face    ((,class (:slant italic :foreground ,sf-black))))
       `(html-helper-underline-face ((,class (:underline t :foreground ,sf-black))))
       `(html-tag-face              ((,class (:foreground "blue"))))

       `(ilog-non-change-face ((,class (:height 2.0 :foreground "#6434a3"))))
       `(ilog-change-face     ((,class (:height 2.0 :foreground "#008200"))))
       `(ilog-echo-face       ((,class (:height 2.0 :foreground "#006fe0"))))
       `(ilog-load-face       ((,class (:foreground "#ba36a5"))))
       `(ilog-message-face    ((,class (:foreground "#808080"))))

       `(indent-guide-face ((,class (:foreground "#d3d3d3"))))

       `(info-file         ((,class (:family "Sans Serif" :height 1.8 :weight bold :box (:line-width 1 :color "#0000cc") :foreground "cornflower blue" :background "LightSteelBlue1"))))
       `(info-header-node  ((,class (:underline t :foreground "orange")))) ; nodes in header
       `(info-header-xref  ((,class (:underline t :foreground "dodger blue")))) ; cross references in header
       `(info-index-match  ((,class (:weight bold :foreground nil :background "#fdbd33")))) ; when using `i'
       `(info-menu-header  ((,class ,ol2))) ; menu titles (headers) -- major topics
       `(info-menu-star    ((,class (:foreground ,sf-black)))) ; every 3rd menu item
       `(info-node         ((,class (:underline t :foreground "blue")))) ; node names
       `(info-quoted-name  ((,class ,code-inline)))
       `(info-string       ((,class ,string)))
       `(info-title-1      ((,class ,ol1)))
       `(info-xref         ((,class (:underline t :foreground "#006daf")))) ; unvisited cross-references
       `(info-xref-visited ((,class (:underline t :foreground "magenta4")))) ; previously visited cross-references

       `(js2-error                    ((,class (:box (:line-width 1 :color ,sf-red) :background ,sf-lightred))))
       `(js2-external-variable        ((,class (:foreground "#ff0000" :background "#fff8f8"))))
       `(js2-function-param           ((,class ,function-param)))
       `(js2-instance-member          ((,class (:foreground "DarkOrchid"))))
       `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,sf-red))))
       `(js2-jsdoc-html-tag-name      ((,class (:foreground ,sf-red))))
       `(js2-jsdoc-tag                ((,class (:weight normal :foreground "#6434a3"))))
       `(js2-jsdoc-type               ((,class (:foreground "SteelBlue"))))
       `(js2-jsdoc-value              ((,class (:weight normal :foreground "#ba36a5")))) ; #800080
       `(js2-magic-paren              ((,class (:underline t))))
       `(js2-private-function-call    ((,class (:foreground "goldenrod"))))
       `(js2-private-member           ((,class (:foreground "PeachPuff3"))))
       `(js2-warning                  ((,class (:underline "orange"))))

       ;; Org non-standard faces.
       `(leuven-summerfruit-org-deadline-overdue ((,class (:foreground "#f22659"))))
       `(leuven-summerfruit-org-deadline-today ((,class (:weight bold :foreground "#4f4a3d" :background "#ffffcc"))))
       `(leuven-summerfruit-org-deadline-tomorrow ((,class (:foreground "#40a80b"))))
       `(leuven-summerfruit-org-deadline-future ((,class (:foreground "#40a80b"))))
       `(leuven-summerfruit-gnus-unseen ((,class (:weight bold :foreground "#fc7202"))))
       `(leuven-summerfruit-gnus-date ((,class (:foreground "#ff80bf"))))
       `(leuven-summerfruit-gnus-size ((,class (:foreground "#8fbf60"))))
       `(leuven-summerfruit-todo-items-face ((,class (:weight bold :foreground "#ff3125" :background "#ffff88"))))

       `(light-symbol-face ((,class (:background "#ffffa0"))))

     ;;`(linum ((,class (:foreground ,sf-white :background ,sf-red :weight bold))))
       `(line-number ((,class (:foreground ,sf-white :background ,sf-blue6 :weight bold))))

       `(log-view-file ((,class (:foreground "#0000cc" :background "#eaf2f5"))))
       `(log-view-message ((,class (:foreground ,sf-black :background "#edea74"))))
       `(lui-button-face ((,class ,link)))
       `(lui-highlight-face ((,class (:box '(:line-width 1 :color "#cc0000") :foreground "#cc0000" :background "#ffff88")))) ; my nickname
       `(lui-time-stamp-face ((,class (:foreground "purple"))))

       ;; Magit
       `(magit-blame-header     ((,class (:inherit magit-diff-file-header))))
       `(magit-blame-heading    ((,class (:overline "#a7a7a7" :foreground ,sf-red :background "#e6e6e6"))))
       `(magit-blame-hash       ((,class (:overline "#a7a7a7" :foreground ,sf-red :background "#e6e6e6"))))
       `(magit-blame-name       ((,class (:overline "#a7a7a7" :foreground "#036a07" :background "#e6e6e6"))))
       `(magit-blame-date       ((,class (:overline "#a7a7a7" :foreground "blue" :background "#e6e6e6"))))
       `(magit-blame-summary    ((,class (:overline "#a7a7a7" :weight bold :foreground "#707070" :background "#e6e6e6"))))
       `(magit-branch           ((,class ,vc-branch)))
       `(magit-diff-add         ((,class ,diff-added)))
       `(magit-diff-del         ((,class ,diff-removed)))
       `(magit-diff-file-header ((,class (:height 1.1 :weight bold :foreground "#4183c4"))))
       `(magit-diff-hunk-header ((,class ,diff-hunk-header)))
       `(magit-diff-none        ((,class ,diff-none)))
       `(magit-header           ((,class (:foreground ,sf-white :background "#ff4040"))))
       `(magit-item-highlight   ((,class (:background "#eaf2f5"))))
       `(magit-item-mark        ((,class ,marked-line)))
       `(magit-log-head-label   ((,class (:box (:line-width 1 :color "blue" :style nil)))))
       `(magit-log-tag-label    ((,class (:box (:line-width 1 :color "#00cc00" :style nil)))))
       `(magit-section-title    ((,class (:family "Sans Serif" :height 1.8 :weight bold :foreground "cornflower blue" :inherit nil))))

       `(makefile-space-face ((,class (:background "hot pink"))))
       `(makefile-targets ((,class (:weight bold :foreground "blue"))))

       ;; Markdown
     ;;`(markdown-blockquote-face       ((,class ())))
       `(markdown-bold-face             ((,class (:inherit bold))))
     ;;`(markdown-comment-face          ((,class ())))
     ;;`(markdown-footnote-face         ((,class ())))
     ;;`(markdown-header-delimiter-face ((,class ())))
     ;;`(markdown-header-face           ((,class ())))
       `(markdown-header-face-1         ((,class ,ol1)))
       `(markdown-header-face-2         ((,class ,ol2)))
       `(markdown-header-face-3         ((,class ,ol3)))
       `(markdown-header-face-4         ((,class ,ol4)))
       `(markdown-header-face-5         ((,class ,ol5)))
       `(markdown-header-face-6         ((,class ,ol6)))
     ;;`(markdown-header-rule-face      ((,class ())))
       `(markdown-inline-code-face      ((,class ,code-inline)))
       `(markdown-italic-face           ((,class (:inherit italic))))
       `(markdown-language-keyword-face ((,class (:inherit org-block-begin-line))))
     ;;`(markdown-line-break-face       ((,class ())))
       `(markdown-link-face             ((,class ,link-no-underline)))
     ;;`(markdown-link-title-face       ((,class ())))
     ;;`(markdown-list-face             ((,class ())))
     ;;`(markdown-math-face             ((,class ())))
     ;;`(markdown-metadata-key-face     ((,class ())))
     ;;`(markdown-metadata-value-face   ((,class ())))
     ;;`(markdown-missing-link-face     ((,class ())))
       `(markdown-pre-face              ((,class (:inherit org-block-background))))
     ;;`(markdown-reference-face        ((,class ())))
     ;;`(markdown-strike-through-face   ((,class ())))
       `(markdown-url-face              ((,class ,link)))


       `(match ((,class ,match)))           ; Used for grep matches.
       `(mc/cursor-bar-face ((,class (:height 1.0 :foreground "#1664c4" :background "#1664c4"))))
       `(mc/cursor-face ((,class (:inverse-video t))))
       `(mc/region-face ((,class (:inherit region))))

       `(mm-uu-extract ((,class ,code-block)))
       `(moccur-current-line-face ((,class (:foreground ,sf-black :background "#ffffcc"))))
       `(moccur-face ((,class (:foreground ,sf-black :background "#ffff99"))))
       `(next-error ((,class ,volatile-highlight-supersize)))
       `(nobreak-space ((,class (:background "#cce8f6"))))

       ;; NXML
       `(nxml-attribute-local-name-face          ((,class ,xml-attribute)))
       `(nxml-attribute-value-delimiter-face     ((,class (:foreground "green4"))))
       `(nxml-attribute-value-face               ((,class (:foreground "green4"))))
       `(nxml-comment-content-face               ((,class (:slant italic :foreground "red"))))
       `(nxml-comment-delimiter-face             ((,class (:foreground "red"))))
       `(nxml-element-local-name                 ((,class ,xml-tag)))
       `(nxml-element-local-name-face            ((,class (:foreground "blue"))))
       `(nxml-processing-instruction-target-face ((,class (:foreground "purple1"))))
       `(nxml-tag-delimiter-face                 ((,class (:foreground "blue"))))
       `(nxml-tag-slash-face                     ((,class (:foreground "blue"))))

       ;; Org
       `(org-agenda-block-count      ((,class (:weight bold :foreground "#a5a5a5"))))
       `(org-agenda-calendar-event   ((,class (:weight bold :foreground "#3774cc" :background "#e4ebfe"))))
       `(org-agenda-calendar-sexp    ((,class (:foreground "#327acd" :background "#f3f7fc"))))
       `(org-agenda-clocking         ((,class (:foreground ,sf-black :background "#eec900"))))
       `(org-agenda-column-dateline  ((,class ,column)))
       `(org-agenda-current-time     ((,class (:underline t :foreground "#1662af"))))
       `(org-agenda-date             ((,class (,@(leuven-summerfruit-scale-font leuven-summerfruit-scale-org-agenda-structure 1.6) :weight bold :foreground "#1662af"))))
       `(org-agenda-date-today       ((,class (,@(leuven-summerfruit-scale-font leuven-summerfruit-scale-org-agenda-structure 1.6) :weight bold :foreground "#4f4a3d" :background "#ffffcc"))))
       `(org-agenda-date-weekend     ((,class (,@(leuven-summerfruit-scale-font leuven-summerfruit-scale-org-agenda-structure 1.6) :weight bold :foreground "#4e4e4e"))))
       `(org-agenda-diary            ((,class (:weight bold :foreground "green4" :background "light blue"))))
       `(org-agenda-dimmed-todo-face ((,class (:foreground "gold2"))))
       `(org-agenda-done             ((,class (:foreground "#555555"))))
       `(org-agenda-filter-category  ((,class (:weight bold :foreground "orange"))))
       `(org-agenda-filter-effort    ((,class (:weight bold :foreground "orange"))))
       `(org-agenda-filter-regexp    ((,class (:weight bold :foreground "orange"))))
       `(org-agenda-filter-tags      ((,class (:weight bold :foreground "orange"))))
       `(org-agenda-restriction-lock ((,class (:background "#e77d63"))))
       `(org-agenda-structure        ((,class (,@(leuven-summerfruit-scale-font leuven-summerfruit-scale-org-agenda-structure 1.6) :weight bold :foreground ,sf-blue4))))
       `(org-archived                ((,class (:foreground "gray70"))))
       `(org-beamer-tag              ((,class (:box (:line-width 1 :color "#fabc18") :foreground "#2c2c2c" :background "#fff8d0"))))
       `(org-block                   ((,class ,code-block)))
       `(org-block-background        ((,class (:background "#eeeeee" :extend t)))) ;; :inherit fixed-pitch))))
       `(org-block-begin-line        ((,class (:underline "#a7a6aa" :foreground "#555555" :background "#e2e1d5" :extend t))))
       `(org-block-end-line          ((,class (:overline "#a7a6aa" :foreground "#555555" :background "#e2e1d5" :extend t))))
       `(org-checkbox                ((,class (:weight bold :box (:line-width 1 :style pressed-button) :foreground "#123555" :background "#a3a3a3"))))
       `(org-clock-overlay           ((,class (:foreground ,sf-white :background "SkyBlue4"))))
       `(org-code                    ((,class ,code-inline)))
       `(org-column                  ((,class ,column)))
       `(org-column-title            ((,class ,column)))
       `(org-date                    ((,class (:underline t :foreground "#00459e"))))
       `(org-default                 ((,class (:foreground ,sf-black :background "#ffffff"))))
       `(org-dim                     ((,class (:foreground "#aaaaaa"))))
       `(org-document-info           ((,class (:foreground "#484848"))))
       `(org-document-info-keyword   ((,class (:foreground ,sf-blue4 :background "#eaeaff"))))
       `(org-document-title          ((,class (:height 1.8 :weight bold :foreground ,sf-black))))
       `(org-done                    ((,class (:weight bold :box (:line-width 1 :color "#bbbbbb") :foreground "#bbbbbb" :background "#f0f0f0"))))
       `(org-drawer                  ((,class (:foreground "light sky blue"))))
       `(org-ellipsis                ((,class (:underline nil :foreground "#999999")))) ; #FFEE62
       `(org-example                 ((,class (:foreground "blue" :background "#eaffea"))))
       `(org-footnote                ((,class (:underline t :foreground ,sf-blue4))))
       `(org-formula                 ((,class (:foreground "chocolate1"))))
       `(org-headline-done           ((,class (:height 1.0 :weight normal :strike-through t :foreground "#adadad"))))
       `(org-hide                    ((,class (:foreground "#e2e2e2"))))
       `(org-inlinetask              ((,class (:box (:line-width 1 :color "#ebebeb") :foreground "#777777" :background "#ffffd6"))))
       `(org-latex-and-related       ((,class (:foreground ,sf-blue3 :background ,sf-white))))
       `(org-level-1                 ((,class ,ol1)))
       `(org-level-2                 ((,class ,ol2)))
       `(org-level-3                 ((,class ,ol3)))
       `(org-level-4                 ((,class ,ol4)))
       `(org-level-5                 ((,class ,ol5)))
       `(org-level-6                 ((,class ,ol6)))
       `(org-level-7                 ((,class ,ol7)))
       `(org-level-8                 ((,class ,ol8)))
       `(org-link                    ((,class ,link)))
       `(org-list-dt                 ((,class (:weight bold :foreground "#335ea8"))))
       `(org-macro                   ((,class (:weight bold :foreground "#edb802"))))
       `(org-meta-line               ((,class (:slant italic :foreground "#aaaaaa"))))
       `(org-mode-line-clock         ((,class (:box (:line-width 1 :color "#335ea8") :foreground ,sf-black :background "#ffa335"))))
       `(org-mode-line-clock-overrun ((,class (:weight bold :box (:line-width 1 :color "#335ea8") :foreground ,sf-white :background "#ff4040"))))
       `(org-number-of-items         ((,class (:weight bold :foreground ,sf-white :background "#79ba79"))))
       `(org-property-value          ((,class (:foreground "#00a000"))))
       `(org-quote                   ((,class (:slant italic :foreground "dim gray" :background "#ffffe0"))))
       `(org-scheduled               ((,class (:foreground ,sf-black))))
       `(org-scheduled-previously    ((,class (:foreground "#1466c6"))))
       `(org-scheduled-today         ((,class (:weight bold :foreground "#4f4a3d" :background "#ffffcc"))))
       `(org-sexp-date               ((,class (:foreground "#3774cc"))))
       `(org-special-keyword         ((,class (:weight bold :foreground "#00bb00" :background "#eaffea"))))
       `(org-table                   ((,class (:foreground "dark green" :background "#eaffea")))) ;; :inherit fixed-pitch))))
       `(org-tag                     ((,class (:weight normal :slant italic :foreground "#9a9fa4" :background ,sf-white))))
       `(org-target                  ((,class (:foreground "#ff6daf"))))
       `(org-time-grid               ((,class (:foreground "#cfcfcf"))))
       `(org-todo                    ((,class (:weight bold :box (:line-width 1 :color "#d8aba7") :foreground "#d8aba7" :background "#ffe6e4"))))
       `(org-upcoming-deadline       ((,class (:foreground "#ff5555"))))
       `(org-verbatim                ((,class (:foreground "#0066cc" :background "#f7fdff"))))
       `(org-verse                   ((,class (:slant italic :foreground "dim gray" :background "#eeeeee"))))
       `(org-warning                 ((,class (:weight bold :foreground ,sf-black :background "#cce7ff"))))

       ;; Outline
       `(outline-1 ((,class ,ol1)))
       `(outline-2 ((,class ,ol2)))
       `(outline-3 ((,class ,ol3)))
       `(outline-4 ((,class ,ol4)))
       `(outline-5 ((,class ,ol5)))
       `(outline-6 ((,class ,ol6)))
       `(outline-7 ((,class ,ol7)))
       `(outline-8 ((,class ,ol8)))

       ;;
       `(pabbrev-debug-display-label-face ((,class (:foreground ,sf-white :background "#a62154"))))
       `(pabbrev-suggestions-face ((,class (:weight bold :foreground ,sf-white :background "red"))))
       `(pabbrev-suggestions-label-face ((,class (:weight bold :foreground ,sf-white :background "purple"))))
       `(paren-face-match ((,class ,paren-matched)))
       `(paren-face-mismatch ((,class ,paren-unmatched)))
       `(paren-face-no-match ((,class ,paren-unmatched)))
       `(persp-selected-face ((,class (:weight bold :foreground "#eef5fe"))))
       `(powerline-active1 ((,class (:foreground "#85ceeb" :background "#383838" :inherit mode-line))))
       `(powerline-active2 ((,class (:foreground "#85ceeb" :background "#4070b6" :inherit mode-line))))
       `(powerline-inactive1 ((,class (:foreground "#f0f0ef" :background "#686868" :inherit mode-line-inactive))))
       `(powerline-inactive2 ((,class (:foreground "#f0f0ef" :background "#a9a9a9" :inherit mode-line-inactive))))

       ;; Rainbow delimiters
       `(rainbow-delimiters-depth-1-face    ((,class (:foreground "#383838"       ))))
       `(rainbow-delimiters-depth-2-face    ((,class (:foreground "deep pink"     ))))
       `(rainbow-delimiters-depth-3-face    ((,class (:foreground "#036a07"       ))))
       `(rainbow-delimiters-depth-4-face    ((,class (:foreground "deep sky blue" ))))
       `(rainbow-delimiters-depth-4-face    ((,class (:foreground "dark red"      ))))
       `(rainbow-delimiters-depth-5-face    ((,class (:foreground "orchid"        ))))
       `(rainbow-delimiters-depth-6-face    ((,class (:foreground "steel blue"    ))))
       `(rainbow-delimiters-depth-7-face    ((,class (:foreground ,sf-orange       ))))
       `(rainbow-delimiters-depth-8-face    ((,class (:foreground "#cc3333"       ))))
       `(rainbow-delimiters-depth-9-face    ((,class (:foreground "#edb802"       ))))
       `(rainbow-delimiters-mismatched-face ((,class ,paren-unmatched)))
       `(rainbow-delimiters-unmatched-face  ((,class ,paren-unmatched)))

       ;;
       `(recover-this-file ((,class (:weight bold :background "#ff3f3f"))))
       `(rng-error ((,class (:weight bold :foreground "red" :background "#fbe3e4"))))
       `(sh-heredoc ((,class (:foreground "blue" :background "#eef5fe"))))
       `(sh-quoted-exec ((,class (:foreground "#ff1493"))))
       `(shadow ((,class ,shadow)))         ; Used for grep context lines.
       `(shell-option-face ((,class (:foreground "forest green"))))
       `(shell-output-2-face ((,class (:foreground "blue"))))
       `(shell-output-3-face ((,class (:foreground "purple"))))
       `(shell-output-face ((,class (:foreground ,sf-black))))
       ;; `(shell-prompt-face ((,class (:weight bold :foreground "yellow"))))
       `(shm-current-face ((,class (:background "#eee8d5"))))
       `(shm-quarantine-face ((,class (:background "lemonchiffon"))))
       `(show-paren-match ((,class ,paren-matched)))
       `(show-paren-mismatch ((,class ,paren-unmatched)))
       `(sml-modeline-end-face ((,class (:background "#6badf6")))) ; #335EA8
       `(sml-modeline-vis-face ((,class (:background "#1979ca"))))
       `(term ((,class (:foreground ,sf-black :background "#ffffff"))))

       ;; `(sp-pair-overlay-face ((,class ())))
       ;; `(sp-show-pair-enclosing ((,class ())))
       ;; `(sp-show-pair-match-face ((,class ()))) ; ~ Pair highlighting (matching tags).
       ;; `(sp-show-pair-mismatch-face ((,class ())))
       ;; `(sp-wrap-overlay-closing-pair ((,class ())))
       ;; `(sp-wrap-overlay-face ((,class ())))
       ;; `(sp-wrap-overlay-opening-pair ((,class ())))
       ;; `(sp-wrap-tag-overlay-face ((,class ())))

       `(speedbar-button-face ((,class (:foreground "green4"))))
       `(speedbar-directory-face ((,class (:foreground "blue4"))))
       `(speedbar-file-face ((,class (:foreground "cyan4"))))
       `(speedbar-highlight-face ((,class ,volatile-highlight)))
       `(speedbar-selected-face ((,class (:underline t :foreground "red"))))
       `(speedbar-tag-face ((,class (:foreground "brown"))))
       `(svn-status-directory-face ((,class ,directory)))
       `(svn-status-filename-face ((,class (:weight bold :foreground "#4183c4"))))
       `(svn-status-locked-face ((,class (:weight bold :foreground "red"))))
       `(svn-status-marked-face ((,class ,marked-line)))
       `(svn-status-marked-popup-face ((,class (:weight bold :foreground "green3"))))
       `(svn-status-switched-face ((,class (:slant italic :foreground "gray55"))))
       `(svn-status-symlink-face ((,class ,symlink)))
       `(svn-status-update-available-face ((,class (:foreground "orange"))))
       `(tex-verbatim ((,class (:foreground "blue"))))
       `(tool-bar ((,class (:box (:line-width 1 :style released-button) :foreground ,sf-black :background "gray75"))))
       `(tooltip ((,class (:foreground ,sf-black :background "light yellow"))))
       `(traverse-match-face ((,class (:weight bold :foreground "blue violet"))))

       ;;
       `(vc-annotate-face-3F3FFF ((,class (:foreground "#3f3fff" :background ,sf-black))))
       `(vc-annotate-face-3F6CFF ((,class (:foreground "#3f3fff" :background ,sf-black))))
       `(vc-annotate-face-3F99FF ((,class (:foreground "#3f99ff" :background ,sf-black))))
       `(vc-annotate-face-3FC6FF ((,class (:foreground "#3f99ff" :background ,sf-black))))
       `(vc-annotate-face-3FF3FF ((,class (:foreground "#3ff3ff" :background ,sf-black))))
       `(vc-annotate-face-3FFF56 ((,class (:foreground "#4bff4b" :background ,sf-black))))
       `(vc-annotate-face-3FFF83 ((,class (:foreground "#3fffb0" :background ,sf-black))))
       `(vc-annotate-face-3FFFB0 ((,class (:foreground "#3fffb0" :background ,sf-black))))
       `(vc-annotate-face-3FFFDD ((,class (:foreground "#3ff3ff" :background ,sf-black))))
       `(vc-annotate-face-56FF3F ((,class (:foreground "#4bff4b" :background ,sf-black))))
       `(vc-annotate-face-83FF3F ((,class (:foreground "#b0ff3f" :background ,sf-black))))
       `(vc-annotate-face-B0FF3F ((,class (:foreground "#b0ff3f" :background ,sf-black))))
       `(vc-annotate-face-DDFF3F ((,class (:foreground "#fff33f" :background ,sf-black))))
       `(vc-annotate-face-F6FFCC ((,class (:foreground ,sf-black :background "#ffffc0"))))
       `(vc-annotate-face-FF3F3F ((,class (:foreground "#ff3f3f" :background ,sf-black))))
       `(vc-annotate-face-FF6C3F ((,class (:foreground "#ff3f3f" :background ,sf-black))))
       `(vc-annotate-face-FF993F ((,class (:foreground "#ff993f" :background ,sf-black))))
       `(vc-annotate-face-FFC63F ((,class (:foreground "#ff993f" :background ,sf-black))))
       `(vc-annotate-face-FFF33F ((,class (:foreground "#fff33f" :background ,sf-black))))

       ;; ;; vc
       ;; (vc-up-to-date-state    ((,c :foreground ,(gc 'green-1))))
       ;; (vc-edited-state        ((,c :foreground ,(gc 'yellow+1))))
       ;; (vc-missing-state       ((,c :foreground ,(gc 'red))))
       ;; (vc-conflict-state      ((,c :foreground ,(gc 'red+2) :weight bold)))
       ;; (vc-locked-state        ((,c :foreground ,(gc 'cyan-1))))
       ;; (vc-locally-added-state ((,c :foreground ,(gc 'blue))))
       ;; (vc-needs-update-state  ((,c :foreground ,(gc 'magenta))))
       ;; (vc-removed-state       ((,c :foreground ,(gc 'red-1))))

       `(vhl/default-face ((,class ,volatile-highlight))) ; `volatile-highlights.el' (for undo, yank).

       ;;
       `(w3m-anchor                            ((,class ,link)))
       `(w3m-arrived-anchor                    ((,class (:foreground "purple1"))))
       `(w3m-bitmap-image-face                 ((,class (:foreground "gray4" :background "green"))))
       `(w3m-bold                              ((,class (:weight bold :foreground ,sf-black))))
       `(w3m-current-anchor                    ((,class (:weight bold :underline t :foreground "blue"))))
       `(w3m-form                              ((,class (:underline t :foreground "tan1"))))
       `(w3m-form-button-face                  ((,class (:weight bold :underline t :foreground "gray4" :background "light grey"))))
       `(w3m-form-button-mouse-face            ((,class (:underline t :foreground "light grey" :background "#2b7e2a"))))
       `(w3m-form-button-pressed-face          ((,class (:weight bold :underline t :foreground "gray4" :background "light grey"))))
       `(w3m-header-line-location-content-face ((,class (:foreground "#7f7f7f":background "#f7f7f7"))))
       `(w3m-header-line-location-title-face   ((,class (:foreground "#2c55b1" :background "#f7f7f7"))))
       `(w3m-history-current-url-face          ((,class (:foreground "lemon chiffon"))))
       `(w3m-image-face                        ((,class (:weight bold :foreground "DarkSeaGreen2"))))
       `(w3m-link-numbering                    ((,class (:foreground "#b4c7eb")))) ; mouseless browsing
       `(w3m-strike-through-face               ((,class (:strike-through t))))
       `(w3m-underline-face                    ((,class (:underline t))))

       ;; Web mode
     ;;`(web-mode-block-attr-name-face           ((,class ())))
     ;;`(web-mode-block-attr-value-face          ((,class ())))
     ;;`(web-mode-block-comment-face             ((,class ())))
     ;;`(web-mode-block-control-face             ((,class ())))
     ;;`(web-mode-block-delimiter-face           ((,class ())))
     ;;`(web-mode-block-face                     ((,class ())))
     ;;`(web-mode-block-string-face              ((,class ())))
     ;;`(web-mode-bold-face                      ((,class ())))
     ;;`(web-mode-builtin-face                   ((,class ())))
     ;;`(web-mode-comment-face                   ((,class ())))
     ;;`(web-mode-comment-keyword-face           ((,class ())))
     ;;`(web-mode-constant-face                  ((,class ())))
     ;;`(web-mode-css-at-rule-face               ((,class ())))
     ;;`(web-mode-css-color-face                 ((,class ())))
     ;;`(web-mode-css-comment-face               ((,class ())))
     ;;`(web-mode-css-function-face              ((,class ())))
     ;;`(web-mode-css-priority-face              ((,class ())))
     ;;`(web-mode-css-property-name-face         ((,class ())))
     ;;`(web-mode-css-pseudo-class-face          ((,class ())))
     ;;`(web-mode-css-selector-face              ((,class ())))
     ;;`(web-mode-css-string-face                ((,class ())))
     ;;`(web-mode-css-variable-face              ((,class ())))
     ;;`(web-mode-current-column-highlight-face  ((,class ())))
       `(web-mode-current-element-highlight-face ((,class (:background "#99ccff")))) ; #FFEE80
     ;;`(web-mode-doctype-face                   ((,class ())))
     ;;`(web-mode-error-face                     ((,class ())))
     ;;`(web-mode-filter-face                    ((,class ())))
       `(web-mode-folded-face                    ((,class (:box (:line-width 1 :color "#777777") :foreground "#9a9a6a" :background "#f3f349"))))
     ;;`(web-mode-function-call-face             ((,class ())))
     ;;`(web-mode-function-name-face             ((,class ())))
     ;;`(web-mode-html-attr-custom-face          ((,class ())))
     ;;`(web-mode-html-attr-engine-face          ((,class ())))
     ;;`(web-mode-html-attr-equal-face           ((,class ())))
       `(web-mode-html-attr-name-face            ((,class ,xml-attribute)))
     ;;`(web-mode-html-attr-value-face           ((,class ())))
     ;;`(web-mode-html-entity-face               ((,class ())))
       `(web-mode-html-tag-bracket-face          ((,class ,xml-tag)))
     ;;`(web-mode-html-tag-custom-face           ((,class ())))
       `(web-mode-html-tag-face                  ((,class ,xml-tag)))
     ;;`(web-mode-html-tag-namespaced-face       ((,class ())))
     ;;`(web-mode-inlay-face                     ((,class ())))
     ;;`(web-mode-italic-face                    ((,class ())))
     ;;`(web-mode-javascript-comment-face        ((,class ())))
     ;;`(web-mode-javascript-string-face         ((,class ())))
     ;;`(web-mode-json-comment-face              ((,class ())))
     ;;`(web-mode-json-context-face              ((,class ())))
     ;;`(web-mode-json-key-face                  ((,class ())))
     ;;`(web-mode-json-string-face               ((,class ())))
     ;;`(web-mode-jsx-depth-1-face               ((,class ())))
     ;;`(web-mode-jsx-depth-2-face               ((,class ())))
     ;;`(web-mode-jsx-depth-3-face               ((,class ())))
     ;;`(web-mode-jsx-depth-4-face               ((,class ())))
     ;;`(web-mode-keyword-face                   ((,class ())))
     ;;`(web-mode-param-name-face                ((,class ())))
     ;;`(web-mode-part-comment-face              ((,class ())))
       `(web-mode-part-face                      ((,class (:background "#ffffe0"))))
     ;;`(web-mode-part-string-face               ((,class ())))
     ;;`(web-mode-preprocessor-face              ((,class ())))
       `(web-mode-script-face                    ((,class (:background "#eff0f1"))))
     ;;`(web-mode-sql-keyword-face               ((,class ())))
     ;;`(web-mode-string-face                    ((,class ())))
     ;;`(web-mode-style-face                     ((,class ())))
     ;;`(web-mode-symbol-face                    ((,class ())))
     ;;`(web-mode-type-face                      ((,class ())))
     ;;`(web-mode-underline-face                 ((,class ())))
     ;;`(web-mode-variable-name-face             ((,class ())))
     ;;`(web-mode-warning-face                   ((,class ())))
     ;;`(web-mode-whitespace-face                ((,class ())))

       `(which-func ((,class (:weight bold :slant italic :foreground ,sf-white))))
       ;; `(which-key-command-description-face)
       ;; `(which-key-group-description-face)
       ;; `(which-key-highlighted-command-face)
       ;; `(which-key-key-face)
       `(which-key-local-map-description-face ((,class (:weight bold :background "#f3f7fc" :inherit which-key-command-description-face))))
       ;; `(which-key-note-face)
       ;; `(which-key-separator-face)
       ;; `(which-key-special-key-face)
       `(widget-button ((,class ,link)))
       `(widget-button-pressed ((,class (:foreground "red"))))
       `(widget-documentation ((,class (:foreground "green4"))))
       `(widget-field ((,class (:background "gray85"))))
       `(widget-inactive ((,class (:foreground "dim gray"))))
       `(widget-single-line-field ((,class (:background "gray85"))))
       `(woman-bold ((,class (:weight bold :foreground "#f13d3d"))))
       `(woman-italic ((,class (:weight bold :slant italic :foreground "#46be1b"))))
       `(woman-symbol ((,class (:weight bold :foreground "purple"))))
       `(yas-field-debug-face ((,class (:foreground ,sf-white :background "#a62154"))))
       `(yas-field-highlight-face ((,class (:box (:line-width 1 :color "#838383") :foreground ,sf-black :background "#d4dcd8"))))

       ;; `(ztreep-arrow-face ((,class ())))
       ;; `(ztreep-diff-header-face ((,class ())))
       ;; `(ztreep-diff-header-small-face ((,class ())))
       `(ztreep-diff-model-add-face ((,class (:weight bold :foreground "#008800"))))
       `(ztreep-diff-model-diff-face ((,class (:weight bold :foreground "#0044dd"))))
       `(ztreep-diff-model-ignored-face ((,class (:strike-through t :foreground "#9e9e9e"))))
       `(ztreep-diff-model-normal-face ((,class (:foreground ,sf-black))))
       ;; `(ztreep-expand-sign-face ((,class ())))
       ;; `(ztreep-header-face ((,class ())))
       ;; `(ztreep-leaf-face ((,class ())))
       ;; `(ztreep-node-face ((,class ())))

       )))

(custom-theme-set-variables 'leuven-summerfruit

                            ;; highlight-sexp-mode.
                            '(hl-sexp-background-color "#efebe9")

                            '(ansi-color-faces-vector
                              [default default default italic underline success warning error])

                            ;; Colors used in Shell mode.
                            '(ansi-color-names-vector
                              ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
                            )

;;;###autoload
(when (and (boundp 'custom-theme-load-path)
           load-file-name)
  ;; Add theme folder to `custom-theme-load-path' when installing over MELPA.
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;;;###autoload
(when (string-match "/etc/themes/$"
                    (file-name-directory (or load-file-name (buffer-file-name))))
  (message "To stay up-to-date, you should better install and use leuven-summerfruit-theme from MELPA.")
  (sit-for 2))

(provide-theme 'leuven-summerfruit)

;; This is for the sake of Emacs.
;; Local Variables:
;; no-byte-compile: t
;; time-stamp-end: "$"
;; time-stamp-format: "%:y%02m%02d.%02H%02M"
;; time-stamp-start: "Version: "
;; End:
