;; (use-package! centaur-tabs
;;   :config
;;   (centaur-tabs-mode t)
;;   ;("C-S-tab" . centaur-tabs-backward)
;;   ;("C-tab" . centaur-tabs-forward)
;;  )

;;(setq doom-font                (font-spec :family "Source Code Pro" :size 13 :weight 'regular)
;;      doom-variable-pitch-font (font-spec :family "sans"            :size 13
;;))

; enable syntax highlighting for vimrc files
    (add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq tramp-default-method "ssh")

(modify-syntax-entry ?_ "w")

(remove-hook 'text-mode-hook #'auto-fill-mode)
  (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
  (add-hook 'text-mode-hook #'visual-line-mode)
(setq-default fill-column 120)

  ;; Make evil-mode up/down operate in screen lines instead of logical lines
  (define-key evil-motion-state-map "j" 'evil-next-visual-line)
  (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
  ;; Also in visual mode
  (define-key evil-visual-state-map "j" 'evil-next-visual-line)
  (define-key evil-visual-state-map "k" 'evil-previous-visual-line)

(setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))

(add-to-list 'auto-mode-alist '("\\.xdc\\'" . tcl-mode))

(setq projectile-sort-order 'recently-active)

(menu-bar-mode 1)

(after! company
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0.2 company-minimum-prefix-length 3)
  (set-company-backend! '(company-tabnine company-yasnippet  company-files))
  ;(set-company-backend! 'org-mode '(company-roam company-files company-dabbrev))
  ;;(set-company-backend! '(tcl-mode) '(company-tabnine company-yasnippet))
  (set-company-backend! '(vhdl-mode) '(company-tabnine company-yasnippet))
  )

(map! :leader "t t" 'doom/ivy-tasks)

(defun peck-dashboard-widget-banner ()
  (let ((point (point)))
    (mapc (lambda (line)
            (insert (propertize (+doom-dashboard--center +doom-dashboard--width line)
                                'face 'doom-dashboard-banner) " ")
            (insert "\n"))
          '(" E M A C S "))
    (when (and (display-graphic-p)
               (stringp fancy-splash-image)
               (file-readable-p fancy-splash-image))
      (let ((image (create-image (fancy-splash-image-file))))
        (add-text-properties
         point (point) `(display ,image rear-nonsticky (display)))
        (save-excursion
          (goto-char point)
          (insert (make-string
                   (truncate
                    (max 0 (+ 1 (/ (- +doom-dashboard--width
                                      (car (image-size image nil)))
                                   2))))
                   ? ))))
      (insert (make-string (or (cdr +doom-dashboard-banner-padding) 0)
                           ?\n)))))

(setq +doom-dashboard-functions
      '(peck-dashboard-widget-banner
        doom-dashboard-widget-shortmenu
        doom-dashboard-widget-loaded
        doom-dashboard-widget-footer))

(add-to-list 'load-path "~/.doom.d/themes/")
(add-to-list 'custom-theme-load-path "~/.doom.d/themes/")
(setq doom-theme 'leuven-summerfruit)

; (after! solarized-emacs
;   (custom-theme-set-faces
;    'solarized-light
;    '(font-lock-comment-face ((t (:background "#FFFFFF"))))
;    '(default ((t (:background "#FFFFFF"))))
;    )
;   )

(setq hl-line-mode nil)
(setq global-hl-line-mode nil)
(setq doom-buffer-hl-line-mode nil)

(defun align-to-colon (begin end)
  "Align region to colon (:) signs"
  (interactive "r")
  (align-regexp begin end
                (rx (group (zero-or-more (syntax whitespace))) ":") 1 1 ))

(defun align-to-comma (begin end)
  "Align region to comma  signs"
  (interactive "r")
  (align-regexp begin end
                (rx "," (group (zero-or-more (syntax whitespace))) ) 1 1 ))


(defun align-to-equals (begin end)
  "Align region to equal signs"
  (interactive "r")
  (align-regexp begin end
                (rx (group (zero-or-more (syntax whitespace))) "=") 1 1 ))

(defun align-to-hash (begin end)
  "Align region to hash ( => ) signs"
  (interactive "r")
  (align-regexp begin end
                (rx (group (zero-or-more (syntax whitespace))) "=>") 1 1 ))

;; work with this
(defun align-to-comma-before (begin end)
  "Align region to equal signs"
  (interactive "r")
  (align-regexp begin end
                (rx (group (zero-or-more (syntax whitespace))) ",") 1 1 ))

(global-evil-leader-mode)

;; counsel fzf search
(defun searchdirs ()
  (interactive)
  (counsel-fzf "" "~/")
)

(define-key evil-normal-state-map (kbd "C-o") 'searchdirs)

(evil-leader/set-key "f" 'searchdirs)
;; leader x for helm execute
(evil-leader/set-key "x" 'counsel-M-x)

(evil-leader/set-key "pp" '+ivy/project-search)

;; (defun fzf-recentf ()
;;   (interactive)
;;   (fzf-with-entries recentf-list
;;     (lambda (f) (when (file-exists-p f) (find-file f))))
;; )

;; (evil-leader/set-key "rr" 'fzf-recentf)
 (evil-leader/set-key "rr" 'projectile-recentf)

;; delete trailing whitespace
(evil-leader/set-key "rtw" 'delete-trailing-whitespace)

;; ;;  helm-projectile
;; (evil-leader/set-key "c" 'helm-projectile-ag)
;;
;; ;;  helm-locate
;; (setq helm-locate-fuzzy-match nil
;;       helm-locate-command "mdfind -interpret -name %s %s")
;; (evil-leader/set-key "f" 'helm-locate)

(evil-leader/set-key "g" 'magit-status)

;; map helm mini to leader
;;(evil-leader/set-key "b" 'helm-mini)

;; map dired to leader
(evil-leader/set-key "e" 'dired)

(evil-leader/set-key "pp" 'org-publish-current-project)

;; (evil-leader/set-key "tag" 'projectile-regenerate-tags)

(after! evil-maps
    ;; Ctrl-P
    (define-key evil-normal-state-map "\C-p" nil)
    (bind-key* "C-p" 'projectile-find-file)

    ;; let enter open org mode links
    (define-key evil-motion-state-map (kbd "SPC") nil)
    (define-key evil-motion-state-map (kbd "RET") 'org-open-at-point)
    (define-key evil-motion-state-map (kbd "TAB") nil)
    (setq org-return-follows-links t)

    ;; Backspace to switch to last buffer
    (defun er-switch-to-previous-buffer ()
        "Switch to previously open buffer. Repeated invocations toggle between the two most recently open buffers."
        (interactive)
        (switch-to-buffer (other-buffer (current-buffer) 1)))
    (define-key evil-normal-state-map (kbd "DEL") 'er-switch-to-previous-buffer)


    (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
    (define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)
    (define-key evil-visual-state-map (kbd "C-a") 'evil-numbers/inc-at-pt-incremental)
    (define-key evil-visual-state-map (kbd "C-x") 'evil-numbers/dec-at-pt-incremental)
)

;; vhdl mode will wrap comments after some # of characters
(setq vhdl-end-comment-column 200)
(setq vhdl-prompt-for-comments nil)
;;(add-hook 'vhdl-mode-hook
;;(setq auto-fill-mode nil)
;;)

(setq org-directory "~/Dropbox/notes")
(setq org-default-notes-file (concat org-directory "/notes.org"))

; https://github.com/sk8ingdom/.emacs.d/blob/master/org-mode-config/org-capture-templates.el
(after! org
  (add-to-list 'org-capture-templates
               '("s" "Shopping" item (file+headline +org-capture-todo-file "Shopping")
               "- [ ] %?" :prepend t))
  (setq org-startup-folded 'f)
  (setq org-startup-with-inline-images t)
  (org-display-inline-images t t)
  )

; Allow M-Ret to split list items
(setq org-M-RET-may-split-line t)

;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)

(use-package! org-download
  :config
  (setq-default org-download-method            'directory
                org-download-screenshot-method "gnome-screenshot -a -f %s"
                org-download-image-dir         "./images/downloads"
                org-download-heading-lvl       0
                ;;org-download-link-format       "[[file:%s]]"
                ;;org-download-image-attr-list   ("#+attr_org: :width 800px")
                org-download-annotate-function (lambda (link) "")
                org-download-image-org-width   800
                )
)

(after! org-attach-screenshot
  (setq org-attach-screenshot-command-line "gnome-screenshot -a -c -f %f")
  )

(after! org
  (setq org-list-allow-alphabetical t)
  (setq org-publish-project-alist
        '(
          ;; ... add all the components here (see below)...
          ("org-notes"
           :base-directory "~/Dropbox/notes/"
           :base-extension "org"
           :publishing-directory "~/notes_html/"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4  ; Just the default for this project.
           :auto-preamble t
           )

          ("org-static"
           :base-directory "~/Dropbox/notes/"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
           :publishing-directory "~/notes_html/"
           :recursive t
           :publishing-function org-publish-attachment
           )

          ("org" :components ("org-notes" "org-static"))

          )
        )
  )

(use-package! org-roam

  :commands
  (org-roam-insert org-roam-find-file org-roam-switch-to-buffer org-roam)

  :hook
  (after-init . org-roam-mode)

  :init

  (setq org-roam-directory "~/Dropbox/notes/")
  (map! :leader
        :prefix "n"
        :desc "Org-Roam-Insert" "i" #'org-roam-insert
        :desc "Org-Roam-Find"   "/" #'org-roam-find-file
        :desc "Org-Roam-Buffer" "r" #'org-roam
        :desc "Org-Roam-Show-Graph" "g" #'org-roam-show-graph
        )

  (set-company-backend! 'org-mode
    '(company-org-roam :with company-dabbrev :with company-yasnippet))

                                        ;(setq org-roam-link-title-format "Org:%s")
  (setq org-roam-db-location "~/Dropbox/notes/org-roam.db")
  (setq org-roam-backlinks-mode-hook
        '(
          (flyspell-mode)
          (define-key evil-motion-state-map (kbd "RET") 'org-roam-open-at-point)
          )
        )

  (setq org-roam-completion-system 'ivy)

  :config

  (require 'org-roam-protocol)

  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "${title}"
           :head "#+TITLE: ${title}\n#+SETUPFILE: \"setup.org\"\n"
           :unnarrowed t))
        )
  )
