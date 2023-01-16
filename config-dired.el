;;; -*- lexical-binding: t; -*-

;; Add git commit into into dired
;; (add-hook 'dired-after-readin-hook
;; 'dired-git-info-auto-enable)

;; better dired soring
(after! dired-x
  (setq dired-omit-extensions (remove ".bin" dired-omit-extensions))
  (setq dired-omit-extensions (remove ".bit" dired-omit-extensions))
  (setq dired-listing-switches "-a1vBhl  --group-directories-first"))

(after! diredfl
  (add-to-list 'diredfl-compressed-extensions ".zst"))

(after! dired-aux
  (setq dired-compress-file-default-suffix ".zst")
  (setq dired-compress-directory-default-suffix ".tar.zst"))
