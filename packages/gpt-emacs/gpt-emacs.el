;;; -*- lexical-binding: t; -*-
;;; Based loosely on https://github.com/samrawal/gpt-emacs-macro
;;; Code:

(require 'plz)
(require 'json)

(defvar openai-api-key ""
  "A key for openai")

(defvar gpt-cost-per-token
  (/ 2.0 1000.0)
  "Cost for single GPT token")

(defun gpt-call (api-key prompt data)
  ""
  (let* ((json-array-type 'list)
         (post (plz 'post "https://api.openai.com/v1/completions"
                 :headers `(("Content-Type" . "application/json")
                            ("Authorization" . ,(concat  "Bearer " api-key)))
                 :body (json-encode `(("model" . "text-davinci-003")
                                      ("prompt" . ,(format "%s .\\n\\n%s" prompt data))
                                      ("temperature" . 0.7)
                                      ("max_tokens" . 256)
                                      ("top_p" . 1)
                                      ("frequency_penalty" . 0)
                                      ("presence_penalty" . 0)))
                 :as #'json-read
                 :then 'sync))) post))

(defun gpt-emacs ()
  "Use GPT-3 on the active selection, with an optional prompt."
  (interactive)
  (let* ((beg (region-beginning))
         (end (region-end))
         (selection
          (if (region-active-p)
              (buffer-substring-no-properties  beg end) ""))
         (prompt (read-string "Command: ")))

    ;; if we have a selection but not a prompt just switch them
    (when (and selection (string-empty-p prompt))
      (setq prompt selection))

    (when (not (string-empty-p prompt))
      (let* ((rx-json (gpt-call openai-api-key prompt selection))
             (text (string-trim (cdaadr (assoc 'choices rx-json))))
             (usage (cdr (assoc 'total_tokens (assoc 'usage rx-json)))))

        (print selection)
        (end-of-line)
        (open-line 1)
        (insert text)
        (message (format "Usage: %d tokens (%f cents)" usage (* usage gpt-cost-per-token)))))))

(provide 'gpt-emacs)
