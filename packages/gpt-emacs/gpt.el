;;; gpt.el --- Functions for interacting with GPT-3 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Andrew Peck

;; Author: Andrew Peck <andrew.peck@cern.ch>
;; Version: 0.0.0
;; Url: https://github.com/andrewpeck/doom.d
;; Package-Requires: ((emacs "27.1") (plz "0.4-pre"))
;; Keywords: gpt3 openai
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>

;;; Commentary:
;;
;; This extension allows the use of the GPT-3 API through Emacs

;;; Code:

(require 'plz)
(require 'json)

(defvar gpt-api-key "sk-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" "A key for openai.")
(defvar gpt-cost-per-token (/ 2.0 1000.0) "Cost for single GPT token.")
(defvar gpt-text-model "gpt-3.5-turbo" "Model used by gpt.")
(defvar gpt-max-tokens 256 "Max number of tokens.")
(defvar gpt-temperature 0.7 "GPT3 Temperature.")
(defvar gpt-top-p 1 "GPT3 Top_p. Distribution of probably of common tokens.")
;; (defvar gpt-api-role "You are a helpful coding and writing assistant." "GPT3 Role")

(defun gpt-api-call (prompt)
  "Posts a GPT api request based on the PROMPT.
Returns a list decoded from the JSON reponse."

  (let* ((json-array-type 'list)
         (post-body
          (json-encode (list
                        (cons "model" gpt-text-model)
                        (cons "max_tokens" gpt-max-tokens)
                        (cons "temperature" gpt-temperature)
                        (cons "messages"
                              (list (list (cons "role" "user")
                                          (cons "content" prompt))))
                        (cons "top_p" gpt-top-p)
                        (cons "frequency_penalty" 0)
                        (cons "presence_penalty" 0))))
         (post-headers `(("Content-Type" . "application/json")
                         ("Authorization" . ,(concat  "Bearer " gpt-api-key))))
         (post (plz 'post
                 "https://api.openai.com/v1/chat/completions"
                 :headers post-headers :body post-body
                 :as #'json-read :then 'sync))) post))

;;;###autoload
(defun gpt-prompt ()
  "Use GPT-3 on the active selection, with an optional prompt.

It takes an optional prompt string from the user, and if no
prompt is given it will use the selected text as the prompt
instead. It then calls the GPT-3 API with the prompt and the
selected text, and retrieves the response as a JSON object. The
response is parsed to extract the resulting text and the usage
cost in tokens. The resulting text is then inserted into the
buffer, and the usage cost is displayed in a message."
  (interactive)
  (let ((selection ""))

    (when (region-active-p)
      (setq selection (buffer-substring-no-properties
                       (region-beginning)
                       (region-end))))
    (let ((prompt (read-string "Command: ")))

      ;; if we have a selection but not a prompt just switch them
      (when (and selection (string-empty-p prompt))
        (setq prompt selection))

      (when (not (string-empty-p prompt))
        (let* ((prompt-text (format "%s .\\n\\n%s" prompt selection))
               (rx-json (gpt-api-call prompt-text))
               (text (string-trim
                      (cdr (assoc 'content (cdaadr (assoc 'choices rx-json))))))
               (usage (cdr (assoc 'total_tokens (assoc 'usage rx-json)))))

          (print selection)
          (progn
            (when (region-active-p)
              (deactivate-mark)
              (forward-line -1))

            (end-of-line)
            (insert "\n"))
          (insert text)
          (beginning-of-line)
          (message (format "Usage: %d tokens (%f cents)" usage
                           (* usage gpt-cost-per-token))))))))

(provide 'gpt)
;;; gpt.el ends here
