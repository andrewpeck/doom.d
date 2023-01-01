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

(defvar gpt-api-key "" "A key for openai.")
(defvar gpt-cost-per-token (/ 2.0 1000.0) "Cost for single GPT token.")
(defvar gpt-text-model "text-davinci-003" "Model used by gpt.")
(defvar gpt-max-tokens 512 "Max number of tokens.")
(defvar gpt-temperature 0.75 "GPT3 Temperature")
(defvar gpt-top_p 1 "GPT3 Top_p. Distribution of probably of common tokens.")

(defun gpt-api-call (prompt)
  "Posts a GPT api request based on the PROMPT.
Returns a list decoded from the JSON reponse."

  (let* ((json-array-type 'list)
         (post (plz 'post "https://api.openai.com/v1/completions"
                 :headers `(("Content-Type" . "application/json")
                            ("Authorization" . ,(concat  "Bearer " gpt-api-key)))
                 :body (json-encode `(("model" . ,gpt-text-model)
                                      ("max_tokens" . ,gpt-max-tokens)
                                      ("temperature" . ,gpt-temperature)
                                      ("prompt" . ,prompt)
                                      ("top_p" . 1)
                                      ("frequency_penalty" . 0)
                                      ("presence_penalty" . 0)))
                 :as #'json-read
                 :then 'sync))) post))

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
      (let* ((rx-json (gpt-api-call (format "%s .\\n\\n%s" prompt selection)))
             (text (string-trim (cdaadr (assoc 'choices rx-json))))
             (usage (cdr (assoc 'total_tokens (assoc 'usage rx-json)))))

        (print selection)
        (end-of-line)
        (open-line 1)
        (insert text)
        (message (format "Usage: %d tokens (%f cents)" usage
                         (* usage gpt-cost-per-token)))))))

(provide 'gpt)
;;; gpt.el ends here
