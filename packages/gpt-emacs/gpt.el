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
;; This extension allows the use of the OpenAI through Emacs

;;; Code:

(require 'plz)
(require 'json)
(require 'dash)

(defvar gpt-api-key "sk-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" "A key for openai.")
(defvar gpt-cost-per-token (/ 2.0 1000.0) "Cost for single GPT token.")
(defvar gpt-text-model "gpt-4-1106-preview" "Model used by gpt.")

(defvar gpt-max-tokens 1024 "Max number of tokens.")
(defvar gpt-temperature 0.7 "GPT Temperature.")
(defvar gpt-top-p 1 "GPT Top_p. Distribution of probably of common tokens.")

(defun gpt--api-call (prompt callback)
  "Posts a GPT api request based on the PROMPT, and execute CALLBACK on it.

The callback will be executed asynchronously."

  (let* ((json-array-type 'list)
         (callback (or callback 'sync))

         (post-body
          (json-encode (list (cons "model" gpt-text-model)
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

         (response (plz 'post "https://api.openai.com/v1/chat/completions"
                     :headers post-headers
                     :body post-body
                     :as #'json-read
                     :then (lambda (alist)
                             ;; (message (format  "GPT Response: %s" alist))
                             (funcall callback alist)))))

    ;; (message (format  "GPT Posting: %s %s" post-headers post-body))
    ;; (when response )

    ;; return the response
    response))


(defun gpt--extract-message (response)
  "Extract the message from a GPT RESPONSE decoded json data structure."
  (when response
    (let ((msg (or (cdr  (assoc 'content (assoc 'message (aref  (cdr (assoc 'choices response)) 0))))
                        (cdr (assoc 'content (-filter 'consp (-flatten response)))))))
      (unless msg
        (error (format "No reponse from GPT query! %s" response))) msg)))

(defun gpt--process-response (json &optional prompt buffer-name buffer-point)
  ""

  (save-excursion
    (if (not json)
        (message "GPT API call failed!")

      (let ((response (gpt--extract-message json))
            (usage (cdr (assoc 'total_tokens (assoc 'usage json)))))

        (when response

          (if buffer-name

              (with-current-buffer buffer-name
                (goto-char buffer-point)
                (insert (format "\n%s\n" response)))

            (progn

              (doom/open-scratch-buffer)
              (goto-char (point-min))
              (insert "* GPT Response\n")

              ;; (when prompt
              ;;   (insert "** Prompt\n")
              ;;   (insert "#+begin_src markdown\n")
              ;;   (insert prompt)
              ;;   (insert "\n#+end_src\n"))

              (insert "** Reponse\n")
              (insert "#+begin_src markdown\n")
              (insert response)
              (insert "\n#+end_src\n")

              (newline)
              (goto-char (point-min))))

          (message (format "Usage: %d tokens (%f cents)" usage
                           (* usage gpt-cost-per-token))))))))

(defun gpt--execute-prompt (prompt &optional buffer-name buffer-point)
  "Execute a GPT-based prompt with or without a selection.

PROMPT is a string representing the initial prompt for the GPT
model. If a region is active in the current buffer, the selected
text is included in the query to the GPT model, formatted as an
extension of the prompt."

  (let ((selection nil))
    (when (region-active-p)
      (setq selection (buffer-substring-no-properties
                       (region-beginning)
                       (region-end))))
    (let* ((query (if selection (format "%s .\\n\\n%s" prompt selection) prompt)))
      (gpt--api-call query (lambda (x) (gpt--process-response x query buffer-name buffer-point))))))

;;;###autoload
(defun gpt-prompt ()
  "Dear AI, I want to do something on the selected text."

  (interactive)
  (let ((prompt
         (or (read-string "Command: ")
          "You are a helpful coding assistant for an
 electronics and FPGA engineer. Please answer concisely. Please describe
 the following code or following the instructions.")))
    (gpt--execute-prompt prompt)))

;;;###autoload
(defun gpt-inline ()
  "Dear AI, do something and be concise."
  (interactive)
  (let ((prompt
         (concat "You are a helpful coding assistant for an
 electronics and FPGA engineer. Please answer only in code and keep it very concise. No explanation, only generate the requested code. Output only plain text. Do not output markdown"
                 (read-string "Command: "))))
    (gpt--execute-prompt prompt (buffer-name) (point))))

;;;###autoload
(defun gpt-debug ()
  "Dear AI, help me debug please!!"
  (interactive)
  (gpt--execute-prompt "There is a bug in the following code, please help me find it."))

;;;###autoload
(defun gpt-writing ()
  "Dear AI, improve my writing please!!"
  (interactive)
  (gpt--execute-prompt "Please improve the writing of this text. I am an electronics engineer and am likely writing technical documentation or a message to communicate technical information."))

;;;###autoload
(defun gpt-grammar ()
  "Dear AI, correct my grammar please!!"
  (interactive)
  (gpt--execute-prompt "Please correct the grammar and spelling of this text."))

;;;###autoload
(defun gpt-doc ()
  "Dear AI, write some documentation please!!"
  (interactive)
  (gpt--execute-prompt "Please write documentation for the following code."))

;;;###autoload
(defun gpt-docstring ()
  "Dear AI, write a docstring please!!"
  (interactive)
  (gpt--execute-prompt "Please write a docstring for the following code. Please make the docstring brief."))

;;;###autoload
(defun gpt-suggest ()
  "Dear AI, make suggestions please!!"
  (interactive)
  (gpt--execute-prompt "Please make suggestions on how to improve the following code. I don't want general coding guidelines. I want specific suggestions on this code and how it can be made simpler, or better performing. If you don't have any good suggestions please just say so. Do not rewrite the code, just provide suggestions."))

;;;###autoload
(defun gpt-improve ()
  "Dear AI, improve please!!"
  (interactive)
  (gpt--execute-prompt "Please improve the following code and describe what you changed. Answer formatted in markdown."))

;;;###autoload
(defun gpt-help ()
  "Dear AI, help please!!"
  (interactive)
  (gpt--execute-prompt "I don't understand the following code. Can you please give me a basic explanation?"))

;;;###autoload
(defun gpt-refactor ()
  "Dear AI, refactor please!!"
  (interactive)
  (gpt--execute-prompt "Please refactor the following code and describe what you changed. Answer formatted in in markdown."))

(provide 'gpt)
;;; gpt.el ends here
