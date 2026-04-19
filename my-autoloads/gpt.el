;; -*- lexical-binding: t; -*-

(defun openai (prompt &optional model)
  "Query the OpenAI API with PROMPT, returning the response as a string.

Optionally select a MODEL"
  (let* ((model (or model "gpt-4o-mini"))
         (script (format "import sys
from openai import OpenAI
client = OpenAI()
resp = client.responses.create(model=%S, input=sys.stdin.read())
print(resp.output_text)" model))
         (out-buf (generate-new-buffer " *openai-output*"))
         (output (unwind-protect
                     (progn
                       (with-temp-buffer
                         (insert prompt)
                         (call-process-region (point-min) (point-max)
                                              "/usr/bin/python3" nil out-buf nil
                                              "-c" script))
                       (with-current-buffer out-buf (buffer-string)))
                   (kill-buffer out-buf)))
         (output (string-trim-right output "\n")))
    (when (or (not output) (string-empty-p output))
      (user-error "No response from OpenAI API"))
    output))

(defun gpt--process-response (response &optional buffer-name buffer-point)
  "Process the RESPONSE from the GPT API and display it.

If BUFFER-NAME is provided,insert the response at BUFFER-POINT in that
buffer. Otherwise, open or create a buffer named \"*gpt*\" in Org mode
and display the response there."
  (save-excursion
    (when response
      (if buffer-name
          (with-current-buffer buffer-name
            (goto-char buffer-point) (insert (format "\n%s\n" response)))
        (progn
          (pop-to-buffer (get-buffer-create "*gpt*")) (org-mode)
          (goto-char (point-min)) (insert "* GPT Response\n")
          (insert "** Reponse\n") (insert "#+begin_src markdown\n")
          (insert response) (insert "\n#+end_src\n") (newline)
          (goto-char (point-min)))))))

(defun gpt--execute-prompt (prompt &optional buffer-name buffer-point no-region)
  "Execute a GPT-based prompt with or without a selection.

PROMPT is a string representing the initial prompt for the GPT
model. If a region is active in the current buffer, the selected
text is included in the query to the GPT model, formatted as an
extension of the prompt.

Output will go to BUFFER-NAME."

  (let ((selection nil))
    (when (and (not no-region) (region-active-p))
      (setq selection (buffer-substring-no-properties
                       (region-beginning)
                       (region-end))))
    (let* ((query (if selection (format "%s .\\n\\n%s" prompt selection) prompt))
           (response (openai query)))
      (gpt--process-response response
                             buffer-name
                             buffer-point))))

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

;;;###autoload
(defun gpt-get-docstring ()
  "Insert/overwrite the docstring for the current defun."
  (interactive)
  (save-excursion
    (if-let* ((bounds (cond ((derived-mode-p 'verilog-mode 'verilog-ts-mode)
                             (save-excursion
                               (let ((start (progn (verilog-beg-of-defun) (point)))
                                     (end   (progn (verilog-end-of-defun) (point))))
                                 (cons start end))))
                            (t (bounds-of-thing-at-point 'defun))))
              (beg (car bounds)) (end (cdr bounds))
              (form-str (buffer-substring-no-properties beg end))
              (prompt (string-join
                       (list
                        (format "Please write a concise docstring for this function being developed in %s." major-mode)
                        (format "The docstring should follow standard %s conventions." major-mode)
                        "No markdown. No added commentary. No code fences. Just a bare docstring. No outer quotes."
                        ;; (when (equal major-mode 'emacs-lisp-mode)
                        ;;   "Use \`..\' for single quotes. First line should be less than 80 characters. Insert a blank between the first and second lines. The code is: \n"
                        ;;   )
                        form-str
                        ) " "))
              (doc (openai prompt)))
        (progn
          (message doc)
          (kill-new doc))
      (error "Failed to get function definition"))))

(provide 'gpt)
