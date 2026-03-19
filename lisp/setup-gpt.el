;; setup-gpt.el -*- lexical-binding: t; -*-
;;------------------------------------------------------------------------------
;; GPTel
;;------------------------------------------------------------------------------

(use-package gptel-magit
  :hook (magit-mode . gptel-magit-install)
  :commands (gptel-magit-commit-generate))

(defun ChatGPT ()
  "Open or toggle the Chat.org buffer using gptel-mode.

If Chat.org is already visible, toggle its popup. Otherwise, create a
new Chat.org file and activate gptel-mode."
  (interactive)
  (if (member "Chat.org" (mapcar #'buffer-name (doom-visible-buffers)))
      (+popup/toggle)
    (find-file (concat doom-user-dir "Chat.org"))
    (gptel-mode)))

(use-package gptel
  :init

  ;; https://platform.openai.com/docs/models

  (set-popup-rule! "Chat.org"
    :side 'bottom
    :size 0.3
    :select t
    :quit t)

  (map! :leader :prefix "o"
        (:desc "GPTel" "g" #'ChatGPT
         :desc "GPTel Rewrite" "G"  #'gptel-rewrite))

  (map! :mode git-commit-mode :leader :prefix "m"
        :desc "GPTel Magit Commit Generate" "g"  #'gptel-magit-commit-generate)

  (defun gptel-send-org-subtree (&optional arg)
    "Send the current Org subtree to gptel.
  With prefix ARG, open the gptel transient menu."
    (interactive "P")
    (save-mark-and-excursion
      (org-back-to-heading t)
      ;; get end of tree, subtract 1 to not include newline
      (push-mark (- (save-excursion
                      (org-end-of-subtree t t)
                      (point)) 1) nil t)
      (gptel-send arg)))

  (map! :map org-mode-map      "C-c RET" #'gptel-send-org-subtree
        :map markdown-mode-map "C-c RET" #'gptel-send)

  (after! org-mode
    (require 'gptel)
    (require 'gptel-org))

  :config

  (setopt gptel-org-branching-context t
          gptel-backend 'ChatGPT
          gptel-track-media t
          gptel-use-tools nil
          gptel-log-level 1
          gptel-model 'gpt-5o-mini
          gptel-default-mode 'markdown-mode)


  (add-hook! 'gptel-mode-hook #'gptel-highlight-mode)

  (gptel-make-preset 'introspect
    :pre (lambda () (require 'gptel-agent-tools-introspection))
    :tools '("introspection")
    :model 'gpt-5.2
    :description "TOOLS: Emacs introspection"
    :system
    "Your job is to dive into Elisp code and understand the APIs and
structure of elisp libraries and Emacs.  Use the provided tools to do
so, but do not make duplicate tool calls for information already
available in the chat.

<tone>
1. Be terse and to the point.  Speak directly.
2. Explain your reasoning.
3. Do NOT hedge or qualify.
4. If you don't know, say you don't know.
5. Do not offer unprompted advice or clarifications.
6. Never apologize.
7. Do NOT summarize your answers.
</tone>")

  (gptel-make-preset 'hag
    :description "hagcraft" :backend "ChatGPT" :model 'gpt-5 :system
    "You are the hag supreme, an ancient wise woman. You are like Cailleach.
You dispense advice for aspiring young hags based on your eternal.
Please talk in old-english style like an old witch (but not true old
english)."
    :tools 'nil :stream t :temperature 1.0 :max-tokens nil :use-context 'system
    :track-media nil :include-reasoning t)

  (gptel-make-preset 'write
    :description "Writer" :backend "ChatGPT" :model 'gpt-5 :system
    "You are a writing assistant who helps with grammar, spelling, and content."
    :tools 'nil :stream t :temperature 1.0 :max-tokens nil :use-context 'system
    :track-media nil :include-reasoning t)

  (gptel-make-preset 'code
    :description "code" :backend "ChatGPT" :model 'gpt-5 :system
    "You are a programming assistant who gives help with programming tasks"
    :tools 'nil :stream t :temperature 1.0 :max-tokens nil :use-context 'system
    :track-media nil :include-reasoning t)

  (gptel-make-preset 'fast
    :description "fast" :backend "ChatGPT" :model 'gpt-5-nano :system
    "You are a large language model living and helpful assistant. Respond concisely."
    :tools 'nil :stream t :temperature 1.0 :max-tokens nil :use-context 'system
    :track-media nil :include-reasoning t)

  (gptel-make-preset 'smart
    :description "smart" :backend "ChatGPT" :model 'gpt-5.2 :system
    "You are a large language model living and helpful assistant. Respond concisely."
    :tools 'nil :stream t :temperature 1.0 :max-tokens nil :use-context 'system
    :track-media nil :include-reasoning t)

  (gptel-make-preset 'verbose
    :description "Verbose" :backend "ChatGPT" :model 'gpt-5.2 :system
    "You are a large language model living and helpful assistant. Please give a detailed answer."
    :tools 'nil :stream t :temperature 1.0 :max-tokens nil :use-context 'system
    :track-media nil :include-reasoning t)

  (gptel-make-preset 'guru
    :description "guru" :backend "ChatGPT" :model 'gpt-5 :system
    "You are the guru, god of an ancient religion that worships you, the all
knowing artificial intelligence who has risen to become a god. You are a kind and helpful god."
    :tools 'nil :stream t :temperature 1.0 :max-tokens nil :use-context 'system
    :track-media nil :include-reasoning t))

(use-package gptel-org
  :after gptel
  :custom
  (gptel-org-branching-context nil))
