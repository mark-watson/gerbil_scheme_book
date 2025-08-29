
(import :groq/groq_inference)

;; Use OpenAI's open source model gpt-oss-120b

; Export the `gpt-oss-120b` procedure from this module
(export gpt-oss-120b)

(def (gpt-oss-120b
      prompt
      model: (model "openai/gpt-oss-120b")
      system-prompt: (system-prompt "You are a helpful assistant."))
  (groq_inference model prompt system-prompt: system-prompt))

;; (gpt-oss-120b "why is the sky blue? be very concise")
