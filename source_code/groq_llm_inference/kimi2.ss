
(import :groq/groq_inference)

;; Use Moonshot AI's best kimi2 model (MOE: 1 triliion parameters, 32B resident).

; Export the `kimi2` procedure from this module
(export kimi2)

(def (kimi2 prompt
            model: (model "moonshotai/kimi-k2-instruct")
            system-prompt: (system-prompt "You are a helpful assistant."))
  (groq_inference model prompt system-prompt: system-prompt))

;; (kimi2 "why is the sky blue? be very concise")
