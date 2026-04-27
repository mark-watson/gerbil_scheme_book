;;; example_tools.ss — Tool definitions for Ollama tool/function calling
;;; Ported from the Common Lisp ollama-tools.lisp example.
;;;
;;; Each tool is a hash table with keys: name, description, parameters, handler.
;;; The handler is a Gerbil procedure that takes an arguments hash table
;;; and returns a string result.

(import :std/text/json)
(export make-tool-registry
        register-tool!
        tool-registry-lookup
        tool-registry-schemas
        get-weather-handler
        calculate-handler
        default-tool-registry)

;;; ----------------------------------------------------------------
;;; Tool registry — a hash table mapping tool name -> tool record
;;; ----------------------------------------------------------------

(def (make-tool-registry)
  "Create an empty tool registry (hash table)."
  (make-hash-table))

(def (register-tool! registry name description parameters handler)
  "Register a tool in the registry.
   NAME: string — the function name the LLM will use.
   DESCRIPTION: string — human-readable description.
   PARAMETERS: hash table — JSON-schema-style parameter spec.
   HANDLER: procedure — (lambda (args-hash) ...) -> string."
  (hash-put! registry name
    (list->hash-table
      `(("name" . ,name)
        ("description" . ,description)
        ("parameters" . ,parameters)
        ("handler" . ,handler)))))

(def (tool-registry-lookup registry name)
  "Look up a tool record by name.  Returns #f if not found."
  (hash-get registry name))

(def (tool-registry-schemas registry . tool-names)
  "Return a list of tool schemas (suitable for the Ollama API 'tools' field).
   If TOOL-NAMES is empty, return schemas for all registered tools.
   Each schema is a hash table with 'type' and 'function' keys."
  (let ((names (if (null? tool-names)
                   (hash-keys registry)
                   tool-names)))
    (map (lambda (n)
           (let ((tool (tool-registry-lookup registry n)))
             (when tool
               (list->hash-table
                 `(("type" . "function")
                   ("function" .
                    ,(list->hash-table
                       `(("name" . ,(hash-ref tool "name"))
                         ("description" . ,(hash-ref tool "description"))
                         ("parameters" . ,(hash-ref tool "parameters"))))))))))
         names)))

;;; ----------------------------------------------------------------
;;; Example tool handlers
;;; ----------------------------------------------------------------

(def (get-weather-handler args)
  "Handler for the get_weather tool.
   ARGS is a hash table; expects key 'location'."
  (let ((location (or (hash-get args 'location) "Unknown")))
    (string-append "Weather in " (if (string? location) location "Unknown")
                   ": Sunny, 72°F")))

(def (calculate-handler args)
  "Handler for the calculate tool.
   ARGS is a hash table; expects key 'expression'."
  (let ((expression (hash-get args 'expression)))
    (if expression
        (with-catch
          (lambda (e) (string-append "Error calculating: "
                        (with-output-to-string (lambda () (display-exception e)))))
          (lambda () (let ((result (eval (read (open-input-string expression)))))
                       (string-append "Result: " (number->string result)))))
        "No expression provided")))

;;; ----------------------------------------------------------------
;;; Pre-built default registry with get_weather and calculate
;;; ----------------------------------------------------------------

(def default-tool-registry
  (let ((reg (make-tool-registry)))

    ;; get_weather
    (register-tool! reg
      "get_weather"
      "Get current weather for a location"
      (list->hash-table
        `(("type" . "object")
          ("properties" .
           ,(list->hash-table
              `(("location" .
                 ,(list->hash-table
                    `(("type" . "string")
                      ("description" . "The city name")))))))
          ("required" . ("location"))))
      get-weather-handler)

    ;; calculate
    (register-tool! reg
      "calculate"
      "Perform a mathematical calculation"
      (list->hash-table
        `(("type" . "object")
          ("properties" .
           ,(list->hash-table
              `(("expression" .
                 ,(list->hash-table
                    `(("type" . "string")
                      ("description" . "Math expression like (+ 2 2)")))))))
          ("required" . ("expression"))))
      calculate-handler)

    reg))
