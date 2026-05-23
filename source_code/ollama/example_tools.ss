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

(def (valid-math-string? str)
  "Return #t if the string contains only numbers, math operators (+, -, *, /), parentheses, dots, and whitespace."
  (let ((len (string-length str)))
    (let loop ((i 0))
      (if (= i len)
          #t
          (let ((c (string-ref str i)))
            (if (or (char-numeric? c)
                    (member c '(#\+ #\- #\* #\/ #\( #\) #\. #\space #\tab #\newline #\return)))
                (loop (+ i 1))
                #f))))))

(def (eval-infix-pass1 tokens)
  "Evaluates all * and / operations left-to-right to enforce precedence."
  (let loop ((in tokens) (out '()))
    (cond
     ((null? in) (reverse out))
     ((and (not (null? (cdr in)))
           (member (cadr in) '(* /)))
      (let* ((a (car in))
             (op (cadr in))
             (b (caddr in))
             (rest (cdddr in))
             (res (case op
                    ((*) (* a b))
                    ((/) (/ a b)))))
        (loop (cons res rest) out)))
     (else
      (loop (cdr in) (cons (car in) out))))))

(def (eval-infix-pass2 tokens)
  "Evaluates all + and - operations left-associatively."
  (if (null? tokens)
      0
      (let loop ((val (car tokens)) (rest (cdr tokens)))
        (cond
         ((null? rest) val)
         (else
          (let ((op (car rest))
                (b (cadr rest)))
            (case op
              ((+) (loop (+ val b) (cddr rest)))
              ((-) (loop (- val b) (cddr rest)))
              (else (error "Unsupported operator in pass 2" op)))))))))

(def (eval-infix tokens)
  "Evaluate an infix list of resolved tokens."
  (eval-infix-pass2 (eval-infix-pass1 tokens)))

(def (resolve-sub-expressions tokens)
  "Map over tokens and recursively evaluate any nested lists."
  (map (lambda (tok)
         (if (list? tok)
             (eval-infix-or-prefix tok)
             tok))
       tokens))

(def (eval-infix-or-prefix expr)
  "Evaluate an S-expression structure which could be prefix or infix."
  (cond
   ((number? expr) expr)
   ((and (list? expr) (not (null? expr)))
    (if (member (car expr) '(+ - * /))
        (let ((args (map eval-infix-or-prefix (cdr expr))))
          (case (car expr)
            ((+) (apply + args))
            ((-) (apply - args))
            ((*) (apply * args))
            ((/) (apply / args))
            (else (error "Unsupported operator" (car expr)))))
        (eval-infix (resolve-sub-expressions expr))))
   (else (error "Unsupported expression type" expr))))

(def (read-all-tokens port)
  "Read all tokens from a port until EOF."
  (let loop ((acc '()))
    (let ((x (read port)))
      (if (eof-object? x)
          (reverse acc)
          (loop (cons x acc))))))

(def (calculate-handler args)
  "Handler for the calculate tool.
   ARGS is a hash table; expects key 'expression'."
  (let ((expression (hash-get args 'expression)))
    (cond
     ((not expression) "No expression provided")
     ((not (valid-math-string? expression)) "Error: Expression contains forbidden characters or operators")
     (else
      (with-catch
        (lambda (e) (string-append "Error calculating: "
                      (with-output-to-string (lambda () (display-exception e)))))
        (lambda ()
          (let* ((port (open-input-string expression))
                 (tokens (read-all-tokens port)))
            (cond
             ((null? tokens) "Result: (empty)")
             ((and (= (length tokens) 1) (not (list? (car tokens))))
              (let ((res (eval-infix-or-prefix (car tokens))))
                (string-append "Result: " (number->string res))))
             (else
              (let ((res (eval-infix (resolve-sub-expressions tokens))))
                (string-append "Result: " (number->string res))))))))))))


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
