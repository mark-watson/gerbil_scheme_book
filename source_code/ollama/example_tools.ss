;;; example_tools.ss — Tool definitions for Ollama tool/function calling
;;; Ported from the Common Lisp ollama-tools.lisp example.
;;;
;;; Each tool is a struct with fields: name, description, parameters, handler.
;;; The handler is a Gerbil procedure that takes an arguments hash table
;;; and returns a string result.

(import :std/text/json :std/srfi/13)
(export make-tool-registry
        register-tool!
        tool-registry-lookup
        tool-registry-schemas
        get-weather-handler
        calculate-handler
        default-tool-registry
        (struct-out tool))

;;; ----------------------------------------------------------------
;;; Tool record definition
;;; ----------------------------------------------------------------

(defstruct tool (name description parameters handler))

;;; ----------------------------------------------------------------
;;; Tool registry — a hash table mapping tool name -> tool struct
;;; ----------------------------------------------------------------

;; Create an empty tool registry (hash table).
(def (make-tool-registry)
  (make-hash-table))

;; Register a tool in the registry.
;; NAME: string — the function name the LLM will use.
;; DESCRIPTION: string — human-readable description.
;; PARAMETERS: hash table — JSON-schema-style parameter spec.
;; HANDLER: procedure — (lambda (args-hash) ...) -> string.
(def (register-tool! registry name description parameters handler)
  (hash-put! registry name
    (make-tool name description parameters handler)))

;; Look up a tool record by name. Returns #f if not found.
(def (tool-registry-lookup registry name)
  (hash-get registry name))

;; Return a list of tool schemas (suitable for the Ollama API 'tools' field).
;; If TOOL-NAMES is empty, return schemas for all registered tools.
;; Unknown tool names are filtered out instead of returning void.
(def (tool-registry-schemas registry . tool-names)
  (let ((names (if (null? tool-names)
                   (hash-keys registry)
                   tool-names)))
    (let loop ((ns names) (acc '()))
      (if (null? ns)
          (reverse acc)
          (let ((rec (tool-registry-lookup registry (car ns))))
            (if rec
                (let ((schema
                       (list->hash-table
                        `(("type" . "function")
                          ("function" .
                           ,(list->hash-table
                             `(("name" . ,(tool-name rec))
                               ("description" . ,(tool-description rec))
                               ("parameters" . ,(tool-parameters rec)))))))))
                  (loop (cdr ns) (cons schema acc)))
                (loop (cdr ns) acc)))))))

;;; ----------------------------------------------------------------
;;; Example tool handlers
;;; ----------------------------------------------------------------

;; Handler for the get_weather tool. ARGS is a hash table; expects key 'location'.
(def (get-weather-handler args)
  (let ((location (or (hash-get args 'location) "Unknown")))
    (string-append "Weather in " (if (string? location) location "Unknown")
                   ": Sunny, 72°F")))

;; Character-based tokenizer for math expressions.
;; Handles spaces, no-space operators ("42*17"), negative numbers and subtraction ("3 -2").
(def (tokenize-math str)
  (let ((chars (string->list str))
        (tokens '()))
    (let loop ((cs chars) (prev-tok #f))
      (cond
       ((null? cs) (reverse tokens))
       ((char-whitespace? (car cs))
        (loop (cdr cs) prev-tok))
       ((member (car cs) '(#\( #\) #\+ #\* #\/))
        (let ((tok (string->symbol (string (car cs)))))
          (set! tokens (cons tok tokens))
          (loop (cdr cs) tok)))
       ((char=? (car cs) #\-)
        (if (not (memq prev-tok '(#f + - * / |(|)))
            (let ((tok '-))
              (set! tokens (cons tok tokens))
              (loop (cdr cs) tok))
            (if (and (pair? (cdr cs)) (or (char-numeric? (cadr cs)) (char=? (cadr cs) #\.)))
                (let read-num ((rest (cdr cs)) (num-cs (list #\-)))
                  (if (and (pair? rest) (or (char-numeric? (car rest)) (char=? (car rest) #\.)))
                      (read-num (cdr rest) (cons (car rest) num-cs))
                      (let* ((num-str (list->string (reverse num-cs)))
                             (num (string->number num-str)))
                        (set! tokens (cons (or num 0) tokens))
                        (loop rest (or num 0)))))
                (let ((tok '-))
                  (set! tokens (cons tok tokens))
                  (loop (cdr cs) tok)))))
       ((or (char-numeric? (car cs)) (char=? (car cs) #\.))
        (let read-num ((rest cs) (num-cs '()))
          (if (and (pair? rest) (or (char-numeric? (car rest)) (char=? (car rest) #\.)))
              (read-num (cdr rest) (cons (car rest) num-cs))
              (let* ((num-str (list->string (reverse num-cs)))
                     (num (string->number num-str)))
                (set! tokens (cons (or num 0) tokens))
                (loop rest (or num 0))))))
       (else
        (error "Unexpected character in math expression" (car cs)))))))

;; Group parenthesized tokens into nested lists
(def (group-parens tokens)
  (let loop ((ts tokens) (stack (list '())))
    (cond
     ((null? ts)
      (if (= (length stack) 1)
          (reverse (car stack))
          (error "Mismatched parentheses: unclosed '('")))
     ((eq? (car ts) '|(|)
      (loop (cdr ts) (cons '() stack)))
     ((eq? (car ts) '|)|)
      (if (null? (cdr stack))
          (error "Mismatched parentheses: extra ')'")
          (let ((inner (reverse (car stack))))
            (loop (cdr ts) (cons (cons inner (cadr stack)) (cddr stack))))))
     (else
      (loop (cdr ts) (cons (cons (car ts) (car stack)) (cdr stack)))))))

;; Safe division returning inexact numbers when not whole
(def (safe-div a b)
  (if (zero? b)
      (error "Division by zero")
      (let ((res (/ a b)))
        (if (exact? res)
            (if (integer? res) res (exact->inexact res))
            res))))

;; Pass 1: left-to-right * and /
(def (eval-infix-pass1 tokens)
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
                    ((/) (safe-div a b)))))
        (loop (cons res rest) out)))
     (else
      (loop (cdr in) (cons (car in) out))))))

;; Pass 2: left-associative + and -
(def (eval-infix-pass2 tokens)
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
              (else (error "Unsupported operator in expression" op)))))))))

;; Evaluate an expression which may be prefix or infix
(def (eval-expr expr)
  (cond
   ((number? expr) expr)
   ((and (list? expr) (not (null? expr)))
    (if (member (car expr) '(+ - * /))
        (let ((args (map eval-expr (cdr expr))))
          (case (car expr)
            ((+) (apply + args))
            ((-) (if (= (length args) 1) (- (car args)) (apply - args)))
            ((*) (apply * args))
            ((/) (if (= (length args) 1)
                     (safe-div 1 (car args))
                     (foldl safe-div (car args) (cdr args))))))
        (let* ((resolved (map (lambda (x) (if (list? x) (eval-expr x) x)) expr))
               (resolved (if (and (pair? resolved) (eq? (car resolved) '-))
                             (cons 0 resolved)
                             resolved)))
          (if (= (length resolved) 1)
              (car resolved)
              (eval-infix-pass2 (eval-infix-pass1 resolved))))))
   (else (error "Unsupported expression" expr))))

;; Handler for the calculate tool. ARGS is a hash table; expects key 'expression'.
(def (calculate-handler args)
  (let ((expression (hash-get args 'expression)))
    (cond
     ((not expression) "No expression provided")
     (else
      (with-catch
        (lambda (e)
          (let ((p (open-output-string)))
            (display-exception e p)
            (string-append "Error calculating: " (get-output-string p))))
        (lambda ()
          (let* ((tokens (tokenize-math expression))
                 (tree (group-parens tokens)))
            (if (null? tree)
                "Result: (empty)"
                (let ((res (eval-expr tree)))
                  (string-append "Result: " (number->string res)))))))))))

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
                      ("description" . "Math expression like 42 * 17 or (+ 2 2)")))))))
          ("required" . ("expression"))))
      calculate-handler)

    reg))
