# Command Line Application For NLP

We will use both my NLP library from a prior chapter as well as the library for using OpenAI's GPT-5 model to write two command line utilities.

## Re-using the NLP Library For a Command Line Utility to Identify Categories or Topics in Input Text


TBD

The code for this example can be used interpretively using **gxi** or compiled into a static application.

Listing for **categpories.ss**:

```scheme
#!/usr/bin/env gxi

(import :nlp/main
        :nlp/nlp
        :std/iter
        :std/misc/ports)  ;; read-all-as-string

(export main)

(def get-categories (lambda (x) (map car (cadddr x))))

(def (remove-txt str substr)
  (if (string-suffix? substr str)
      (substring str 0 (- (string-length str) (string-length substr)))
      str))

(def (main . args)
  (let* ((input-str (if (null? args)
                        (read-all-as-string (current-input-port)) ; piped stdin
                        (string-join args " ")))                  ; argv
         (response (nlp/main#process-string input-str)))
    (for (s (get-categories response))
      (displayln (remove-txt s ".txt"))))
)
```

This command line utility can either use all command line arguments, joined as a string with interleaving spaces, or text can be piped into the utility via **stdin**:


```console
$ gxc -O -exe -o categories categories.ss                             
/tmp/gxc.1757624261.449784/nlp__utils.scm:
/tmp/gxc.1757624261.449784/nlp__fasttag.scm:
/tmp/gxc.1757624261.449784/nlp__proper-names.scm:
/tmp/gxc.1757624261.449784/nlp__category.scm:
/tmp/gxc.1757624261.449784/nlp__main.scm:
/tmp/gxc.1757624261.449784/nlp__nlp.scm:
/tmp/gxc.1757624261.449784/categories.scm:
/Users/markw/GITHUB/gerbil_scheme_book/source_code/command_line_NLP/categories__exe.scm:
/tmp/gxc.1757624261.449784/nlp__utils.c:
/tmp/gxc.1757624261.449784/nlp__fasttag.c:
/tmp/gxc.1757624261.449784/nlp__proper-names.c:
/tmp/gxc.1757624261.449784/nlp__category.c:
/tmp/gxc.1757624261.449784/nlp__main.c:
/tmp/gxc.1757624261.449784/nlp__nlp.c:
/tmp/gxc.1757624261.449784/categories.c:
/Users/markw/GITHUB/gerbil_scheme_book/source_code/command_line_NLP/categories__exe.c:
/Users/markw/GITHUB/gerbil_scheme_book/source_code/command_line_NLP/categories__exe_.c:
Marks-Mac-mini:command_line_NLP $ ./categories health exercise vitamines 
health_exercise
health_nutrition
Marks-Mac-mini:command_line_NLP $ ./categories.ss health exercise vitamines
health_exercise
health_nutrition
Marks-Mac-mini:command_line_NLP $ cat ~/temp/Clojure-AI-Book-Code/docs_qa/data/health.txt | ./categories.ss
computers_ai_search
health
Marks-Mac-mini:command_line_NLP $ cat ~/temp/Clojure-AI-Book-Code/docs_qa/data/health.txt | ./categories   
computers_ai_search
health
```


## Using OpenAI's GPT-5 model To Summarize Input Text

TBD
