# Command Line Application For NLP

We will use both my NLP library from a prior chapter as well as the library for using OpenAI's GPT-5 model to write two command line utilities.

## Re-using the NLP Library For a Command Line Utility to Identify Categories or Topics in Input Text

This program is a command-line utility written in Gerbil Scheme that acts as a wrapper for a Natural Language Processing (NLP) library (that we saw in an earlier chapter) to perform text classification. The script is designed to be highly versatile, capable of accepting text input either directly from command-line arguments or from a piped stream via standard input, making it a useful component in a shell-based data processing pipeline. Its core purpose is to take a block of text, send it to the NLP engine for analysis, and then extract a list of classification categories from the resulting data structure. After extracting these categories, it performs a minor cleanup operation on each one—removing a trailing .txt file extension—before printing each clean category name on a new line to the console.

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

This command line utility can either use all command line arguments, joined as a string with interleaving spaces, or text can be piped into the utility via **stdin**.

The core logic resides within the main function which first determines the source of the input text. The (if (null? args) ...) expression is a standard idiom for a command-line tool, allowing it to function seamlessly whether it's receiving data through a pipe or as direct arguments. After capturing the input, the program makes a single call to the external **nlp/main#process-string** function, which handles the complex task of NLP analysis. The program concludes by iterating through the results with a **for** loop, processing each identified category and printing it to standard output using displayln.

Two small helper functions provide insight into the data the script is handling. The **get-categories** function uses a combination of cadddr and map car, which tells us that it expects a very specific nested list structure from the NLP library and is designed to extract a list of names from it. This tight coupling to the library's output format is common in wrapper scripts. The second helper function **remove-txt** is a simple string manipulation function whose existence is required because the raw category data from the model includes file extensions that are not desired in the final output.

Here we build an executable and run both the compiled and linked executable and also run interpretivey using **gxi**. We also take input from the command line or via **stdin**:

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
Marks-Mac-mini:command_line_NLP $ cat ~/GITHUB/Clojure-AI-Book-Code/docs_qa/data/health.txt | ./categories.ss
computers_ai_search
health
Marks-Mac-mini:command_line_NLP $ cat ~/GITHUB/Clojure-AI-Book-Code/docs_qa/data/health.txt | ./categories   
computers_ai_search
health
```


## Using OpenAI's GPT-5 model To Summarize Input Text

TBD
