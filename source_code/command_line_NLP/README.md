# Using the NLP library developed in another chapter

## Quick example

```console
$ gxi
Gerbil v0.18.1-78-gc5546da0 on Gambit v4.9.5-124-g6d1a9a9b
> (import :nlp/main)
> nlp/main#process-string
#<procedure #10 nlp/main#process-string>
> (nlp/main#process-string "President Joe Biden went to Congress to talk about the economy and the budget deficit. He mentioned that tech companies like Google and Microsoft are doing well.")
(#("President"
   "Joe"
   "Biden"
   "went"
   "to"
   "Congress"
   "to"
   "talk"
   "about"
   "the"
   "economy"
   "and"
   "the"
   "budget"
   "deficit."
   "He"
   "mentioned"
   "that"
   "tech"
   "companies"
   "like"
   "Google"
   "and"
   "Microsoft"
   "are"
   "doing"
   "well.")
 #("NNP"
   "NNP"
   "NNP"
   "VBD"
   #0="TO"
   "NNP"
   #0#
   "VB"
   "IN"
   #1="DT"
   "NN"
   #2="CC"
   #1#
   "NN"
   #3="CD"
   "PRP"
   "VBN"
   "IN"
   "NN"
   "NNS"
   "IN"
   "NN"
   #2#
   "NNP"
   "VBP"
   "VBG"
   #3#)
 ()
 (("computers_ai_search.txt" 371200) ("computers_microsoft.txt" 206210))
 ()
 ()
 ())
> 
```

## Reading all command line arguments as a string

```
$ gxc -O -exe -o test_command_string test_command_string.ss
$ ./test_command_string the dog ran fast. 1 2 3
the dog ran fast. 1 2 3
```

```
$ gxc -O -exe -o categories categories.ss
/tmp/gxc.1757623741.458168/nlp__utils.scm:
/tmp/gxc.1757623741.458168/nlp__fasttag.scm:
/tmp/gxc.1757623741.458168/nlp__proper-names.scm:
/tmp/gxc.1757623741.458168/nlp__category.scm:
/tmp/gxc.1757623741.458168/nlp__main.scm:
/tmp/gxc.1757623741.458168/nlp__nlp.scm:
/tmp/gxc.1757623741.458168/categories.scm:
/Users/markw/GITHUB/gerbil_scheme_book/source_code/command_line_NLP/categories__exe.scm:
/tmp/gxc.1757623741.458168/nlp__utils.c:
/tmp/gxc.1757623741.458168/nlp__fasttag.c:
/tmp/gxc.1757623741.458168/nlp__proper-names.c:
/tmp/gxc.1757623741.458168/nlp__category.c:
/tmp/gxc.1757623741.458168/nlp__main.c:
/tmp/gxc.1757623741.458168/nlp__nlp.c:
/tmp/gxc.1757623741.458168/categories.c:
/Users/markw/GITHUB/gerbil_scheme_book/source_code/command_line_NLP/categories__exe.c:
/Users/markw/GITHUB/gerbil_scheme_book/source_code/command_line_NLP/categories__exe_.c:
```