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

