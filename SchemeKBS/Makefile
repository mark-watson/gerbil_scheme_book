clean:
	rm -f *~
	rm -f */*~
	rm -f \#*
	rm -f */\#*
	rm -f *.c
	rm -f generated-code/*.c
	rm -f a.out
	rm -f kbtm

app:
	gsc -c data/stop-words.scm
	gsc -c utils.scm
	gsc -c fasttag.scm
	gsc -c category.scm
	gsc -c levenshtein.scm
	gsc -c json.scm
	gsc -c generated-code/cat-data-tables.scm
	gsc -c generated-code/lexdata.scm
	gsc -c proper-names.scm
	gsc -c summarize.scm
	gsc -c key-phrases.scm
	gsc -c main.scm
	gsc -c testapp.scm
	gsc -link category.c key-phrases.c main.c summarize.c data/stop-words.c utils.c fasttag.c levenshtein.c json.c proper-names.c generated-code/cat-data-tables.c  generated-code/lexdata.c   testapp.c 
	gcc -I/Library/Gambit-C/current/include -L/Library/Gambit-C/current/lib category.c key-phrases.c main.c summarize.c data/stop-words.c utils.c fasttag.c levenshtein.c json.c proper-names.c generated-code/cat-data-tables.c generated-code/lexdata.c   testapp.c testapp_.c -lgambc -o kbtm
