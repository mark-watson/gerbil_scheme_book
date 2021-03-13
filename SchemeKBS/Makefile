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
	gsc -c data/stop-words.ss
	gsc -c utils.ss
	gsc -c fasttag.ss
	gsc -c category.ss
	gsc -c levenshtein.ss
	gsc -c json.ss
	gsc -c generated-code/cat-data-tables.ss
	gsc -c generated-code/lexdata.ss
	gsc -c proper-names.ss
	gsc -c summarize.ss
	gsc -c key-phrases.ss
	gsc -c main.ss
	gsc -c testapp.ss
	gsc -link category.c key-phrases.c main.c summarize.c data/stop-words.c utils.c fasttag.c levenshtein.c json.c proper-names.c generated-code/cat-data-tables.c  generated-code/lexdata.c   testapp.c 
	gcc -I/Library/Gambit-C/current/include -L/Library/Gambit-C/current/lib category.c key-phrases.c main.c summarize.c data/stop-words.c utils.c fasttag.c levenshtein.c json.c proper-names.c generated-code/cat-data-tables.c generated-code/lexdata.c   testapp.c testapp_.c -lgambc -o kbtm
