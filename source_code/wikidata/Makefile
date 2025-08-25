run:
	gxi -L wikidata.ss -

test:
	gxi -L wikidata.ss -e "(test2)"

run-agent:
	WDQS_UA="$${WDQS_UA:-YourApp/1.0 (https://your.site; you@site)}" gxi -L wikidata.ss -e "(test2-ua)"

test1:
	gxi -L wikidata.ss -e "(test1)"

run-agent1:
	WDQS_UA="$${WDQS_UA:-YourApp/1.0 (https://your.site; you@site)}" gxi -L wikidata.ss -e "(test1-ua)"
