# Knowledge Books Systems NLP Utilities rewritten in Gerbil SchemeKBS

## Test run:

On some systems, you might run into link compatibility probelems. I did on macOS when I brew installed Gerbil Scheme and later brew updated openssl to a newer version.

As a result of this configuration problem, the **make test** target runs an interpretter target, bypassing any potential link problems.

```
make test
cat .gerbil/test-output.json | jq
```

## Building the command line tool

```
$ make
$ .gerbil/bin/testapp -i data/testdata/climate_g8.txt -o output.json
$ cat output.json | jq

  ... lots of output not shown...
    "VBD",
    "CD"
  ],
  "key-phrases": [
    "clean energy",
    "developing countries"
  ],
  "categories": [
    [
      "news_economy.txt",
      136750
    ],
    [
      "news_war.txt",
      117290
    ]
  ]
}
```