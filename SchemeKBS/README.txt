README NOTES
------------

1. when running in aquamacs, use to set working directory:

(current-directory "/Users/markw/Documents/Coding/SchemeStuff/SchemeKBS")


2. to test the generated JSON data:

./kbtm -i data/testdata/climate_g8.txt -o test123.txt

irb
require 'pp'
require 'rubygems'
require 'json'
txt = File.open("test123.txt").read
result = JSON.parse(txt)
puts "\n\n\n"
pp result

