text = ''
puts "(define data123 '("
File.open("lexicon.txt") { |f|  text = f.read }
text.split("\n").each {|line|
    tokens = line.split
    if !tokens[0].index("\\")
        puts " (\"" + tokens[0] + "\" . \"" + tokens[1] + "\")"
    end
}
puts " )"
puts ")"
puts "(define lex-hash (list->table data123 size: 110000))"
