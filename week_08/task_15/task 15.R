library(stringr)
library(tidyverse)

string1 <- "This is a string"
string2 <- 'If I want to include a "quote" inside a string, I use single quotes'
writeLines(string2)

double_quote <- "\"" # or '"'
single_quote <- '\'' # or ''"

x <- c("\"", "\\")
writeLines(x) # erases unecessary \

# \n: new line, \t: tab more info: ?'"' or ?"'"

x <- "\u00b5"
c("one", "two", "three")

# String length
str_length(c("a", "R for data science", NA))

# combining strings
str_c("c", "y")
str_c("x", "y", "z")

str_c("x", "y", sep = ", ") # sep: how to control

x <- c("abc", NA)
str_c("|-", x, "-|")
str_c("|-", str_replace_na(x), "-|") # str_replace_na(): print as "NA"

str_c("prefix-", c("a", "b", "c"), "-suffix")

# using if() or TRUE (show statement) or FALSE (don't show statement) 
name <- "Hadley"
time_of_day <- "morning"
birthday <- TRUE # or FALSE

str_c("Good ", time_of_day, " ", name, if(birthday) " and Happy BIRTHDAY", ".")

str_c(c("x", "y", "z"), collapse = ", ")
# =
str_c("x", "y", "z", sep = ", ")

# Subsetting strings 
x <- c("Apple", "Banana", "Pear")
str_sub(x, 1, 3) # str_sub() take start and end

str_sub(x, -3, -1)

str_sub("a", 1, 5)
str_sub("a", 2, 5) # gives empty

str_sub(x, 1, 1) <- str_to_lower(str_sub(x, 1, 1)) # use str_sub() as mutate()
x

# Locales
str_to_upper(c("i", "ı"))
str_to_upper(c("i", "ı"), locale = "tr")

x <- c("apple", "eggplant", "banana")
str_sort(x, locale = "en") # english
str_sort(x, locale = "haw") # hawaiian

# Matching patterns with refular expressions
# Basic matches
x <- c("apple", "banana", "pear")
str_view(x, "an")

str_view(x, ".a.") # .: any letter

dot <- "\\."
writeLines(dot)

str_view(c("abc", "a.c", "bef"), "a\\.c")

# to match a literal \ you need to write "\\\\"
x <- "a\\b"
writeLines(x)
str_view(x, "\\\\")

# Anchors
x <- c("apple", "banana", "pear")
str_view(x, "^a")
# ^: to match the start of the string.
# $: to match the end of the string
str_view(x, "a$")

x <- c("apple pie", "apple", "apple cake")
str_view(x, "apple")
str_view(x, "^apple$")

# Character classes and alternatives
# \d: matches any digit - need "\\d"
# \s: matches any whitespace - need "\\s"
# [abc]: matches a, b or ,c
# [^abc]: matches anything except a, b, or c

str_view(c("abc", "a.c", "a*c", "a c"), "a[.]c")
str_view(c("abc", "a.c", "a*c", "a c"), ".[*]c")
str_view(c("abc", "a.c", "a*c", "a c"), "a[ ]")

str_view(c("grey", "gray"), "gr(e|a)y")

# Repetition
x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
str_view(x, "CC?")

# ?: 0 or 1
# +: 1 or more
# *: 0 or more

str_view(x, "CC+")
str_view(x, "CC*")
str_view(x, 'C[LX]+')

# {n}: exactly n times
# {n,}: n times or more
# {,m}: at most m times
# {n,m}: between n and m times

str_view(x, "C{2}")
str_view(x, "C{2,}")
# match the longest strin possible
str_view(x, "C{2,3}")
# adding ? matches the shortest string possible
str_view(x, "C{2,3}?")
str_view(x, 'C[LX]+?')

# grouping and backreferences
str_view(fruit, "(..)\\1", match = TRUE)

# Tools

# Detect matches
x <- c("apple", "banana", "pear")
str_detect(x, "e")

words
# How many common words start with t?
sum(str_detect(words, "^t"))
# What proportion of common words end with a vowel?
mean(str_detect(words, "[aeiou]$"))

# Find all words containing at least one vowel, and negate
no_vowels_1 <- !str_detect(words, "[aeiou]")

# Find all words consisting only of consonants (non-vowels)
no_vowels_2 <- str_detect(words, "^[^aeiou]+$")
identical(no_vowels_1, no_vowels_2)

words[str_detect(words, "x$")]
# =
str_subset(words, "x$")

df <- tibble(word = words, i = seq_along(word)) # seq_along() numbers
df %>% filter(str_detect(word, "x$"))

x <- c("apple", "banana", "pear")
str_count(x, "a")

# On average, how many vowels per word?
mean(str_count(words, "[aeiou]"))

df %>% mutate(vowels = str_count(word, "[aeiou]"),
              constates = str_count(words, "[^aeiou]"))

# never overlap
str_count("abababa", "aba")

str_view_all("abababa", "aba")

# Extract matches
length(sentences)

colours <- c("red", "orange", "yellow", "green", "blue", "purple")
colour_match <- str_c(colours, collapse = "|")

has_colour <- str_subset(sentences, colour_match)
matches <- str_extract(has_colour, colour_match)

more <- sentences[str_count(sentences, colour_match) > 1]

str_view_all(more, colour_match)

str_extract(more, colour_match)

# str_extract_all() returns a list
str_extract_all(more, colour_match)

str_extract_all(more, colour_match, simplify = TRUE)

x <- c("a", "a b", "a b c")
str_extract_all(x, "[a-z]", simplify = TRUE) # [a-z] a to z

# Grouped matches

noun <- "(a|the) ([^ ]+)" # a or the, first is whitespace +: one or more

has_noun <- sentences %>% 
  str_subset(noun)

has_noun %>% str_extract(noun)

# str_match() gives each individual component
has_noun %>% str_match(noun)
# =
tibble(sentence = sentences) %>% 
  tidyr::extract(
    sentence, c("article", "noun"), "(a|the) ([^ ]+)",
    remove = FALSE
  )

# replacing matches
x <- c("apple", "pear", "banana")
str_replace(x, "[aeiou]", "-")
str_replace_all(x, "[aeiou]", "-")

x <- c("1 house", "2 cars", "3 people")
str_replace_all(x, c("1" = "one", "2" = "two", "3" = "three"))

# change order
sentences %>% 
  str_replace("([^ ]+) ([^ ]+) ([^ ]+)", "\\1 \\3 \\2") %>% head()

# splitting
sentences %>% head() %>% str_split(" ")

"a|b|c|d" %>% str_split("\\|") %>% .[[1]] # .[[1]]: extract the first element

sentences %>% head() %>% str_split(" ", simplify = TRUE)

fields <- c("Name: Hadley", "Country: NZ", "Age: 35")
fields %>% str_split(": ", n = 2, simplify = TRUE) # n: number of pieces

x <- "This is a sentence.  This is another sentence."
# split up by character
str_view_all(x, boundary("word"))

str_split(x, " ")[[1]]
str_split(x, boundary("word"))[[1]]

# str_locate() and str_locate_all() give you the starting and ending positions of each match

# other types of pattern
str_view(fruit, "nana")
str_view(fruit, regex("nana"))

bananas <- c("banana", "Banana", "BANANA")
str_view(bananas, "banana")

str_view(bananas, regex("banana", ignore_case = TRUE))

# multiline = TRUE allows ^ and $ to match the start and end of each line rather than the start and end of the complete string.
x <- "Line 1\nLine 2\nLine 3"
str_extract_all(x, "^Line")[[1]]
str_extract_all(x, regex("^Line", multiline = TRUE))[[1]]

# comments = TRUE allows you to use comments and white space
phone <- regex("
  \\(?     # optional opening parens ?: 0 or 1
  (\\d{3}) # area code
  [) -]?   # optional closing parens, space, or dash
  (\\d{3}) # another three numbers
  [ -]?    # optional space or dash
  (\\d{3}) # three more numbers
  ", comments = TRUE)

str_match("514-791-8141", phone)
# dotall = TRUE allows . to match everything, including \n

# fixed(): matches exactly the specified sequence of bytes
microbenchmark::microbenchmark(
  fixed = str_detect(sentences, fixed("the")),
  regex = str_detect(sentences, "the"),
  times = 20
)

a1 <- "\u00e1"
a2 <- "a\u0301"
c(a1, a2)
#> [1] "á" "á"
a1 == a2
#> [1] FALSE

# using fixed() with non_English data is not good.
str_detect(a1, fixed(a2))

# use coll() - collation
str_detect(a1, coll(a2))

i <- c("I", "İ", "i", "ı")
str_subset(i, coll("i", ignore_case = TRUE))

str_subset(i, coll("i", ignore_case = TRUE, locale = "tr"))

stringi::stri_locale_info()

x <- "This is a sentence."
str_view_all(x, boundary("word"))

str_extract_all(x, boundary("word"))

# other uses of regular expressions
apropos("replace")

head(dir(pattern = "\\.Rmd$"))

# can use stringi

# Text analysis
library(readr)
text_ran <- read_lines("https://byuistats.github.io/M335/data/randomletters.txt")

text_num <- read_lines("https://byuistats.github.io/M335/data/randomletters_wnumbers.txt")

# With the randomletters.txt file, pull out every 1700 letter and 
# find the quote that is hidden. The quote ends with a period 
# (there may be some extra letters at the end). 
# You should be pulling out the 1st letter, 
# then the 1700th, then the 3400th, and then continue to count by 1700.

texts <- text_ran %>% 
  str_split("") %>% 
  unlist()

len <- sum(str_count(texts))

#str_extract_all(texts, "\"")

texts[c(1, seq(1700, len, 1700))] %>% str_flatten()

####
str_sub(text_ran,seq(0,len,1700),seq(0,len,1700)) %>% str_flatten()
####

# With the randomletters_wnumbers.txt file, 
# find all the numbers hidden and convert those numbers to letters 
# using the letters order in the alphabet to decipher the message. 
# The message starts with "experts".

nums <- as.numeric(str_extract_all(text_num, "[0-9]+")[[1]])
# str(nums)
letters[nums] %>% str_flatten()


# With the randomletters.txt file, 
# remove all the spaces and periods from the string then find
# the longest sequence of vowels.
ran <- str_replace_all(text_ran, "[ .]", "")
v <- unlist(str_extract_all(ran, regex("[aeiou]+", ignore_case = TRUE)))
v[str_length(v) == max(str_length(v))]


