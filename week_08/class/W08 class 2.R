library(tidyverse)
library(downloader)

temp <- tempfile()
download("http://scriptures.nephi.org/downloads/lds-scriptures.csv.zip", temp, mode = "wb")
temp <- unzip(temp)
scripture_data <- 
  read_csv(temp, 
           col_types = cols(book_subtitle = col_character(),
                            volume_subtitle = col_character()))
scripture_data %>% glimpse()

scripture_data %>% 
  mutate(commas = str_count(scripture_text,",")) %>% 
  summarise(comma_count = sum(commas))

scripture_data %>% 
  mutate(commas = str_count(scripture_text,",")) %>% 
  sample_n(10) %>% 
  select(scripture_text,commas)

scripture_data %>% 
  mutate(commas = str_count(scripture_text,",")) %>% 
  group_by(volume_title) %>% 
  summarise(comma_count = sum(commas))

library(stringi)
sentences[1]
stri_stats_latex(sentences[1])
stri_stats_latex(sentences[1])["Words"]
stri_stats_latex(sentences[1]) %>% .["Words"] # .: previous work
#Note that the . above is a place holder for the input to the pipe. The last two lines above are the same. 

sentences[1:10]
stri_stats_latex(sentences[1:10])
stri_stats_latex(sentences[1:10]) %>% .["Words"]

str_count(sentences[1:10])

sentences[1:10] %>% 
  tibble(text = .) %>% 
  mutate(chars = str_count(text)) %>% 
  mutate(words = stri_stats_latex(text) %>% .["Words"])
#But we still need to make sure each sentence is sent into stri_stats_latex(), not the entire vector.

sentences[1:10] %>% 
  tibble(text = .) %>% 
  group_by(text) %>% #When we group_by, then we split the text vector into several nested vectors. 
  mutate(words = stri_stats_latex(text) %>% .["Words"])

# not to overlap the same sentences by using row_number()
sentences[1:10] %>% 
  tibble(text = .) %>% 
  mutate(id = row_number()) %>% 
  group_by(id) %>% 
  mutate(words = stri_stats_latex(text) %>% .["Words"])

# library(purrr): already in tidyverse

count_words <- function(x){stri_stats_latex(x) %>% .["Words"]}
count_words(sentences[1])
map(sentences[1:10], count_words) # list
map(sentences[1:10], count_words) %>% unlist()
map_int(sentences[1:10], count_words) # vector

map_int(sentences[1:10], function(x){stri_stats_latex(x) %>% .["Words"]})
map_int(sentences[1:10], ~{stri_stats_latex(.x) %>% .["Words"]})

sentences[1:10] %>% 
  tibble(text = .) %>% 
  mutate(words = map_int(text, function(x){stri_stats_latex(x) %>% .["Words"]}))

tibble(
  text = sentences[1:10],
  words = map_int(text, function(x) stri_stats_latex(x)["Words"])
)
tibble(text = sentences[1:10]) %>% 
  mutate(words = map_int(text, function(x) stri_stats_latex(x)["Words"]))

count_words <- function(x){stri_stats_latex(x) %>% .["Words"]}
tibble(text = sentences[1:10]) %>% 
  mutate(words = map_int(text,count_words))



abc_data <- read_csv("https://storybench.org/reinventingtv/abc7ny.csv")
kcra_data <- read_csv("https://storybench.org/reinventingtv/kcra.csv")
headlines <- 
  rbind(abc_data,kcra_data)
headlines

headlines %>% 
  select(headline) %>% 
  mutate(contains_sandy = str_detect(headline,"Sandy")) %>% 
  filter(contains_sandy)

headlines %>% 
  select(headline) %>% 
  mutate(contains_sandy = str_detect(headline,"(?<!Superstorm )Sandy(?! Kenyon)(?! Hook)")) %>% 
  filter(contains_sandy)

headlines_3cities <- headlines %>% 
  select(headline) %>% 
  mutate(city = str_extract(headline,"Sacramento|New York|Sandy")) %>% 
  filter(!is.na(city))
headlines_3cities

#Let's summaries the table above
headlines_3cities %>%
  group_by(city) %>% 
  summarise(
    count=n()
  )

my_city_names <- c("Sacramento","New York","Sandy","Austin","Idaho Falls","Brooklyn","Stockton") 
str_flatten(my_city_names,collapse = "|")

headlines_my_cities <- headlines %>% 
  select(headline) %>% 
  mutate(city = str_extract(headline,str_flatten(my_city_names,collapse = "|"))) %>% 
  filter(!is.na(city))
headlines_my_cities

#Let's summaries the table above
headlines_my_cities %>%
  group_by(city) %>% 
  summarise(
    count=n()
  )

########3

################################################
#  Manipulating strings with base R functions  #
################################################

# One string

my_string <- "The Church of Jesus Christ of Latter-day Saints"

grep("Church", my_string)
grep("church", my_string)

grep("Church", my_string, value = TRUE)
grep("church", my_string, value = TRUE)

grepl("Church", my_string)
grepl("church", my_string)

# What happens if we have multiple strings?
# Remember, R is a "vectorized" language

my_strings <- c("The","quick brown fox", "jumped", "over the", "lazy dog.")

grep("fox", my_strings) # grep(): gives index number
grep("foxes", my_strings)

grep("fox", my_strings, value = TRUE)
grep("foxes", my_strings, value = TRUE)

grepl("fox", my_strings)
grepl("foxes", my_strings)

# We can use the results from `grep` and `grepl` to index a vector

my_strings <- c("one", "one two", "three", "one one one")

my_strings[1]

grep("one", my_strings) # gives index number
my_strings[grep("one", my_strings)]

grepl("one", my_strings)
my_strings[grepl("one", my_strings)]

# We can also use `grepl` to count things!
# How would you interpret this number?

sum(grepl("one", my_strings))



#######################################
#         REGULAR EXPRESSIONS         #
#######################################

# Let's start with a vector with multiple strings
# (Like a column of a dataset)

my_strings <- c("The woods are lovely, dark and deep,",
                "But I have promises to keep,",
                "And miles to go before I sleep,",
                "And miles to go before I sleep.")

# Goal: count the number of vowels

# Try searching for "a" first

grep("a", my_strings)
grepl("a", my_strings)

# This is a good time to switch over to the `stringr` package
# (which is a part of the tidyverse)
#library(tidyverse)

# library(stringr)

# Look at the cheat sheet, and tell me how to count the number of vowels
# https://github.com/rstudio/cheatsheets/blob/master/strings.pdf

str_detect(my_strings, "a")  # grepl
str_which(my_strings, "a")   # grep
str_count(my_strings, "a")

str_count(my_strings, "a") +
  str_count(my_strings, "e") +
  str_count(my_strings, "i") +
  str_count(my_strings, "o") +
  str_count(my_strings, "u")

# Less typing....

str_count(my_strings, "a|e|i|o|u")
sum(str_count(my_strings, "a|e|i|o|u"))

# Other ways to do it

my_string <- "Katie Larson"
str_count(my_string)
str_count(my_string, "a|e|i|o|u")
str_count(my_string, "[aeiou]")
str_count(my_string, "[^aeiou]")
str_count(my_string, "[^aeiou ]")

# New goal: Count all letters 
# (alphabet, not including white space, puncuation, or digits/numbers)

str_count(my_strings, "[:alpha:]")

# How many non-alphabetical character do we have?

str_length(my_strings)
str_count(my_strings, "[:alpha:]")
str_length(my_strings) - str_count(my_strings, "[:alpha:]")

str_count(my_strings, "^[:alpha:]") # doesnt' work
str_count(my_strings, "[^:alpha:]") # doesnt' work

# Look at the cheat sheet....can you tell what to do?

# Let's google it
# And then test it on one of these websites:
# https://regexr.com/
# https://regex101.com/

str_count(my_strings, "[^a-zA-Z]")

# Using double escapes

str_count(my_strings, "\w")

#install.packages("htmlwidgets")
library(htmlwidgets)
str_count(my_strings, "\\w"); str_view_all(my_strings, "\\w")
str_count(my_strings, "\\W"); str_view_all(my_strings, "\\W")

# Quick example of why we have to use double escape (double backslash)
#my_string <- ""Hello!" she said."

my_string <- "\"Hello!\" she said."
my_string
cat(my_string)

# Want to search for a repeating pattern? Use quantifiers

str_count(my_strings, "[aeiou]")    # exactly one
str_view_all(my_strings, "[aeiou]")

str_count(my_strings, "[aeiou]?")   # zero or one
str_view_all(my_strings, "[aeiou]?")

str_count(my_strings, "[aeiou]*")   # zero or more
str_view_all(my_strings, "[aeiou]*")

str_count(my_strings, "[aeiou]+")   # one or more
str_view_all(my_strings, "[aeiou]+")

str_count(my_strings, "[aeiou]{2}") # exactly two
str_view_all(my_strings, "[aeiou]{2}")




######################################
#       OTHER USEFUL FUNCTIONS       #
######################################

my_strings <- c("The woods are lovely, dark and deep,",
                "But I have promises to keep,",
                "And miles to go before I sleep,",
                "And miles to go before I sleep.")

### SUBSET STRINGS

# subset
str_sub(my_strings, 1, 5)    # a bunch of letters
str_sub(my_strings, 1, 1)    # one letter
str_sub(my_strings, -5, -1)  # negative indices work too

# Does this work?
str_sub(my_strings, c(1,1,1), c(1,2,3)) # wait, what is this doing?
str_sub(my_strings[1], c(1,1,1), c(1,2,3)) # better

# extract
str_extract(my_strings, "a")
str_extract_all(my_strings, "a")
str_extract_all(my_strings, "a", simplify = TRUE)

###  MUTATE STRINGS

# substitute with set locations (indices)
temp <- my_strings
temp
str_sub(temp, 1, 3) <- "HELLO"
temp

# substitute, based on match
str_replace(my_strings, "a", "AAA")
my_strings %>% str_replace("a", "AAA")
my_strings %>% str_replace_all("a", "AAA")

# remove
str_replace_all(my_strings, "a", "")
str_remove(my_strings, "a")

### JOIN AND SPLIT

# If you have multiple columns or vectors or strings
# join
month.abb
month.name
str_c(month.abb, month.name)
str_c(month.abb, month.name, sep = "BOO")

# collapse
str_c(my_strings, collapse = "BOO!")

# split
str_split(my_strings, " ")
str_split_fixed(my_strings, " ")
str_split_fixed(my_strings, " ", n = 3)

