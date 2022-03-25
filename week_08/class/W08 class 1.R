library(tidyverse)

sentences[1:10]
sentences[1:10] %>% str_count("the")
sentences[1:10] %>% str_count(regex("the", ignore_case = TRUE))

length(sentences)
(months <- str_c(month.abb, collapse = "|"))
str_subset(sentences, months) # extract from sentences that matches month.abb -> search 12 month abb in sentences
str_subset(sentences[1:10], "The|the")
str_count(sentences[1:10], "The|the")

letters
str_c(letters, collapse = "|")
sentences[1:10] %>% str_count(str_c(letters, collapse = "|")) # cares upper or lower
sentences[1:10] %>% str_count(" ")
sentences[1:10] %>% str_length()

sentences[1:10] %>% str_count("[a-z]")
sentences[1:10] %>% str_count("[A-Z]")
sentences[1:10] %>% str_count("[a-zA-Z]")
sentences[1:10] %>% str_count("[:alpha:]") # count letters. lower: "[:lower:]". upper: "[:upper:]"

sentences[1:10] %>% str_length()
sentences[1:10] %>% str_count(str_c("[:alpha:]"))
sentences[1:10] %>% str_count("[:alpha:]")
sentences[1:10] %>% str_count(str_c("[^[:alpha:]]")) # ^: anything except --> anything except letters

# Task 15 help
sentences[1]
sentences[1] %>% 
  str_split("") %>% 
  unlist() %>% # make it into a vector
  .[c(1,seq(5,100,2))] %>% # .: # c(1, seq()): 1st element, 5th element and add 2 until 100
  tibble(myname = .) %>% # tibble(): make into dplyr --> can eliminate NA
  filter(!is.na(myname)) %>% 
  pull(myname) %>% 
  str_flatten() # str_flatten(): opposite of ste_split()

sentences[1:10]
sentences[1:10] %>% str_count("([:alpha:])\\1") # find repeated letters. \\1: find it again
sentences[1:10] %>% str_locate_all("([:alpha:])\\1")
sentences[1:10] %>% str_detect("([:alpha:])\\1")
sentences[1:10] %>% str_extract("([:alpha:])\\1")
sentences[1:10] %>% str_extract_all("([:alpha:])\\1")
sentences[1:10] %>% str_to_lower()

sentences[101:110]
sentences[101:110] %>% str_count("'")
sentences[101:110] %>% str_count(".") #,: special character: select all but new line
sentences[101:110] %>% str_count("[.]")
sentences[101:110] %>% str_count("\\.")
#What exactly is being captured with the count.  Change count to extract_all
sentences[101:110] %>% str_extract_all(".")
sentences[101:110] %>% str_extract_all("[.]")

sentences[101:110]
sentences[101:110] %>% str_count("the")
sentences[101:110] %>% str_count("the|The")
sentences[101:110] %>% str_to_lower() %>% str_count("the")

library(downloader)
library(stringi)

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
  sample_n(10) %>%  # grab random 10 verses.
  select(scripture_text,commas)

scripture_data %>% 
  mutate(commas = str_count(scripture_text,",")) %>% 
  group_by(volume_title) %>% 
  summarise(comma_count = sum(commas))

scripture_data %>% 
  mutate(itcametopass = str_count(scripture_text,"And it came to pass")) %>% 
  group_by(volume_title) %>% 
  summarise(count = sum(itcametopass))

scripture_data %>% 
  filter(volume_title == "Book of Mormon") %>% 
  mutate(hasitcamoetopass = str_detect(scripture_text, "it came to pass")) %>% 
  filter(hasitcamoetopass == TRUE)

