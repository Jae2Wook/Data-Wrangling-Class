library(tidyverse)
library(readr)
library(stringi)

####
# To get the standard works data
scriptures <- rio::import("http://scriptures.nephi.org/downloads/lds-scriptures.csv.zip")

# To get the Savior names
bmnames <- read_rds(gzcon(url("https://byuistats.github.io/M335/data/BoM_SaviorNames.rds")))
####

download("http://scriptures.nephi.org/downloads/lds-scriptures.csv.zip", "temp.zip")
unzip("temp.zip")

# how to handle column warnings
# https://readr.tidyverse.org/articles/readr.html

scripture <- rio::import("http://scriptures.nephi.org/downloads/lds-scriptures.csv.zip",
                           col_types = cols(book_subtitle = col_character(),
                                            volume_subtitle = col_character()))



savior <- rio::import("https://byuistats.github.io/M335/data/BoM_SaviorNames.rds")

mormon <- scripture %>% filter(volume_title == "Book of Mormon") %>% pull(scripture_text) %>% str_flatten(collapse = " ")


names <- savior %>% pull(name) %>% str_flatten(collapse = " |") %>% str_replace_all(", ", " |")

words <- mormon %>% str_split(pattern = names) %>% unlist() %>% map_int(function(x) {stri_stats_latex(x) %>% .["Words"]})

words

words %>% mean()




