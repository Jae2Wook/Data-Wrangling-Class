library(tidyverse)
library(downloader)

####
zip_file <- tempfile()
unzip_dir <- tempfile()
url <- "http://scriptures.nephi.org/downloads/lds-scriptures.csv.zip"
download(url, dest = zip_file, mode = "wb")
unzip(zipfile = zip_file, exdir = unzip_dir)
file_name <- list.files(unzip_dir)
scripture <- read_csv(file = file.path(unzip_dir, file_name))
####

download("http://scriptures.nephi.org/downloads/lds-scriptures.csv.zip", "temp.zip")
unzip("temp.zip")

# how to handle column warnings
# https://readr.tidyverse.org/articles/readr.html

scripture_data <- read_csv("lds-scriptures.csv",
                           col_types = cols(book_subtitle = col_character(),
                                            volume_subtitle = col_character()))

View(scripture_data)

# What is the average verse length (number of words) 
# in the New Testament compared to the Book of Mormon?

scripture_new <- scripture_data %>% filter(volume_title == "New Testament")
scripture_new$lengths <- str_count(scripture_new$scripture_text, " ") + 1
mean(scripture_new$lengths)

scripture_mor <- scripture_data %>% filter(volume_title == "Book of Mormon")
scripture_mor$lengths <- str_count(scripture_mor$scripture_text, " ") + 1
mean(scripture_mor$lengths)

###
scripture_new_mor <- scripture_data %>% filter(volume_title == "New Testament" | volume_title == "Book of Mormon")
scripture_new_mor$lengths <- str_count(scripture_new_mor$scripture_text, " ") + 1

scripture_new_mor %>% group_by(volume_title) %>% summarise(`avg number of words` = mean(lengths))

####


# How often is the word "Jesus" in the New Testament compared to 
# the Book of Mormon?

scripture_new %>% select(scripture_text) %>% str_count("Jesus")
sum(scripture_new %>% pull(scripture_text) %>% str_count("Jesus"))

scripture_mor %>% select(scripture_text) %>% str_count("Jesus")
sum(scripture_mor %>% pull(scripture_text) %>% str_count("Jesus"))

# scripture_mor %>% pull(scripture_text) %>% str_view("Jesus")

#######
scripture_new_mor <- scripture_data %>% filter(volume_title == "New Testament" | volume_title == "Book of Mormon")
scripture_new_mor$Je <- str_count(scripture_new_mor$scripture_text, "Jesus")
View(scripture_new_mor)

scripture_new_mor %>% group_by(volume_title) %>% summarise(`number of Jesus` = sum(Je))
#######


# What does the distribution of verse word counts 
# look like for each book in the Book of Mormon? (Visualize)

View(scripture_mor)

# scripture_mor %>% group_by(book_title) %>% sum(lengths)

ggplot(scripture_new_mor %>% filter(volume_title == "Book of Mormon"), aes(x = book_title, y = lengths, color = book_title)) +
  geom_boxplot() +
  labs(title = "Book of Mormon Each Book Verse Word Counts", x = "Book Title", y = "Number of Words") +
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.major.x = element_blank())






