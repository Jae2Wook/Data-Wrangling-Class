library(tidyverse)
library(readr)
library(maps)

NY <- read_csv("https://storybench.org/reinventingtv/abc7ny.csv")

CA <- read_csv("https://storybench.org/reinventingtv/kcra.csv")

News <- bind_rows(NY, CA)


cities <- separate(us.cities, col = name, into = c("city", "state"), sep = -3)

cities_vector <- unlist(cities$city)
cities_str <- unique(str_c(cities_vector, collapse = "|"))

# For each headline, identify the name of the city 
# mentioned in the headline (if any).
News$headline %>% str_extract_all(cities_str, simplify = FALSE)

News$headline %>% str_match_all(cities_str)

#####
News$cities_mentioned_ext <- News$headline %>% str_extract_all(cities_str)
View(News)
News$cities_mentioned_mat <- News$headline %>% str_match_all(cities_str)
News$cities_mentioned1 <- ifelse(News$cities_mentioned %>% str_detect(cities_str), 1, 0)
##### OR

### use
News$cities_mentioned <- News$headline %>% str_extract_all(cities_str, simplify = TRUE)

News$cities_mentioned1 <- ifelse(News$cities_mentioned %>% str_detect(cities_str), 1, 0)


# Question 1: For the 15 cities with the most mentions overall, 
# create a graphic that summarizes their mentions. Write a paragraph 
# in which you discuss the results. Do they make sense? Do you need to 
# make changes? If something looks wrong, fix your code and run it again to 
# find the new top 15.
View(News)

top_15 <- News %>% group_by(cities_mentioned) %>% summarise(n = n()) %>% arrange(desc(n))
top_15[2:16,]
View(top_15[2:16,])

top_20 <- top_15[2:25,]

View(top_20)

# Sacramento, New York, Las Vegas, Stockton, Manhattan, Houston, Newark

cities_str <- str_replace(cities_str, "New York", "New York(?!er)")

cities_str <- str_replace(cities_str, "Sandy Springs", "abc")

cities_str <- str_replace(cities_str, "Sandy", "(?<!Superstorm )Sandy(?! Hook)(?! Kenyon)")

cities_str <- str_replace(cities_str, "Modesto", "Modesto(?! Christian)")

cities_str <- str_replace(cities_str, "Moore", "(?<!Brian  )(?<!Roy )(?<!Mandy )(?<!Bud )(?<!for )(?<!Kayla )Moore(?!\'s)")

cities_str <- str_replace(cities_str, "Charlotte", "(?<!Princess )Charlotte(?!\'s)(?!sville)")

cities_str <- str_replace(cities_str, "Davis", "(?<!Viola )(?<!Dave )(?<!Kim )(?<!UC )(?<!Paige )Davis")

cities_str <- str_replace(cities_str, "Folsom", "(?<!Lake )Folsom(?! Lake)")

cities_str <- str_replace(cities_str, "Clinton", "(?<!Chelsea )(?<!, )(?<!Hillary )(?<!and )Clinton(?! shares)(?! speaks)(?! Foundation)")

cities_str <- str_replace(cities_str, "Taylor", "(?<!Will )Taylor(?! Swift)(?! sells)(?!\'s)")

cities_str <- str_replace(cities_str, "Chicago", "Chicago(?! Cubs)(?! Bears)")

cities_str <- str_replace(cities_str, "Brea", "Brea(?!k)(?!t)(?!s)()?!d")

# "(?<!Superstorm )Sandy(?! Kenyon)(?! Hook)")
News %>% filter(cities_mentioned == "Sandy") %>% select(headline) %>% View()

News$cities_mentioned <- News$headline %>% str_extract_all(cities_str, simplify = TRUE)
View(News)

top_15_1 <- News %>% group_by(cities_mentioned) %>% summarise(n = n()) %>% arrange(desc(n))

News %>% filter(cities_mentioned == "Houston") %>% select(headline) %>% View()

View(top_15_1[2:16,])

ggplot(top_15_1[2:16,], aes(x = cities_mentioned[,1], y = n, fill = cities_mentioned[,1])) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), size = 3, nudge_y = 30) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none", panel.grid.major.x = element_blank()) +
  labs(title = "Citiies Mentioned in News Headline", x = "City Names", y = "Counts")

sum(top_15_1$n)

# Question 2: For those same 15 cities, create a graphic to show 
# the headline count for each city for each month. Write a paragraph to 
# discuss meaningful insights from the graph about headlines over time for 
# certain cities and/or other features and trends you notice in the graph.

library(lubridate)
library(stringi)

# News$cities_mentioned <- News$headline %>% str_extract(cities_str)

cities_15 <- unlist(top_15_1 %>% pull(cities_mentioned)) %>% stri_remove_empty()

News_new <- News %>% filter(cities_mentioned[,1] %in% cities_15)

News_new <- News_new %>% mutate(date = lubridate::mdy_hm(`datetime`))
View(News_new)

News_new <- News_new %>% separate(datetime, into = c("Month", "Day", "Year"),sep = " ")
News_new$Day <- str_remove(News_new$Day, ",")

### News_new$month_name <- format(News_new$date,"%B")

# News_new$Date <- str_c(News_new$Month, News_new$Year, sep = " ")

News_new$Month <- factor(News_new$Month, levels = c("July", "August", "September", "October", "November", "December", "January"))

ggplot(News_new, aes(x = cities_mentioned[,1], fill = cities_mentioned[,1])) +
  geom_bar() +
  facet_wrap(.~Month, nrow = 1) +
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Cities Mentioned Every Month", x = "City Names", y = "Counts")



# Question 3: Create a graphic specifically dedicated to comparing 
# the headlines generated about Houston, TX and Charlotte, NC over time 
# (by month). What trends do you notice?

three <- c("Houston", "Charlotte")

News_Hou <- News %>% filter(cities_mentioned[,1] %in% three)
View(News_Hou)

News_Hou <- News_Hou %>% separate(datetime, into = c("Month", "Day", "Year"),sep = " ")
News_Hou$Day <- str_remove(News_Hou$Day, ",")

News_Hou$Month <- factor(News_Hou$Month, levels = c("July", "August", "September", "October", "November", "December", "January"))

ggplot(News_Hou, aes(x = cities_mentioned[,1], fill = cities_mentioned[,1])) +
  geom_bar() +
  facet_wrap(.~Month, nrow = 1) +
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Cities Mentioned Every Month", x = "City Names", y = "Counts")




