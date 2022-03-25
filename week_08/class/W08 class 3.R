library(tidyverse)

abc_data <- read_csv("https://storybench.org/reinventingtv/abc7ny.csv")
kcra_data <- read_csv("https://storybench.org/reinventingtv/kcra.csv")
headlines <- 
  rbind(abc_data,kcra_data)
headlines

headlines %>% 
  select(headline) %>% 
  mutate(contains_sandy = str_detect(headline,"(?<!Superstorm )Sandy(?! Kenyon)(?! Hook)")) %>% 
  filter(contains_sandy)

my_city_names <- c("Sacramento","New York","Sandy","Austin","Idaho Falls","Brooklyn","Stockton") 
str_flatten(my_city_names,collapse = "|")

headlines_my_cities <- headlines %>% 
  select(headline) %>% 
  mutate(city = str_extract(headline, str_flatten(my_city_names,collapse = "|"))) %>% 
  filter(!is.na(city))
headlines_my_cities

#Let's summaries the table above
headlines_my_cities %>%
  group_by(city) %>% 
  summarise(
    count=n()
  )


library(maps)
us.cities

city_names <- us.cities %>%
  separate(name, c("city" , "state"), sep = -3) %>% 
  pull(city)
my <- city_names %>% unique(str_flatten(collapse = "|"))
city_names








