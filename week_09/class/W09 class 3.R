library(tidyverse)
mpg %>% glimpse()

mpg %>% 
  select(cyl,cty) %>% 
  group_by(cyl) %>% 
  summarise(ave = mean(cty))


my_mean <- function(df) { mean(df$cty) }
mpg %>% 
  group_by(cyl) %>% 
  nest %>% # nest: make into tipples
  mutate(my_ave = map_dbl(data,my_mean)) %>% 
  select(-data)

my_mean2 <- function(my_cyl) { 
  mpg %>% filter(cyl == my_cyl) %>% pull(cty) %>% mean()
}
mpg %>% 
  distinct(cyl) %>% 
  mutate(my_ave2 = map_dbl(cyl,my_mean2))

my_mean3 <- function(df,var) { df %>% pull({{var}}) %>% mean() } # {{}}: name of the "column"
mpg %>% 
  group_by(cyl) %>% 
  nest %>% 
  mutate(my_ave = map_dbl(data,function(x) {my_mean3(x,cty)} )) %>% 
  select(-data)

mpg %>% 
  group_by(cyl) %>% 
  nest %>% 
  mutate(my_ave = map_dbl(data,my_mean3,cty)) %>% # map(): what to loop, what function, more variable, more var, ...
  select(-data)

mpg %>% 
  group_by(cyl) %>% 
  nest %>% 
  mutate(my_ave = map_dbl(data,my_mean3,hwy)) %>% 
  select(-data)

mpg %>% 
  group_by(cyl,trans) %>% 
  nest %>% 
  mutate(my_ave = map_dbl(data,my_mean3,hwy)) %>% 
  select(-data)

iris %>% glimpse()
iris %>% 
  group_by(Species) %>% 
  nest %>% 
  mutate(my_ave = map_dbl(data,my_mean3,Sepal.Length)) %>% 
  select(-data)

nycflights13::flights %>% glimpse()
nycflights13::flights %>% 
  group_by(carrier) %>% 
  nest %>% 
  mutate(my_ave = map_dbl(data,my_mean3,arr_delay)) %>% 
  select(-data)

my_mean4 <- function(df,var) { df %>% pull({{var}}) %>% mean(na.rm = TRUE) }

nycflights13::flights %>% 
  group_by(carrier) %>% 
  nest %>% 
  mutate(my_ave = map_dbl(data,my_mean4,arr_delay)) %>% 
  select(-data)


#####

my_summary <- function(df,var,fun,...) { df %>% pull({{var}}) %>% fun(...) }
q3 <- function(x,...){quantile(x,0.75,...)}
q1 <- function(x,...){quantile(x,0.25,...)}

nycflights13::flights %>% 
  group_by(carrier) %>% 
  nest %>% 
  mutate(my_min = map_dbl(data,my_summary,arr_delay,min,na.rm=TRUE)) %>%
  mutate(my_q1 = map_dbl(data,my_summary,arr_delay,q1,na.rm=TRUE)) %>% 
  mutate(my_med = map_dbl(data,my_summary,arr_delay,median,na.rm=TRUE)) %>% 
  mutate(my_q3 = map_dbl(data,my_summary,arr_delay,q3,na.rm=TRUE)) %>% 
  mutate(my_max = map_dbl(data,my_summary,arr_delay,max,na.rm=TRUE)) %>% 
  mutate(my_mean = map_dbl(data,my_summary,arr_delay,mean,na.rm=TRUE)) %>% 
  select(-data)

nycflights13::flights %>% 
  ggplot() +
  aes(x=carrier, y = arr_delay) +
  geom_boxplot()


bmnames <- rio::import("https://byuistats.github.io/M335/data/BoM_SaviorNames.rds") %>% pull(name)
# =
bmnames2 <- read_rds(gzcon(url("https://byuistats.github.io/M335/data/BoM_SaviorNames.rds")))

all.equal(bmnames, bmnames2)


scripture %>% 
  filter(volume_title == "Book of Mormon") %>% 
  pull(scripture_text) %>% str_flatten(collapse = " ")

sentences %>% 
  str_flatten(collapse = " ")

sentences %>% 
  str_flatten(collapse = " ") %>% 
  str_locate_all(pattern = "The|the|and")

sentences %>% 
  str_flatten(collapse = " ") %>% 
  str_split(pattern = "The|the|and")

sentences %>% 
  str_flatten(collapse = " ") %>% 
  str_split(pattern = "The |the |and ")

sentences %>% 
  str_flatten(collapse = " ") %>% 
  str_split(pattern = "The |the |and ") %>% 
  str_length()

sentences %>% 
  str_flatten(collapse = " ") %>% 
  str_split(pattern = "The |the |and ") %>% 
  unlist() %>% 
  str_length()

key_words <- c("The ","the ", "and ", "bank") #no need for a space after "bank". 
search_pattern <- key_words %>% str_flatten(collapse = "|")

sentences %>% 
  str_flatten(collapse = " ") %>% 
  str_split(pattern = search_pattern) %>% 
  unlist() %>% 
  str_length()

fruit <- c("apple", "banana", "pear", "pineapple")
str_locate_all(fruit, "a")
str_locate_all(fruit, "e")
str_locate_all(fruit, c("a", "b", "p", "p"))

