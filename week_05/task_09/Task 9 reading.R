# Tidy Data
# 1) Each variables must have its own column
# 2) Each observation must have its own row
# 3) Each value must have its own cell

library(tidyverse)

# Put each dataset in a tibble
# Put each variable in a column

# compute rate per 10,000
table1 %>% mutate(rate = cases / population * 10000)

# compute cases per year.
table1 %>% count(year, wt = cases)
?count

ggplot(table1, aes(year, cases)) +
  geom_line(aes(group = country), color = "grey50") +
  geom_point(aes(color = country))

# Pivoting
table4a

# longer
#columns are variables --> need variables
table4a %>% 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "cases")
# columns --> names_to, values to another column --> values_to

table4b %>% 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "population")

tidy4a <- table4a %>% 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "cases")
tidy4b <- table4b %>% 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "population")

left_join(tidy4a, tidy4b)

# wider
table2

table2 %>% 
  pivot_wider(names_from = type, values_from = count)

stocks <- tibble(
  year   = c(2015, 2015, 2016, 2016),
  half  = c(   1,    2,     1,    2),
  return = c(1.88, 0.59, 0.92, 0.17)
)

stocks %>% 
  pivot_wider(names_from = year, values_from = return) %>% 
  pivot_longer(`2015`: `2016`, names_to = "year", values_to = "return")

table4a %>% 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "cases")

people <- tribble(
  ~name,             ~names,  ~values,
  #-----------------|--------|------
  "Phillip Woods",   "age",       45,
  "Phillip Woods",   "height",   186,
  "Jessica Cordero", "age",       37,
  "Jessica Cordero", "height",   156
)

people %>% 
  pivot_wider(names_from = names, values_from = values)

preg <- tribble(
  ~pregnant, ~male, ~female,
  "yes",     NA,    10,
  "no",      20,    12
)

preg %>% 
  pivot_longer(c(male, female), names_to = "gender", values_to = "numbers")

# Separating and uniting

# Separating
table3
table3 %>% 
  separate(rate, into = c("cases", "population"))
# separate() will split values wherever it sees a non-alphanumeric character (number nor letter)
table3 %>% 
  separate(rate, into = c("cases", "population"), sep = "/")

# convert to better types
table3 %>% 
  separate(rate, into = c("cases", "population"), convert = TRUE)

table3 %>% 
  separate(year, into = c("century", "year"), sep = 2)

# Unite
table5 %>% 
  unite(new, century, year)
 table5 %>% 
   unite(new, century, year, sep = "")

# Missing values

stocks <- tibble(
   year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
   qtr    = c(   1,    2,    3,    4,    2,    3,    4),
   return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
 )

stocks %>% 
  pivot_wider(names_from = year, values_from = return)

stocks %>% 
  pivot_wider(names_from = year, values_from = return) %>% 
  pivot_longer(c(`2015`, `2016`),
               names_to = "year",
               values_to = "return",
               values_drop_na = TRUE)

# complete() takes a set of columns, and finds all unique combinations.
# It then ensures the origianl dataset contains all those values, fillign in NA's
stocks %>% 
  complete(year, qtr)


treatment <- tribble(
  ~ person,           ~ treatment, ~response,
  "Derrick Whitmore", 1,           7,
  NA,                 2,           10,
  NA,                 3,           9,
  "Katherine Burke",  1,           4
)

# fill(): replace by the most recent non-missing value
treatment %>% 
  fill(person)

who
who1 <- who %>% 
  pivot_longer(new_sp_m014:newrel_f65, names_to = "key", values_to = "cases", values_drop_na = TRUE)

who1 %>% 
  count(key)

who2 <- who1 %>% 
  mutate(key = stringr::str_replace(key, "newre1", "new_re1"))
glimpse(who2)

who3 <- who2 %>% 
  separate(key, c("new", "type", "sexage"), sep = "_")

who3 %>% 
  count(new)

glimpse(who3)
who4 <- who3 %>% 
  select(-new, -iso2, -iso3)

who5 <- who4 %>% 
  separate(sexage, c("sex", "age"), sep = 1)
glimpse(who5)

who %>% 
  pivot_longer(
    new_sp_m014:newrel_f65,
    names_to = "key",
    values_to = "cases",
    values_drop_na = TRUE
  ) %>% 
  mutate(
    key = stringr::str_replace(key, "newre1", "new_re1")
  ) %>% 
  separate(key, c("new", "var", "sexage")) %>% 
  select(-c(new, iso2, iso3)) %>% 
  separate(sexage, c("sex", "age"), sep = 1)


# Writing to File
# write_csv()
# or
# write_tsv()
# Always encoding strings in UTF-8
# Saving dates and date-times in ISO8601 format so they are easily parsed elsewhere.

# export a csv file to Excel: write_excel_csv()

write_csv(challenge, "challenge.csv")

# write_rds() and read_rds()


# Save an ogject to a file
saveRDS(ogject, file = "my_data.rds")
# Restore the object
readRDS(file = "my_data.rds")

saveRDS(mtcars, "mtcaes.rds")
my_data <- readRDS("mtcars.rds")

# Save multiple objects
save(data1, data2, rile = "data.RData")
# To load the data again
load("data.RData")

save.image(file = "my_work_space.RData")
load("my_work_space.RData")

###########
# to read .rds file in GitHub
library(readr)
data <- read_rds(url("https://github.com/byuistats/data/blob/master/Dart_Expert_Dow_6month_anova/Dart_Expert_Dow_6month_anova.RDS?raw=true"))
View(data)

# First, create a time series plot that shows 
# the six-month returns (y-axis) across time (x-axis) for PROS, DARTS, and DJIA
ggplot(data, aes(x = contest_period, y = value, group = variable, color = variable)) +
  geom_line()

# Now we need to "tidy" the data. Use the contestant_period column to create 
# new month_end and year_end columns. (Try using separate() and/or extract() from tidyr.
# (Save your tidy data as an .rds object)

tidy_data <- data %>% 
  separate(contest_period, c("month_start", "month_end_year"), sep = "-") %>% 
  separate(month_end_year, c("month_end", "year_end"), sep = -4) %>% 
  select(-month_start)

saveRDS(tidy_data, "tidy_data.rds")

# Using the new columns created above, write code that outputs a tibble of 
# the DJIA returns that matches (as closely as possible) the table shown below 
# (aka, “pivot_wider” the data)

DJIA_data <- tidy_data %>% filter(variable == "DJIA")

DJIA_data %>% distinct(month_end)

DJIA_data <- DJIA_data %>% mutate(month_end = stringr::str_replace(month_end, "Dec.", "December")) %>% 
  mutate(month_end = stringr::str_replace(month_end, "Febuary", "February")) %>% 
  mutate(month_end = stringr::str_replace(month_end, "Decembermber", "December"))
DJIA_data %>% distinct(month_end)

DJIA_wide <- DJIA_data %>% 
  pivot_wider(names_from = year_end, values_from = value) %>% 
  mutate(Month = case_when(
    month_end %in% "January" ~ 1,
    month_end %in% 'February' ~ 2,
    month_end %in% "March" ~ 3,
    month_end %in% "April" ~ 4,
    month_end %in% "May" ~ 5,
    month_end %in% "June" ~ 6,
    month_end %in% "July" ~ 7,
    month_end %in% "August" ~ 8,
    month_end %in% "September" ~ 9,
    month_end %in% "October" ~ 10,
    month_end %in% "November" ~ 11,
    month_end %in% "December" ~ 12
  )) %>% 
  arrange(Month) %>% select(-c(variable, Month))

colnames(DJIA_wide)[which(names(DJIA_wide) == "month_end")] <- "Month"

library(knitr)
library(pander)
kable(DJIA_wide)
pander(DJIA_wide)






