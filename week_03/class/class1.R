library(pacman)

pacman::p_load(tidyverse)

#Data located at https://github.com/nytimes/covid-19-data
#The RAW CSV files give direct access to data

covid_url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv"

covid_data <- 
  read_csv(covid_url) %>% 
  glimpse()

covid_data %>% 
  ggplot(aes(x = date, y = cases)) +
  geom_line()

daily <- covid_data %>% 
  mutate(daily_new_cases = cases - lag(cases))
daily <- daily[-c(1),]
View(daily)

daily %>% 
  ggplot() +
  geom_line(aes(x = date, y = daily_new_cases))

pacman::p_load(maps)

mpg %>% glimpse()

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = "blue"))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = class)

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity))
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")

######3

#Grab the state covid data 
covid_state_url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"

covid_state_data <- 
  read_csv(covid_state_url) %>% 
  glimpse()

covid_state_data %>% 
  filter(state %in% c("Idaho","Utah")) %>% 
  ggplot(aes(x = date, y = cases)) +
  geom_line() +
  facet_grid(state ~ .)
# a.
covid_state_data %>% 
  filter(state %in% c("Idaho","Utah")) %>% 
  ggplot(aes(x = date, y = cases)) +
  geom_line() +
  facet_grid(.~state)

# b.
covid_state_data %>% 
  filter(state %in% c("Idaho","Utah")) %>% 
  ggplot(aes(x = date, y = cases, color = state)) +
  geom_line()

# c.
covid_state_data %>% 
  filter(state %in% c("Idaho","Utah")) %>% 
  ggplot(aes(x = date, y = cases)) +
  geom_line() +
  facet_grid(.~state)


# e.
county <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"

county <- read_csv(county)
county %>% glimpse()


I <- county %>% filter(state %in% c("Idaho"))
I <- I %>% filter(county == c("Madison", "Bonneville"))
I
ggplot(I, aes(x = date, y = cases, color = county)) +
  geom_line() +
  facet_grid(~state)


# 2
ggplot(data = mpg, aes(x = displ, y = hwy, color = class, group = class)) + 
  geom_point(aes(size = cyl)) +
  geom_smooth()

# 3
View(mpg)
ggplot(mpg, aes(x = class, y = cty)) +
  geom_boxplot() +
  coord_flip()

# 4
library(readr)
#library(ggplot2)

rcw <- read_csv("https://byuistats.github.io/M335/data/rcw.csv", 
                col_types = cols(Semester_Date = col_date(format = "%m/%d/%y"), 
                                 Semester = col_factor(levels = c("Winter", "Spring", "Fall"))))
head(rcw)

ggplot(data = rcw, 
       mapping = aes(x = Semester_Date, 
                     y = Count, 
                     color = Department)) +
  geom_line() +
  geom_point()

ggplot(data = rcw, 
       mapping = aes(x = Semester_Date,
                     fill= Department)) +
  geom_bar()

ggplot(data = rcw, 
       mapping = aes(x = Semester_Date,
                     y = Count,
                     fill= Department)) +
  geom_col()
