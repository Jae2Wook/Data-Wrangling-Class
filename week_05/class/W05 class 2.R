library(tidyverse)
library(readr)
library(haven)
library(readxl)
library(downloader)
url_sav <- "https://github.com/byuistats/data/blob/master/Dart_Expert_Dow_6month_anova/Dart_Expert_Dow_6month_anova.sav?raw=true"
url_xlsx <- "https://github.com/byuistats/data/blob/master/Dart_Expert_Dow_6month_anova/Dart_Expert_Dow_6month_anova.xlsx?raw=true"

my_sav <- read_sav(url_sav)
my_xlsx <- read_xlsx(url_xlsx)

# use tempfile() for xlsx because the file is for downloading
temp <- tempfile()
download(url_xlsx, destfile = temp, mode = "wb")
my_xlsx <- read_xlsx(temp)


all.equal(my_sav,my_xlsx)

all.equal(my_sav,my_xlsx, check.attributes = FALSE) # attributes: the method that save data


dat_xl <- read_xlsx("https://byuistats.github.io/M335/data/heights/Height.xlsx",skip = 2)
#?download
#We need mode = "wb" for cross platform compatibility. 
temp <- tempfile(fileext = "xlsx")
download("https://byuistats.github.io/M335/data/heights/Height.xlsx",temp, mode = "wb")
dat_xl <- read_xlsx(temp)
dat_xl %>% View()
#The skip = 2 allows us to start reading in the data from row 3 (where the column names are located). 
dat_xl <- read_xlsx(temp,skip = 2)
dat_xl %>% View()

xl <- dat_xl %>%
  glimpse() %>% 
  pivot_longer(
    cols = c(-Code,-`Continent, Region, Country`), 
    names_to = "year", 
    values_to = "height.cm"
  ) %>%
  glimpse() %>% 
  mutate(height.in = height.cm/2.54) %>%
  mutate(year = as.integer(year)) %>% #This line forces years to be ints, not char, so that plots appear nice.
  filter(year %% 10 == 0) %>% ##This line will immediately get rid of everything but the decade years. 
  mutate(country = `Continent, Region, Country`) %>% #Let's scrap the old name.
  select(Code,country,year,height.cm,height.in) %>% 
  glimpse()

xl %>%
  ggplot(aes(x = year,y = height.in)) +
  geom_point() +
  geom_point(
    data = xl %>% 
      filter(
        country == "Germany" | 
          country == "German Democratic Republic (until 1990)"
      ), 
    mapping = aes(x = year,y = height.in),
    color = 'yellow'
  ) +
  labs(
    title = "Estimated Height In Each Country by Decade", 
    subtitle = "compared to Germany",
    x = "Year",
    y = "Height (in.)")
