library(tidyverse)

#The rio package will simplify reading in Excel files. We need the third sheet in the dataset, so sheet = "Full data" provides the needed code. 
#install.packages("rio")
my_data <- rio::import("https://www.rug.nl/ggdc/historicaldevelopment/maddison/data/mpd2020.xlsx",sheet = "Full data")
my_data <- na.omit(my_data)
View(my_data)

#If the source site is down, you can also grab the data from a temporary repository created for this coding challenge. 
#my_data1 <- rio::import("https://byuimath.com/bmw/s21/mpd2020.xlsx",sheet = "Full data")
#View(my_data1)

#This line of code will read in the region table, which you'll need to assign each country to its appropriate region. 
my_region_table <- read_csv("https://byuimath.com/bmw/s21/my_region_table.csv")
View(my_region_table)



#######################
# This code will create my_region_table automatically, and is provided 
# for those interested in how to create region tables.  
#######################
#install.packages("countrycode")
library(countrycode)
my_region_table <-
  codelist_panel %>% 
  select(iso3c) %>% 
  distinct() %>% 
  mutate(
    country = countrycode(iso3c, origin = 'iso3c', destination = 'country.name.en'),
    region_un = countrycode(iso3c, origin = 'iso3c', destination = 'un.regionsub.name'),
    region_euro = countrycode(iso3c, origin = 'iso3c', destination = 'eurocontrol_pru'),
    region = case_when(
      region_euro == "Middle East" ~ region_euro,
      TRUE ~ region_un
    )
  ) %>% 
  filter(!is.na(region)) %>% 
  select(country,iso3c,region)

View(my_region_table)

#This table was created by using the United Nations sub-region name 
# (Middle East is not an option) along with the European Organisation for the 
# Safety of Air Navigation to get the regions listed as Middle East. 
# The UN designation was used for everything not tagged Middle East. 

colnames(my_region_table)[2] <- "countrycode"

my_region_table %>% distinct(region)

my_region_table$region <- stringr::str_replace(my_region_table$region, "Australia and New Zealand", "Western Offshoots")
my_region_table$region <- stringr::str_replace(my_region_table$region, "Northern America", "Western Offshoots")
my_region_table$region <- stringr::str_replace(my_region_table$region, "Eastern Asia", "East Asia")
my_region_table$region <- stringr::str_replace(my_region_table$region, "South-eastern Asia", "South and South-East Asia")
my_region_table$region <- stringr::str_replace(my_region_table$region, "Southern Asia", "South and South-East Asia")
my_region_table$region <- stringr::str_replace(my_region_table$region, "Latin America and the Caribbean", "Latin America")

sum(is.na(my_data))

#my_data[rowSums(is.na(my_data)) > 0,] %>% View()

my_data <- left_join(my_data, my_region_table %>% select(-country), by = "countrycode")
View(my_data)

my_regional <- my_data %>% group_by(year, region) %>% summarise(gdppc = weighted.mean(gdppc, pop, na.rm = TRUE))
my_regional <- my_regional %>% filter(year >= 1820)
my_regional <- my_regional %>% filter(region %in% c("Western Offshoots", "Western Europe", "Eastern Europe", "Middle East", "East Asia", "Latin America", "South and South-East Asia", "Sub-Saharan Africa"))

world_data <- my_data %>% group_by(year) %>% summarise(gdppc = sum(gdppc), pop = sum(pop), region = "World")
View(world_data)

my_regional <- na.omit(my_regional)

View(my_regional)

ggplot(my_regional, aes(year, gdppc, color = region)) +
  geom_point() +
  geom_line() +
  theme_bw()




