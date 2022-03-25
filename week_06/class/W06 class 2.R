library(tidyverse)
library(ggrepel)
library(directlabels)
library(ourworldindata)

# .libPaths("C:/Program Files/R/R-4.0.3/library")

financing_healthcare %>% 
  filter(!is.na(continent)) %>% 
  select(country) %>%  
  unique()

########################################
#            CLASS ACTIVITY            #
########################################

# Your goal is to match this graph as closely as possible:
# https://ourworldindata.org/grapher/carbon-footprint-travel-mode?tab=chart&stackMode=absolute&region=World

# make sure you load the packages you need
library(tidyverse)

# you'll need to download the data from the website first
carbon <- read_csv("https://raw.githubusercontent.com/BYUI335/M335_WI21_Woodruff_Ben/master/week_06_relational-data/carbon-footprint-travel-mode.csv?token=AAMFLQXQ2ATMKSMMNG7Q7FLAW7NZ4")

# an example of a very simple bar chart
carbon %>% 
  ggplot(aes(x = `GHG emissions (gCO2e/km)`, y = Entity)) +
  geom_col()



########## DATA WRANGLING ##########

# these vectors will help with the graph
flights <- c("Bus",
             "Long-haul flight (economy)",
             "Short-haul flight (economy)")
public <- c("National rail",
            "Ferry (foot passenger)",
            "Eurostar (international rail)")
private <- c("Medium car (diesel)",
             "Medium car (petrol)",
             "Motorcycle (medium)",
             "Petrol car, 2 passengers",
             "Medium electric vehicle (UK electricity)",
             "Domestic flight")
chart_entities <- c("Domestic flight",
                    "Medium car (petrol)",
                    "Medium car (diesel)",
                    "Short-haul flight (economy)",
                    "Long-haul flight (economy)",
                    "Bus",
                    "Taxi",
                    "Black cab (taxi)",
                    "Motorcycle (medium)",
                    "Petrol car, 2 passengers",
                    "Medium electric vehicle (UK electricity)",
                    "National rail",
                    "Ferry (foot passenger)",
                    "Eurostar (international rail)")

# data wrangling
carbon <- carbon %>% 
  mutate(grams = round(`GHG emissions (gCO2e/km)`*100),
         mode = case_when(
           Entity %in% flights ~ "flight",
           Entity %in% public ~ "public",
           Entity %in% private ~ "private",
           TRUE ~ "other"
         ))




########## MAKE the PLOT ##########

View(carbon)

carbon %>% 
  filter(Entity %in% chart_entities) %>% 
  
  # basic plot
  ggplot(aes(x = grams, y = reorder(Entity, grams), fill = mode)) + # -grams changes order
  geom_col() +
  
  # add labels
  labs(x = "",
       y = "",
       title = "Carbon footprint of travel per kilometer, 2018",
       subtitle = "",
       caption = "Source: UK Department for Business, Energy & Industrial Strategy. Greenhouse gas reporting: conversion factors 2019.\nNote: Data is based on official conversion factors used in UK reporting. These factors may vary slightly depending on the country, and assumed\noccupancy of public transport such as buses and trains.") +
  
  # adjust the color
  scale_color_manual(values = c("flight" = "#ab3345",
                                "private" = "#835ca2",
                                "public" = "#279792",
                                "other" = "#d9bd4c")) +
  
  # make sure the x-axis matches
  scale_x_continuous(limits = c(0,350),
                     breaks = seq(0,250,50),
                     labels = function(x){paste(x, "grams")},
                     expand = c(0,0)) +
  
  # label each bar
  geom_text(aes(label = paste(grams, "g")), nudge_x = 10, size = 3) +
  
  # final touches
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted"),
        plot.caption = element_text(hjust = 0))
