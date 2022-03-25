# https://otexts.com/fpp2/ : forcasting data

library(readr)
library(lubridate)
library(tidyverse)

data <- read_csv("https://byuistats.github.io/M335/data/sales.csv")
View(data)

data$Time <- with_tz(data$Time, tz = "America/Denver")

data %>% str_count("Missing")
mis <- which(data$Name %in% "Missing")

data <- data[-mis,]
View(data)
data %>% str_count("Missing")

#data$daily <- format(data$Time, format = "%Y-%m-%d") %>% as.Date()
data$daily <- floor_date(data$Time, "day") %>% as.Date()
data$weekly <- floor_date(data$Time, "week") %>% as.Date()
# data$monthly <- format(data$Time, format = "%Y-%m")
data$monthly <- floor_date(data$Time, "month") %>% as.Date()
data$hours <- round_date(data$Time, "hour") %>% update(yday = 1) %>% format(format = "%H")


data %>% group_by(daily) %>% sum(Amount) # why not working?

min(data$daily)

# daily
daily <- data %>% group_by(Name,Type, daily) %>% summarise(sum = sum(Amount))
View(daily)
min(data$daily)
max(data$daily)

ggplot(daily, aes(daily, sum, fill = Type)) +
  geom_bar(stat = "identity") +
  scale_x_date(limits = c(min(data$daily), max(data$daily)), date_labels = "%B %d %Y") +
  facet_wrap(.~Name, nrow = 1) +
  theme_bw() +
  labs(title = "Daily Production Amount by Companies", x = "Date", y = "Amount") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# weekly
weekly <- data %>% group_by(Name,Type, weekly) %>% summarise(sum = sum(Amount))
View(weekly)
min(weekly$weekly)
max(weekly$weekly)


ggplot(weekly, aes(weekly, sum, fill = Type)) +
  geom_bar(stat = "identity") +
  facet_wrap(.~Name, nrow = 1) +
  scale_x_date(limits = c(min(weekly$weekly), max(weekly$weekly)), date_labels = "%B %d %Y") +
  theme_bw() +
  labs(title = "Weekly Production Amount by Companies", x = "Date", y = "Amount") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# monthly
monthly <- data %>% group_by(Name,Type, monthly) %>% summarise(sum = sum(Amount))
View(monthly)

min(monthly$monthly)
max(monthly$monthly)

ggplot(monthly, aes(monthly, sum, fill = Type)) +
  geom_bar(stat = "identity") +
  facet_wrap(.~Name, nrow = 1) +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank()) +
  labs(title = "Monthly Production Amount by Companies", x = "Date", y = "Amount")

# Provide a visualization that gives insight into hours of operation for each company.
hourly <- data %>% group_by(Name, Type, hours) %>% summarise(sum = sum(Amount))
View(hourly)

ggplot(hourly, aes(Name, sum, fill = Type)) + 
  geom_bar(stat = "identity") +
  facet_wrap(.~hours) +
  theme_bw() +
  labs(title = "Hourly Production Amount by Companies", x = "Company", y = "Amount") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = -0.02))
  

# We don’t have employee numbers, but customer traffic (number of transactions) may be helpful. Provide a visualization on customer traffic for each company.





