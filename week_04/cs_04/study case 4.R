library(nycflights13)
library(tidyverse)
library(pander)

View(flights)

# Q1) If I am leaving before noon, which two airlines do you recommend
# at each airport (JFK, LGA, EWR) that will have the lowest delay 
# time at the 75th percentile?

# People will buy a ticket by looking at the scheduled departure time.

flights_noon <- flights[flights$sched_dep_time < 1200,]
View(flights_noon)
sum(is.na(flights_noon$sched_dep_time))
sum(is.na(flights_noon$dep_time))
flights_noon <- flights_noon %>% drop_na()
View(flights_noon)
sum(is.na(flights_noon))

flights_noon %>% group_by(carrier) %>% count(carrier)
flights_noon <- flights_noon %>% filter(carrier != "OO")

flights_noon_con <- flights_noon %>% group_by(origin, carrier) %>% count(dep_delay <= 0)
View(flights_noon_con)
delay_T <- flights_noon_con %>% group_by(origin, carrier) %>% slice(2)
delay_F <- flights_noon_con %>% group_by(origin, carrier) %>% slice(1)
percen <- (delay_T$n) / (delay_T$n + delay_F$n) * 100
tb <- tibble(delay_T$origin, delay_T$carrier, delay_T$`dep_delay <= 0`, delay_T$n, delay_F$`dep_delay <= 0`, delay_F$n, percen)

percen_over_75 <- tb %>% group_by(`delay_T$origin`, `delay_T$carrier`) %>% filter(percen >= 75)

ggplot(percen_over_75, aes(x = `delay_T$carrier`, y = percen, fill = `delay_T$carrier`)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(y = "On-time Percentage", x = "Carriers", fill = "Carriers", title = "Lowest Delay Time Airlines \nthat are Over 75 Percentile by Airports") +
  facet_wrap(.~`delay_T$origin`) +
  theme_bw()

top_2 <- percen_over_75 %>% group_by(`delay_T$origin`) %>% top_n(n = 2, wt = percen)
top_2 <- top_2 %>% select(`delay_T$origin`, `delay_T$carrier` , percen )
  
# Q2) Which origin airport is best to minimize my chances of a 
# late arrival when I am using Delta Airlines (DL)?

flights_DL <- flights %>% filter(carrier == "DL")

sum(is.na(flights_DL))

flights_DL <- flights_DL %>% drop_na()

flights_DL1 <- flights_DL %>% group_by(origin) %>% count(arr_delay <= 0)

DL_T <- flights_DL1 %>% group_by(origin) %>% slice(2)
DL_F <- flights_DL1 %>% group_by(origin) %>% slice(1)
percent <- DL_T$n / (DL_T$n + DL_F$n) * 100

tb_DL <- tibble(DL_T$origin, DL_T$`arr_delay <= 0`, DL_T$n, DL_F$`arr_delay <= 0`, DL_F$n, percent)

ggplot(tb_DL, aes(x = `DL_T$origin`, y = percent, fill = `DL_T$origin`)) +
  geom_bar(stat = "identity") +
  labs(x = "Origins", y = "Percentage of Arriving On-Time", title = "On-Time Arriving Percentage\nin Each Airport") +
  theme_bw() +
  theme(legend.position = "none")

DLDL <- tibble(tb_DL$`DL_T$origin`, tb_DL$percent)
colnames(DLDL) <- c("Origin", "On-time Percentage")
DLDL %>% pander()

# `JFK` origin airport is best to minimize my chances of a 
# late arrival when using Delta Airlines (DL)

# Q3) Which destination airport is the worst airport 
# for arrival time? (you decide on the metric for "worst")



