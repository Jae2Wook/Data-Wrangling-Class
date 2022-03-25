library(tidyverse)
library(lubridate)

my_URL <- "https://byuistats.github.io/M335/data/sales.csv"
my_data <- read_csv(my_URL)

my_data %>% 
  ggplot(aes(x = Time, y = Amount)) +
  geom_point()

my_data %>% 
  ggplot(aes(x = Time, y = Amount)) +
  geom_bin2d()

my_data %>% 
  mutate(Time = with_tz(Time, tzone = "America/Denver")) %>% 
  ggplot(aes(x = hour(Time), y = Amount)) +
  geom_point()


my_data %>% 
  mutate(Time = with_tz(Time, tzone = "America/Denver")) %>% 
  ggplot(aes(x = hour(Time), y = Amount)) +
  geom_bin2d()

my_data %>% 
  ggplot(aes(x = hour(Time), y = Name)) +
  geom_point()
my_data %>% 
  ggplot(aes(x = hour(Time), y = Name)) +
  geom_jitter()
my_data %>% 
  ggplot(aes(x = hour(Time), y = Name)) +
  geom_bin2d()

my_data %>% 
  mutate(month = month(Time)) %>% 
  group_by(month, Name) %>% 
  summarize(total_revenue = sum(Amount)) %>% 
  ggplot() +
  aes(x = month, y = total_revenue, color = Name) +
  geom_point() +
  geom_line()




