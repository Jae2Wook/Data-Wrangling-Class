pacman::p_load(tidyverse)
iris
iris %>% 
  arrange(Sepal.Length) %>% 
  slice((n() - 4): n()) # last 5 -  n(): counts numbers of data set

iris %>% 
  filter(Species %in% c("setosa","versicolor")) %>% 
  group_by(Species) %>% 
  summarise(
    count = n(),
    med = median(Sepal.Width, na.rm = TRUE)
  ) %>% 
  ungroup()

iris %>% 
  filter(Species %in% c("setosa","versicolor")) %>% 
  group_by(Species) %>% 
  slice(1:2)

library(bakeoff)
only_first_last <- ratings %>% 
  group_by(series) %>% 
  slice(1, n()) %>% 
  mutate(which_episode = ifelse(episode == 1, "First", "Last")) %>% 
  ungroup() %>% 
  mutate(series_f = as.factor(series))

View(only_first_last)

ggplot(data = only_first_last, 
       mapping = aes(x = which_episode, 
                     y = viewers_7day, 
                     group = series_f,
                     color = series_f)) +
  geom_line() +
  geom_point(size = 5)


iris %>% arrange(Sepal.Length) %>% 
  slice(1:6)

testdat <- iris %>% select(Petal.Width, Sepal.Width)
testdat
