library(gapminder)
library(tidyverse)

my_dat <- 
  mutate(gapminder, pop100k = pop/100000) %>% 
  filter(country != "Kuwait")

(my_dat <- 
    mutate(gapminder, pop100k = pop/100000) %>% 
    filter(country != "Kuwait")) # (): Shows data in console


(my_sum <- my_dat %>% 
    group_by(continent, year) %>% 
    summarise(mean_gdp = weighted.mean(gdpPercap, pop100k)))
(my_sum <- my_dat %>% 
    group_by(continent, year) %>% 
    summarise(mean_gdp = weighted.mean(gdpPercap, pop100k), country = country, pop100k = pop100k))

(my_dat2 <- gapminder %>% 
    filter(country != "Kuwait") %>% 
    mutate(pop100k = pop/100000) %>% 
    group_by(continent, year) %>% 
    mutate(mean_gdp = weighted.mean(gdpPercap, pop100k)) %>% 
    ungroup())

my_plot <- ggplot(my_dat, aes(x = year, y = gdpPercap))

my_plot + geom_point()

my_plot + geom_point(mapping = aes(color = continent, size = pop100k))

my_plot + 
  geom_point(mapping = aes(color = continent, size = pop100k)) +
  geom_line(mapping = aes(group = country, color = continent)) +
  facet_grid(.~continent)
