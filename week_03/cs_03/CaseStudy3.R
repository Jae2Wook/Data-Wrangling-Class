library(gapminder)
library(tidyverse)

View(gapminder)

# Pick one quantitative variable and create a chart 
# that summarizes the distribution. Kuwait
ggplot(gapminder, aes(x = year, y = gdpPercap, by = country)) +
  geom_line(aes(color = continent)) +
  geom_line(gapminder[gapminder$country == "Kuwait",], mapping = aes(color = country), size = 2)
# same
ggplot(data=gapminder, aes(x=year, y=gdpPercap, by=country)) + 
  geom_line(aes(color=continent)) +
  geom_line(data=gapminder[gapminder$country == "Kuwait",], aes(color=country), size=2)


a <- gapminder %>% group_by(continent) %>% filter(continent == "Africa")
a$gdpPercap[which(a$gdpPercap == max(a$gdpPercap))]

Afri <- filter(gapminder, continent %in% c("Africa"))
Afri <- filter(Afri, gdpPercap == max(gdpPercap))
Afri

Asia <- filter(gapminder, continent %in% c("Asia"))
Asia <- filter(Asia, gdpPercap == max(gdpPercap))
Asia

Amer <- filter(gapminder, continent %in% c("Americas"))
Amer <- filter(Amer, gdpPercap == max(gdpPercap))
Amer

Eu <- filter(gapminder, continent %in% c("Europe"))
Eu <- filter(Eu, gdpPercap == max(gdpPercap))
Eu

ggplot(gapminder, aes(x = year, y = gdpPercap, by = country)) +
  geom_line(aes(color = continent)) +
  geom_line(data = gapminder[gapminder$country == "Norway",], aes(color = continent), size = 2)

# Pick one qualitative variable and create a chart 
# that summarizes the distribution.
ggplot(gapminder %>% group_by(continent) %>% distinct(country), aes(x = continent)) +
  geom_bar()

gapminder %>% group_by(continent) %>% distinct(country) %>% count(continent)
count(gapminder %>% distinct(country))

# Pick any two variables and create a chart that summarizes 
# the bivariate distribution (the relationship between the two).
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(aes(color = continent)) +
  coord_cartesian(xlim = c(0, 55000))


# Write a description for each of the three charts 
# above about what you learn from the chart.

# Norway ended up being the highest GDP per capita nation.
# Africa nations are more recorded than the other nations. 
# There are some nations that GDP per capita is small but life expectancy is high.
# However, the relationship looks like as GDP per capita increases, life expectancy increases with diminishing degree. 


# Rebuild


ggplot(filter(gapminder, country != "Kuwait"), aes(x = lifeExp, y = gdpPercap)) +
  geom_point(aes(size = pop, color = continent)) +
  facet_wrap(~year, nrow = 1) +
  scale_y_continuous(trans = "sqrt") +
  labs(x = "Life Expectancy", y = "GDP per capita", size = "Population (100k)", color = "continent") +
  guides(size = guide_legend(order = 1), color = guide_legend(order = 2)) +
  scale_size(range = c(0.1, 10), breaks = 100000 * c(2500, 5000, 7500, 10000, 12500), labels = c("2500", "5000", "7500", "10000", "12500")) +
  theme_bw()

ggsave("case3.png", width = 15, units = "in")

# additional info for scaling
# http://www.rebeccabarter.com/blog/2017-11-17-ggplot2_tutorial/
