library(tidyverse)
library(gapminder)
library(ggplot2)


gap <- gapminder %>% filter(country != "Kuwait")

# Recreate the graphic shown below using gapminder dataset
sum(is.na(gap))

gap1 <- gap %>% group_by(continent, year) %>% summarise(wt_avg_gdp = weighted.mean(gdpPercap, pop), pop = sum(pop))

  
ggplot(gap) +
  geom_point(aes(x = year, y = gdpPercap, group = country, size = pop, color = continent)) +
  geom_line(aes(x = year, y = gdpPercap, group = country, color = continent)) +
  geom_line(data = gap1, mapping = aes(x = year, y = wt_avg_gdp)) +
  geom_point(data = gap1, mapping = aes(x = year, y = wt_avg_gdp, size = pop)) +
  facet_wrap(.~continent, nrow = 1) +
  labs(x = "Year", y = "GDP per capita", color = "Continent", size = "Population (100k)") +
  guides(size = guide_legend(order = 1), color = guide_legend(order = 2)) +
  scale_size(range = c(0.1, 5), breaks = 100000 * c(10000, 20000, 30000), labels = c("10000", "20000", "30000")) +
  theme_bw()
  
  

# Visualize Africa, Americas and Asia life expectancy over time.

# Within each continent find which country had the 
# largest overall increase in life expectancy from 1952 to 2007

three <- gap %>% filter(continent %in% c("Africa", "Americas", "Asia"))
life_chng <- three %>% group_by(country) %>% slice(1, n())

life_chng$change <- ((life_chng %>% group_by(country) %>% filter(year == 2007))$lifeExp - (life_chng %>% group_by(country) %>% filter(year == 1952))$lifeExp) / (life_chng %>% group_by(country) %>% filter(year == 2007))$lifeExp * 100

yr_1952 <- filter(life_chng, year == 1952)
yr_2007 <- filter(life_chng, year == 2007)
change <- (yr_2007$lifeExp - yr_1952$lifeExp) / yr_1952$lifeExp * 100
tb <- tibble(yr_1952$country, yr_1952$continent, change)
View(tb)

large_coun <- tb %>% group_by(`yr_1952$continent`) %>% top_n(n = 1, wt = change) %>% pull(`yr_1952$country`)
# Within each continent find which country had the 
# smallest overall increase (or even a decrease) from 1952 to 2007

small_coun <- tb %>% group_by(`yr_1952$continent`) %>% top_n(n = -1, wt = change) %>% pull(`yr_1952$country`)

# Display these country's life expectancy as it has changed 
# over the years (you should have 6 countries in this plot)

six <- gap %>% filter(country %in% large_coun | country %in% small_coun)
six <- six %>% mutate(hi_lo = ifelse(country %in% large_coun, "Largest Increase", "Smallest Increase"))
six_max_date <- six %>% filter(year == min(year))

ggplot(six, aes(x = year, y = lifeExp, group = country, color = hi_lo)) +
  geom_point() +
  geom_line() +
  labs(x = "Year", y = "Life Expectency", title = "Countries with Largest and Smallest\nLife Expecency Change Change", color = "Group")+
  ggrepel::geom_label_repel(data = six_max_date, mapping = aes(label = country)) +
  theme_bw()


  