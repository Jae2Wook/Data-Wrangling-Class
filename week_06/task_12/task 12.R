library(tidyverse)
library(ourworldindata)

View(financing_healthcare)

health <- financing_healthcare[-1,]

# select country
health %>% count(country)

in_use <- health %>% count(country) %>% filter(n > 5) %>% pull(country)

# select columns becasue of lots of NA
# total rows: 36873

chekc_na <- health %>%
  select(everything()) %>%  # replace to your needs
  summarise_all(funs(sum(is.na(.))))

health <- health %>% select(year, country, continent, child_mort)# %>% drop_na()
health <- health %>% filter(country %in% as.factor(in_use)) %>% drop_na()

View(health)

health_con <- health %>% group_by(continent) %>% distinct(country)
ggplot(health_con, aes(x = continent)) +
  geom_bar()

max(health$child_mort)

# Create at least three graphics that help you explore 
# the data and understand child mortality (child_mort column). 

ggplot(health, aes(x = year, y = child_mort, group = country)) +
  geom_point() +
  geom_line()

ggplot(health, aes(y = child_mort)) +
  geom_boxplot()

#####
bin_size <- 10

cars %>% 
  mutate(bin_dist = factor(dist%/%bin_size*10)) %>% 
  ggplot(aes(x = bin_dist, y = speed)) +
  geom_boxplot()

ggplot(cars, aes(x = dist, y = speed)) +
  geom_boxplot()
#####


ggplot(health, aes(x = year, y = child_mort, color = continent)) +
  geom_jitter() +
  facet_wrap(.~continent) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



View(health_bin <- health %>% mutate(year_bin = factor(year%/%20 * 20)))

ggplot(health_bin, aes(x = year_bin, y = child_mort)) +
  geom_boxplot() +
  facet_wrap(.~continent) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_cartesian(ylim = c(0, 770))

ggplot(health_bin, aes(x = year_bin, y = child_mort, group = continent)) +
  geom_smooth() +
  facet_wrap(.~continent) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_cartesian(ylim = c(0, 770))

# Visualization for Presentation. Create a graphic you 
# could use in a final presentation to communicate something 
# interesting about child mortality.

health_bin <- health_bin %>% mutate(child_mort_percentage = child_mort/1000 * 100)

ggplot(health_bin, aes(x = year_bin, y = child_mort_percentage)) +
  geom_boxplot(aes(color = continent)) +
  geom_smooth(aes(group = continent)) +
  facet_wrap(.~continent) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_cartesian(ylim = c(0, 80)) +
  theme(legend.position = "none", panel.grid = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = c(20, 40, 60, 80), linetype = "dashed") +
  geom_hline(yintercept = c(0), linetype = "solid") +
  labs(title = "Child Mortality by Time Series", x = "Year", y = "Child Mortality Rate")





