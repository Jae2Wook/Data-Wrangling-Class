# task 23
library(leaflet)

m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
m  # Print the map

library(tidyverse)
library(USAboundaries)
library(buildings)

permits %>% distinct(variable)

permits %>% 
  filter(variable == "Single Family") %>% 
  filter(year > 2000) %>% 
  group_by(StateAbbr, year) %>% 
  summarise(total_permits = sum(value)) %>% 
  ggplot() +
  aes(x = year, y  = total_permits) +
  geom_line(aes(group = StateAbbr)) +
  facet_wrap(~StateAbbr)


library(geofacet)

state_ranks %>% glimpse()

ggplot(state_ranks, aes(variable, rank, fill = variable)) +
  geom_col() +
  coord_flip() +
  facet_geo(~ state) +
  theme_bw()

sa_pop_dens %>% glimpse()
ggplot(sa_pop_dens, aes(factor(year), density, fill = factor(year))) +
  geom_col() +
  facet_geo(~ province, grid = "sa_prov_grid1") +
  labs(title = "South Africa population density by province",
       caption = "Data Source: Statistics SA Census",
       y = "Population density per square km") +
  theme_bw()

#  https://cran.r-project.org/web/packages/geofacet/vignettes/geofacet.html f

get_grid_names()

us_state_grid2

# label = code
# check us_state_grid2
us_state_grid2 %>% View()

election %>% glimpse()
ggplot(election, aes(candidate, pct, fill = candidate)) +
  geom_col() +
  scale_fill_manual(values = c("#4e79a7", "#e15759", "#59a14f")) +
  facet_geo(~ state, grid = "us_state_grid2", label = "code") +
  theme_bw() +
  coord_flip() +
  labs(title = "2016 Election Results",
       caption = "Data Source: http://bit.ly/2016votecount",
       x = NULL,
       y = "Percentage of Voters") +
  theme(strip.text.x = element_text(size = 6))


# ID

us_id_counties_grid1

permits %>% 
  filter(variable == "Single Family") %>% 
  filter(year > 2000) %>% 
  filter(StateAbbr == "ID") %>%  
  mutate(name = countyname) %>% 
  ggplot() +
  aes(x = year, y  = total_permits) +
  geom_line(aes(group = countyname)) +
  facet_geo(~countyname, grid = "us_id_counties_grid1")


