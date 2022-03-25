library(sf)
library(tidyverse)

nc <- read_sf(system.file("shape/nc.shp", package = "sf"))
plot(nc)

glimpse(nc)
plot(nc$geometry)
plot(nc$geometry[[1]]) #This just plots the polygon(s) in the first item in the list column

ggplot() +
  geom_sf(data = nc) # geom_sf(): finds geometry column

ggplot() +
  geom_sf(aes(fill = PERIMETER), data = nc, colour = "white") # border color = color

library(maps)
nz_map <- map("nz", plot = FALSE)
class(nz_map) ## not sf
class(nc)
plot(nz_map)

# convert "map" to "sf"
nz_sf <- st_as_sf(nz_map)
class(nz_sf)
plot(nz_sf)
ggplot() +
  geom_sf(data = nz_sf)

ggplot() +
  geom_sf(data = nz_sf) +
  geom_sf(data = nc)

## Gets the geometry
# install.packages("USAboundaries")
library(USAboundaries)
state_data <- us_states()
# https://stackoverflow.com/questions/62438256/using-usaboundaries
devtools::install_github("ropensci/USAboundariesData")
ggplot() +
  geom_sf(data = state_data)

state_data %>% View()
# Filter out the unneeded data and then plot. I'll let you tackle this in break out rooms. 

state_data <- state_data %>% filter(jurisdiction_type == "state") %>% 
  filter(state_abbr != "AK") %>% 
  filter(state_abbr != "HI")

idaho <- us_counties() %>% filter(state_name == "Idaho")

cities <- us_cities() %>% group_by(state_name) %>% arrange(desc(population)) %>% slice(1:3) %>% mutate(number = row_number()) %>% ungroup()

View(cities)

cities$long <- 1:nrow(cities) %>%
  map_dbl(function(x) cities$long[x] = cities$geometry[[x]][1])

cities$lat <- 1:nrow(cities) %>%
  map_dbl(function(x) cities$lat[x] = cities$geometry[[x]][2])

cities <- cities %>% filter(state_abbr != "HI") %>% 
  filter(state_abbr != "AK")

cities$population <- as.numeric(cities$population)

ggplot() +
  geom_sf(data = state_data, fill = NA) +
  geom_sf(data = idaho, fill = NA) +
  geom_point(cities, mapping = aes(x = long, y = lat, size = population/1000, color = number)) +
  scale_x_continuous(NULL) +
  scale_y_continuous(NULL) +
  labs(size = "Population\n(1000)") +
  guides(color = FALSE) +
  scale_size_manual(values = c('#00008B')) +
  ggrepel::geom_label_repel(aes(x = long, y = lat, label = city), data = cities %>% filter(number == 1), segment.color =  "darkblue", segment.size = 0.7, color = "darkblue", size = 2.5) +
  theme_bw()


