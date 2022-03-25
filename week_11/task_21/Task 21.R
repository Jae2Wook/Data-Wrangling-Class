library(USAboundaries)
library(sf)
library(ggplot2) # geom_sf()
library(tidyverse)

# always set quiet = TRUE, stringsAsFactors = FALSE
nc <- read_sf(system.file("shape/nc.shp", package = "sf"), quiet = TRUE, stringsAsFactors = FALSE)

# .shp: contains the geometry
# .shx: contains an index int othat geometry
# .dbf: contains metadata about each geometry
# .prf: contains the coordinate system and projection information

library(maps)
# If I get a patial object created by another package, use st_as_sf() to convert into sf
nz_map <- map("nz", plot = FALSE)
View(nz_map)
# st_as_sf(): Convert foreign object to an sf object
nz_sf <- st_as_sf(nz_map)
View(nz_sf)

head(nc)

nc$geometry

plot(nc$geometry)

nz_sf %>% 
  mutate(area = as.numeric(st_area(geometry))) %>% 
  filter(area > 1e10)

str(nc$geometry)

plot(nc$geometry[[1]])

n <- nc$geometry %>% map_int(length)
table(n)

interesting <- nc$geometry[n == 3][[1]]
plot(interesting)

str(interesting)

# need to know exact what the numeric positions mean.
# --> coordinate reference system (crs)
# often spatial data is described in latitude and longitude
st_is_longlat(nc)

st_crs(nc)

nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

View(nc)

ggplot() +
  geom_sf(data = nc, inherit.aes = FALSE,
          fill = NA, color = "black", size = 0.1
  )

View(states)

ggplot() +
  geom_sf(aes(fill = AREA), data = nc, color = "white")

ggplot() +
  geom_sf(data = states) +
  geom_sf(data = nc)

ggplot() +
  geom_sf(data = nc) +
  annotate("point", x = -80, y = 35, color = "red", size = 3) # annotate("text", label = "Some text")

ggplot() +
  geom_sf(data = nc) +
  annotate("point", x = -80, y = 35, color = "red", size = 3) +
  coord_sf(xlim = c(-81, -79), ylim = c(34, 36))

ggplot() +
  geom_sf(data = states) +
  coord_sf(crs = st_crs(102003)) # not working?

# US boundaries
states_1840 <- us_states("1840-03-12") # Not working
plot(st_geometry(states_1840))
title("U.S. states boundaries on March 3, 1840")

states_contemporary <- us_states()
plot(st_geometry(states_contemporary))
title("Contemporary U.S. state boundaries")

counteis_va_1787 <- us_counties("1787-09-17", states = "Virginia") # not working
plot(st_geometry(counteis_va_1787))
title("County boundaries in Virginia in 1787")

counties_va <- us_counties(states = "Virginia")
plot(st_geometry(counties_va))
title("Contemporary county boundaries in Virginia")

counties_va_highres <- us_counties(states = "Virginia", resolution = "high")
plot(st_geometry(counties_va_highres))
title("Higher resolution contemporary county boundaries in Virginia")

congress <- us_congressional(states = "California")
plot(st_geometry(congress))
title("Congressional district boundaries in California")

# State planes projections
va <- us_states(states = "VA", resolution = "high")
plot(st_geometry(va), graticule = TRUE)

va_projection <- state_plane("VA")
va <- st_transform(va, va_projection)
plot(st_geometry(va), graticule = TRUE)


### Task

# https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
# https://stackoverflow.com/questions/58228137/how-can-i-fill-na-values-in-a-ggplot-geom-map-with-zigzags-or-oblique-lines

library(USAboundariesData)
library(ggrepel)

states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
View(states)
#id <- read_sf(system.file("shape/idaho.shp", package = "sf"), quiet = TRUE, stringsAsFactors = FALSE)
#id <- read_sf(system.file("shape/id.shp", package = "sf"), quiet = TRUE, stringsAsFactors = FALSE)

us_cities() %>% View()
us_states() %>% View()
us_counties() %>% View()

idaho <- us_counties() %>% filter(state_name == "Idaho")
View(idaho)

ggplot() +
  geom_sf(data = states, fill = NA) +
  geom_sf(data = idaho) +
  geom_point(us_cities, mapping = aes(size = population))
  ggrepel::geom_label_repel(data = us_cities, mapping = aes(label = city)

