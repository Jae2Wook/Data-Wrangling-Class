library(tidyverse)
library(leaflet)

# Introduction

# Create a map widget by calling leaflet().
# Add layers (i.e., features) to the map by using layer functions 
# (e.g. addTiles, addMarkers, addPolygons) to modify the map widget.

leaflet() %>% 
  addTiles() %>%  # add default OpenStreetMap map file
  addMarkers(lng=174.768, lat=-36.852, popup = "The birthplace of R")

# Map widget

leaflet(options = leafletOptions(minZoom = 0, maxZoom = 18))

# setView(): sets the center of the map view nad the zoom level
# fitBound(): fits the view into the rectangle [lng1, lat1] - [lng2, lat2]
# clearBound(): clears bound

# leaflet() and the map layer functions have an optional data parameter that is designed to receive spatial data in one of several forms:
# Base R: lng/lat matrix, data frame with lng,lat columns.
# sp package: SpatialPoints[DataFrame], Line/Lines, SpatialLines[DataFrame], Polygon/Polygons, SpatialPolygons[DataFrame]

# if data is a SpatialPolygonsDataFrame object, 
# then calling addPolygons on that map widget will 
# know to add the polygons from that SpatialPolygonsDataFrame.

df = data.frame(Lat = 1:10, Long = rnorm(10))
leaflet(df) %>% addCircles()
# = 
leaflet(df) %>% addCircles(lng = ~Long, lat = ~Lat)
# =
leaflet() %>% addCircles(data = df)
# = 
leaflet() %>% addCircles(data = df, lat = ~ Lat, lng = ~ Long)

library(sp)
Sr1 <- Polygon(cbind(c(2, 4, 4, 1, 2), c(2, 3, 5, 4, 2)))
Sr2 <-  Polygon(cbind(c(5, 4, 2, 5), c(2, 3, 2, 2)))
Sr3 <-  Polygon(cbind(c(4, 4, 5, 10, 4), c(5, 3, 2, 5, 5)))
Sr4 <-  Polygon(cbind(c(5, 6, 6, 5, 5), c(4, 4, 3, 3, 4)), hole = TRUE)
Srs1 = Polygons(list(Sr1), "s1")
Srs2 = Polygons(list(Sr2), "s2")
Srs3 = Polygons(list(Sr4, Sr3), "s3/4")
SpP = SpatialPolygons(list(Srs1, Srs2, Srs3), 1:3)

leaflet(height = "300px") %>% addPolygons(data = SpP)

library(maps)
mapStates <- map("state", fill  = TRUE, plot = FALSE)
leaflet(data = mapStates) %>% addTiles() %>% 
  addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)

m = leaflet() %>% addTiles()
df = data.frame(
  lat = rnorm(100),
  lng = rnorm(100),
  size = runif(100, 5, 20),
  color = sample(colors(), 100)
)

m <- leaflet(df) %>% addTiles()
m %>% addCircleMarkers(radius = ~size, color = ~color, fill = FALSE)
m %>% addCircleMarkers(radius = runif(100, 4, 10), color = c("red"))

# Basemaps
# addTiles() with no arguments; by default, OpenStreetMap tiles are used.
m <- leaflet() %>% setView(lng = -71.0589, lat = 42.3601, zoom = 12)
m %>% addTiles()

# third-party tiles
m %>% addProviderTiles(providers$Stamen.Toner)
m %>% addProviderTiles(providers$CartoDB.Positron)
m %>% addProviderTiles(providers$Esri.NatGeoWorldMap)

# custom tile URL template
leaflet() %>% addTiles() %>% setView(-93.65, 42.0285, zoom = 4) %>%
  addWMSTiles(
    "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
    layers = "nexrad-n0r-900913",
    options =  WMSTileOptions(format = "image/png", transparent = TRUE),
    attribution = "Weather data © 2012 IEM Nexrad"
  )

# combining tile layers
m %>% addProviderTiles(providers$MtbMap) %>% 
  addProviderTiles(providers$Stamen.TonerLines, options = providerTileOptions(opacity = 0.35)) %>% 
  addProviderTiles(providers$Stamen.TonerLabels)

# Markers

# SpatialPoints or SpatialPointsDataFrame objects (from the sp package)
# POINT, sfc_POINT, and sf objects (from the sf package); only X and Y dimensions will be considered

# Icon markers
data(quakes)
# Show first 20 rows from the `quakes` dataset
leaflet(data = quakes[1:20,]) %>% addTiles() %>%
  addMarkers(~long, ~lat, popup = ~as.character(mag), label = ~as.character(mag))

# customizing marker icons
greenLeafIcon <- makeIcon(
  iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png",
  iconWidth = 38, iconHeight = 95,
  iconAnchorX = 22, iconAnchorY = 94,
  shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
  shadowWidth = 50, shadowHeight = 64,
  shadowAnchorX = 4, shadowAnchorY = 62
)

leaflet(data = quakes[1:4,]) %>% addTiles() %>%
  addMarkers(~long, ~lat, icon = greenLeafIcon)

quakes1 <- quakes[1:10,]

leafIcons <- icons(
  iconUrl = ifelse(quakes1$mag < 4.6,
                   "http://leafletjs.com/examples/custom-icons/leaf-green.png",
                   "http://leafletjs.com/examples/custom-icons/leaf-red.png"
  ),
  iconWidth = 38, iconHeight = 95,
  iconAnchorX = 22, iconAnchorY = 94,
  shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
  shadowWidth = 50, shadowHeight = 64,
  shadowAnchorX = 4, shadowAnchorY = 62
)

leaflet(data = quakes1) %>% addTiles() %>%
  addMarkers(~long, ~lat, icon = leafIcons)

# Make a list of icons. We'll index into it based on name.
oceanIcons <- iconList(
  ship = makeIcon("ferry-18.png", "ferry-18@2x.png", 18, 18),
  pirate = makeIcon("danger-24.png", "danger-24@2x.png", 24, 24)
)

# Some fake data
df <- sp::SpatialPointsDataFrame(
  cbind(
    (runif(20) - .5) * 10 - 90.620130,  # lng
    (runif(20) - .5) * 3.8 + 25.638077  # lat
  ),
  data.frame(type = factor(
    ifelse(runif(20) > 0.75, "pirate", "ship"),
    c("ship", "pirate")
  ))
)

leaflet(df) %>% addTiles() %>%
  # Select from oceanIcons based on df$type
  addMarkers(icon = ~oceanIcons[type])

# first 20 quakes
df.20 <- quakes[1:20,]

getColor <- function(quakes) {
  sapply(quakes$mag, function(mag) {
    if(mag <= 4) {
      "green"
    } else if(mag <= 5) {
      "orange"
    } else {
      "red"
    } })
}

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(df.20)
)

leaflet(df.20) %>% addTiles() %>%
  addAwesomeMarkers(~long, ~lat, icon=icons, label=~as.character(mag))

# Marker cluster
leaflet(quakes) %>% addTiles() %>% addMarkers(
  clusterOptions = markerClusterOptions()
)

leaflet(df) %>% addTiles() %>% addCircleMarkers()

# Create a palette that maps factor levels to colors
pal <- colorFactor(c("navy", "red"), domain = c("ship", "pirate"))

leaflet(df) %>% addTiles() %>%
  addCircleMarkers(
    radius = ~ifelse(type == "ship", 6, 10),
    color = ~pal(type),
    stroke = FALSE, fillOpacity = 0.5
  )

# popups and labels

content <- paste(sep = "<br/>",
                 "<b><a href='http://www.samurainoodle.com'>Samurai Noodle</a></b>",
                 "606 5th Ave. S",
                 "Seattle, WA 98138"
)

leaflet() %>% addTiles() %>%
  addPopups(-122.327298, 47.597131, content,
            options = popupOptions(closeButton = FALSE)
  )

library(htmltools)

df <- read.csv(textConnection(
  "Name,Lat,Long
Samurai Noodle,47.597131,-122.327298
Kukai Ramen,47.6154,-122.327157
Tsukushinbo,47.59987,-122.326726"
))

leaflet(df) %>% addTiles() %>%
  addMarkers(~Long, ~Lat, popup = ~htmlEscape(Name))

# customizing marker labels
# Change Text Size and text Only and also a custom CSS
leaflet() %>% addTiles() %>% setView(-118.456554, 34.09, 13) %>%
  addMarkers(
    lng = -118.456554, lat = 34.105,
    label = "Default Label",
    labelOptions = labelOptions(noHide = T)) %>%
  addMarkers(
    lng = -118.456554, lat = 34.095,
    label = "Label w/o surrounding box",
    labelOptions = labelOptions(noHide = T, textOnly = TRUE)) %>%
  addMarkers(
    lng = -118.456554, lat = 34.085,
    label = "label w/ textsize 15px",
    labelOptions = labelOptions(noHide = T, textsize = "15px")) %>%
  addMarkers(
    lng = -118.456554, lat = 34.075,
    label = "Label w/ custom CSS style",
    labelOptions = labelOptions(noHide = T, direction = "bottom",
                                style = list(
                                  "color" = "red",
                                  "font-family" = "serif",
                                  "font-style" = "italic",
                                  "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                  "font-size" = "12px",
                                  "border-color" = "rgba(0,0,0,0.5)"
                                )))

# Lines and Shapes
library(rgdal)

# From https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html
states <- readOGR("shp/cb_2013_us_state_20m.shp",
                  layer = "cb_2013_us_state_20m", GDAL1_integer64_policy = TRUE)

neStates <- subset(states, states$STUSPS %in% c(
  "CT","ME","MA","NH","RI","VT","NY","NJ","PA"
))

leaflet(neStates) %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("YlOrRd", ALAND)(ALAND),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))
library(albersusa)
fullsize <- usa_sf()
object.size(fullsize)

cities <- read.csv(textConnection("
City,Lat,Long,Pop
Boston,42.3601,-71.0589,645966
Hartford,41.7627,-72.6743,125017
New York City,40.7127,-74.0059,8406000
Philadelphia,39.9500,-75.1667,1553000
Pittsburgh,40.4397,-79.9764,305841
Providence,41.8236,-71.4222,177994
"))

# Circle
leaflet(cities) %>% addTiles() %>%
  addCircles(lng = ~Long, lat = ~Lat, weight = 1,
             radius = ~sqrt(Pop) * 30, popup = ~City
  )

# Rectangles
leaflet() %>% addTiles() %>%
  addRectangles(
    lng1=-118.456554, lat1=34.078039,
    lng2=-118.436383, lat2=34.062717,
    fillColor = "transparent"
  )

# Colors
# Call the color function (colorNumeric) to create a new palette function
pal <- colorNumeric(c("red", "green", "blue"), 1:10)
# Pass the palette function a data vector to get the corresponding colors
pal(c(1,6,9))

# Colors (Not Working)

library(rgdal)

# From http://data.okfn.org/data/datasets/geo-boundaries-world-110m
countries <- readOGR("json/countries.geojson")
map <- leaflet(countries)
par(mar = c(5,5,0,0), cex = 0.8)
hist(countries$gdp_md_est, breaks = 20, main = "")

# Create a continuous palette function
pal <- colorNumeric(
  palette = "Blues",
  domain = countries$gdp_md_est)

# Apply the function to provide RGB colors to addPolygons
map %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
              color = ~pal(gdp_md_est))

binpal <- colorBin("Blues", countries$gdp_md_est, 6, pretty = FALSE)

map %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
              color = ~binpal(gdp_md_est))

qpal <- colorQuantile("Blues", countries$gdp_md_est, n = 7)
map %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
              color = ~qpal(gdp_md_est))

# Make up some random levels. (TODO: Better example)
countries$category <- factor(sample.int(5L, nrow(countries), TRUE))

factpal <- colorFactor(topo.colors(5), countries$category)

leaflet(countries) %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
              color = ~factpal(category))

# Layers

outline <- quakes[chull(quakes$long, quakes$lat),]

map <- leaflet(quakes) %>%
  # Base groups
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(providers$Stamen.Toner, group = "Toner") %>% # draw continent
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>% # draw continent
  # Overlay groups
  addCircles(~long, ~lat, ~10^mag/5, stroke = F, group = "Quakes") %>% # draw circles
  addPolygons(data = outline, lng = ~long, lat = ~lat,
              fill = F, weight = 2, color = "#FFFFCC", group = "Outline") %>% # draw lines
  # Layers control
  addLayersControl(
    baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
    overlayGroups = c("Quakes", "Outline"),
    options = layersControlOptions(collapsed = FALSE) # add controller
  )
map

map %>% hideGroup("Outline")


# marker cluster
quakes <- quakes %>%
  dplyr::mutate(mag.level = cut(mag,c(3,4,5,6),
                                labels = c('>3 & <=4', '>4 & <=5', '>5 & <=6')))

quakes.df <- split(quakes, quakes$mag.level)

l <- leaflet() %>% addTiles()

names(quakes.df) %>%
  purrr::walk( function(df) {
    l <<- l %>% # <<- what is this mean?
      addMarkers(data=quakes.df[[df]],
                 lng=~long, lat=~lat,
                 label=~as.character(mag),
                 popup=~as.character(mag),
                 group = df,
                 clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
                 labelOptions = labelOptions(noHide = F,
                                             direction = 'auto'))
  })

l %>%
  addLayersControl(
    overlayGroups = names(quakes.df),
    options = layersControlOptions(collapsed = FALSE)
  )

# Task
haticon <- makeIcon(iconUrl = "https://static.vecteezy.com/system/resources/previews/001/209/031/non_2x/graduation-hat-png.png",
                    iconWidth = 64, iconHeight = 64,
                    iconAnchorX = 22, iconAnchorY = 94)

content <- paste(sep = "<br/>",
                 "<b><a href = 'https://apply.grad.ucsd.edu/departments/economics'>Univ of California San Diego</a></b>",
                 "Sequoyah Hall",
                 "Economics Department")

m <- leaflet() %>%
  setView(lng = -117.2405, lat = 32.8821, zoom = 12) %>% 
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
  #addMarkers(~long, ~lat, icon = haticon)
  addPopups(lng = -117.2405, lat = 32.8821, content, options = popupOptions(closeButton = FALSE))

m

