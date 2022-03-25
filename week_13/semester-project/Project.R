# https://towardsdatascience.com/eye-catching-animated-maps-in-r-a-simple-introduction-3559d8c33be1
# https://covid19.who.int/
# https://d4tagirl.com/2017/05/how-to-plot-animated-maps-with-gganimate
# https://rpubs.com/rdwinkelman/covid19_us_spread_gif
# https://www.datanovia.com/en/blog/gganimate-how-to-create-plots-with-beautiful-animation-in-r/

library(tidyverse)
library(rgdal)
library(downloader)
library(sf)
library(fs)
library(leaflet)
library(shiny)
library(gganimate)
library(gifski)
library(transformr)

my_URL <- "http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip"

df <- tempfile() # put the file
uf <- tempfile() # directory
download(my_URL, df, mode = "wb")
unzip(df, exdir = uf)
my_map <- read_sf(uf)
file_delete(df)
dir_delete(uf) # delete beuase the file is big

View(my_map)

data <- read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv")
View(data)

data <- na.omit(data)

colnames(my_map)[2] <- "Country_code"

data <- left_join(data, my_map, by = "Country_code") %>% select("Date_reported", "Country_code", "Country.x", "New_cases", "Cumulative_cases", "LON", "LAT", "geometry" )

colnames(data)[3] <- "Country"

data[rowSums(is.na(data)) > 0,]

data <- data %>% filter(Country_code != "BQ")
data <- data %>% filter(Country_code != "XK")
data <- data %>% filter(Country_code != "CW")
data <- data %>% filter(Country_code != "XC")
data <- data %>% filter(Country_code != "XB")
data <- data %>% filter(Country_code != "SX")
data <- data %>% filter(Country_code != "SS")

World <- data %>% group_by(Date_reported) %>% summarise(New_cases = sum(New_cases))
World$Country_code <- "World"

US <- data %>% filter(Country_code == "US") %>% select(Date_reported, New_cases, Country_code)

worldUS <- rbind(World, US)

max(data$New_cases)
min(data$New_cases)

paletteBins <- paletteBins <- seq(0, 500000, 50000)
colorPalette <- colorBin(palette = "YlOrBr", domain = data$New_cases, na.color = "transparent", bins = paletteBins)

leaflet(data %>% filter(Date_reported == "2021-04-29")) %>% 
  addTiles() %>% 
  setView(lat = 0, lng = 0, zoom = 2) %>% 
  addCircleMarkers(lng = ~LON,
                   lat = ~LAT,
                   radius = ~log(New_cases),
                   weight = 1,
                   opacity = 1,
                   color = ~ifelse(New_cases > 0, "black", "transparent"),
                   fillColor = ~ifelse(New_cases > 0, colorPalette(New_cases), "transparent"), 
                   fillOpacity = 0.8) %>% 
  addLegend(pal = colorPalette, values = data$New_cases, opacity = 0.9, title = "New Cases", position = "bottomleft")



paletteBins <- paletteBins <- c(0, 50000, 100000, 250000, 500000, 1000000, 2500000, 5000000, 10000000, 25000000, 50000000)
colorPalette <- colorBin(palette = "YlOrBr", domain = data$Cumulative_cases, na.color = "transparent", bins = paletteBins)

leaflet(data %>% filter(Date_reported == "2021-04-29")) %>% 
  addTiles() %>% 
  setView(lat = 0, lng = 0, zoom = 2) %>% 
  addCircleMarkers(lng = ~LON,
                   lat = ~LAT,
                   radius = ~log(Cumulative_cases),
                   weight = 1,
                   opacity = 1,
                   color = ~ifelse(Cumulative_cases > 0, "black", "transparent"),
                   fillColor = ~ifelse(Cumulative_cases > 0, colorPalette(Cumulative_cases), "transparent"), 
                   fillOpacity = 0.8) %>% 
  addLegend(pal = colorPalette, values = data$Cumulative_cases, opacity = 0.9, title = "Cumulative Cases", position = "bottomleft") %>% 
  sliderInput("Country_code", "Date_reported",
            min = min(data$Date_reported),
            max = max(data$Date_reported),
            value = min(data$Date_reported),
            step = 1,
            timeFormat = "%d %b %y",
            snimate = animationOptions(interval = 500, loop = FALSE))


leaflet(data %>% filter(Date_reported == "2021-04-29")) %>% 
  addTiles() %>% 
  setView(lat = 0, lng = 0, zoom = 2) %>% 
  addPolygons(layerId = ~Country_code,
              fillColor = ~colorPalette(Cumulative_cases),
              stroke = TRUE,
              fillOpacity = 1,
              color = "white",
              weight = 1) %>% 
  addLegend(pal = colorPalette, values = data$Cumulative_cases, opacity = 0.9, title = "Cumulative Cases", position = "bottomleft")

# not working polygons

leaflet(data %>% filter(Date_reported == "2021-04-29")) %>% 
  addTiles()  %>% 
  setView(lat = 0, lng = 0, zoom=2) %>%
  addPolygons( 
    layerId = ~Country_code,
    fillColor = ~colorPalette(Cumulative_cases),
    stroke = TRUE, 
    fillOpacity = 1, 
    color = "white", 
    weight = 1,
    label = ~lapply(LabelText, htmltools::HTML)) %>%
  addLegend(pal = colorPalette, values = data$Cumulative_cases, opacity=0.9, title = "Cases", position = "bottomleft")

# map

world <- map_data("world")

p <- ggplot(data) +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "black", fill = "transparent", size = 0.01
  ) +
  theme_bw() +
  #geom_polygon(data, mapping = aes(x = LON, y = LAT, group = Country_code, fill = New_cases/ 100000)) +
  #scale_fill_gradient(low = "white", high = "red") +
  geom_point(data, mapping = aes(x = LON, y = LAT, group = Country_code, size = New_cases/ 100000), color = "red") +
  theme(legend.position = "bottomleft", panel.grid = element_blank(), panel.border = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank()) +
  theme_void() +
  labs(title = "World COVID-19 Time Series", subtitle = "Date: {frame_time}", size = "New Cases\n(100,000)") +
  transition_time(Date_reported)
  
animate(p, renderer = gifski_renderer())

options(scipen = 999)
p1 <- ggplot(worldUS) +
  geom_line(aes(x = Date_reported, y = New_cases, color = Country_code)) +
  geom_point(aes(x = Date_reported, y = New_cases, color = Country_code), size = 1.5) +
  scale_y_continuous(labels = scales::comma) +
  geom_segment(aes(x = Date_reported, y = New_cases, xend = max(data$Date_reported), yend = New_cases), linetype = 2, colour = 'grey') + 
  geom_text(aes(x = max(data$Date_reported) - 40, y= New_cases, label = New_cases), hjust = 0) +
  #ggrepel::geom_label_repel(data = data %>% filter(Country_code %in% c("US", "GB", "IN")), mapping = aes(x = Date_reported, y = New_cases, label = New_cases), nudge_y = -2) +
  transition_reveal(Date_reported) +
  labs(title = "COVID-19 Change", x = "Date", y = "New Cases", color = "Country") +
  theme_bw()


animate(p1, renderer = gifski_renderer())


airq <- airquality
airq$Month <- format(ISOdate(2004,1:12,1),"%B")[airq$Month]

ggplot(airq, aes(Day, Temp, group = Month)) + 
  geom_line() + 
  geom_segment(aes(xend = 31, yend = Temp), linetype = 2, colour = 'grey') + 
  geom_point(size = 2) + 
  geom_text(aes(x = 31.1, label = Month), hjust = 0) + 
  transition_reveal(Day) + 
  coord_cartesian(clip = 'off') + 
  labs(title = 'Temperature in New York', y = 'Temperature (°F)') + 
  theme_minimal() + 
  theme(plot.margin = margin(5.5, 40, 5.5, 5.5))


## ui
source("UI/ui_overview.R", local = TRUE)
source("UI/ui_plots.R", local = TRUE)
source("UI/ui_about.R", local = TRUE)
source("UI/ui_fullTable.R", local = TRUE)


ui <- fluidPage(
  title = "COVID-19 International Cases",
  tags$head(
    tags$link(rel = "shorcut icon", type = "image/png", href = "logo.png")
  ),
  tags$style(type = "text/css", ".containter-fluid {padding-left: 0px; padding-right; 0px !important;}"),
  tags$style(type = "text/css", ".navbar {margin-bottom: 0px;}"),
  tags$style(type = "text/css", ".content {padding: 0px;}"),
  tags$style(type = "text/css", ".row {margin-left: 0px; margin-right: 0px;}"),
  tags$style(HTML(".col-sm-12 {padding:5px; margin-bottom: -15px;}")),
  tags$style(HTML(".col-sm-6 {padding:5px; margin-bottom: -15px;}")),
  navbarPage(
    title = div("COVID-19 Statistical Analysis Simulator App", style = "padding-left: 10px"),
    collapsible = TRUE,
    fluid = TRUE,
    tabPanel("Overview", page_overview, value = "page-overview"),
    tabPanel("Table", page_fullTable, value = "page-fullTable"),
    tabPanel("Plots", page_plots, value = "page-plots"),
    tabPanel("About", page_about, value = "page-about")
    
  )
)

