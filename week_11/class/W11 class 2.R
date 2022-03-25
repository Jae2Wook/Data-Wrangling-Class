library(sf)
library(tidyverse)

ID_counties <- USAboundaries::us_counties(states = "ID")
st_crs(ID_counties)
ggplot()+geom_sf(data = ID_counties) + theme_bw()


my_proj <- "+proj=robin +datum=WGS84"  
ID_tr <- ID_counties %>% st_transform(crs = my_proj)
st_crs(ID_tr)
ggplot()+geom_sf(data = ID_tr) + theme_bw()

ggplot() + 
  geom_sf(data = ID_counties) + 
  theme_bw() +
  coord_sf(crs = st_crs("+proj=robin +datum=WGS84"))

ggplot() + 
  geom_sf(data = ID_counties) + 
  theme_bw() +
  coord_sf(crs = st_crs(5041))


###
pacman::p_load(downloader, sf, fs, tidyverse)

my_URL_shape <- "https://byuistats.github.io/M335/data/shp.zip"

df <- tempfile() # put the file
uf <- tempfile() # directory
download(my_URL_shape, df, mode = "wb")
unzip(df, exdir = uf)
my_raw_shape <- read_sf(uf)
file_delete(df); dir_delete(uf) # delete because the file is big

ggplot()+geom_sf(data = my_raw_shape)
my_raw_shape %>% glimpse()
my_tidy_shape <- my_raw_shape %>% filter(StateName == "Idaho") %>% glimpse()
ggplot()+geom_sf(data = my_tidy_shape)

# well --> shape file or geojason
# down load and get url
# chrome://downloads

my_URL_well <- "https://opendata.arcgis.com/api/v3/datasets/1abb666937894ae4a87afe9655d2ff83_1/downloads/data?format=shp&spatialRefId=4326"

df <- tempfile() # put the file
uf <- tempfile() # directory
download(my_URL_well, df, mode = "wb")
unzip(df, exdir = uf)
my_raw_well <- read_sf(uf)
file_delete(df); dir_delete(uf) # delete beuase the file is big


#ggplot()+geom_sf(data = my_raw_well)
my_raw_well %>% glimpse()
my_tidy_well <- my_raw_well %>% filter(Production > 5000) %>% glimpse()
ggplot()+geom_sf(data = my_tidy_well)


ggplot() +
  geom_sf(data = my_tidy_well) +
  geom_sf(data - my_tidy_shape) +
  geom_sf(data = my_tidy_well, color = "red")

#### Dam

my_URL_dam <- "https://opendata.arcgis.com/api/v3/datasets/e163d7da3b84410ab94700a92b7735ce_0/downloads/data?format=shp&spatialRefId=4326"

df <- tempfile() # put the file
uf <- tempfile() # directory
download(my_URL_dam, df, mode = "wb")
unzip(df, exdir = uf)
my_raw_dam <- read_sf(uf)
file_delete(df); dir_delete(uf) # delete beuase the file is big


#ggplot()+geom_sf(data = my_raw_well)
my_raw_dam %>% glimpse()
my_tidy_dam <- my_raw_dam %>% filter(SurfaceAre > 50) %>% glimpse()
ggplot()+geom_sf(data = my_tidy_dam)


### water

my_URL_water <- "https://research.idwr.idaho.gov/gis/Spatial/Hydrography/streams_lakes/c_250k/hyd250.zip"

df <- tempfile() # put the file
uf <- tempfile() # directory
download(my_URL_water, df, mode = "wb")
unzip(df, exdir = uf)
my_raw_water <- read_sf(uf)
file_delete(df); dir_delete(uf) # delete beuase the file is big


#ggplot()+geom_sf(data = my_raw_well)
my_raw_water %>% glimpse()
# my_tidy_water <- my_raw_water %>% filter(Production > 5000) %>% glimpse()
ggplot()+geom_sf(data = my_raw_water)

## together

colors <- c("Water" = "skyblue", "Well" = "orange", "Dam" = "darkgreen")

ggplot() +
  geom_sf(data = my_tidy_well) + # set the coord first
  geom_sf(data = my_tidy_shape, fill = "transparent") +
  geom_sf(data = my_raw_water, aes(color = "skyblue")) +
  geom_sf(data = my_tidy_well, aes(color = "orange2")) +
  geom_sf(data = my_tidy_dam, aes(color = "darkgreen")) +
  labs(title = "Water Sources in Idaho", color = NULL) +
  scale_color_identity(breaks = c("skyblue", "orange2", "darkgreen"), labels = c("Water", "Well", "Dam"), guide = "legend") + 
  theme_bw()



## directory with downloaded files

install.packages("here")
library(here) # get directory to this file working directory
my_file_well <- here("filename","Wells.zip") 
#Where on my computer do I need to put the data?
#How do I make this reproducible so someone else can generate the same chart?

unzipfolder <- tempfile()
unzip(my_file_well, exdir = unzipfolder)
my_raw_well <- read_sf(unzipfolder)
dir_delete(unzipfolder)

ggplot()+geom_sf(data = my_raw_well)
my_raw_well %>% glimpse()
my_tidy_well <- my_raw_well %>% filter(Production >5000) %>%  glimpse()
ggplot()+geom_sf(data = my_tidy_well)
