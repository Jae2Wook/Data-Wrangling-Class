---
title: "Task 22"
author: "Jae Wook Jung"
date: "6월 30, 2021"
output:
  html_document:  
    keep_md: true
    code_folding: hide
    toc: true
    toc_float: true
    fig_height: 6
    fig_width: 12
    fig_align: 'center'
---




```r
library(tidyverse)
library(downloader)
library(sf)
library(fs)
library(ggplot2)
```


```r
my_URL_shape <- "https://byuistats.github.io/M335/data/shp.zip"

df <- tempfile() # put the file
uf <- tempfile() # directory
download(my_URL_shape, df, mode = "wb")
unzip(df, exdir = uf)
my_raw_shape <- read_sf(uf)
file_delete(df)
dir_delete(uf) # delete because the file is big

#ggplot()+geom_sf(data = my_raw_shape)
#my_raw_shape %>% glimpse()
my_tidy_shape <- my_raw_shape %>% filter(StateName == "Idaho")
#ggplot()+geom_sf(data = my_tidy_shape)
```


```r
my_URL_well <- "https://opendata.arcgis.com/api/v3/datasets/1abb666937894ae4a87afe9655d2ff83_1/downloads/data?format=shp&spatialRefId=4326"

df <- tempfile() # put the file
uf <- tempfile() # directory
download(my_URL_well, df, mode = "wb")
unzip(df, exdir = uf)
my_raw_well <- read_sf(uf)
file_delete(df)
dir_delete(uf) # delete beuase the file is big

my_tidy_well <- my_raw_well %>% filter(Production > 5000)
```


```r
my_URL_dam <- "https://opendata.arcgis.com/api/v3/datasets/e163d7da3b84410ab94700a92b7735ce_0/downloads/data?format=shp&spatialRefId=4326"

df <- tempfile() # put the file
uf <- tempfile() # directory
download(my_URL_dam, df, mode = "wb")
unzip(df, exdir = uf)
my_raw_dam <- read_sf(uf)
file_delete(df)
dir_delete(uf) # delete beuase the file is big

my_tidy_dam <- my_raw_dam %>% filter(SurfaceAre > 50)
```


```r
my_URL_water <- "https://research.idwr.idaho.gov/gis/Spatial/Hydrography/streams_lakes/c_250k/hyd250.zip"

df <- tempfile() # put the file
uf <- tempfile() # directory
download(my_URL_water, df, mode = "wb")
unzip(df, exdir = uf)
my_raw_water <- read_sf(uf)
file_delete(df)
dir_delete(uf) # delete beuase the file is big
```


```r
ggplot() +
  geom_sf(data = my_tidy_well) +
  geom_sf(data = my_tidy_shape, fill = NA) +
  geom_sf(data = my_raw_water, aes(color = "skyblue")) +
  geom_sf(data = my_tidy_well, aes(color = "orange2")) +
  geom_sf(data = my_tidy_dam, aes(color = "darkgreen")) +
  scale_color_identity(breaks = c("orange2", "darkgreen", "skyblue"), labels = c("Well", "Dam", "Water"), guide = "legend") +
  labs(title = "Water Sources in Idaho") +
  theme_bw()
```

![](Task-22_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
ggsave("Idaho_water.png")
```

