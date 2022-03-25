---
title: "Task 23"
author: "Jae Wook Jung"
date: "7월 07, 2021"
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
library(leaflet)
```


```r
haticon <- makeIcon(iconUrl = "https://static.vecteezy.com/system/resources/previews/001/209/031/non_2x/graduation-hat-png.png",
                    iconWidth = 64, iconHeight = 64,
                    iconAnchorX = 22, iconAnchorY = 94)
```


```r
content <- paste(sep = "<br/>",
                 "<b><a href = 'https://apply.grad.ucsd.edu/departments/economics'>Univ of California San Diego</a></b>",
                 "Sequoyah Hall",
                 "Economics Department")
```


```r
m <- leaflet() %>%
  setView(lng = -117.2405, lat = 32.8821, zoom = 12) %>% 
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
  #addMarkers(~long, ~lat, icon = haticon)
  addPopups(lng = -117.2405, lat = 32.8821, content, options = popupOptions(closeButton = FALSE))

m
```

<!--html_preserve--><div id="htmlwidget-27e1b1986da02640271d" style="width:1152px;height:576px;" class="leaflet html-widget"></div>
<script type="application/json" data-for="htmlwidget-27e1b1986da02640271d">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"setView":[[32.8821,-117.2405],12,[]],"calls":[{"method":"addProviderTiles","args":["Esri.NatGeoWorldMap",null,null,{"errorTileUrl":"","noWrap":false,"detectRetina":false}]},{"method":"addPopups","args":[32.8821,-117.2405,"<b><a href = 'https://apply.grad.ucsd.edu/departments/economics'>Univ of California San Diego<\/a><\/b><br/>Sequoyah Hall<br/>Economics Department",null,null,{"maxWidth":300,"minWidth":50,"autoPan":true,"keepInView":false,"closeButton":false,"className":""}]}],"limits":{"lat":[32.8821,32.8821],"lng":[-117.2405,-117.2405]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

