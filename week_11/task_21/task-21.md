---
title: "Task 21"
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
library(sf)
library(USAboundaries)
```


```r
state_data <- us_states()

state_data <- state_data %>% filter(jurisdiction_type == "state") %>% 
  filter(state_abbr != "AK") %>% 
  filter(state_abbr != "HI")

idaho <- us_counties() %>% filter(state_name == "Idaho")

cities <- us_cities() %>% group_by(state_name) %>% arrange(desc(population)) %>% slice(1:3) %>% mutate(number = row_number()) %>% ungroup()
```


```r
cities <-  cities %>% mutate(long = "")
cities <-  cities %>% mutate(lat = "")

cities$long <- 1:nrow(cities) %>%
  map_dbl(function(x) cities$long[x] = cities$geometry[[x]][1])

cities$lat <- 1:nrow(cities) %>%
  map_dbl(function(x) cities$lat[x] = cities$geometry[[x]][2])

cities <- cities %>% filter(state_abbr != "HI") %>% 
  filter(state_abbr != "AK")
```


```r
ggplot() +
  geom_sf(data = state_data, fill = NA) +
  geom_sf(data = idaho, fill = NA) +
  geom_point(cities, mapping = aes(x = long, y = lat, size = population/1000, color = number)) +
  scale_x_continuous(NULL) +
  scale_y_continuous(NULL) +
  labs(size = "Population\n(1000)") +
  guides(color = FALSE) +
  ggrepel::geom_label_repel(aes(x = long, y = lat, label = city), data = cities %>% filter(number == 1), segment.color =  "darkblue", segment.size = 0.7, color = "darkblue", size = 2.5) +
  guides(size = guide_legend(override.aes = (list(color = "darkblue")))) +
  theme_bw()
```

![](task-21_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
#cities$population <- as.factor(cities$population)

#p + scale_size_manual(values = c('#00008B'))


# ggsave("task21.png", width = 15, units = "in")
```

