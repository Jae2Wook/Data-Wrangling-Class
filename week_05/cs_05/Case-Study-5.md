---
title: "Case Study 5"
author: "Jae Wook Jung"
date: "5월 21, 2021"
output:
  html_document:  
    keep_md: true
    toc: true
    code_folding: hide
    toc_float: true
    fig_height: 6
    fig_width: 12
    fig_align: 'center'
---




```r
library(tidyverse)
library(haven)
library(foreign)
library(readxl)
library(downloader)

cm_to_in <- function(cm){
  return(cm / 2.54)
}

feet_to_in <- function(feet){
  return(feet * 12)
}

in_to_cm <- function(ch){
  return(ch * 2.54)
}
```

#### Download Data


```r
german_male_conscript <- read_dta("https://byuistats.github.io/M335/data/heights/germanconscr.dta")
colnames(german_male_conscript) <- c("town", "DOBY", "height", "age", "co")

g19 <- german_male_conscript %>% mutate(birth_year = DOBY, height.in = cm_to_in(height), height.cm = height, study = "german conscript") %>% select(birth_year, height.in, height.cm, study)
```


```r
bavarian_height <- read_dta("https://byuistats.github.io/M335/data/heights/germanprison.dta")

b19 <- bavarian_height %>% mutate(birth_year = bdec, height.in = cm_to_in(height), height.cm = height, study = "bavarian") %>% select(birth_year, height.in, height.cm, study)
```


```r
zip_file <- tempfile()
unzip_dir <- tempfile()
url <- "https://byuistats.github.io/M335/data/heights/Heights_south-east.zip"
download(url, dest = zip_file, mode = "wb")
unzip(zipfile = zip_file, exdir = unzip_dir)
#list.files(unzip_dir)
file_name <- list.files(unzip_dir, pattern = "DBF")
# file.path(unzip_dir, file_name)
data_dbf <- read.dbf(file = file.path(unzip_dir, file_name))

g18 <- data_dbf %>% mutate(birth_year = GEBJ, height.in = cm_to_in(CMETER), height.cm = CMETER, study = "southern german soldiers") %>% select(birth_year, height.in, height.cm, study)
```


```r
height_data <- read_csv(url("https://raw.githubusercontent.com/hadley/r4ds/master/data/heights.csv"))
height_data$DOBY <- 1950
height_data <- height_data %>% mutate(height = height + 100)

height_data <- height_data %>% filter(sex == "male")
us20 <- height_data %>% mutate(birth_year = DOBY, height.in = cm_to_in(height), height.cm = height, study = "BLS data") %>% select(birth_year, height.in, height.cm, study)
```


```r
survey <- read_sav("http://www.ssc.wisc.edu/nsfh/wave3/NSFH3%20Apr%202005%20release/main05022005.sav")
survey <- survey %>% mutate(DOBY = DOBY + 1900)

w20 <- survey %>% filter(RT216F %in% c(4, 5, 6), RT216I %in% c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
w20 <- w20 %>% select(DOBY, RT216F, RT216I, RT1D1)# %>% drop_na()
w20 <- w20 %>% mutate(height.in = feet_to_in(RT216F) + RT216I, height.cm = in_to_cm(height.in))
w20 <- w20 %>% mutate(birth_year = DOBY, study = "Wisconsin data") %>% select(birth_year, height.in, height.cm, study)
```

#### Combine the 5 datasets into one tidy dataset and save the dataset (preferably as an R data file).


```r
alld <- bind_rows(b19, g18, g19, us20, w20)

saveRDS(alld, file = "stuff.RDS")
a <- readRDS("stuff.RDS")
```

#### One graph show individual heights (no summaries) and be faceted by study.


```r
ggplot(a, aes(x = birth_year, y = height.in, color = study)) +
  geom_point() +
  facet_wrap(.~study, nrow = 1) +
  labs(title = "Height by Data Set", y = "Height in Inches", x = "Year") +
  scale_y_continuous(labels = function(x) paste0(x, " in")) +
  theme_bw() +
  theme(legend.position = "none")
```

![](Case-Study-5_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

#### One other graph of your choice that tried to answer the question. You could try creating a "decade" column and showing height summaries by decade.


```r
a <- a %>% mutate(decade = birth_year %/% 10 * 10) %>% drop_na()

a1 <- a %>% group_by(decade) %>% summarise(n = n())

ggplot(a, aes(x = decade, y = height.in, group = decade)) +
  geom_boxplot() +
  labs(title = "Height in Inches by Decades", y = "Height", x = "Year") +
  scale_y_continuous(labels = function(x) paste0(x, " in")) +
  scale_x_continuous(breaks = seq(min(a$decade, na.rm = TRUE), max(a$decade, na.rm = TRUE), by = 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  annotate("text", x = c(a1$decade), y = 50, label = c(a1$n), angle = 90, size = 3, color = "skyblue4") +
  annotate("text", x = 1750, y = 53, label = "# of data in decades", size = 3, color = "skyblue4")
```

![](Case-Study-5_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

