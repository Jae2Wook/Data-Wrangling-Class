---
title: "Task 17"
author: "Jae Wook Jung"
date: "6월 14, 2021"
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
library(readr)
library(lubridate)
library(tidyverse)

data <- read_csv("https://byuistats.github.io/M335/data/waitlist_DP_108.csv")

data <- data %>% mutate(date = lubridate::mdy_hm(`Registration Date`))

data <- data %>% filter(`Course Sec` == "FDMAT108-18")
```

#### Function 1: Create a function that calculates the % of currently registered students who were at one time on the waitlist.


```r
Function_1 <- function(x){
  waited <- x %>% group_by(`Person ID`) %>% filter(Status == "Wait List") %>% pull(`Person ID`)
  
  who_waited <- x %>% filter(`Person ID` %in% waited)
  waited_num <- who_waited %>% distinct(`Person ID`) %>% count()
  
  wait_regist <- who_waited %>% group_by(`Person ID`) %>% filter(Status == "Registered") %>% pull(`Person ID`)
  
  finally_regist <- who_waited %>% filter(`Person ID` %in% wait_regist)
  registed_num <- finally_regist %>% distinct(`Person ID`) %>% count()
  
  paste0(round(registed_num / waited_num * 100, 2), "%")
}

Function_1(data)
```

```
## [1] "25.81%"
```

#### Function 2: Create a function that calculates the % of students who were ever on the waitlist that are currently registered for for the class.


```r
Function_2 <- function(x){
  waited <- x %>% group_by(`Person ID`) %>% filter(Status == "Wait List") %>% pull(`Person ID`)
  
  who_waited <- x %>% filter(`Person ID` %in% waited)
  waited_num <- who_waited %>% distinct(`Person ID`) %>% count()
  
  wait_regist <- who_waited %>% group_by(`Person ID`) %>% filter(Status == "Registered") %>% pull(`Person ID`)
  
  finally_regist <- who_waited %>% filter(`Person ID` %in% wait_regist)
  registed_num <- finally_regist %>% distinct(`Person ID`) %>% count()
  
  cur_student <- x %>% group_by(`Person ID`) %>% count(`Person ID`) %>% filter(n == 1) %>% nrow()
  
  paste0(round(registed_num / cur_student * 100, 2), "%")
  
}


Function_2(data)
```

```
## [1] "30.77%"
```


