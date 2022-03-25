---
title: "Task 15"
author: "Jae Wook Jung"
date: "6월 07, 2021"
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
library(stringr)
library(tidyverse)
library(readr)

text_ran <- read_lines("https://byuistats.github.io/M335/data/randomletters.txt")

text_num <- read_lines("https://byuistats.github.io/M335/data/randomletters_wnumbers.txt")
```

#### With the randomletters.txt file, pull out every 1700 letter and find the quote that is hidden. The quote ends with a period (there may be some extra letters at the end). You should be pulling out the 1st letter, then the 1700th, then the 3400th, and then continue to count by 1700. 


```r
texts <- text_ran %>% 
  str_split("") %>% 
  unlist()

len <- sum(str_count(texts))

texts[c(1, seq(1700, len, 1700))] %>% str_flatten()
```

```
## [1] "the plural of anecdote is not data.z anfra"
```

#### With the randomletters_wnumbers.txt file, find all the numbers hidden and convert those numbers to letters using the letters order in the alphabet to decipher the message. The message starts with "experts".


```r
nums <- as.numeric(str_extract_all(text_num, "[0-9]+")[[1]])

letters[nums] %>% str_flatten()
```

```
## [1] "expertsoftenpossessmoredatathanjudgment"
```

#### With the randomletters.txt file, remove all the spaces and periods from the string then find the longest sequence of vowels.


```r
ran <- str_replace_all(text_ran, "[ .]", "")
v <- unlist(str_extract_all(ran, regex("[aeiou]+", ignore_case = TRUE)))
v[str_length(v) == max(str_length(v))]
```

```
## [1] "oaaoooo"
```

