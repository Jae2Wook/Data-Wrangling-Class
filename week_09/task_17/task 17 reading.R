library(tidyverse)

df <- tibble(a = rnorm(10),
             b = rnorm(10),
             c = rnorm(10),
             d = rnorm(10))

# rescales each column to have a range from 0 to 1. (A - min)/(max - min)
df$a <- (df$a - min(df$a, na.rm = TRUE)) / (max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df$b <- (df$b - min(df$b, na.rm = TRUE)) / (max(df$b, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df$c <- (df$c - min(df$c, na.rm = TRUE)) / (max(df$c, na.rm = TRUE) - min(df$c, na.rm = TRUE))
df$d <- (df$d - min(df$d, na.rm = TRUE)) / (max(df$d, na.rm = TRUE) - min(df$d, na.rm = TRUE))

(df$a - min(df$a, na.rm = TRUE)) / (max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))

x <- df$a
(x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

rng <- range(x, na.rm = TRUE)
(x - rng[1]) / (rng[2] - rng[1])

rescale01 <- function(x){
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

rescale01(c(0, 5, 10))

# Steps to create a new function
# 1: pick name
# 2: input arguments in ()
# 3: develop body in {}

rescale01(c(-10, 0, 10))
rescale01(c(1, 2, 3, NA, 5))

# Unit testing: convert informal to formal
df$a <- rescale01(df$a)
df$b <- rescale01(df$b)
df$c <- rescale01(df$c)
df$d <- rescale01(df$d)

# rescale fails because of Inf
x <- c(1:10, Inf)
rescale01(x)

rescale01 <- function(x){
  rng <- range(x, na.rm = TRUE, finite = TRUE) # add finite = TRUE
  (x - rng[1]) / (rng[2] - rng[1])
}

rescale01(x)

mean(is.na(x))

x / sum(x, na.rm = TRUE)
sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)

is_directory <- function(x) file.info(x)$isdir
is_readable <- function(x) file.access(x, 4) == 0

# Use long lines of - and = to make it easy to spot the breaks.
# Load data --------------------------------------

# Plot data --------------------------------------
#Ctrl + Shift + R

# Conditional execution
if(condition){
  # code executed when condition is True
} else{
  # code executed when condition is FALSE
}

has_name <- function(x){
  nms <- names(x)
  if(is.null(nms)){
    rep(FALSE, length(x))
  }else{
    !is.na(nms) & nms != ""
  }
}

# Use || or && to combine multiple logical expressions.

identical(0L, 0)

x <- sqrt(2)^2
x == 2
x - 2
# Use
near(x, 2)

# Multiple conditions
if(this){
  # do that
} else if{
  # do something else
} else {
  # rest
}

function(x, y, op){
  switch(op,
         plus = x + y,
         minus = x - y,
         times = x * y,
         divide = x / y,
         stop("Unkown op!"))
}

# long series of chained if. Use switch()

# eliminate long chains of "if" is cut()

if(y < 0 && debug) {
  message("Y is negative")
}

if(y == 0){
  log(x)
} else{
  y^x
}

## short if
y <- 10
x <- if(y < 20) "Too low" else "Too high"




if(y < 20){
  x <- "Too low"
} else{
  x <- "Too high"
}
x

# CUT()
# default: left interval is closed

#Low, if x \inx∈ [0, 150).
#Medium, if x \inx∈ [150, 200).
#High, if x \inx∈ [200, \infty∞).

# rigth = TRUE : interval right isde is closed

x <- c(75, 150, 160, 151, 216, 149)

categories <- cut(x, breaks = c(0, 150, 200, Inf),
                  labels = c("low", "medium", "high")) #, right = TRUE)

data.frame(x, categories)

temp <- c(-20, 30, 18, 2, 22, 10, 50, 8)
if (temp <= 0) {
  "freezing"
} else if (temp <= 10) {
  "cold"
} else if (temp <= 20) {
  "cool"
} else if (temp <= 30) {
  "warm"
} else {
  "hot"
}
# if() else() runs only one input, not the multiple inputs
# =
cut(temp, breaks = c(-Inf, 0, 10, 20, 30, Inf), labels = c("freezing", "cold", "cool", "warm", "hot"), right = TRUE)

mean_ci <- function(x, conf = 0.95){
  se <- sd(x) / sqrt(length(x))
  alpha <- 1 - conf
  mean(x) + se * qnorm(c(alpha / 2, 1 - alpha /2))
}

x <- runif(100)

mean_ci(x)
mean_ci(x, conf = 0.99)

wt_mean <- function(x, w){
  sum(x * w) / sum(w)
}

Wt_var <- function(x, w){
  mu <- wt_mean(x, w)
  sum(w * (x - mu)^2) / sum(2)
}

Wt_sd <- function(x, w){
  sqrt(wt_var(x, w))
}

wt_mean(1:6, 1:3)

wt_mean <- function(x, w){
  if(length(x) != length(w)){
    stop("`x` and `w` must be the same length", call. = FALSE)
  }
  sum(w * x) / sum(w)
}

wt_mean <- function(x, w, na.rm = FALSE){
  if(!is.logical(na.rm)) {
    stop("`na.rm` must be logical")
  }
  if (length(na.rm) != 1) {
    stop("`na.rm` must be length 1")
  }
  if(length(x) != length(w)) {
    stop("`x` and `w` must be the same length", call. = FALSE)
  }
  if(na.rm) {
    miss <- is.na(x) | is.na(w)
    x <- x[!miss]
    w <- w[!miss]
  }
  sum(w * x) / sum(w)
}

wt_mean <- function(x, w, na.rm = FALSE){
  stopifnot(is.logical(na.rm), length(na.rm) == 1)
  stopifnot(length(x) == length(w))
  if (na.rm){
    miss <- is.na(x) | is.na(w)
    x <- x[!miss]
    w <- w[!miss]
  }
  sum(w * x) / sum(w)
}
wt_mean(1:6, 6:1, na.rm = FALSE)

sum(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

stringr::str_c("a", "b", "c", "d", "e", "f")

# send ... on to another function
commas <- function(...) stringr::str_c(..., collapse = ", ")
commas(letters[1:10])

rule <- function(..., pad = "-"){
  title <- paste0(...)
  width <- getOption("width") - nchar(title) - 5
  cat(title, " ", stringr::str_dup(pad, width), "\n", sep = "")
}

# str_dup(string, times): string: Input character vector. times: Number of times to duplicate each string.
rule("Importnat output")

x <- c(1, 2)
sum(x, na.mr = TRUE) # ??????

# Return values
complicated_function <- function(x, y, z){
  if (length(x) == 0 || length(y) == 0) {
    return (0)
  }
  #complicated code here
}

f <- function(){
  if(x){
    # 
    #
    #
    #
    
  } else {
    # return something short
  }
}

f <- function() {
  if(!x) {
    return(something_short)
  }
  #
  #
  #
  #
  
}


show_missings <- function(df){
  n <- sum(is.na(df))
  cat("Missing values: ", n, "\n", sep = "")
  invisible(df)
}

show_missings(mtcars)
class(x)
dim(x)

mtcars %>% show_missings() %>% 
  mutate(mpg = ifelse(mpg<20, NA, mpg)) %>% 
  show_missings()

# Environment
f <- function(x) {
  x + y
}

`+` <- function(x, y){
  if(runif(1) < 0.1){
    sum(x, y)
  } else{
    sum(x, y) * 1.1
  }
}

table(replicate(1000, 1 + 2))
rm(`+`)
# rm(): remove objects



#############3### Chart and Analysis
library(readr)
library(lubridate)
library(tidyverse)
data <- read_csv("https://byuistats.github.io/M335/data/waitlist_DP_108.csv")
View(data)
data <- data %>% mutate(date = lubridate::mdy_hm(`Registration Date`))

data <- data %>% filter(`Course Sec` == "FDMAT108-18")
View(data)
# Hint: Each function calculates a different fraction (percentage). 
# The numerators of the fractions are the same. It's the denominator 
# (the comparison group) that changes!

# Function 1: Create a function that calculates the % of currently 
# registered students who were at one time on the waitlist.
waited <- data %>% group_by(`Person ID`) %>% filter(Status == "Wait List") %>% pull(`Person ID`)

who_waited <- data %>% filter(`Person ID` %in% waited)

waited_num <- who_waited %>% distinct(`Person ID`) %>% count()

data %>% group_by(`Person ID`) %>% filter(Status == "Wait List") %>% pull(`Person ID`) %>% 

wait_regist <- who_waited %>% group_by(`Person ID`) %>% filter(Status == "Registered") %>% pull(`Person ID`)

finally_regist <- who_waited %>% filter(`Person ID` %in% wait_regist)
registed_num <- finally_regist %>% distinct(`Person ID`) %>% count()

Function_1 <- function(x, y){
  paste0(y / x * 100, "%")
}

Function_1(waited_num, registed_num)

##### this
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

# Function 2: Create a function that calculates the % of students 
# who were ever on the waitlist that are currently registered for for the class.

Function_2 <- function(x){
  waited <- x %>% group_by(`Person ID`) %>% filter(Status == "Wait List") %>% pull(`Person ID`)
  
  who_waited <- x %>% filter(`Person ID` %in% waited)
  waited_num <- who_waited %>% distinct(`Person ID`) %>% count()
  
  wait_regist <- who_waited %>% group_by(`Person ID`) %>% filter(Status == "Registered") %>% pull(`Person ID`)
  
  finally_regist <- who_waited %>% filter(`Person ID` %in% wait_regist)
  registed_num <- finally_regist %>% distinct(`Person ID`) %>% count()
  
  cur_student <- x %>% group_by(`Person ID`) %>% filter(Status == "Registered") %>% distinct(`Person ID`) %>% nrow()
  
  paste0(round(registed_num / cur_student * 100, 2), "%")
  
}

Function_2(data)



