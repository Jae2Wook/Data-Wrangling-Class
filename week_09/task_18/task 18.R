library(tidyverse)

# The MAP function

# map(): makes a list
# map_lgl(): makes a logical vector
# map_int(): makes an integer vector
# map_dbl(): makes a double vector
# map_chr(): makes a character vector


df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

map_dbl(df, mean)
map_dbl(df, median)
map_dbl(df, sd)

df %>% map_dbl(mean, trim = 0.5)
df %>% map_dbl(median)
df %>% map_dbl(sd)

z <- list(x = 1:3, y = 4:5)
map_int(z, length)

models <- mtcars %>% split(.$cyl) %>% 
  map(function(df) lm(mpg ~ wt, data = df))

# split(): divides the data in the vector x into the groups defined by f.
models <- mtcars %>% split(.$cyl) %>% 
  map(~lm(mpg ~ wt, data = .))

models %>% map(summary) %>% map_dbl(~.$r.squared)
# =
models %>% map(summary) %>% map_dbl("r.squared")

x1 <- list(
  c(0.27, 0.37, 0.57, 0.91, 0.20),
  c(0.90, 0.94, 0.66, 0.63, 0.06), 
  c(0.21, 0.18, 0.69, 0.38, 0.77)
)
x2 <- list(
  c(0.50, 0.72, 0.99, 0.38, 0.78), 
  c(0.93, 0.21, 0.65, 0.13, 0.27), 
  c(0.39, 0.01, 0.38, 0.87, 0.34)
)

# lapply(): similar to map()
# sapply(): wrapper around lapply()
threshold <- function(x, cutoff = 0.8) x[x > cutoff]
x1 %>% sapply(threshold) %>% str()

x2 %>% sapply(threshold) %>% str()

# Dealing with failure
# safely(): show error to NULL

safe_log <- safely(log)
str(safe_log(10))
str(safe_log("a"))

x <- list(1, 10, "a")
y <- x %>% map(safely(log))
str(y)

# transpose(): wrap the same name
y <- y %>% transpose()
str(y)

is_ok <- y$error %>% map_lgl(is_null)
x[!is_ok]

# flatten_dbl(): list can be anything for flatten (as a list is returned), but the contents must match the type for the other functions.
y$result[is_ok] %>% flatten_dbl()

# possibly(): gives a default value to return when there is an error
x <- list(1, 10, "a")
x %>% map_dbl(possibly(log, NA_real_))

# quietly(): captures printed output of error
x <- list(1, -1)
x %>% map(quietly(log)) %>% str()

# Mapping over multiple arguments
mu <- list(5, 10, -3)
mu %>% map(rnorm, n = 5) %>% str()

sigma <- list(1, 5, 10)
seq_along(mu) %>% 
  map(~rnorm(5, mu[[.]], sigma[[.]])) %>% 
  str()
# mu sigma map2(mu, sigma, rnorm, n = 5)
#  5    1      rnorm(5, 1, n = 5)
# 10    5      rnorm(10, 5, n = 5)
# -3   10      rnorm(-3, 10, n = 5)

map2(mu, sigma, rnorm, n = 5) %>% str()

###
map2 <- function(x, y, f...){
  out <- vector("list", length(x))
  for(i in seq_along(x)){
    out[[i]] <- f(x[[i]], y[[i]], ...)
  }
  out
}
###

# pmap(): takes a list of argument
n <- list(1, 3, 5)
args1 <- list(n, mu, sigma)

args1 %>% pmap(rnorm) %>% str()

#        args1        pmap(args1)
#     1    5    1     rnorm(1, 5, 1)
#     3   10    5     rnorm(3, 10, 5)
#     5   -3   10     rnorm(5, -3, 10)

args2 <- list(mean = mu, sd = sigma, n = n)
args2 %>% 
  pmap(rnorm) %>% 
  str()

params <- tribble(
  ~mean, ~sd, ~n,
  5,     1,  1,
  10,     5,  3,
  -3,    10,  5
)

params %>% pmap(rnorm)

f <- c("runif", "rnorm", "rpois")
param <- list(
  list(min = -1, max = 1), 
  list(sd = 5), 
  list(lambda = 10)
)

invoke_map(f, param, n = 5) %>% str()

sim <- tribble(
  ~f,      ~params,
  "runif", list(min = -1, max = 1),
  "rnorm", list(sd = 5),
  "rpois", list(lambda = 10)
)

sim %>% mutate(sim = invoke_map(f, params, n = 10))

# Walk

x <- list(1, "a", 3)
x %>% walk(print)

plots <- mtcars %>% 
  split(.$cyl) %>% 
  map(~ggplot(., aes(mpg, wt)) + geom_point())
paths <- stringr::str_c(names(plots), ".pdf")

# list(paths, plots): gives 3 plots
pwalk(list(paths, plots), ggsave, path = tempdir())

# predicate functions: True or False

iris %>% 
  keep(is.factor) %>% 
  str()

iris %>% 
  discard(is.factor) %>% 
  str()

x <- list(1:5, letters, list(10))

x %>% some(is_character)

x %>% every(is_vector)

x <- sample(10)
x
# detect(): find the first element
x %>% 
  detect(~ . > 5)

x %>% 
  detect_index(~ . > 5)

x %>% 
  head_while(~ . > 5)

x %>% 
  tail_while(~ . > 5)

# reduce and accumulate

dfs <- list(
  age = tibble(name = "John", age = 30),
  sex = tibble(name = c("John", "Mary"), sex = c("M", "F")),
  trt = tibble(name = "Mary", treatment = "A")
)

dfs %>% reduce(full_join)

vs <- list(
  c(1, 3, 5, 6, 10),
  c(1, 2, 3, 7, 8, 10),
  c(1, 2, 3, 4, 8, 9, 10)
)

vs %>% reduce(intersect)

x <- sample(10)
x

x %>% accumulate(`+`)

my_first_list <- list(my_number = 5,
                      my_vector = c("a", "b", "c"),
                      my_dataframe = data.frame(a = 1:3, b = c("q", "b", "z"), c = c("bananas", "are", "so very great")))
my_first_list

addTen <- function(.x){
  return(.x + 10)
}

map(.x = c(1, 4, 7), .f = addTen)
map(c(1, 4, 7), addTen)
map(list(1, 4, 7), addTen)

map(data.frame(a = 1, b = 4, c = 7), addTen)

map_dbl(c(1, 4, 7), addTen)

map_chr(c(1, 4, 7), addTen) %>% str()

map_df(c(1, 4, 7), function(.x){
  return(data.frame(old_number = .x,
                    new_number = addTen(.x)))
})

# modify(): like map(). always returns an object the same type as the input object
modify(c(1, 4, 7), addTen)
modify(list(1, 4, 7), addTen)

modify(data.frame(1, 4, 7), addTen)

# .p: predicted function - the second argument
modify_if(.x = list(1, 4, 7),
          .p = function(x) x > 5,
          .f = addTen)


# tilde-dot shorthand for anonymous functions
function(x){
  x + 10
}
# =
~{.x + 10}

map_dbl(c(1, 4, 7), ~{.x + 10})

gapminder_orig <- read.csv("https://raw.githubusercontent.com/swcarpentry/r-novice-gapminder/gh-pages/_episodes_rmd/data/gapminder-FiveYearData.csv")
# define a copy of the original dataset that we will clean and play with 
gapminder <- gapminder_orig

dim(gapminder)
head(gapminder)
gapminder %>% map_chr(class)

######3
gapminder %>% map_dbl(n_distinct)
##########

# map_df(): combines the data frames row-wise into a single data frame
gapminder %>% map_df(~(data.frame(n_distinct = n_distinct(.x),
                                  class = class(.x))))

gapminder %>% map_df(~(data.frame(n_distinct = n_distinct(.x),
                                  class = class(.x))),
                     .id = "variable")

# pluck(2): pluck second column data
.x <- gapminder %>% pluck(1)
head(.x)

data.frame(n_distinct = n_distinct(.x),
          class = class(.x))

gapminder %>% map_df(~(data.frame(n_distinct = n_distinct(.x),
                                  class = class(.x))),
                     .d = "variable")

map2(.x = object1,
     .y = object2,
     .f = plotFunction(.x, .y))

continent_year <- gapminder %>% distinct(continent, year)
continent_year

continents <- continent_year %>% pull(continent) %>% as.character
years <- continent_year %>% pull(year)

.x <- continents[1]
.y <- years[1]

gapminder %>% 
  filter(continent == .x,
         year == .y) %>% 
  ggplot() +
  geom_point(aes(x = gdpPercap, y = lifeExp)) +
  ggtitle(glue::glue(.x, " ", .y))


plot_list <- map2(.x  = continents,
                  .y = years,
                  .f = ~{
                    gapminder %>% 
                      filter(continent == .x,
                             year == .y) %>% 
                      ggplot() +
                      geom_point(aes(x = gdpPercap, y = lifeExp)) +
                      ggtitle(glue::glue(.x, " ", .y))
                  })

plot_list[1]
plot_list[30]

# pmap(): allows iterate over an arbitrary number of objects (more than 2)

gapminder_nested <- gapminder %>% 
  group_by(continent) %>% 
  nest()
gapminder_nested

colnames(gapminder_nested)
gapminder_nested$data[[1]]
gapminder_nested$continent[[2]]

# extract the first entry from the data column
gapminder_nested %>% 
  pluck("data", 1)

gapminder_nested %>% pluck("data", 5)



tibble(vec_col = 1:10) %>% 
  mutate(vec_sum = sum(vec_col))

tibble(list_col = list(c(1, 5, 7), 
                       5, 
                       c(10, 10, 11))) %>%
  mutate(list_sum = sum(list_col))

tibble(list_col = list(c(1, 5, 7),
                       5,
                       c(10, 10, 11))) %>% 
  mutate(list_sum = map(list_col, sum))

tibble(list_col = list(c(1, 5, 7), 
                       5, 
                       c(10, 10, 11))) %>%
  mutate(list_sum = map(list_col, sum)) %>% 
  pull(list_sum)

tibble(list_col = list(c(1, 5, 7), 
                       5, 
                       c(10, 10, 11))) %>% 
  mutate(list_sum = map_dbl(list_col, sum))

gapminder_nested %>% 
  mutate(avg_lifeExp = mean(data$lifeExp))

.x <- gapminder_nested %>% pluck("data", 1) # has all aisa
n_distinct(.x$year)
mean(.x$lifeExp)

gapminder_nested %>% 
  mutate(avg_lifeExp = map_dbl(data, ~{mean(.x$lifeExp)}))

gapminder_nested <- gapminder_nested %>% 
  mutate(lm_obj = map(data, ~lm(lifeExp ~ pop + gdpPercap + year, data = .x)))

# linear model for Asia
gapminder_nested %>% pluck("lm_obj", 1)

gapminder_nested <- gapminder_nested %>% 
  mutate(pred = map2(lm_obj, data, function(.lm, .data) predict(.lm, .data)))

# map2(): automatically perform the action on all combinations that can be made from the vectors

gapminder_nested <- gapminder_nested %>% 
  mutate(cor = map2_dbl(pred, data, function(.pred, .data) cor(.pred, .data$lifeExp)))

# keep 5 rows from each continent
set.seed(23489)
gapminder_list <- gapminder %>% split(gapminder$continent) %>% map(~sample_n(., 5))

# keep(): keeps elements of a list that satisfy a fiven condition
# select_if(): selects columns of a dta frame that satisfy a given condition
gapminder_list %>% 
  keep(~mean(.x$lifeExp) > 70) # keep the continent that has mean lifeExp over 70

# reduce(): combine (reduce) all of the elements of a list into a single object
reduce(c(1, 2, 3), sum)

accumulate(c(1, 2, 3), sum)

# rbinds(): bind the rows of the list back together into a single data frame
gapminder_list %>% 
  reduce(rbind)

# every(): True False
gapminder_list %>% every(~{mean(.x$life) > 70})

gapminder_list %>% some(~{mean(.x$life) > 70})

list(1, x(2, 5, 1), "a") %>% has_element("a")


gapminder %>% 
  group_by(continent) %>% 
  nest() %>% 
  mutate(lm_obj = map(data, ~lm(lifeExp ~ pop + year + gdpPercap, data = .))) %>% 
  mutate(lm_tidy = map(lm_obj, broom::tidy)) %>% 
  ungroup() %>% 
  transmute(continent, lm_tidy) %>%  #transmute(): add new variables and drops existing ones
  unnest(cols = c(lm_tidy))

# unnest(): give the name of a list-column containing data frames, and it row-binds the data frames together, repeating the outer columns the right number of times to line up.

#######3 Charts and analysis
library(lubridate)

data <- read_csv("https://byuistats.github.io/M335/data/waitlist_DP_108.csv")

data <- data %>% mutate(date = lubradate::mdy_hm(`Registration Date`))

#data <- data %>% filter(`Course Sec` == "FDMAT108-18")
View(data)

# Function 1: The % of currently registered students were 
# at one time on the waitlist (Currently registered students and those who were on the waitlist at one time)/(Currently registered students)
# 530, 476, 479, 262

data %>% group_by(`Person ID`) %>% slice(n()) %>% filter(Status == "Registered") %>% ungroup() %>% filter(`Waitlist Reason` == "Waitlist Registered") %>% count() %>% pull()

Fun_1 <- function(x, course){
  deno <- x %>% filter(`Course Sec` == course) %>% group_by(`Person ID`) %>% slice(n()) %>% filter(Status == "Registered") %>% ungroup() %>% count() %>% pull()
  
  numer <- x %>% filter(`Course Sec` == course) %>% group_by(`Person ID`) %>% slice(n()) %>% filter(Status == "Registered") %>% ungroup() %>% filter(`Waitlist Reason` == "Waitlist Registered") %>% count() %>% pull()
  
  round(numer / deno * 100, 2)
  
}

courses <- data %>% select(`Course Sec`) %>% distinct() %>% pull()

function1 <- tibble(course = courses,
       percentage1 = map_dbl(course, function(x) Fun_1(data, x)))

function1

# Function 2: The % of students who were ever on the 
# waitlist that are currently registered for for the class  (those who were on the waitlist at one and currently registered students)/(those who were on the waitlist at one time)

data %>% filter(`Course Sec` == course) %>% group_by(`Person ID`) %>% filter(Status == "Wait List") %>% distinct(`Person ID`) %>% ungroup() %>% count() %>% pull()

Fun_2 <- function(x, course){
  deno <- x %>% filter(`Course Sec` == course) %>% group_by(`Person ID`) %>% filter(Status == "Wait List") %>% distinct(`Person ID`) %>% ungroup() %>% count() %>% pull()
  
  numer <- x %>% filter(`Course Sec` == course) %>% group_by(`Person ID`) %>% slice(n()) %>% filter(Status == "Registered") %>% ungroup() %>% filter(`Waitlist Reason` == "Waitlist Registered") %>% count() %>% pull()
  
  round(numer / deno * 100, 2)
  
}

# paste0(map_dbl(courses, function(x) Fun_2(data, x)), " %")

function2 <- tibble(course = courses,
       percentage2 = map_dbl(courses, function(x) Fun_2(data, x)))

function2

# Create a plot or two that explore the relationship between 
# the two metrics above, while incorporating other variable(s) 
# from the dataset (e.g. semester)


ggplot(function1, aes(x = course, y = percentage1, fill = course)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Percentage of Waitlist Student in a Class", x = "Courses", y = "Percentage") +
  scale_y_continuous(labels = function(x) paste(x, "%"))

ggplot(function2, aes(x = course, y = percentage2, fill = course)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Percentage of Waitlist Student Got Into A Class", x = "Courses", y = "Percentage") +
  scale_y_continuous(labels = function(x) paste0(x, "%"))

# Do you see a trend/pattern? If so, describe it. 
# If not, discuss what other factors may be affecting these 
# waitlist percentages that we haven't taken into account























