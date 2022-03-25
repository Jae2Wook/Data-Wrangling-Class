library(nycflights13)
library(tidyverse)

flights

# filter(): filter rows
filter(flights, month == 1, day == 1)
# = 
filter(flights, month == 1 & day == 1)

# Comparisons (inequality signs)
sqrt(2) ^ 2 == 2
#> [1] FALSE
1 / 49 * 49 == 1
#> [1] FALSE

#  every number you see is an approximation
near(sqrt(2) ^ 2,  2)
#> [1] TRUE
near(1 / 49 * 49, 1)
#> [1] TRUE

# Logical operators
# x & !y, xor(x, y) = !(x & y)

filter(flights, month == 1 | month == 12)
# =
filter(flights, month %in% c(11, 12))

filter(flights, !(arr_delay > 120 | dep_delay > 120))
# =
filter(flights, arr_delay <= 120, dep_delay <= 120)

# almost any operation involving an unknown value will also be unknown.

x <- NA
is.na(x)
sum(is.na(x))

# filter() excludes both FALSE and NA values.
df <- tibble(x = c(1, NA, 3))
filter(df, x > 1)
# filter() only includes rows where the condition is TRUE
filter(df, is.na(x) | x > 1)

arrange(flights, year, month, day)

arrange(flights, desc(dep_delay))

df <- tibble(x = c(5, 2, NA))
arrange(df, x)
arrange(df, desc(x))

# select(): subset
select(flights, year, month, day)
select(flights, c("year", "month", "day"))

# Select all columns between year and day (inclusive)
select(flights, year:day)

# Select all columns except those from year to day (inclusive)
select(flights, -(year:day))

# start_with("abc")" matches names that begin with "abc"
# ends_with("xyz): matches names that end with "xyz"
# contains("ijk"): matches names that contains"ijk"
# matches("(.) \\ 1"): selects variables that match a regular expression. This one matches any variables that contain repeated characters.
# num_range("x", 1:3): matches x1, x2 and x3.

#rename(data, new_name = old_name)
rename(flights, tail_num = tailnum)

# This is useful if you have a handful of variables you’d like to move to the start of the data frame.
select(flights, time_hour, air_time, everything())

select(flights, contains("TIME"))

# any_of: Match any of these characters exactly once.
vars <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights, any_of(vars))

# mutate(): Add new variables

flights_sml <- select(flights, year:day, ends_with("delay"), distance, air_time)
mutate(flights_sml, gain = dep_delay - arr_delay, speed = distance / air_time * 60)
mutate(flights_sml, gain = dep_delay - arr_delay, hours = air_time / 60, gain_per_hour = gain / hours)

# transmute(): only want to keep the new variables
transmute(flights, gain = dep_delay - arr_delay, hours = air_time / 60, gain_per_hour = gain / hours)

# %/%: integer division, %%: remainder
transmute(flights, dep_time, hour = dep_time %/% 100, minute = dep_time %% 100)

x <- 1:10
lag(x)
lead(x)

# cum...(): cumulative
cumsum(x)
cummean(x)
cumprod(x)
cummax(x)

y <- c(1,2,2,NA,3,4)
min_rank(y)
min_rank(desc(y))

row_number(y)
dense_rank(y)
percent_rank(y)
cume_dist(y)

# Summarise()
summarise(flights, delay = mean(dep_delay, na.rm = TRUE))

# save columns with group_by() and do summarise()
by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))

by_dest <- group_by(flights, dest)
delay <- summarise(by_dest, count = n(), dist = mean(distance, na.rm = TRUE), delay = mean(arr_delay, na.rm = TRUE))
View(delay)
delay <- filter(delay, count > 20, dest != "HNL")
ggplot(delay, aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth()

delays <- flights %>% group_by(dest) %>% summarise(count = n(), dist = mean(distance, na.rm = TRUE), delay = mean(arr_delay, na.rm = TRUE)) %>% filter(count > 20 , dest != "HNL")

# missing values
flights %>% group_by(year, month, day) %>% summarise(mean = mean(dep_delay, na.rm = TRUE))

not_cancelled <- flights %>% filter(!is.na(dep_delay), !is.na(arr_delay))
not_cancelled %>% group_by(year, month, day) %>% summarise(mean = mean(dep_delay))

# Count
delays <- not_cancelled %>% group_by(tailnum) %>% summarise(delay = mean(arr_delay))
ggplot(delays, aes(x = delay)) +
  geom_freqpoly(binwidth = 10)

delays <- not_cancelled %>% group_by(tailnum) %>% summarise(delay = mean(arr_delay, na.rm = TRUE), n = n())
ggplot(delays, aes(x = n, y = delay)) +
  geom_point(alpha = 1/10)

delays %>% filter(n > 25) %>% 
  ggplot(aes(x = n, y = delay)) +
  geom_point(alpha = 1/10)

library(Lahman)
batting <- as_tibble(Lahman::Batting)
batters <- batting %>% 
  group_by(playerID) %>% 
  summarise(ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE), ab = sum(AB, na.rm = TRUE))

batters %>% filter(ab > 100) %>% 
  ggplot(aes(x = ab, y = ba)) +
  geom_point() +
  geom_smooth(se = FALSE)

batters %>% arrange(desc(ba))

not_cancelled %>% group_by(year, month, day) %>% 
  summarise(avg_delay1 = mean(arr_delay),
            avg_delay2 = mean(arr_delay[arr_delay > 0]))

# IQR(): interquartile range
# mad(): median absolute deviation
not_cancelled %>% 
  group_by(dest) %>% 
  summarise(distance_sd = sd(distance)) %>% 
  arrange(desc(distance_sd))

# quantile(x, 0.25): value of x that is greater than 25% of the values, and less than the remaining 75%.
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(first = min(dep_time), last = max(dep_time))

# first(x), nth(x, 2), last(x)
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(first_dep = first(dep_time), last_dep = last(dep_time))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  mutate(r = min_rank(desc(dep_time))) %>% 
  filter(r %in% range(r))

# n_distinct():  To count the number of distinct (unique) values
not_cancelled %>% 
  group_by(dest) %>% 
  summarise(carriers = n_distinct(carrier)) %>% 
  arrange(desc(carriers))

not_cancelled %>% 
  count(dest)

# “count” (sum) the total number of miles a plane flew:
not_cancelled %>% 
  count(tailnum, wt = distance)

# Counts and proportions of logical values: sum(x > 10), mean(y == 0).
# TRUE is converted to 1 and FALSE to 0.
# sum(x) gives the number of TRUEs in x, and mean(x) gives the proportion.
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(n_early = sum(dep_time < 500))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(hour_prop = mean(arr_delay > 60))

# grouping by multiple variables
daily <- group_by(flights, year, month, day)
per_day <- summarise(daily, flights = n())
per_month <- summarise(per_day, flights = sum(flights))
per_year <- summarise(per_month, flights = sum(flights))

# ungroup
daily %>% ungroup() %>% 
  summarise(flights = n())

# grouped mutates
# find the worst members of each group:
flights_sml %>% 
  group_by(year, month, day) %>% 
  filter(rank(desc(arr_delay)) < 10)

# find all groups bigger than a threshold:
popular_dests <- flights %>% 
  group_by(dest) %>% 
  filter(n() > 365)

# standardise to compute per group metrics:
popular_dests %>% 
  filter(arr_delay > 0) %>% 
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>% 
  select(year:day, dest, arr_delay, prop_delay)

##############3
# Chart and analysis

remotes::install_github("apreshill/bakeoff")
library(bakeoff)
?ratings
View(ratings)

only_first_last <- ratings %>% 
  group_by(series) %>% # group by "series"
  slice(1, n()) %>% # by each "series" get rows of first and the last (number of each "series")
  mutate(which_episode = ifelse(episode == 1, "First", "Last")) %>% # make a new column with in each series there are 2 episodes and if episode is 1 it is the "first".
  ungroup() %>% # ungroup it = become back to original setting but the data set has changed.
  mutate(series_f = as.factor(series)) # make a new column that "series" are factors


View(only_first_last)

ggplot(only_first_last, aes(x = which_episode, y = viewers_7day, group = series_f, color = series_f)) + # group and color by series_f
  geom_line() + # draw line graphs
  geom_point(size = 5) # draw points

# Filter the data to seasons with 10 episodes. 
# Then create one visualization that displays the 7 day ratings for each 
# episode per series and also shows how mean ratings changed across series.

ratings %>% group_by(series) %>% summarise(n = n()) #count series

ten_episodes <- ratings %>% group_by(series) %>% summarise(n = n()) %>% filter(n == 10) %>% pull(series)
ten_episodes

ten <- filter(ratings, series %in% ten_episodes)

ggplot(ten, aes(x = episode, y = viewers_7day)) +
  geom_line(aes(group = series)) +
  geom_point() +
  facet_wrap(.~series) +
  theme_bw() +
  labs(title = "7 Day Ratings for Each Season", x = "Episode", y = "7 Day Ratings")

ten_mean <- ten %>% group_by(series) %>% summarise(mean_ratings = mean(viewers_7day))

ggplot(ten_mean, aes(x = series, y = mean_ratings)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(title = "Mean 7 Day Ratings for Each Season", x = "Episode", y = "7 Day Ratings")

# Discuss what you see in the graph and point out interesting features 
# of the data. (Perhaps a little internet surfing or googling 
# will help you explain what you see.)

# The 7 day ratings were in the increasing trend until series 7.
# However, the ratings are a little above 9 from series 8 to 10.
# This can be found in both graphs.
# To me group_by() %>% summarise() feature is really interesting and useful because
# these two features allow me to do many calculations depends on what kind of data I need.
#


