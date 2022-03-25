library(tidyverse)
library(lubridate)
library(nycflights13)

# creating date/times

today()

now()

ymd("2017-01-31")
mdy("January 31ts, 2017")
dmy("31-Jan-2017")

ymd(20170131)

ymd_hms("2017-01-31 20:11:59")
mdy_hm("01/31/2017 08:01")

ymd(20170131, tz = "UTC")

flights %>% 
  select(year, month, day, hour, minute)

# make_datetime()
flights %>% 
  select(year, month, day, hour, minute) %>% 
  mutate(departure = make_datetime(year, month, day, hour, minute))

# %/%: integer division, %%: remainder
make_datetime_100 <- function(year, month, day, time){
  make_datetime(year, month, day, time %/% 100, time %% 100)
}


View(flights)
flights_dt <- flights %>% 
  filter(!is.na(dep_time), !is.na(arr_time)) %>% 
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) %>% 
  select(origin, dest, ends_with("delay"), ends_with("time"))

View(flights_dt)

flights_dt %>% 
  ggplot(aes(dep_time)) +
  geom_freqpoly(binwidth = 86400) # 86400 sec = 1 day

flights_dt %>% 
  filter(dep_time < ymd(20130102)) %>% 
  ggplot(aes(dep_time)) +
  geom_freqpoly(binwidth = 600) # 600 sec = 10 mintues

as_datetime(today())
as_datetime(now())

as_datetime(60 * 60 * 10)
as_date(365 * 10 + 2)

# Date-time components
datetime <- ymd_hms("2016-07-08 12:34:56")

year(datetime)
month(datetime)
mday(datetime) # day of the month

yday(datetime) # day of the year
wday(datetime) # day of the week
minute(datetime)

month(datetime, label = TRUE)

wday(datetime, label = TRUE, abbr = FALSE)

flights_dt %>% 
  mutate(wday = wday(dep_time, label = TRUE)) %>% 
  ggplot(aes(x = wday)) +
  geom_bar()

flights_dt %>% 
  mutate(minute = minute(dep_time)) %>% 
  group_by(minute) %>% 
  summarise(
    avg_delay = mean(arr_delay, na.rm = TRUE),
    n = n()) %>% 
  ggplot(aes(minute, avg_delay)) +
  geom_line()

sched_dep <- flights_dt %>% 
  mutate(minute = minute(sched_dep_time)) %>% 
  group_by(minute) %>% 
  summarise(avg_delay = mean(arr_delay, na.rm = TRUE),
            n = n())

ggplot(sched_dep, aes(minute, avg_delay)) +
  geom_line()

ggplot(sched_dep, aes(minute, n)) +
  geom_line()

# floor_date(), round_date(), ceiling_date()
flights_dt %>% 
  count(week = floor_date(dep_time, "week")) %>% 
  ggplot(aes(week, n)) +
  geom_line()

datetime <- ymd_hms("2016-07-08 12:34:56")

year(datetime) <- 2020
month(datetime) <- 01
hour(datetime) <- hour(datetime) + 1
datetime

update(datetime, year = 2020, month = 2, mday = 2, hour = 2)

ymd("2015-02-01") %>% 
  update(mday = 30)

ymd("2015-02-01") %>% 
  update(hour = 400)

yday(datetime)

View(flights_dt)
flights_dt %>% 
  mutate(dep_hour = update(dep_time, yday = 1)) %>% 
  ggplot(aes(dep_hour)) +
  geom_freqpoly(binwidth = 300)

# set year day as 1 for all the rows, so we can only see the hours when flights flew
View(flights_dt %>%  mutate(dep_hour = update(dep_time, yday = 1)))

# Time spans
h_age <- today() - ymd(19791014)

# duration(): uses seconds
as.duration(h_age)

dseconds(15)

dminutes(10)

dhours(c(12, 14))

ddays(0:5)
dweeks(3)
dyears(1)

2*dyears(1)

dyears(1) + dweeks(12) + dhours(15)

tomorrow <- today() + ddays(1)

lasy_year <- today() - dyears(1)

one_pm <- ymd_hms("2016-03-12 13:00:00", tz = "America/New_York")
one_pm + ddays(1) # time zone changes

# Periods
one_pm
one_pm + days(1)

seconds(15)
minutes(10)
hours(c(12, 24))
days(7)
months(1:6)
weeks(3)
years(1)

10 * (months(6) + days(1))
days(50) + hours(25) + minutes(2)

# a leap year
ymd("2016-01-01") + dyears(1)
ymd("2016-01-01") + years(1)

# daylight savings time
one_pm + ddays(1)
one_pm + days(1)

flights_dt %>% 
  filter(arr_time < dep_time)

flights_dt <- flights_dt %>% 
  mutate(overnight = arr_time < dep_time, # True or false
         arr_time = arr_time + days(overnight * 1), # if it is overnight, add one day
         sched_arr_time = sched_arr_time + days(overnight *1)) # same as here

View(flights_dt)

flights_dt %>% 
  filter(overnight, arr_time < dep_time)

# Intervals
years(1) / days(1)

next_year <- today() + years(1)
(today() %--% next_year) / ddays(1)

# integer division
(today() %--% next_year) %/% days(1)

Sys.timezone()

length(OlsonNames())
head(OlsonNames())

x1 <- ymd_hms("2015-06-01 12:00:00", tz = "America/New_York")
x2 <- ymd_hms("2015-06-01 18:00:00", tz = "Europe/Copenhagen")
x3 <- ymd_hms("2015-06-02 05:00:00", tz = "Pacific/Auckland")

# they are in the same time
x1 - x2
x1 - x3

x4 <- c(x1, x2, x3)
x4

# with_tz(): keep the instant in time the same, and change how it's displayed
x4a <- with_tz(x4, tzone = "Australia/Lord_howe")

x4a - x4

# force_tz(): labelled with the incorrect time zone.
x4b <- force_tz(x4, tzone = "Australia/Lord_Howe")
x4b - x4

library(lubridate)

arrive <- ymd_hms("2011-06-04 12:00:00", tz = "Pacific/Auckland")
second(arrive) <- 0
wday(arrive)
wday(arrive, label = TRUE)

meeting <- ymd_hms("2011-07-01 09:00:00", tz = "Pacific/Auckland")
with_tz(meeting, "America/Chicago")

mistake <- force_tz(meeting, "America/Chicago")

with_tz(mistake, "Pacific/Auckland")

leave <- ymd_hms("2011-08-10 14:00:00", tz = "Pacific/Auckland")

# Time intervals
auckland <- interval(arrive, leave)

auckland <- arrive %--% leave

jsm <- interval(ymd(20110720, tz = "Pacific/Auckland"), ymd(20110831, tz = "Pacific/Auckland"))

int_overlaps(jsm, auckland)

setdiff(auckland, jsm)

leap_year(2011)

ymd(20110101) + dyears(1)
ymd(20110101) + years(1)

meetings <- meeting + weeks(0:5)

meetings %within% jsm

# how long is the interval
auckland / ddays(1)
auckland / ddays(2)

auckland / dminutes(1)

auckland %/% months(1)

as.period(auckland %% months(1)) # left over time after calculating by one month
as.period(auckland) # calculate whole interval

jan31 <- ymd("2013-01-31")
jan31 + months(0:11)

floor_date(jan31, "month") + months(0:11) + days(31)

# adding one month
jan31 %m+% months(0:11)

last_day <- function(date){
  ceiling_date(date, "month") - days(1)
}

last_day("2017-02-21")

date <- ymd_hms("2017-03-21 05:00:00")
floor_date(date, "month")

# https://vcg.informatik.uni-rostock.de/~ct/timeviz/timeviz.html?

########3

library(readr)
data <- read_csv("https://byuistats.github.io/M335/data/carwash.csv")
View(data)

data$time <- with_tz(data$time, "America/Denver")

data$hours <- ceiling_date(data$time, "hour")

#data$fixed_day <- update(data$hours, yday = 1)

#data1 <- data %>% group_by(fixed_day) %>% summarise(sales = sum(amount))


library(riem)

min(data$time)
max(data$time)

temper <- riem_measures(station = "RXE", date_start = "2016-05-13", date_end = "2016-07-18")
View(temper)
temper1 <- temper %>% filter(tmpf != is.na(tmpf)) %>% select(valid, tmpf)
temper1$hours <- ceiling_date(temper1$valid, "hour")
View(temper1)

total <- left_join(data, temper1, by = "hours")
total <- total %>% filter(tmpf != is.na(tmpf))
total$fixed_day <- update(total$hours, yday = 1)

View(total)

all <- total %>% group_by(hours) %>% summarise(sales = sum(amount), temperature = tmpf)

all$fixed <- update(all$hours, yday = 1)
#all$hour <- format(as.POSIXct(strptime(all$fixed,"%Y/%m/%d %H:%M:%S",tz="")) ,format = "%H:%M")
class(all$fixed)
all$hour <- format(all$fixed, format='%H')
View(all)

ggplot(all, aes(temperature, sales)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(.~ hour) +
  theme_bw() +
  labs(title = "Sales and Temperature Relationship by Hours", x = "Temperature", y = "Sales")
