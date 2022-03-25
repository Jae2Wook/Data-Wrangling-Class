library(tidyverse)
library(lubridate)
today()
wday(today()) # week day Sunday: 1. Monday: 2 ...
wday(today()+6)
wday(today()) + 6
(wday(today()) + 6) %% 7
now()

OlsonNames()
?OlsonNames()
Sys.timezone(location = TRUE)

my_date <- ymd_hm("2020-03-19-16-30")
my_date <- ymd_hm("2020-03-19T16:30", tz = "America/Denver")
year(my_date)
hour(my_date)
wday(my_date)
minute(my_date)
second(my_date)
month(my_date)
my_date+1
my_date+100
my_date+100000
my_date+days(6)
my_date+months(1)
my_date+days(12)
my_date+days(12)+months(1) # why NA? There is no April 31st.

today()
now()

my_date
now()-my_date

with_tz(now(),"UTC")
with_tz(now(),"MDT")
OlsonNames()

?OlsonNames
Sys.timezone(location = TRUE)

my_date
my_date %>% with_tz("America/Denver")
my_date %>% force_tz("America/Denver")

met_date <- ymd_hm("1999-08-30-08-50")
met_date 

met_date %>% force_tz("America/Denver")
# = 
met_date <- ymd_hm("1999-08-30-08-50", tz = "America/Denver")

#Why is this wrong?
#Let's fix the time zone. 
met_date <- ymd_hm("1999-08-30-08-50", tz = "America/Denver")
#Let's also fix the day of the week. (Thursday)
wday(met_date)
wday(met_date - days(4)) # : Thursday
#Then lets find out how long we've known each other.  
met_date <- ymd_hm("1999-08-26-08-50", tz = "America/Denver")

now() - met_date
as.period(now() - met_date, unit = "years")
period(now() - met_date, unit = "years")
as.period(interval(now() - met_date), unit = "years")

time_length(difftime(now(), met_date), "years")


library(riem)
my_temp_raw <- riem_measures(station = "RXE", date_start = "2021-04-20", date_end = "2021-06-21")



