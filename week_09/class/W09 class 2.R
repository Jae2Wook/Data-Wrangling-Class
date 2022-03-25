library(tidyverse)
library(lubridate)
my_URL <- "https://byuistats.github.io/M335/data/waitlist_DP_108.csv"
my_data <- read_csv(my_URL) 
my_tidy <- my_data %>% 
  mutate(date = lubridate::mdy_hms(`Registration Date`))

course <- "FDMAT108-18"

percent_registered_on_waitlist <- function(course = "FDMAT108-18"){
  my_summary <- my_tidy %>% 
    filter(`Course Sec` == course) %>% 
    filter(Status == "Registered") %>%
    group_by(`Waitlist Reason`) %>% 
    summarise(count = n())
  (my_total <- sum(my_summary$count))
  (my_count <- my_summary %>% filter(`Waitlist Reason`=="Waitlist Registered") %>% .$count)
  my_count/my_total
}

percent_registered_on_waitlist()


my_tidy %>% 
  filter(`Course Sec` == course) %>% glimpse() %>% View()

my_summary <- 
  my_tidy %>% 
  filter(`Course Sec` == course) %>%
  filter(Status == "Registered") %>% 
  group_by(`Waitlist Reason`) %>% 
  summarise(count = n()) %>% glimpse()
(my_total <- sum(my_summary$count))
(my_count <- my_summary %>% filter(`Waitlist Reason`=="Waitlist Registered") %>% .$count)
my_count/my_total


my_tidy %>% 
  filter(`Course Sec` == course) %>% 
  group_by(`Person ID`) %>% 
  slice(n())

View(my_tidy)

# We can use slice_tail to do the same thing. 
my_tidy %>% 
  filter(`Course Sec` == course) %>% 
  group_by(`Person ID`) %>% 
  slice_tail(n=1) 


my_tidy %>% 
  filter(`Course Sec` == course) %>% 
  arrange(`Person ID`,desc(date)) %>% #View() #We'll uncomment this view to see what happens after arranging. 
  distinct(`Person ID`,.keep_all = TRUE) %>% View()

#Pay attention to student #5.  This table above says they dropped, but they actually registered on the same day. 
my_tidy %>% 
  filter(`Person ID`==5)


my_tidy %>% 
  filter(`Course Sec` == course) %>% 
  arrange(`Person ID`,desc(date),desc(Status)) %>% #View()
  distinct(`Person ID`,.keep_all = TRUE)

# = 
my_tidy %>% 
  filter(`Course Sec` == course) %>% 
  arrange(`Person ID`,desc(date)) %>% 
  group_by(`Person ID`) %>% 
  slice(1)
#We still haven't fixed the problem with student 5.

########

registered_students <- my_tidy %>% 
  filter(`Course Sec` == course) %>% 
  arrange(`Person ID`,desc(date),desc(Status)) %>%
  distinct(`Person ID`,.keep_all = TRUE) %>% 
  filter(Status == "Registered")
registered_students
count(registered_students)

ids_of_students_from_waitlist <- 
  my_tidy %>% 
  filter(`Course Sec` == course) %>% 
  filter(Status == "Wait List") %>% 
  select(`Person ID`)
ids_of_students_from_waitlist

#we can use an inner join to instantly grab only those people that occur in both. 
inner_join(registered_students,ids_of_students_from_waitlist)
count(inner_join(registered_students,ids_of_students_from_waitlist))





percent_currently_registered_on_waitlist <- function(x,course){
  registered_students <- x %>% 
    filter(`Course Sec` == course) %>% 
    arrange(`Person ID`,desc(date),desc(Status)) %>%
    distinct(`Person ID`,.keep_all = TRUE) %>%
    filter(Status == "Registered")
  denom <- count(registered_students) %>% pull()
  
  ids_of_students_from_waitlist <- 
    x %>% 
    filter(`Course Sec` == course) %>% 
    filter(Status == "Wait List") %>% 
    select(`Person ID`)
  
  students_in_both <- inner_join(registered_students,ids_of_students_from_waitlist,by = "Person ID")
  numer <- count(students_in_both) %>% pull()
  
  percent <- numer/denom
  percent*100
}


#######
courses <- 
  my_tidy %>% 
  select(`Course Sec`) %>% 
  unique() %>% 
  pull()
courses

#The map_dbl function allows us to pass each course into our desired function. 
map_dbl(courses,function(x) percent_currently_registered_on_waitlist(my_tidy, x) )

tibble(
  course = courses,
  percent1 = map_dbl(course,function(x) percent_currently_registered_on_waitlist(my_tidy, x) )
)

new_percent_currently_registered_on_waitlist <- function(course){
  registered_students <- my_tidy %>% 
    filter(`Course Sec` == course) %>% 
    arrange(`Person ID`,desc(date),desc(Status)) %>%
    distinct(`Person ID`,.keep_all = TRUE) %>%
    filter(Status == "Registered")
  denom <- count(registered_students) %>% pull()
  
  ids_of_students_from_waitlist <- 
    my_tidy %>% 
    filter(`Course Sec` == course) %>% 
    filter(Status == "Wait List") %>% 
    select(`Person ID`)
  
  students_in_both <- inner_join(registered_students,ids_of_students_from_waitlist,by = "Person ID")
  numer <- count(students_in_both) %>% pull()
  
  percent <- numer/denom
  percent*100
}
#Check that it works on the first course
new_percent_currently_registered_on_waitlist(courses[1])
#Map each course into the function. 
#I don't have to use anonymous function notation this time. 
#The function only has one input, so the map notation is much simpler. 
map_dbl(courses, new_percent_currently_registered_on_waitlist)

#We arrange things in a nice little tibble. 
tibble(
  course = courses,
  percent1 = map_dbl(course,new_percent_currently_registered_on_waitlist )
)

nest_percent_currently_registered_on_waitlist <- function(x){
  registered_students <- x %>% 
    #filter(`Course Sec` == course) %>% #We deal with this BEFORE sending a dataframe in. 
    arrange(`Person ID`,desc(date),desc(Status)) %>%
    distinct(`Person ID`,.keep_all = TRUE) %>%
    filter(Status == "Registered")
  denom <- count(registered_students) %>% pull()
  
  ids_of_students_from_waitlist <- 
    x %>% 
    #filter(`Course Sec` == course) %>% #Again, this id done BEFORE sending a dataframe in. 
    filter(Status == "Wait List") %>% 
    select(`Person ID`)
  
  students_in_both <- inner_join(registered_students,ids_of_students_from_waitlist,by = "Person ID")
  numer <- count(students_in_both) %>% pull()
  
  percent <- numer/denom
  percent*100
}


my_tidy %>% 
  group_by(`Course Sec`) %>% 
  nest() %>% 
  mutate(percent1 = map_dbl(data,nest_percent_currently_registered_on_waitlist ))










