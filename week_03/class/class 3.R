pacman::p_load(tidyverse)
rcw <- read_csv(
  "https://byuistats.github.io/M335/data/rcw.csv",
  col_types = 
    cols(Semester_Date = col_date(format = "%m/%d/%y"), 
         Semester = col_factor(
           levels = c("Winter", "Spring", "Fall") ) ) )

ggplot(rcw, aes(x = Semester_Date, y = Count, fill = Department)) +
  geom_col(position="dodge")

ggplot(rcw, aes(x = Semester_Date, y = Count, fill = Department)) +
  geom_col(position="fill")
ggplot(rcw, aes(x = Semester_Date, fill = Department)) +
  geom_bar()

ggplot(rcw, aes(x = Semester_Date, y = Count, fill = Department)) +
  geom_col()

ggplot(rcw, aes(x = as.factor(Semester_Date), y = Count, fill = Department)) +
  geom_col(position="fill")


library(readr)
mydata <- read_csv("week_03/class/positive-rate-daily-smoothed.csv", col_types = cols(
  Entity = col_character(),
  Code = col_character(),
  Day = col_date(format = ""),
  short_term_positivity_rate = col_double()
))
glimpse(mydata)

mydata_wrangled <- mydata %>% 
  filter(Entity %in%c("Germany", "Ethiopia", "Chile", "France"))

mydata_wrangled %>% ggplot(aes(x = Day, y = short_term_positivity_rate, group = Entity, color = Entity)) +
  geom_line() +
  geom_point(size = 0.5) +
  labs(x = "Date", y = "Cases")


plastics <- read_csv("week_03/class/plastics-top-rivers.csv")
glimpse(plastics)

wrangled <- plastics %>% top_n(n = 10, wt = `Share of global plastics emitted to ocean`) %>% arrange(desc(`Share of global plastics emitted to ocean`))

wrangled %>% ggplot(aes(x = `Share of global plastics emitted to ocean`, y = as.factor(Entity), fill = Entity)) +
  geom_col()
