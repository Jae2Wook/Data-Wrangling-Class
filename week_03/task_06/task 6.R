library(readr) # opens installed library
library(tidyverse)
library(ggplot2)

rcw <- read_csv("https://byuistats.github.io/M335/data/rcw.csv", col_types = cols(Semester_Date = col_date(format = "%m/%d/%y"), Semester = col_factor(levels = c("Winter", "Spring", "Fall"))))

# col_types = cols(): fix the type of the Semester_Date column by specifying that Semester_Date is a date column
# and I can specify by col_double(), col_logical(), col_character(), col_date(format = "")

head(rcw) # shows first six rows of the data set.
str(rcw) # shows the data set structure.

ggplot(data = rcw, 
       mapping = aes(x = Semester_Date, 
                     y = Count, 
                     color = Department)) + # x-axis: Semester_Date, y-axis: Count, color by Departments 
  geom_line() + # line plot
  geom_point() # scatter plot


# Create your own chart to show R&CW attendance trends over time.
rcw$Semester_Date = as.factor(rcw$Semester_Date)

ggplot(rcw, aes(x = Semester_Date, y = Count)) +
  geom_bar(stat = "identity", aes(fill = Department)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black"), axis.text.y = element_text(color = "black")) +
  labs(title = "R&CW Attendance Trends Over Time \nby Departments",
       y = "Attendees", x = "Dates")

aggregate(rcw$Count, by=list(Date = rcw$Semester_Date), FUN=sum)

rcw %>% group_by(Semester_Date, Department)

rcw %>% select(Semester_Date) %>% unique()

rcw %>% pull(Semester_Date) %>% unique() # pull() makes a vector

rcw %>% distinct(Semester_Date) # Similar to select() %>% unique


# Yearly growth
ggplot(rcw, aes(x = Year, y = Count)) +
  geom_bar(stat = "identity", aes(fill = Department))

ggplot(rcw, aes(x = Semester_Date, y = Count)) +
  geom_bar(stat = "identity", aes(fill = Department), position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

rcw %>% group_by(Semester_Date)
rcw1 <- rcw %>% select(c("Semester_Date", "Department", "Count"))
View(rcw1)

# pivot table https://stackoverflow.com/questions/18622854/how-to-create-a-pivot-table-in-r-with-multiple-3-variables
install.packages("reshape2")
library(reshape2)

x <- rcw %>% filter(Year == "2018")
sum(x[, "Count"])

# better than the dodge graph
ggplot(rcw, aes(x = Semester_Date, y = Count)) + 
  geom_bar(stat="identity", width = 0.25, fill = "steelblue") +
  facet_wrap(~Department, nrow =2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "R&CW Attendance Trends Over Time \nby Departments", x = "Dates", y = "Attendees")



# Write a short description of the trends you see in the graphs.
# We can not conclude when are the constant high attendants for R&CW with seeing the data we have.
# We can see that ME, CSEE, and Chem majors are the biggest attendees.

# 


