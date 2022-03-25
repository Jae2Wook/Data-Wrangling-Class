library(tidyverse)
library(Lahman)
library(blscrapeR)

df <- bls_api("CUSR0000SA0")
View(df)

df1 <- inflation_adjust(1995)
View(df1)

library(blscrapeR)
df <- bls_api("CUSR0000SA0", startyear = 2014, endyear = 2015)
View(df)
# Set base value.
base_value <- 100
# Get CPI from base period (January 2014).
base_cpi <- subset(df, year==2014 & periodName=="January", select = "value")
# Get the CPI for the new period (February 2015).
new_cpi <- subset(df, year==2015 & periodName=="February", select = "value")
# Calculate the updated value of our $100 investment.
(base_value / base_cpi) * new_cpi

# Find the 4-5 different data sets that you will need to show the full player
# names and full college names, as well as their annual earnings.

glimpse(Salaries)
glimpse(CollegePlaying)
glimpse(People)
glimpse(Schools)

### start - Utah players
school <- Schools %>% select(schoolID, name_full, state) %>% filter(state == "UT")
school_player <- CollegePlaying %>% inner_join(school, by = "schoolID") %>% select(-state)

# school_player %>% group_by(playerID) %>% count(schoolID) %>% count(playerID)# %>% View() # three guys played in two schools

UT_player <- school_player %>% group_by(playerID) %>% arrange(yearID) %>% slice(n()) %>% select(-schoolID, -yearID) # %>% count(playerID) %>% View()

# real name
peop <- People %>% select(playerID, nameFirst, nameLast)
peop$full_name <- str_c(peop$nameFirst, peop$nameLast, sep = " ")
peop <- peop %>% select(-nameFirst, -nameLast)

UT_player <- UT_player %>% inner_join(peop, by = "playerID")

# Adding salary
players <- UT_player %>% inner_join(Salaries, by = "playerID") %>% select(-lgID, -teamID)
View(players)

# 2020 based inflation
values <- inflation_adjust(base_year = 2020)

# implying to the salary
players$adj_salary <- 0
for (i in 1:nrow(players)) {
  players$adj_salary[i] = players$salary[i] / filter(values, values$year == players$yearID[i]) %>% pull(adj_value)
}

## graph
coun <- data.frame(table(players$name_full))

ggplot(players, aes(x = name_full, fill = name_full)) + 
  geom_bar() +
  geom_text(aes(label = ..count..), stat = "count", nudge_y = 4) + # vjust = -0.5
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Number of MLB Players from Their School", y = "Number of Players", x = "School Name")

options(scipen=5)
ggplot(players, aes(x = name_full, y = adj_salary, color = name_full)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::comma_format(big.mark = ",")) +
  labs(title = "Salary for Each School Graduate", x = "School", y = "2020 Inflation Adjusted Salary ($)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

ggplot(players, aes(x = yearID, y = adj_salary, color = name_full)) +
  geom_point() +
  labs(color = "School Name", title = "Salary for Each School Graduate", x = "School", y = "2020 Inflation Adjusted Salary ($)") +
  scale_y_continuous(labels = scales::comma_format(big.mark = ",")) +
  geom_hline(yintercept = c(6250000, 3750000), linetype = 2, color = "blue") +
  theme_bw()

