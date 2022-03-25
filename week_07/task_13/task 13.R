# Relational data: multiple tables of data

# Mutating joins: add new variables to one data frame from
# mathcing observations in another.

# Filtering joins: filter observations from one data frame
# based on whether or not they match an observation
# in the other table.

# Set operations: treat observations as if they were set elements.

library(tidyverse)
library(nycflights13)

airlines
airports
planes
weather

# key: connect each pair of tables.
# primary key: unique variable in own table
# foreign key: unique variable in another table.

planes %>% count(tailnum) %>% filter(n>1)
# tailnum is primary key

weather %>% count(year, month, day, hour, origin) %>% filter(n > 1)

# neither unique:
flights %>% 
  count(year, month, day, flight) %>% 
  filter(n > 1)

flights %>% 
  count(year, month, day, tailnum) %>% 
  filter(n > 1)

# useful to add one with mutate() and row_number().
# row_number() sounds good to me

## Mutating joins: combine variables from two tables.

flights2 <- flights %>% 
  select(year:day, hour, origin, dest, tailnum, carrier)

# left_join()
flights %>% 
  select(-origin, -dest) %>% 
  left_joing(airlines, by = "carrier")

# mutate(match())
flights2 %>% 
  select(-origin, -dest) %>% 
  mutate(name = airlines$name[match(carrier, airlines$carrier)])

x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  3, "x3"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  4, "y3"
)

# inner join: join common variables in both data set..

x %>% inner_join(y, by = "key")

# outer join: keeps obserations that appear in at least one of the tables.
# left join: keeps all observations in x.
# right join: keeps all observations in y.
# full join: keeps all observations in x and y

# Duplicate keys

x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  1, "x4"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2"
)

left_join(x, y, by = "key")

# This is usually an error because in neither table do the keys uniquely identify an observation.
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  3, "x4"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  2, "y3",
  3, "y4"
)

left_join(x, y, by = "key")

# natural join: by = NULL, appear in both tables.

flights2 %>% left_join(weather)

flights2 %>% left_join(planes, by = "tailnum")

# ???????????
flights2 %>% 
  left_join(airports, by = c("dest" = "faa"))

flights2 %>% 
  left_join(airports, c("origin" = "faa"))

library(maps)

airports %>%
  semi_join(flights, c("faa" = "dest")) %>%
  ggplot(aes(lon, lat)) +
  borders("state") +
  geom_point() +
  coord_quickmap()

# semi_join(x, y) keeps all observations in x that have a match in y.
# anti_join(x, y) drops all observations in x that have a match in y.

top_dest <- flights %>% 
  count(dest, sort = TRUE) %>% 
  head(10)

flights %>% 
  filter(dest %in% top_dest$dest)

flights %>% 
  semi_join(top_dest)

flights %>% 
  anti_join(planes, by = "tailnum") %>% 
  count(tailnum, sort = TRUE)

# Join problems
# 1. Identify primary key.
airports %>% count(alt, lon) %>% filter(n > 1)

# 2. Should be no NAs.

# 3. Check foreign keys match primary keys in another table. Use anti_join()

df1 <- tribble(
  ~x, ~y,
  1,  1,
  2,  1
)
df2 <- tribble(
  ~x, ~y,
  1,  1,
  1,  2
)

intersect(df1, df2)

union(df1, df2)

# return observations in x, but not in y.
setdiff(df1, df2)

setdiff(df2, df1)

# Data Research

# Part 1
Setp 1: Make an outline what I want to do with the data.
Step 2: Play with data (types of variables, are data interactive, missing values)
Step 3: Make into tidy data
Step 4: Visualize (for exploration; quick graph)
Step 5: Answer my question. (Using model: 60% training, 40% testing)

# Part 2
# Find 3-5 potential data sources (that are free). Include a link to each source, and document some information about the source
(Kaggle COVID 19)(https://www.kaggle.com/gauravduttakiit/covid-19)
It has eight csv data sets. The data is up to date. It has US and worldwide aggregate data.

(St. Louis FRED)[https://fred.stlouisfed.org/]
It has many economical data sets. It not only has economical data but also fun data too such as
airlines ticket price data.

(World Bank)[https://data.worldbank.org/]
World Bank is a national institutional bank for developing countries.
This data will show out of the U.S. situation well.

https://datatopics.worldbank.org/sdgatlas/goal-10-reduced-inequalities/

# Write R code that reads in, formats, and quickly visualizes the data (Aka, perform exploratory data analysis for each source.)

Kaggle

library(readr)
covid_data <- read_csv("C:/Users/Jae/Desktop/R/Math 335/project/countries-aggregated.csv/countries-aggregated.csv")
View(covid_data)

options(scipen = 999)
covid_data %>% ggplot(aes(x = Date, y = Confirmed)) +
  geom_line(aes(group = Country, color = Country)) +
  geom_line(data = covid_data %>% filter(Country == "US"), aes(x = Date, y = Confirmed, color = Country), size = 1.5) +
  scale_y_continuous(labels = scales::comma_format(big.mark = ',')) + #, decimal.mark = '.')) +
  scale_x_date(expand = expansion(mult = c(0, 0.01)), date_breaks = "2 month") +
  theme(legend.position = "none")

class(covid_data$Date)

covid_data %>% filter(Date == max(Date), Confirmed == max(Confirmed))

sum(covid_data %>% filter(Country == "US") %>% select(Confirmed))

FRED(CPI)

CPI <- read_csv("C:/Users/Jae/Desktop/R/Math 335/project/CPILFESL.csv")

CPI <- CPI %>% lag(CPILFESL)
CPI$last <- lag(CPI$CPILFESL, 1)
CPI$increase_rate <- (CPI$CPILFESL - CPI$last) / CPI$last * 100
CPI <- CPI[-1,]

ggplot(CPI, aes(x = DATE, y = increase_rate)) +
  geom_line()


FRED(inequality)

LA_county <- read_csv("C:/Users/Jae/Desktop/R/Math 335/project/inequality/2020RATIO006037.csv")
Sanfran_county <- read_csv("C:/Users/Jae/Desktop/R/Math 335/project/inequality/2020RATIO006075.csv")
Cook_county <- read_csv("C:/Users/Jae/Desktop/R/Math 335/project/inequality/2020RATIO017031.csv")
Hudson_county <- read_csv("C:/Users/Jae/Desktop/R/Math 335/project/inequality/2020RATIO034017.csv")
NY_county <- read_csv("C:/Users/Jae/Desktop/R/Math 335/project/inequality/2020RATIO036061.csv")

inequality <- LA_county %>%  left_join(Sanfran_county)
inequality <- inequality %>% left_join(Cook_county)
inequality <- inequality %>% left_join(Hudson_county)
inequality <- inequality %>% left_join(NY_county)
inequality <- inequality %>% pivot_longer(cols = c(`2020RATIO006037`, `2020RATIO006075`, `2020RATIO017031`, `2020RATIO034017`, `2020RATIO036061`), names_to = "city", values_to = "value")
inequality <- inequality %>% group_by(DATE) %>% mutate(average = (sum(`2020RATIO006037`, `2020RATIO006075`, `2020RATIO017031`, `2020RATIO034017`, `2020RATIO036061`)/5))

ggplot(inequality, aes(x = DATE, y = average)) +
  geom_line()


# Part 3: Summarize the advantages and limitations of using these data sources to address your original project question. If needed, identify an alternative question you could use for your project.

Kaggle:
My mind is settled since the case study 2. This data is up-to-date. It has confirmed, recovered and death by every country.
I will use the COVID-19 data and show daily change all around the world by showing map graph.
Also, I want to have a line graph that can move simutaneously with the map graph.
Since, the data from Kaggle has all the nations and all the days since COVID-19 happened, I would enjoy using these data sets. 
But I am not sure why the line graph is not dropping since the data is not 30,000,000.

FRED(CPI):
The data is about Consumer Price Index for All Urban Consumers: All Items Less Food and Energy in U.S. City Average.
Nowadays inflation is the big issue. The data exist up to 2021 April 1st. Central Bank said this will be a temperal inflation.
We do not know how long is temperal. We can know after it happened. 
There were two spikes around 1975 and 1980. In early 2020, there was the biggest spike downward.
After that, the spike is keep going up. I am not sure how economists will interprete this.
However, I am sure this is not usual. The limitation is I am not sure which data I can add to the current one, so I get a cool insigths from aggregate data.

I tried to get data from the World Bank data website. However, I could not figure it out.
FRED(inequality):
This data represents the ratio of the mean income for the highest quintile (top 20 percent) of earners divided by the mean income of the lowest quintile (bottom 20 percent) of earners in a particular county.
As we can see the inequality ratio is increasing. I am interested in studying income inequality, however, I could not figure out what features I have to see more. So, sadly, I have to stop here.

