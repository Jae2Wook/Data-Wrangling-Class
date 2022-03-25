library(tidyverse)
#devtools::install_github("drsimonj/ourworldindata")
library(ourworldindata)
financing_healthcare %>% glimpse()

#What does child mortality look like by continent in 2013?
financing_healthcare %>% 
  filter(year==2013) %>% 
  filter(!is.na(continent)) %>%
  ggplot(aes(x=continent,y = child_mort)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() 

#Let's find two countries in each continent with the highest child mortality. 
top_2 <- 
  financing_healthcare %>% 
  filter(year==2013) %>% 
  filter(!is.na(continent)) %>%
  group_by(continent) %>% 
  top_n(2,child_mort) 
#I swapped to point because I was unable to get ggrepel to get the labels on the required point. 
#In the future, I'll digest this: https://github.com/slowkow/ggrepel/issues/24
financing_healthcare %>% 
  filter(year==2013) %>% 
  filter(!is.na(continent)) %>%
  ggplot(aes(x=continent,y = child_mort, label = country)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point() +
  ggrepel::geom_label_repel(data = top_2,nudge_y = 30) +
  expand_limits(y=200)

#For positioning jitter with geom_label_repel.
#https://github.com/slowkow/ggrepel/issues/52  
pos <- position_jitter(width = 0.5, seed = 1) # specify jitter position
#For adding blanks as "" is skipped with labels.
# https://stackoverflow.com/questions/52397363/r-ggplot2-ggrepel-label-a-subset-of-points-while-being-aware-of-all-points/52398505  
financing_healthcare %>% 
  filter(year==2013) %>% 
  filter(!is.na(continent)) %>%
  mutate(mylabel = if_else(country %in% (top_2 %>% pull(country)), country, "")) %>% 
  ggplot(aes(x=continent,y = child_mort, label = mylabel)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(position = pos) + # know jitter spots
  ggrepel::geom_label_repel(box.padding = 1, position = pos, max.overlaps = 1000) + # know jitter position.
  expand_limits(y=200)
#While doing this, I learned perhaps my new favorite way to add labels.
#Create a mylabel column in the dataset
#Use mutate to populate the column, and put "" in spots I don't care about. 



band_members
band_instruments
band_members %>% inner_join(band_instruments)
band_members %>% left_join(band_instruments)
band_members %>% right_join(band_instruments)
band_members %>% full_join(band_instruments)
band_members %>% anti_join(band_instruments)
band_members %>% semi_join(band_instruments)

#We'll create two tables and view them, using tibble (tidy table)
book_table <- tibble(
  Book = c("Moby Dick","Harry Potter, Half Blood Prince","White Fang","Old Yeller","Bartleby"),
  Author_name = c("Herman Melville","JK Rowling","Jack London","Fred Gipson","Herman Melville"),
  word_count = c(206052,169441,72071,35968,8000)
)
book_table

author_table <- tibble(
  Author = c("Jane Austen","Herman Melville","JK Rowling","Fred Gipson"),
  Birth_Yr = c("1775","1819","1965","1908"),
  gender = c("F","M","F","M")
)
author_table

#Now let's compare the 4 different joints.  What is the difference? How are duplicates dealt with? What about missing data?
book_table %>% left_join(author_table, by = c("Author_name" = "Author"))
book_table %>% right_join(author_table, by = c("Author_name" = "Author"))
book_table %>% inner_join(author_table, by = c("Author_name" = "Author"))
book_table %>% full_join(author_table, by = c("Author_name" = "Author"))


########


hmm <- financing_healthcare %>% 
  group_by(year) %>% 
  mutate(average_child_mort = mean(child_mort)) %>% 
  select(year, average_child_mort) %>% 
  ungroup()

hmm2 <- hmm %>% 
  group_by(year) %>% 
  distinct() %>% 
  ungroup()


pre_19 <- hmm %>% 
  group_by(year) %>% 
  filter(year %in% c(1850, 1900)) %>% 
  distinct()

post_19 <- hmm %>% 
  group_by(year) %>% 
  filter(year %in% c(1901, 1951)) %>% 
  distinct()

pre_19_rate <- round((394.4203-416.1875)/(1900-1850), 2)
post_19_rate <- round((195.1917-393.1986)/(1951-1901),2)

ggplot() + 
  geom_jitter(hmm2, mapping = aes(x=year, y = average_child_mort)) +
  geom_smooth(hmm2, mapping = aes(x=year, y = average_child_mort))+
  geom_segment(aes(x = 1850, y = 422, xend = 1900, yend = 400), size = 1, color = "red", data = hmm2) +
  geom_segment(aes(x = 1901, y = 393, xend = 1951, yend = 195), size = 1, color = "red", data = hmm2)+
  annotate(geom = "text",
           label = paste('Decreasing by', pre_19_rate ),
           x = 1873,
           y = 415,
           angle = -14,
           vjust = -1.25,
           color = 'red',
           size = 4) +
  annotate(geom = "text",
           label = paste('Decreasing by', post_19_rate ),
           x = 1925,
           y = 300,
           angle = -56,
           vjust = 2,
           color = 'red',
           size = 4) +
  geom_vline(xintercept = 1896, color = 'blue', size = 1)+
  geom_text(label = "Dr.Budin displays incubators at Berlin World’s Fair.", aes( x = 1896, y =250, angle = 90, vjust = -1)) +
  labs(x= "Year", y = "Average Child Mortality Rates", title = "Worldwide Average Child Mortality Trends Down Sharply after 1900", subtitle = "Possible contribution being the use of incubators with premie babies")