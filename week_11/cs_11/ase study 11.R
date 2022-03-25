library(tidyverse)
library(USAboundaries)
library(buildings)
library(geofacet)

#  You will need to merge it with the spatial data, so find the FIPS column 
# and make the type and name match the spatial data
# Create state-level permit totals from the county level data

states <- us_states()
states %>% View()
permits %>% View() # StateAbbr # state: FIPS number


# Create at least one chart that shows trends in single 
# family building permits across the US over time

permits_2000 <- permits %>% filter(year > 2000) %>% filter(variable == "Single Family")

permits_2000_states <- permits_2000 %>% group_by(StateAbbr, year) %>% summarise(total_permits = sum(value))
View(permits_2000_states)

us_state_grid2


# big collapse
ggplot(permits_2000_states %>% filter(StateAbbr %in% c("CA", "TX", "NC", "GA", "FL", "AZ")), aes(x = year, y = total_permits)) +
  geom_line(aes(group = StateAbbr)) +
  facet_geo(~StateAbbr, grid = "us_state_grid2", label = "name") + 
  theme_bw() +
  labs(title = "Number of Permits for Single Family for Big Collapse in 2007", x = "Year", y = "Total Number of Permits") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_continuous(breaks = seq(2001, 2010, 2))

# small collapse
ggplot(permits_2000_states %>% filter(!StateAbbr %in% c("CA", "TX", "NC", "GA", "FL", "AZ")), aes(x = year, y = total_permits)) +
  geom_line(aes(group = StateAbbr)) +
  facet_geo(~StateAbbr, grid = "us_state_grid2", label = "name") + 
  theme_bw() +
  labs(title = "Number of Permits for Single Family for Small Collapse in 2007", x = "Year", y = "Total Number of Permits") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_continuous(breaks = seq(2001, 2010, 2))

# as a whole
ggplot(permits_2000_states, aes(x = year, y = total_permits)) +
  geom_line(aes(group = StateAbbr)) +
  facet_geo(~StateAbbr, grid = "us_state_grid2", label = "name") + 
  theme_bw() +
  labs(title = "Number of Permits for Single Family for Small Collapse in 2007", x = "Year", y = "Total Number of Permits") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_continuous(breaks = seq(2001, 2010, 2))


# Create at least one chart that shows trends in single family 
# building permits across counties in your state, over time

permits_2000_ID <- permits_2000 %>% filter(StateAbbr == "ID") %>% group_by(countyname, year) %>% summarise(total_permits = sum(value)) %>% separate(countyname, c("countyname", NA), sep = -7)
permits_2000_ID <- permits_2000_ID %>% separate(countyname, c("countyname", NA), sep = -7)

#permits_2000_ID %>% distinct(countyname) %>% View()
#us_id_counties_grid1 %>% distinct(name) %>% View()
#identical(us_id_counties_grid1$name, permits_2000_ID$countyname)

ggplot(permits_2000_ID, aes(x = year, y = total_permits)) +
  geom_line(aes(group = countyname)) +
  facet_geo(~countyname, grid = "us_id_counties_grid1", label = "name") +
  theme_bw() +
  labs(title = "Idaho Number of Permits for Single Family", x = "Year", y = "Total Number of Permits") +
  scale_x_continuous(breaks = seq(2001, 2010, 2)) +
  theme(axis.title.x = element_text(angle = 90, hjust = 1))



# Create at least one additional chart that could be useful for the news article



# Save your work in an .Rmd file with 1-2 paragraphs 
# summarizing your graphics and the choices you made in your visualization 
# to address the reporter’s needs