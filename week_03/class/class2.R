pacman::p_load(tidyverse)
covid_state_url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
covid_state_data <- read_csv(covid_state_url) 

View(covid_state_data)

covid_state_data %>% 
  ggplot(aes(x = date, y = cases, color = state)) +
  geom_line()

covid_state_data %>% 
  filter(state %in% c("Utah","Idaho","Colorado")) %>% 
  ggplot(aes(x = date, y = cases, color = state)) +
  geom_line()

top_5_states <- 
  covid_state_data %>% 
  filter(date == max(date)) %>% 
  select(state,cases) %>% 
  arrange(desc(cases)) %>% 
  slice(1:5) %>% 
  pull(state) %>%
  glimpse()

covid_state_data %>% 
  filter(state %in% top_5_states) %>% 
  ggplot(aes(x = date, y = cases, color = state)) +
  geom_line()

covid_state_data %>% 
  filter(state %in% top_5_states) %>% 
  ggplot(aes(x = date, y = cases, color = fct_relevel(state,top_5_states))) +
  geom_line()

covid_state_data %>% 
  filter(state %in% top_5_states) %>% 
  ggplot(aes(x = date, y = cases, color = factor(state, top_5_states))) +
  geom_line()

covid_state_data %>% 
  filter(state %in% top_5_states) %>% 
  ggplot(aes(x = date, y = cases, color = fct_relevel(state,top_5_states))) +
  geom_line() +
  labs(color = "State") +
  theme_bw()


covid_state_data %>% 
  filter(state %in% c("Utah","Idaho","Colorado")) %>% 
  ggplot(aes(x = date, y = cases, color = state)) +
  geom_line() +
  coord_flip()

options(scipen = 999)
covid_state_data %>% 
  mutate(int_cases = as.integer(cases)) %>% 
  filter(state %in% c("Utah","Idaho","Colorado")) %>% 
  ggplot(aes(x = date, y = cases, color = state)) +
  geom_line()

covid_state_data %>% 
  filter(state %in% c("Utah","Idaho","Colorado")) %>% 
  ggplot(aes(x = date, y = cases, color = state)) +
  geom_line() +
  scale_y_log10()

covid_state_data %>% 
  filter(state %in% c("Utah","Idaho","Colorado")) %>% 
  ggplot() +
  geom_point(aes(x = date, y = cases, color = state)) +
  geom_line(aes(x = date, y = cases, group = state))


rcw <- read_csv("https://byuistats.github.io/M335/data/rcw.csv", 
                col_types = cols(Semester_Date = col_date(format = "%m/%d/%y"), 
                                 Semester = col_factor(levels = c("Winter", "Spring", "Fall"))))
head(rcw)

ggplot(data = rcw, 
       mapping = aes(x = Semester_Date, 
                     y = Count, 
                     color = Department)) +
  geom_line() +
  geom_point()

ggplot(data = rcw, 
       mapping = aes(x = Semester_Date, 
                     fill= Department)) +
  geom_bar()

####
  top_5_states_with_max_date <- 
  covid_state_data %>% 
  filter(date == max(date)) %>% 
  top_n(cases, n = 5) %>% 
  arrange(desc(cases)) %>%
  glimpse()

top_5_states <- top_5_states_with_max_date %>% 
  pull(state)

covid_state_data %>% 
  filter(state %in% top_5_states) %>% 
  ggplot(aes(x = date, y = cases, color = fct_relevel(state,top_5_states))) +
  geom_line() +
  ggrepel::geom_label_repel( # label --> text
    data = top_5_states_with_max_date, # legend in the last day
    mapping = aes(label = state)) +
  labs(color = "State") +
  theme_bw() +
  theme(legend.position = "none")

# geom_text(), geom_label()
