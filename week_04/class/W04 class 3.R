library(nycflights13)

flights %>% 
  ggplot() +
  geom_boxplot(aes(x=origin, y = dep_delay))
flights %>% 
  ggplot() +
  geom_boxplot(aes(x=origin, y = dep_delay)) +
  coord_cartesian(ylim = c(-25,20))

q3s <- flights %>% 
  filter(sched_dep_time < 1200) %>% 
  group_by(origin,carrier) %>% 
  summarize(
    q3 = quantile(arr_delay, 3/4, na.rm = TRUE)
  )

flights %>% 
  group_by(origin) %>% 
  summarize(
    min = quantile(arr_delay, 0/4, na.rm = TRUE),
    q1 = quantile(arr_delay, 1/4, na.rm = TRUE),
    median = quantile(arr_delay, 2/4, na.rm = TRUE),
    q3 = quantile(arr_delay, 3/4, na.rm = TRUE),
    max = quantile(arr_delay, 4/4, na.rm = TRUE)
  )
