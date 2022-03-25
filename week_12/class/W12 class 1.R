pacman::p_load(tidyverse,tidyquant,quantmod,dygraphs)

start_date <- "2020-10-01"

#Pick my stocks.
my_symb <- c("WMT","AMZN","MSFT")
my_stocks <- 
  tq_get(my_symb, from = start_date)

#Pick friend stocks.
fr_symb <- c("GE","AMD","GOOG")
fr_stocks <- 
  tq_get(fr_symb, from = start_date)

my_stocks %>% glimpse()
my_stocks %>% class()

my_stocks %>%
  ggplot(aes(x = date,y = adjusted,color = symbol)) +
  geom_line()
fr_stocks %>%
  ggplot(aes(x = date,y = adjusted,color = symbol)) +
  geom_line()

my_adj <- 
  my_stocks %>% 
  filter(date == start_date) %>%
  pull(adjusted) %>%
  glimpse()

my_stocks_adj <- my_stocks %>%
  mutate(
    adjusted = case_when(
      symbol == my_symb[1] ~ adjusted / my_adj[1],
      symbol == my_symb[2] ~ adjusted / my_adj[2],
      symbol == my_symb[3] ~ adjusted / my_adj[3]
    )
  ) %>% glimpse()
my_stocks_adj %>% 
  ggplot(aes(x = date, y = adjusted, color = symbol)) +
  geom_line()

fr_adj <- 
  fr_stocks %>% 
  filter(date == start_date) %>%
  pull(adjusted) %>%
  glimpse()

fr_stocks_adj <- fr_stocks %>%
  mutate(
    adjusted = case_when(
      symbol == fr_symb[1] ~ adjusted / fr_adj[1],
      symbol == fr_symb[2] ~ adjusted / fr_adj[2],
      symbol == fr_symb[3] ~ adjusted / fr_adj[3]
    )
  ) %>% glimpse()
fr_stocks_adj %>% 
  ggplot(aes(x = date, y = adjusted, color = symbol)) +
  geom_line()

my_stocks_adj_mean <-
  my_stocks_adj %>%
  group_by(date) %>%
  summarise(
    average_return = mean(adjusted)
  ) %>% glimpse() 
my_stocks_adj_mean %>% 
  ggplot(aes(x = date, y = average_return)) +
  geom_line()

fr_stocks_adj_mean <- 
  fr_stocks_adj %>% 
  group_by(date) %>%
  summarise(
    average_return = mean(adjusted)
  ) %>% glimpse() 
fr_stocks_adj_mean %>% 
  ggplot(aes(x = date, y = average_return)) +
  geom_line()

ggplot() +
  geom_line(data = my_stocks_adj_mean, 
            mapping = aes(x = date, y = average_return), color = "blue") +
  geom_line(data = fr_stocks_adj_mean, 
            mapping = aes(x = date, y = average_return), color = "red")

bind_rows(list(my_stocks_adj_mean, fr_stocks_adj_mean), .id = "who") %>% 
  mutate( who = ifelse(who == 1,"Mine","Yours")) %>% 
  ggplot(aes(x = date, y = average_return, color = who)) +
  geom_line()


# Task 24

pacman::p_load(tidyverse,tidyquant,quantmod,dygraphs)

stockKR <- 
  tq_get("KR")

stockKR %>% 
  filter(date>"2016-03-24") %>%
  filter(date<"2021-03-24") %>%
  ggplot(aes(x=date)) +
  geom_candlestick(aes(x=date, open = open, high = high, low=low, close=close))

stockKR %>% class()
stock_xts <- timetk::tk_xts(
  data = stockKR %>% 
    filter(date > today() - years(5)) %>%
    filter(date < today()), 
  select = c(open, high, low,close), 
  date_var = date
)
stock_xts %>% class() # xts allow to use Dygraph

dygraph(stock_xts) %>%  
  dyRangeSelector(dateWindow = c(today()-years(2), today())) # range default

stock_xts <- timetk::tk_xts(
  data = stockKR %>% 
    filter(date>"2016-01-01"), 
  select = c(open, high, low,close), 
  date_var = date
)
dygraph(stock_xts) %>%  
  dyRebase(value = 10000) %>%
  dyRangeSelector() %>% 
  dyAnnotation("2018-6-20", text = "A", tooltip = "Closed 2 stores")













