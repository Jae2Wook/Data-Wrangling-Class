pacman::p_load(tidyverse,tidyquant,quantmod,dygraphs)

stockKR <- tq_get("KR")
stockKR %>% glimpse()
stockKR %>% class()

stockKR %>% 
  ggplot(aes(x=date)) +
  geom_candlestick(aes(x=date, open = open, high = high, low=low, close=close))

stock_xts <- timetk::tk_xts(
  data = stockKR %>% 
    filter(date>"2016-01-01"), 
  select = c(open, high, low,close), 
  date_var = date
)
stock_xts

dygraph(stock_xts) %>%  
  dyRebase(value = 1) %>%
  dyRangeSelector()

stock_xts %>% glimpse()
stock_xts %>% class()
stockKR %>% glimpse()
stockKR %>% class()


tickers_today <- c("CXW", "F", "GM", "KR", "WDC", "NKE","T", "WFC", "WMT")
my_stocks <- 
  tq_get(tickers_today)
my_stocks %>% glimpse()

my_stock %>% 
  select(symbol, data, adjusted) %>% 
  ggplot() +
  aes(x = date, y = adjusted, color = symbol) +
  geom_line()

my_stocks_pivot_wider <- my_stocks %>% 
  select(symbol, date, adjusted) %>% 
  pivot_wider(names_from = symbol, values_from = adjusted)

my_stocks_xts <- timetk::tk_xts(
  data = my_stocks_pivot_wider,
  date_var = date
)

dygraph(mystocks_xts) %>% 
  dyRebase(value = 10000) %>% 
  dyRangeSelector()

# appointment
# https://docs.google.com/spreadsheets/d/1sM72yK_JoC3cd-bzHm5ttZA_RFIxv5U1AC3rwlAIDdI/edit#gid=0

