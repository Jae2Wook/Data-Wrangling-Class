library(tidyverse)
library(dygraphs)
library(tidyquant)
library(quantmod)

tickers_today <- c("CXW", "F", "GM", "KR", "WDC", "NKE","T", "WDAY", "WFC", "WMT")

stocks <- tq_get(tickers_today)

stocks <- stocks %>% filter(date >= today() - years(5))

## individual

stocks_inter <- stocks %>% select(symbol, date, adjusted) %>% pivot_wider(names_from = symbol, values_from = adjusted)

stock_xts <- timetk::tk_xts(data = stocks_inter, date_var = date)

dygraph(stock_xts) %>% 
  dyRebase(value = 25000) %>% 
  dyRangeSelector()

#### overall

stocks_avg <- stocks %>% select(symbol, date, adjusted) %>% group_by(date) %>% summarise(`average adjusted` = mean(adjusted))

stocks_avg_xts <- timetk::tk_xts(data = stocks_avg, date_var = date)

dygraph(stocks_avg_xts) %>% 
  dyRebase(value = 25000) %>% 
  dyRangeSelector()

## using volume

stocks

stocks$gap <- stocks$high - stocks$low

max(stocks$gap)

stocks %>% group_by(symbol) %>% summarise(big_vol = max(volume))

ggplot(stocks %>% filter(symbol == "CXW") %>% filter(volume < 2500000)) +
  geom_point(aes(x = volume, y = gap)) +
  geom_smooth(aes(x = volume, y = gap))

ggplot(stocks %>% filter(symbol == "KR") %>% filter(volume < 50000000)) +
  geom_point(aes(x = volume, y = gap)) +
  geom_smooth(aes(x = volume, y = gap))

ggplot(stocks %>% filter(symbol == "WFC")) +
  geom_point(aes(x = volume, y = gap)) +
  geom_smooth(aes(x = volume, y = gap))

options(scipen = 999)
## Has big Volume
ggplot(stocks %>% filter(symbol %in% c("F", "KR", "T", "WFC")) %>% filter(volume < 100000000)) +
  geom_point(aes(x = volume, y = gap)) +
  geom_smooth(aes(x = volume, y = gap)) +
  theme_bw() +
  labs(title = "Stocks with Big Volumes", x = "Volumne", y = "Gap Between Day High and Low") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(labels = scales::comma) +
  facet_wrap(.~symbol)

# Has small volume

stocks %>% filter(!symbol %in% c("F", "KR", "T", "WFC")) %>% distinct(symbol)
ggplot(stocks %>% filter(!symbol %in% c("F", "KR", "T", "WFC")) %>% filter(volume < 60000000)) +
  geom_point(aes(x = volume, y = gap)) +
  geom_smooth(aes(x = volume, y = gap)) +
  theme_bw() +
  labs(title = "Stocks with Big Volumes", x = "Volumne", y = "Gap Between Day High and Low") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(labels = scales::comma) +
  facet_wrap(.~symbol)

ggplot(stocks) +
  geom_line(aes(x = date, y = volume, color = symbol))
