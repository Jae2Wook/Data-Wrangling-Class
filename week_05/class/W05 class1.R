library(gapminder)
library(tidyverse)

# not good for tidyverse
gap_wider <- gapminder %>% 
  select(country,year,lifeExp) %>% 
  pivot_wider( names_from = year, values_from = lifeExp)

gap_long <- gap_wider %>% 
  pivot_longer(cols = -country, names_to = "year", values_to = "cases") %>% 
  mutate(year = as.integer(year))

gapminder %>% 
  ggplot(aes(x = year, y = lifeExp)) +
  geom_point()

table4a  # cases
#> # A tibble: 3 x 3
#>   country     `1999` `2000`
#> * <chr>        <int>  <int>
#> 1 Afghanistan    745   2666
#> 2 Brazil       37737  80488
#> 3 China       212258 213766
colnames((table4a))

table4a %>% 
  pivot_longer(cols = c(`1999`, `2000`), names_to = "year", values_to = "cases")

table4a %>% 
  pivot_longer(cols = -country, names_to = "year", values_to = "cases")

table3
#> # A tibble: 6 x 3
#>   country      year rate             
#> * <chr>       <int> <chr>            
#> 1 Afghanistan  1999 745/19987071     
#> 2 Afghanistan  2000 2666/20595360    
#> 3 Brazil       1999 37737/172006362  
#> 4 Brazil       2000 80488/174504898  
#> 5 China        1999 212258/1272915272
#> 6 China        2000 213766/1280428583
table3 %>% 
  separate(rate, into = c("cases", "population"), sep = "/")



