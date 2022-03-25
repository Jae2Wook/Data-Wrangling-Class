library(tidyverse)

abc_data <- read_csv("https://storybench.org/reinventingtv/abc7ny.csv")
kcra_data <- read_csv("https://storybench.org/reinventingtv/kcra.csv")

headlines <- 
  rbind(abc_data,kcra_data) %>%
  glimpse()

#Grab stuff that has Charlotte, but not Charlottesville
headlines %>% 
  mutate(has_charlotte = str_detect(headline,"Charlotte(?!sville)")) %>% 
  select(headline,has_charlotte) %>% 
  filter(has_charlotte == TRUE)

library(stringi)
sentences[1]
sentences[1] %>% 
  stri_stats_latex() %>% .["Words"]

count_words <- function(x){
  #put lots of code here
  #The function will return the last line
  stri_stats_latex(x) %>% .["Words"]
}

#This vectorizes count words instantly 
count_words_vectorized <- function(x){
  #put lots of code here
  #The function will return the last line
  map_int(x,function(x){stri_stats_latex(x) %>% .["Words"]})
}


sentences[1]
sentences[1] %>% count_words()
sentences[1] %>% count_words_vectorized()

sentences[1:10]
sentences[1:10] %>% count_words()
sentences[1:10] %>% count_words_vectorized()


sentences[1:10] %>% 
  tibble(sentence = .) %>% 
  mutate(words = count_words(sentence))

sentences[1:10] %>% 
  tibble(sentence = .) %>% 
  mutate(words = count_words_vectorized(sentence))

#We can use a unique identifier
sentences[1:10] %>% 
  tibble(sentence = . ) %>% 
  mutate(id = row_number()) %>% 
  group_by(id) %>% 
  mutate(words = count_words(sentence))

#We can group by
tibble(sentence = sentences[1:10] ) %>% 
  mutate(id = row_number()) %>% 
  group_by(id) %>% 
  mutate(words = count_words(sentence))

#Inside purrr
map(sentences[1:10],count_words) #%>% unlist() %>% as.vector()
map_int(sentences[1:10],count_words)

map(sentences[1:10],count_words) 
map(sentences[1:10],count_words) %>% unlist() %>% as.vector()
map_int(sentences[1:10],count_words) 
count_words_vectorized(sentences[1:10])
sentences[1:10] %>% count_words_vectorized()

str_count(sentences[1:10], pattern = " ")

count_words_vec <- function(x){
  map(x, count_words)
}

count_words_vec <- function(x){
  map_int(x,count_words)
}

sentences[1:10] %>% 
  count_words_vec()


count_words_vec <- function(x){
  map_int(x,function(y) stri_stats_latex(y)["Words"])
}

sentences[1:10] %>% 
  count_words_vec()


sentences[1:10] %>% 
  tibble(sentence = .) %>% 
  mutate(words = count_words_vec(sentence))

sentences[1:10] %>% 
  tibble(sentence = .) %>% 
  mutate(words = map_int(sentence,count_words))

sentences[1:10] %>% 
  tibble(sentence = .) %>% 
  mutate(words = map_int(sentence,function(y) stri_stats_latex(y)["Words"]))



######################
# New Stuff for Math 119
######################


###################################################
#These functions return the coefficients from MLE. 
#They use Cramer's rule for ease of parsing the code.
#These functions can be sources in a separate file. 
#They require passing both t and y, so no global variables are stored. 
###################################################
cramers.rule.2d <- function(a,b,c,d,e,f){
  #solves ax+by=c, dx+ey=f, 
  #returns c(x,y)
  c(c*e-b*f,a*f-c*d)/(a*e-b*d)
}
cramers.rule.1d <- function(a,b){
  #solves ax=b,  
  #returns x
  b/a
}
fit.1 <- function(t,y){
  C1.1 <- sum(t*(y-100))
  C2.1 <- sum(t^2)
  cramers.rule.1d(C2.1,C1.1)
}
fit.2 <- function(t,y){
  C1.2 <- sum((y-100)*t)
  C2.2 <- sum(t^2)
  C3.2 <- sum(t^3)
  C4.2 <- sum((y-100)*t^2)
  C5.2 <- sum(t^4)
  cramers.rule.2d(C2.2, C3.2, C1.2, C3.2, C5.2, C4.2)
}
fit.4 <- function(t,y){
  C1.4 <- sum(t*(y-100))
  C2.4 <- sum(t^2)
  C3.4 <- sum(t*log(0.005*t+1))
  C4.4 <- sum((y-100)*log(0.005*t+1))
  C5.4 <- sum((log(0.005*t+1))^2)
  cramers.rule.2d(C2.4,C3.4,C1.4,C3.4,C5.4,C4.4)
}
fit.5 <- function(t,y){
  C1.5 <- sum(t*(y-100*exp(-0.00005*t))*exp(-0.00005*t))
  C2.5 <- sum((t*exp(-0.00005*t))^2)
  cramers.rule.1d(C2.5,C1.5)
}
fit.6 <- function(t,y){
  C1.6 <- sum((y-100)*t)
  C2.6 <- sum(t^2)
  C3.6 <- sum(t*(1-exp(-0.0003*t)))
  C4.6 <- sum((y-100)*(1-exp(-0.0003*t)))
  C5.6 <- sum((1-exp(-0.0003*t))^2)
  cramers.rule.2d(C2.6,C3.6,C1.6,C3.6,C5.6,C4.6)
}

##############################################
# Now we can just specify what we wish to fit
##############################################
#devtools::install_github("byuidatascience/data4led")
library(data4led)
coef_from_seed <- function(seed = 2021){
  bulb <- led_bulb(1,seed=seed)
  
  t <- bulb$hours
  y <- bulb$percent_intensity
  
  values <- c()
  values[1]<-fit.1(t,y)
  values[2:3]<-fit.2(t,y)
  values[4:5]<-fit.4(t,y)
  values[6]<-fit.5(t,y)
  values[7:8]<-fit.6(t,y)
  data.frame("coef"=c("a1 in f1", "a1 in f2", "a2 in f2", "a1 in f4", "a2 in f4", "a1 in f5", "a1 in f6", "a2 in f6"),"value"=values)
}

coef_from_seed(2023)
coef_from_seed(2023)
my_seeds<-c(5435,4775,1324,7705,7320,2074,4002,2843,4223,1913,
            4401,8139,9452,1915,1423,1625,9938,6848,1464,5407,2874,7827,1600,8794,4872)
my_seeds

tibble(
  seed = my_seeds,
) %>% 
  mutate(coeffs = map(seed,coef_from_seed)) %>% 
  unnest(cols = c(coeffs)) %>% 
  pivot_wider(names_from = coef, values_from = value) %>% 
  View()

##### 

my_text <- read_lines("http://byuimath.com/bmw/all/335/uploads/Schedule/sample_converted_pdf.txt")
my_text
first_names <- my_text %>% 
  str_extract_all(pattern = "First Name:.*") %>%  # .: any character, *: any number of spaces
  unlist() %>% 
  str_replace_all(" {2,}","") %>% # {2,}: spaces two or more
  str_replace_all("First Name:","")
first_names

last_names <- my_text %>% 
  str_extract_all(pattern = "Last Name:.*") %>% 
  unlist() %>% 
  str_replace_all(" {2,}","") %>% 
  str_replace_all("Last Name:","")
last_names

email <- my_text %>% 
  str_extract_all(pattern = "Email \\(Conference updates will be sent.*") %>% 
  unlist() %>% 
  str_replace_all(" {2,}","") %>% 
  str_replace_all("Email \\(Conference updates will be sent","")
email

#The past function allows me to quickly combine two strings. 
paste("my","dog")
paste("my","dog", sep = "")
#because sep = "" is so common, but the default is sep = " ", the base R paste0 function exists. 
paste0("my","dog")


#We can now use functions to greatly simplify the above. 
extract_field <- function(searchstring){ 
  my_text %>% 
    str_extract_all(pattern = paste0(searchstring,".*")) %>% 
    unlist() %>% 
    str_replace_all(" {2,}","") %>% 
    str_replace_all(searchstring,"")
}

extract_field("First Name:")
last = extract_field("Last Name:")
email = extract_field("Email \\(Conference updates will be sent")


tibble(
  first = extract_field("First Name:"),
  last = extract_field("Last Name:"),
  email = extract_field("Email \\(Conference updates will be sent"),
  home = extract_field("Home Institution/Affiliation:"),
  status = extract_field("MAA Membership Status:"),
  position = extract_field("Position:"),
  gender = extract_field("Gender \\(for demographic purposes\\):"),
  liason = extract_field("Interested in attending a Liaison mtg Sat "),
  moderator = extract_field("Willing to moderate a Contributed Talk"),
  session = extract_field("-If so, Session Preferences:")
)

#####
library(lubridate)
my_URL <- "https://byuistats.github.io/M335/data/waitlist_DP_108.csv"
my_data <- read_csv(my_URL) 
my_tidy <- my_data %>% 
  mutate(date = lubridate::mdy_hm(`Registration Date`))
#my_tidy %>% View()
#my_tidy %>% glimpse()
# [ ] Function 1: Create a function that calculates the % of currently registered students who were at one time on the waitlist.
#What is wrong with the following?
course <- "FDMAT108-18"
percent_who_registered_from_waitlist <- function(){}




