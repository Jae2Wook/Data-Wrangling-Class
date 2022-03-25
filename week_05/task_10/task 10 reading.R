library(tidyverse)

# read_csv(): comma delimited files
# read_csv2(): semicolon delimited (,: used as decimal place)
# read_tsv(): tab delimited files
# read_delim(): delimiter

# read_fwf(): fixed width file
# fwf_widths(): specify fields by width
# fwf_position(): specify fields by position
# read_table(): reads a common variation of fixed width files where columns are separated by white space.

# read_log(): Apache style log files.

read_csv("a, b, c
         1, 2, 3
         4, 5, 6")

read_csv("The first line of metadata
         The second line of metadata
         x, y, z
         1, 2, 3", skip = 2)

read_csv("# A comment I want to skip
         x, y, z
         1, 2, 3", comment = "#")

read_csv("1, 2, 3\n 4, 5, 6", col_names = FALSE)

read_csv("1, 2, 3\n4, 5, 6", col_names = c("x", "y", "z"))

read_csv("a, b, c\n1, 2, .", na = ".")

read_csv("a,b\n1,2,3\n4,5,6")
read_csv("a,b,c\n1,2\n1,2,3,4")
read_csv("a,b\n\"1")
read_csv("a,b\n1,2\na,b")
read_csv("a;b\n1;3")

str(parse_logical(c("TRUE", "FALSE", "NA")))
str(parse_integer(c("1", "2", "3")))
str(parse_date(c("2010-01-01", "1979-10-14")))

parse_integer(c("1", "231", ".", "456"), na = ".")

x <- parse_integer(c("123", "345", "abc", "123.45"))
x
problems(x)

parse_double("1.23")
parse_double("1,23", locale = locale(decimal_mark = ","))

parse_number("$100")
parse_number("20%")
parse_number("It cost $123.45")
parse_number("123.456.789", locale = locale(grouping_mark = "."))

# String
charToRaw("Hadley")

x1 <- "El Ni\xf1o was particularly bad this year"
x2 <- "\x82\xb1\x82\xf1\x82\xc9\x82\xbf\x82\xcd"

parse_character(x1, locale = locale(encoding = "Latin1"))
# b1: +- (Latin1), (Latin2): a
parse_character(x2, locale = locale(encoding = "Shift-JIS"))

guess_encoding(charToRaw(x1))

# Factor
fruit <- c("apple", "banana")
parse_factor(c("apple", "banana", "bananana"), levels = fruit)

# Dates
parse_datetime("2020-10-01T2010")
parse_datetime("20101010")
parse_date("2020-10-01")

library(hms)
parse_time("01:10 am")
parse_time("20:10:01")

# %Y: year
# %B: full name month, %m, 2digits, %b: abb
# %d: 3 digits
# %H: 24 hrs
# %M: minutes
# %OS: real seconds
# %Z: time zone

parse_date("01/02/15", "%m/%d/%y")
parse_date("01/02/15", "%d/%m/%y")
parse_date("1 janvier 2015", "%d %B %Y", locale = locale("fr"))

# Parsing File
guess_parser("2010-10-01")
guess_parser("15:01")
guess_parser(c("TRUE", "FALSE"))
str(parse_guess("2010-10-10"))

challenge2 <- read_csv(readr_example("challenge.csv"), guess_max = 1001)
challenge2 <- read_csv(readr_example("challenge.csv", col_types = cols(.default = col_character())))

df <- tribble(
  ~x, ~y,
  "1", "1.21",
  "2", "2.32",
  "3", "4.56"
)
df
type_convert(df)

# Writing to a file
write_csv(challenge, "challenge.csv")

# Haven
# read_sas()
# read_sav()
# read_dta(): Stata file

library(haven)
# SAS
read_sav("mtcars.sas7bdat")
write_sas(mtcars, "mtcars.sas7bdat")

# SPSS
read_sav("mtcars.sav")
write_sav(mtcars, "mtcars.sav")

# Stata
read_dta("mtcars.dta")
write(mtcars, "mtcars.dta")

read_excel(xl, n_max = 3)
read_excel(xl, range = "C1:E4")
# =
read_excel(xl, range = cell_row(1:4))
read_excel(xl, range = cell_cols("B:D"))
read_excel(xl, na = "value")

# Save an ogject to a file
saveRDS(ogject, file = "my_data.rds")
# Restore the object
readRDS(file = "my_data.rds")

# Save multiple objects
save(data1, data2, rile = "data.RData")
# To load the data again
load("data.RData")

library(readr)
data <- read_rds(url("https://github.com/byuistats/data/blob/master/Dart_Expert_Dow_6month_anova/Dart_Expert_Dow_6month_anova.RDS?raw=true"))


# Use read_rds(url("WEBLOCATION.rds")) 
# to download and read the .rds file type
library(readr)
data_rds <- readRDS(url("https://github.com/byuistats/data/raw/master/Dart_Expert_Dow_6month_anova/Dart_Expert_Dow_6month_anova.RDS"))
View(data_rds)
data_rds2 <- read_rds(url("https://github.com/byuistats/data/blob/master/Dart_Expert_Dow_6month_anova/Dart_Expert_Dow_6month_anova.RDS?raw=true"))
View(data_rds2)
data_csv <- read_csv(url("https://raw.githubusercontent.com/byuistats/data/master/Dart_Expert_Dow_6month_anova/Dart_Expert_Dow_6month_anova.csv"))
View(data_csv)

library(haven)
data_sav <- read_sav("https://github.com/byuistats/data/raw/master/Dart_Expert_Dow_6month_anova/Dart_Expert_Dow_6month_anova.sav")
View(data_sav)
data_dta <- read_dta("https://github.com/byuistats/data/raw/master/Dart_Expert_Dow_6month_anova/Dart_Expert_Dow_6month_anova.dta")
View(data_dta)

# Use the library(downloader) R package and use 
# the download(mode = "wb") function to download the xlsx data 
# as read_xlsx() cannot read files from the web path.
# Use tempfile() function to download and save the file.
library(downloader)
library(readxl)
temp <- tempfile() # tmp <- tempfile(fileext=".xlsx")
download("https://github.com/byuistats/data/raw/master/Dart_Expert_Dow_6month_anova/Dart_Expert_Dow_6month_anova.xlsx", destfile = temp, mode = "wb") # wb: binary format
data_xl <- read_xlsx(temp)
View(data_xl)

#########
web.file <- "https://github.com/byuistats/data/blob/master/Dart_Expert_Dow_6month_anova/Dart_Expert_Dow_6month_anova.xlsx"
tmp <- tempfile(fileext=".xlsx")
download(web.file, destfile=tmp, mode="wb")
data_xl <- read_excel(tmp, sheet = "Discharge")
View(data_xl)

web.file <- "https://github.com/byuistats/data/blob/master/Dart_Expert_Dow_6month_anova/Dart_Expert_Dow_6month_anova.xlsx"
tmp <- tempfile(fileext=".xlsx")
zip <- download.file(web.file, destfile=tmp, mode="wb")
unzipped <- unzip(tmp)
data_xl <- read_excel(tmp, sheet = "Discharge")
View(data_xl)

zip <- download(web.file, dest="dataset.zip", mode="wb") 
unzipped <- unzip("dataset.zip")
zip1 <- read_xlsx("dataset.zip")
View(zip1)

##########

# Check that all five files you have imported into R 
# are in fact the same with all.equal()
all.equal(data_rds, data_sav, check.attributes = FALSE)
all.equal(data_rds, data_csv, check.attributes = FALSE)
all.equal(data_rds, data_dta, check.attributes = FALSE)
all.equal(data_rds, data_xl, check.attributes = FALSE)

# Use one of the files to make a graphic showing the performance of 
# the Dart, DJIA, and Pro stock selections.
# Include a boxplot, the jittered returns, and the average return in your graphic.

ggplot(data_sav, aes(x = variable, y = value, color = variable)) + 
  geom_jitter() +
  geom_boxplot() +
  # stat_summary(fun.y=mean, geom="point", shape=20, size=3, color="black") +
  theme(legend.position = "none") +
  stat_summary(aes(shape="mean",group=1),fun.y = "mean",size = 2, geom = "point", show.legend = T)

ggsave("Task10.png", width = 5, units = "in") 

#####

View(data_rds)

data <- data_rds %>% separate(contest_period, c("begin", "end"), sep = "-") %>% 
  separate(end, c("end_month", "year"), sep = -4) %>% 
  select(-begin) %>% 
  pivot_wider(names_from = variable, values_from = value)

data1 <- data %>% mutate(end_month = stringr::str_replace(end_month, "Dec.", "December")) %>% 
  mutate(end_month = stringr::str_replace(end_month, "Decembermber", "December")) %>% 
  mutate(end_month = stringr::str_replace(end_month, "Febuary ", "February"))

data1 %>% count(end_month)

mean(data1$PROS)
mean(data1$DARTS)
mean(data1$DJIA)

#data_rds %>% group_by(variable) %>% mean(value, na.rm = TRUE)

mean_value <- data.frame(variable = c("DARTS", "DJIA", "PROS"), mean = c(10.947, 4.521, 6.793))

data2 <- data_rds %>% separate(contest_period, c("begin", "end"), sep = "-") %>% 
  separate(end, c("end_month", "year"), sep = -4) %>% 
  select(-begin)

ggplot(data2, aes(x = year, y = value)) + 
  geom_jitter() +
  geom_boxplot() +
  facet_wrap(.~ variable) +
  geom_hline(aes(yintercept = value, color = value), mean_value)




mean_wt <- data.frame(cyl = c(4, 6, 8), wt = c(2.28, 3.11, 4.00))
ggplot(mtcars, aes(mpg, wt, colour = wt)) +
  geom_point() +
  geom_hline(aes(yintercept = wt, colour = wt), mean_wt) +
  facet_wrap(~ cyl)
