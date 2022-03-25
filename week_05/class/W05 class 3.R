library(tidyverse)
library(haven)

dat_gercon <- read_dta("https://byuistats.github.io/M335/data/heights/germanconscr.dta") %>% 
  glimpse(width = 60)

# Rows: 1,382
# Columns: 5
# $ gebger <chr> "brueckenau", "brueckenau", "brueckenau", …
# $ bdec   <dbl> 1850, 1850, 1850, 1850, 1850, 1850, 1850, …
# $ height <dbl> 169.62, 156.47, 172.45, 168.22, 166.59, 16…
# $ age    <dbl> 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21…
# $ co     <chr> "de-se", "de-se", "de-se", "de-se", "de-se…
  
dat_gercon %>% 
  mutate(birth_year = bdec,
         height.cm = height,
         height.in = height/2.54,
         study_id = "gercon") %>% 
  select(birth_year, height.in, height.cm, study_id) %>% 
  glimpse(width = 60)
 
# Rows: 1,382
# Columns: 4
# $ birth_year <dbl> 1850, 1850, 1850, 1850, 1850, 1850, 18…
# $ height.cm  <dbl> 169.62, 156.47, 172.45, 168.22, 166.59…
# $ height.in  <dbl> 66.77953, 61.60236, 67.89370, 66.22835…
# $ study_id   <chr> "gercon", "gercon", "gercon", "gercon"…


# zip file
library(foreign)
url_heights_se <- "https://byuistats.github.io/M335/data/heights/Heights_south-east.zip"
zip_file <- tempfile()
unzip_dir <- tempfile()
downloader::download(url_heights_se, zip_file, mode = "wb")
unzip(zipfile = zip_file, exdir = unzip_dir) #exdir: extract directory

list.files(unzip_dir) #This let's you check if the zip file has more than one file 
#What would happen if a zip file has more than one file and we didn't check that first?
list.files(unzip_dir, pattern = "DBF") #This will give the name of the files that end in DBF
file_name <- list.files(unzip_dir, pattern = "DBF")
file_name

unzip_dir
file_name
file.path(unzip_dir, file_name) # file.path() combines route to the file.

dat_dbf <- read.dbf(file = file.path(unzip_dir, file_name)) %>%
  glimpse()

dat_dbf %>% select(GEBJ, CMETER) %>% glimpse()


###
url_heights_se <- "https://byuistats.github.io/M335/data/heights/Heights_south-east.zip"

zip_file <- tempfile()
unzip_dir <- tempfile()
downloader::download(url_heights_se, zip_file, mode = "wb")
unzip(zipfile = zip_file, exdir = unzip_dir)
file_name <- list.files(unzip_dir, pattern = "DBF")

dat_dbf <- read.dbf(file = file.path(unzip_dir, file_name)) %>%
  glimpse()
###


dat_nsheight <- read_sav("http://www.ssc.wisc.edu/nsfh/wave3/NSFH3%20Apr%202005%20release/main05022005.sav") %>% 
  # as_tibble() %>%
  glimpse() 

dat_nsheight %>% 
  select(RT216I) %>%
  filter(RT216I < 0) %>%
  glimpse() 

dat_nsheight %>% 
  select(RT216I) %>%
  filter(RT216I > 11) %>%
  glimpse() 

dat_nsheight %>% 
  select(RT216I) %>%
  filter(RT216I >= 0,RT216I <= 11) %>%
  glimpse() 

# doby