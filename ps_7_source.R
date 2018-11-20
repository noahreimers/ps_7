library(tidyverse)
library(dplyr)
library(knitr)
library(scales)
library(foreign)
library(fs)
library(utils)
library(RCurl)
library(curl)
library(kableExtra)
library(lubridate)

df <- read.csv("mt_2_results.csv") %>% 
  mutate(state = as.character(state))

download.file(url = "https://github.com/TheUpshot/2018-live-poll-results/archive/master.zip", destfile = "master.zip", quiet = TRUE, mode = "wb")

unzip("master.zip")

file_names <- dir_ls("2018-live-poll-results-master/data")

ps7start <- map_dfr(file_names, read_csv, .id = "source")

file_delete("master.zip")

newps7 <- ps7start %>% 
  mutate(sen = str_detect(source, ("sen"))) %>%
  mutate(gov = str_detect(source, ("gov"))) %>%
  filter(gov == "FALSE", sen == "FALSE")

wave3 <- newps7 %>%  
  mutate(wave = str_sub(source, start = -5, end = -5)) %>%  
  filter(wave == 3) %>% 
  mutate(state_district = str_sub(source, -10, -7)) %>% 
  mutate(state = str_sub(source, -10, -9)) %>% 
  mutate(district = str_sub(source, -8, -7)) %>% 
  #select(wave, state_district, state, district) %>%  
  mutate(state = toupper(state))

both <- left_join(wave3, df, by = "state")

