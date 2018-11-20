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

#Here we are reading in Mr. Schroeder's data and then creating a state_district variable that we can use to map it with the Upshot data.
# Then we made the state and state_district variables lower case, just so that will stay consisent across each.

df <- read.csv("mt_2_results.csv") %>% 
  mutate(state = as.character(ï..state)) %>% 
  unite("state_district", c("ï..state", "district"), sep = "-") %>% 
  mutate(state_district = tolower(state_district)) %>% 
  mutate(state = tolower(state))

# Here we are downloading the Upshot data directly from GitHub and then unzip the actual zip file to get at the data within the 
# 2018-live-poll-results folder and then delete the zip file afterwards, because it is no longer needed.

download.file(url = "https://github.com/TheUpshot/2018-live-poll-results/archive/master.zip", destfile = "master.zip", quiet = TRUE, mode = "wb")

unzip("master.zip")

file_names <- dir_ls("2018-live-poll-results-master/data")

ps7start <- map_dfr(file_names, read_csv, .id = "source")

file_delete("master.zip")

# After succesfully getting the data into R, we begin the work of removing the senator and governor polls conducted.

newps7 <- ps7start %>% 
  mutate(sen = str_detect(source, ("sen"))) %>%
  mutate(gov = str_detect(source, ("gov"))) %>%
  filter(gov == "FALSE", sen == "FALSE")

# In this chunk we are using the source column that was mapped from when the data was read in, and creating the wave variable
# to only use the polls that were conducted in the 3rd wave.

wave3 <- newps7 %>%  
  mutate(wave = str_sub(source, start = -5, end = -5)) %>%  
  filter(wave == 3) %>% 
  
# Using the same technique, we created both the state and the district variables from the source, and then unite them to create
# a state_district variable that combines the two, and I found that changing the remove portion of the unite function keeps those
# two columns in place while creating the new one as well.
  
  mutate(state = str_sub(source, -10, -9)) %>% 
  mutate(district = str_sub(source, -8, -7)) %>%
  unite("state_district", c("state", "district"), sep = "-", remove = FALSE) 

both <- left_join(wave3, df, by = "state_district")

