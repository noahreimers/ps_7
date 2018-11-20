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

df <- read.csv("mt_2_results.csv", stringsAsFactors = FALSE) %>% 
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

# Performed a left join on the two datasets and joined by the state_district variable that was created in each.

both <- left_join(wave3, df, by = "state_district")



# These lines of code below are determining the predicted democratic advantage in each of the district races. The group_by and
# tally and spread allow for the weighting and counting of the responses and the total interviews conducted


polled <- both %>% 
  select(state_district, response, final_weight) %>%
  group_by(response, state_district) %>%
  tally(wt = final_weight) %>%
  spread(response, n) 

# This line of code is making all of the n/a values equal to 0.

polled[is.na(polled)] <- 0

#These lines of code are converting the predicted advantages into percentages.

polled <- polled %>% 
  mutate(total = Dem + Rep + Und + `3` + `4` + `5`) %>%
  mutate(dem_advantage = ((Dem - Rep) / total)*100) %>%
  select(state_district, dem_advantage)



df[, 4]  <- as.numeric(df[, 4])
df[, 5]  <- as.numeric(df[, 5])
df[, 6]  <- as.numeric(df[, 6])

results <- df %>%  
  mutate(total = rep_votes + dem_votes + other_votes) %>%
  mutate(dem_advantage = ((rep_votes - dem_votes) / total)*100) %>%
  select(state_district, win_party, dem_advantage)






  
write_rds(final_data,"ps_7_reimers_pirrmann_ryan/ps7.rds",compress="none")
