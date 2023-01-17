
### Packages ---------------------------------------
library(tidyverse)
library(lubridate)

### Load Data -------------------------------------
catapult <- read.csv("catapult_example.csv", header = TRUE) %>%
  janitor::clean_names()

catapult

### Adjust Time ------------------------------------
# hms() function to split out duration to its component parts into a string
single_time <- catapult %>% 
  slice(1) %>% 
  pull(duration)

single_time

single_time2 <- hms(single_time)
single_time2

# Select each component 
hour(single_time2)
minute(single_time2)
second(single_time2)

# To get to minutes, hour needs to be multiplied by 60, and seconds need to be divided by 60. Then add together
hour(single_time2)*60 + minute(single_time2) + second(single_time2)/60

## Apply this to all of the data
catapult <- catapult %>%
  mutate(hour_min_sec = hms(duration),
    pract_time = hour(hour_min_sec) * 60 + minute(hour_min_sec) + second(hour_min_sec) / 60)

catapult

### Adjust Date ------------------------------------
catapult$date <- as.Date(catapult$date, "%m/%d/%y")

catapult

### Clean Up --------------------------------------
catapult %>%
  select(-duration, -hour_min_sec) %>%
  mutate(across(.cols = player_load:pract_time,
                ~round(.x, 1)),
         player_load_per_min = round(player_load / pract_time, 2))



