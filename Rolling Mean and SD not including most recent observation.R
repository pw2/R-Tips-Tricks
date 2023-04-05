
### Rolling Mean and SD not including the most recent observation
## Load Libraries
library(tidyverse)
library(zoo)

## Simulate Data
set.seed(45)
d <- tibble(
  day = 1:10,
  training_load = round(runif(n = 10, min = 250, max = 350), 0)
)

d

## Calculate the rolling z-score but use a 
# rolling mean and sd not inclusive of the most recent day

d <- d %>%
  mutate(lag_mean = lag(rollapplyr(data = training_load, width = day, FUN = mean, fill = NA)),
         lag_sd = lag(rollapplyr(data = training_load, width = day, FUN = sd, fill = NA)),
         z_score = (training_load - lag_mean) / lag_sd)

d

## Check to see that the lag_mean and lag_sd in the first 4 rows represents
# the data in those respective columns in row 5

first_four <- d %>%
  slice(1:4) %>%
  pull(training_load)

mean(first_four)
sd(first_four)

# Worked!

### More complex example
# Since we were using days as the width argument, what if 
# we have dates instead of days? Also, what if we have multiple athletes 
# in the data set?

set.seed(67)
d <- tibble(
  training_date = rep(seq(as.Date("2023-01-01"),as.Date("2023-01-05"), by = 1), times = 3),
  athlete = rep(c("Karl", "Bonnie", "Thomas"), each = 5),
  training_load = round(runif(n = 15, min = 250, max = 350), 0)
)

d

## create a days column by identifying the row number of observation for
# each athlete
d <- d %>%
  group_by(athlete) %>%
  mutate(training_day = row_number(), 
         lag_mean = lag(rollapplyr(data = training_load, width = training_day, FUN = mean, fill = NA)),
         lag_sd = lag(rollapplyr(data = training_load, width = training_day, FUN = sd, fill = NA)),
         z_score = (training_load - lag_mean) / lag_sd)


d
