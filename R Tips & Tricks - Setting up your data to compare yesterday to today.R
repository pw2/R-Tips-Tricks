
### R Tips & Tricks: Setting up your data to compare yesterday to today

## Load packages
library(tidyverse)
library(lubridate)

###### Example 1: Simple Example ------------------------------------------
## Create fake data
day <- 1:10
trainingLoad <- round(rnorm(n = length(day), mean = 460, sd = 60))
soreness <- round(runif(n = length(day), min = 3, max = 6), 0)
df <- data.frame(day, trainingLoad, soreness)
df

## Evaluate trainingLoad the before to soreness the next day

df %>%
  mutate(trainingLoad_lag = lag(trainingLoad))

###### Example 2: Working Across Weeks ------------------------------------------
## Create fake data

date <- c(seq(as.Date("2020/01/05"), as.Date("2020/01/08"), by = "days"),
          seq(as.Date("2020/01/12"), as.Date("2020/01/15"), by = "days"))
trainingLoad <- round(rnorm(n = length(date), mean = 460, sd = 60), 0)
soreness <- round(runif(n = length(date), min = 3, max = 6), 0)
df <- data.frame(date, trainingLoad, soreness)
df


### Wrong Way !! ###

df %>%
  mutate(trainingLoad_lag = lag(trainingLoad))

# The date's are not all sequential

## Option 1: Always create a mesocycle in your data frame so you can use it to group by
mesocycle <- rep(c(1, 2), each = 4)
df <- data.frame(mesocycle, df)
df

# Group by Mesocycle

df %>%
  group_by(mesocycle) %>%
  mutate(trainingLoad_lag = lag(trainingLoad))


## Option 2: Use the lubridate package to find the week corresponding to the dates
date <- c(seq(as.Date("2020/01/05"), as.Date("2020/01/08"), by = "days"),
          seq(as.Date("2020/01/11"), as.Date("2020/01/14"), by = "days"))
trainingLoad <- round(rnorm(n = length(date), mean = 460, sd = 60), 0)
soreness <- round(runif(n = length(date), min = 3, max = 6), 0)
df <- data.frame(date, trainingLoad, soreness)
df


# Add in the week
df <- df %>%
  mutate(trainingWeek = week(date))

# Group by trainingWeek

df %>%
  group_by(trainingWeek) %>%
  mutate(trainingLoad_lag = lag(trainingLoad))


###### Example 3: Multiple Athletes ------------------------------------------
## Create fake data
athlete <- rep(LETTERS[1:3], each = 8)
mesocycle <- rep(rep(c(1, 2), each = 4), times = 3)
date <- rep(c(seq(as.Date("2020/01/05"), as.Date("2020/01/08"), by = "days"),
          seq(as.Date("2020/01/12"), as.Date("2020/01/15"), by = "days")), times = 3)
trainingLoad <- round(rnorm(n = length(date), mean = 460, sd = 60), 0)
soreness <- round(runif(n = length(date), min = 3, max = 6), 0)
df <- data.frame(athlete, mesocycle, date, trainingLoad, soreness)
df

# Group by athlete and mesocycle

  df %>%
    group_by(athlete, mesocycle) %>%
    mutate(trainingLoad_lag = lag(trainingLoad)) %>%
    as.data.frame()
