
### Normalizing test dates & calculating test differences

## load packages ----------------------------------------------
library(tidyverse)
library(lubridate)

## Simulate data ----------------------------------------------
set.seed(78)
dat <- tibble(
  
  athlete = rep(c("Tom", "Bob", "Franklin"), times = c(5, 10, 3)),
  test_dates = c(
    seq(as.Date("2023-01-01"), as.Date("2023-01-5"), by = "days"),
    seq(as.Date("2023-02-15"), as.Date("2023-02-24"), by = "days"),
    as.Date(c("2023-01-19", "2023-01-30", "2023-02-26"))
  ),
  jump_height = round(rnorm(n = 18, mean = 28, sd = 2.5), 1)
  
)

dat

### Create a test number ------------------------------------------
dat <- dat %>%
  group_by(athlete) %>%
  mutate(test_day = row_number())

dat

### Calculate the time between tests -------------------------------
dat <- dat %>%
  group_by(athlete) %>%
  mutate(time_btw_tests = coalesce(as.numeric(as.character(difftime(test_dates, lag(test_dates)))), 0))

dat


### Calculate difference in jump height from one day to the next -------------------
dat <- dat %>%
  group_by(athlete) %>%
  mutate(test_to_test_diff = jump_height - lag(jump_height))

dat


### Calculate difference in jump height from each test to the baseline test -------------
dat <- dat %>%
  group_by(athlete) %>%
  mutate(test_to_baseline_diff = jump_height - jump_height[1])

dat


### All at once -------------------------------------------------------------------------
set.seed(78)
dat <- tibble(
  
  athlete = rep(c("Tom", "Bob", "Franklin"), times = c(5, 10, 3)),
  test_dates = c(
    seq(as.Date("2023-01-01"), as.Date("2023-01-5"), by = "days"),
    seq(as.Date("2023-02-15"), as.Date("2023-02-24"), by = "days"),
    as.Date(c("2023-01-19", "2023-01-30", "2023-02-26"))
  ),
  jump_height = round(rnorm(n = 18, mean = 28, sd = 2.5), 1)
  
)

dat %>%
  group_by(athlete) %>%
  mutate(test_day = row_number(),
         time_btw_tests = coalesce(as.numeric(as.character(difftime(test_dates, lag(test_dates)))), 0),
         test_to_test_diff = jump_height - lag(jump_height),
         test_to_baseline_diff = jump_height - jump_height[1])
