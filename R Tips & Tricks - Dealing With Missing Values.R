### R Tips & Tricks: Dealing With Missing Values ###
## Patrick Ward

## Load tidyverse
library(tidyverse)


## Create fake data
df <- tibble(
  var1 = c(4, 2, Inf, 3, NA, 6, NA, NaN, 44, 23, 78),
  var2 = c(7.8, NaN, 70, 1, 8, -Inf, NA, 99, 12, 3, 2.2))

df

## Get a count of the number of NA, NaN, and Inf in each column
data.frame(NA_cols = sapply(df, function(x) sum(length(which(is.na(x))))),
           NaN_cols = sapply(df, function(x) sum(length(which(is.nan(x))))),
           Inf_cols = sapply(df, function(x) sum(length(which(is.infinite(x)))))) %>%
  t()


## Remove Rows with NA and Inf
# One row at a time
df %>%
  filter(!is.na(var1),
         !is.na(var2),
         !is.infinite(var1),
         !is.infinite(var2))

# All numeric rows at once
df %>%
  filter_if(is.numeric, all_vars(!is.na(.))) %>%
  filter_if(is.numeric, all_vars(!is.infinite(.)))

## Use tidyverse to convert NA and NaN to the median of column 1 and Inf to 0 in both columns

df <- df %>% mutate(var1_new = ifelse(is.na(var1), median(var1, na.rm = T), ifelse(
    is.infinite(var1), 0, var1)), 
  var2_new = ifelse(is.na(var2), median(var2, na.rm = T), ifelse(
    is.infinite(var2), 0, var2)))

df


