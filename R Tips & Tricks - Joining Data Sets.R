### R Tips & Tricks: Joining Data Sets ###
## Patrick Ward

## Load tidyverse

library(tidyverse)

## Make two data frames

trainingData <- data.frame(
  Name = c(rep(c("Bob", "Mike", "Jeff"), each = 4), rep("James", each = 3)),
  Day = c(rep(1:4, times = 3), 1:3),
  trainignLoad = round(rnorm(n = 15, mean = 400, sd = 150), 0))

wellnessData <- data.frame(
  Name = c(rep(c("Bob", "Mike", "Jeff"), each = 2), rep("James", each = 4)),
  Day = c(rep(1:2, times = 3), 1:4),
  wellness = round(rnorm(n = 10, mean = 6, sd = 1), 0))

trainingData
wellnessData


## left_join()
# Keeps all rows in the left data frame and finds all matches in the right data frame
# NA's are placed in the right data frame wherever a match is not found

trainingData %>%
  left_join(wellnessData, by = c("Name", "Day")) %>%
  as.data.frame()

## right_join
# Keeps all rows in the right data frame and finds all matches in the left data frame
# NA's are placed in the left data frame wherever a match is not found

trainingData %>%
  right_join(wellnessData, by = c("Name", "Day")) %>%
  as.data.frame()

## Inner Join
# Only retains complete matches between the left and right data frames

trainingData %>%
  inner_join(wellnessData, by = c("Name", "Day")) %>%
  as.data.frame()

## Anti Join
# Returns all non matches between the left and right data frames

trainingData %>%
  anti_join(wellnessData, by = c("Name", "Day")) %>%
  as.data.frame()

## Full Join
# retains all rows and returns NA for any rows that don't have a match

trainingData %>%
  full_join(wellnessData, by = c("Name", "Day")) %>%
  as.data.frame()
