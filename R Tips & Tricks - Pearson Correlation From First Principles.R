### R Tips & Tricks -- Pearson's Correlation from First Principles ###
## Patrick Ward

### Load Packages ------------------------------------------------------
library(Lahman)
library(tidyverse)
theme_set(theme_bw())

### Load & Pre-process Data ------------------------------------------------------
# Getting Batting data from Lahman database
Batting %>%
  head()

# Get 2016 season for those that had at least 200 at bats and only get RBI and H stats
yr2016 <- Batting %>%
  filter(yearID == 2016,
         AB >= 200) %>%
  select(playerID, yearID, RBI, H)

# Get Birth Year's from the Master Dataset
birthYr <- Master %>%
  select(playerID, birthYear)

# Join the Birth Year to the 2016 hitting data, calculate age, and calculate a discrete bin for age
yr2016 <- yr2016 %>%
  left_join(birthYr, by = "playerID") %>%
  mutate(Age = yearID - birthYear,
         AgeBin = cut_number(Age,
                             n = 4))

yr2016 %>% head()


### Plot -----------------------------------------------------------------------
# How many players in each Age bin?
yr2016 %>%
  count(AgeBin) %>%
  ggplot(aes(x = n, y = reorder(AgeBin, n))) +
  geom_col(alpha = 0.8) +
  labs(x = "Number of Players per Age Bin",
       y = "",
       title = "Number of Players per Age Bin",
       subtitle = "2016 Season, Hitters with at least 200 AB") +
  theme(axis.text = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 15))

# Visualize the relationship between Hits and RBI

yr2016 %>%
  ggplot(aes(x = H, y = RBI)) +
  geom_jitter(size = 4,
              alpha = 0.8) +
  geom_smooth(method = "lm",
              size = 2.1) +
  labs(x = "Hits",
       y = "RBI",
       title = "H ~ RBI",
       subtitle = "2016 Season, Hitters with at least 200 AB") +
  theme(axis.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 15))


### Correlation -----------------------------------------------------------------------
## First with built in R functions foir correlation
# basic correlation
cor(yr2016$H, yr2016$RBI)

# correlation with confidence intervals
cor.test(yr2016$H, yr2016$RBI) 

### Correlation from first principles
# Custom Function to Calculate Pearson's Correlation Coefficient
cor_function <- function(x, y){
  
  dat <- data.frame(x, y)
  dat <- dat %>%
    mutate(diff_x = x - mean(x),
           diff_y = y - mean(y),
           xy = diff_x * diff_y,
           x2 = diff_x^2,
           y2 = diff_y^2)
  
  r <- sum(dat$xy) / sqrt(sum(dat$x2) * sum(dat$y2))
  return(r)
}

# Custom Function to Calculate Confidence Interval for Correlation
# NOTE: the CI option is to be input either 0.9, 0.95, or 0.99 for 90%, 95% and 99% CI, respectively
cor.CI <- function(r, N, CI){
  
  fisher.Z <- .5*log((1+r)/(1-r))
  
  se.Z <- sqrt(1/(N-3))
  
  if(CI == .9){
    MOE <- 1.65*se.Z}
  else {
    if(CI == .95){
      MOE <- 1.95*se.Z}
    else {
      if(CI ==.99){
        MOE <- 2.58*se.Z}
      else{
        NA
      }
    }
  }
  
  Lower.Z <- fisher.Z - MOE
  Upper.Z <- fisher.Z + MOE
  Lower.cor.CI <- (exp(2*Lower.Z)-1)/(exp(2*Lower.Z)+1)
  Upper.cor.CI <- (exp(2*Upper.Z)-1)/(exp(2*Upper.Z)+1)
  Correlation.Coefficient.CI <- data.frame(r, Lower.cor.CI, Upper.cor.CI)
  Correlation.Coefficient.CI <- round(Correlation.Coefficient.CI, 3)
  return(Correlation.Coefficient.CI)
  }

# Apply the functions to see that they produce the same results as the R functions
cor_function(yr2016$H, yr2016$RBI)
cor.CI(r = 0.816,
       N = nrow(yr2016),
       CI = 0.95)


### Correlation Within Age Bin-----------------------------------------
# With R Functions
yr2016 %>%
  group_by(AgeBin) %>%
  summarize(COR = cor(H, RBI),
            COR_Low_CI = cor.test(H, RBI)$conf.int[1],
            COR_High_CI = cor.test(H, RBI)$conf.int[2])

# With the custom build functions
yr2016 %>%
  group_by(AgeBin) %>%
  summarize(COR = cor_function(H, RBI),
            cor.CI(r = COR,
                   N = n(),
                   CI = 0.95)[2],
            cor.CI(r = COR,
                   N = n(),
                   CI = 0.95)[3])




