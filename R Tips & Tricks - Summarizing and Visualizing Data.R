### R Tips & Tricks: Summarizing and Visualizing Data ###
## Patrick Ward

## load packages -----------------------------------------------
library(tidyverse) # for data manipulation and visualization
library(gridExtra) # for organizing plot grid
library(psych) # for summary statistics

theme_set(theme_bw()) # setting the background theme for plots

## Simulate Data -----------------------------------------------
# Two periodization groups: Traditional & Block
# 16 week training program
# Pre- and post-test squat performance

set.seed(454)
participant <- 1:20
group <- rep(c("traditional periodization", "block periodization"),
             each = 20)
pre_squat <- c(round(rnorm(n = 20, mean = 130, sd = 15), 0),
               round(rnorm(n = 20, mean = 127, sd = 16), 0))
post_squat <- c(round(rnorm(n = 20, mean = 136, sd = 10), 0),
                round(rnorm(n = 20, mean = 147, sd = 6), 0))
dat <- data.frame(participant, group, pre_squat, post_squat)

dat %>% head()

### Data Manipulation ----------------------------------------------------
## Data is currently in wide format
# Turn data into long format
dat_long <- pivot_longer(data = dat, 
             cols = c("pre_squat", "post_squat"),
             names_to = "test",
             values_to = "performance")

dat_long %>% head()

# turn back into wide format
dat_wide <- pivot_wider(data = dat_long,
                        names_from = test,
                        values_from = performance)

dat_wide %>% head()

### Summary Statistics -------------------------------------------------
# Summary stats for all data using describe() from the psych package

describe(dat_long$performance)

# Summary stats by group using describeBy() from the psych package

describeBy(dat_long$performance, dat_long$group)

# Use the tidyverse package to code summary statistics by group and test time

dat_long %>%
  mutate(test = fct_relevel(test, levels = c("pre_squat", "post_squat"))) %>%
  group_by(group, test) %>%
  summarize(N = n(),
            Mean = mean(performance),
            SD = sd(performance))

## Create a column for the difference from pre to post for each participant
dat$Diff <- with(dat, post_squat - pre_squat)
dat %>% head()

# Describe the difference for each group
dat %>%
  group_by(group) %>%
  summarize(N = n(),
            Mean_Diff = mean(Diff),
            SD_Diff = sd(Diff))

# could have also used describeBy() from the psych package

describeBy(dat$Diff, dat$group)

## Producing Quantiles
# Quantiles for the difference in performance
quantile(dat$Diff)

# Quantiles with more probabilities returned
quantile(dat$Diff, 
         probs = seq(from = 0,
                     to = 1,
                     by = 0.1))

# Quantiles for the difference by group
by(dat$Diff, dat$group, quantile)

# Same thing but with more quantiles returned
by(dat$Diff, dat$group, quantile, probs = seq(from = 0, to = 1, by = 0.1))


#### Data Visualization ------------------------------------------------------
## Box Plots of Difference by Group
dat %>%
  ggplot(aes(x = Diff, y = group)) +
  geom_boxplot() +
  geom_vline(aes(xintercept = 0),
             color = "red",
             size = 1.2,
             linetype = "dashed") +
  labs(x = "Post - Pre Difference in Squat",
       y = "Training Group",
       title = "Changes in Squat Strength from 16-week Program")

## Histogram of Differences by Group
dat %>%
  ggplot(aes(x = Diff, fill = group)) +
  geom_density(alpha = 0.6) +
  geom_vline(aes(xintercept = 0),
             color = "red",
             size = 1.2,
             linetype = "dashed") +
  labs(x = "Post - Pre Difference in Squat",
       y = "",
       title = "Changes in Squat Strength from 16-week Program")

## Violin Plot with Points
dat %>%
  ggplot(aes(x = group, y = Diff)) +
  geom_violin() + 
  stat_summary(fun = mean,
               geom = "point",
               color = "black",
               size = 4) +
  geom_jitter(color = "grey",
              size = 3) +
  geom_hline(aes(yintercept = 0),
             color = "red",
             size = 1.2,
             linetype = "dashed") +
  labs(x = "Post - Pre Difference in Squat",
       y = "",
       title = "Changes in Squat Strength from 16-week Program")

## Joint Plot
# Build the main plot
main_plot <- dat %>%
  ggplot(aes(x = pre_squat, 
             y = post_squat, 
             color = group)) +
  geom_point(size = 3,
             alpha = 0.8) +
  theme(legend.position = "bottom")

# build margin plots
x_axis_hist <- dat %>%
  ggplot(aes(x = pre_squat, fill = group)) +
  geom_density(alpha = 0.3) +
  xlim(80, 170) +
  theme(legend.position =  "none")

y_axis_hist <- dat %>%
  ggplot(aes(x = post_squat, fill = group)) +
  geom_density(alpha = 0.3) +
  xlim(80, 170) +
  coord_flip() +
  theme(legend.position = "none")

# build an empty plot to occupy space in the second column of the plot grid
empty_space <- ggplot() +
  geom_point(aes(x = 1, y = 1), color = "white") +
  theme(axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())

# Arrange the plot grid
grid.arrange(x_axis_hist,
             empty_space,
             main_plot,
             y_axis_hist,
             ncol = 2,
             nrow = 2,
             widths = c(4, 1),
             heights = c(1, 4))

## Individual Participant Plots
dat_long %>%
  mutate(test = fct_relevel(test, levels = c("pre_squat", "post_squat"))) %>%
  ggplot(aes(x = test, y = performance, color = as.factor(participant), group = as.factor(participant))) +
  geom_point(size = 4) +
  geom_line(size = 1.1) +
  facet_wrap(~group)


