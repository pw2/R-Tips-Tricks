
### Within column iteration

library(tidyverse)

## simulate data
set.seed(1)
subject <- rep(LETTERS[1:3], each = 50)
day <- rep(1:50, times = 3)
value <- c(
  round(rnorm(n = 20, mean = 120, sd = 40), 2),
  round(rnorm(n = 10, mean = 150, sd = 20), 2),
  round(rnorm(n = 20, mean = 110, sd = 30), 2),
  round(rnorm(n = 20, mean = 120, sd = 40), 2),
  round(rnorm(n = 10, mean = 150, sd = 20), 2),
  round(rnorm(n = 20, mean = 110, sd = 30), 2),
  round(rnorm(n = 20, mean = 120, sd = 40), 2),
  round(rnorm(n = 10, mean = 150, sd = 20), 2),
  round(rnorm(n = 20, mean = 110, sd = 30), 2))

df_1 <- data.frame(subject, day, value)
df_1 %>% head()

### Create a data frame of one subject for a simple example
df_2 <- df_1 %>%
  filter(subject == "A")


### EWMA 
# Each computation of ewma will become the input of the next computation. 
# Use accumulate()
# EWMA_t = lambda*x_t + (1 - lambda) * Z_t-1

lamda <- 0.3

# .x = the prior value in our new column
# .y = the value in the column of our observed data
# .f = the function to apply to the data
# First value in the new column will begin with the first value in the observed data column

df_2 <- df_2 %>%
  mutate(ewma = accumulate(value, ~ lamda * .y + (1 - lamda) * .x))

df_2 %>%
  pivot_longer(cols = value:ewma,
               names_to = "measurements",
               values_to = "observations") %>%
  ggplot(aes(x = day, y = observations, color = measurements)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c(value = "grey", ewma = "red")) +
  facet_wrap(~subject, nrow = 2) +
  theme_light()


## Now do it within each subject (grouped by each subject)

df_1 <- df_1 %>%
  group_by(subject) %>%
  mutate(ewma = accumulate(value, ~ lamda * .y + (1 - lamda) * .x))

df_1 %>%
  pivot_longer(cols = value:ewma,
               names_to = "measurements",
               values_to = "observations") %>%
  ggplot(aes(x = day, y = observations, color = measurements)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c(value = "grey", ewma = "red")) +
  facet_wrap(~subject, nrow = 2) +
  theme_light()


#### What if we want the first value of the new column to begin with 0?
# The .init argument allows us to specify the start value, however, it will add +1 rows
# So we need to subtract a row from the bottom of the data since we are starting with 0
# and forcing everything downward

df_2 %>%
  mutate(ewma = accumulate(value[-n()], ~ lamda * .y + (1 - lamda) * .x, .init = 0)) %>%
  head()


