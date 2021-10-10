## Simulation of preferential attachment
# from The Success Equation by Michael Mauboussin, pg. 119

### load tidyverse
library(tidyverse)

theme_set(theme_classic())

## Create the jar of marbles data
marbles <- c(
  rep("red", 5),
  rep("black", 4),
  rep("yellow", 3),
  rep("green", 2),
  rep("blue", 1)
)

table(marbles)
round(prop.table(table(marbles)), 2)

## Blind folded, you randomly select a marble from the jar. You then return that marble
# to the jar and add one of the same color

marble_picker <- sample(x = marbles,
       size = 1,
       replace = TRUE)

marble_picker

marbles_one_pick <- c(marbles, marble_picker)
table(marbles_one_pick)

## We now want to do this 100 times to see which marble ends up having the most in the jar

n_picks <- 100

jar <- c(
  rep("red", 5),
  rep("black", 4),
  rep("yellow", 3),
  rep("green", 2),
  rep("blue", 1)
)

for(i in 1:n_picks){
  
  pick <- sample(x = jar,
                 size = 1,
                 replace = TRUE)
  
  jar <- c(jar, pick)
  
}

jar

# how many of each marble?
table(jar)
round(prop.table(table(jar)), 2)

# create a data frame
marbles_df <- tibble(marbles = jar)

## get the rolling count of each marble after every trial
# First, summarize the count of the starting values
marbles_starting <- marbles_df %>%
  slice(1:15) %>%
  count(marbles) %>%
  rename(counter = n) %>%
  mutate(row_id = 0)

marbles_starting

## now add a counter for each of the 100 trials
marbles_trials <- marbles_df %>% 
  slice(16:n()) %>%
  group_by(marbles) %>%
  mutate(counter = row_number()) %>%
  ungroup() %>%
  mutate(row_id = 1:n())

## put the two data sets together and plot
marbles_starting %>%
  bind_rows(marbles_trials) %>%
  group_by(marbles) %>%
  mutate(roll_count = row_number()-1 + counter[1],
         final_label = case_when(row_id == max(row_id) ~ marbles)) %>%
  ggplot(aes(x = row_id, y = roll_count, color = marbles)) +
  geom_step(size = 1.3) +
  geom_label(aes(label = final_label)) +
  scale_color_manual(values = c("black" = "black",
                                "blue" = "blue",
                                "green" = "green",
                                "red" = "red",
                                "yellow" = "#FDDA0D")) +
  labs(x = "Marble Picking Trials",
       y = "Rolling Count of Total Marbles",
       title = "Simulation of Preferential Attachment via Marble Picking Simulation",
       subtitle = "From The Success Equation by Michael Mauboussin, pg. 119") +
  theme(axis.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 15),
        legend.position = "none",
        panel.grid.major.y = element_line(color = "grey"),
        panel.grid.marjor.x = NULL)
