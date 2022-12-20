
### Load libraries -----------------------------------------------
library(tidyverse)
library(randomNames)

### Data -------------------------------------------------------
set.seed(2022)
dat <- tibble(
  participant = randomNames(n = 20),
  performance = rnorm(n = 20, mean = 100, sd = 10))

dat %>%
  head()


### boxplot with points ---------------------------------------------
dat %>%
  mutate(of_interest = case_when(participant %in% c("Gallegos, Dennis", "Vonfeldt, Mckenna") ~ participant,
                                 TRUE ~ "everyone else")) %>%
  ggplot(aes(x = performance, y = factor(0))) +
  geom_boxplot(width = 0.2) +
  geom_point(aes(x = performance, y = factor(0),
                  fill = of_interest),
              position = position_nudge(y = -0.2),
              shape = 21,
              size = 8,
              color = "black",
              alpha = 0.6) +
  scale_fill_manual(values = c("Gallegos, Dennis" = "#E69F00", "Vonfeldt, Mckenna" = "#56B4E9", "everyone else" = "#999999")) +
  labs(x = "Performance",
       title = "Team Performance",
       fill = "Participants") +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 18),
        legend.position = "top")


### boxplot with dotplot ---------------------------------------------
# Horizontal
dat %>%
  mutate(of_interest = case_when(participant %in% c("Gallegos, Dennis", "Vonfeldt, Mckenna") ~ participant,
                                 TRUE ~ "everyone else")) %>%
  ggplot(aes(x = performance, y = positional)) +
  geom_boxplot(aes(y = 0.2),
    width = 0.2) +
  geom_dotplot(aes(y = 0, 
                   fill = of_interest),
              color = "black",
              stackdir="center",
              binaxis = "x",
              alpha = 0.6) +
  scale_fill_manual(values = c("Gallegos, Dennis" = "#E69F00", "Vonfeldt, Mckenna" = "#56B4E9", "everyone else" = "#999999")) +
  labs(x = "Performance",
       title = "Team Performance",
       fill = "Participants") +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 18),
        legend.position = "top",
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 11),
        legend.key.size = unit(2, "line")) 

# vertical
dat %>%
  mutate(of_interest = case_when(participant %in% c("Gallegos, Dennis", "Vonfeldt, Mckenna") ~ participant,
                                 TRUE ~ "everyone else")) %>%
  ggplot(aes(x=positional, y= performance)) +
  geom_dotplot(aes(x = 1.75, 
                   fill = of_interest), 
               binaxis="y", 
               stackdir="center") +
  geom_boxplot(aes(x = 2), 
               width=0.2) +
  scale_fill_manual(values = c("Gallegos, Dennis" = "#E69F00", "Vonfeldt, Mckenna" = "#56B4E9", "everyone else" = "#999999")) +
  labs(y = "Performance",
       title = "Team Performance",
       fill = "Participants") +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 18),
        legend.position = "top",
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 11),
        legend.key.size = unit(2, "line")) +
  xlim(1.5, 2.25)



