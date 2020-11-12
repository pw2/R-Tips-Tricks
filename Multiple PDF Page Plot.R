
### Multiple PDF Page Plot ###

## Load packages and custom z-score function
library(tidyverse)
library(patchwork)

z_score <- function(x){
  z = (x - mean(x, na.rm = T)) / sd(x, na.rm = T)
  return(z)
}


## simulate data
athlete <- rep(LETTERS[1:10], each = 10)
pos <- rep(c("DB", "LB", "DL", "OL", "WR"), each = 20)
week <- rep(1:10, times = 10)
Total_Dist <- round(rnorm(n = length(athlete), mean = 3200, sd = 400), 0) 
HSR <- round(rnorm(n = length(athlete), mean = 450, sd = 100), 0)

df <- data.frame(athlete, pos, week, Total_Dist, HSR)
df %>% head()


## calculate individual z-scores for both training load variables
df <- df %>%
  group_by(athlete) %>%
  mutate(TD_z = z_score(Total_Dist),
         HSR_z = z_score(HSR))

### Build a function for the plots to loop over position group

plt_function <- function(POS){
  
  dist_plt <- df %>%
    filter(pos == POS) %>%
    ggplot(aes(x = as.factor(week), y = TD_z, group = 1)) +
    geom_hline(yintercept = 0) +
    geom_line(size = 1) +
    geom_area(fill = "light green", 
              alpha = 0.7) +
    facet_wrap(~athlete) +
    theme_bw() +
    theme(axis.text.x = element_text(size = 9, face = "bold"),
          axis.text.y = element_text(size = 9, face = "bold"),
          strip.background = element_rect(fill = "black"),
          strip.text = element_text(color = "white", face = "bold", size = 8)) +
    labs(x = "",
         y = "Total Distance",
         title = "Weekly Training Distance",
         subtitle = paste("Position", POS, sep = " = ")) +
    ylim(c(-3.5, 3.5))
  
  hsr_plt <- df %>%
    filter(pos == POS) %>%
    ggplot(aes(x = as.factor(week), y = HSR_z, group = 1)) +
    geom_hline(yintercept = 0) +
    geom_line(size = 1) +
    geom_area(fill = "light green", 
              alpha = 0.7) +
    facet_wrap(~athlete) +
    theme_bw() +
    theme(axis.text.x = element_text(size = 9, face = "bold"),
          axis.text.y = element_text(size = 9, face = "bold"),
          strip.background = element_rect(fill = "black"),
          strip.text = element_text(color = "white", face = "bold", size = 8)) +
    labs(x = "",
         y = "HSR",
         title = "Weekly HSR",
         subtitle = paste("Position", POS, sep = " = ")) +
    ylim(c(-3.5, 3.5))
  
  
  plots <- dist_plt | hsr_plt
  plots
  
}


## Test out the plot function to see if it works
plt_function(POS = "DB")

## Apply the plot function for each position group and print it out to a multipage PDF
pdf(width = 12, height = 8, "Team.pdf")
plt_function(POS = "DB")
plt_function(POS = "LB")
plt_function(POS = "DL")
plt_function(POS = "OL")
plt_function(POS = "WR")
dev.off()





