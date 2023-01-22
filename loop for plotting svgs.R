

library(tidyverse)
library(patchwork)

theme_set(theme_bw())

## data
dat <- mtcars %>%
  mutate(cyl = as.factor(cyl))

## example of the plots
p1 <- dat %>%
  ggplot(aes(x = drat, y = hp)) +
  geom_point(size = 5) +
  geom_smooth(method = "lm",
              se = FALSE) +
  ggtitle("hp ~ drat")

p2 <- dat %>%
  count(carb) %>%
  mutate(carb = as.factor(carb)) %>%
  ggplot(aes(x = n, y = reorder(carb, n))) +
  geom_col() +
  labs(x = "Count",
       y = "Carb",
       title = "Carb Count")

p3 <- dat %>%
  ggplot(aes(x = wt)) +
  geom_histogram(fill = "light grey",
                 color = "black",
                 bins = 5) +
  ggtitle("Engine Weight")


(p2 | p3) / p1

## We want to create an svg of each plot by cylinder (cyl)

# create a cyl id to loop over
cyl_id <- unique(dat$cyl)
length(cyl_id)

# create a plot function for each cyl
plt_func <- function(i){
  p1 <- i %>%
    ggplot(aes(x = drat, y = hp)) +
    geom_point(size = 5) +
    geom_smooth(method = "lm",
                se = FALSE) +
    ggtitle("hp ~ drat")
  
  p2 <- i %>%
    count(carb) %>%
    mutate(carb = as.factor(carb)) %>%
    ggplot(aes(x = n, y = reorder(carb, n))) +
    geom_col() +
    labs(x = "Count",
         y = "Carb",
         title = "Carb Count")
  
  p3 <- i %>%
    ggplot(aes(x = wt)) +
    geom_histogram(fill = "light grey",
                   color = "black",
                   bins = 5) +
    ggtitle("Engine Weight")
  
  three_plt <- (p2 | p3) / p1
  
  
  ggsave(three_plt, file = paste0(unique(i$cyl), ".svg"))
}

# setwd("name of the file path where you want to save the files goes here")
dat %>% 
  split(.$cyl) %>% 
  map(plt_func)


##### If you need a title for the plots ----------------------------
# use the plot_annotation() function from {patchwork}

(p2 | p3) / p1 + plot_annotation(title = "Engine cylinders")

## Title in the loop
# create a plot function for each cyl
plt_func <- function(i){
  p1 <- i %>%
    ggplot(aes(x = drat, y = hp)) +
    geom_point(size = 5) +
    geom_smooth(method = "lm",
                se = FALSE) +
    ggtitle("hp ~ drat")
  
  p2 <- i %>%
    count(carb) %>%
    mutate(carb = as.factor(carb)) %>%
    ggplot(aes(x = n, y = reorder(carb, n))) +
    geom_col() +
    labs(x = "Count",
         y = "Carb",
         title = "Carb Count")
  
  p3 <- i %>%
    ggplot(aes(x = wt)) +
    geom_histogram(fill = "light grey",
                   color = "black",
                   bins = 5) +
    ggtitle("Engine Weight")
  
  cyl_name <- i %>% 
    select(cyl) %>%
    distinct(cyl) %>%
    pull(cyl)
  
  three_plt <- (p2 | p3) / p1 +
    plot_annotation(title = paste(cyl_name, "cylinder", sep = " "))
  
  ggsave(three_plt, file = paste0(unique(i$cyl), ".svg"))
}

# setwd("name of the file path where you want to save the files goes here")
dat %>% 
  split(.$cyl) %>% 
  map(plt_func)
