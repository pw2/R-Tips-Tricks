

### Load libraries -----------------------------------------------
library(tidyverse)
library(randomNames)
library(plotly)

### Data -------------------------------------------------------
set.seed(2022)
dat <- tibble(
  participant = randomNames(n = 20),
  performance = rnorm(n = 20, mean = 100, sd = 10))

dat %>%
  head()

### Build plotly plot -------------------------------------------
# Set plot base
perf_plt <- plot_ly(dat,
                    type = "box") %>%
  group_by(participant)

# Vertical plot
vert_plt <- perf_plt %>%
  add_boxplot(y = ~performance,
              boxpoints = "all",
              line = list(color = 'black'),
              text = ~participant,
              marker = list(color = '#56B4E9',
                            size = 15)) %>% 
  layout(xaxis = list(showticklabels = FALSE)) %>%
  layout(yaxis = list(title = "Performance")) %>%
  layout(title = "Team Performance")       

# Horizontal plot
hz_plt <- perf_plt %>%
  add_boxplot(x = ~performance,
              boxpoints = "all",
              line = list(color = 'black'),
              text = ~participant,
              marker = list(color = '#E69F00',
                            size = 15)) %>% 
  layout(yaxis = list(showticklabels = FALSE)) %>%
  layout(xaxis = list(title = "Performance")) %>%
  layout(title = "Team Performance") 

## put the two plots next to each other
subplot(vert_plt, hz_plt)


### plotly with selection box -------------------------------------------
# set 'particpant' as the group to select
person_of_interest <- highlight_key(dat, ~participant)

# create a new base plotly plot using the person_of_interest_element
selection_perf_plt <- plot_ly(person_of_interest,
                    type = "box") %>%
  group_by(participant)

# build the plot
plt_selector <- selection_perf_plt %>%
  group_by(participant) %>%
  add_boxplot(x = ~performance,
              boxpoints = "all",
              line = list(color = 'black'),
              text = ~participant,
              marker = list(color = '#56B4E9',
                            size = 15)) %>% 
  layout(yaxis = list(showticklabels = FALSE)) %>%
  layout(xaxis = list(title = "Performance")) %>%
  layout(title = "Team Performance")   

# create the selector tool
plt_selector %>%
  highlight(on = 'plotly_click',
              off = 'plotly_doubleclick',
              selectize = TRUE,
              dynamic = TRUE,
              persistent = TRUE)

