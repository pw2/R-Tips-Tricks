
## Removing columns with NA for fluid table building in shiny

## packages ---------------------------------------------------
library(tidyverse)
library(shiny)

## simulate data ----------------------------------------------
d <- tribble(
  ~player, ~position, ~stat1, ~stat2, ~stat3,
  'Frank', 'Pitcher', 10, NA, 33,
  'Tom', 'Batter', NA, 14, 12,
  'Jeff', 'Batter', NA, 5, NA,
  'Harold', 'Pitcher/Batter', 12, 33, 9
)

d

## remove columns with NA for Frank, using Base R --------------------------

frank <- d %>% 
  filter(player == 'Frank')

# colSums() can be used to count the NA's in each column
colSums(is.na(frank))

# We see that stat2 has 1 NA
# We can now retain only those columns with 0 NAs

frank[ , colSums(is.na(frank)) == 0]

## remove columns with NA for Frank, using dyplr --------------------------
# In dplyr use the select_if() function

frank %>%
  select_if(~!is.na(.x))

## Build a shiny app that fluidly retains the columns without NA ----------
# UI
ui <- fluidPage(
  
  sidebarPanel(
    
    selectInput(inputId = 'player',
                label = "Select Player",
                choices = sort(unique(d$player)),
                selected = FALSE,
                multiple = FALSE)
    
  ),
  
  mainPanel(
    
    tableOutput(outputId = 'tbl')
  )
)

# Server
server <- function(input, output){
  
  # get selected player
  dat_tbl <- reactive({
    
    d %>%
      filter(player == input$player)
    
  })
  
  # build table
  output$tbl <- renderTable({
    
    dat_tbl() %>%
      setNames(c("Player", "Position", "Stat 1", "Stat 2", "Stat 3")) %>%
      select_if(~!is.na(.x))
    
  })
  
}


# deploy
shinyApp(ui, server)
