### packages ------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(psych)
library(shiny)
library(plotly)

theme_set(theme_light())

### load & clean data ---------------------------------------------------------
cmj <- read.csv(file.choose(), header = TRUE) %>%
  janitor::clean_names() %>%
  mutate(date = dmy(date))

player_positions <- data.frame(name = unique(cmj$name),
                               position = c(rep("Forwards", times = 15),
                                            rep("Mids", times = 15),
                                            rep("Backs", times = 15)))

# join position data with jump data
cmj <- cmj %>%
  inner_join(player_positions)

### determine typical error and meaningful change -----------------------------
# We will use the first 2 sessions and pretend that they are our test-retest data
# Instead of 0.2 (smallest worthwhile change), which is a tiny fraction of the between
# subject SD, I'll use a moderate change (0.6), to keep things simple.

change_standards <- cmj %>%
  group_by(name) %>%
  mutate(test_id = row_number()) %>%
  filter(test_id < 3) %>%
  select(name, test_id, rel_con_peak_power) %>%
  pivot_wider(names_from = test_id,
              names_prefix = "test_",
              values_from = rel_con_peak_power) %>%
  mutate(diff = test_2 - test_1) %>%
  ungroup() %>%
  summarize(TEM = sd(diff) / sqrt(2),
            moderate_change = 0.6 * sd(c(test_1, test_2)))


### Shiny App -----------------------------------------------------------------------------

## Set up user interface

ui <- fluidPage(
  
  ## set title of the app
  titlePanel("Team CMJ Analysis"),
  
  ## create a selection bar for position group that works across all tabs
  sidebarPanel(
    selectInput(inputId = "position",
                label = "Select Position Group:",
                choices = unique(cmj$position),
                selected = "Backs",
                multiple = FALSE),
    width = 2
  ),
  
  ## set up 2 tabs: One for team daily analysis and one for player time series
  tabsetPanel(
    
    tabPanel(title = "testDay",
             
             selectInput(inputId = "date",
                         label = "Select Date:",
                         choices = unique(cmj$date)[-1],
                         selected = max(cmj$date),
                         multiple = FALSE),
             
             mainPanel(plotOutput(outputId = "day_plt", width = "100%", height = "650px"),
                       width = 12)),
    
    tabPanel(title = "Player Time Series",
             
             mainPanel(plotlyOutput(outputId = "player_plt", width = "100%", height = "700px"),
                       width = 12))
  )
  
)


server <- function(input, output){
  
  ##### Day plot tab ####
  ## day plot data
  day_dat <- reactive({
    
    d <- cmj %>%
      group_by(name) %>%
      mutate(change_power = rel_con_peak_power - lag(rel_con_peak_power)) %>%
      filter(date == input$date,
             position == input$position)
    
    d
    
  })
  
  ## day plot
  output$day_plt <- renderPlot({
    
    day_dat() %>%
      ggplot(aes(x = reorder(name, change_power), y = change_power)) +
      geom_rect(aes(ymin = -change_standards$moderate_change, ymax = change_standards$moderate_change),
                xmin = 0,
                xmax = Inf,
                fill = "light grey",
                alpha = 0.6) +
      geom_hline(yintercept = 0) +
      geom_point(size = 4) +
      geom_errorbar(aes(ymin = change_power - change_standards$TEM, ymax = change_power + change_standards$TEM),
                    width = 0.2,
                    size = 1.2) +
      theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
            axis.text = element_text(size = 16, face = "bold"),
            axis.title = element_text(size = 18, face = "bold"),
            plot.title = element_text(size = 22)) +
      labs(x = NULL,
           y = "Weekly Change",
           title = "Week-to-Week Change in Realtive Concentric Peak Power")
    
  })
  
  ##### Player plot tab ####
  ## player plot data
  
  player_dat <- reactive({
    
    d <- cmj %>%
      group_by(name) %>%
      mutate(avg = mean(rel_con_peak_power),
             sd = sd(rel_con_peak_power),
             change = rel_con_peak_power - lag(rel_con_peak_power),
             change_flag = ifelse(change >= change_standards$moderate_change | change <= -change_standards$moderate_change, "Flag", "No Flag")) %>%
      filter(position == input$position)
    
    d
  })
  
  ## player plot
  output$player_plt <- renderPlotly({
    
    plt <- player_dat() %>%
      ggplot(aes(x = date, y = rel_con_peak_power, label = change)) +
      geom_rect(aes(ymin = avg - sd, ymax = avg + sd),
                xmin = 0,
                xmax = Inf,
                fill = "light grey",
                alpha = 0.6) +
      geom_hline(aes(yintercept = avg - sd),
                 color = "black",
                 linetype = "dashed",
                 size = 1.2) +
      geom_hline(aes(yintercept = avg + sd),
                 color = "black",
                 linetype = "dashed",
                 size = 1.2) +
      geom_hline(aes(yintercept = avg), size = 1) +
      geom_line(size = 1) +
      geom_point(shape = 21,
                 size = 3,
                 aes(fill = change_flag)) +
      facet_wrap(~name) +
      scale_fill_manual(values = c("red", "black", "black")) +
      theme(axis.text = element_text(size = 13, face = "bold"),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            plot.title = element_text(size = 18),
            strip.background = element_rect(fill = "black"),
            strip.text = element_text(size = 13, face = "bold"),
            legend.position = "none") +
      labs(x = NULL,
           y = NULL,
           title = "Relative Concentric Peak Power")
    
    ggplotly(plt)
    
  })
  
  
}



shinyApp(ui, server)


