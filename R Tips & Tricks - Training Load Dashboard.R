#### R Tips & Tricks: Building a Training Load Dashboard
## by Patrick Ward

### Packages ------------------------------------------------------------------------
library(tidyverse)
library(shiny)
theme_set(theme_classic())

# custom function for calculating z-score
z_score <- function(x){
  z = (x - mean(x, na.rm = T)) / sd(x, na.rm = T)
  return(z)
}

### Simulate Data -------------------------------------------------------------------
set.seed(55)
athlete <- rep(LETTERS[1:10], each = 15)
position <- rep(c("fwd", "fwd", "fwd", "fwd", "guard", "guard", "guard", "center", "center", "center"), each = 15)
week <- rep(c("pre_1", "pre_2", "pre_3", 1:12), times = 10)
training_load <- round(rnorm(n = length(athlete), mean = 1500, sd = 350), 1)

df <- data.frame(athlete, position, week, training_load)
df$flag <- factor(df$flag, levels = c("high", "normal", "low"))
df$week <- factor(df$week, levels = c("pre_1", "pre_2", "pre_3", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))

df %>% head()

################################
########## Shiny App ###########
################################

tl_ui <- fluidPage(
  
  # select position group input
  selectInput(input = "position2",
              label = "position",
              choices = df %>% ungroup() %>% distinct(., position) %>% pull(position),
              selected = "fwd"),
  
  # select week input
  checkboxGroupInput(inputId = "week2",
                     label = "week",
                     choices = df %>% ungroup() %>% distinct(., week) %>% pull(week),
                     selected = "pre",
                     inline = T),
  
  mainPanel(plotOutput(outputId = "tl.plot"),
            tableOutput(outputId = "tl.tbl"))
)



tl_server <- function(input, output){
  
  # get data for plot
  dat1 <- reactive({
    d <- df %>% 
      group_by(athlete) %>%
      mutate(tl_z = z_score(training_load),
             flag = ifelse(tl_z < -1, "low",
                           ifelse(tl_z > 1, "high",
                                  "normal"))) %>%
      filter(position %in% input$position2,
             week %in% input$week2)
    d
  })
  
  # make plot
  output$tl.plot <- renderPlot({
    d <- dat1()
    tl.plot <- ggplot(d, aes(x = week, y = tl_z, group = 1)) +
      geom_rect(aes(ymin = -1,
                    ymax = 1),
                xmin = 0,
                xmax = Inf,
                fill = "light grey",
                alpha = 0.2) +
      geom_hline(aes(yintercept = 0),
                 size = 1.2) +
      geom_line(size = 1.1) +
      geom_point(aes(color = flag),
                 size = 3) +
      ylim(-3, 3) +
      facet_wrap(~athlete) +
      theme(strip.background = element_rect(fill = "black"),
            strip.text = element_text(color = "white", 
                                      face = "bold", 
                                      size = 14),
            axis.text = element_text(face = "bold",
                                     size = 12),
            axis.title = element_text(face = "bold",
                                      size = 14)) +
      labs(x = "Training Week",
           y = "Athlete Standardized Training Load")
    
    print(tl.plot)
  })
  
  # get data for table
  dat2 <- reactive({
    d <- df %>% 
      filter(position %in% input$position2,
             week %in% input$week2) %>%
      select(athlete, week, training_load) %>%
      mutate(training_load = round(training_load, 0)) %>%
      pivot_wider(.,
                  names_from = week,
                  values_from = training_load)
    
    d
  })
  
  # build the table
  output$tl.tbl <- renderTable(dat2(), align = "l")
  
}


shinyApp(tl_ui, tl_server)



