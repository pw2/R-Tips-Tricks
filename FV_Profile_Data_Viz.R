#### Force-Velocity Profile Data Visualization ####
## by Patrick Ward


## Load Packages -------------------------------------------------
library(tidyverse) # for data manipulation & visuaization
library(shiny) # for web app development

theme_set(theme_light()) # set plot background theme


## Load CSV file -----------------------------------------------
fv_profile <- read.csv(file.choose(), header = T)

# We will want a trend line that represents the average for Velocity and Power
# This will help us compare individuals
# We can build a 2nd order polynomial regressions on our data for both variables

fit_velo <- lm(Velocity ~ poly(Force, 2), data = fv_profile)
fit_power <- lm(Power ~ poly(Force, 2), data = fv_profile)

# Now we create a data set of values for Force and then predict velocity and power over them
# We will create a large number of variables between the min and max Force values observed in the data

avg_tbl <- data.frame(Force = seq(from = min(fv_profile$Force),
                                  to = max(fv_profile$Force),
                                  by = 0.5))

# now make predictions
avg_tbl$Velocity_Avg <- predict(fit_velo, newdata = avg_tbl)
avg_tbl$Power_Avg = predict(fit_power, newdata = avg_tbl)
colnames(avg_tbl)[1] <- "Force_Grp"


## Plot Force-Velocity Profile ---------------------------------
## All athletes and team average trend
ggplot(fv_profile,
       aes(x = Force, y = Velocity)) +
  geom_point(size = 3) +
  stat_smooth(method='lm', 
              formula = y~poly(x, 2),
              se= F) +
  labs(title = "FV Profile",
       subtitle = "Team Overview",
       caption = "Gymaware Data") +
  theme(axis.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16, face = "bold"),
        plot.caption = element_text(color = "blue", face = "bold", size = 12))

## All athletes and team average trend but with a dashed trend line
ggplot(fv_profile,
       aes(x = Force, y = Velocity)) +
  geom_point(size = 3) +
  stat_smooth(method='lm', 
              formula = y~poly(x, 2),
              se= F,
              linetype = "dashed") +
  labs(title = "FV Profile",
       subtitle = "Team Overview",
       caption = "Gymaware Data") +
  theme(axis.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16, face = "bold"),
        plot.caption = element_text(color = "blue", face = "bold", size = 12))

## Force-Velocity-Power -- Dual Y-Axis
ggplot(fv_profile,
       aes(x = Force, y = Velocity)) +
  geom_point(size = 3,
             color = "blue") +
  geom_point(aes(y = Power/1000),
             color = "red",
             size = 3) +
  scale_y_continuous(
    "Velocity", 
    sec.axis = sec_axis(~ . *1000, name = "Power")) +
  stat_smooth(aes(y = Velocity),
              method='lm', 
              formula = y~poly(x, 2),
              se= F) +
  stat_smooth(aes(y = Power/1000),
              method='lm', 
              formula = y~poly(x, 2),
              se= F,
              color = "red") +
  labs(title = "FVP Profile",
       subtitle = "Blue = Velocity | Red = Power",
       caption = "Gymaware Data") +
  theme(axis.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16, face = "bold"),
        plot.caption = element_text(color = "blue", face = "bold", size = 12))


## Individual Players on the same plot
ggplot(fv_profile,
       aes(x = Force, y = Velocity, color = Name)) +
  geom_point(size = 3) +
  stat_smooth(method='lm', 
              formula = y~poly(x, 2),
              se= F) +
  theme(strip.background = element_rect(fill = "black"),
        strip.text = element_text(color = "white",
                                  face = "bold")) +
  labs(title = "FV Profile",
       subtitle = "Team Overview",
       caption = "Gymaware Data") +
  theme(axis.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16, face = "bold"),
        plot.caption = element_text(color = "blue", face = "bold", size = 12))


## Individual Players as facets
ggplot(fv_profile,
       aes(x = Force, y = Velocity, color = Name)) +
  geom_point(size = 3) +
  stat_smooth(method='lm', 
              formula = y~poly(x, 2),
              se= F) +
  facet_wrap(~Name,
             ncol = 2) +
  theme(strip.background = element_rect(fill = "black"),
        strip.text = element_text(color = "white",
                                  face = "bold")) +
  labs(title = "FV Profile",
       subtitle = "Team Overview",
       caption = "Gymaware Data") +
  theme(axis.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16, face = "bold"),
        plot.caption = element_text(color = "blue", face = "bold", size = 12))

## Individual Players as facets with team average trend line
ggplot(fv_profile,
       aes(x = Force, y = Velocity)) +
  geom_point(aes(color = Name), 
             size = 3) +
  stat_smooth(aes(color = Name),
              method='lm', 
              formula = y~poly(x, 2),
              se= F) +
  geom_line(aes(x = Force_Grp, y = Velocity_Avg), data = avg_tbl,
            linetype = "dashed",
            size = 1) +
  facet_wrap(~Name,
             ncol = 2) +
  theme(strip.background = element_rect(fill = "black"),
        strip.text = element_text(color = "white",
                                  face = "bold")) +
  labs(title = "FV Profile",
       subtitle = "Team Overview",
       caption = "Gymaware Data") +
  theme(axis.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16, face = "bold"),
        plot.caption = element_text(color = "blue", face = "bold", size = 12))


# Force-Velocity-Power Dual Axis
ggplot(fv_profile,
       aes(x = Force, y = Velocity)) +
  geom_point(size = 3,
             color = "blue") +
  geom_point(aes(y = Power/1000),
             color = "red",
             size = 3) +
  scale_y_continuous(
    "Velocity", 
    sec.axis = sec_axis(~ . *1000, name = "Power")) +
  stat_smooth(aes(y = Velocity),
              method='lm', 
              formula = y~poly(x, 2),
              se= F) +
  stat_smooth(aes(y = Power/1000),
              method='lm', 
              formula = y~poly(x, 2),
              se= F,
              color = "red") +
  labs(title = "FVP Profile",
       subtitle = "Blue = Velocity | Red = Power",
       caption = "Gymaware Data") +
  facet_wrap(~Name,
             ncol = 2) +
  theme(axis.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16, face = "bold"),
        plot.caption = element_text(color = "blue", face = "bold", size = 12),
        strip.background = element_rect(fill = "black"),
        strip.text = element_text(color = "white", face = "bold"))


## Shiny Interactive Plot Development ---------------------------------------

###########################################
### Version 1: Independent Player Plots ###
## Create user interface

ui_v1 <- fluidPage(
  
  # Select plot inputs
  selectInput(input = "Player",
              label = "Player",
              choices = fv_profile %>% distinct(., Name) %>% pull(Name),
              selected = "A"),
  
  plotOutput(outputId = "fv.plot", 
             width = "75%")
  )


## Create the web server

server_v1 <- function(input, output){
  
  ## get data for plot
  dat <- reactive({
    dataset <- fv_profile %>% filter(Name == input$Player)
    dataset
  })
  
  ## create plot
  output$fv.plot <- renderPlot({
    d <- dat()
    fv.plot <- ggplot(d,
                      aes(x = Force, y = Velocity)) +
      geom_point(size = 3) +
      stat_smooth(method='lm', 
                  formula = y~poly(x, 2),
                  se= F) +
      theme(strip.background = element_rect(fill = "black"),
            strip.text = element_text(color = "white",
                                      face = "bold")) +
      labs(title = "FV Profile",
           subtitle = "Team Overview",
           caption = "Gymaware Data") +
      theme(axis.text = element_text(size = 12, face = "bold"),
            axis.title = element_text(size = 14, face = "bold"),
            plot.title = element_text(size = 18, face = "bold"),
            plot.subtitle = element_text(size = 16, face = "bold"),
            plot.caption = element_text(color = "blue", face = "bold", size = 12))
    
    
    print(fv.plot)			
  })
}


shinyApp(ui = ui_v1, server = server_v1)


###########################################
### Version 2: Place Players in Facets ####
## Create user interface

ui_2 <- fluidPage(
  
  # Select plot inputs -- allow for multiple players to be selected
  selectizeInput(inputId = "Player",
              label = "Player",
              choices = fv_profile %>% distinct(., Name) %>% pull(Name),
              selected = "",
              multiple = T),
  
  plotOutput(outputId = "fv.plot")
  )


## Create the web server

server_2 <- function(input, output){
  
  ## get data for plot
  dat <- reactive({
    dataset <- fv_profile %>% filter(Name %in% input$Player)
    dataset
  })
  
  ## create plot
  output$fv.plot <- renderPlot({
    d <- dat()
    fv.plot <- ggplot(d,
                      aes(x = Force, y = Velocity)) +
      geom_point(size = 3) +
      stat_smooth(method='lm', 
                  formula = y~poly(x, 2),
                  se= F) +
      facet_wrap(~Name, 
                 ncol = 3) +
      theme(strip.background = element_rect(fill = "black"),
            strip.text = element_text(color = "white",
                                      face = "bold")) +
      labs(title = "FV Profile",
           subtitle = "Team Overview",
           caption = "Gymaware Data") +
      theme(axis.text = element_text(size = 12, face = "bold", angle = 60, vjust = 1, hjust = 1),
            axis.title = element_text(size = 14, face = "bold"),
            plot.title = element_text(size = 18, face = "bold"),
            plot.subtitle = element_text(size = 16, face = "bold"),
            plot.caption = element_text(color = "blue", face = "bold", size = 12))
    
    
    print(fv.plot)			
  })
}


shinyApp(ui = ui_2, server = server_2)



###########################################
##### Version 3: Force-Velocty-Power ######
## Create user interface

ui_3 <- fluidPage(
  
  # Select plot inputs -- allow for multiple players to be selected
  selectizeInput(inputId = "Player",
                 label = "Player",
                 choices = fv_profile %>% distinct(., Name) %>% pull(Name),
                 selected = "",
                 multiple = T),
  
  plotOutput(outputId = "fv.plot")
)


## Create the web server

server_3 <- function(input, output){
  
  ## get data for plot
  dat <- reactive({
    dataset <- fv_profile %>% filter(Name %in% input$Player)
    dataset
  })
  
  ## create plot
  output$fv.plot <- renderPlot({
    d <- dat()
    fv.plot <- ggplot(d,
                      aes(x = Force, y = Velocity)) +
      geom_point(size = 3,
                 color = "blue") +
      geom_point(aes(y = Power/1000),
                 color = "red",
                 size = 3) +
      scale_y_continuous(
        "Velocity", 
        sec.axis = sec_axis(~ . *1000, name = "Power")) +
      stat_smooth(aes(y = Velocity),
                  method='lm', 
                  formula = y~poly(x, 2),
                  se= F) +
      stat_smooth(aes(y = Power/1000),
                  method='lm', 
                  formula = y~poly(x, 2),
                  se= F,
                  color = "red") +
      labs(title = "FVP Profile",
           subtitle = "Blue = Velocity | Red = Power",
           caption = "Gymaware Data") +
      facet_wrap(~Name,
                 ncol = 2) +
      theme(axis.text = element_text(size = 12, face = "bold"),
            axis.title = element_text(size = 14, face = "bold"),
            plot.title = element_text(size = 18, face = "bold"),
            plot.subtitle = element_text(size = 16, face = "bold"),
            plot.caption = element_text(color = "blue", face = "bold", size = 12),
            strip.background = element_rect(fill = "black"),
            strip.text = element_text(color = "white", face = "bold"))
    
    
    print(fv.plot)			
  })
}


shinyApp(ui = ui_3, server = server_3)




###########################################################
##### Version 4: Force-Velocty-Power & Group Average ######
## Create user interface

ui_4 <- fluidPage(
  
  # Select plot inputs -- allow for multiple players to be selected
  selectizeInput(inputId = "Player",
                 label = "Player",
                 choices = fv_profile %>% distinct(., Name) %>% pull(Name),
                 selected = "",
                 multiple = T),
  
  plotOutput(outputId = "fv.plot")
)


## Create the web server

server_4 <- function(input, output){
  
  ## get data for plot
  dat <- reactive({
    dataset <- fv_profile %>% filter(Name %in% input$Player)
    dataset
  })
  
  ## create plot
  output$fv.plot <- renderPlot({
    d <- dat()
    fv.plot <- ggplot(d,
                      aes(x = Force, y = Velocity)) +
      geom_point(size = 3,
                 color = "blue") +
      geom_point(aes(y = Power/1000),
                 color = "red",
                 size = 3) +
      scale_y_continuous(
        "Velocity", 
        sec.axis = sec_axis(~ . *1000, name = "Power")) +
      stat_smooth(aes(y = Velocity),
                  method='lm', 
                  formula = y~poly(x, 2),
                  se= F) +
      geom_line(aes(x = Force_Grp, y = Velocity_Avg), data = avg_tbl,
                linetype = "dashed",
                size = 1,
                color = "blue") +
      stat_smooth(aes(y = Power/1000),
                  method='lm', 
                  formula = y~poly(x, 2),
                  se= F,
                  color = "red") +
      geom_line(aes(x = Force_Grp, y = Power_Avg/1000), data = avg_tbl,
                linetype = "dashed",
                size = 1,
                color = "red") +
      labs(title = "FVP Profile",
           subtitle = "Blue = Velocity | Red = Power\nDashed Line = Team Avg",
           caption = "Gymaware Data") +
      facet_wrap(~Name,
                 ncol = 2) +
      theme(axis.text = element_text(size = 12, face = "bold"),
            axis.title = element_text(size = 14, face = "bold"),
            plot.title = element_text(size = 18, face = "bold"),
            plot.subtitle = element_text(size = 16, face = "bold"),
            plot.caption = element_text(color = "blue", face = "bold", size = 12),
            strip.background = element_rect(fill = "black"),
            strip.text = element_text(color = "white", face = "bold"))
    
    
    print(fv.plot)			
  })
}


shinyApp(ui = ui_4, server = server_4)
