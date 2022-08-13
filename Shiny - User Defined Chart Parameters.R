
## Shiny - User Defined Chart Parameters

#### Load packages ------------------------------------------------
library(tidyverse)
library(shiny)
library(Lahman)
library(gt)
library(plotly)

theme_set(theme_minimal() + 
            theme(
              axis.text = element_text(face = "bold", size = 12),
              legend.title = element_blank(),
              legend.position = "none"
            ) )

#### helper functions -------------------------------------------

z_score <- function(x){
  z = (x - mean(x, na.rm = T)) / sd(x, na.rm = T)
  return(z)
}

t_score <- function(x){
  t = (x * 10) + 50
  t = ifelse(t > 100, 100, 
             ifelse(t < 0, 0, t))
  return(t)
}


#### Get Data ---------------------------------------------------

dat <- Batting %>%
  filter(between(yearID, left = 2017, right = 2019),
         AB >= 200) %>% 
  group_by(yearID, playerID) %>%
  summarize(across(.cols = G:GIDP,
         ~sum(.x)),
         .groups = "drop") %>%
  mutate(ba = H / AB,
         obp = (H + BB + HBP) / (AB + HBP + SF),
         slg = ((H - X2B - X3B - HR) + X2B*2 + X3B*3 + HR*4) / AB,
         ops = obp + slg,
         hr_rate = H / AB) %>%
  select(playerID, yearID, AB, ba:hr_rate) %>%
  mutate(across(.cols = ba:hr_rate,
                list(z = z_score)),
         across(.cols = ba_z:hr_rate_z,
                list(t = t_score))) %>%
  left_join(People %>%
              mutate(name = paste(nameLast, nameFirst, sep = ", ")) %>%
              select(playerID, name)) %>%
  relocate(name, .before = yearID)


dat_long <-  Batting %>%
  filter(between(yearID, left = 2017, right = 2019),
         AB >= 200) %>% 
  group_by(playerID) %>%
  summarize(across(.cols = G:GIDP,
                   ~sum(.x)),
            .groups = "drop") %>%
  mutate(ba = H / AB,
         obp = (H + BB + HBP) / (AB + HBP + SF),
         slg = ((H - X2B - X3B - HR) + X2B*2 + X3B*3 + HR*4) / AB,
         ops = obp + slg,
         hr_rate = H / AB) %>%
  select(playerID, AB, ba:hr_rate) %>%
  mutate(across(.cols = ba:hr_rate,
                list(z = z_score)),
         across(.cols = ba_z:hr_rate_z,
                list(t = t_score))) %>%
  left_join(People %>%
              mutate(name = paste(nameLast, nameFirst, sep = ", ")) %>%
              select(playerID, name)) %>%
  relocate(name, .before = AB) %>%
  select(playerID:AB, ends_with("z_t")) %>%
  pivot_longer(cols = -c(playerID, name, AB),
               names_to = "stat") %>%
  mutate(stat = case_when(stat == "ba_z_t" ~ "BA",
                          stat == "obp_z_t" ~ "OBP",
                          stat == "slg_z_t" ~ "SLG",
                          stat == "ops_z_t" ~ "OPS",
                          stat == "hr_rate_z_t" ~ "HR Rate"))


dat %>%
  head()

dat_long %>%
  head()


### create the plots & table to see what they will look like
dat_long %>%
  filter(name == "Abreu, Jose") %>%
  ggplot(aes(x = stat, y = value, fill = stat)) +
  geom_col(color = "white", width = 0.75) +
  coord_polar(theta = "x") +
  geom_hline(yintercept = seq(50, 50, by = 1), size = 1.2) +
  labs(x = "", y = "") +
  ylim(0, 100)


time_plt <- dat %>% 
  filter(name == "Abreu, Jose") %>%
  ggplot(aes(x = as.factor(yearID), 
             y = obp_z,
             group = 1,
             label = AB,
             label2 = obp)) +
  geom_hline(yintercept = 0,
             size = 1.1,
             linetype = "dashed") +
  geom_line(size = 1.2) +
  geom_point(shape = 21,
             size = 6,
             color = "black",
             fill = "white") +
  ylim(-3, 3) 


ggplotly(time_plt)


dat %>%
  filter(name == "Abreu, Jose") %>%
  select(yearID, AB:hr_rate, ends_with("z_t")) %>%
  gt(rowname_col = "yearID") %>%
  fmt_number(columns = ba:hr_rate,
              decimals = 3) %>%
  cols_label(
    AB = md("**AB**"),
    ba = md("**Batting Avg**"),
    obp = md("**OBP**"),
    slg = md("**SLG**"),
    ops = md("**OPS**"),
    hr_rate = md("**Home Run Rate**")
  ) %>%
  tab_header(title = md("**Jose Abreu**")) %>%
  opt_align_table_header(align = "left") %>%
  tab_options(column_labels.border.top.color = "transparent",
              column_labels.border.top.width = px(3),
              table.border.top.color = "transparent",
              table.border.bottom.color = "transparent") %>%
  cols_align(align = "center") %>%
  cols_hide(columns = ends_with("z_t")) %>%
  tab_style(
    style = cell_fill(color = "palegreen"),
    location = cells_body(
      columns = ba,
      rows = ba_z_t > 60
    )
  )  %>%
  tab_style(
    style = cell_fill(color = "red"),
    location = cells_body(
      columns = ba,
      rows = ba_z_t < 40
    )
  )  %>%
  tab_style(
    style = cell_fill(color = "palegreen"),
    location = cells_body(
      columns = obp,
      rows = obp_z_t > 60
    )
  )  %>%
  tab_style(
    style = cell_fill(color = "red"),
    location = cells_body(
      columns = obp,
      rows = obp_z_t < 40
    )
  )  %>%
  tab_style(
    style = cell_fill(color = "palegreen"),
    location = cells_body(
      columns = slg,
      rows = slg_z_t > 60
    )
  )  %>%
  tab_style(
    style = cell_fill(color = "red"),
    location = cells_body(
      columns = slg,
      rows = slg_z_t < 40
    )
  )  %>%
  tab_style(
    style = cell_fill(color = "palegreen"),
    location = cells_body(
      columns = ops,
      rows = ops_z_t > 60
    )
  )  %>%
  tab_style(
    style = cell_fill(color = "red"),
    location = cells_body(
      columns = ops,
      rows = ops_z_t < 40
    )
  )  %>%
  tab_style(
    style = cell_fill(color = "palegreen"),
    location = cells_body(
      columns = hr_rate,
      rows = hr_rate_z_t > 60
    )
  )  %>%
  tab_style(
    style = cell_fill(color = "red"),
    location = cells_body(
      columns = hr_rate,
      rows = hr_rate_z_t < 40
    )
  ) 


#### Turn the above plots and table into functions to not cloud up the shiny app
## table function
tbl_func <- function(NAME){
  
  dat %>%
  filter(name == NAME) %>%
  select(yearID, AB:hr_rate, ends_with("z_t")) %>%
  gt(rowname_col = "yearID") %>%
  fmt_number(columns = ba:hr_rate,
             decimals = 3) %>%
  cols_label(
    AB = md("**AB**"),
    ba = md("**Batting Avg**"),
    obp = md("**OBP**"),
    slg = md("**SLG**"),
    ops = md("**OPS**"),
    hr_rate = md("**Home Run Rate**")
  ) %>%
  tab_header(title = NAME) %>%
  opt_align_table_header(align = "left") %>%
  tab_options(column_labels.border.top.color = "transparent",
              column_labels.border.top.width = px(3),
              table.border.top.color = "transparent",
              table.border.bottom.color = "transparent") %>%
  cols_align(align = "center") %>%
  cols_hide(columns = ends_with("z_t")) %>%
    tab_style(
      style = cell_fill(color = "palegreen"),
      location = cells_body(
        columns = ba,
        rows = ba_z_t > 60
      )
    )  %>%
    tab_style(
      style = cell_fill(color = "red"),
      location = cells_body(
        columns = ba,
        rows = ba_z_t < 40
      )
    )  %>%
    tab_style(
      style = cell_fill(color = "palegreen"),
      location = cells_body(
        columns = obp,
        rows = obp_z_t > 60
      )
    )  %>%
    tab_style(
      style = cell_fill(color = "red"),
      location = cells_body(
        columns = obp,
        rows = obp_z_t < 40
      )
    )  %>%
    tab_style(
      style = cell_fill(color = "palegreen"),
      location = cells_body(
        columns = slg,
        rows = slg_z_t > 60
      )
    )  %>%
    tab_style(
      style = cell_fill(color = "red"),
      location = cells_body(
        columns = slg,
        rows = slg_z_t < 40
      )
    )  %>%
    tab_style(
      style = cell_fill(color = "palegreen"),
      location = cells_body(
        columns = ops,
        rows = ops_z_t > 60
      )
    )  %>%
    tab_style(
      style = cell_fill(color = "red"),
      location = cells_body(
        columns = ops,
        rows = ops_z_t < 40
      )
    )  %>%
    tab_style(
      style = cell_fill(color = "palegreen"),
      location = cells_body(
        columns = hr_rate,
        rows = hr_rate_z_t > 60
      )
    )  %>%
    tab_style(
      style = cell_fill(color = "red"),
      location = cells_body(
        columns = hr_rate,
        rows = hr_rate_z_t < 40
      )
    ) 
}


## Polar plot function
polar_plt <- function(NAME, STATS){
  
  dat_long %>%
    filter(name == NAME,
           stat %in% STATS) %>%
    ggplot(aes(x = stat, y = value, fill = stat)) +
    geom_col(color = "white", width = 0.75) +
    coord_polar(theta = "x") +
    geom_hline(yintercept = seq(50, 50, by = 1), size = 1.2) +
    labs(x = "", y = "") +
    ylim(0, 100)
  
} 


## time series plot function
time_plt <- function(NAME, STAT){
  
  STAT <- case_when(STAT == "BA" ~ "ba",
                    STAT == "OBP" ~ "obp",
                    STAT == "SLG" ~ "slg",
                    STAT == "OPS" ~ "ops",
                    STAT == "HR Rate" ~ "hr_rate")
  
  stat_z <- paste0(STAT, "_z")
  
  p <- dat %>% 
    filter(name == NAME) %>%
    select(yearID, AB, STAT, stat_z) %>%
    setNames(., c("yearID", "AB", "STAT", "stat_z")) %>%
    ggplot(aes(x = as.factor(yearID), 
               y = stat_z,
               group = 1,
               label = NAME,
               label2 = AB,
               lable3 = STAT)) +
    geom_hline(yintercept = 0,
               size = 1.1,
               linetype = "dashed") +
    geom_line(size = 1.2) +
    geom_point(shape = 21,
               size = 6,
               color = "black",
               fill = "white") +
    ylim(-4, 4) 
  
  
  ggplotly(p)
  
}



## test functions
tbl_func(NAME = sample(x = unique(dat$name), size = 1, replace = FALSE))

polar_plt(NAME = sample(x = unique(dat$name), size = 1, replace = FALSE),
          STATS = c("BA", "OBP", "HR Rate"))

time_plt(NAME = sample(x = unique(dat$name), size = 1, replace = FALSE),
         STAT = "BA")


#### Shiny App ---------------------------------------------------------------

## User Interface
ui <- fluidPage(
  
  titlePanel("MLB Hitters Shiny App\n2017-2019"),
  
  
  sidebarPanel(width = 3,
             selectInput("name",
                             label = "Choose a Player:",
                             choices = unique(dat$name),
                             selected = NULL,
                             multiple = FALSE),
              
              selectInput("stat",
                          label = "Choose stats for polar plot:",
                          choices = unique(dat_long$stat),
                          selected = NULL,
                          multiple = TRUE),
              
              selectInput("time_stat",
                          label = "Choose stat for time series:",
                          choices = unique(dat_long$stat),
                          selected = NULL,
                          multiple = FALSE)
  ),
  
  
  mainPanel(
    
    gt_output(outputId = "tbl"),
    
    fluidRow(
      
      column(6, plotOutput(outputId = "polar")),
      column(6, plotlyOutput(outputId = "time"))
    )
    
  )
)



server <- function(input, output){
  
  ## get player selected for table
  NAME <- reactive({
    
    dat_long %>%
      filter(name == input$name) %>%
      distinct(name, .keep_all = FALSE) %>%
      pull(name)
    })
  
  ## get stats for polar plot
  polar_stats <- reactive({
    
    dat_long %>%
      filter(stat %in% c(input$stat)) %>%
      pull(stat)
    
  })
  
  ## get stat for time series
  ts_stat <- reactive({
    
    dat %>%
      select(ba:hr_rate) %>%
      setNames(., c("BA", "OBP", "SLG", "OPS", "HR Rate")) %>%
      select(input$time_stat) %>% 
      colnames()
    
  })
  
  ## table output
  output$tbl <- render_gt(
      tbl_func(NAME = NAME())
  )
  
  ## polar plot output
  output$polar <- renderPlot(
    
    polar_plt(NAME = NAME(),
              STAT = polar_stats())
    
  )
  
  ## time series plot output
  output$time <- renderPlotly(
    
    time_plt(NAME = NAME(),
             STAT = ts_stat())
    
  )

  
}





shinyApp(ui, server)
