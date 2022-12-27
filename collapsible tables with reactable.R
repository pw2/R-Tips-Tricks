
## Collapsible Tables with {reactable}

### Packages ---------------------------------------------------
library(tidyverse)
library(Lahman)
library(reactable)

## z-score custom function
z_score <- function(x){
  z <- (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  return(z)
}

### Data Preparation -------------------------------------------
df <- Batting %>%
  filter(yearID == 2019,
         AB >= 250) %>%
  left_join(People) %>%
  left_join(Fielding %>% select(playerID, yearID, POS, PO, E)) %>%
  mutate(name = paste(nameLast, nameFirst, sep = ", ")) %>%
  mutate(obp = (H + BB + HBP) / (AB + BB + HBP + SF),
         batting_avg = H / AB,
         pct_g = G / 162) %>%
  select(name, lgID, teamID, POS, G, pct_g, AB, batting_avg, obp, H, HR, PO, E) %>%
  mutate(across(.cols = batting_avg:E,
                ~z_score(.x),
                .names = "{.col}_z"),
         E_z = E_z * -1)

df %>%
  head()

### Collapsible Table -----------------------------------------------------------
df %>%
  reactable(
    
    # set the collapsible sections
    groupBy = c("lgID", "teamID"),
    
    # Set up the style for each column
    columns = list(
      
      name = colDef(name = "Player"),
      
      teamID = colDef(name = "Team"),

      
      POS = colDef(name = "Position"),
      
      G  = colDef(name = "Games"),
      
      pct_g  = colDef(name = "% Games Played",
                      format = colFormat(digits = 1,
                                         suffix = "%")),
      
      AB  = colDef(name = "AB"),
      
      batting_avg = colDef(name = "Batting Avg",
                           format = colFormat(digits = 3),
                           style = function(value, id){
                             if(df$batting_avg_z[id] >= 1){
                               color <- "#CCFF99"} else {
                                 if(df$batting_avg_z[id] <= -1){
                                   color <- "#FFCCCC"} else {
                                 color <- "white"
                               }
                               }
                             list(background = color)
                           }
                           ),
      
      obp = colDef(name = "OBP",
                   format = colFormat(digits = 3),
                   style = function(value, id){
                     if(df$obp_z[id] >= 1){
                       color <- "#CCFF99"} else {
                         if(df$obp_z[id] <= -1){
                           color <- "#FFCCCC"} else {
                             color <- "white"
                           }
                       }
                     list(background = color)
                   }),
      
      H = colDef(name = "Hits",
                 style = function(value, id){
                   if(df$H_z[id] >= 1.5){
                     color <- "#CCFF99"} else {
                       if(df$H_z[id] <= -1.5){
                         color <- "#FFCCCC"} else {
                           color <- "white"
                         }
                     }
                   list(background = color)
                 }),
      
      HR = colDef(name = "HR",
                  style = function(value, id){
                    if(df$HR_z[id] >= 1.5){
                      color <- "#CCFF99"} else {
                        if(df$HR_z[id] <= -1.5){
                          color <- "#FFCCCC"} else {
                            color <- "white"
                          }
                      }
                    list(background = color)
                  }),
      
      PO  = colDef(name = "Put Outs"),

      E  = colDef(name = "Fielding Errors"),
      
      batting_avg_z = colDef(show = FALSE),
      obp_z = colDef(show = FALSE),
      H_z = colDef(show = FALSE),
      HR_z = colDef(show = FALSE),
      PO_z = colDef(show = FALSE),
      E_z = colDef(show = FALSE)
    ),
    defaultColDef = colDef(
      headerStyle = list(background = "#CCCCCC", color = "black")),
    bordered = TRUE,
    highlight = TRUE,
    showSortIcon = TRUE,
    filterable = TRUE
    )
