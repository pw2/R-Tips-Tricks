
### Excel within column iteration for loop

library(tidyverse)

df <- tibble(
  id = 1:5,
  val = c(5,7,3,4,2)
)

df

## We want to create a new value
# New Value = observed + 2 * lag(new value)
# The first value for the new value is the first observation in row one for value

N <- length(df$val)
new_val <- c(df$val[1], rep(NA, N-1))

for(i in 2:N){

  new_val[i] <- df$val[i] + new_val[i - 1]*2
  
}

df$new_val <- new_val
df


iterate_column_func <- function(df, val){
  
  N <- length(df$val)
  new_val <- c(df$val[1], rep(NA, N-1))
  
  for(i in 2:N){
    
    new_val[i] <- df$val[i] + new_val[i - 1]*2
    
  }
  
  df$new_val <- new_val
  return(df)
}

iterate_column_func(df, val)


#### What if the data has multiple subjects?

df_2 <- tibble(
  subject = as.factor(rep(1:10, each = 5)),
  id = rep(1:5, times = 10),
  val = round(runif(n = 50, min = 10, max = 20), 0)
)

df_2


iterate_column_func <- function(x){
  
  N <- length(x)
  new_val <- c(x[1], rep(NA, N-1))
  
  for(i in 2:N){
    
    new_val[i] <- x[i] + new_val[i - 1]*2
    
  }
  
  new_val <- as.data.frame(new_val)
  return(new_val)
}

new_df <- df_2 %>%
  group_by(subject) %>% 
  group_modify(~iterate_column_func(.x$val)) %>%
  ungroup()

df_2 %>%
  bind_cols(new_df %>% select(-subject)) %>% as.data.frame()
