
### load packages
library(tidymodels)
library(tidyverse)

############ Regression Example ############
### get data
df <- mtcars
head(df)

### cross validation folds
df_cv <- vfold_cv(df, v = 10)
df_cv

### specify linear model
lm_spec <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

### recipe
mpg_rec <- recipe(mpg ~ cyl + disp + wt, data = df)
mpg_rec

### workflow
mpg_wf <- workflow() %>%
  add_recipe(mpg_rec) %>%
  add_model(lm_spec)

### set a control function to save the predictions from the model fit to the CV-folds
ctrl <- control_resamples(save_pred = TRUE)

### fit model
mpg_lm <- mpg_wf %>%
  fit_resamples(
    resamples = df_cv, 
    control = ctrl
  )

mpg_lm

### view model metrics
collect_metrics(mpg_lm)

### get predictions
mpg_lm %>%
  unnest(cols = .predictions) %>%
  select(.pred, mpg)

## Fit the final model & extract the workflow
mpg_final <- mpg_wf %>% 
  fit(df) %>%
  extract_fit_parsnip()

mpg_final

## Save model to use later
# save(mpg_final, file = "mpg_final.rda")

############ Classification Example ############
### get data
df <- mtcars
head(df)
table(df$cyl)

df$cyl <- as.factor(df$cyl)

### cross validation folds
df_cv <- vfold_cv(df, v = 10)
df_cv

### specify linear model
rf_spec <-rand_forest() %>%
  set_engine("randomForest", importance = TRUE) %>%
  set_mode("classification")

### recipe
cyl_rec <- recipe(cyl ~ mpg + disp + wt, data = df)
cyl_rec

### workflow
cyl_wf <- workflow() %>%
  add_recipe(cyl_rec) %>%
  add_model(rf_spec)

### set a control function to save the predictions from the model fit to the CV-folds
ctrl <- control_resamples(save_pred = TRUE)

### fit model
cyl_rf <- cyl_wf %>%
  fit_resamples(
    resamples = df_cv, 
    control = ctrl
  )

cyl_rf

### view model metrics
collect_metrics(cyl_rf)

### get predictions
pred_group <- cyl_rf %>%
  unnest(cols = .predictions) %>%
  select(.pred_4, .pred_6, .pred_8, .pred_class, cyl) 

pred_group

table('predicted' = pred_group$.pred_class, 'observed' = pred_group$cyl)

## Fit the final model & extract the workflow
cyl_final <- cyl_wf  %>% 
  fit(df) %>%
  extract_fit_parsnip()

cyl_final

## Save model to use later
# save(cyl_final, file = "cyl_final.rda")
