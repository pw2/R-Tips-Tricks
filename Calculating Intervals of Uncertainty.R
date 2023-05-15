
## Load packages
library(tidyverse)

theme_set(theme_classic())

## Get data
d <- mtcars
d %>%
  head()

## fit model
fit_lm <- lm(mpg ~ wt + carb, data = d)
summary(fit_lm)

## Get a few rows to make predictions on
set.seed(1234)
d_sample <- d %>%
  sample_n(size = 5) %>%
  select(mpg, wt, carb)

d_sample

## 95% Confidence Intervals
d_sample %>%
  bind_cols(
    predict(fit_lm, newdata = d_sample, interval = "confidence", level = 0.95)
  )

## Calculate the 95% confidence interval by hand
level <- 0.95
alpha <- 1 - (1 - level) / 2
t_crit <- qt(p = alpha, df = fit_lm$df.residual)

d_sample %>%
  mutate(pred = predict(fit_lm, newdata = .),
         se_pred = predict(fit_lm, newdata = ., se = TRUE)$se.fit,
         cl95 = t_crit * se_pred,
         lwr = pred - cl95,
         upr = pred + cl95)

## 95% Confidence Intervals with z-critical value
d_sample %>%
  mutate(pred = predict(fit_lm, newdata = .),
         se_pred = predict(fit_lm, newdata = ., se = TRUE)$se.fit,
         lwr = pred + qnorm(p = 0.025, mean = 0, sd = 1) * se_pred,
         upr = pred + qnorm(p = 0.975, mean = 0, sd = 1) * se_pred)

## 95% Quantile Intervals via Simulation
d_sample %>%
  mutate(pred = predict(fit_lm, newdata = .),
         se_pred = predict(fit_lm, newdata = ., se = TRUE)$se.fit) %>%
  rowwise() %>%
  mutate(lwr = quantile(rnorm(n = 1000, mean = pred, sd = se_pred), probs = 0.025),
         upr = quantile(rnorm(n = 1000, mean = pred, sd = se_pred), probs = 0.975))

## 95% quantile intervals via Simulation with full distribution
N <- 1000
pred_sim <- list()

set.seed(8945)
for(i in 1:nrow(d_sample)){
  
  pred <- predict(fit_lm, newdata = d_sample[i, ])
  se_pred <- predict(fit_lm, newdata = d_sample[i, ], se = TRUE)$se.fit
  
  pred_sim[[i]] <- rnorm(n = N, mean = pred, sd = se_pred)
  
}

sim_df <- tibble(
  sample_row = rep(1:5, each = N),
  pred_sim = unlist(pred_sim)
  )

sim_df %>%
  head()

# get predictions and quantile intervals
sim_df %>%
  group_by(sample_row) %>%
  summarize(pred = mean(pred_sim),
         lwr = quantile(pred_sim, probs = 0.025),
         upr = quantile(pred_sim, probs = 0.975)) %>%
  mutate(sample_row = rownames(d_sample))

# plot the predicted distributions
sim_df %>%
  mutate(actual_value = rep(d_sample$mpg, each = N),
         sample_row = case_when(sample_row == 1 ~ "Hornet 4 Drive",
                                sample_row == 2 ~ "Toyota Corolla",
                                sample_row == 3 ~ "Honda Civic",
                                sample_row == 4 ~ "Ferrari Dino",
                                sample_row == 5 ~ "Pontiac Firebird")) %>%
  ggplot(aes(x = pred_sim)) +
  geom_histogram(color = "white",
                 fill = "light grey") +
  geom_vline(aes(xintercept = actual_value),
             color = "red",
             size = 1.2,
             linetype = "dashed") +
  facet_wrap(~sample_row, scale = "free_x") +
  labs(x = "Predicted Simulation",
       y = "count",
       title = "Predicted Simulation with actual observation (red line)",
       subtitle = "Note that the x-axis are specific to that simulation and not the same")


## 95% Prediction Intervals
d_sample %>%
  bind_cols(
    predict(fit_lm, newdata = d_sample, interval = "predict", level = 0.95)
  )


## 95% prediction intervals from a simulated distribution 
# store the model residual standard error
sigma <- summary(fit_lm)$sigma

# run simulation
N <- 1000
pred_sim2 <- list()

set.seed(85)
for(i in 1:nrow(d_sample)){
  
  pred <- predict(fit_lm, newdata = d_sample[i, ])
  se_pred <- predict(fit_lm, newdata = d_sample[i, ], se = TRUE)$se.fit
  
  pred_sim2[[i]] <- rnorm(n = N, mean = pred, sd = se_pred) + rnorm(n = N, mean = 0, sd = sigma)
  
}

# put results in a data frame
sim_df2 <- tibble(
  sample_row = rep(1:5, each = N),
  pred_sim2 = unlist(pred_sim2)
)

sim_df2 %>%
  head()

# get predictions and intervals
sim_df2 %>%
  group_by(sample_row) %>%
  summarize(pred = mean(pred_sim2),
            lwr = quantile(pred_sim2, probs = 0.025),
            upr = quantile(pred_sim2, probs = 0.975)) %>%
  mutate(sample_row = rownames(d_sample))

# plot the predicted distributions
sim_df2 %>%
  mutate(actual_value = rep(d_sample$mpg, each = N),
         sample_row = case_when(sample_row == 1 ~ "Hornet 4 Drive",
                                sample_row == 2 ~ "Toyota Corolla",
                                sample_row == 3 ~ "Honda Civic",
                                sample_row == 4 ~ "Ferrari Dino",
                                sample_row == 5 ~ "Pontiac Firebird")) %>%
  ggplot(aes(x = pred_sim2)) +
  geom_histogram(color = "white",
                 fill = "light grey") +
  geom_vline(aes(xintercept = actual_value),
             color = "red",
             size = 1.2,
             linetype = "dashed")  +
  facet_wrap(~sample_row, scale = "free_x") +
  labs(x = "Predicted Simulation",
       y = "count",
       title = "Predicted Simulation with actual observation (red line)",
       subtitle = "Note that the x-axis are specific to that simulation and not the same")
