
## Load packages
library(tidyverse)
library(neuralnet)
library(mlbench)

## z-score function
z_score <- function(x){
  z <- (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  return(z)
}

## get data
data("BostonHousing")

## z-score features
d <- BostonHousing %>%
  select(medv, rm, dis, indus) %>%
  mutate(across(.cols = rm:indus,
                ~z_score(.x),
                .names = "{.col}_z"))
  
d %>% 
  head()

## remove first observation for making a prediction on after training
train <- d[-1, ]
test <- d[1, ]

## Simple neural network with one hidden layer
set.seed(9164)
fit_nn <- neuralnet(medv ~ rm_z + dis_z + indus_z,
                    data = train,
                    hidden = 1,
                    err.fct = "sse",
                    linear.output = TRUE)

## plot the neural network
plot(fit_nn)


## Predictions are formed using the weights (black) and biases
# Store the weights and biases from the plot and put them into their own elements
rm_weight <- 1.09872
dis_weight <- -0.05993
indus_weight <- -0.49887

# There is also a bias in the hidden layer
hidden_weight <- 35.95032

bias_1 <- -1.68717
bias_2 <- 14.85824

## Predict on the new observation
test

# Start by applying the weights to their z-scored values, sum them together and add
# in the first bias
input <- bias_1 + test$rm_z * rm_weight + test$dis_z * dis_weight + test$indus_z * indus_weight
input

# transform input -- the neural network is using a sigmoid function
input_sig <- 1/(1+exp(-input))
input_sig

# The transformed input now goes through the hidden layer by multiplying it
# to the hidden weight and then adding in the second bias
prediction <- bias_2 + input_sig * hidden_weight
prediction

## Compare the output to what we would get if we used the predict() function
predict(fit_nn, newdata = test)


# Same result! 
# The neural network basically functioned like 2 linear regression equations stacked together!