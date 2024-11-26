install.packages("glmnet")
install.packages("readxl")
install.packages("keras")
install.packages("GA")
library(GA)
library(readxl)
library(dplyr)
library(tidyr)
library(glmnet)
library(forecast)
library(keras)
# Ultra-short term forecasting using winter data (plant 1)
#LSTM model
data <- read_excel("/Users/machang/Desktop/2023/new/plant1.xlsx")
data[] <- lapply(data, as.numeric)
train_data <- data[1:1157, ] 
test_data <- data[1158:1170, ]
mean_value1 <- mean(train_data$`total radiation`)
sd_value1 <- sd(train_data$`total radiation`)
z_total <- (train_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(train_data$`direct solar radiation`)
sd_value2 <- sd(train_data$`direct solar radiation`)
z_direct <- (train_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(train_data$power)
sd_value3 <- sd(train_data$power)
z_power <- (train_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
fitness <- function(params) {
  tryCatch({
    nodes <- as.integer(params[1])  
    learning_rate <- params[2]
    model <- keras_model_sequential()
    model %>%
      layer_lstm(units = nodes, input_shape = c(1, 3)) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(lr = learning_rate)
    )
    history <- model %>% fit(
      x = input_data,
      y = z_power,
      epochs = 10,
      batch_size = 32,
      verbose = 0
    )
    train_loss <- history$metrics$loss
    return(train_loss[length(train_loss)])
  }, error = function(e) {
    message("Error in fitness function: ", conditionMessage(e))
    return(NA)  
  })
}
set.seed(123) 
lower_bounds <- c(32, 0.001)
upper_bounds <- c(128, 0.1)
bounds <- matrix(c(lower_bounds, upper_bounds), nrow = 2, byrow = TRUE)
ga_result <- ga(type = "real-valued",
                fitness = fitness,
                lower = lower_bounds,  
                upper = upper_bounds, 
                popSize = 50,    
                maxiter = 20)     
best_fitness<-min(ga_result@fitness)
best_index <- which.min(ga_result@fitness)  
best_params <- ga_result@solution[1, ]
cat("Best Parameters (corresponding to the minimum fitness value): \n")
cat("Nodes:", best_params[1], ", Learning Rate:", best_params[2], "\n")
cat("Minimum Fitness Value:", best_fitness, "\n")
best_nodes <- best_params[1]
best_learning_rate <- best_params[2]
best_model <- keras_model_sequential()
best_model %>%
  layer_lstm(units = best_nodes, input_shape = c(1, 3)) %>%
  layer_dense(units = 1)
best_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = best_learning_rate)
)
history <- best_model %>% fit(
  x = input_data,
  y = z_power,
  epochs = 10,
  batch_size = 32,
  verbose = 1
)
mean_value1 <- mean(test_data$`total radiation`)
sd_value1 <- sd(test_data$`total radiation`)
z_total <- (test_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(test_data$`direct solar radiation`)
sd_value2 <- sd(test_data$`direct solar radiation`)
z_direct <- (test_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(test_data$power)
sd_value3 <- sd(test_data$power)
z_power <- (test_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
validation_pred <- best_model %>% predict(input_data)
print(validation_pred)
mean_power <- mean(test_data$power, na.rm = TRUE)
print(mean_power)
sd_power <- sd(test_data$power, na.rm = TRUE)
print(sd_power)
denormalized_forecast <- validation_pred * sd_power+ mean_power
length(denormalized_forecast)
denormalized_forecast <- pmax(denormalized_forecast, 0)
print(denormalized_forecast)
RMSE <- sqrt(mean((denormalized_forecast- test_data$power)^2))
print(RMSE)


# Ultra-short term forecasting using spring data (plant 1)
#LSTM model
data <- read_excel("/Users/machang/Desktop/2023/new/plant1.xlsx")
data[] <- lapply(data, as.numeric)
train_data <- data[1171:2353, ] 
test_data <- data[2354:2366, ]

mean_value1 <- mean(train_data$`total radiation`)
sd_value1 <- sd(train_data$`total radiation`)
z_total <- (train_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(train_data$`direct solar radiation`)
sd_value2 <- sd(train_data$`direct solar radiation`)
z_direct <- (train_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(train_data$power)
sd_value3 <- sd(train_data$power)
z_power <- (train_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
fitness <- function(params) {
  tryCatch({
    nodes <- as.integer(params[1])  
    learning_rate <- params[2]
    model <- keras_model_sequential()
    model %>%
      layer_lstm(units = nodes, input_shape = c(1, 3)) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(lr = learning_rate)
    )
    history <- model %>% fit(
      x = input_data,
      y = z_power,
      epochs = 10,
      batch_size = 32,
      verbose = 0
    )
    train_loss <- history$metrics$loss
    return(train_loss[length(train_loss)])
  }, error = function(e) {
    message("Error in fitness function: ", conditionMessage(e))
    return(NA)  
  })
}
set.seed(123) 
lower_bounds <- c(32, 0.001)
upper_bounds <- c(128, 0.1)
bounds <- matrix(c(lower_bounds, upper_bounds), nrow = 2, byrow = TRUE)
ga_result <- ga(type = "real-valued",
                fitness = fitness,
                lower = lower_bounds,  
                upper = upper_bounds, 
                popSize = 50,    
                maxiter = 20)     
best_fitness<-min(ga_result@fitness)
best_index <- which.min(ga_result@fitness)  
best_params <- ga_result@solution[1, ]
cat("Best Parameters (corresponding to the minimum fitness value): \n")
cat("Nodes:", best_params[1], ", Learning Rate:", best_params[2], "\n")
cat("Minimum Fitness Value:", best_fitness, "\n")
best_nodes <- best_params[1]
best_learning_rate <- best_params[2]
best_model <- keras_model_sequential()
best_model %>%
  layer_lstm(units = best_nodes, input_shape = c(1, 3)) %>%
  layer_dense(units = 1)
best_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = best_learning_rate)
)
history <- best_model %>% fit(
  x = input_data,
  y = z_power,
  epochs = 10,
  batch_size = 32,
  verbose = 1
)
mean_value1 <- mean(test_data$`total radiation`)
sd_value1 <- sd(test_data$`total radiation`)
z_total <- (test_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(test_data$`direct solar radiation`)
sd_value2 <- sd(test_data$`direct solar radiation`)
z_direct <- (test_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(test_data$power)
sd_value3 <- sd(test_data$power)
z_power <- (test_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
validation_pred <- best_model %>% predict(input_data)
print(validation_pred)
mean_power <- mean(test_data$power, na.rm = TRUE)
print(mean_power)
sd_power <- sd(test_data$power, na.rm = TRUE)
print(sd_power)
denormalized_forecast <- validation_pred * sd_power+ mean_power
length(denormalized_forecast)
denormalized_forecast <- pmax(denormalized_forecast, 0)
print(denormalized_forecast)
RMSE <- sqrt(mean((denormalized_forecast- test_data$power)^2))
print(RMSE)


# Ultra-short term forecasting using summer data (plant 1)
#LSTM model
data <- read_excel("/Users/machang/Desktop/2023/new/plant1.xlsx")
data[] <- lapply(data, as.numeric)
train_data <- data[2367:3549, ] 
test_data <- data[3550:3562, ]
mean_value1 <- mean(train_data$`total radiation`)
sd_value1 <- sd(train_data$`total radiation`)
z_total <- (train_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(train_data$`direct solar radiation`)
sd_value2 <- sd(train_data$`direct solar radiation`)
z_direct <- (train_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(train_data$power)
sd_value3 <- sd(train_data$power)
z_power <- (train_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
fitness <- function(params) {
  tryCatch({
    nodes <- as.integer(params[1])  
    learning_rate <- params[2]
    model <- keras_model_sequential()
    model %>%
      layer_lstm(units = nodes, input_shape = c(1, 3)) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(lr = learning_rate)
    )
    history <- model %>% fit(
      x = input_data,
      y = z_power,
      epochs = 10,
      batch_size = 32,
      verbose = 0
    )
    train_loss <- history$metrics$loss
    return(train_loss[length(train_loss)])
  }, error = function(e) {
    message("Error in fitness function: ", conditionMessage(e))
    return(NA)  
  })
}
set.seed(123) 
lower_bounds <- c(32, 0.001)
upper_bounds <- c(128, 0.1)
bounds <- matrix(c(lower_bounds, upper_bounds), nrow = 2, byrow = TRUE)
ga_result <- ga(type = "real-valued",
                fitness = fitness,
                lower = lower_bounds,  
                upper = upper_bounds, 
                popSize = 50,    
                maxiter = 20)     
best_fitness<-min(ga_result@fitness)
best_index <- which.min(ga_result@fitness)  
best_params <- ga_result@solution[1, ]
cat("Best Parameters (corresponding to the minimum fitness value): \n")
cat("Nodes:", best_params[1], ", Learning Rate:", best_params[2], "\n")
cat("Minimum Fitness Value:", best_fitness, "\n")
best_nodes <- best_params[1]
best_learning_rate <- best_params[2]
best_model <- keras_model_sequential()
best_model %>%
  layer_lstm(units = best_nodes, input_shape = c(1, 3)) %>%
  layer_dense(units = 1)
best_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = best_learning_rate)
)
history <- best_model %>% fit(
  x = input_data,
  y = z_power,
  epochs = 10,
  batch_size = 32,
  verbose = 1
)
mean_value1 <- mean(test_data$`total radiation`)
sd_value1 <- sd(test_data$`total radiation`)
z_total <- (test_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(test_data$`direct solar radiation`)
sd_value2 <- sd(test_data$`direct solar radiation`)
z_direct <- (test_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(test_data$power)
sd_value3 <- sd(test_data$power)
z_power <- (test_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
validation_pred <- best_model %>% predict(input_data)
print(validation_pred)
mean_power <- mean(test_data$power, na.rm = TRUE)
print(mean_power)
sd_power <- sd(test_data$power, na.rm = TRUE)
print(sd_power)
denormalized_forecast <- validation_pred * sd_power+ mean_power
length(denormalized_forecast)
denormalized_forecast <- pmax(denormalized_forecast, 0)
print(denormalized_forecast)
RMSE <- sqrt(mean((denormalized_forecast- test_data$power)^2))
print(RMSE)





# Ultra-short term forecasting using autumn data (plant 1)
#LSTM model
data <- read_excel("/Users/machang/Desktop/2023/new/plant1.xlsx")
data[] <- lapply(data, as.numeric)
train_data <- data[3563:4732, ] 
test_data <- data[4733:4745, ]
mean_value1 <- mean(train_data$`total radiation`)
sd_value1 <- sd(train_data$`total radiation`)
z_total <- (train_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(train_data$`direct solar radiation`)
sd_value2 <- sd(train_data$`direct solar radiation`)
z_direct <- (train_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(train_data$power)
sd_value3 <- sd(train_data$power)
z_power <- (train_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
fitness <- function(params) {
  tryCatch({
    nodes <- as.integer(params[1])  
    learning_rate <- params[2]
    model <- keras_model_sequential()
    model %>%
      layer_lstm(units = nodes, input_shape = c(1, 3)) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(lr = learning_rate)
    )
    history <- model %>% fit(
      x = input_data,
      y = z_power,
      epochs = 10,
      batch_size = 32,
      verbose = 0
    )
    train_loss <- history$metrics$loss
    return(train_loss[length(train_loss)])
  }, error = function(e) {
    message("Error in fitness function: ", conditionMessage(e))
    return(NA)  
  })
}
set.seed(123) 
lower_bounds <- c(32, 0.001)
upper_bounds <- c(128, 0.1)
bounds <- matrix(c(lower_bounds, upper_bounds), nrow = 2, byrow = TRUE)
ga_result <- ga(type = "real-valued",
                fitness = fitness,
                lower = lower_bounds,  
                upper = upper_bounds, 
                popSize = 50,    
                maxiter = 20)     
best_fitness<-min(ga_result@fitness)
best_index <- which.min(ga_result@fitness)  
best_params <- ga_result@solution[1, ]
cat("Best Parameters (corresponding to the minimum fitness value): \n")
cat("Nodes:", best_params[1], ", Learning Rate:", best_params[2], "\n")
cat("Minimum Fitness Value:", best_fitness, "\n")
best_nodes <- best_params[1]
best_learning_rate <- best_params[2]
best_model <- keras_model_sequential()
best_model %>%
  layer_lstm(units = best_nodes, input_shape = c(1, 3)) %>%
  layer_dense(units = 1)
best_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = best_learning_rate)
)
history <- best_model %>% fit(
  x = input_data,
  y = z_power,
  epochs = 10,
  batch_size = 32,
  verbose = 1
)
mean_value1 <- mean(test_data$`total radiation`)
sd_value1 <- sd(test_data$`total radiation`)
z_total <- (test_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(test_data$`direct solar radiation`)
sd_value2 <- sd(test_data$`direct solar radiation`)
z_direct <- (test_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(test_data$power)
sd_value3 <- sd(test_data$power)
z_power <- (test_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
validation_pred <- best_model %>% predict(input_data)
print(validation_pred)
mean_power <- mean(test_data$power, na.rm = TRUE)
print(mean_power)
sd_power <- sd(test_data$power, na.rm = TRUE)
print(sd_power)
denormalized_forecast <- validation_pred * sd_power+ mean_power
length(denormalized_forecast)
denormalized_forecast <- pmax(denormalized_forecast, 0)
print(denormalized_forecast)
RMSE <- sqrt(mean((denormalized_forecast- test_data$power)^2))
print(RMSE)



# Ultra-short term forecasting using winter data (plant 2)
#LSTM model
data <- read_excel("/Users/machang/Desktop/2023/new/plant2.xlsx")
data[] <- lapply(data, as.numeric)
train_data <- data[1:1157, ] 
test_data <- data[1158:1170, ]
mean_value1 <- mean(train_data$`total radiation`)
sd_value1 <- sd(train_data$`total radiation`)
z_total <- (train_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(train_data$`direct solar radiation`)
sd_value2 <- sd(train_data$`direct solar radiation`)
z_direct <- (train_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(train_data$power)
sd_value3 <- sd(train_data$power)
z_power <- (train_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
fitness <- function(params) {
  tryCatch({
    nodes <- as.integer(params[1])  
    learning_rate <- params[2]
    model <- keras_model_sequential()
    model %>%
      layer_lstm(units = nodes, input_shape = c(1, 3)) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(lr = learning_rate)
    )
    history <- model %>% fit(
      x = input_data,
      y = z_power,
      epochs = 10,
      batch_size = 32,
      verbose = 0
    )
    train_loss <- history$metrics$loss
    return(train_loss[length(train_loss)])
  }, error = function(e) {
    message("Error in fitness function: ", conditionMessage(e))
    return(NA)  
  })
}
set.seed(123) 
lower_bounds <- c(32, 0.001)
upper_bounds <- c(128, 0.1)
bounds <- matrix(c(lower_bounds, upper_bounds), nrow = 2, byrow = TRUE)
ga_result <- ga(type = "real-valued",
                fitness = fitness,
                lower = lower_bounds,  
                upper = upper_bounds, 
                popSize = 50,    
                maxiter = 20)     
best_fitness<-min(ga_result@fitness)
best_index <- which.min(ga_result@fitness)  
best_params <- ga_result@solution[1, ]
cat("Best Parameters (corresponding to the minimum fitness value): \n")
cat("Nodes:", best_params[1], ", Learning Rate:", best_params[2], "\n")
cat("Minimum Fitness Value:", best_fitness, "\n")
best_nodes <- best_params[1]
best_learning_rate <- best_params[2]
best_model <- keras_model_sequential()
best_model %>%
  layer_lstm(units = best_nodes, input_shape = c(1, 3)) %>%
  layer_dense(units = 1)
best_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = best_learning_rate)
)
history <- best_model %>% fit(
  x = input_data,
  y = z_power,
  epochs = 10,
  batch_size = 32,
  verbose = 1
)
mean_value1 <- mean(test_data$`total radiation`)
sd_value1 <- sd(test_data$`total radiation`)
z_total <- (test_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(test_data$`direct solar radiation`)
sd_value2 <- sd(test_data$`direct solar radiation`)
z_direct <- (test_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(test_data$power)
sd_value3 <- sd(test_data$power)
z_power <- (test_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
validation_pred <- best_model %>% predict(input_data)
print(validation_pred)
mean_power <- mean(test_data$power, na.rm = TRUE)
print(mean_power)
sd_power <- sd(test_data$power, na.rm = TRUE)
print(sd_power)
denormalized_forecast <- validation_pred * sd_power+ mean_power
length(denormalized_forecast)
denormalized_forecast <- pmax(denormalized_forecast, 0)
print(denormalized_forecast)
RMSE <- sqrt(mean((denormalized_forecast- test_data$power)^2))
print(RMSE)


# Ultra-short term forecasting using spring data (plant 2)
#LSTM model
data <- read_excel("/Users/machang/Desktop/2023/new/plant2.xlsx")
data[] <- lapply(data, as.numeric)
train_data <- data[1171:2353, ] 
test_data <- data[2354:2366, ]
mean_value1 <- mean(train_data$`total radiation`)
sd_value1 <- sd(train_data$`total radiation`)
z_total <- (train_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(train_data$`direct solar radiation`)
sd_value2 <- sd(train_data$`direct solar radiation`)
z_direct <- (train_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(train_data$power)
sd_value3 <- sd(train_data$power)
z_power <- (train_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
fitness <- function(params) {
  tryCatch({
    nodes <- as.integer(params[1])  
    learning_rate <- params[2]
    model <- keras_model_sequential()
    model %>%
      layer_lstm(units = nodes, input_shape = c(1, 3)) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(lr = learning_rate)
    )
    history <- model %>% fit(
      x = input_data,
      y = z_power,
      epochs = 10,
      batch_size = 32,
      verbose = 0
    )
    train_loss <- history$metrics$loss
    return(train_loss[length(train_loss)])
  }, error = function(e) {
    message("Error in fitness function: ", conditionMessage(e))
    return(NA)  
  })
}
set.seed(123) 
lower_bounds <- c(32, 0.001)
upper_bounds <- c(128, 0.1)
bounds <- matrix(c(lower_bounds, upper_bounds), nrow = 2, byrow = TRUE)
ga_result <- ga(type = "real-valued",
                fitness = fitness,
                lower = lower_bounds,  
                upper = upper_bounds, 
                popSize = 50,    
                maxiter = 20)     
best_fitness<-min(ga_result@fitness)
best_index <- which.min(ga_result@fitness)  
best_params <- ga_result@solution[1, ]
cat("Best Parameters (corresponding to the minimum fitness value): \n")
cat("Nodes:", best_params[1], ", Learning Rate:", best_params[2], "\n")
cat("Minimum Fitness Value:", best_fitness, "\n")
best_nodes <- best_params[1]
best_learning_rate <- best_params[2]
best_model <- keras_model_sequential()
best_model %>%
  layer_lstm(units = best_nodes, input_shape = c(1, 3)) %>%
  layer_dense(units = 1)
best_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = best_learning_rate)
)
history <- best_model %>% fit(
  x = input_data,
  y = z_power,
  epochs = 10,
  batch_size = 32,
  verbose = 1
)
mean_value1 <- mean(test_data$`total radiation`)
sd_value1 <- sd(test_data$`total radiation`)
z_total <- (test_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(test_data$`direct solar radiation`)
sd_value2 <- sd(test_data$`direct solar radiation`)
z_direct <- (test_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(test_data$power)
sd_value3 <- sd(test_data$power)
z_power <- (test_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
validation_pred <- best_model %>% predict(input_data)
print(validation_pred)
mean_power <- mean(test_data$power, na.rm = TRUE)
print(mean_power)
sd_power <- sd(test_data$power, na.rm = TRUE)
print(sd_power)
denormalized_forecast <- validation_pred * sd_power+ mean_power
length(denormalized_forecast)
denormalized_forecast <- pmax(denormalized_forecast, 0)
print(denormalized_forecast)
RMSE <- sqrt(mean((denormalized_forecast- test_data$power)^2))
print(RMSE)



# Ultra-short term forecasting using summer data (plant 2)
#LSTM model
data <- read_excel("/Users/machang/Desktop/2023/new/plant2.xlsx")
data[] <- lapply(data, as.numeric)
train_data <- data[2367:3549, ] 
test_data <- data[3550:3562, ]
mean_value1 <- mean(train_data$`total radiation`)
sd_value1 <- sd(train_data$`total radiation`)
z_total <- (train_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(train_data$`direct solar radiation`)
sd_value2 <- sd(train_data$`direct solar radiation`)
z_direct <- (train_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(train_data$power)
sd_value3 <- sd(train_data$power)
z_power <- (train_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
fitness <- function(params) {
  tryCatch({
    nodes <- as.integer(params[1])  
    learning_rate <- params[2]
    model <- keras_model_sequential()
    model %>%
      layer_lstm(units = nodes, input_shape = c(1, 3)) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(lr = learning_rate)
    )
    history <- model %>% fit(
      x = input_data,
      y = z_power,
      epochs = 10,
      batch_size = 32,
      verbose = 0
    )
    train_loss <- history$metrics$loss
    return(train_loss[length(train_loss)])
  }, error = function(e) {
    message("Error in fitness function: ", conditionMessage(e))
    return(NA)  
  })
}
set.seed(123) 
lower_bounds <- c(32, 0.001)
upper_bounds <- c(128, 0.1)
bounds <- matrix(c(lower_bounds, upper_bounds), nrow = 2, byrow = TRUE)
ga_result <- ga(type = "real-valued",
                fitness = fitness,
                lower = lower_bounds,  
                upper = upper_bounds, 
                popSize = 50,    
                maxiter = 20)     
best_fitness<-min(ga_result@fitness)
best_index <- which.min(ga_result@fitness)  
best_params <- ga_result@solution[1, ]
cat("Best Parameters (corresponding to the minimum fitness value): \n")
cat("Nodes:", best_params[1], ", Learning Rate:", best_params[2], "\n")
cat("Minimum Fitness Value:", best_fitness, "\n")
best_nodes <- best_params[1]
best_learning_rate <- best_params[2]
best_model <- keras_model_sequential()
best_model %>%
  layer_lstm(units = best_nodes, input_shape = c(1, 3)) %>%
  layer_dense(units = 1)
best_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = best_learning_rate)
)
history <- best_model %>% fit(
  x = input_data,
  y = z_power,
  epochs = 10,
  batch_size = 32,
  verbose = 1
)
mean_value1 <- mean(test_data$`total radiation`)
sd_value1 <- sd(test_data$`total radiation`)
z_total <- (test_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(test_data$`direct solar radiation`)
sd_value2 <- sd(test_data$`direct solar radiation`)
z_direct <- (test_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(test_data$power)
sd_value3 <- sd(test_data$power)
z_power <- (test_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
validation_pred <- best_model %>% predict(input_data)
print(validation_pred)
mean_power <- mean(test_data$power, na.rm = TRUE)
print(mean_power)
sd_power <- sd(test_data$power, na.rm = TRUE)
print(sd_power)
denormalized_forecast <- validation_pred * sd_power+ mean_power
length(denormalized_forecast)
denormalized_forecast <- pmax(denormalized_forecast, 0)
print(denormalized_forecast)
RMSE <- sqrt(mean((denormalized_forecast- test_data$power)^2))
print(RMSE)





# Ultra-short term forecasting using autumn data (plant 2)
#LSTM model
data <- read_excel("/Users/machang/Desktop/2023/new/plant2.xlsx")
data[] <- lapply(data, as.numeric)
train_data <- data[3563:4732, ] 
test_data <- data[4733:4745, ]
mean_value1 <- mean(train_data$`total radiation`)
sd_value1 <- sd(train_data$`total radiation`)
z_total <- (train_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(train_data$`direct solar radiation`)
sd_value2 <- sd(train_data$`direct solar radiation`)
z_direct <- (train_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(train_data$power)
sd_value3 <- sd(train_data$power)
z_power <- (train_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
fitness <- function(params) {
  tryCatch({
    nodes <- as.integer(params[1])  
    learning_rate <- params[2]
    model <- keras_model_sequential()
    model %>%
      layer_lstm(units = nodes, input_shape = c(1, 3)) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(lr = learning_rate)
    )
    history <- model %>% fit(
      x = input_data,
      y = z_power,
      epochs = 10,
      batch_size = 32,
      verbose = 0
    )
    train_loss <- history$metrics$loss
    return(train_loss[length(train_loss)])
  }, error = function(e) {
    message("Error in fitness function: ", conditionMessage(e))
    return(NA)  
  })
}
set.seed(123) 
lower_bounds <- c(32, 0.001)
upper_bounds <- c(128, 0.1)
bounds <- matrix(c(lower_bounds, upper_bounds), nrow = 2, byrow = TRUE)
ga_result <- ga(type = "real-valued",
                fitness = fitness,
                lower = lower_bounds,  
                upper = upper_bounds, 
                popSize = 50,    
                maxiter = 20)     
best_fitness<-min(ga_result@fitness)
best_index <- which.min(ga_result@fitness)  
best_params <- ga_result@solution[1, ]
cat("Best Parameters (corresponding to the minimum fitness value): \n")
cat("Nodes:", best_params[1], ", Learning Rate:", best_params[2], "\n")
cat("Minimum Fitness Value:", best_fitness, "\n")
best_nodes <- best_params[1]
best_learning_rate <- best_params[2]
best_model <- keras_model_sequential()
best_model %>%
  layer_lstm(units = best_nodes, input_shape = c(1, 3)) %>%
  layer_dense(units = 1)
best_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = best_learning_rate)
)
history <- best_model %>% fit(
  x = input_data,
  y = z_power,
  epochs = 10,
  batch_size = 32,
  verbose = 1
)
mean_value1 <- mean(test_data$`total radiation`)
sd_value1 <- sd(test_data$`total radiation`)
z_total <- (test_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(test_data$`direct solar radiation`)
sd_value2 <- sd(test_data$`direct solar radiation`)
z_direct <- (test_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(test_data$power)
sd_value3 <- sd(test_data$power)
z_power <- (test_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
validation_pred <- best_model %>% predict(input_data)
print(validation_pred)
mean_power <- mean(test_data$power, na.rm = TRUE)
print(mean_power)
sd_power <- sd(test_data$power, na.rm = TRUE)
print(sd_power)
denormalized_forecast <- validation_pred * sd_power+ mean_power
length(denormalized_forecast)
denormalized_forecast <- pmax(denormalized_forecast, 0)
print(denormalized_forecast)
RMSE <- sqrt(mean((denormalized_forecast- test_data$power)^2))
print(RMSE)




# Ultra-short term forecasting using winter data (plant 3)
#LSTM model
data <- read_excel("/Users/machang/Desktop/2023/new/plant3.xlsx")
data[] <- lapply(data, as.numeric)
train_data <- data[1:1157, ] 
test_data <- data[1158:1170, ]
mean_value1 <- mean(train_data$`total radiation`)
sd_value1 <- sd(train_data$`total radiation`)
z_total <- (train_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(train_data$`direct solar radiation`)
sd_value2 <- sd(train_data$`direct solar radiation`)
z_direct <- (train_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(train_data$power)
sd_value3 <- sd(train_data$power)
z_power <- (train_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
fitness <- function(params) {
  tryCatch({
    nodes <- as.integer(params[1])  
    learning_rate <- params[2]
    model <- keras_model_sequential()
    model %>%
      layer_lstm(units = nodes, input_shape = c(1, 3)) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(lr = learning_rate)
    )
    history <- model %>% fit(
      x = input_data,
      y = z_power,
      epochs = 10,
      batch_size = 32,
      verbose = 0
    )
    train_loss <- history$metrics$loss
    return(train_loss[length(train_loss)])
  }, error = function(e) {
    message("Error in fitness function: ", conditionMessage(e))
    return(NA)  
  })
}
set.seed(123) 
lower_bounds <- c(32, 0.001)
upper_bounds <- c(128, 0.1)
bounds <- matrix(c(lower_bounds, upper_bounds), nrow = 2, byrow = TRUE)
ga_result <- ga(type = "real-valued",
                fitness = fitness,
                lower = lower_bounds,  
                upper = upper_bounds, 
                popSize = 50,    
                maxiter = 20)     
best_fitness<-min(ga_result@fitness)
best_index <- which.min(ga_result@fitness)  
best_params <- ga_result@solution[1, ]
cat("Best Parameters (corresponding to the minimum fitness value): \n")
cat("Nodes:", best_params[1], ", Learning Rate:", best_params[2], "\n")
cat("Minimum Fitness Value:", best_fitness, "\n")
best_nodes <- best_params[1]
best_learning_rate <- best_params[2]
best_model <- keras_model_sequential()
best_model %>%
  layer_lstm(units = best_nodes, input_shape = c(1, 3)) %>%
  layer_dense(units = 1)
best_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = best_learning_rate)
)
history <- best_model %>% fit(
  x = input_data,
  y = z_power,
  epochs = 10,
  batch_size = 32,
  verbose = 1
)
mean_value1 <- mean(test_data$`total radiation`)
sd_value1 <- sd(test_data$`total radiation`)
z_total <- (test_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(test_data$`direct solar radiation`)
sd_value2 <- sd(test_data$`direct solar radiation`)
z_direct <- (test_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(test_data$power)
sd_value3 <- sd(test_data$power)
z_power <- (test_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
validation_pred <- best_model %>% predict(input_data)
print(validation_pred)
mean_power <- mean(test_data$power, na.rm = TRUE)
print(mean_power)
sd_power <- sd(test_data$power, na.rm = TRUE)
print(sd_power)
denormalized_forecast <- validation_pred * sd_power+ mean_power
length(denormalized_forecast)
denormalized_forecast <- pmax(denormalized_forecast, 0)
print(denormalized_forecast)
RMSE <- sqrt(mean((denormalized_forecast- test_data$power)^2))
print(RMSE)


# Ultra-short term forecasting using spring data (plant 3)
#LSTM model
data <- read_excel("/Users/machang/Desktop/2023/new/plant3.xlsx")
data[] <- lapply(data, as.numeric)
train_data <- data[1171:2353, ] 
test_data <- data[2354:2366, ]
mean_value1 <- mean(train_data$`total radiation`)
sd_value1 <- sd(train_data$`total radiation`)
z_total <- (train_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(train_data$`direct solar radiation`)
sd_value2 <- sd(train_data$`direct solar radiation`)
z_direct <- (train_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(train_data$power)
sd_value3 <- sd(train_data$power)
z_power <- (train_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
fitness <- function(params) {
  tryCatch({
    nodes <- as.integer(params[1])  
    learning_rate <- params[2]
    model <- keras_model_sequential()
    model %>%
      layer_lstm(units = nodes, input_shape = c(1, 3)) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(lr = learning_rate)
    )
    history <- model %>% fit(
      x = input_data,
      y = z_power,
      epochs = 10,
      batch_size = 32,
      verbose = 0
    )
    train_loss <- history$metrics$loss
    return(train_loss[length(train_loss)])
  }, error = function(e) {
    message("Error in fitness function: ", conditionMessage(e))
    return(NA)  
  })
}
set.seed(123) 
lower_bounds <- c(32, 0.001)
upper_bounds <- c(128, 0.1)
bounds <- matrix(c(lower_bounds, upper_bounds), nrow = 2, byrow = TRUE)
ga_result <- ga(type = "real-valued",
                fitness = fitness,
                lower = lower_bounds,  
                upper = upper_bounds, 
                popSize = 50,    
                maxiter = 20)     
best_fitness<-min(ga_result@fitness)
best_index <- which.min(ga_result@fitness)  
best_params <- ga_result@solution[1, ]
cat("Best Parameters (corresponding to the minimum fitness value): \n")
cat("Nodes:", best_params[1], ", Learning Rate:", best_params[2], "\n")
cat("Minimum Fitness Value:", best_fitness, "\n")
best_nodes <- best_params[1]
best_learning_rate <- best_params[2]
best_model <- keras_model_sequential()
best_model %>%
  layer_lstm(units = best_nodes, input_shape = c(1, 3)) %>%
  layer_dense(units = 1)
best_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = best_learning_rate)
)
history <- best_model %>% fit(
  x = input_data,
  y = z_power,
  epochs = 10,
  batch_size = 32,
  verbose = 1
)
mean_value1 <- mean(test_data$`total radiation`)
sd_value1 <- sd(test_data$`total radiation`)
z_total <- (test_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(test_data$`direct solar radiation`)
sd_value2 <- sd(test_data$`direct solar radiation`)
z_direct <- (test_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(test_data$power)
sd_value3 <- sd(test_data$power)
z_power <- (test_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
validation_pred <- best_model %>% predict(input_data)
print(validation_pred)
mean_power <- mean(test_data$power, na.rm = TRUE)
print(mean_power)
sd_power <- sd(test_data$power, na.rm = TRUE)
print(sd_power)
denormalized_forecast <- validation_pred * sd_power+ mean_power
length(denormalized_forecast)
denormalized_forecast <- pmax(denormalized_forecast, 0)
print(denormalized_forecast)
RMSE <- sqrt(mean((denormalized_forecast- test_data$power)^2))
print(RMSE)



# Ultra-short term forecasting using summer data (plant 3)
#LSTM model
data <- read_excel("/Users/machang/Desktop/2023/new/plant3.xlsx")
data[] <- lapply(data, as.numeric)
train_data <- data[2367:3549, ] 
test_data <- data[3550:3562, ]
mean_value1 <- mean(train_data$`total radiation`)
sd_value1 <- sd(train_data$`total radiation`)
z_total <- (train_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(train_data$`direct solar radiation`)
sd_value2 <- sd(train_data$`direct solar radiation`)
z_direct <- (train_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(train_data$power)
sd_value3 <- sd(train_data$power)
z_power <- (train_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
fitness <- function(params) {
  tryCatch({
    nodes <- as.integer(params[1])  
    learning_rate <- params[2]
    model <- keras_model_sequential()
    model %>%
      layer_lstm(units = nodes, input_shape = c(1, 3)) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(lr = learning_rate)
    )
    history <- model %>% fit(
      x = input_data,
      y = z_power,
      epochs = 10,
      batch_size = 32,
      verbose = 0
    )
    train_loss <- history$metrics$loss
    return(train_loss[length(train_loss)])
  }, error = function(e) {
    message("Error in fitness function: ", conditionMessage(e))
    return(NA)  
  })
}
set.seed(123) 
lower_bounds <- c(32, 0.001)
upper_bounds <- c(128, 0.1)
bounds <- matrix(c(lower_bounds, upper_bounds), nrow = 2, byrow = TRUE)
ga_result <- ga(type = "real-valued",
                fitness = fitness,
                lower = lower_bounds,  
                upper = upper_bounds, 
                popSize = 50,    
                maxiter = 20)     
best_fitness<-min(ga_result@fitness)
best_index <- which.min(ga_result@fitness)  
best_params <- ga_result@solution[1, ]
cat("Best Parameters (corresponding to the minimum fitness value): \n")
cat("Nodes:", best_params[1], ", Learning Rate:", best_params[2], "\n")
cat("Minimum Fitness Value:", best_fitness, "\n")
best_nodes <- best_params[1]
best_learning_rate <- best_params[2]
best_model <- keras_model_sequential()
best_model %>%
  layer_lstm(units = best_nodes, input_shape = c(1, 3)) %>%
  layer_dense(units = 1)
best_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = best_learning_rate)
)
history <- best_model %>% fit(
  x = input_data,
  y = z_power,
  epochs = 10,
  batch_size = 32,
  verbose = 1
)
mean_value1 <- mean(test_data$`total radiation`)
sd_value1 <- sd(test_data$`total radiation`)
z_total <- (test_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(test_data$`direct solar radiation`)
sd_value2 <- sd(test_data$`direct solar radiation`)
z_direct <- (test_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(test_data$power)
sd_value3 <- sd(test_data$power)
z_power <- (test_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
validation_pred <- best_model %>% predict(input_data)
print(validation_pred)
mean_power <- mean(test_data$power, na.rm = TRUE)
print(mean_power)
sd_power <- sd(test_data$power, na.rm = TRUE)
print(sd_power)
denormalized_forecast <- validation_pred * sd_power+ mean_power
length(denormalized_forecast)
denormalized_forecast <- pmax(denormalized_forecast, 0)
print(denormalized_forecast)
RMSE <- sqrt(mean((denormalized_forecast- test_data$power)^2))
print(RMSE)





# Ultra-short term forecasting using autumn data (plant 3)
#LSTM model
data <- read_excel("/Users/machang/Desktop/2023/new/plant3.xlsx")
data[] <- lapply(data, as.numeric)
train_data <- data[3563:4732, ] 
test_data <- data[4733:4745, ]
mean_value1 <- mean(train_data$`total radiation`)
sd_value1 <- sd(train_data$`total radiation`)
z_total <- (train_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(train_data$`direct solar radiation`)
sd_value2 <- sd(train_data$`direct solar radiation`)
z_direct <- (train_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(train_data$power)
sd_value3 <- sd(train_data$power)
z_power <- (train_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
fitness <- function(params) {
  tryCatch({
    nodes <- as.integer(params[1])  
    learning_rate <- params[2]
    model <- keras_model_sequential()
    model %>%
      layer_lstm(units = nodes, input_shape = c(1, 3)) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(lr = learning_rate)
    )
    history <- model %>% fit(
      x = input_data,
      y = z_power,
      epochs = 10,
      batch_size = 32,
      verbose = 0
    )
    train_loss <- history$metrics$loss
    return(train_loss[length(train_loss)])
  }, error = function(e) {
    message("Error in fitness function: ", conditionMessage(e))
    return(NA)  
  })
}
set.seed(123) 
lower_bounds <- c(32, 0.001)
upper_bounds <- c(128, 0.1)
bounds <- matrix(c(lower_bounds, upper_bounds), nrow = 2, byrow = TRUE)
ga_result <- ga(type = "real-valued",
                fitness = fitness,
                lower = lower_bounds,  
                upper = upper_bounds, 
                popSize = 50,    
                maxiter = 20)     
best_fitness<-min(ga_result@fitness)
best_index <- which.min(ga_result@fitness)  
best_params <- ga_result@solution[1, ]
cat("Best Parameters (corresponding to the minimum fitness value): \n")
cat("Nodes:", best_params[1], ", Learning Rate:", best_params[2], "\n")
cat("Minimum Fitness Value:", best_fitness, "\n")
best_nodes <- best_params[1]
best_learning_rate <- best_params[2]
best_model <- keras_model_sequential()
best_model %>%
  layer_lstm(units = best_nodes, input_shape = c(1, 3)) %>%
  layer_dense(units = 1)
best_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = best_learning_rate)
)
history <- best_model %>% fit(
  x = input_data,
  y = z_power,
  epochs = 10,
  batch_size = 32,
  verbose = 1
)
mean_value1 <- mean(test_data$`total radiation`)
sd_value1 <- sd(test_data$`total radiation`)
z_total <- (test_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(test_data$`direct solar radiation`)
sd_value2 <- sd(test_data$`direct solar radiation`)
z_direct <- (test_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(test_data$power)
sd_value3 <- sd(test_data$power)
z_power <- (test_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
validation_pred <- best_model %>% predict(input_data)
print(validation_pred)
mean_power <- mean(test_data$power, na.rm = TRUE)
print(mean_power)
sd_power <- sd(test_data$power, na.rm = TRUE)
print(sd_power)
denormalized_forecast <- validation_pred * sd_power+ mean_power
length(denormalized_forecast)
denormalized_forecast <- pmax(denormalized_forecast, 0)
print(denormalized_forecast)
RMSE <- sqrt(mean((denormalized_forecast- test_data$power)^2))
print(RMSE)




# Ultra-short term forecasting using winter data (plant 4)
#LSTM model
data <- read_excel("/Users/machang/Desktop/2023/new/plant4.xlsx")
data[] <- lapply(data, as.numeric)
train_data <- data[1:1157, ] 
test_data <- data[1158:1170, ]
mean_value1 <- mean(train_data$`total radiation`)
sd_value1 <- sd(train_data$`total radiation`)
z_total <- (train_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(train_data$`direct solar radiation`)
sd_value2 <- sd(train_data$`direct solar radiation`)
z_direct <- (train_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(train_data$power)
sd_value3 <- sd(train_data$power)
z_power <- (train_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
fitness <- function(params) {
  tryCatch({
    nodes <- as.integer(params[1])  
    learning_rate <- params[2]
    model <- keras_model_sequential()
    model %>%
      layer_lstm(units = nodes, input_shape = c(1, 3)) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(lr = learning_rate)
    )
    history <- model %>% fit(
      x = input_data,
      y = z_power,
      epochs = 10,
      batch_size = 32,
      verbose = 0
    )
    train_loss <- history$metrics$loss
    return(train_loss[length(train_loss)])
  }, error = function(e) {
    message("Error in fitness function: ", conditionMessage(e))
    return(NA)  
  })
}
set.seed(123) 
lower_bounds <- c(32, 0.001)
upper_bounds <- c(128, 0.1)
bounds <- matrix(c(lower_bounds, upper_bounds), nrow = 2, byrow = TRUE)
ga_result <- ga(type = "real-valued",
                fitness = fitness,
                lower = lower_bounds,  
                upper = upper_bounds, 
                popSize = 50,    
                maxiter = 20)     
best_fitness<-min(ga_result@fitness)
best_index <- which.min(ga_result@fitness)  
best_params <- ga_result@solution[1, ]
cat("Best Parameters (corresponding to the minimum fitness value): \n")
cat("Nodes:", best_params[1], ", Learning Rate:", best_params[2], "\n")
cat("Minimum Fitness Value:", best_fitness, "\n")
best_nodes <- best_params[1]
best_learning_rate <- best_params[2]
best_model <- keras_model_sequential()
best_model %>%
  layer_lstm(units = best_nodes, input_shape = c(1, 3)) %>%
  layer_dense(units = 1)
best_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = best_learning_rate)
)
history <- best_model %>% fit(
  x = input_data,
  y = z_power,
  epochs = 10,
  batch_size = 32,
  verbose = 1
)
mean_value1 <- mean(test_data$`total radiation`)
sd_value1 <- sd(test_data$`total radiation`)
z_total <- (test_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(test_data$`direct solar radiation`)
sd_value2 <- sd(test_data$`direct solar radiation`)
z_direct <- (test_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(test_data$power)
sd_value3 <- sd(test_data$power)
z_power <- (test_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
validation_pred <- best_model %>% predict(input_data)
print(validation_pred)
mean_power <- mean(test_data$power, na.rm = TRUE)
print(mean_power)
sd_power <- sd(test_data$power, na.rm = TRUE)
print(sd_power)
denormalized_forecast <- validation_pred * sd_power+ mean_power
length(denormalized_forecast)
denormalized_forecast <- pmax(denormalized_forecast, 0)
print(denormalized_forecast)
RMSE <- sqrt(mean((denormalized_forecast- test_data$power)^2))
print(RMSE)


# Ultra-short term forecasting using spring data (plant 4)
#LSTM model
data <- read_excel("/Users/machang/Desktop/2023/new/plant4.xlsx")
data[] <- lapply(data, as.numeric)
train_data <- data[1171:2353, ] 
test_data <- data[2354:2366, ]
mean_value1 <- mean(train_data$`total radiation`)
sd_value1 <- sd(train_data$`total radiation`)
z_total <- (train_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(train_data$`direct solar radiation`)
sd_value2 <- sd(train_data$`direct solar radiation`)
z_direct <- (train_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(train_data$power)
sd_value3 <- sd(train_data$power)
z_power <- (train_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
fitness <- function(params) {
  tryCatch({
    nodes <- as.integer(params[1])  
    learning_rate <- params[2]
    model <- keras_model_sequential()
    model %>%
      layer_lstm(units = nodes, input_shape = c(1, 3)) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(lr = learning_rate)
    )
    history <- model %>% fit(
      x = input_data,
      y = z_power,
      epochs = 10,
      batch_size = 32,
      verbose = 0
    )
    train_loss <- history$metrics$loss
    return(train_loss[length(train_loss)])
  }, error = function(e) {
    message("Error in fitness function: ", conditionMessage(e))
    return(NA)  
  })
}
set.seed(123) 
lower_bounds <- c(32, 0.001)
upper_bounds <- c(128, 0.1)
bounds <- matrix(c(lower_bounds, upper_bounds), nrow = 2, byrow = TRUE)
ga_result <- ga(type = "real-valued",
                fitness = fitness,
                lower = lower_bounds,  
                upper = upper_bounds, 
                popSize = 50,    
                maxiter = 20)     
best_fitness<-min(ga_result@fitness)
best_index <- which.min(ga_result@fitness)  
best_params <- ga_result@solution[1, ]
cat("Best Parameters (corresponding to the minimum fitness value): \n")
cat("Nodes:", best_params[1], ", Learning Rate:", best_params[2], "\n")
cat("Minimum Fitness Value:", best_fitness, "\n")
best_nodes <- best_params[1]
best_learning_rate <- best_params[2]
best_model <- keras_model_sequential()
best_model %>%
  layer_lstm(units = best_nodes, input_shape = c(1, 3)) %>%
  layer_dense(units = 1)
best_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = best_learning_rate)
)
history <- best_model %>% fit(
  x = input_data,
  y = z_power,
  epochs = 10,
  batch_size = 32,
  verbose = 1
)
mean_value1 <- mean(test_data$`total radiation`)
sd_value1 <- sd(test_data$`total radiation`)
z_total <- (test_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(test_data$`direct solar radiation`)
sd_value2 <- sd(test_data$`direct solar radiation`)
z_direct <- (test_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(test_data$power)
sd_value3 <- sd(test_data$power)
z_power <- (test_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
validation_pred <- best_model %>% predict(input_data)
print(validation_pred)
mean_power <- mean(test_data$power, na.rm = TRUE)
print(mean_power)
sd_power <- sd(test_data$power, na.rm = TRUE)
print(sd_power)
denormalized_forecast <- validation_pred * sd_power+ mean_power
length(denormalized_forecast)
denormalized_forecast <- pmax(denormalized_forecast, 0)
print(denormalized_forecast)
RMSE <- sqrt(mean((denormalized_forecast- test_data$power)^2))
print(RMSE)



# Ultra-short term forecasting using summer data (plant 4)
#LSTM model
data <- read_excel("/Users/machang/Desktop/2023/new/plant4.xlsx")
data[] <- lapply(data, as.numeric)
train_data <- data[2367:3549, ] 
test_data <- data[3550:3562, ]
mean_value1 <- mean(train_data$`total radiation`)
sd_value1 <- sd(train_data$`total radiation`)
z_total <- (train_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(train_data$`direct solar radiation`)
sd_value2 <- sd(train_data$`direct solar radiation`)
z_direct <- (train_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(train_data$power)
sd_value3 <- sd(train_data$power)
z_power <- (train_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
fitness <- function(params) {
  tryCatch({
    nodes <- as.integer(params[1])  
    learning_rate <- params[2]
    model <- keras_model_sequential()
    model %>%
      layer_lstm(units = nodes, input_shape = c(1, 3)) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(lr = learning_rate)
    )
    history <- model %>% fit(
      x = input_data,
      y = z_power,
      epochs = 10,
      batch_size = 32,
      verbose = 0
    )
    train_loss <- history$metrics$loss
    return(train_loss[length(train_loss)])
  }, error = function(e) {
    message("Error in fitness function: ", conditionMessage(e))
    return(NA)  
  })
}
set.seed(123) 
lower_bounds <- c(32, 0.001)
upper_bounds <- c(128, 0.1)
bounds <- matrix(c(lower_bounds, upper_bounds), nrow = 2, byrow = TRUE)
ga_result <- ga(type = "real-valued",
                fitness = fitness,
                lower = lower_bounds,  
                upper = upper_bounds, 
                popSize = 50,    
                maxiter = 20)     
best_fitness<-min(ga_result@fitness)
best_index <- which.min(ga_result@fitness)  
best_params <- ga_result@solution[1, ]
cat("Best Parameters (corresponding to the minimum fitness value): \n")
cat("Nodes:", best_params[1], ", Learning Rate:", best_params[2], "\n")
cat("Minimum Fitness Value:", best_fitness, "\n")
best_nodes <- best_params[1]
best_learning_rate <- best_params[2]
best_model <- keras_model_sequential()
best_model %>%
  layer_lstm(units = best_nodes, input_shape = c(1, 3)) %>%
  layer_dense(units = 1)
best_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = best_learning_rate)
)
history <- best_model %>% fit(
  x = input_data,
  y = z_power,
  epochs = 10,
  batch_size = 32,
  verbose = 1
)
mean_value1 <- mean(test_data$`total radiation`)
sd_value1 <- sd(test_data$`total radiation`)
z_total <- (test_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(test_data$`direct solar radiation`)
sd_value2 <- sd(test_data$`direct solar radiation`)
z_direct <- (test_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(test_data$power)
sd_value3 <- sd(test_data$power)
z_power <- (test_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
validation_pred <- best_model %>% predict(input_data)
print(validation_pred)
mean_power <- mean(test_data$power, na.rm = TRUE)
print(mean_power)
sd_power <- sd(test_data$power, na.rm = TRUE)
print(sd_power)
denormalized_forecast <- validation_pred * sd_power+ mean_power
length(denormalized_forecast)
denormalized_forecast <- pmax(denormalized_forecast, 0)
print(denormalized_forecast)
RMSE <- sqrt(mean((denormalized_forecast- test_data$power)^2))
print(RMSE)





# Ultra-short term forecasting using autumn data (plant 4)
#LSTM model
data <- read_excel("/Users/machang/Desktop/2023/new/plant4.xlsx")
data[] <- lapply(data, as.numeric)
train_data <- data[3563:4732, ] 
test_data <- data[4733:4745, ]
mean_value1 <- mean(train_data$`total radiation`)
sd_value1 <- sd(train_data$`total radiation`)
z_total <- (train_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(train_data$`direct solar radiation`)
sd_value2 <- sd(train_data$`direct solar radiation`)
z_direct <- (train_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(train_data$power)
sd_value3 <- sd(train_data$power)
z_power <- (train_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
fitness <- function(params) {
  tryCatch({
    nodes <- as.integer(params[1])  
    learning_rate <- params[2]
    model <- keras_model_sequential()
    model %>%
      layer_lstm(units = nodes, input_shape = c(1, 3)) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(lr = learning_rate)
    )
    history <- model %>% fit(
      x = input_data,
      y = z_power,
      epochs = 10,
      batch_size = 32,
      verbose = 0
    )
    train_loss <- history$metrics$loss
    return(train_loss[length(train_loss)])
  }, error = function(e) {
    message("Error in fitness function: ", conditionMessage(e))
    return(NA)  
  })
}
set.seed(123) 
lower_bounds <- c(32, 0.001)
upper_bounds <- c(128, 0.1)
bounds <- matrix(c(lower_bounds, upper_bounds), nrow = 2, byrow = TRUE)
ga_result <- ga(type = "real-valued",
                fitness = fitness,
                lower = lower_bounds,  
                upper = upper_bounds, 
                popSize = 50,    
                maxiter = 20)     
best_fitness<-min(ga_result@fitness)
best_index <- which.min(ga_result@fitness)  
best_params <- ga_result@solution[1, ]
cat("Best Parameters (corresponding to the minimum fitness value): \n")
cat("Nodes:", best_params[1], ", Learning Rate:", best_params[2], "\n")
cat("Minimum Fitness Value:", best_fitness, "\n")
best_nodes <- best_params[1]
best_learning_rate <- best_params[2]
best_model <- keras_model_sequential()
best_model %>%
  layer_lstm(units = best_nodes, input_shape = c(1, 3)) %>%
  layer_dense(units = 1)
best_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = best_learning_rate)
)
history <- best_model %>% fit(
  x = input_data,
  y = z_power,
  epochs = 10,
  batch_size = 32,
  verbose = 1
)
mean_value1 <- mean(test_data$`total radiation`)
sd_value1 <- sd(test_data$`total radiation`)
z_total <- (test_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(test_data$`direct solar radiation`)
sd_value2 <- sd(test_data$`direct solar radiation`)
z_direct <- (test_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(test_data$power)
sd_value3 <- sd(test_data$power)
z_power <- (test_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
validation_pred <- best_model %>% predict(input_data)
print(validation_pred)
mean_power <- mean(test_data$power, na.rm = TRUE)
print(mean_power)
sd_power <- sd(test_data$power, na.rm = TRUE)
print(sd_power)
denormalized_forecast <- validation_pred * sd_power+ mean_power
length(denormalized_forecast)
denormalized_forecast <- pmax(denormalized_forecast, 0)
print(denormalized_forecast)
RMSE <- sqrt(mean((denormalized_forecast- test_data$power)^2))
print(RMSE)




# Ultra-short term forecasting using winter data (plant 5)
#LSTM model
data <- read_excel("/Users/machang/Desktop/2023/new/plant5.xlsx")
data[] <- lapply(data, as.numeric)
train_data <- data[1:1157, ] 
test_data <- data[1158:1170, ]
mean_value1 <- mean(train_data$`total radiation`)
sd_value1 <- sd(train_data$`total radiation`)
z_total <- (train_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(train_data$`direct solar radiation`)
sd_value2 <- sd(train_data$`direct solar radiation`)
z_direct <- (train_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(train_data$power)
sd_value3 <- sd(train_data$power)
z_power <- (train_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
fitness <- function(params) {
  tryCatch({
    nodes <- as.integer(params[1])  
    learning_rate <- params[2]
    model <- keras_model_sequential()
    model %>%
      layer_lstm(units = nodes, input_shape = c(1, 3)) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(lr = learning_rate)
    )
    history <- model %>% fit(
      x = input_data,
      y = z_power,
      epochs = 10,
      batch_size = 32,
      verbose = 0
    )
    train_loss <- history$metrics$loss
    return(train_loss[length(train_loss)])
  }, error = function(e) {
    message("Error in fitness function: ", conditionMessage(e))
    return(NA)  
  })
}
set.seed(123) 
lower_bounds <- c(32, 0.001)
upper_bounds <- c(128, 0.1)
bounds <- matrix(c(lower_bounds, upper_bounds), nrow = 2, byrow = TRUE)
ga_result <- ga(type = "real-valued",
                fitness = fitness,
                lower = lower_bounds,  
                upper = upper_bounds, 
                popSize = 50,    
                maxiter = 20)     
best_fitness<-min(ga_result@fitness)
best_index <- which.min(ga_result@fitness)  
best_params <- ga_result@solution[1, ]
cat("Best Parameters (corresponding to the minimum fitness value): \n")
cat("Nodes:", best_params[1], ", Learning Rate:", best_params[2], "\n")
cat("Minimum Fitness Value:", best_fitness, "\n")
best_nodes <- best_params[1]
best_learning_rate <- best_params[2]
best_model <- keras_model_sequential()
best_model %>%
  layer_lstm(units = best_nodes, input_shape = c(1, 3)) %>%
  layer_dense(units = 1)
best_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = best_learning_rate)
)
history <- best_model %>% fit(
  x = input_data,
  y = z_power,
  epochs = 10,
  batch_size = 32,
  verbose = 1
)
mean_value1 <- mean(test_data$`total radiation`)
sd_value1 <- sd(test_data$`total radiation`)
z_total <- (test_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(test_data$`direct solar radiation`)
sd_value2 <- sd(test_data$`direct solar radiation`)
z_direct <- (test_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(test_data$power)
sd_value3 <- sd(test_data$power)
z_power <- (test_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
validation_pred <- best_model %>% predict(input_data)
print(validation_pred)
mean_power <- mean(test_data$power, na.rm = TRUE)
print(mean_power)
sd_power <- sd(test_data$power, na.rm = TRUE)
print(sd_power)
denormalized_forecast <- validation_pred * sd_power+ mean_power
length(denormalized_forecast)
denormalized_forecast <- pmax(denormalized_forecast, 0)
print(denormalized_forecast)
RMSE <- sqrt(mean((denormalized_forecast- test_data$power)^2))
print(RMSE)


# Ultra-short term forecasting using spring data (plant 5)
#LSTM model
data <- read_excel("/Users/machang/Desktop/2023/new/plant5.xlsx")
data[] <- lapply(data, as.numeric)
train_data <- data[1171:2353, ] 
test_data <- data[2354:2366, ]
mean_value1 <- mean(train_data$`total radiation`)
sd_value1 <- sd(train_data$`total radiation`)
z_total <- (train_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(train_data$`direct solar radiation`)
sd_value2 <- sd(train_data$`direct solar radiation`)
z_direct <- (train_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(train_data$power)
sd_value3 <- sd(train_data$power)
z_power <- (train_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
fitness <- function(params) {
  tryCatch({
    nodes <- as.integer(params[1])  
    learning_rate <- params[2]
    model <- keras_model_sequential()
    model %>%
      layer_lstm(units = nodes, input_shape = c(1, 3)) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(lr = learning_rate)
    )
    history <- model %>% fit(
      x = input_data,
      y = z_power,
      epochs = 10,
      batch_size = 32,
      verbose = 0
    )
    train_loss <- history$metrics$loss
    return(train_loss[length(train_loss)])
  }, error = function(e) {
    message("Error in fitness function: ", conditionMessage(e))
    return(NA)  
  })
}
set.seed(123) 
lower_bounds <- c(32, 0.001)
upper_bounds <- c(128, 0.1)
bounds <- matrix(c(lower_bounds, upper_bounds), nrow = 2, byrow = TRUE)
ga_result <- ga(type = "real-valued",
                fitness = fitness,
                lower = lower_bounds,  
                upper = upper_bounds, 
                popSize = 50,    
                maxiter = 20)     
best_fitness<-min(ga_result@fitness)
best_index <- which.min(ga_result@fitness)  
best_params <- ga_result@solution[1, ]
cat("Best Parameters (corresponding to the minimum fitness value): \n")
cat("Nodes:", best_params[1], ", Learning Rate:", best_params[2], "\n")
cat("Minimum Fitness Value:", best_fitness, "\n")
best_nodes <- best_params[1]
best_learning_rate <- best_params[2]
best_model <- keras_model_sequential()
best_model %>%
  layer_lstm(units = best_nodes, input_shape = c(1, 3)) %>%
  layer_dense(units = 1)
best_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = best_learning_rate)
)
history <- best_model %>% fit(
  x = input_data,
  y = z_power,
  epochs = 10,
  batch_size = 32,
  verbose = 1
)
mean_value1 <- mean(test_data$`total radiation`)
sd_value1 <- sd(test_data$`total radiation`)
z_total <- (test_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(test_data$`direct solar radiation`)
sd_value2 <- sd(test_data$`direct solar radiation`)
z_direct <- (test_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(test_data$power)
sd_value3 <- sd(test_data$power)
z_power <- (test_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
validation_pred <- best_model %>% predict(input_data)
print(validation_pred)
mean_power <- mean(test_data$power, na.rm = TRUE)
print(mean_power)
sd_power <- sd(test_data$power, na.rm = TRUE)
print(sd_power)
denormalized_forecast <- validation_pred * sd_power+ mean_power
length(denormalized_forecast)
denormalized_forecast <- pmax(denormalized_forecast, 0)
print(denormalized_forecast)
RMSE <- sqrt(mean((denormalized_forecast- test_data$power)^2))
print(RMSE)



# Ultra-short term forecasting using summer data (plant 5)
#LSTM model
data <- read_excel("/Users/machang/Desktop/2023/new/plant5.xlsx")
data[] <- lapply(data, as.numeric)
train_data <- data[2367:3549, ] 
test_data <- data[3550:3562, ]
mean_value1 <- mean(train_data$`total radiation`)
sd_value1 <- sd(train_data$`total radiation`)
z_total <- (train_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(train_data$`direct solar radiation`)
sd_value2 <- sd(train_data$`direct solar radiation`)
z_direct <- (train_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(train_data$power)
sd_value3 <- sd(train_data$power)
z_power <- (train_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
fitness <- function(params) {
  tryCatch({
    nodes <- as.integer(params[1])  
    learning_rate <- params[2]
    model <- keras_model_sequential()
    model %>%
      layer_lstm(units = nodes, input_shape = c(1, 3)) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(lr = learning_rate)
    )
    history <- model %>% fit(
      x = input_data,
      y = z_power,
      epochs = 10,
      batch_size = 32,
      verbose = 0
    )
    train_loss <- history$metrics$loss
    return(train_loss[length(train_loss)])
  }, error = function(e) {
    message("Error in fitness function: ", conditionMessage(e))
    return(NA)  
  })
}
set.seed(123) 
lower_bounds <- c(32, 0.001)
upper_bounds <- c(128, 0.1)
bounds <- matrix(c(lower_bounds, upper_bounds), nrow = 2, byrow = TRUE)
ga_result <- ga(type = "real-valued",
                fitness = fitness,
                lower = lower_bounds,  
                upper = upper_bounds, 
                popSize = 50,    
                maxiter = 20)     
best_fitness<-min(ga_result@fitness)
best_index <- which.min(ga_result@fitness)  
best_params <- ga_result@solution[1, ]
cat("Best Parameters (corresponding to the minimum fitness value): \n")
cat("Nodes:", best_params[1], ", Learning Rate:", best_params[2], "\n")
cat("Minimum Fitness Value:", best_fitness, "\n")
best_nodes <- best_params[1]
best_learning_rate <- best_params[2]
best_model <- keras_model_sequential()
best_model %>%
  layer_lstm(units = best_nodes, input_shape = c(1, 3)) %>%
  layer_dense(units = 1)
best_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = best_learning_rate)
)
history <- best_model %>% fit(
  x = input_data,
  y = z_power,
  epochs = 10,
  batch_size = 32,
  verbose = 1
)
mean_value1 <- mean(test_data$`total radiation`)
sd_value1 <- sd(test_data$`total radiation`)
z_total <- (test_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(test_data$`direct solar radiation`)
sd_value2 <- sd(test_data$`direct solar radiation`)
z_direct <- (test_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(test_data$power)
sd_value3 <- sd(test_data$power)
z_power <- (test_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
validation_pred <- best_model %>% predict(input_data)
print(validation_pred)
mean_power <- mean(test_data$power, na.rm = TRUE)
print(mean_power)
sd_power <- sd(test_data$power, na.rm = TRUE)
print(sd_power)
denormalized_forecast <- validation_pred * sd_power+ mean_power
length(denormalized_forecast)
denormalized_forecast <- pmax(denormalized_forecast, 0)
print(denormalized_forecast)
RMSE <- sqrt(mean((denormalized_forecast- test_data$power)^2))
print(RMSE)





# Ultra-short term forecasting using autumn data (plant 5)
#LSTM model
data <- read_excel("/Users/machang/Desktop/2023/new/plant5.xlsx")
data[] <- lapply(data, as.numeric)
train_data <- data[3563:4732, ] 
test_data <- data[4733:4745, ]

mean_value1 <- mean(train_data$`total radiation`)
sd_value1 <- sd(train_data$`total radiation`)
z_total <- (train_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(train_data$`direct solar radiation`)
sd_value2 <- sd(train_data$`direct solar radiation`)
z_direct <- (train_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(train_data$power)
sd_value3 <- sd(train_data$power)
z_power <- (train_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
fitness <- function(params) {
  tryCatch({
    nodes <- as.integer(params[1])  
    learning_rate <- params[2]
    model <- keras_model_sequential()
    model %>%
      layer_lstm(units = nodes, input_shape = c(1, 3)) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(lr = learning_rate)
    )
    history <- model %>% fit(
      x = input_data,
      y = z_power,
      epochs = 10,
      batch_size = 32,
      verbose = 0
    )
    train_loss <- history$metrics$loss
    return(train_loss[length(train_loss)])
  }, error = function(e) {
    message("Error in fitness function: ", conditionMessage(e))
    return(NA)  
  })
}
set.seed(123) 
lower_bounds <- c(32, 0.001)
upper_bounds <- c(128, 0.1)
bounds <- matrix(c(lower_bounds, upper_bounds), nrow = 2, byrow = TRUE)
ga_result <- ga(type = "real-valued",
                fitness = fitness,
                lower = lower_bounds,  
                upper = upper_bounds, 
                popSize = 50,    
                maxiter = 20)     
best_fitness<-min(ga_result@fitness)
best_index <- which.min(ga_result@fitness)  
best_params <- ga_result@solution[1, ]
cat("Best Parameters (corresponding to the minimum fitness value): \n")
cat("Nodes:", best_params[1], ", Learning Rate:", best_params[2], "\n")
cat("Minimum Fitness Value:", best_fitness, "\n")
best_nodes <- best_params[1]
best_learning_rate <- best_params[2]
best_model <- keras_model_sequential()
best_model %>%
  layer_lstm(units = best_nodes, input_shape = c(1, 3)) %>%
  layer_dense(units = 1)
best_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = best_learning_rate)
)
history <- best_model %>% fit(
  x = input_data,
  y = z_power,
  epochs = 10,
  batch_size = 32,
  verbose = 1
)
mean_value1 <- mean(test_data$`total radiation`)
sd_value1 <- sd(test_data$`total radiation`)
z_total <- (test_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(test_data$`direct solar radiation`)
sd_value2 <- sd(test_data$`direct solar radiation`)
z_direct <- (test_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(test_data$power)
sd_value3 <- sd(test_data$power)
z_power <- (test_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
validation_pred <- best_model %>% predict(input_data)
print(validation_pred)
mean_power <- mean(test_data$power, na.rm = TRUE)
print(mean_power)
sd_power <- sd(test_data$power, na.rm = TRUE)
print(sd_power)
denormalized_forecast <- validation_pred * sd_power+ mean_power
length(denormalized_forecast)
denormalized_forecast <- pmax(denormalized_forecast, 0)
print(denormalized_forecast)
RMSE <- sqrt(mean((denormalized_forecast- test_data$power)^2))
print(RMSE)




# Short term forecasting using winter data (plant 1)
#LSTM model
data <- read_excel("/Users/machang/Desktop/2023/new/plant1.xlsx")
data[] <- lapply(data, as.numeric)
train_data <- data[1:1105, ] 
test_data <- data[1106:1170, ]
mean_value1 <- mean(train_data$`total radiation`)
sd_value1 <- sd(train_data$`total radiation`)
z_total <- (train_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(train_data$`direct solar radiation`)
sd_value2 <- sd(train_data$`direct solar radiation`)
z_direct <- (train_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(train_data$power)
sd_value3 <- sd(train_data$power)
z_power <- (train_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
fitness <- function(params) {
  tryCatch({
    nodes <- as.integer(params[1])  
    learning_rate <- params[2]
    model <- keras_model_sequential()
    model %>%
      layer_lstm(units = nodes, input_shape = c(1, 3)) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(lr = learning_rate)
    )
    history <- model %>% fit(
      x = input_data,
      y = z_power,
      epochs = 10,
      batch_size = 32,
      verbose = 0
    )
    train_loss <- history$metrics$loss
    return(train_loss[length(train_loss)])
  }, error = function(e) {
    message("Error in fitness function: ", conditionMessage(e))
    return(NA)  
  })
}
set.seed(123) 
lower_bounds <- c(32, 0.001)
upper_bounds <- c(128, 0.1)
bounds <- matrix(c(lower_bounds, upper_bounds), nrow = 2, byrow = TRUE)
ga_result <- ga(type = "real-valued",
                fitness = fitness,
                lower = lower_bounds,  
                upper = upper_bounds, 
                popSize = 50,    
                maxiter = 20)     
best_fitness<-min(ga_result@fitness)
best_index <- which.min(ga_result@fitness)  
best_params <- ga_result@solution[1, ]
cat("Best Parameters (corresponding to the minimum fitness value): \n")
cat("Nodes:", best_params[1], ", Learning Rate:", best_params[2], "\n")
cat("Minimum Fitness Value:", best_fitness, "\n")
best_nodes <- best_params[1]
best_learning_rate <- best_params[2]
best_model <- keras_model_sequential()
best_model %>%
  layer_lstm(units = best_nodes, input_shape = c(1, 3)) %>%
  layer_dense(units = 1)
best_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = best_learning_rate)
)
history <- best_model %>% fit(
  x = input_data,
  y = z_power,
  epochs = 10,
  batch_size = 32,
  verbose = 1
)
mean_value1 <- mean(test_data$`total radiation`)
sd_value1 <- sd(test_data$`total radiation`)
z_total <- (test_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(test_data$`direct solar radiation`)
sd_value2 <- sd(test_data$`direct solar radiation`)
z_direct <- (test_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(test_data$power)
sd_value3 <- sd(test_data$power)
z_power <- (test_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
validation_pred <- best_model %>% predict(input_data)
print(validation_pred)
mean_power <- mean(test_data$power, na.rm = TRUE)
print(mean_power)
sd_power <- sd(test_data$power, na.rm = TRUE)
print(sd_power)
denormalized_forecast <- validation_pred * sd_power+ mean_power
length(denormalized_forecast)
denormalized_forecast <- pmax(denormalized_forecast, 0)
print(denormalized_forecast)
RMSE <- sqrt(mean((denormalized_forecast- test_data$power)^2))
print(RMSE)


# Short term forecasting using spring data (plant 1)
#LSTM model
data <- read_excel("/Users/machang/Desktop/2023/new/plant1.xlsx")
data[] <- lapply(data, as.numeric)
train_data <- data[1171:2301, ] 
test_data <- data[2302:2366, ]
mean_value1 <- mean(train_data$`total radiation`)
sd_value1 <- sd(train_data$`total radiation`)
z_total <- (train_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(train_data$`direct solar radiation`)
sd_value2 <- sd(train_data$`direct solar radiation`)
z_direct <- (train_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(train_data$power)
sd_value3 <- sd(train_data$power)
z_power <- (train_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
fitness <- function(params) {
  tryCatch({
    nodes <- as.integer(params[1])  
    learning_rate <- params[2]
    model <- keras_model_sequential()
    model %>%
      layer_lstm(units = nodes, input_shape = c(1, 3)) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(lr = learning_rate)
    )
    history <- model %>% fit(
      x = input_data,
      y = z_power,
      epochs = 10,
      batch_size = 32,
      verbose = 0
    )
    train_loss <- history$metrics$loss
    return(train_loss[length(train_loss)])
  }, error = function(e) {
    message("Error in fitness function: ", conditionMessage(e))
    return(NA)  
  })
}
set.seed(123) 
lower_bounds <- c(32, 0.001)
upper_bounds <- c(128, 0.1)
bounds <- matrix(c(lower_bounds, upper_bounds), nrow = 2, byrow = TRUE)
ga_result <- ga(type = "real-valued",
                fitness = fitness,
                lower = lower_bounds,  
                upper = upper_bounds, 
                popSize = 50,    
                maxiter = 20)     
best_fitness<-min(ga_result@fitness)
best_index <- which.min(ga_result@fitness)  
best_params <- ga_result@solution[1, ]
cat("Best Parameters (corresponding to the minimum fitness value): \n")
cat("Nodes:", best_params[1], ", Learning Rate:", best_params[2], "\n")
cat("Minimum Fitness Value:", best_fitness, "\n")
best_nodes <- best_params[1]
best_learning_rate <- best_params[2]
best_model <- keras_model_sequential()
best_model %>%
  layer_lstm(units = best_nodes, input_shape = c(1, 3)) %>%
  layer_dense(units = 1)
best_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = best_learning_rate)
)
history <- best_model %>% fit(
  x = input_data,
  y = z_power,
  epochs = 10,
  batch_size = 32,
  verbose = 1
)
mean_value1 <- mean(test_data$`total radiation`)
sd_value1 <- sd(test_data$`total radiation`)
z_total <- (test_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(test_data$`direct solar radiation`)
sd_value2 <- sd(test_data$`direct solar radiation`)
z_direct <- (test_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(test_data$power)
sd_value3 <- sd(test_data$power)
z_power <- (test_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
validation_pred <- best_model %>% predict(input_data)
print(validation_pred)
mean_power <- mean(test_data$power, na.rm = TRUE)
print(mean_power)
sd_power <- sd(test_data$power, na.rm = TRUE)
print(sd_power)
denormalized_forecast <- validation_pred * sd_power+ mean_power
length(denormalized_forecast)
denormalized_forecast <- pmax(denormalized_forecast, 0)
print(denormalized_forecast)
RMSE <- sqrt(mean((denormalized_forecast- test_data$power)^2))
print(RMSE)



# Short term forecasting using summer data (plant 1)
#LSTM model
data <- read_excel("/Users/machang/Desktop/2023/new/plant1.xlsx")
data[] <- lapply(data, as.numeric)
train_data <- data[2367:3497, ] 
test_data <- data[3498:3562, ]

mean_value1 <- mean(train_data$`total radiation`)
sd_value1 <- sd(train_data$`total radiation`)
z_total <- (train_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(train_data$`direct solar radiation`)
sd_value2 <- sd(train_data$`direct solar radiation`)
z_direct <- (train_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(train_data$power)
sd_value3 <- sd(train_data$power)
z_power <- (train_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
fitness <- function(params) {
  tryCatch({
    nodes <- as.integer(params[1])  
    learning_rate <- params[2]
    model <- keras_model_sequential()
    model %>%
      layer_lstm(units = nodes, input_shape = c(1, 3)) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(lr = learning_rate)
    )
    history <- model %>% fit(
      x = input_data,
      y = z_power,
      epochs = 10,
      batch_size = 32,
      verbose = 0
    )
    train_loss <- history$metrics$loss
    return(train_loss[length(train_loss)])
  }, error = function(e) {
    message("Error in fitness function: ", conditionMessage(e))
    return(NA)  
  })
}
set.seed(123) 
lower_bounds <- c(32, 0.001)
upper_bounds <- c(128, 0.1)
bounds <- matrix(c(lower_bounds, upper_bounds), nrow = 2, byrow = TRUE)
ga_result <- ga(type = "real-valued",
                fitness = fitness,
                lower = lower_bounds,  
                upper = upper_bounds, 
                popSize = 50,    
                maxiter = 20)     
best_fitness<-min(ga_result@fitness)
best_index <- which.min(ga_result@fitness)  
best_params <- ga_result@solution[1, ]
cat("Best Parameters (corresponding to the minimum fitness value): \n")
cat("Nodes:", best_params[1], ", Learning Rate:", best_params[2], "\n")
cat("Minimum Fitness Value:", best_fitness, "\n")
best_nodes <- best_params[1]
best_learning_rate <- best_params[2]
best_model <- keras_model_sequential()
best_model %>%
  layer_lstm(units = best_nodes, input_shape = c(1, 3)) %>%
  layer_dense(units = 1)
best_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = best_learning_rate)
)
history <- best_model %>% fit(
  x = input_data,
  y = z_power,
  epochs = 10,
  batch_size = 32,
  verbose = 1
)
mean_value1 <- mean(test_data$`total radiation`)
sd_value1 <- sd(test_data$`total radiation`)
z_total <- (test_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(test_data$`direct solar radiation`)
sd_value2 <- sd(test_data$`direct solar radiation`)
z_direct <- (test_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(test_data$power)
sd_value3 <- sd(test_data$power)
z_power <- (test_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
validation_pred <- best_model %>% predict(input_data)
print(validation_pred)
mean_power <- mean(test_data$power, na.rm = TRUE)
print(mean_power)
sd_power <- sd(test_data$power, na.rm = TRUE)
print(sd_power)
denormalized_forecast <- validation_pred * sd_power+ mean_power
length(denormalized_forecast)
denormalized_forecast <- pmax(denormalized_forecast, 0)
print(denormalized_forecast)
RMSE <- sqrt(mean((denormalized_forecast- test_data$power)^2))
print(RMSE)




# Short term forecasting using autumn data (plant 1)
#LSTM model
data <- read_excel("/Users/machang/Desktop/2023/new/plant1.xlsx")
data[] <- lapply(data, as.numeric)
train_data <- data[3563:4680, ] 
test_data <- data[4681:4745, ]
mean_value1 <- mean(train_data$`total radiation`)
sd_value1 <- sd(train_data$`total radiation`)
z_total <- (train_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(train_data$`direct solar radiation`)
sd_value2 <- sd(train_data$`direct solar radiation`)
z_direct <- (train_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(train_data$power)
sd_value3 <- sd(train_data$power)
z_power <- (train_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
fitness <- function(params) {
  tryCatch({
    nodes <- as.integer(params[1])  
    learning_rate <- params[2]
    model <- keras_model_sequential()
    model %>%
      layer_lstm(units = nodes, input_shape = c(1, 3)) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(lr = learning_rate)
    )
    history <- model %>% fit(
      x = input_data,
      y = z_power,
      epochs = 10,
      batch_size = 32,
      verbose = 0
    )
    train_loss <- history$metrics$loss
    return(train_loss[length(train_loss)])
  }, error = function(e) {
    message("Error in fitness function: ", conditionMessage(e))
    return(NA)  
  })
}
set.seed(123) 
lower_bounds <- c(32, 0.001)
upper_bounds <- c(128, 0.1)
bounds <- matrix(c(lower_bounds, upper_bounds), nrow = 2, byrow = TRUE)
ga_result <- ga(type = "real-valued",
                fitness = fitness,
                lower = lower_bounds,  
                upper = upper_bounds, 
                popSize = 50,    
                maxiter = 20)     
best_fitness<-min(ga_result@fitness)
best_index <- which.min(ga_result@fitness)  
best_params <- ga_result@solution[1, ]
cat("Best Parameters (corresponding to the minimum fitness value): \n")
cat("Nodes:", best_params[1], ", Learning Rate:", best_params[2], "\n")
cat("Minimum Fitness Value:", best_fitness, "\n")
best_nodes <- best_params[1]
best_learning_rate <- best_params[2]
best_model <- keras_model_sequential()
best_model %>%
  layer_lstm(units = best_nodes, input_shape = c(1, 3)) %>%
  layer_dense(units = 1)
best_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = best_learning_rate)
)
history <- best_model %>% fit(
  x = input_data,
  y = z_power,
  epochs = 10,
  batch_size = 32,
  verbose = 1
)
mean_value1 <- mean(test_data$`total radiation`)
sd_value1 <- sd(test_data$`total radiation`)
z_total <- (test_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(test_data$`direct solar radiation`)
sd_value2 <- sd(test_data$`direct solar radiation`)
z_direct <- (test_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(test_data$power)
sd_value3 <- sd(test_data$power)
z_power <- (test_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
validation_pred <- best_model %>% predict(input_data)
print(validation_pred)
mean_power <- mean(test_data$power, na.rm = TRUE)
print(mean_power)
sd_power <- sd(test_data$power, na.rm = TRUE)
print(sd_power)
denormalized_forecast <- validation_pred * sd_power+ mean_power
length(denormalized_forecast)
denormalized_forecast <- pmax(denormalized_forecast, 0)
print(denormalized_forecast)
RMSE <- sqrt(mean((denormalized_forecast- test_data$power)^2))
print(RMSE)




# Short term forecasting using winter data (plant 2)
#LSTM model
data <- read_excel("/Users/machang/Desktop/2023/new/plant2.xlsx")
data[] <- lapply(data, as.numeric)
train_data <- data[1:1105, ] 
test_data <- data[1106:1170, ]
mean_value1 <- mean(train_data$`total radiation`)
sd_value1 <- sd(train_data$`total radiation`)
z_total <- (train_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(train_data$`direct solar radiation`)
sd_value2 <- sd(train_data$`direct solar radiation`)
z_direct <- (train_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(train_data$power)
sd_value3 <- sd(train_data$power)
z_power <- (train_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
fitness <- function(params) {
  tryCatch({
    nodes <- as.integer(params[1])  
    learning_rate <- params[2]
    model <- keras_model_sequential()
    model %>%
      layer_lstm(units = nodes, input_shape = c(1, 3)) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(lr = learning_rate)
    )
    history <- model %>% fit(
      x = input_data,
      y = z_power,
      epochs = 10,
      batch_size = 32,
      verbose = 0
    )
    train_loss <- history$metrics$loss
    return(train_loss[length(train_loss)])
  }, error = function(e) {
    message("Error in fitness function: ", conditionMessage(e))
    return(NA)  
  })
}
set.seed(123) 
lower_bounds <- c(32, 0.001)
upper_bounds <- c(128, 0.1)
bounds <- matrix(c(lower_bounds, upper_bounds), nrow = 2, byrow = TRUE)
ga_result <- ga(type = "real-valued",
                fitness = fitness,
                lower = lower_bounds,  
                upper = upper_bounds, 
                popSize = 50,    
                maxiter = 20)     
best_fitness<-min(ga_result@fitness)
best_index <- which.min(ga_result@fitness)  
best_params <- ga_result@solution[1, ]
cat("Best Parameters (corresponding to the minimum fitness value): \n")
cat("Nodes:", best_params[1], ", Learning Rate:", best_params[2], "\n")
cat("Minimum Fitness Value:", best_fitness, "\n")
best_nodes <- best_params[1]
best_learning_rate <- best_params[2]
best_model <- keras_model_sequential()
best_model %>%
  layer_lstm(units = best_nodes, input_shape = c(1, 3)) %>%
  layer_dense(units = 1)
best_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = best_learning_rate)
)
history <- best_model %>% fit(
  x = input_data,
  y = z_power,
  epochs = 10,
  batch_size = 32,
  verbose = 1
)
mean_value1 <- mean(test_data$`total radiation`)
sd_value1 <- sd(test_data$`total radiation`)
z_total <- (test_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(test_data$`direct solar radiation`)
sd_value2 <- sd(test_data$`direct solar radiation`)
z_direct <- (test_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(test_data$power)
sd_value3 <- sd(test_data$power)
z_power <- (test_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
validation_pred <- best_model %>% predict(input_data)
print(validation_pred)
mean_power <- mean(test_data$power, na.rm = TRUE)
print(mean_power)
sd_power <- sd(test_data$power, na.rm = TRUE)
print(sd_power)
denormalized_forecast <- validation_pred * sd_power+ mean_power
length(denormalized_forecast)
denormalized_forecast <- pmax(denormalized_forecast, 0)
print(denormalized_forecast)
RMSE <- sqrt(mean((denormalized_forecast- test_data$power)^2))
print(RMSE)


# Short term forecasting using spring data (plant 2)
#LSTM model
data <- read_excel("/Users/machang/Desktop/2023/new/plant2.xlsx")
data[] <- lapply(data, as.numeric)
train_data <- data[1171:2301, ] 
test_data <- data[2302:2366, ]
mean_value1 <- mean(train_data$`total radiation`)
sd_value1 <- sd(train_data$`total radiation`)
z_total <- (train_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(train_data$`direct solar radiation`)
sd_value2 <- sd(train_data$`direct solar radiation`)
z_direct <- (train_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(train_data$power)
sd_value3 <- sd(train_data$power)
z_power <- (train_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
fitness <- function(params) {
  tryCatch({
    nodes <- as.integer(params[1])  
    learning_rate <- params[2]
    model <- keras_model_sequential()
    model %>%
      layer_lstm(units = nodes, input_shape = c(1, 3)) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(lr = learning_rate)
    )
    history <- model %>% fit(
      x = input_data,
      y = z_power,
      epochs = 10,
      batch_size = 32,
      verbose = 0
    )
    train_loss <- history$metrics$loss
    return(train_loss[length(train_loss)])
  }, error = function(e) {
    message("Error in fitness function: ", conditionMessage(e))
    return(NA)  
  })
}
set.seed(123) 
lower_bounds <- c(32, 0.001)
upper_bounds <- c(128, 0.1)
bounds <- matrix(c(lower_bounds, upper_bounds), nrow = 2, byrow = TRUE)
ga_result <- ga(type = "real-valued",
                fitness = fitness,
                lower = lower_bounds,  
                upper = upper_bounds, 
                popSize = 50,    
                maxiter = 20)     
best_fitness<-min(ga_result@fitness)
best_index <- which.min(ga_result@fitness)  
best_params <- ga_result@solution[1, ]
cat("Best Parameters (corresponding to the minimum fitness value): \n")
cat("Nodes:", best_params[1], ", Learning Rate:", best_params[2], "\n")
cat("Minimum Fitness Value:", best_fitness, "\n")
best_nodes <- best_params[1]
best_learning_rate <- best_params[2]
best_model <- keras_model_sequential()
best_model %>%
  layer_lstm(units = best_nodes, input_shape = c(1, 3)) %>%
  layer_dense(units = 1)
best_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = best_learning_rate)
)
history <- best_model %>% fit(
  x = input_data,
  y = z_power,
  epochs = 10,
  batch_size = 32,
  verbose = 1
)
mean_value1 <- mean(test_data$`total radiation`)
sd_value1 <- sd(test_data$`total radiation`)
z_total <- (test_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(test_data$`direct solar radiation`)
sd_value2 <- sd(test_data$`direct solar radiation`)
z_direct <- (test_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(test_data$power)
sd_value3 <- sd(test_data$power)
z_power <- (test_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
validation_pred <- best_model %>% predict(input_data)
print(validation_pred)
mean_power <- mean(test_data$power, na.rm = TRUE)
print(mean_power)
sd_power <- sd(test_data$power, na.rm = TRUE)
print(sd_power)
denormalized_forecast <- validation_pred * sd_power+ mean_power
length(denormalized_forecast)
denormalized_forecast <- pmax(denormalized_forecast, 0)
print(denormalized_forecast)
RMSE <- sqrt(mean((denormalized_forecast- test_data$power)^2))
print(RMSE)



# Short term forecasting using summer data (plant 2)
#LSTM model
data <- read_excel("/Users/machang/Desktop/2023/new/plant2.xlsx")
data[] <- lapply(data, as.numeric)
train_data <- data[2367:3497, ] 
test_data <- data[3498:3562, ]
mean_value1 <- mean(train_data$`total radiation`)
sd_value1 <- sd(train_data$`total radiation`)
z_total <- (train_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(train_data$`direct solar radiation`)
sd_value2 <- sd(train_data$`direct solar radiation`)
z_direct <- (train_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(train_data$power)
sd_value3 <- sd(train_data$power)
z_power <- (train_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
fitness <- function(params) {
  tryCatch({
    nodes <- as.integer(params[1])  
    learning_rate <- params[2]
    model <- keras_model_sequential()
    model %>%
      layer_lstm(units = nodes, input_shape = c(1, 3)) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(lr = learning_rate)
    )
    history <- model %>% fit(
      x = input_data,
      y = z_power,
      epochs = 10,
      batch_size = 32,
      verbose = 0
    )
    train_loss <- history$metrics$loss
    return(train_loss[length(train_loss)])
  }, error = function(e) {
    message("Error in fitness function: ", conditionMessage(e))
    return(NA)  
  })
}
set.seed(123) 
lower_bounds <- c(32, 0.001)
upper_bounds <- c(128, 0.1)
bounds <- matrix(c(lower_bounds, upper_bounds), nrow = 2, byrow = TRUE)
ga_result <- ga(type = "real-valued",
                fitness = fitness,
                lower = lower_bounds,  
                upper = upper_bounds, 
                popSize = 50,    
                maxiter = 20)     
best_fitness<-min(ga_result@fitness)
best_index <- which.min(ga_result@fitness)  
best_params <- ga_result@solution[1, ]
cat("Best Parameters (corresponding to the minimum fitness value): \n")
cat("Nodes:", best_params[1], ", Learning Rate:", best_params[2], "\n")
cat("Minimum Fitness Value:", best_fitness, "\n")
best_nodes <- best_params[1]
best_learning_rate <- best_params[2]
best_model <- keras_model_sequential()
best_model %>%
  layer_lstm(units = best_nodes, input_shape = c(1, 3)) %>%
  layer_dense(units = 1)
best_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = best_learning_rate)
)
history <- best_model %>% fit(
  x = input_data,
  y = z_power,
  epochs = 10,
  batch_size = 32,
  verbose = 1
)
mean_value1 <- mean(test_data$`total radiation`)
sd_value1 <- sd(test_data$`total radiation`)
z_total <- (test_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(test_data$`direct solar radiation`)
sd_value2 <- sd(test_data$`direct solar radiation`)
z_direct <- (test_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(test_data$power)
sd_value3 <- sd(test_data$power)
z_power <- (test_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
validation_pred <- best_model %>% predict(input_data)
print(validation_pred)
mean_power <- mean(test_data$power, na.rm = TRUE)
print(mean_power)
sd_power <- sd(test_data$power, na.rm = TRUE)
print(sd_power)
denormalized_forecast <- validation_pred * sd_power+ mean_power
length(denormalized_forecast)
denormalized_forecast <- pmax(denormalized_forecast, 0)
print(denormalized_forecast)
RMSE <- sqrt(mean((denormalized_forecast- test_data$power)^2))
print(RMSE)





# Short term forecasting using autumn data (plant 2)
#LSTM model
data <- read_excel("/Users/machang/Desktop/2023/new/plant2.xlsx")
data[] <- lapply(data, as.numeric)
train_data <- data[3563:4680, ] 
test_data <- data[4681:4745, ]
mean_value1 <- mean(train_data$`total radiation`)
sd_value1 <- sd(train_data$`total radiation`)
z_total <- (train_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(train_data$`direct solar radiation`)
sd_value2 <- sd(train_data$`direct solar radiation`)
z_direct <- (train_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(train_data$power)
sd_value3 <- sd(train_data$power)
z_power <- (train_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
fitness <- function(params) {
  tryCatch({
    nodes <- as.integer(params[1])  
    learning_rate <- params[2]
    model <- keras_model_sequential()
    model %>%
      layer_lstm(units = nodes, input_shape = c(1, 3)) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(lr = learning_rate)
    )
    history <- model %>% fit(
      x = input_data,
      y = z_power,
      epochs = 10,
      batch_size = 32,
      verbose = 0
    )
    train_loss <- history$metrics$loss
    return(train_loss[length(train_loss)])
  }, error = function(e) {
    message("Error in fitness function: ", conditionMessage(e))
    return(NA)  
  })
}
set.seed(123) 
lower_bounds <- c(32, 0.001)
upper_bounds <- c(128, 0.1)
bounds <- matrix(c(lower_bounds, upper_bounds), nrow = 2, byrow = TRUE)
ga_result <- ga(type = "real-valued",
                fitness = fitness,
                lower = lower_bounds,  
                upper = upper_bounds, 
                popSize = 50,    
                maxiter = 20)     
best_fitness<-min(ga_result@fitness)
best_index <- which.min(ga_result@fitness)  
best_params <- ga_result@solution[1, ]
cat("Best Parameters (corresponding to the minimum fitness value): \n")
cat("Nodes:", best_params[1], ", Learning Rate:", best_params[2], "\n")
cat("Minimum Fitness Value:", best_fitness, "\n")
best_nodes <- best_params[1]
best_learning_rate <- best_params[2]
best_model <- keras_model_sequential()
best_model %>%
  layer_lstm(units = best_nodes, input_shape = c(1, 3)) %>%
  layer_dense(units = 1)
best_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = best_learning_rate)
)
history <- best_model %>% fit(
  x = input_data,
  y = z_power,
  epochs = 10,
  batch_size = 32,
  verbose = 1
)
mean_value1 <- mean(test_data$`total radiation`)
sd_value1 <- sd(test_data$`total radiation`)
z_total <- (test_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(test_data$`direct solar radiation`)
sd_value2 <- sd(test_data$`direct solar radiation`)
z_direct <- (test_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(test_data$power)
sd_value3 <- sd(test_data$power)
z_power <- (test_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
validation_pred <- best_model %>% predict(input_data)
print(validation_pred)
mean_power <- mean(test_data$power, na.rm = TRUE)
print(mean_power)
sd_power <- sd(test_data$power, na.rm = TRUE)
print(sd_power)
denormalized_forecast <- validation_pred * sd_power+ mean_power
length(denormalized_forecast)
denormalized_forecast <- pmax(denormalized_forecast, 0)
print(denormalized_forecast)
RMSE <- sqrt(mean((denormalized_forecast- test_data$power)^2))
print(RMSE)





# Short term forecasting using winter data (plant 3)
#LSTM model
data <- read_excel("/Users/machang/Desktop/2023/new/plant3.xlsx")
data[] <- lapply(data, as.numeric)
train_data <- data[1:1105, ] 
test_data <- data[1106:1170, ]
mean_value1 <- mean(train_data$`total radiation`)
sd_value1 <- sd(train_data$`total radiation`)
z_total <- (train_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(train_data$`direct solar radiation`)
sd_value2 <- sd(train_data$`direct solar radiation`)
z_direct <- (train_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(train_data$power)
sd_value3 <- sd(train_data$power)
z_power <- (train_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
fitness <- function(params) {
  tryCatch({
    nodes <- as.integer(params[1])  
    learning_rate <- params[2]
    model <- keras_model_sequential()
    model %>%
      layer_lstm(units = nodes, input_shape = c(1, 3)) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(lr = learning_rate)
    )
    history <- model %>% fit(
      x = input_data,
      y = z_power,
      epochs = 10,
      batch_size = 32,
      verbose = 0
    )
    train_loss <- history$metrics$loss
    return(train_loss[length(train_loss)])
  }, error = function(e) {
    message("Error in fitness function: ", conditionMessage(e))
    return(NA)  
  })
}
set.seed(123) 
lower_bounds <- c(32, 0.001)
upper_bounds <- c(128, 0.1)
bounds <- matrix(c(lower_bounds, upper_bounds), nrow = 2, byrow = TRUE)
ga_result <- ga(type = "real-valued",
                fitness = fitness,
                lower = lower_bounds,  
                upper = upper_bounds, 
                popSize = 50,    
                maxiter = 20)     
best_fitness<-min(ga_result@fitness)
best_index <- which.min(ga_result@fitness)  
best_params <- ga_result@solution[1, ]
cat("Best Parameters (corresponding to the minimum fitness value): \n")
cat("Nodes:", best_params[1], ", Learning Rate:", best_params[2], "\n")
cat("Minimum Fitness Value:", best_fitness, "\n")
best_nodes <- best_params[1]
best_learning_rate <- best_params[2]
best_model <- keras_model_sequential()
best_model %>%
  layer_lstm(units = best_nodes, input_shape = c(1, 3)) %>%
  layer_dense(units = 1)
best_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = best_learning_rate)
)
history <- best_model %>% fit(
  x = input_data,
  y = z_power,
  epochs = 10,
  batch_size = 32,
  verbose = 1
)
mean_value1 <- mean(test_data$`total radiation`)
sd_value1 <- sd(test_data$`total radiation`)
z_total <- (test_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(test_data$`direct solar radiation`)
sd_value2 <- sd(test_data$`direct solar radiation`)
z_direct <- (test_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(test_data$power)
sd_value3 <- sd(test_data$power)
z_power <- (test_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
validation_pred <- best_model %>% predict(input_data)
print(validation_pred)
mean_power <- mean(test_data$power, na.rm = TRUE)
print(mean_power)
sd_power <- sd(test_data$power, na.rm = TRUE)
print(sd_power)
denormalized_forecast <- validation_pred * sd_power+ mean_power
length(denormalized_forecast)
denormalized_forecast <- pmax(denormalized_forecast, 0)
print(denormalized_forecast)
RMSE <- sqrt(mean((denormalized_forecast- test_data$power)^2))
print(RMSE)


# Short term forecasting using spring data (plant 3)
#LSTM model
data <- read_excel("/Users/machang/Desktop/2023/new/plant3.xlsx")
data[] <- lapply(data, as.numeric)
train_data <- data[1171:2301, ] 
test_data <- data[2302:2366, ]
mean_value1 <- mean(train_data$`total radiation`)
sd_value1 <- sd(train_data$`total radiation`)
z_total <- (train_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(train_data$`direct solar radiation`)
sd_value2 <- sd(train_data$`direct solar radiation`)
z_direct <- (train_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(train_data$power)
sd_value3 <- sd(train_data$power)
z_power <- (train_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
fitness <- function(params) {
  tryCatch({
    nodes <- as.integer(params[1])  
    learning_rate <- params[2]
    model <- keras_model_sequential()
    model %>%
      layer_lstm(units = nodes, input_shape = c(1, 3)) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(lr = learning_rate)
    )
    history <- model %>% fit(
      x = input_data,
      y = z_power,
      epochs = 10,
      batch_size = 32,
      verbose = 0
    )
    train_loss <- history$metrics$loss
    return(train_loss[length(train_loss)])
  }, error = function(e) {
    message("Error in fitness function: ", conditionMessage(e))
    return(NA)  
  })
}
set.seed(123) 
lower_bounds <- c(32, 0.001)
upper_bounds <- c(128, 0.1)
bounds <- matrix(c(lower_bounds, upper_bounds), nrow = 2, byrow = TRUE)
ga_result <- ga(type = "real-valued",
                fitness = fitness,
                lower = lower_bounds,  
                upper = upper_bounds, 
                popSize = 50,    
                maxiter = 20)     
best_fitness<-min(ga_result@fitness)
best_index <- which.min(ga_result@fitness)  
best_params <- ga_result@solution[1, ]
cat("Best Parameters (corresponding to the minimum fitness value): \n")
cat("Nodes:", best_params[1], ", Learning Rate:", best_params[2], "\n")
cat("Minimum Fitness Value:", best_fitness, "\n")
best_nodes <- best_params[1]
best_learning_rate <- best_params[2]
best_model <- keras_model_sequential()
best_model %>%
  layer_lstm(units = best_nodes, input_shape = c(1, 3)) %>%
  layer_dense(units = 1)
best_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = best_learning_rate)
)
history <- best_model %>% fit(
  x = input_data,
  y = z_power,
  epochs = 10,
  batch_size = 32,
  verbose = 1
)
mean_value1 <- mean(test_data$`total radiation`)
sd_value1 <- sd(test_data$`total radiation`)
z_total <- (test_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(test_data$`direct solar radiation`)
sd_value2 <- sd(test_data$`direct solar radiation`)
z_direct <- (test_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(test_data$power)
sd_value3 <- sd(test_data$power)
z_power <- (test_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
validation_pred <- best_model %>% predict(input_data)
print(validation_pred)
mean_power <- mean(test_data$power, na.rm = TRUE)
print(mean_power)
sd_power <- sd(test_data$power, na.rm = TRUE)
print(sd_power)
denormalized_forecast <- validation_pred * sd_power+ mean_power
length(denormalized_forecast)
denormalized_forecast <- pmax(denormalized_forecast, 0)
print(denormalized_forecast)
RMSE <- sqrt(mean((denormalized_forecast- test_data$power)^2))
print(RMSE)



# Short term forecasting using summer data (plant 3)
#LSTM model
data <- read_excel("/Users/machang/Desktop/2023/new/plant3.xlsx")
data[] <- lapply(data, as.numeric)
train_data <- data[2367:3497, ] 
test_data <- data[3498:3562, ]
mean_value1 <- mean(train_data$`total radiation`)
sd_value1 <- sd(train_data$`total radiation`)
z_total <- (train_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(train_data$`direct solar radiation`)
sd_value2 <- sd(train_data$`direct solar radiation`)
z_direct <- (train_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(train_data$power)
sd_value3 <- sd(train_data$power)
z_power <- (train_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
fitness <- function(params) {
  tryCatch({
    nodes <- as.integer(params[1])  
    learning_rate <- params[2]
    model <- keras_model_sequential()
    model %>%
      layer_lstm(units = nodes, input_shape = c(1, 3)) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(lr = learning_rate)
    )
    history <- model %>% fit(
      x = input_data,
      y = z_power,
      epochs = 10,
      batch_size = 32,
      verbose = 0
    )
    train_loss <- history$metrics$loss
    return(train_loss[length(train_loss)])
  }, error = function(e) {
    message("Error in fitness function: ", conditionMessage(e))
    return(NA)  
  })
}
set.seed(123) 
lower_bounds <- c(32, 0.001)
upper_bounds <- c(128, 0.1)
bounds <- matrix(c(lower_bounds, upper_bounds), nrow = 2, byrow = TRUE)
ga_result <- ga(type = "real-valued",
                fitness = fitness,
                lower = lower_bounds,  
                upper = upper_bounds, 
                popSize = 50,    
                maxiter = 20)     
best_fitness<-min(ga_result@fitness)
best_index <- which.min(ga_result@fitness)  
best_params <- ga_result@solution[1, ]
cat("Best Parameters (corresponding to the minimum fitness value): \n")
cat("Nodes:", best_params[1], ", Learning Rate:", best_params[2], "\n")
cat("Minimum Fitness Value:", best_fitness, "\n")
best_nodes <- best_params[1]
best_learning_rate <- best_params[2]
best_model <- keras_model_sequential()
best_model %>%
  layer_lstm(units = best_nodes, input_shape = c(1, 3)) %>%
  layer_dense(units = 1)
best_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = best_learning_rate)
)
history <- best_model %>% fit(
  x = input_data,
  y = z_power,
  epochs = 10,
  batch_size = 32,
  verbose = 1
)
mean_value1 <- mean(test_data$`total radiation`)
sd_value1 <- sd(test_data$`total radiation`)
z_total <- (test_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(test_data$`direct solar radiation`)
sd_value2 <- sd(test_data$`direct solar radiation`)
z_direct <- (test_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(test_data$power)
sd_value3 <- sd(test_data$power)
z_power <- (test_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
validation_pred <- best_model %>% predict(input_data)
print(validation_pred)
mean_power <- mean(test_data$power, na.rm = TRUE)
print(mean_power)
sd_power <- sd(test_data$power, na.rm = TRUE)
print(sd_power)
denormalized_forecast <- validation_pred * sd_power+ mean_power
length(denormalized_forecast)
denormalized_forecast <- pmax(denormalized_forecast, 0)
print(denormalized_forecast)
RMSE <- sqrt(mean((denormalized_forecast- test_data$power)^2))
print(RMSE)





# Short term forecasting using autumn data (plant 3)
#LSTM model
data <- read_excel("/Users/machang/Desktop/2023/new/plant3.xlsx")
data[] <- lapply(data, as.numeric)
train_data <- data[3563:4680, ] 
test_data <- data[4681:4745, ]
mean_value1 <- mean(train_data$`total radiation`)
sd_value1 <- sd(train_data$`total radiation`)
z_total <- (train_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(train_data$`direct solar radiation`)
sd_value2 <- sd(train_data$`direct solar radiation`)
z_direct <- (train_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(train_data$power)
sd_value3 <- sd(train_data$power)
z_power <- (train_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
fitness <- function(params) {
  tryCatch({
    nodes <- as.integer(params[1])  
    learning_rate <- params[2]
    model <- keras_model_sequential()
    model %>%
      layer_lstm(units = nodes, input_shape = c(1, 3)) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(lr = learning_rate)
    )
    history <- model %>% fit(
      x = input_data,
      y = z_power,
      epochs = 10,
      batch_size = 32,
      verbose = 0
    )
    train_loss <- history$metrics$loss
    return(train_loss[length(train_loss)])
  }, error = function(e) {
    message("Error in fitness function: ", conditionMessage(e))
    return(NA)  
  })
}
set.seed(123) 
lower_bounds <- c(32, 0.001)
upper_bounds <- c(128, 0.1)
bounds <- matrix(c(lower_bounds, upper_bounds), nrow = 2, byrow = TRUE)
ga_result <- ga(type = "real-valued",
                fitness = fitness,
                lower = lower_bounds,  
                upper = upper_bounds, 
                popSize = 50,    
                maxiter = 20)     
best_fitness<-min(ga_result@fitness)
best_index <- which.min(ga_result@fitness)  
best_params <- ga_result@solution[1, ]
cat("Best Parameters (corresponding to the minimum fitness value): \n")
cat("Nodes:", best_params[1], ", Learning Rate:", best_params[2], "\n")
cat("Minimum Fitness Value:", best_fitness, "\n")
best_nodes <- best_params[1]
best_learning_rate <- best_params[2]
best_model <- keras_model_sequential()
best_model %>%
  layer_lstm(units = best_nodes, input_shape = c(1, 3)) %>%
  layer_dense(units = 1)
best_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = best_learning_rate)
)
history <- best_model %>% fit(
  x = input_data,
  y = z_power,
  epochs = 10,
  batch_size = 32,
  verbose = 1
)
mean_value1 <- mean(test_data$`total radiation`)
sd_value1 <- sd(test_data$`total radiation`)
z_total <- (test_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(test_data$`direct solar radiation`)
sd_value2 <- sd(test_data$`direct solar radiation`)
z_direct <- (test_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(test_data$power)
sd_value3 <- sd(test_data$power)
z_power <- (test_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
validation_pred <- best_model %>% predict(input_data)
print(validation_pred)
mean_power <- mean(test_data$power, na.rm = TRUE)
print(mean_power)
sd_power <- sd(test_data$power, na.rm = TRUE)
print(sd_power)
denormalized_forecast <- validation_pred * sd_power+ mean_power
length(denormalized_forecast)
denormalized_forecast <- pmax(denormalized_forecast, 0)
print(denormalized_forecast)
RMSE <- sqrt(mean((denormalized_forecast- test_data$power)^2))
print(RMSE)




# Short term forecasting using winter data (plant 4)
#LSTM model
data <- read_excel("/Users/machang/Desktop/2023/new/plant4.xlsx")
data[] <- lapply(data, as.numeric)
train_data <- data[1:1105, ] 
test_data <- data[1106:1170, ]
mean_value1 <- mean(train_data$`total radiation`)
sd_value1 <- sd(train_data$`total radiation`)
z_total <- (train_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(train_data$`direct solar radiation`)
sd_value2 <- sd(train_data$`direct solar radiation`)
z_direct <- (train_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(train_data$power)
sd_value3 <- sd(train_data$power)
z_power <- (train_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
fitness <- function(params) {
  tryCatch({
    nodes <- as.integer(params[1])  
    learning_rate <- params[2]
    model <- keras_model_sequential()
    model %>%
      layer_lstm(units = nodes, input_shape = c(1, 3)) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(lr = learning_rate)
    )
    history <- model %>% fit(
      x = input_data,
      y = z_power,
      epochs = 10,
      batch_size = 32,
      verbose = 0
    )
    train_loss <- history$metrics$loss
    return(train_loss[length(train_loss)])
  }, error = function(e) {
    message("Error in fitness function: ", conditionMessage(e))
    return(NA)  
  })
}
set.seed(123) 
lower_bounds <- c(32, 0.001)
upper_bounds <- c(128, 0.1)
bounds <- matrix(c(lower_bounds, upper_bounds), nrow = 2, byrow = TRUE)
ga_result <- ga(type = "real-valued",
                fitness = fitness,
                lower = lower_bounds,  
                upper = upper_bounds, 
                popSize = 50,    
                maxiter = 20)     
best_fitness<-min(ga_result@fitness)
best_index <- which.min(ga_result@fitness)  
best_params <- ga_result@solution[1, ]
cat("Best Parameters (corresponding to the minimum fitness value): \n")
cat("Nodes:", best_params[1], ", Learning Rate:", best_params[2], "\n")
cat("Minimum Fitness Value:", best_fitness, "\n")
best_nodes <- best_params[1]
best_learning_rate <- best_params[2]
best_model <- keras_model_sequential()
best_model %>%
  layer_lstm(units = best_nodes, input_shape = c(1, 3)) %>%
  layer_dense(units = 1)
best_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = best_learning_rate)
)
history <- best_model %>% fit(
  x = input_data,
  y = z_power,
  epochs = 10,
  batch_size = 32,
  verbose = 1
)
mean_value1 <- mean(test_data$`total radiation`)
sd_value1 <- sd(test_data$`total radiation`)
z_total <- (test_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(test_data$`direct solar radiation`)
sd_value2 <- sd(test_data$`direct solar radiation`)
z_direct <- (test_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(test_data$power)
sd_value3 <- sd(test_data$power)
z_power <- (test_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
validation_pred <- best_model %>% predict(input_data)
print(validation_pred)
mean_power <- mean(test_data$power, na.rm = TRUE)
print(mean_power)
sd_power <- sd(test_data$power, na.rm = TRUE)
print(sd_power)
denormalized_forecast <- validation_pred * sd_power+ mean_power
length(denormalized_forecast)
denormalized_forecast <- pmax(denormalized_forecast, 0)
print(denormalized_forecast)
RMSE <- sqrt(mean((denormalized_forecast- test_data$power)^2))
print(RMSE)


# Short term forecasting using spring data (plant 4)
#LSTM model
data <- read_excel("/Users/machang/Desktop/2023/new/plant4.xlsx")
data[] <- lapply(data, as.numeric)
train_data <- data[1171:2301, ] 
test_data <- data[2302:2366, ]
mean_value1 <- mean(train_data$`total radiation`)
sd_value1 <- sd(train_data$`total radiation`)
z_total <- (train_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(train_data$`direct solar radiation`)
sd_value2 <- sd(train_data$`direct solar radiation`)
z_direct <- (train_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(train_data$power)
sd_value3 <- sd(train_data$power)
z_power <- (train_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
fitness <- function(params) {
  tryCatch({
    nodes <- as.integer(params[1])  
    learning_rate <- params[2]
    model <- keras_model_sequential()
    model %>%
      layer_lstm(units = nodes, input_shape = c(1, 3)) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(lr = learning_rate)
    )
    history <- model %>% fit(
      x = input_data,
      y = z_power,
      epochs = 10,
      batch_size = 32,
      verbose = 0
    )
    train_loss <- history$metrics$loss
    return(train_loss[length(train_loss)])
  }, error = function(e) {
    message("Error in fitness function: ", conditionMessage(e))
    return(NA)  
  })
}
set.seed(123) 
lower_bounds <- c(32, 0.001)
upper_bounds <- c(128, 0.1)
bounds <- matrix(c(lower_bounds, upper_bounds), nrow = 2, byrow = TRUE)
ga_result <- ga(type = "real-valued",
                fitness = fitness,
                lower = lower_bounds,  
                upper = upper_bounds, 
                popSize = 50,    
                maxiter = 20)     
best_fitness<-min(ga_result@fitness)
best_index <- which.min(ga_result@fitness)  
best_params <- ga_result@solution[1, ]
cat("Best Parameters (corresponding to the minimum fitness value): \n")
cat("Nodes:", best_params[1], ", Learning Rate:", best_params[2], "\n")
cat("Minimum Fitness Value:", best_fitness, "\n")
best_nodes <- best_params[1]
best_learning_rate <- best_params[2]
best_model <- keras_model_sequential()
best_model %>%
  layer_lstm(units = best_nodes, input_shape = c(1, 3)) %>%
  layer_dense(units = 1)
best_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = best_learning_rate)
)
history <- best_model %>% fit(
  x = input_data,
  y = z_power,
  epochs = 10,
  batch_size = 32,
  verbose = 1
)
mean_value1 <- mean(test_data$`total radiation`)
sd_value1 <- sd(test_data$`total radiation`)
z_total <- (test_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(test_data$`direct solar radiation`)
sd_value2 <- sd(test_data$`direct solar radiation`)
z_direct <- (test_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(test_data$power)
sd_value3 <- sd(test_data$power)
z_power <- (test_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
validation_pred <- best_model %>% predict(input_data)
print(validation_pred)
mean_power <- mean(test_data$power, na.rm = TRUE)
print(mean_power)
sd_power <- sd(test_data$power, na.rm = TRUE)
print(sd_power)
denormalized_forecast <- validation_pred * sd_power+ mean_power
length(denormalized_forecast)
denormalized_forecast <- pmax(denormalized_forecast, 0)
print(denormalized_forecast)
RMSE <- sqrt(mean((denormalized_forecast- test_data$power)^2))
print(RMSE)



# Short term forecasting using summer data (plant 4)
#LSTM model
data <- read_excel("/Users/machang/Desktop/2023/new/plant4.xlsx")
data[] <- lapply(data, as.numeric)
train_data <- data[2367:3497, ] 
test_data <- data[3498:3562, ]
mean_value1 <- mean(train_data$`total radiation`)
sd_value1 <- sd(train_data$`total radiation`)
z_total <- (train_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(train_data$`direct solar radiation`)
sd_value2 <- sd(train_data$`direct solar radiation`)
z_direct <- (train_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(train_data$power)
sd_value3 <- sd(train_data$power)
z_power <- (train_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
fitness <- function(params) {
  tryCatch({
    nodes <- as.integer(params[1])  
    learning_rate <- params[2]
    model <- keras_model_sequential()
    model %>%
      layer_lstm(units = nodes, input_shape = c(1, 3)) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(lr = learning_rate)
    )
    history <- model %>% fit(
      x = input_data,
      y = z_power,
      epochs = 10,
      batch_size = 32,
      verbose = 0
    )
    train_loss <- history$metrics$loss
    return(train_loss[length(train_loss)])
  }, error = function(e) {
    message("Error in fitness function: ", conditionMessage(e))
    return(NA)  
  })
}
set.seed(123) 
lower_bounds <- c(32, 0.001)
upper_bounds <- c(128, 0.1)
bounds <- matrix(c(lower_bounds, upper_bounds), nrow = 2, byrow = TRUE)
ga_result <- ga(type = "real-valued",
                fitness = fitness,
                lower = lower_bounds,  
                upper = upper_bounds, 
                popSize = 50,    
                maxiter = 20)     
best_fitness<-min(ga_result@fitness)
best_index <- which.min(ga_result@fitness)  
best_params <- ga_result@solution[1, ]
cat("Best Parameters (corresponding to the minimum fitness value): \n")
cat("Nodes:", best_params[1], ", Learning Rate:", best_params[2], "\n")
cat("Minimum Fitness Value:", best_fitness, "\n")
best_nodes <- best_params[1]
best_learning_rate <- best_params[2]
best_model <- keras_model_sequential()
best_model %>%
  layer_lstm(units = best_nodes, input_shape = c(1, 3)) %>%
  layer_dense(units = 1)
best_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = best_learning_rate)
)
history <- best_model %>% fit(
  x = input_data,
  y = z_power,
  epochs = 10,
  batch_size = 32,
  verbose = 1
)
mean_value1 <- mean(test_data$`total radiation`)
sd_value1 <- sd(test_data$`total radiation`)
z_total <- (test_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(test_data$`direct solar radiation`)
sd_value2 <- sd(test_data$`direct solar radiation`)
z_direct <- (test_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(test_data$power)
sd_value3 <- sd(test_data$power)
z_power <- (test_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
validation_pred <- best_model %>% predict(input_data)
print(validation_pred)
mean_power <- mean(test_data$power, na.rm = TRUE)
print(mean_power)
sd_power <- sd(test_data$power, na.rm = TRUE)
print(sd_power)
denormalized_forecast <- validation_pred * sd_power+ mean_power
length(denormalized_forecast)
denormalized_forecast <- pmax(denormalized_forecast, 0)
print(denormalized_forecast)
RMSE <- sqrt(mean((denormalized_forecast- test_data$power)^2))
print(RMSE)





# Short term forecasting using autumn data (plant 4)
#LSTM model
data <- read_excel("/Users/machang/Desktop/2023/new/plant4.xlsx")
data[] <- lapply(data, as.numeric)
train_data <- data[3563:4680, ] 
test_data <- data[4681:4745, ]
mean_value1 <- mean(train_data$`total radiation`)
sd_value1 <- sd(train_data$`total radiation`)
z_total <- (train_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(train_data$`direct solar radiation`)
sd_value2 <- sd(train_data$`direct solar radiation`)
z_direct <- (train_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(train_data$power)
sd_value3 <- sd(train_data$power)
z_power <- (train_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
fitness <- function(params) {
  tryCatch({
    nodes <- as.integer(params[1])  
    learning_rate <- params[2]
    model <- keras_model_sequential()
    model %>%
      layer_lstm(units = nodes, input_shape = c(1, 3)) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(lr = learning_rate)
    )
    history <- model %>% fit(
      x = input_data,
      y = z_power,
      epochs = 10,
      batch_size = 32,
      verbose = 0
    )
    train_loss <- history$metrics$loss
    return(train_loss[length(train_loss)])
  }, error = function(e) {
    message("Error in fitness function: ", conditionMessage(e))
    return(NA)  
  })
}
set.seed(123) 
lower_bounds <- c(32, 0.001)
upper_bounds <- c(128, 0.1)
bounds <- matrix(c(lower_bounds, upper_bounds), nrow = 2, byrow = TRUE)
ga_result <- ga(type = "real-valued",
                fitness = fitness,
                lower = lower_bounds,  
                upper = upper_bounds, 
                popSize = 50,    
                maxiter = 20)     
best_fitness<-min(ga_result@fitness)
best_index <- which.min(ga_result@fitness)  
best_params <- ga_result@solution[1, ]
cat("Best Parameters (corresponding to the minimum fitness value): \n")
cat("Nodes:", best_params[1], ", Learning Rate:", best_params[2], "\n")
cat("Minimum Fitness Value:", best_fitness, "\n")
best_nodes <- best_params[1]
best_learning_rate <- best_params[2]
best_model <- keras_model_sequential()
best_model %>%
  layer_lstm(units = best_nodes, input_shape = c(1, 3)) %>%
  layer_dense(units = 1)
best_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = best_learning_rate)
)
history <- best_model %>% fit(
  x = input_data,
  y = z_power,
  epochs = 10,
  batch_size = 32,
  verbose = 1
)
mean_value1 <- mean(test_data$`total radiation`)
sd_value1 <- sd(test_data$`total radiation`)
z_total <- (test_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(test_data$`direct solar radiation`)
sd_value2 <- sd(test_data$`direct solar radiation`)
z_direct <- (test_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(test_data$power)
sd_value3 <- sd(test_data$power)
z_power <- (test_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
validation_pred <- best_model %>% predict(input_data)
print(validation_pred)
mean_power <- mean(test_data$power, na.rm = TRUE)
print(mean_power)
sd_power <- sd(test_data$power, na.rm = TRUE)
print(sd_power)
denormalized_forecast <- validation_pred * sd_power+ mean_power
length(denormalized_forecast)
denormalized_forecast <- pmax(denormalized_forecast, 0)
print(denormalized_forecast)
RMSE <- sqrt(mean((denormalized_forecast- test_data$power)^2))
print(RMSE)




# Short term forecasting using winter data (plant 5)
#LSTM model
data <- read_excel("/Users/machang/Desktop/2023/new/plant5.xlsx")
data[] <- lapply(data, as.numeric)
train_data <- data[1:1105, ] 
test_data <- data[1106:1170, ]
mean_value1 <- mean(train_data$`total radiation`)
sd_value1 <- sd(train_data$`total radiation`)
z_total <- (train_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(train_data$`direct solar radiation`)
sd_value2 <- sd(train_data$`direct solar radiation`)
z_direct <- (train_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(train_data$power)
sd_value3 <- sd(train_data$power)
z_power <- (train_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
fitness <- function(params) {
  tryCatch({
    nodes <- as.integer(params[1])  
    learning_rate <- params[2]
    model <- keras_model_sequential()
    model %>%
      layer_lstm(units = nodes, input_shape = c(1, 3)) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(lr = learning_rate)
    )
    history <- model %>% fit(
      x = input_data,
      y = z_power,
      epochs = 10,
      batch_size = 32,
      verbose = 0
    )
    train_loss <- history$metrics$loss
    return(train_loss[length(train_loss)])
  }, error = function(e) {
    message("Error in fitness function: ", conditionMessage(e))
    return(NA)  
  })
}
set.seed(123) 
lower_bounds <- c(32, 0.001)
upper_bounds <- c(128, 0.1)
bounds <- matrix(c(lower_bounds, upper_bounds), nrow = 2, byrow = TRUE)
ga_result <- ga(type = "real-valued",
                fitness = fitness,
                lower = lower_bounds,  
                upper = upper_bounds, 
                popSize = 50,    
                maxiter = 20)     
best_fitness<-min(ga_result@fitness)
best_index <- which.min(ga_result@fitness)  
best_params <- ga_result@solution[1, ]
cat("Best Parameters (corresponding to the minimum fitness value): \n")
cat("Nodes:", best_params[1], ", Learning Rate:", best_params[2], "\n")
cat("Minimum Fitness Value:", best_fitness, "\n")
best_nodes <- best_params[1]
best_learning_rate <- best_params[2]
best_model <- keras_model_sequential()
best_model %>%
  layer_lstm(units = best_nodes, input_shape = c(1, 3)) %>%
  layer_dense(units = 1)
best_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = best_learning_rate)
)
history <- best_model %>% fit(
  x = input_data,
  y = z_power,
  epochs = 10,
  batch_size = 32,
  verbose = 1
)
mean_value1 <- mean(test_data$`total radiation`)
sd_value1 <- sd(test_data$`total radiation`)
z_total <- (test_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(test_data$`direct solar radiation`)
sd_value2 <- sd(test_data$`direct solar radiation`)
z_direct <- (test_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(test_data$power)
sd_value3 <- sd(test_data$power)
z_power <- (test_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
validation_pred <- best_model %>% predict(input_data)
print(validation_pred)
mean_power <- mean(test_data$power, na.rm = TRUE)
print(mean_power)
sd_power <- sd(test_data$power, na.rm = TRUE)
print(sd_power)
denormalized_forecast <- validation_pred * sd_power+ mean_power
length(denormalized_forecast)
denormalized_forecast <- pmax(denormalized_forecast, 0)
print(denormalized_forecast)
RMSE <- sqrt(mean((denormalized_forecast- test_data$power)^2))
print(RMSE)


# Short term forecasting using spring data (plant 5)
#LSTM model
data <- read_excel("/Users/machang/Desktop/2023/new/plant5.xlsx")
data[] <- lapply(data, as.numeric)
train_data <- data[1171:2301, ] 
test_data <- data[2302:2366, ]
mean_value1 <- mean(train_data$`total radiation`)
sd_value1 <- sd(train_data$`total radiation`)
z_total <- (train_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(train_data$`direct solar radiation`)
sd_value2 <- sd(train_data$`direct solar radiation`)
z_direct <- (train_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(train_data$power)
sd_value3 <- sd(train_data$power)
z_power <- (train_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
fitness <- function(params) {
  tryCatch({
    nodes <- as.integer(params[1])  
    learning_rate <- params[2]
    model <- keras_model_sequential()
    model %>%
      layer_lstm(units = nodes, input_shape = c(1, 3)) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(lr = learning_rate)
    )
    history <- model %>% fit(
      x = input_data,
      y = z_power,
      epochs = 10,
      batch_size = 32,
      verbose = 0
    )
    train_loss <- history$metrics$loss
    return(train_loss[length(train_loss)])
  }, error = function(e) {
    message("Error in fitness function: ", conditionMessage(e))
    return(NA)  
  })
}
set.seed(123) 
lower_bounds <- c(32, 0.001)
upper_bounds <- c(128, 0.1)
bounds <- matrix(c(lower_bounds, upper_bounds), nrow = 2, byrow = TRUE)
ga_result <- ga(type = "real-valued",
                fitness = fitness,
                lower = lower_bounds,  
                upper = upper_bounds, 
                popSize = 50,    
                maxiter = 20)     
best_fitness<-min(ga_result@fitness)
best_index <- which.min(ga_result@fitness)  
best_params <- ga_result@solution[1, ]
cat("Best Parameters (corresponding to the minimum fitness value): \n")
cat("Nodes:", best_params[1], ", Learning Rate:", best_params[2], "\n")
cat("Minimum Fitness Value:", best_fitness, "\n")
best_nodes <- best_params[1]
best_learning_rate <- best_params[2]
best_model <- keras_model_sequential()
best_model %>%
  layer_lstm(units = best_nodes, input_shape = c(1, 3)) %>%
  layer_dense(units = 1)
best_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = best_learning_rate)
)
history <- best_model %>% fit(
  x = input_data,
  y = z_power,
  epochs = 10,
  batch_size = 32,
  verbose = 1
)
mean_value1 <- mean(test_data$`total radiation`)
sd_value1 <- sd(test_data$`total radiation`)
z_total <- (test_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(test_data$`direct solar radiation`)
sd_value2 <- sd(test_data$`direct solar radiation`)
z_direct <- (test_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(test_data$power)
sd_value3 <- sd(test_data$power)
z_power <- (test_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
validation_pred <- best_model %>% predict(input_data)
print(validation_pred)
mean_power <- mean(test_data$power, na.rm = TRUE)
print(mean_power)
sd_power <- sd(test_data$power, na.rm = TRUE)
print(sd_power)
denormalized_forecast <- validation_pred * sd_power+ mean_power
length(denormalized_forecast)
denormalized_forecast <- pmax(denormalized_forecast, 0)
print(denormalized_forecast)
RMSE <- sqrt(mean((denormalized_forecast- test_data$power)^2))
print(RMSE)



# Short term forecasting using summer data (plant 5)
#LSTM model
data <- read_excel("/Users/machang/Desktop/2023/new/plant5.xlsx")
data[] <- lapply(data, as.numeric)
train_data <- data[2367:3497, ] 
test_data <- data[3498:3562, ]
mean_value1 <- mean(train_data$`total radiation`)
sd_value1 <- sd(train_data$`total radiation`)
z_total <- (train_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(train_data$`direct solar radiation`)
sd_value2 <- sd(train_data$`direct solar radiation`)
z_direct <- (train_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(train_data$power)
sd_value3 <- sd(train_data$power)
z_power <- (train_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
fitness <- function(params) {
  tryCatch({
    nodes <- as.integer(params[1])  
    learning_rate <- params[2]
    model <- keras_model_sequential()
    model %>%
      layer_lstm(units = nodes, input_shape = c(1, 3)) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(lr = learning_rate)
    )
    history <- model %>% fit(
      x = input_data,
      y = z_power,
      epochs = 10,
      batch_size = 32,
      verbose = 0
    )
    train_loss <- history$metrics$loss
    return(train_loss[length(train_loss)])
  }, error = function(e) {
    message("Error in fitness function: ", conditionMessage(e))
    return(NA)  
  })
}
set.seed(123) 
lower_bounds <- c(32, 0.001)
upper_bounds <- c(128, 0.1)
bounds <- matrix(c(lower_bounds, upper_bounds), nrow = 2, byrow = TRUE)
ga_result <- ga(type = "real-valued",
                fitness = fitness,
                lower = lower_bounds,  
                upper = upper_bounds, 
                popSize = 50,    
                maxiter = 20)     
best_fitness<-min(ga_result@fitness)
best_index <- which.min(ga_result@fitness)  
best_params <- ga_result@solution[1, ]
cat("Best Parameters (corresponding to the minimum fitness value): \n")
cat("Nodes:", best_params[1], ", Learning Rate:", best_params[2], "\n")
cat("Minimum Fitness Value:", best_fitness, "\n")
best_nodes <- best_params[1]
best_learning_rate <- best_params[2]
best_model <- keras_model_sequential()
best_model %>%
  layer_lstm(units = best_nodes, input_shape = c(1, 3)) %>%
  layer_dense(units = 1)
best_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = best_learning_rate)
)
history <- best_model %>% fit(
  x = input_data,
  y = z_power,
  epochs = 10,
  batch_size = 32,
  verbose = 1
)
mean_value1 <- mean(test_data$`total radiation`)
sd_value1 <- sd(test_data$`total radiation`)
z_total <- (test_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(test_data$`direct solar radiation`)
sd_value2 <- sd(test_data$`direct solar radiation`)
z_direct <- (test_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(test_data$power)
sd_value3 <- sd(test_data$power)
z_power <- (test_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
validation_pred <- best_model %>% predict(input_data)
print(validation_pred)
mean_power <- mean(test_data$power, na.rm = TRUE)
print(mean_power)
sd_power <- sd(test_data$power, na.rm = TRUE)
print(sd_power)
denormalized_forecast <- validation_pred * sd_power+ mean_power
length(denormalized_forecast)
denormalized_forecast <- pmax(denormalized_forecast, 0)
print(denormalized_forecast)
RMSE <- sqrt(mean((denormalized_forecast- test_data$power)^2))
print(RMSE)





# Short term forecasting using autumn data (plant 5)
#LSTM model
data <- read_excel("/Users/machang/Desktop/2023/new/plant5.xlsx")
data[] <- lapply(data, as.numeric)
train_data <- data[3563:4680, ] 
test_data <- data[4681:4745, ]

mean_value1 <- mean(train_data$`total radiation`)
sd_value1 <- sd(train_data$`total radiation`)
z_total <- (train_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(train_data$`direct solar radiation`)
sd_value2 <- sd(train_data$`direct solar radiation`)
z_direct <- (train_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(train_data$power)
sd_value3 <- sd(train_data$power)
z_power <- (train_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
fitness <- function(params) {
  tryCatch({
    nodes <- as.integer(params[1])  
    learning_rate <- params[2]
    model <- keras_model_sequential()
    model %>%
      layer_lstm(units = nodes, input_shape = c(1, 3)) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(lr = learning_rate)
    )
    history <- model %>% fit(
      x = input_data,
      y = z_power,
      epochs = 10,
      batch_size = 32,
      verbose = 0
    )
    train_loss <- history$metrics$loss
    return(train_loss[length(train_loss)])
  }, error = function(e) {
    message("Error in fitness function: ", conditionMessage(e))
    return(NA)  
  })
}
set.seed(123) 
lower_bounds <- c(32, 0.001)
upper_bounds <- c(128, 0.1)
bounds <- matrix(c(lower_bounds, upper_bounds), nrow = 2, byrow = TRUE)
ga_result <- ga(type = "real-valued",
                fitness = fitness,
                lower = lower_bounds,  
                upper = upper_bounds, 
                popSize = 50,    
                maxiter = 20)     
best_fitness<-min(ga_result@fitness)
best_index <- which.min(ga_result@fitness)  
best_params <- ga_result@solution[1, ]
cat("Best Parameters (corresponding to the minimum fitness value): \n")
cat("Nodes:", best_params[1], ", Learning Rate:", best_params[2], "\n")
cat("Minimum Fitness Value:", best_fitness, "\n")
best_nodes <- best_params[1]
best_learning_rate <- best_params[2]
best_model <- keras_model_sequential()
best_model %>%
  layer_lstm(units = best_nodes, input_shape = c(1, 3)) %>%
  layer_dense(units = 1)
best_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = best_learning_rate)
)
history <- best_model %>% fit(
  x = input_data,
  y = z_power,
  epochs = 10,
  batch_size = 32,
  verbose = 1
)
mean_value1 <- mean(test_data$`total radiation`)
sd_value1 <- sd(test_data$`total radiation`)
z_total <- (test_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(test_data$`direct solar radiation`)
sd_value2 <- sd(test_data$`direct solar radiation`)
z_direct <- (test_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(test_data$power)
sd_value3 <- sd(test_data$power)
z_power <- (test_data$power - mean_value3) / sd_value3
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))
validation_pred <- best_model %>% predict(input_data)
print(validation_pred)
mean_power <- mean(test_data$power, na.rm = TRUE)
print(mean_power)
sd_power <- sd(test_data$power, na.rm = TRUE)
print(sd_power)
denormalized_forecast <- validation_pred * sd_power+ mean_power
length(denormalized_forecast)
denormalized_forecast <- pmax(denormalized_forecast, 0)
print(denormalized_forecast)
RMSE <- sqrt(mean((denormalized_forecast- test_data$power)^2))
print(RMSE)

