# 2-step procedure
Forecasting the performances of photovoltaic power plants in Jiangsu(China) using a new hybrid modelling approach
## Example
Note: The proposed model and LSTM model both require the installation of the following R packages: glmnet, readxl, keras, and GA before they can be used. The smart persistence model need to load the 'suncalc' package and call the Python 'PVLIB' package in R via the 'reticulate' package.

Here, the winter data of plant 1 is used for ultra-short term forecasting as an example to illustrate the three models used in the paper.
### Example 1. Proposed model
```r
source('code of proposed model.R')

# Data source
data <- read_excel("/Users/machang/Desktop/2023/new/plant1.xlsx")
data[] <- lapply(data, as.numeric)
train_data <- data[1:1157, ] 
test_data <- data[1158:1170, ]

# It is classified according to the real output power
category0 <- train_data[train_data$power == 0, ]
category1 <- train_data[train_data$power != 0, ]
new_data <- category1
mean_value1 <- mean(new_data$`total radiation`)
sd_value1 <- sd(new_data$`total radiation`)
z_total <- (new_data$`total radiation` - mean_value1) / sd_value1
mean_value2 <- mean(new_data$`direct solar radiation`)
sd_value2 <- sd(new_data$`direct solar radiation`)
z_direct <- (new_data$`direct solar radiation` - mean_value2) / sd_value2
mean_value3 <- mean(new_data$power)
sd_value3 <- sd(new_data$power)
z_power <- (new_data$power - mean_value3) / sd_value3

#Integrate data belonging to category 1 into new data
input_data <- array(c(z_total, z_direct, z_power), dim = c(length(z_total), 1, 3))

#Define a fitness function used for optimizing a model
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

# Genetic algorithm is used to optimize two hyperparameters of the LSTM model: the number of nodes in the LSTM layer and the learning rate
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
best_fitness <- min(ga_result@fitness) 
best_index <- which.min(ga_result@fitness) 
best_params <- ga_result@solution[1, ]  
cat("Best Parameters (corresponding to the minimum fitness value): \n")
cat("Nodes:", best_params[1], ", Learning Rate:", best_params[2], "\n")
cat("Minimum Fitness Value:", best_fitness, "\n")

#Extract best parameters
best_nodes <- best_params[1]
best_learning_rate <- best_params[2]

#Define the best model
best_model <- keras_model_sequential()

#Compile the model
best_model %>%
  layer_lstm(units = best_nodes, input_shape = c(1, 3)) %>%
  layer_dense(units = 1)
best_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = best_learning_rate)
)

#Train the model
history <- best_model %>% fit(
  x = input_data,
  y = z_power,
  epochs = 10,
  batch_size = 32,
  verbose = 1
)

#Fitting the logistic regression model and recording accuracy, precision, recall rate
xreg_data <- train_data[, c("direct solar radiation")]
y <- ifelse(train_data$power == 0, 0, 1)  
log_reg_model <- glm(y ~ ., data = xreg_data, family = "binomial")
pred_train <- ifelse(predict(log_reg_model, type = "response") > 0.5, 1, 0)
accuracy_train <- mean(pred_train == y)
print(accuracy_train)
conf_matrix <- table(y, pred_train)
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
print(precision)
recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
print(recall)

#Making predictions
xreg_data <- test_data[, c("direct solar radiation")]
y <- ifelse(test_data$power == 0, 0, 1)  
log_reg_model <- glm(y ~ ., data = xreg_data, family = "binomial")
pred_test <- ifelse(predict(log_reg_model, type = "response") > 0.5, 1, 0)
num_class_0 <- sum(pred_test == 0)
num_class_1 <- sum(pred_test == 1)
print(num_class_0)
print(num_class_1)
index_class_0 <- which(pred_test == 0)
index_class_1 <- which(pred_test == 1)
print(index_class_0)
print(index_class_1)
accuracy_test <- mean(pred_test == y)
print(accuracy_test)
new_data1 <- test_data %>% 
  filter(predict(log_reg_model, newdata1 = ., type = "response") > 0.5)
nrow(new_data1)
mean_value1<-mean(new_data1$`total radiation`)
sd_value1 <-sd(new_data1$`total radiation`)
z_total <-(new_data1$`total radiation`-mean_value1)/sd_value1
mean_value2<-mean(new_data1$`direct solar radiation`)
sd_value2 <-sd(new_data1$`direct solar radiation`)
z_direct <-(new_data1$`direct solar radiation`-mean_value2)/sd_value2
mean_value3<-mean(new_data1$power)
sd_value3 <-sd(new_data1$power)
z_power <-(new_data1$power-mean_value3)/sd_value3
z_power_3d <- array_reshape(z_power, c(length(z_power), 1, 1))
test_input <- array(c(z_total, z_direct,z_power), dim = c(length(z_total), 1, 3)) 

#Using the best LSTM model for forecasting in category 1
validation_pred <- best_model %>% predict(test_input)
print(validation_pred)
mean_power <- mean(test_data$power, na.rm = TRUE)
print(mean_power)
sd_power <- sd(test_data$power, na.rm = TRUE)
print(sd_power)
denormalized_forecast <- validation_pred * sd_power+ mean_power
print(denormalized_forecast)
denormalized_forecast <- pmax(denormalized_forecast, 0)
print(denormalized_forecast)
new_forecast <- rep(0, nrow(test_data))

#Combine the predicted values from category 0 and category 1
new_forecast[index_class_1] <- denormalized_forecast
print(new_forecast)

#RMSE calculation
RMSE <- sqrt(mean((new_forecast- test_data$power)^2))
print(RMSE)
```
### Example 2. LSTM model
```r
source('code of lstm model.R')
# It is similar to the proposed model except that the classification step is missing
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
```
### Example 3. Smart persistence model
Note: Both zenith Angle and GHI data under clear sky conditions are the physical information needed to calculate the smart persistence model. Zenith Angle information is obtained using the R 'suncalc' package, GHI data under clear sky conditions by calling the Python 'PVLIB' package in R via the 'reticulate' package.
```r
source('GHI and Zenith Angle.R')
#1.Obtain these two physicl information first
#Latitude and longitude of plant 1
latitude <- 32.7816  
longitude <- 119.459
# Date we need
start_date <- "2023-02-28"  
url <- paste0("https://power.larc.nasa.gov/api/temporal/hourly/point?parameters=ALLSKY_SFC_SW_DWN&community=RE&latitude=", 
              latitude, "&longitude=", longitude, "&start=", gsub("-", "", start_date), "&end=", gsub("-", "", start_date), "&format=JSON")
response <- GET(url)
data <- content(response, "parsed")
ghi_data <- data$properties$parameter$ALLSKY_SFC_SW_DWN
filtered_ghi <- ghi_data[as.numeric(substr(names(ghi_data), 9, 10)) >= 6 & as.numeric(substr(names(ghi_data), 9, 10)) <= 18]
time_labels <- names(filtered_ghi)
hourly_times <- as.POSIXct(paste(start_date, substr(time_labels, 9, 10), "00:00"), tz = "UTC")  
zenith_angles <- sapply(hourly_times, function(time) {
  sun_position <- getSunlightPosition(date = time, lat = latitude, lon = longitude)
  90 - sun_position$altitude * (180 / pi)  
})
result_df <- data.frame(
  Time = time_labels,
  GHI = unlist(filtered_ghi),
  ZenithAngle = zenith_angles
)
print(result_df)
print(result_df$GHI)
print(result_df$ZenithAngle)


#2. Only after obtaining the physical information of the above two can a smart persistence model be trained
source('code of smart persistence model.R')

# The resulting physical information is put into the original data set for easy training
data <- read_excel("/Users/machang/Desktop/2023/new/plant1.xlsx")
data[] <- lapply(data, as.numeric)  
train_data <- data[1:1170, ]  
test_data <- data[1158:1170, ]  
cos_zenith_train <- cos(train_data$`zenith angle`)
cos_zenith_train[cos_zenith_train < 0] <- 0

# Calculate Global Horizontal Irradiance for training set
ghi_train <- train_data$`direct solar radiation` + 
  train_data$`scattered radiation` * cos_zenith_train
ghi_clear_sky_train <- train_data$`GHI clear shy`
power_train <- train_data$power

# Calculate Scaling Constant
C <- max(power_train, na.rm = TRUE) / max(ghi_clear_sky_train, na.rm = TRUE)
print(C)
cos_zenith_test <- cos(test_data$`zenith angle`)
cos_zenith_test[cos_zenith_test < 0] <- 0
ghi_test_direct <- test_data$`direct solar radiation`
ghi_test_scattered <- test_data$`scattered radiation` * cos_zenith_test
ghi_test <- ghi_test_direct + ghi_test_scattered
print(ghi_test_direct)
print(ghi_test_scattered)
print(ghi_test)

#Calculate Clearness Index
ghi_test_clear_sky <-test_data$`GHI clear shy`
Kt_test <- ghi_test / ghi_test_clear_sky
Kt_test[is.na(Kt_test) | is.infinite(Kt_test)] <- 0

# Formula of smart peristence model
predicted_power <- C * Kt_test * ghi_test_clear_sky
print("Predicted Power:")
print(predicted_power)

```
