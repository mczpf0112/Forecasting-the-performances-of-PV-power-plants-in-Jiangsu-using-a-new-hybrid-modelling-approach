#plant1
library(dplyr)
library(readxl)
data <- read_excel("/Users/machang/Desktop/2023/new/plant1.xlsx")
data[] <- lapply(data, as.numeric)  
train_data <- data[1:1170, ]  
test_data <- data[1158:1170, ]  
cos_zenith_train <- cos(train_data$`zenith angle`)
cos_zenith_train[cos_zenith_train < 0] <- 0
ghi_train <- train_data$`direct solar radiation` + 
  train_data$`scattered radiation` * cos_zenith_train
ghi_clear_sky_train <- train_data$`GHI clear shy`
power_train <- train_data$power
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
ghi_test_clear_sky <-test_data$`GHI clear shy`
Kt_test <- ghi_test / ghi_test_clear_sky
Kt_test[is.na(Kt_test) | is.infinite(Kt_test)] <- 0  
predicted_power <- C * Kt_test * ghi_test_clear_sky
print("Predicted Power:")
print(predicted_power)




library(dplyr)
library(readxl)
data <- read_excel("/Users/machang/Desktop/2023/new/plant1.xlsx")
data[] <- lapply(data, as.numeric)  
train_data <- data[1171:2353, ]  
test_data <- data[2354:2366, ]  
cos_zenith_train <- cos(train_data$`zenith angle`)
cos_zenith_train[cos_zenith_train < 0] <- 0
ghi_train <- train_data$`direct solar radiation` + 
  train_data$`scattered radiation` * cos_zenith_train
ghi_clear_sky_train <- train_data$`GHI clear shy`
power_train <- train_data$power
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
ghi_test_clear_sky <-test_data$`GHI clear shy`
Kt_test <- ghi_test / ghi_test_clear_sky
Kt_test[is.na(Kt_test) | is.infinite(Kt_test)] <- 0  
predicted_power <- C * Kt_test * ghi_test_clear_sky
print("Predicted Power:")
print(predicted_power)



library(dplyr)
library(readxl)
data <- read_excel("/Users/machang/Desktop/2023/new/plant1.xlsx")
data[] <- lapply(data, as.numeric)  
train_data <- data[2367:3549, ]  
test_data <- data[3550:3562, ]  
cos_zenith_train <- cos(train_data$`zenith angle`)
cos_zenith_train[cos_zenith_train < 0] <- 0
ghi_train <- train_data$`direct solar radiation` + 
  train_data$`scattered radiation` * cos_zenith_train
ghi_clear_sky_train <- train_data$`GHI clear shy`
power_train <- train_data$power
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
ghi_test_clear_sky <-test_data$`GHI clear shy`
Kt_test <- ghi_test / ghi_test_clear_sky
Kt_test[is.na(Kt_test) | is.infinite(Kt_test)] <- 0  
predicted_power <- C * Kt_test * ghi_test_clear_sky
print("Predicted Power:")
print(predicted_power)


library(dplyr)
library(readxl)
data <- read_excel("/Users/machang/Desktop/2023/new/plant1.xlsx")
data[] <- lapply(data, as.numeric)  
train_data <- data[3563:4732, ]  
test_data <- data[4733:4745, ]  
cos_zenith_train <- cos(train_data$`zenith angle`)
cos_zenith_train[cos_zenith_train < 0] <- 0
ghi_train <- train_data$`direct solar radiation` + 
  train_data$`scattered radiation` * cos_zenith_train
ghi_clear_sky_train <- train_data$`GHI clear shy`
power_train <- train_data$power
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
ghi_test_clear_sky <-test_data$`GHI clear shy`
Kt_test <- ghi_test / ghi_test_clear_sky
Kt_test[is.na(Kt_test) | is.infinite(Kt_test)] <- 0  
predicted_power <- C * Kt_test * ghi_test_clear_sky
print("Predicted Power:")
print(predicted_power)

#plant2
library(dplyr)
library(readxl)
data <- read_excel("/Users/machang/Desktop/2023/new/plant2.xlsx")
data[] <- lapply(data, as.numeric)  
train_data <- data[1:1170, ]  
test_data <- data[1158:1170, ]  
cos_zenith_train <- cos(train_data$`zenith angle`)
cos_zenith_train[cos_zenith_train < 0] <- 0
ghi_train <- train_data$`direct solar radiation` + 
  train_data$`scattered radiation` * cos_zenith_train
ghi_clear_sky_train <- train_data$`GHI clear shy`
power_train <- train_data$power
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
ghi_test_clear_sky <-test_data$`GHI clear shy`
Kt_test <- ghi_test / ghi_test_clear_sky
Kt_test[is.na(Kt_test) | is.infinite(Kt_test)] <- 0  
predicted_power <- C * Kt_test * ghi_test_clear_sky
print("Predicted Power:")
print(predicted_power)




library(dplyr)
library(readxl)
data <- read_excel("/Users/machang/Desktop/2023/new/plant2.xlsx")
data[] <- lapply(data, as.numeric)  
train_data <- data[1171:2353, ]  
test_data <- data[2354:2366, ]  
cos_zenith_train <- cos(train_data$`zenith angle`)
cos_zenith_train[cos_zenith_train < 0] <- 0
ghi_train <- train_data$`direct solar radiation` + 
  train_data$`scattered radiation` * cos_zenith_train
ghi_clear_sky_train <- train_data$`GHI clear shy`
power_train <- train_data$power
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
ghi_test_clear_sky <-test_data$`GHI clear shy`
Kt_test <- ghi_test / ghi_test_clear_sky
Kt_test[is.na(Kt_test) | is.infinite(Kt_test)] <- 0  
predicted_power <- C * Kt_test * ghi_test_clear_sky
print("Predicted Power:")
print(predicted_power)



library(dplyr)
library(readxl)
data <- read_excel("/Users/machang/Desktop/2023/new/plant2.xlsx")
data[] <- lapply(data, as.numeric)  
train_data <- data[2367:3549, ]  
test_data <- data[3550:3562, ]  
cos_zenith_train <- cos(train_data$`zenith angle`)
cos_zenith_train[cos_zenith_train < 0] <- 0
ghi_train <- train_data$`direct solar radiation` + 
  train_data$`scattered radiation` * cos_zenith_train
ghi_clear_sky_train <- train_data$`GHI clear shy`
power_train <- train_data$power
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
ghi_test_clear_sky <-test_data$`GHI clear shy`
Kt_test <- ghi_test / ghi_test_clear_sky
Kt_test[is.na(Kt_test) | is.infinite(Kt_test)] <- 0  
predicted_power <- C * Kt_test * ghi_test_clear_sky
print("Predicted Power:")
print(predicted_power)


library(dplyr)
library(readxl)
data <- read_excel("/Users/machang/Desktop/2023/new/plant2.xlsx")
data[] <- lapply(data, as.numeric)  
train_data <- data[3563:4732, ]  
test_data <- data[4733:4745, ]  
cos_zenith_train <- cos(train_data$`zenith angle`)
cos_zenith_train[cos_zenith_train < 0] <- 0
ghi_train <- train_data$`direct solar radiation` + 
  train_data$`scattered radiation` * cos_zenith_train
ghi_clear_sky_train <- train_data$`GHI clear shy`
power_train <- train_data$power
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
ghi_test_clear_sky <-test_data$`GHI clear shy`
Kt_test <- ghi_test / ghi_test_clear_sky
Kt_test[is.na(Kt_test) | is.infinite(Kt_test)] <- 0  
predicted_power <- C * Kt_test * ghi_test_clear_sky
print("Predicted Power:")
print(predicted_power)



#plant3
library(dplyr)
library(readxl)
data <- read_excel("/Users/machang/Desktop/2023/new/plant3.xlsx")
data[] <- lapply(data, as.numeric)  
train_data <- data[1:1170, ]  
test_data <- data[1158:1170, ]  
cos_zenith_train <- cos(train_data$`zenith angle`)
cos_zenith_train[cos_zenith_train < 0] <- 0
ghi_train <- train_data$`direct solar radiation` + 
  train_data$`scattered radiation` * cos_zenith_train
ghi_clear_sky_train <- train_data$`GHI clear shy`
power_train <- train_data$power
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
ghi_test_clear_sky <-test_data$`GHI clear shy`
Kt_test <- ghi_test / ghi_test_clear_sky
Kt_test[is.na(Kt_test) | is.infinite(Kt_test)] <- 0  
predicted_power <- C * Kt_test * ghi_test_clear_sky
print("Predicted Power:")
print(predicted_power)




library(dplyr)
library(readxl)
data <- read_excel("/Users/machang/Desktop/2023/new/plant3.xlsx")
data[] <- lapply(data, as.numeric)  
train_data <- data[1171:2353, ]  
test_data <- data[2354:2366, ]  
cos_zenith_train <- cos(train_data$`zenith angle`)
cos_zenith_train[cos_zenith_train < 0] <- 0
ghi_train <- train_data$`direct solar radiation` + 
  train_data$`scattered radiation` * cos_zenith_train
ghi_clear_sky_train <- train_data$`GHI clear shy`
power_train <- train_data$power
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
ghi_test_clear_sky <-test_data$`GHI clear shy`
Kt_test <- ghi_test / ghi_test_clear_sky
Kt_test[is.na(Kt_test) | is.infinite(Kt_test)] <- 0  
predicted_power <- C * Kt_test * ghi_test_clear_sky
print("Predicted Power:")
print(predicted_power)



library(dplyr)
library(readxl)
data <- read_excel("/Users/machang/Desktop/2023/new/plant3.xlsx")
data[] <- lapply(data, as.numeric)  
train_data <- data[2367:3549, ]  
test_data <- data[3550:3562, ]  
cos_zenith_train <- cos(train_data$`zenith angle`)
cos_zenith_train[cos_zenith_train < 0] <- 0
ghi_train <- train_data$`direct solar radiation` + 
  train_data$`scattered radiation` * cos_zenith_train
ghi_clear_sky_train <- train_data$`GHI clear shy`
power_train <- train_data$power
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
ghi_test_clear_sky <-test_data$`GHI clear shy`
Kt_test <- ghi_test / ghi_test_clear_sky
Kt_test[is.na(Kt_test) | is.infinite(Kt_test)] <- 0  
predicted_power <- C * Kt_test * ghi_test_clear_sky
print("Predicted Power:")
print(predicted_power)


library(dplyr)
library(readxl)
data <- read_excel("/Users/machang/Desktop/2023/new/plant3.xlsx")
data[] <- lapply(data, as.numeric)  
train_data <- data[3563:4732, ]  
test_data <- data[4733:4745, ]  
cos_zenith_train <- cos(train_data$`zenith angle`)
cos_zenith_train[cos_zenith_train < 0] <- 0
ghi_train <- train_data$`direct solar radiation` + 
  train_data$`scattered radiation` * cos_zenith_train
ghi_clear_sky_train <- train_data$`GHI clear shy`
power_train <- train_data$power
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
ghi_test_clear_sky <-test_data$`GHI clear shy`
Kt_test <- ghi_test / ghi_test_clear_sky
Kt_test[is.na(Kt_test) | is.infinite(Kt_test)] <- 0  
predicted_power <- C * Kt_test * ghi_test_clear_sky
print("Predicted Power:")
print(predicted_power)


#plant4
library(dplyr)
library(readxl)
data <- read_excel("/Users/machang/Desktop/2023/new/plant4.xlsx")
data[] <- lapply(data, as.numeric)  
train_data <- data[1:1170, ]  
test_data <- data[1158:1170, ]  
cos_zenith_train <- cos(train_data$`zenith angle`)
cos_zenith_train[cos_zenith_train < 0] <- 0
ghi_train <- train_data$`direct solar radiation` + 
  train_data$`scattered radiation` * cos_zenith_train
ghi_clear_sky_train <- train_data$`GHI clear shy`
power_train <- train_data$power
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
ghi_test_clear_sky <-test_data$`GHI clear shy`
Kt_test <- ghi_test / ghi_test_clear_sky
Kt_test[is.na(Kt_test) | is.infinite(Kt_test)] <- 0  
predicted_power <- C * Kt_test * ghi_test_clear_sky
print("Predicted Power:")
print(predicted_power)




library(dplyr)
library(readxl)
data <- read_excel("/Users/machang/Desktop/2023/new/plant4.xlsx")
data[] <- lapply(data, as.numeric)  
train_data <- data[1171:2353, ]  
test_data <- data[2354:2366, ]  
cos_zenith_train <- cos(train_data$`zenith angle`)
cos_zenith_train[cos_zenith_train < 0] <- 0
ghi_train <- train_data$`direct solar radiation` + 
  train_data$`scattered radiation` * cos_zenith_train
ghi_clear_sky_train <- train_data$`GHI clear shy`
power_train <- train_data$power
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
ghi_test_clear_sky <-test_data$`GHI clear shy`
Kt_test <- ghi_test / ghi_test_clear_sky
Kt_test[is.na(Kt_test) | is.infinite(Kt_test)] <- 0  
predicted_power <- C * Kt_test * ghi_test_clear_sky
print("Predicted Power:")
print(predicted_power)



library(dplyr)
library(readxl)
data <- read_excel("/Users/machang/Desktop/2023/new/plant4.xlsx")
data[] <- lapply(data, as.numeric)  
train_data <- data[2363:3549, ]  
test_data <- data[3550:3562, ]  
cos_zenith_train <- cos(train_data$`zenith angle`)
cos_zenith_train[cos_zenith_train < 0] <- 0
ghi_train <- train_data$`direct solar radiation` + 
  train_data$`scattered radiation` * cos_zenith_train
ghi_clear_sky_train <- train_data$`GHI clear shy`
power_train <- train_data$power
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
ghi_test_clear_sky <-test_data$`GHI clear shy`
Kt_test <- ghi_test / ghi_test_clear_sky
Kt_test[is.na(Kt_test) | is.infinite(Kt_test)] <- 0  
predicted_power <- C * Kt_test * ghi_test_clear_sky
print("Predicted Power:")
print(predicted_power)

library(dplyr)
library(readxl)
data <- read_excel("/Users/machang/Desktop/2023/new/plant4.xlsx")
data[] <- lapply(data, as.numeric)  
train_data <- data[3563:4732, ]  
test_data <- data[4733:4745, ]  
cos_zenith_train <- cos(train_data$`zenith angle`)
cos_zenith_train[cos_zenith_train < 0] <- 0
ghi_train <- train_data$`direct solar radiation` + 
  train_data$`scattered radiation` * cos_zenith_train
ghi_clear_sky_train <- train_data$`GHI clear shy`
power_train <- train_data$power
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
ghi_test_clear_sky <-test_data$`GHI clear shy`
Kt_test <- ghi_test / ghi_test_clear_sky
Kt_test[is.na(Kt_test) | is.infinite(Kt_test)] <- 0  
predicted_power <- C * Kt_test * ghi_test_clear_sky
print("Predicted Power:")
print(predicted_power)


#plant5
library(dplyr)
library(readxl)
data <- read_excel("/Users/machang/Desktop/2023/new/plant5.xlsx")
data[] <- lapply(data, as.numeric)  
train_data <- data[1:1170, ]  
test_data <- data[1158:1170, ]  
cos_zenith_train <- cos(train_data$`zenith angle`)
cos_zenith_train[cos_zenith_train < 0] <- 0
ghi_train <- train_data$`direct solar radiation` + 
  train_data$`scattered radiation` * cos_zenith_train
ghi_clear_sky_train <- train_data$`GHI clear shy`
power_train <- train_data$power
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
ghi_test_clear_sky <-test_data$`GHI clear shy`
Kt_test <- ghi_test / ghi_test_clear_sky
Kt_test[is.na(Kt_test) | is.infinite(Kt_test)] <- 0  
predicted_power <- C * Kt_test * ghi_test_clear_sky
print("Predicted Power:")
print(predicted_power)




library(dplyr)
library(readxl)
data <- read_excel("/Users/machang/Desktop/2023/new/plant5.xlsx")
data[] <- lapply(data, as.numeric)  
train_data <- data[1171:2353, ]  
test_data <- data[2354:2366, ]  
cos_zenith_train <- cos(train_data$`zenith angle`)
cos_zenith_train[cos_zenith_train < 0] <- 0
ghi_train <- train_data$`direct solar radiation` + 
  train_data$`scattered radiation` * cos_zenith_train
ghi_clear_sky_train <- train_data$`GHI clear shy`
power_train <- train_data$power
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
ghi_test_clear_sky <-test_data$`GHI clear shy`
Kt_test <- ghi_test / ghi_test_clear_sky
Kt_test[is.na(Kt_test) | is.infinite(Kt_test)] <- 0  
predicted_power <- C * Kt_test * ghi_test_clear_sky
print("Predicted Power:")
print(predicted_power)



library(dplyr)
library(readxl)
data <- read_excel("/Users/machang/Desktop/2023/new/plant5.xlsx")
data[] <- lapply(data, as.numeric)  
train_data <- data[2363:3549, ]  
test_data <- data[3550:3562, ]  
cos_zenith_train <- cos(train_data$`zenith angle`)
cos_zenith_train[cos_zenith_train < 0] <- 0
ghi_train <- train_data$`direct solar radiation` + 
  train_data$`scattered radiation` * cos_zenith_train
ghi_clear_sky_train <- train_data$`GHI clear shy`
power_train <- train_data$power
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
ghi_test_clear_sky <-test_data$`GHI clear shy`
Kt_test <- ghi_test / ghi_test_clear_sky
Kt_test[is.na(Kt_test) | is.infinite(Kt_test)] <- 0  
predicted_power <- C * Kt_test * ghi_test_clear_sky
print("Predicted Power:")
print(predicted_power)


library(dplyr)
library(readxl)
data <- read_excel("/Users/machang/Desktop/2023/new/plant5.xlsx")
data[] <- lapply(data, as.numeric)  
train_data <- data[3563:4732, ]  
test_data <- data[4733:4745, ]  
cos_zenith_train <- cos(train_data$`zenith angle`)
cos_zenith_train[cos_zenith_train < 0] <- 0
ghi_train <- train_data$`direct solar radiation` + 
  train_data$`scattered radiation` * cos_zenith_train
ghi_clear_sky_train <- train_data$`GHI clear shy`
power_train <- train_data$power
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
ghi_test_clear_sky <-test_data$`GHI clear shy`
Kt_test <- ghi_test / ghi_test_clear_sky
Kt_test[is.na(Kt_test) | is.infinite(Kt_test)] <- 0  
predicted_power <- C * Kt_test * ghi_test_clear_sky
print("Predicted Power:")
print(predicted_power)










#short term
#plant1
library(dplyr)
library(readxl)
data <- read_excel("/Users/machang/Desktop/2023/new/plant1.xlsx")
data[] <- lapply(data, as.numeric)  
train_data <- data[1:1105, ]  
test_data <- data[1106:1170, ]  
cos_zenith_train <- cos(train_data$`zenith angle`)
cos_zenith_train[cos_zenith_train < 0] <- 0
ghi_train <- train_data$`direct solar radiation` + 
  train_data$`scattered radiation` * cos_zenith_train
ghi_clear_sky_train <- train_data$`GHI clear shy`
power_train <- train_data$power
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
ghi_test_clear_sky <-test_data$`GHI clear shy`
Kt_test <- ghi_test / ghi_test_clear_sky
Kt_test[is.na(Kt_test) | is.infinite(Kt_test)] <- 0  
predicted_power <- C * Kt_test * ghi_test_clear_sky
print("Predicted Power:")
print(predicted_power)




library(dplyr)
library(readxl)
data <- read_excel("/Users/machang/Desktop/2023/new/plant1.xlsx")
data[] <- lapply(data, as.numeric)  
train_data <- data[1171:2301, ]  
test_data <- data[2302:2366, ]  
cos_zenith_train <- cos(train_data$`zenith angle`)
cos_zenith_train[cos_zenith_train < 0] <- 0
ghi_train <- train_data$`direct solar radiation` + 
  train_data$`scattered radiation` * cos_zenith_train
ghi_clear_sky_train <- train_data$`GHI clear shy`
power_train <- train_data$power
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
ghi_test_clear_sky <-test_data$`GHI clear shy`
Kt_test <- ghi_test / ghi_test_clear_sky
Kt_test[is.na(Kt_test) | is.infinite(Kt_test)] <- 0  
predicted_power <- C * Kt_test * ghi_test_clear_sky
print("Predicted Power:")
print(predicted_power)



library(dplyr)
library(readxl)
data <- read_excel("/Users/machang/Desktop/2023/new/plant1.xlsx")
data[] <- lapply(data, as.numeric)  
train_data <- data[2367:3497, ]  
test_data <- data[3498:3562, ]  
cos_zenith_train <- cos(train_data$`zenith angle`)
cos_zenith_train[cos_zenith_train < 0] <- 0
ghi_train <- train_data$`direct solar radiation` + 
  train_data$`scattered radiation` * cos_zenith_train
ghi_clear_sky_train <- train_data$`GHI clear shy`
power_train <- train_data$power
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
ghi_test_clear_sky <-test_data$`GHI clear shy`
Kt_test <- ghi_test / ghi_test_clear_sky
Kt_test[is.na(Kt_test) | is.infinite(Kt_test)] <- 0  
predicted_power <- C * Kt_test * ghi_test_clear_sky
print("Predicted Power:")
print(predicted_power)


library(dplyr)
library(readxl)
data <- read_excel("/Users/machang/Desktop/2023/new/plant1.xlsx")
data[] <- lapply(data, as.numeric)  
train_data <- data[3563:4680, ]  
test_data <- data[4681:4745, ]  
cos_zenith_train <- cos(train_data$`zenith angle`)
cos_zenith_train[cos_zenith_train < 0] <- 0
ghi_train <- train_data$`direct solar radiation` + 
  train_data$`scattered radiation` * cos_zenith_train
ghi_clear_sky_train <- train_data$`GHI clear shy`
power_train <- train_data$power
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
ghi_test_clear_sky <-test_data$`GHI clear shy`
Kt_test <- ghi_test / ghi_test_clear_sky
Kt_test[is.na(Kt_test) | is.infinite(Kt_test)] <- 0  
predicted_power <- C * Kt_test * ghi_test_clear_sky
print("Predicted Power:")
print(predicted_power)

#plant2
library(dplyr)
library(readxl)
data <- read_excel("/Users/machang/Desktop/2023/new/plant2.xlsx")
data[] <- lapply(data, as.numeric)  
train_data <- data[1:1105, ]  
test_data <- data[1106:1170, ]  
cos_zenith_train <- cos(train_data$`zenith angle`)
cos_zenith_train[cos_zenith_train < 0] <- 0
ghi_train <- train_data$`direct solar radiation` + 
  train_data$`scattered radiation` * cos_zenith_train
ghi_clear_sky_train <- train_data$`GHI clear shy`
power_train <- train_data$power
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
ghi_test_clear_sky <-test_data$`GHI clear shy`
Kt_test <- ghi_test / ghi_test_clear_sky
Kt_test[is.na(Kt_test) | is.infinite(Kt_test)] <- 0  
predicted_power <- C * Kt_test * ghi_test_clear_sky
print("Predicted Power:")
print(predicted_power)




library(dplyr)
library(readxl)
data <- read_excel("/Users/machang/Desktop/2023/new/plant2.xlsx")
data[] <- lapply(data, as.numeric)  
train_data <- data[1171:2301, ]  
test_data <- data[2302:2366, ]  
cos_zenith_train <- cos(train_data$`zenith angle`)
cos_zenith_train[cos_zenith_train < 0] <- 0
ghi_train <- train_data$`direct solar radiation` + 
  train_data$`scattered radiation` * cos_zenith_train
ghi_clear_sky_train <- train_data$`GHI clear shy`
power_train <- train_data$power
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
ghi_test_clear_sky <-test_data$`GHI clear shy`
Kt_test <- ghi_test / ghi_test_clear_sky
Kt_test[is.na(Kt_test) | is.infinite(Kt_test)] <- 0  
predicted_power <- C * Kt_test * ghi_test_clear_sky
print("Predicted Power:")
print(predicted_power)



library(dplyr)
library(readxl)
data <- read_excel("/Users/machang/Desktop/2023/new/plant2.xlsx")
data[] <- lapply(data, as.numeric)  
train_data <- data[2363:3497, ]  
test_data <- data[3498:3562, ]  
cos_zenith_train <- cos(train_data$`zenith angle`)
cos_zenith_train[cos_zenith_train < 0] <- 0
ghi_train <- train_data$`direct solar radiation` + 
  train_data$`scattered radiation` * cos_zenith_train
ghi_clear_sky_train <- train_data$`GHI clear shy`
power_train <- train_data$power
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
ghi_test_clear_sky <-test_data$`GHI clear shy`
Kt_test <- ghi_test / ghi_test_clear_sky
Kt_test[is.na(Kt_test) | is.infinite(Kt_test)] <- 0  
predicted_power <- C * Kt_test * ghi_test_clear_sky
print("Predicted Power:")
print(predicted_power)


library(dplyr)
library(readxl)
data <- read_excel("/Users/machang/Desktop/2023/new/plant2.xlsx")
data[] <- lapply(data, as.numeric)  
train_data <- data[3563:4680, ]  
test_data <- data[4681:4745, ]  
cos_zenith_train <- cos(train_data$`zenith angle`)
cos_zenith_train[cos_zenith_train < 0] <- 0
ghi_train <- train_data$`direct solar radiation` + 
  train_data$`scattered radiation` * cos_zenith_train
ghi_clear_sky_train <- train_data$`GHI clear shy`
power_train <- train_data$power
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
ghi_test_clear_sky <-test_data$`GHI clear shy`
Kt_test <- ghi_test / ghi_test_clear_sky
Kt_test[is.na(Kt_test) | is.infinite(Kt_test)] <- 0  
predicted_power <- C * Kt_test * ghi_test_clear_sky
print("Predicted Power:")
print(predicted_power)



#plant3
library(dplyr)
library(readxl)
data <- read_excel("/Users/machang/Desktop/2023/new/plant3.xlsx")
data[] <- lapply(data, as.numeric)  
train_data <- data[1:1105, ]  
test_data <- data[1106:1170, ]  
cos_zenith_train <- cos(train_data$`zenith angle`)
cos_zenith_train[cos_zenith_train < 0] <- 0
ghi_train <- train_data$`direct solar radiation` + 
  train_data$`scattered radiation` * cos_zenith_train
ghi_clear_sky_train <- train_data$`GHI clear shy`
power_train <- train_data$power
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
ghi_test_clear_sky <-test_data$`GHI clear shy`
Kt_test <- ghi_test / ghi_test_clear_sky
Kt_test[is.na(Kt_test) | is.infinite(Kt_test)] <- 0  
predicted_power <- C * Kt_test * ghi_test_clear_sky
print("Predicted Power:")
print(predicted_power)




library(dplyr)
library(readxl)
data <- read_excel("/Users/machang/Desktop/2023/new/plant3.xlsx")
data[] <- lapply(data, as.numeric)  
train_data <- data[1171:2301, ]  
test_data <- data[2302:2366, ]  
cos_zenith_train <- cos(train_data$`zenith angle`)
cos_zenith_train[cos_zenith_train < 0] <- 0
ghi_train <- train_data$`direct solar radiation` + 
  train_data$`scattered radiation` * cos_zenith_train
ghi_clear_sky_train <- train_data$`GHI clear shy`
power_train <- train_data$power
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
ghi_test_clear_sky <-test_data$`GHI clear shy`
Kt_test <- ghi_test / ghi_test_clear_sky
Kt_test[is.na(Kt_test) | is.infinite(Kt_test)] <- 0  
predicted_power <- C * Kt_test * ghi_test_clear_sky
print("Predicted Power:")
print(predicted_power)



library(dplyr)
library(readxl)
data <- read_excel("/Users/machang/Desktop/2023/new/plant3.xlsx")
data[] <- lapply(data, as.numeric)  
train_data <- data[2367:3497, ]  
test_data <- data[3498:3562, ]  
cos_zenith_train <- cos(train_data$`zenith angle`)
cos_zenith_train[cos_zenith_train < 0] <- 0
ghi_train <- train_data$`direct solar radiation` + 
  train_data$`scattered radiation` * cos_zenith_train
ghi_clear_sky_train <- train_data$`GHI clear shy`
power_train <- train_data$power
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
ghi_test_clear_sky <-test_data$`GHI clear shy`
Kt_test <- ghi_test / ghi_test_clear_sky
Kt_test[is.na(Kt_test) | is.infinite(Kt_test)] <- 0  
predicted_power <- C * Kt_test * ghi_test_clear_sky
print("Predicted Power:")
print(predicted_power)


library(dplyr)
library(readxl)
data <- read_excel("/Users/machang/Desktop/2023/new/plant3.xlsx")
data[] <- lapply(data, as.numeric)  
train_data <- data[3563:4680, ]  
test_data <- data[4681:4745, ]  
cos_zenith_train <- cos(train_data$`zenith angle`)
cos_zenith_train[cos_zenith_train < 0] <- 0
ghi_train <- train_data$`direct solar radiation` + 
  train_data$`scattered radiation` * cos_zenith_train
ghi_clear_sky_train <- train_data$`GHI clear shy`
power_train <- train_data$power
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
ghi_test_clear_sky <-test_data$`GHI clear shy`
Kt_test <- ghi_test / ghi_test_clear_sky
Kt_test[is.na(Kt_test) | is.infinite(Kt_test)] <- 0  
predicted_power <- C * Kt_test * ghi_test_clear_sky
print("Predicted Power:")
print(predicted_power)


#plant4
library(dplyr)
library(readxl)
data <- read_excel("/Users/machang/Desktop/2023/new/plant4.xlsx")
data[] <- lapply(data, as.numeric)  
train_data <- data[1:1105, ]  
test_data <- data[1106:1170, ]  
cos_zenith_train <- cos(train_data$`zenith angle`)
cos_zenith_train[cos_zenith_train < 0] <- 0
ghi_train <- train_data$`direct solar radiation` + 
  train_data$`scattered radiation` * cos_zenith_train
ghi_clear_sky_train <- train_data$`GHI clear shy`
power_train <- train_data$power
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
ghi_test_clear_sky <-test_data$`GHI clear shy`
Kt_test <- ghi_test / ghi_test_clear_sky
Kt_test[is.na(Kt_test) | is.infinite(Kt_test)] <- 0  
predicted_power <- C * Kt_test * ghi_test_clear_sky
print("Predicted Power:")
print(predicted_power)




library(dplyr)
library(readxl)
data <- read_excel("/Users/machang/Desktop/2023/new/plant4.xlsx")
data[] <- lapply(data, as.numeric)  
train_data <- data[1171:2301, ]  
test_data <- data[2302:2366, ]  
cos_zenith_train <- cos(train_data$`zenith angle`)
cos_zenith_train[cos_zenith_train < 0] <- 0
ghi_train <- train_data$`direct solar radiation` + 
  train_data$`scattered radiation` * cos_zenith_train
ghi_clear_sky_train <- train_data$`GHI clear shy`
power_train <- train_data$power
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
ghi_test_clear_sky <-test_data$`GHI clear shy`
Kt_test <- ghi_test / ghi_test_clear_sky
Kt_test[is.na(Kt_test) | is.infinite(Kt_test)] <- 0  
predicted_power <- C * Kt_test * ghi_test_clear_sky
print("Predicted Power:")
print(predicted_power)



library(dplyr)
library(readxl)
data <- read_excel("/Users/machang/Desktop/2023/new/plant4.xlsx")
data[] <- lapply(data, as.numeric)  
train_data <- data[2367:3497, ]  
test_data <- data[3498:3562, ]  
cos_zenith_train <- cos(train_data$`zenith angle`)
cos_zenith_train[cos_zenith_train < 0] <- 0
ghi_train <- train_data$`direct solar radiation` + 
  train_data$`scattered radiation` * cos_zenith_train
ghi_clear_sky_train <- train_data$`GHI clear shy`
power_train <- train_data$power
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
ghi_test_clear_sky <-test_data$`GHI clear shy`
Kt_test <- ghi_test / ghi_test_clear_sky
Kt_test[is.na(Kt_test) | is.infinite(Kt_test)] <- 0  
predicted_power <- C * Kt_test * ghi_test_clear_sky
print("Predicted Power:")
print(predicted_power)

library(dplyr)
library(readxl)
data <- read_excel("/Users/machang/Desktop/2023/new/plant4.xlsx")
data[] <- lapply(data, as.numeric)  
train_data <- data[3563:4680, ]  
test_data <- data[4681:4745, ]  
cos_zenith_train <- cos(train_data$`zenith angle`)
cos_zenith_train[cos_zenith_train < 0] <- 0
ghi_train <- train_data$`direct solar radiation` + 
  train_data$`scattered radiation` * cos_zenith_train
ghi_clear_sky_train <- train_data$`GHI clear shy`
power_train <- train_data$power
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
ghi_test_clear_sky <-test_data$`GHI clear shy`
Kt_test <- ghi_test / ghi_test_clear_sky
Kt_test[is.na(Kt_test) | is.infinite(Kt_test)] <- 0  
predicted_power <- C * Kt_test * ghi_test_clear_sky
print("Predicted Power:")
print(predicted_power)


#plant5
library(dplyr)
library(readxl)
data <- read_excel("/Users/machang/Desktop/2023/new/plant5.xlsx")
data[] <- lapply(data, as.numeric)  
train_data <- data[1:1105, ]  
test_data <- data[1106:1170, ]  
cos_zenith_train <- cos(train_data$`zenith angle`)
cos_zenith_train[cos_zenith_train < 0] <- 0
ghi_train <- train_data$`direct solar radiation` + 
  train_data$`scattered radiation` * cos_zenith_train
ghi_clear_sky_train <- train_data$`GHI clear shy`
power_train <- train_data$power
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
ghi_test_clear_sky <-test_data$`GHI clear shy`
Kt_test <- ghi_test / ghi_test_clear_sky
Kt_test[is.na(Kt_test) | is.infinite(Kt_test)] <- 0  
predicted_power <- C * Kt_test * ghi_test_clear_sky
print("Predicted Power:")
print(predicted_power)




library(dplyr)
library(readxl)
data <- read_excel("/Users/machang/Desktop/2023/new/plant5.xlsx")
data[] <- lapply(data, as.numeric)  
train_data <- data[1171:2301, ]  
test_data <- data[2302:2366, ]  
cos_zenith_train <- cos(train_data$`zenith angle`)
cos_zenith_train[cos_zenith_train < 0] <- 0
ghi_train <- train_data$`direct solar radiation` + 
  train_data$`scattered radiation` * cos_zenith_train
ghi_clear_sky_train <- train_data$`GHI clear shy`
power_train <- train_data$power
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
ghi_test_clear_sky <-test_data$`GHI clear shy`
Kt_test <- ghi_test / ghi_test_clear_sky
Kt_test[is.na(Kt_test) | is.infinite(Kt_test)] <- 0  
predicted_power <- C * Kt_test * ghi_test_clear_sky
print("Predicted Power:")
print(predicted_power)



library(dplyr)
library(readxl)
data <- read_excel("/Users/machang/Desktop/2023/new/plant5.xlsx")
data[] <- lapply(data, as.numeric)  
train_data <- data[2363:3497, ]  
test_data <- data[3498:3562, ]  
cos_zenith_train <- cos(train_data$`zenith angle`)
cos_zenith_train[cos_zenith_train < 0] <- 0
ghi_train <- train_data$`direct solar radiation` + 
  train_data$`scattered radiation` * cos_zenith_train
ghi_clear_sky_train <- train_data$`GHI clear shy`
power_train <- train_data$power
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
ghi_test_clear_sky <-test_data$`GHI clear shy`
Kt_test <- ghi_test / ghi_test_clear_sky
Kt_test[is.na(Kt_test) | is.infinite(Kt_test)] <- 0  
predicted_power <- C * Kt_test * ghi_test_clear_sky
print("Predicted Power:")
print(predicted_power)


library(dplyr)
library(readxl)
data <- read_excel("/Users/machang/Desktop/2023/new/plant5.xlsx")
data[] <- lapply(data, as.numeric)  
train_data <- data[3563:4680, ]  
test_data <- data[4681:4745, ]  
cos_zenith_train <- cos(train_data$`zenith angle`)
cos_zenith_train[cos_zenith_train < 0] <- 0
ghi_train <- train_data$`direct solar radiation` + 
  train_data$`scattered radiation` * cos_zenith_train
ghi_clear_sky_train <- train_data$`GHI clear shy`
power_train <- train_data$power
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
ghi_test_clear_sky <-test_data$`GHI clear shy`
Kt_test <- ghi_test / ghi_test_clear_sky
Kt_test[is.na(Kt_test) | is.infinite(Kt_test)] <- 0  
predicted_power <- C * Kt_test * ghi_test_clear_sky
print("Predicted Power:")
print(predicted_power)