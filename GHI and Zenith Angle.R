install.packages("httr")
library(httr)
if (!require(suncalc)) {
  install.packages("suncalc")
}
library(suncalc)
#plant1
latitude <- 32.7816  
longitude <- 119.459 
start_date <- "2023-02-28"  #It can be changed to any date we want
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


#plant2
latitude <- 32.9543  
longitude <- 119.2138  
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


#plant3
latitude <- 33.0636 
longitude <- 119.2222  
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

#plant4
latitude <- 34.3468 
longitude <- 119.9552  
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

#plant5
latitude <- 31.8671 
longitude <- 119.9981  
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



