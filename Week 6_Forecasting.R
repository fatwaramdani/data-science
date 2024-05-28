# Check the current working directory
getwd()

# Set working directory
setwd("D:/DATA/DATA SCIENCE FOR SOCIAL SCIENCE/DATA/WEEK 6")

# Load library
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)

# R code to import the data
data <- read_csv("sakura_first_bloom_dates.csv")

# R code to handle missing values
data_cleaned <- na.omit(data)
summary(data_cleaned)

# Removing columns 
data_cleaned[ , c('Currently Being Observed', 
                  '30 Year Average 1981-2010', 
                  'Notes')] <- list(NULL)
summary(data_cleaned)

#Rename only the 1st column names
colnames(data_cleaned)[c(1)] <- c("Site")

# Identify the year columns (assuming they start from the second column)
year_cols <- names(data_cleaned)[2:ncol(data)]
print(year_cols)

# Convert the character year columns to numeric
data_cleaned[, year_cols] <- lapply(data_cleaned[, year_cols], as.numeric)

# Reshape the data from wide to long format using gather()
data_cleaned <- gather(data_cleaned, key = year, value = value, -Site)
View(data_cleaned)

data_cleaned$year <- substr(data_cleaned$value, 1, 4)
data_cleaned$month <- substr(data_cleaned$value, 6, 7)
data_cleaned$date <- substr(data_cleaned$value, 9, 10)

# Convert year, month and date into numeric format (optional)
data_cleaned$year <- as.numeric(data_cleaned$year)
data_cleaned$month <- as.numeric(data_cleaned$month)
data_cleaned$date <- as.numeric(data_cleaned$date)
View(data_cleaned)

# Create the doy
data_cleaned$doy <- yday(data_cleaned$value)

# R code to create a time series plot (DOY)
plot(data_cleaned$year, data_cleaned$doy, 
     type="l", main="Sakura First Month Over Time", 
     xlab="Time", ylab="Day of Year", col="purple")

# Plot with month on x-axis and year on the fill aesthetic
ggplot(data_cleaned, aes(x = year, y = doy)) +
  geom_point(aes(size = 3), alpha = 0.5) +  # Adjust point size and transparency 
  geom_smooth(method = "lm", se = FALSE) +  # Add a linear regression line
  labs(title = "Shift in Asahikawa and Obihiro Sakura First Blooms",
       x = "Year", y = "First Bloom Date") +
  theme_bw()  # Apply a black and white theme

ggplot(data_cleaned, aes(x = year, y = doy)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(title = "Shift in Sakura First Blooms Date",
       x = "Year",
       y = "First Bloom Date (DOY)")

###############
# FORECASTING #
###############

# Subset data for a single country
Asahikawa <- subset(data_cleaned, Site == "Asahikawa")

# Subset only univariate
Asahikawa[ , c('Site', 'year', 'value', 'month', 'date')] <- list(NULL)


#remove rows 67 through 71 (We only use until 2018)
pred_Asahikawa <- Asahikawa[-c(67:71), ]

# Convert it to a time series object.
Asahikawa_ts_actual <- ts(Asahikawa, start = c(1953, 1),frequency = 1)
Asahikawa_ts_pred <- ts(pred_Asahikawa, start = c(1953, 1),frequency = 1)

# Print the timeseries data.
print(Asahikawa_ts_actual)
print(Asahikawa_ts_pred)

summary(Asahikawa_ts_actual)
summary(Asahikawa_ts_pred)

plot(Asahikawa_ts_actual,main = "Actual Data for Sakura First Bloom at Asahikawa")

# Fitting model using auto.arima model 
model <- auto.arima(Asahikawa_ts_pred) 
model

# Next 5 forecasted values  
forecast_data <- forecast(model, 5) 
print(forecast_data)
plot(forecast_data, main = "Forecasting for Sakura First Bloom at Asahikawa")
lines(Asahikawa_ts_actual, type = "l", col = "red")