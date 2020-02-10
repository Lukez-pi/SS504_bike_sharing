library(dplyr)
library(tidyr)
library(stringr)
library(chron)

bike = read.csv("~/SS504_bike_sharing/london_merged.csv")
bike = separate(bike, timestamp, into = c("date", "time"), sep=" ")

# Convert date/time variables to datetime/chron objects
bike$date <- as.Date(bike$date)
bike$time <- chron(times. = bike$time)

# Categorical predictors as factors
# Should this also be done to binary predictors (is_holiday, is_weekend)?
bike$weather_code <- as.factor(bike$weather_code)
bike$season <- as.factor(bike$season)

# Extract year, month, day, hour from date/time variables
# Note: the finest time resolution we have is hour. All minutes/seconds are "00". 
# If curious, run below:
# unique(minutes(bike$time))
# unique(seconds(bike$time))
bike <- bike %>% mutate(year = as.numeric(format(date, "%Y")), 
                        month = as.numeric(format(date, "%m")),
                        day = as.numeric(format(date, "%d")),
                        hour = as.numeric(hours(time))) %>% select(c(-1,-2))

