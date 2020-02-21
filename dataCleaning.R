library(dplyr)
library(tidyr)
library(stringr)
library(chron)

bike = read.csv("~/SS504_bike_sharing/london_merged.csv")
bike = separate(bike, timestamp, into = c("date", "time"), sep=" ")

# Convert date/time variables to datetime/chron objects
bike$date <- as.Date(bike$date)
bike$time <- chron(times. = bike$time)

# Extract year, month, day, hour from date/time variables
# Note: the finest time resolution we have is hour. All minutes/seconds are "00". 
# If curious, run below:
# unique(minutes(bike$time))
# unique(seconds(bike$time))
bike <- bike %>% mutate(year = as.numeric(format(date, "%Y")), 
                        month = as.numeric(format(date, "%m")),
                        day = as.numeric(format(date, "%d")),
                        hour = as.numeric(hours(time)),
                        weekday = weekdays(date)) %>% select(c(-1,-2))

# Take out 2017 to have fewer factors?
bike <- bike %>% filter(year != 2017)

# Categorical predictors as factors
bike[,c("weather_code", "season", "is_holiday", "is_weekend", 
        "year", "month", "day", "hour", "weekday")] <- lapply(bike[,c("weather_code", "season", "is_holiday", 
                                                                         "is_weekend", "year", "month", "day", "hour", "weekday")], factor)
#bike %>% group_by(weekday) %>% summarise(mean(cnt))
# Make vars:
# day of week (monday, tuesday)
#is.sunday


# Data splitting
# set seed to always generate same random numbers
set.seed(123)
smp_siz <- floor(0.5*nrow(bike))
train_ind <- sample(seq_len(nrow(bike)),size = smp_siz)
train <- Smarket[train_ind,]
test <- Smarket[-train_ind,]
