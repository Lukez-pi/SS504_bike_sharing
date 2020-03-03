library(tidyr)
library(stringr)
library(chron)
library(dplyr)

# Change this to your file!
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  #keep for ellie
#setwd(~/Desktop/SS504_bike_sharing)
bike = read.csv("london_merged.csv")

bike = separate(bike, timestamp, into = c("date", "time"), sep=" ")

# Convert date/time variables to datetime/chron objects
bike$date <- as.Date(bike$date)
bike$time <- chron(times. = bike$time)

# Extract year, month, day, hour from date/time variables
# Add "work": 1 = workday, 0 = else
bike <- bike %>% mutate(year = as.numeric(format(date, "%Y")), 
                        month = as.numeric(format(date, "%m")),
                        day = as.numeric(format(date, "%d")),
                        hour = as.numeric(hours(time)),
                        weekday = weekdays(date),
                        work = ifelse(is_holiday + is_weekend == 0, 1, 0)) 

# Categorical predictors as factors
bike[, 8:ncol(bike)] <- lapply(bike[, 8:ncol(bike)], factor)

bike <- select(bike, -c(date, time))

bike <- filter(bike, cnt != 0)

# Data splitting
set.seed(123)
smp_siz <- floor(0.5*nrow(bike))
train_ind <- sample(seq_len(nrow(bike)),size = smp_siz)
train <- bike[train_ind,]
test <- bike[-train_ind,]



wkd_bike <- filter(bike, is_weekend == 1)
hol_bike <- filter(bike, is_holiday == 1)
work_bike <- filter(bike, work == 1)
wkdorhol_bike<-rbind(wkd_bike,hol_bike)


data = bike
rm(bike)


