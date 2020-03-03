---
  title: "Project"
output: html_document
---



```{r}
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  #ET keep for ellie
#setwd(~/Desktop/SS504_bike_sharing)
source("dataCleaning.R")
```


Plots for presentation
```{r}
meanbikes_wkdhol=aggregate(wkdorhol_bike$cnt,list(wkdorhol_bike$hour), mean)
meanbikes_work=aggregate(work_bike$cnt,list(work_bike$hour), mean)
png("Bikes_!Work.png")
barplot(meanbikes_wkdhol$x,names.arg=meanbikes_wkdhol$Group.1, ylab="Avg num of bikes", xlab="Hour", main="Average number of bikes rented by hour, weekend or holiday")
png("Bikes_Work.png")
barplot(meanbikes_work$x,names.arg=meanbikes_work$Group.1, ylab="Avg num of bikes", xlab="Hour", main="Average number of bikes rented by hour, workday")

#plot(data$hour, data$`mean(cnt)`, main = "Average rentals by hour", xlab = "Hour", ylab = "Avg Rentals")
```

```{r}
# Plot counts per hour on weekend, holiday, workday
par(mfrow = c(3, 1), mar=c(4, 4, 2, 1))
plot(hol_bike$hour, hol_bike$cnt, main = "Holiday", xlab = "Hour", ylab = "Count", ylim = c(0, 7600))
plot(wkd_bike$hour, wkd_bike$cnt, main = "Weekend", xlab = "Hour", ylab = "Count", ylim = c(0, 7600))
plot(work_bike$hour, work_bike$cnt, main = "Workday", xlab = "Hour", ylab = "Count", ylim = c(0, 7600))
```


Running decision trees on 1. all 2. weekend 3. holiday 4. workday data
```{r}
# All days
all <- rpart(cnt ~ hour, data = data)
#png("decision_hour.png")
#fancyRpartPlot(all)
#dev.off()

# Weekend
wkd <- rpart(cnt ~ hour, data = wkd_bike)
#fancyRpartPlot(wkd)

# Holiday
hol <- rpart(cnt ~ hour, data = hol_bike)
#fancyRpartPlot(hol)

# Work
work <- rpart(cnt ~ hour, data = work_bike)
#png("decision_workhour.png")
#fancyRpartPlot(work)

#wkd/hol
wkd_hol <- rpart(cnt ~ hour, data = filter(data, work == 0))
#png("decision_!workhour.png")
#fancyRpartPlot(wkd_hol)
```
Here are the hour groupings depending on type of day. I am unsure if there is one grouping that works for both workdays and weekend/holiday.

All days:
  G1 (Sleeping): 22, 23, 0 - 6
G2: 10, 11, 20, 21
G3: 7, 9 , 12 - 16, 19
G4 (Peak): 8, 17, 18

Weekend (same if you group weekend and holiday)
G1: 1 - 7
G2: 0, 8, 9, 20 - 23
G3: 10, 11, 18, 19
G4: 12 - 17

Holiday
G1: 0-8, 21 - 23
G2: 9, 10, 19, 20
G3: 11, 17, 18
G4: 12 - 16

Workday:
  G1: 0 - 5
G2: 6, 22, 23
G3: 10 - 15, 20, 21
G4: 7, 9, 16, 19
G5: 17, 18
G6: 8

Our groups by eye:
  Peak rush: 8, 17, 18
off peak: 7, 9, 16, 19
midday: 10:15
Night: 20:23
sleeping: 0-6

```{r}
# Here I'm making hour_group variables for each day type. We shouldn't run the model on the full data since there's redundant information in these columns

hour_data <- data %>% mutate(
  all_hrs = ifelse(hour %in% c(7, 9 , 12:16, 19), "off_peak", 
                   ifelse(hour %in% c(22, 23, 0:6), "sleeping",
                          ifelse(hour %in% c(10, 11, 20, 21), "morn_eve",
                                 "peak"))),
  wkdhol_hrs = ifelse(hour %in% c(1:7), "!work_sleeping",
                      ifelse(hour %in% c(0, 8, 9, 20:23), "!work_8-9am&10pm-12am", 
                             ifelse(hour %in% c(10, 11, 18, 19), "!work_10-11am&6-7pm",
                                    "!work_12pm-5pm"))),
  hol_hrs = ifelse(hour %in% c(0:8, 21:23), "hol_sleeping", 
                   ifelse(hour %in% c(9, 10, 19, 20), "hol_8-9am_8-9pm",
                          ifelse(hour %in% c(11, 17, 18), "hol_11am_&_5-6pm",
                                 "hol_midday"))),
  work_hrs = ifelse(hour %in% c(0:5), "work_sleeping",
                    ifelse(hour %in% c(6, 22, 23), "work_6am&10-11pm",
                           ifelse(hour %in% c(10:15, 20, 21), "work_10am-3pm&10-11pm",
                                  ifelse(hour %in% c(7, 9, 16, 19), "work_7-9am&4-5pm",
                                         ifelse(hour %in% c(17, 18), "work_5-6pmrush",
                                                "work_8amrush"))))),
  eye_hrs = ifelse(hour %in% c(0:6), "sleeping",
                   ifelse(hour %in% c(20:23), "night",
                          ifelse(hour %in% c(10:15), "midday",
                                 ifelse(hour %in% c(7, 9, 16, 19), "off_peak", 
                                        "peak")))),
  #ET: combine work hrs & wkdhol into one group :
  all_hrs2= ifelse(work%in% c(1), work_hrs, wkdhol_hrs)
)
```

Testing lms based on different hour groups
```{r}
# Hour grouping with all days
# Adjusted R-squared: 0.69
summary(lm(cnt ~ t2 + hum + wind_speed + weather_code + season + work + all_hrs, data = hour_data))

# Hour grouping with all days + interaction of work/all_hrs
# Adjusted R-squared: 0.79
summary(lm(cnt ~ t2 + hum + wind_speed + weather_code + season + work + all_hrs + work:all_hrs, data = hour_data))

# All 24 hours
# Adjusted R-squared: 0.71
summary(lm(cnt ~ t2 + hum + wind_speed + weather_code + season + work + hour, data = hour_data))

# All 24 hours + interaction of work/all 24 hours
# Adjusted R-squared: 0.90
#ET this also adds a lot more vars, maybe we should do a bic test too
summary(lm(cnt ~ t2 + hum + wind_speed + weather_code + season + work + hour + work:hour, data = hour_data))

# Hour grouping by eye (most interpretable)
# Adjusted R-squared: 0.83 (why is this bigger than for all_hrs above?)
#ET: a little surprising, there is one more var in eye_hours comapred to all_hrs? 
summary(lm(cnt ~ t2 + hum + wind_speed + weather_code + season + work + eye_hrs + work:eye_hrs, data = hour_data))

#ET: hour grouping using combo all_hours2 var (this requires less var and does about teh same IN terms of adj-R2 as fully work:hour)
# Adjusted R-squared: 0.89 
#NOTE: workday_sleeping is excluded category, made other vars in order of effect
#reorder levels to make it more readible:
hour_data$all_hrs2<-factor(hour_data$all_hrs2,levels=c("work_sleeping","!work_sleeping","!work_12pm_5pm","!work_8-9am&10pm-12am","work_10am-3pm&10-11pm","!work_12pm-5pm","work_7-9am&4-5pm","work_5-6pmrush","work_8amrush"))
summary(lm(cnt ~ t2 + hum + wind_speed + weather_code + season + all_hrs2, data = hour_data))
```


#Chosen best model from tests above:
besthourslm<-lm(cnt ~ t2 + hum + wind_speed + weather_code + season + all_hrs2, data = hour_data)

```{r}
#Plot of hour groupings
meanbikes_allhours=aggregate(hour_data$cnt,list(hour_data$all_hrs2), mean)
png("Bikes_allhrs2.png")
par(mar = c(7, 5, 2, 2) + 4)
barplot(meanbikes_allhours$x,names.arg=meanbikes_allhours$Group.1, ylab="Avg num of bikes", main="Average number of bikes rented", las=2, space=1)
```
