library(faraway)
library(leaps)
library(corrplot)
library(MASS)
library(ggplot2)
library(rpart)
library(rattle) 
library(rpart.plot) 
library(RColorBrewer)
library(tidyr)
library(stringr)
library(chron)
library(dplyr)
library(sjPlot)
library(dummies)


## Colinearity

# Scatter plots
#png("lognumerics.png")
data %>% mutate(log_cnt = log(cnt)) %>% select(-c(1, 6:ncol(data))) %>%
  pivot_longer(-log_cnt, values_to = "NumericVariables") %>% 
  ggplot(aes(x = NumericVariables, y = log_cnt)) +
  facet_wrap(~ name, scales = "free") +
  geom_point(alpha = 0.1)
#dev.off()
# Numeric variables
#pairs(data[,1:5])[1]

#specify models to try
formula1 <- cnt ~ t1 + t2 + hum + wind_speed + weather_code + is_holiday + is_weekend + season + hour
formula2 <- cnt ~ t2 + hum + wind_speed + weather_code + hour + weekday
formula3 <- cnt ~ t2 + hum + wind_speed + weather_code + hour

lm1 = lm(formula1, data = data)
par(mfrow=c(2,2))
plot(lm1, which=c(1,2,3,4))
vif(lm1)
print("R^2")
c(summary(lm1)$r.squared)
BIC(lm1)

#png("num_corr.png")
corrplot(cor(select(data, t1, t2, hum, wind_speed)))
#dev.off()

#IGNORE ALL THIS, NOT USING IT YET
# library(dummies)
# data_dummies<-dummy.data.frame(data,names=c("season","weather_code"),sep="_") 
# #error message, check to make sure this worked
# unique(data$season) #season coding is 1-4; not 0-3 (need to determine which code is correct)
# unique(data$weather_code)#weather_94 not in dataset
# 
# #rename vars
# test<-select(data_dummies,season_1,season_2,season_3,season_4)
# test <- rename(test, "Spring"=season_1, "Summer"=season_2,"Fall"=season_3, "winter"=season_4)
# data_dummies<-select(data_dummies, work, hum, is_weekend,is_holiday,cnt,work,t1, 
#                                   hour, season_1,season_2,season_3,season_4, 
#                                   weather_code_1, weather_code_2,weather_code_3,weather_code_4,weather_code_7,weather_code_10,weather_code_26)
# 
# lm(cnt~t1,as.numeric(season_1),data=data_dummies)
# lm(cnt~t1,as.numeric(season_2),data=data_dummies)
# 
# 
# corrplot(cor(select(data_dummies, t1, t2, hum, wind_speed, weather_code_1,weather_code_2,weather_code_3,weather_code_4,weather_code_7,weather_code_10,weather_code_26 )))
# png("Corr_allvars.png")
# 
# 
# data_dummies <- rename(data_dummies, "Work"=work, "is_weekend"=is_weekend,"is_holiday"=is_holiday,"BikeCount"=cnt,"Temperature"=t1,
#                                        "Hour"=hour,"Spring"=season_1, "Summer"=season_2,"Fall"=season_3, "winter"=season_4,
#                                        "Clear"=weather_code_1, "FewClouds"=weather_code_2, "Broken Clouds"=weather_code_3,
#                                        "Cloudy"=weather_code_4, "Rain"=weather_code_7, "Thuderstorm"=weather_code_10, "Snow"=weather_code_26) 



#This suggests us to remove t1. 

#will not include month b/c it perfectly predicts season (R Sq=1, anova test results in sig. F statistic (reject null that B1=B2=0))
summary(lm(as.numeric(season)~month, data=data))
model2<-lm(cnt~season, data=data)
model3<-lm(cnt~season+month, data=data)
anova(model2,model3)

#will not include 
model1<-lm(cnt~season, data=data)
model1<-lm(cnt~season+weather_code, data=data)
model4<-lm(cnt~season+t2, data=data)

#NOTE: besthourslm<-lm(cnt ~ t2 + hum + wind_speed + weather_code + season + all_hrs2, data = hour_data)

anova(model1,model2)


