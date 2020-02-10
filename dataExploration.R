source("~/SS504_bike_sharing/dataCleaning.R")

# takes a long time to run
#pairs(~cnt + t1 + t2 + hum + wind_speed + year + month + day + hour, data = bike)
print(cor(bike[,c(-6, -7, -8, -9)]),3)

lm0 <- lm(cnt ~ t1, bike)
lm1 <- lm(cnt ~ t1 + t2, bike)

anova(lm0, lm1)
