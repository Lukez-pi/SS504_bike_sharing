source("~/SS504_bike_sharing/dataCleaning.R")

# Subsample 1000 rows to make things run faster
sbike <- bike[sample(17414, 1000),]

hol_sbike <- filter(sbike, is_holiday==1)

# Notes:
# 8,643 observations in 2015
# 8,699 in 2016
# 72 in 2017

pairs(~cnt + t1 + t2 + hum + wind_speed + year + month + day + hour, data = sbike)
# Correlations without categorical variables
print(cor(sbike[,c(-6, -7, -8, -9)]),3)

# RSS is massive for both linear models
lm0 <- lm(cnt ~ t1, sbike)
lm1 <- lm(cnt ~ t1 + t2, sbike)

anova(lm0, lm1)

plot(sbike$hour, sbike$cnt)
plot(hol_sbike$hour, hol_sbike$cnt)

a <- lm(cnt ~ ., data = bike)
b <- glm(formula = cnt ~ ., family = "poisson", data = bike)


boxCox(lm(cnt ~ t1, data = filter(bike, cnt != 0)))
boxCox(lm(BigMac ~ FoodIndex, data = mac))


######## Plots: we should break up some of these factors
plot(bike$month,bike$cnt)
plot(bike$hour,bike$cnt) # group hours 23-0, 1-5, 6-9, 10-15, 16-19, 20-22
