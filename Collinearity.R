---
  title: "Project"
output: html_document
---

  


```{r}
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  #ET keep for ellie
#setwd(~/Desktop/SS504_bike_sharing)
source("dataCleaning.R")
```



## Colinearity

```{r}
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
```


```{r}
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
```
This suggests us to remove t1. 


## Vanilla Linear Regression
```{r}
reduced_data = subset(data, select=-c(t1, day, month, year, weekday, work))
lm_normal = lm(cnt~. ,data=reduced_data)
par(mfrow=c(2,2))
plot(lm_normal, which=c(1,2,3,4))
BIC(lm_normal)
```

