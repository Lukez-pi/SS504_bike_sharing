---
  title: "Project"
output: html_document
---



## 
We can also try to use BoxCox to transform our data.
```{r}
boxFit = lm(cnt~. ,data=reduced_data[-2016,])
boxcox(boxFit, plotit=T)
abline(v=0.14)
```
The best lambda seem to be roughly 0.14 Therefore, the transforemed response $g_{\lambda}(y) = \frac{y^{(0.14)}-1}{0.14} = 2-2y^{-0.5}$
  ```{r}
transformed = reduced_data[-2016,]
log_transformed = reduced_data[-2016,]
log_transformed$cnt = log(log_transformed$cnt) 
transformed$cnt = (transformed$cnt^0.14-1)/0.14
lm_transformed_normal = lm(cnt~. ,data=transformed)
lm_log_normal = lm(cnt~. ,data=log_transformed)
par(mfrow=c(2,2))
plot(lm_transformed_normal, which=c(1,2,3,4))
```
```{r}
plot(lm_log_normal, which=c(1,2,3,4))
BIC(lm_transformed_normal)
BIC(lm_log_normal)
```


The upper tail fits much better after transformation. But the lower tail seemed to be worse. May consider use Logistic regression to separate our data in to two categories.

We can also review the residuals in the model of log_transformation.
```{r}
original_y = exp(lm_log_normal$fitted.values+(summary(lm_log_normal)$sigma)^2/2)
qqnorm(original_y-reduced_data$cnt)
```


```{r}
fit0 = glm(cnt~., data=reduced_data, family ="gaussian")
fit_poi = glm(cnt~., data=reduced_data, family ="poisson")
fit_gamma_log = glm(cnt~., data=reduced_data[-2016,] , family =Gamma(link="log"))
#fit_gamma_identity = glm(cnt~., data=reduced_data[-2016,], family =Gamma(link="identity"))
BIC(fit0)
BIC(fit_poi)
BIC(fit_gamma_log)
```
It seemed that a fit with Gamma distribution is also good.

To look for best model under the gamma fit and linear fit, may choose forward or backward selection. Subset is almost impossible as we have too many dummy variables, boosting the number of possible models.