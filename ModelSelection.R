
  
### LOOCV for different models----
loocv.lm1 <- function(md1) {
  return(mean((residuals(md1)/(1-hatvalues(md1)))^2))
}
loocv.lm2 <- function(res, hat) {
  return(mean((res/(1-hat))^2))
}

loocv.lm1(fit_poi)
loocv.lm1(fit_gamma_log)
loocv.lm1(lm_normal)
loocv.lm2(original_y-reduced_data$cnt, exp(lm_log_normal$fitted.values+(summary(lm_log_normal)$sigma)^2/2))



# LOOCV - Gamma----
non_neg_data = reduced_data[-2016,]
res = 0
i = 1
while (i < nrow(non_neg_data)) {
  curr_fit = glm(cnt~., data=non_neg_data[-c(i:min(i+499, nrow(non_neg_data))),], family=Gamma(link="log")) # Fit model by removing the i-th row.
  new = non_neg_data[c(i:min(i+499, nrow(non_neg_data))),]
  res = res + sum((predict(curr_fit, new[-1], se.fit = TRUE)$fit - new$cnt)^2) # Predict the value, compute the squared residual.
  i = i + 500
}
print(res/nrow(non_neg_data))


# LOOCV-Poisson----
res = 0
i = 1
while (i < nrow(reduced_data)) {
  print(i)
  curr_fit = glm(cnt~., data=reduced_data[-c(i:min(i+499, nrow(reduced_data))),], family="poisson") # Fit model by removing the i-th row.
  new = reduced_data[c(i:min(i+499, nrow(reduced_data))),]
  res = res + sum((predict(curr_fit, new[-1], se.fit = TRUE)$fit - new$cnt)^2) # Predict the value, compute the squared residual.
  i = i + 500
}
print(res/nrow(reduced_data))

res = 0
i = 1
while (i < nrow(reduced_data)) {
  curr_fit = lm(cnt~., data=reduced_data[-c(i:min(i+499, nrow(reduced_data))),]) # Fit model by removing the i-th row.
  new = reduced_data[c(i:min(i+499, nrow(reduced_data))),]
  res = res + sum((predict(curr_fit, new[-1], se.fit = TRUE)$fit - new$cnt)^2) # Predict the value, compute the squared residual.
  i = i + 500
}
print(res/nrow(reduced_data))



# LOOCV - LogNormal----
res = 0
i = 1

temp = reduced_data[-2016,]
while (i < nrow(reduced_data)) {
  curr_fit = lm(cnt~., data=log_transformed[-c(i:min(i+499, nrow(log_transformed))),]) # Fit model by removing the i-th row.
  new = temp[c(i:min(i+499, nrow(temp))),]
  fitted = predict(curr_fit, new[-1], se.fit = TRUE)$fit
  res = res + sum((exp(fitted+(summary(curr_fit)$sigma)^2/2) - new$cnt)^2) # Predict the value, compute the squared residual.
  i = i + 500
}
print(res/nrow(reduced_data))


# backward/forward selection ----
forward_backward_selection <- function(dataset, model) {
  num_data = nrow(dataset)
  model_empty = lm(cnt~1 ,data=dataset)
  model_full = model
  fit_backward = step(model_full, direction="backward", k=log(num_data))
  fit_forward = step(model_empty, scope= list(upper=model_full, lower=model_empty), direction="forward", k = log(num_data))
  result_list = list(forward = fit_forward, backward = fit_backward)
  return(result_list)
}

model_1 = lm(cnt ~ t2 + hum + wind_speed + weather_code + season + work + eye_hrs + work:eye_hrs, data = hour_data)
forward_backward_selection(hour_data, model_1)
hour_data = filter(hour_data, cnt != 0)
model_2 = lm(log(cnt) ~ t2 + hum + wind_speed + weather_code + season + work + eye_hrs + work:eye_hrs, data = hour_data)
forward_backward_selection(hour_data, model_2)

