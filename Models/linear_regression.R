library(dplyr)
library(Metrics)

### General Functions ###

residual_plots <- function(model){
  # Plots the standard diagnostic plots.
  par(mfrow = c(1, 2))
  
  plot(model, which = 1, caption = "", sub.caption = "")
  plot(model, which = 2, caption = "", sub.caption = "")
  plot(model, which = 3, caption = "", sub.caption = "")
  # plot(mod_reduced, which = 4, caption = "", sub.caption = "")
  plot(model, which = 5, caption = "", sub.caption = "")
  # plot(mod_reduced, which = 6, caption = "", sub.caption = "")
  
  par(mfrow = c(1, 1))
}

extra_residual_plots <- function(model, data_frame){
  # Plots the residuals against other variables.
  residuals <- resid(model)
  
  par(mfrow = c(2, 2))
  
  plot(data_frame$Trend, residuals,
       xlab = "Trend", ylab = "Residuals")
  abline(0,0)
  plot(data_frame$Rooms, residuals,
       xlab = "Rooms", ylab = "Residuals")
  abline(0,0)
  plot(data_frame$Ground_Area, residuals,
       xlab = "Ground Area", ylab = "Residuals")
  abline(0,0)
  plot(data_frame$Home_Area, residuals,
       xlab = "Home Area", ylab = "Residuals")
  abline(0,0)
  
  rm(residuals)
  
  par(mfrow = c(1, 1))
}

leverages_cooks_distance <- function(model){
  # Finds the large leverage points.
  leverages <- hatvalues(model)
  large_leverage <- c()
  
  for (i in 1:length(leverages)){
    if (leverages[i] > 3 * sum(leverages) / length(leverages)){
      large_leverage <- append(large_leverage, i)
    }
  }
  
  print("The large leverage points are:")
  print(large_leverage)
  
  # Finds the influencial points.
  cooks_distance <- cooks.distance(model)
  influencial <- c()
  
  for (i in 1:length(cooks_distance)){
    if (cooks_distance[i] >= 0.5){
      influencial <- append(influencial, i)
    }
  }
  
  print("The influencial points are:")
  print(influencial)
  
  large_leverage_influencial <- append(large_leverage, influencial)
  rm(large_leverage, influencial, i)
  
  return (large_leverage_influencial)
}

predictions <- function(model, data_frame, response, response_text){
  # Defines the prediction, confidence intervals, and prediction intervals.
  pred_confint = predict(model, data_frame, 
                         interval = 'confidence')
  pred_confint = as.data.frame(pred_confint)
  
  data_frame$Fitted <- pred_confint$fit
  data_frame$Lower_Bound_Confint <- pred_confint$lwr
  data_frame$Upper_Bound_Confint <- pred_confint$upr
  
  predint = predict(model, data_frame, 
                    interval = 'prediction')
  predint = as.data.frame(predint)
  
  data_frame$Lower_Bound_Predint <- predint$lwr
  data_frame$Upper_Bound_Predint <- predint$upr
  
  # Plotting the confidence and prediction intervals against fitted values.
  data_frame_arranged = arrange(data_frame, Fitted)
  
  data_frame_arranged[[response]]
  plot(data_frame_arranged$Fitted, data_frame_arranged[[response]],
       xlab = "Predictions", ylab = response_text)
  
  lines(data_frame_arranged$Fitted, data_frame_arranged$Fitted, 
        col = "Red", lwd = 1.5)
  lines(data_frame_arranged$Fitted, data_frame_arranged$Lower_Bound_Confint, 
        col = "Green", lwd = 1.5)
  lines(data_frame_arranged$Fitted, data_frame_arranged$Upper_Bound_Confint, 
        col = "Green", lwd = 1.5)
  lines(data_frame_arranged$Fitted, data_frame_arranged$Lower_Bound_Predint, 
        col = "Blue", lwd = 1.5)
  lines(data_frame_arranged$Fitted, data_frame_arranged$Upper_Bound_Predint, 
        col = "Blue", lwd = 1.5)
  
  # Counting the number of data points inside the prediction interval.
  count_inside_prediction = 0
  
  for (i in 1:length(data_frame_arranged[[response]]))
  {if (data_frame_arranged$Lower_Bound_Predint[i] <= data_frame_arranged[[response]][i] 
       && data_frame_arranged[[response]][i] <= data_frame_arranged$Upper_Bound_Predint[i]) {
    count_inside_prediction = count_inside_prediction + 1
  }}
  
  print("Count of how many actual values lie in the corresponding prediction interval:")
  print(count_inside_prediction)
  print(count_inside_prediction / length(data_frame_arranged[[response]]))
  
  # Counting the number of data points with response larger than the prediction.
  count_bigger_than_prediction = 0
  
  for (i in 1:length(data_frame_arranged[[response]]))
  {if (data_frame_arranged$Fitted[i] < data_frame_arranged[[response]][i]) {
    count_bigger_than_prediction = count_bigger_than_prediction + 1
  }}
  
  print("Count of how many actual values lie above the fitted line:")
  print(count_bigger_than_prediction)
  print(count_bigger_than_prediction / length(data_frame_arranged[[response]]))
  
  print("Root Mean Square Error:")
  print(rmse(data_frame_arranged[[response]], data_frame_arranged$Fitted))
  
  rm(pred_confint, predint, count_inside_prediction, count_bigger_than_prediction, i, data_frame_arranged)
}





### Model with Linear Regression ###

# Constructing the full model with linear regression and response Price.
mod_full <- lm(data = training_df, Price ~ Rooms + Ground_Area + Home_Area + 
               Distance_School + Distance_City_Hall + Age + Wealthy + 
               Municipality)
summary(mod_full)

# Reducing with backward elimination.
mod_reduced = lm(data = training_df, Price ~ Rooms + Ground_Area + Home_Area + 
                 Wealthy + Municipality)
summary(mod_reduced)

# F-test for the reduction.
anova(mod_full, mod_reduced)

# Model checking with different plots.
residual_plots(mod_reduced)

# Outliers from leverage and Cook's distance.
leverages_cooks_distance(mod_reduced)

# Clean
rm(mod_full, mod_reduced)





### Log Model ###

# Constructing the full model with linear regression and response ln_Price.
ln_mod_full <- lm(data = training_df, ln_Price ~ Rooms + Ground_Area + 
                  Home_Area + Distance_School + Distance_City_Hall + Age + 
                  Wealthy + Municipality)
summary(ln_mod_full)

# Reducing with backward elimination.
ln_mod_reduced <- lm(data = training_df, ln_Price ~ Rooms + Ground_Area + 
                     Home_Area + Distance_City_Hall + Age + Wealthy + 
                     Municipality)
summary(ln_mod_reduced)

# F-test for the reduction.
anova(ln_mod_full, ln_mod_reduced)

# Model checking with different plots.
residual_plots(ln_mod_reduced)

# Outliers from the residuals.
outliers <- c()
residuals <- resid(ln_mod_reduced)

for (i in 1:length(residuals)){
  if (abs(residuals[i]) > 1){
    outliers <- append(outliers, i)
  }
}

outliers

# Outliers from leverage and Cook's distance.
outliers <- append(outliers, leverages_cooks_distance(ln_mod_reduced))

# Residual plots with a selection of explanatory variables and Trend.
extra_residual_plots(ln_mod_reduced, training_df)

# The outliers are analysed. The points are removed and a new model developed.
training_df[outliers,]

training_df_no_outliers = training_df[-outliers,]

rm(ln_mod_full, ln_mod_reduced)

ln_mod_full_no_outliers <- lm(data = training_df_no_outliers, 
                                 ln_Price ~ Rooms + Ground_Area + Home_Area + 
                                 Distance_School + Distance_City_Hall + Age + 
                                 Wealthy + Municipality)
summary(ln_mod_full_no_outliers)

rm(outliers, residuals, i)

# Reducing with backward elimination.

ln_mod_reduced_no_outliers <- lm(data = training_df_no_outliers, 
                                 ln_Price ~ Home_Area + Distance_City_Hall + 
                                 Wealthy + Municipality)
summary(ln_mod_reduced_no_outliers)

# F-test for the reduction.

anova(ln_mod_full_no_outliers, ln_mod_reduced_no_outliers)

# Residual plot with respect to Trend.
par(mfrow = c(1, 1))

res_df <- data.frame(Residuals = residuals(ln_mod_reduced_no_outliers), Trend = training_df_no_outliers$Trend)
ab <- coefficients(lm(data = res_df, Residuals ~ Trend))
plot(res_df$Trend, res_df$Residuals, xlab="Trend", ylab="Residuals")
abline(ab[1], ab[2], col="red")
abline(0,0)
rm(res_df, ab)

# Model checking with different plots.
residual_plots(ln_mod_reduced_no_outliers)

# Outliers from the residuals.
outliers <- c()
residuals <- resid(ln_mod_reduced_no_outliers)

for (i in 1:length(residuals)){
  if (abs(residuals[i]) > 1){
    outliers <- append(outliers, i)
  }
}

outliers

# Outliers from leverage and Cook's distance.
outliers <- append(outliers, leverages_cooks_distance(ln_mod_reduced_no_outliers))

rm(outliers, residuals, i)

# Residual plots with a selection of explanatory variables and Trend.
extra_residual_plots(ln_mod_reduced_no_outliers, training_df_no_outliers)





### Predictions ###

par(mfrow = c(1, 2))

# Plot the predicted values for the training set with confidence and prediction intervals.
predictions(ln_mod_reduced_no_outliers, training_df_no_outliers, "ln_Price", "Logarithm of Price")

# Plot the prediction values for the test set with confidence and prediction intervals.
predictions(ln_mod_reduced_no_outliers, test_df, "ln_Price", "Logarithm of Price")

rm(ln_mod_full_no_outliers, ln_mod_reduced_no_outliers)
