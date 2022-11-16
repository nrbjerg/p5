library(dplyr)

### Multiple Linear Regression ###

# We base a model of simple multiple linear regression with the response 
# variable:
# Price

# And explanatory variables:
# Rooms, Ground_Area, Home_Area, Distance_School, Distance_City_Hall, Age, 
# Wealthy, and Municipality

mod_full <- lm(data = training_df, Price ~ Rooms + Ground_Area + Home_Area + 
               Distance_School + Distance_City_Hall + Age + Wealthy + 
               Municipality)
summary(mod_full)

# The above model is okay by R-squared values and F-test. 

# Proceeding by removing the parameter associated with the highest p-value from 
# the t-test and iterating we remove Distance_School, Distance_City_Hall, and 
# Age. The stopping criteria is not no p-value exceeds a 5% significance level.

mod_reduced = lm(data = training_df, Price ~ Rooms + Ground_Area + Home_Area + 
                 Wealthy + Municipality)
summary(mod_reduced)

# The above model is okay by R-squared values and F-test with almost no change
# to the full model.

# We test the reduction against the full model with F-test, which it passes.
anova(mod_full, mod_reduced)



### Model Checking ###

# We make different residual plots checking both the independence of residuals 
# from fitted values, the distribution of the residuals, and the leverage and 
# Cook's distance of the residuals.
par(mfrow = c(2, 2))

plot(mod_reduced, which = 1, caption = "", sub.caption = "")
plot(mod_reduced, which = 2, caption = "", sub.caption = "")
# plot(mod_reduced, which = 3, caption = "", sub.caption = "")
# plot(mod_reduced, which = 4, caption = "", sub.caption = "")
plot(mod_reduced, which = 5, caption = "", sub.caption = "")
plot(mod_reduced, which = 6, caption = "", sub.caption = "")

# We see that there are problems with the residual plot, since we seem to have
# a trumpet shape, that is the variance of the residuals become larger for
# larger fitted values. Also large standardized residuals deviate from the 
# normal distribution. Thus something does not satisfy the assumptions of
# general linear models.

# Outliers from the leverages.
leverages <- hatvalues(mod_reduced)

for (i in 1:820){
  if (leverages[i] > 3*8/820){
    print(i)
  }
}

# Outliers from the Cook's distances.
cooks_distance <- cooks.distance(mod_reduced)

for (i in 1:820){
  if (cooks_distance[i] >= 0.5){
    print(i)
  }
}

# Clean
rm(mod_full, mod_reduced)
rm(leverages, cooks_distance, i)





### Log Model ###

# We base a model of simple multiple linear regression with the response 
# variable:
# ln_Price

# And explanatory variables:
# Rooms, Ground_Area, Home_Area, Distance_School, Distance_City_Hall, Age, 
# Wealthy, and Municipality 

ln_mod_full <- lm(data = training_df, ln_Price ~ Rooms + Ground_Area + 
                  Home_Area + Distance_School + Distance_City_Hall + Age + 
                  Wealthy + Municipality)
summary(ln_mod_full)

# The above model is okay by R-squared values and F-test with R-squared values 
# better than the linear regression model. 

# Proceding as before we remove 

ln_mod_reduced <- lm(data = training_df, ln_Price ~ Rooms + Ground_Area + 
                     Home_Area + Distance_City_Hall + Age + Wealthy + 
                     Municipality)
summary(ln_mod_reduced)

# The above model is okay by R-squared values and F-test with almost no change
# to the full model.

# We test the reduction against the full model with F-test, which it passes.
anova(ln_mod_full, ln_mod_reduced)



### Model Checking ###

# We make different residual plots checking both the independence of residuals 
# from fitted values, the distribution of the residuals, and the leverage and 
# Cook's distance of the residuals.
par(mfrow = c(2, 2))

plot(ln_mod_reduced, which = 1, caption = "", sub.caption = "")
plot(ln_mod_reduced, which = 2, caption = "", sub.caption = "")
# plot(ln_mod_reduced, which = 3, caption = "", sub.caption = "")
# plot(ln_mod_reduced, which = 4, caption = "", sub.caption = "")
plot(ln_mod_reduced, which = 5, caption = "", sub.caption = "")
plot(ln_mod_reduced, which = 6, caption = "", sub.caption = "")

# The residual plot and other plots have improved monumentally.

# Outliers from the residuals.
residuals <- resid(ln_mod_reduced)

for (i in 1:820){
  if (abs(residuals[i]) > 1){
    print(i)
  }
}

# Outliers from the leverages.
leverages <- hatvalues(ln_mod_reduced)

for (i in 1:820){
  if (leverages[i] > 3*10/820){
    print(i)
  }
}

# Outliers from the Cook's distances.
cooks_distance <- cooks.distance(ln_mod_reduced)

for (i in 1:820){
  if (cooks_distance[i] >= 0.5){
    print(i)
  }
}

# The outliers are analysed. The point 
training_df[c(2, 4, 8, 17, 19, 29, 77, 116, 255, 471, 486, 545),]

training_df_no_outliers = training_df[-c(2, 4, 8, 17, 19, 29, 77, 116, 255, 471, 
                                         486, 545),]

ln_mod_reduced_no_outliers <- lm(data = training_df_no_outliers, 
                                 ln_Price ~ Rooms + Ground_Area + Home_Area + 
                                 Distance_School + Distance_City_Hall + Age + 
                                 Wealthy + Municipality)
summary(ln_mod_reduced_no_outliers)

# This improved the R-squared values. We reduced again.

ln_mod_further_reduced_no_outliers <- lm(data = training_df_no_outliers, 
                                         ln_Price ~ Home_Area + 
                                         Distance_City_Hall + Wealthy + 
                                         Municipality)
summary(ln_mod_further_reduced_no_outliers)

anova(ln_mod_reduced_no_outliers, ln_mod_further_reduced_no_outliers)

# We get residual plots:
par(mfrow = c(2, 2))

plot(ln_mod_further_reduced_no_outliers, which = 1, caption = "", sub.caption = "")
plot(ln_mod_further_reduced_no_outliers, which = 2, caption = "", sub.caption = "")
# plot(ln_mod_further_reduced_no_outliers, which = 3, caption = "", sub.caption = "")
# plot(ln_mod_further_reduced_no_outliers, which = 4, caption = "", sub.caption = "")
plot(ln_mod_further_reduced_no_outliers, which = 5, caption = "", sub.caption = "")
plot(ln_mod_further_reduced_no_outliers, which = 6, caption = "", sub.caption = "")

# Outliers from the residuals.
residuals <- resid(ln_mod_reduced_no_outliers)

for (i in 1:802){
  if (abs(residuals[i]) > 1){
    print(i)
  }
}

# Outliers from the leverages.
leverages <- hatvalues(ln_mod_reduced_no_outliers)

for (i in 1:802){
  if (leverages[i] > 3*7/802){
    print(i)
  }
}

# Outliers from the Cook's distances.
cooks_distance <- cooks.distance(ln_mod_reduced_no_outliers)

for (i in 1:802){
  if (cooks_distance[i] >= 0.5){
    print(i)
  }
}



### Predictions ###

par(mfrow = c(1, 2))

# Calculating and adding predictions of the training data based on the reduced 
# log model together with prediction and confidence intervals.
pred_confint = predict(ln_mod_reduced_no_outliers, training_df_no_outliers, 
                       interval = 'confidence')
pred_confint = as.data.frame(pred_confint)

training_df_no_outliers$Fitted <- pred_confint$fit
training_df_no_outliers$Lower_Bound_Confint <- pred_confint$lwr
training_df_no_outliers$Upper_Bound_Confint <- pred_confint$upr

predint = predict(ln_mod_reduced_no_outliers, training_df_no_outliers, 
                  interval = 'prediction')
predint = as.data.frame(predint)

training_df_no_outliers$Lower_Bound_Predint <- predint$lwr
training_df_no_outliers$Upper_Bound_Predint <- predint$upr

# Plotting the confidence and prediction intervals against fitted values.
training_df_no_outliers_arranged = arrange(training_df_no_outliers, Fitted)

plot(data = training_df_no_outliers_arranged, ln_Price ~ Fitted,
     xlab = "Predictions", ylab = "Logarithm of Price")

lines(training_df_no_outliers_arranged$Fitted, training_df_no_outliers_arranged$Fitted, 
      col = "Red", lwd = 1.5)
lines(training_df_no_outliers_arranged$Fitted, training_df_no_outliers_arranged$Lower_Bound_Confint, 
      col = "Green", lwd = 1.5)
lines(training_df_no_outliers_arranged$Fitted, training_df_no_outliers_arranged$Upper_Bound_Confint, 
      col = "Green", lwd = 1.5)
lines(training_df_no_outliers_arranged$Fitted, training_df_no_outliers_arranged$Lower_Bound_Predint, 
      col = "Blue", lwd = 1.5)
lines(training_df_no_outliers_arranged$Fitted, training_df_no_outliers_arranged$Upper_Bound_Predint, 
      col = "Blue", lwd = 1.5)

# Counting the number of data points inside the prediction interval.
count_inside_prediction = 0

for (i in 1:802)
{if (training_df_no_outliers_arranged$Lower_Bound_Predint[i] <= training_df_no_outliers_arranged$ln_Price[i] 
     && training_df_no_outliers_arranged$ln_Price[i] <= training_df_no_outliers_arranged$Upper_Bound_Predint[i]) {
  count_inside_prediction = count_inside_prediction + 1
} else {
  0
}}

count_inside_prediction
count_inside_prediction / 814

# We now replicate the above, but predicting the test data frame. First, we
# calculate the predictions, and prediction and confidence intervals.
pred_confint = predict(ln_mod_reduced_no_outliers, test_df, 
                       interval = 'confidence')
pred_confint = as.data.frame(pred_confint)

test_df$Fitted <- pred_confint$fit
test_df$Lower_Bound_Confint <- pred_confint$lwr
test_df$Upper_Bound_Confint <- pred_confint$upr

predint = predict(ln_mod_reduced_no_outliers, test_df, 
                  interval = 'prediction')
predint = as.data.frame(predint)

test_df$Lower_Bound_Predint <- predint$lwr
test_df$Upper_Bound_Predint <- predint$upr

# Plotting the confidence and prediction intervals against fitted values.
test_df_arranged = arrange(test_df, Fitted)

plot(data = test_df_arranged, ln_Price ~ Fitted,
     xlab = "Predictions", ylab = "Logarithm of Price")

lines(test_df_arranged$Fitted, test_df_arranged$Fitted, 
      col = "Red", lwd = 1.5)
lines(test_df_arranged$Fitted, test_df_arranged$Lower_Bound_Confint, 
      col = "Green", lwd = 1.5)
lines(test_df_arranged$Fitted, test_df_arranged$Upper_Bound_Confint, 
      col = "Green", lwd = 1.5)
lines(test_df_arranged$Fitted, test_df_arranged$Lower_Bound_Predint, 
      col = "Blue", lwd = 1.5)
lines(test_df_arranged$Fitted, test_df_arranged$Upper_Bound_Predint, 
      col = "Blue", lwd = 1.5)

# Counting the number of data points inside the prediction interval.
count_inside_prediction = 0

for (i in 1:43)
{if (test_df_arranged$Lower_Bound_Predint[i] <= test_df_arranged$ln_Price[i] 
     && test_df_arranged$ln_Price[i] <= test_df_arranged$Upper_Bound_Predint[i]) {
  count_inside_prediction = count_inside_prediction + 1
} else {
  0
}}

count_inside_prediction
count_inside_prediction / 43

# Counting the number of data points with ln_Price larger than the prediction.
count_bigger_than_prediction = 0

for (i in 1:43)
{if (test_df_arranged$Fitted[i] < test_df_arranged$ln_Price[i]) {
  count_bigger_than_prediction = count_bigger_than_prediction + 1
} else {
  0
}}

count_bigger_than_prediction
count_bigger_than_prediction / 43


# Clean
rm(ln_mod_full, ln_mod_reduced, ln_mod_reduced_no_outliers, 
   ln_mod_further_reduced_no_outliers)
rm(training_df_no_outliers, training_df_no_outliers_arranged, test_df_arranged)
rm(leverages, cooks_distance, residuals, pred_confint, predint, count_inside_prediction, count_bigger_than_prediction, i)

