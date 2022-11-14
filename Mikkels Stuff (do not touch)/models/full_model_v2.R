### Full Model ###

# The full model with all variables included except Trend, Year, Voting_Area, 
# Parish, Month, Big_Ground.
GLM_Full <- lm(data = training_df, Price ~ Rooms + Ground_Area + Home_Area + 
               Distance_School + Distance_City_Hall + Age + Wealthy + 
               Municipality)
summary(GLM_Full)
# plot(GLM_Full)

# Residual plot:
res_full <- resid(GLM_Full)
plot(fitted(GLM_Full), res_full,
     xlab = "Fitted Values", ylab = "Residuals")
abline(0,0)

# Normal QQ-plot:
qqnorm(res_full, ylab = "Residuals")
qqline(res_full)

# Smooth density plot:
plot(density(res_full))

# The R-squared values and F-test gives reasonable values, which is a good sign.

# The following variables gives a high p-value from the t-test, hence we may 
# omit them:
# Distance_School, Distance_City_Hall, and Age.





### Reduced Model ###

# Reduce the full model based on t-values in a conservative way, that is the 
# above mentioned three variables.
GLM_Reduced <- lm(data = training_df, Price ~ Rooms + Ground_Area + Home_Area + Wealthy + 
                  Municipality)
summary(GLM_Reduced)
# plot(GLM_Reduced)

# Residual plot:
res_reduced <- resid(GLM_Reduced)
plot(fitted(GLM_Reduced), res_reduced,
     xlab = "Fitted Values", ylab = "Residuals")
abline(0,0)

# Normal QQ-plot:
qqnorm(res_reduced, ylab = "Residuals")
qqline(res_reduced)

# Smooth density plot:
plot(density(res_reduced))

# The R-squared values and F-test gives approximatively the same values, which 
# suggest that no accuracy is lost.

# F-testing the above null hypothesis. The F-test is confirms our reduction.
anova(GLM_Full, GLM_Reduced)

# All p-values of the t-test are more than reasonable, thus we conclude that no 
# further reduction is in our interest. We still try and reduce best on testing 
# the limits. The highest p-value is:
# Ground_Area





### Further Reduction with Ground_Area ###

# A further reduction from the reduced model removing Ground_Area.
GLM_Reduced_Further <- lm(data = training_df, Price ~ Rooms + Home_Area +
                          Wealthy + Municipality)
summary(GLM_Reduced_Further)

# Again we get good R-squared values, F-test, and t-test.

# F-testing the further reduction.
anova(GLM_Full, GLM_Reduced_Further)
anova(GLM_Reduced, GLM_Reduced_Further)

# The first test has p-value larger than 0.05. The second test has p-value 
# smaller than 0.05. Thus we may accept the reduction 11 from the full model, 
# but not from the reduced model.





### Transformed Model ###

# Basing a model of the reduced model, but now the logarithm has been taken of
# the variables Price, Home_Area, and Ground_Area.
GLM_Transform <- lm(data = training_df, ln_Price ~ Rooms + ln_Ground_Area + 
                    ln_Home_Area + Wealthy + Municipality)
summary(GLM_Transform)
# plot(GLM_Transform)

# Residual plot:
res_transform <- resid(GLM_Transform)
plot(fitted(GLM_Transform), res_transform,
     xlab = "Fitted Values", ylab = "Residuals")
abline(0,0)

# Normal QQ-plot:
qqnorm(res_transform, ylab = "Residuals")
qqline(res_transform)

# Smooth density plot:
plot(density(res_transform))

# The R-squared values are an improvement partly due to the smaller values, and
# the F-test still shows that the model i significant. Also the t-test is good.


# An alternative.
GLM_Transform <- lm(data = training_df, ln_Price ~ Rooms + Ground_Area + 
                    Home_Area + Wealthy + Municipality)
summary(GLM_Transform)
# plot(GLM_Transform)

# Residual plot:
res_transform <- resid(GLM_Transform)
plot(fitted(GLM_Transform), res_transform,
     xlab = "Fitted Values", ylab = "Residuals")
abline(0,0)

# Normal QQ-plot:
qqnorm(res_transform, ylab = "Residuals")
qqline(res_transform)

# Smooth density plot:
plot(density(res_transform))





rm(res_full, res_reduced, res_transform, GLM_Full, GLM_Reduced_Further)
library(dplyr)

### Prediction of the Sales with the Reduced Model ###

# Creating predictions of the general data frame based on the reduced model.
pred_confint = predict(GLM_Reduced, df, interval = 'confidence')
pred_confint = as.data.frame(pred_confint)

# Adding the predictions and associated confidence interval to the test data 
# frame.
df$Fitted <- pred_confint$fit
df$Lower_Bound_Confint <- pred_confint$lwr
df$Upper_Bound_Confint <- pred_confint$upr

# Creating prediction interval for the above predictions.
predint = predict(GLM_Reduced, df, interval = 'prediction')
predint = as.data.frame(predint)

# Adding the prediction interval to the test data frame.
df$Lower_Bound_Predint <- predint$lwr
df$Upper_Bound_Predint <- predint$upr

# Arrange the data frame by the Fitted variable.
df = arrange(df, Fitted)
df$Index = 1:863

# Plotting the confidence and prediction intervals against index.
plot(data = df, Price ~ Index)
lines(df$Index, df$Fitted, col = "Red", lwd = 1.5)
lines(df$Index, df$Lower_Bound_Confint, col = "Green", lwd = 1.5)
lines(df$Index, df$Upper_Bound_Confint, col = "Green", lwd = 1.5)
lines(df$Index, df$Lower_Bound_Predint, col = "Blue", lwd = 1.5)
lines(df$Index, df$Upper_Bound_Predint, col = "Blue", lwd = 1.5)

# Plotting the confidence and prediction intervals against fitted values.
plot(data = df, Price ~ Fitted)
lines(df$Fitted, df$Fitted, col = "Red", lwd = 1.5)
lines(df$Fitted, df$Lower_Bound_Confint, col = "Green", lwd = 1.5)
lines(df$Fitted, df$Upper_Bound_Confint, col = "Green", lwd = 1.5)
lines(df$Fitted, df$Lower_Bound_Predint, col = "Blue", lwd = 1.5)
lines(df$Fitted, df$Upper_Bound_Predint, col = "Blue", lwd = 1.5)

# Counting the number of Datapoints inside the prediction interval.
count_inside_prediction = 0

for (i in df$Index)
  {if (df$Lower_Bound_Predint[i] <= df$Price[i] && df$Price[i] <= df$Upper_Bound_Predint[i]) {
  count_inside_prediction = count_inside_prediction + 1
} else {
  0
}}

count_inside_prediction/863





### Prediction of the Sales in 2022 with the Reduced Model ###

# Creating predictions of the general data frame based on the reduced model.
pred_confint = predict(GLM_Reduced, test_df, interval = 'confidence')
pred_confint = as.data.frame(pred_confint)

# Adding the predictions and associated confidence interval to the test data 
# frame.
test_df$Fitted <- pred_confint$fit
test_df$Lower_Bound_Confint <- pred_confint$lwr
test_df$Upper_Bound_Confint <- pred_confint$upr

# Creating prediction interval for the above predictions.
predint = predict(GLM_Reduced, test_df, interval = 'prediction')
predint = as.data.frame(predint)

# Adding the prediction interval to the test data frame.
test_df$Lower_Bound_Predint <- predint$lwr
test_df$Upper_Bound_Predint <- predint$upr

# Arrange the data frame by the Fitted variable.
test_df = arrange(test_df, Fitted)
test_df$Index = 1:43

# Plotting the confidence and prediction intervals against index.
plot(data = df, Price ~ Index)
lines(df$Index, df$Fitted, col = "Red", lwd = 1.5)
lines(df$Index, df$Lower_Bound_Confint, col = "Green", lwd = 1.5)
lines(df$Index, df$Upper_Bound_Confint, col = "Green", lwd = 1.5)
lines(df$Index, df$Lower_Bound_Predint, col = "Blue", lwd = 1.5)
lines(df$Index, df$Upper_Bound_Predint, col = "Blue", lwd = 1.5)

# Plotting the confidence and prediction intervals against fitted values.
plot(data = df, Price ~ Fitted)
lines(df$Fitted, df$Fitted, col = "Red", lwd = 1.5)
lines(df$Fitted, df$Lower_Bound_Confint, col = "Green", lwd = 1.5)
lines(df$Fitted, df$Upper_Bound_Confint, col = "Green", lwd = 1.5)
lines(df$Fitted, df$Lower_Bound_Predint, col = "Blue", lwd = 1.5)
lines(df$Fitted, df$Upper_Bound_Predint, col = "Blue", lwd = 1.5)

# Counting the number of Datapoints inside the prediction interval.
count_inside_prediction = 0

for (i in test_df$Index)
{if (test_df$Lower_Bound_Predint[i] <= test_df$Price[i] && test_df$Price[i] <= test_df$Upper_Bound_Predint[i]) {
  count_inside_prediction = count_inside_prediction + 1
} else {
  0
}}

count_inside_prediction/43





### Prediction of the Sales with the Transformed Model ###

# Creating predictions of the general data frame based on the transformed model.
pred_confint = predict(GLM_Transform, df, interval = 'confidence')
pred_confint = as.data.frame(pred_confint)

# Adding the predictions and associated confidence interval to the test data 
# frame.
df$Fitted <- pred_confint$fit
df$Lower_Bound_Confint <- pred_confint$lwr
df$Upper_Bound_Confint <- pred_confint$upr

# Creating prediction interval for the above predictions.
predint = predict(GLM_Transform, df, interval = 'prediction')
predint = as.data.frame(predint)

# Adding the prediction interval to the test data frame.
df$Lower_Bound_Predint <- predint$lwr
df$Upper_Bound_Predint <- predint$upr

# Arrange the data frame by the Fitted variable.
df = arrange(df, Fitted)
df$Index = 1:863

# Plotting the confidence and prediction intervals against index.
plot(data = df, ln_Price ~ Index)
lines(df$Index, df$Fitted, col = "Red", lwd = 1.5)
lines(df$Index, df$Lower_Bound_Confint, col = "Green", lwd = 1.5)
lines(df$Index, df$Upper_Bound_Confint, col = "Green", lwd = 1.5)
lines(df$Index, df$Lower_Bound_Predint, col = "Blue", lwd = 1.5)
lines(df$Index, df$Upper_Bound_Predint, col = "Blue", lwd = 1.5)

# Plotting the confidence and prediction intervals against Fitted.
plot(data = df, ln_Price ~ Fitted)
lines(df$Fitted, df$Fitted, col = "Red")
lines(df$Fitted, df$Lower_Bound_Confint, col = "Green")
lines(df$Fitted, df$Upper_Bound_Confint, col = "Green")
lines(df$Fitted, df$Lower_Bound_Predint, col = "Blue")
lines(df$Fitted, df$Upper_Bound_Predint, col = "Blue")

# Counting the number of Datapoints inside the prediction interval.
count_inside_prediction = 0

for (i in df$Index)
{if (df$Lower_Bound_Predint[i] <= df$ln_Price[i] && df$ln_Price[i] <= df$Upper_Bound_Predint[i]) {
  count_inside_prediction = count_inside_prediction + 1
} else {
  0
}}

count_inside_prediction/863





### Prediction of the Sales  with the Transformed Model in 2022 ###

# Creating predictions of the data in 2022 based on the transformed model.
pred_confint = predict(GLM_Transform, test_df, interval = 'confidence')
pred_confint = as.data.frame(pred_confint)

# Adding the predictions and associated confidence interval to the test data 
# frame.
test_df$Fitted <- pred_confint$fit
test_df$Lower_Bound_Confint <- pred_confint$lwr
test_df$Upper_Bound_Confint <- pred_confint$upr

# Creating prediction interval for the above predictions.
predint = predict(GLM_Transform, test_df, interval = 'prediction')
predint = as.data.frame(predint)

# Adding the prediction interval to the test data frame.
test_df$Lower_Bound_Predint <- predint$lwr
test_df$Upper_Bound_Predint <- predint$upr

# Arrange the data frame by the Fitted variable.
test_df = arrange(test_df, Fitted)
test_df$Index = 1:43

# Plotting the confidence and prediction intervals against index.
plot(data = test_df, ln_Price ~ Index)
lines(test_df$Index, test_df$Fitted, col = "Red", lwd = 1.5)
lines(test_df$Index, test_df$Lower_Bound_Confint, col = "Green", lwd = 1.5)
lines(test_df$Index, test_df$Upper_Bound_Confint, col = "Green", lwd = 1.5)
lines(test_df$Index, test_df$Lower_Bound_Predint, col = "Blue", lwd = 1.5)
lines(test_df$Index, test_df$Upper_Bound_Predint, col = "Blue", lwd = 1.5)

# Plotting the confidence and prediction intervals against Fitted.
plot(data = test_df, ln_Price ~ Fitted)
lines(test_df$Fitted, test_df$Fitted, col = "Red")
lines(test_df$Fitted, test_df$Lower_Bound_Confint, col = "Green")
lines(test_df$Fitted, test_df$Upper_Bound_Confint, col = "Green")
lines(test_df$Fitted, test_df$Lower_Bound_Predint, col = "Blue")
lines(test_df$Fitted, test_df$Upper_Bound_Predint, col = "Blue")

# Counting the number of Datapoints inside the prediction interval.
count_inside_prediction = 0

for (i in test_df$Index)
{if (test_df$Lower_Bound_Predint[i] <= test_df$ln_Price[i] && test_df$ln_Price[i] <= test_df$Upper_Bound_Predint[i]) {
  count_inside_prediction = count_inside_prediction + 1
} else {
  0
}}

count_inside_prediction/43
