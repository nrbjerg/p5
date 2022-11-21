library(dplyr)

### Dividing the Data Set in Municipalities ###
training_df_no_outliers = training_df[-c(2, 4, 8, 17, 19, 29, 77, 116, 255, 471,
                                         486, 545),]
df_Aarhus <- subset(training_df_no_outliers, Municipality == 'Aarhus')
test_df_Aarhus <- subset(test_df, Municipality == 'Aarhus')

### Model for Aarhus ###

# Full model restricted to Aarhus Omit variables Trend, Year, Voting_Area,
# Parish, Municipality.
GLM_Aarhus_Full <- lm(data = df_Aarhus,
                          ln_Price ~ Rooms + Ground_Area + Home_Area +
                            Distance_School + Distance_City_Hall + Age + Wealthy)
summary(GLM_Aarhus_Full)
plot(GLM_Aarhus_Full)

#Remove variables due to Cook's distance and leverage
remove_points_with_to_high_leverage <- function (mod, df) {
  k <- length(coefficients(mod))
  border <- 3 * k / nrow(df)
  indicies_to_drop <- c()
  leverages <- hatvalues(mod)
  for (row in 1:nrow(df)) {
    if (leverages[row] > border) {
      indicies_to_drop <- append(row, indicies_to_drop)
    }
  }
  print("Removing indicies with leverages:")
  print(leverages[indicies_to_drop])
  return(df[-indicies_to_drop,])
}

dfrem_Aarhus <- remove_points_with_to_high_leverage(GLM_Aarhus_Full,df_Aarhus)
GLMrem_Aarhus_Full <- lm(data = dfrem_Aarhus,
                           ln_Price ~ Rooms + Ground_Area + Home_Area +
                             Distance_School + Distance_City_Hall + Age + Wealthy)
summary(GLMrem_Aarhus_Full)
plot(GLMrem_Aarhus_Full)

#Reduce the model due to F-test and Pr(>t)
GLMrem_Aarhus_red1 <- lm(data = dfrem_Aarhus,
                            ln_Price ~ Rooms + Home_Area +
                              Distance_School + Distance_City_Hall + Age + Wealthy)
summary(GLMrem_Aarhus_red1)
anova(GLMrem_Aarhus_Full,GLMrem_Aarhus_red1)

GLMrem_Aarhus_red2 <- lm(data = dfrem_Aarhus,
                            ln_Price ~ Rooms + Home_Area +
                              Distance_School + Distance_City_Hall + Wealthy)
summary(GLMrem_Aarhus_red2)
anova(GLMrem_Aarhus_Full,GLMrem_Aarhus_red2)

GLMrem_Aarhus_red3 <- lm(data = dfrem_Aarhus,
                            ln_Price ~ Rooms + Home_Area +
                              Distance_City_Hall + Wealthy)
summary(GLMrem_Aarhus_red3)
anova(GLMrem_Aarhus_Full,GLMrem_Aarhus_red3)

GLMrem_Aarhus_red4 <- lm(data = dfrem_Aarhus,
                            ln_Price ~ Rooms + Home_Area +
                            Wealthy)
summary(GLMrem_Aarhus_red4)
anova(GLMrem_Aarhus_Full,GLMrem_Aarhus_red4)

GLMrem_Aarhus_red5 <- lm(data = dfrem_Aarhus,
                         ln_Price ~ Home_Area +
                           Wealthy)
summary(GLMrem_Aarhus_red5)
anova(GLMrem_Aarhus_Full,GLMrem_Aarhus_red5)
plot(GLMrem_Aarhus_red5)


#Plot of Residuels
res_Aarhus <- resid(GLMrem_Aarhus_red4)
plot(fitted(GLMrem_Aarhus_red5), res_Aarhus,
     xlab = "Fitted Values", ylab = "Residuals")
abline(0,0)

# Smooth density plot:
plot(density(rstandard(GLMrem_Aarhus_red5)))
plot(density(dfrem_Aarhus$ln_Price))



#Prediction
pred_confint = as.data.frame(predict(GLMrem_Aarhus_red5, df_Aarhus,
                       interval = 'confidence'))


df_Aarhus$Fitted <- pred_confint$fit
df_Aarhus$Lower_Bound_Confint <- pred_confint$lwr
df_Aarhus$Upper_Bound_Confint <- pred_confint$upr

predint = as.data.frame(predict(GLMrem_Aarhus_red5, df_Aarhus,
                  interval = 'prediction'))

df_Aarhus$Lower_Bound_Predint <- predint$lwr
df_Aarhus$Upper_Bound_Predint <- predint$upr

# Plotting the confidence and prediction intervals against fitted values.
df_Aarhus_arranged = arrange(df_Aarhus, Fitted)

plot(data = df_Aarhus_arranged, ln_Price ~ Fitted,
     xlab = "Predictions", ylab = "Logarithm of Price")

lines(df_Aarhus_arranged$Fitted, df_Aarhus_arranged$Fitted,
      col = "Red", lwd = 1.5)
lines(df_Aarhus_arranged$Fitted, df_Aarhus_arranged$Lower_Bound_Confint,
      col = "Green", lwd = 1.5)
lines(df_Aarhus_arranged$Fitted, df_Aarhus_arranged$Upper_Bound_Confint,
      col = "Green", lwd = 1.5)
lines(df_Aarhus_arranged$Fitted, df_Aarhus_arranged$Lower_Bound_Predint,
      col = "Blue", lwd = 1.5)
lines(df_Aarhus_arranged$Fitted, df_Aarhus_arranged$Upper_Bound_Predint,
      col = "Blue", lwd = 1.5)

# Counting the number of data points inside the prediction interval.
count_inside_prediction = 0

for (i in 1:166)
{if (df_Aarhus_arranged$Lower_Bound_Predint[i] <= df_Aarhus_arranged$ln_Price[i]
     && df_Aarhus_arranged$ln_Price[i] <= df_Aarhus_arranged$Upper_Bound_Predint[i]) {
  count_inside_prediction = count_inside_prediction + 1
} else {
  0
}}

count_inside_prediction
count_inside_prediction / 166

# We now replicate the above, but predicting the test data frame. First, we
# calculate the predictions, and prediction and confidence intervals.
pred_confint = as.data.frame(predict(GLMrem_Aarhus_red5, test_df,
                       interval = 'confidence'))

test_df$Fitted <- pred_confint$fit
test_df$Lower_Bound_Confint <- pred_confint$lwr
test_df$Upper_Bound_Confint <- pred_confint$upr

predint = predict(GLMrem_Aarhus_red5, test_df,
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
{if (test_df_arranged$Lower_Bound_Predint[i] <= log(test_df_arranged$Price[i])
     && log(test_df_arranged$Price[i]) <= test_df_arranged$Upper_Bound_Predint[i]) {
  count_inside_prediction = count_inside_prediction + 1
} else {
  0
}}

count_inside_prediction
count_inside_prediction / 43

# Counting the number of data points with log(Price) larger than the prediction.
count_bigger_than_prediction = 0

for (i in 1:43)
{if (test_df_arranged$Fitted[i] < test_df_arranged$ln_Price[i]) {
  count_bigger_than_prediction = count_bigger_than_prediction + 1
} else {
  0
}}

count_bigger_than_prediction
count_bigger_than_prediction / 43
