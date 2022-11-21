library(dplyr)

### Dividing the Data Set in Municipalities ###
training_df_no_outliers = training_df[-c(2, 4, 8, 17, 19, 29, 77, 116, 255, 471,
                                         486, 545),]
df_Copenhagen <- subset(training_df_no_outliers, Municipality == 'Copenhagen')
test_df_Copenhagen <- subset(test_df, Municipality == 'Copenhagen')

### Model for Copenhagen ###

# Full model restricted to Copenhagen Omit variables Trend, Year, Voting_Area,
# Parish, Municipality.
GLM_Copenhagen_Full <- lm(data = df_Copenhagen,
                      ln_Price ~ Rooms + Ground_Area + Home_Area +
                        Distance_School + Distance_City_Hall + Age + Wealthy)
summary(GLM_Copenhagen_Full)
plot(GLM_Copenhagen_Full)

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

dfrem_Copenhagen <- remove_points_with_to_high_leverage(GLM_Copenhagen_Full,df_Copenhagen)
GLMrem_Copenhagen_Full <- lm(data = dfrem_Copenhagen,
                         ln_Price ~ Rooms + Ground_Area + Home_Area +
                           Distance_School + Distance_City_Hall + Age + Wealthy)
summary(GLMrem_Copenhagen_Full)
plot(GLMrem_Copenhagen_Full)

#Reduce the model due to F-test and Pr(>t)
GLMrem_Copenhagen_red1 <- lm(data = dfrem_Copenhagen,
                         ln_Price ~ Rooms + Ground_Area + Home_Area +
                           Distance_School + Age + Wealthy)
summary(GLMrem_Copenhagen_red1)
anova(GLMrem_Copenhagen_Full,GLMrem_Copenhagen_red1)

GLMrem_Copenhagen_red2 <- lm(data = dfrem_Copenhagen,
                           ln_Price ~ Rooms + Home_Area +
                               Distance_School + Age + Wealthy)
summary(GLMrem_Copenhagen_red2)
anova(GLMrem_Copenhagen_Full,GLMrem_Copenhagen_red2)

GLMrem_Copenhagen_red3 <- lm(data = dfrem_Copenhagen,
                             ln_Price ~ Home_Area +
                               Distance_School + Age + Wealthy)
summary(GLMrem_Copenhagen_red3)
anova(GLMrem_Copenhagen_Full,GLMrem_Copenhagen_red3)

GLMrem_Copenhagen_red4 <- lm(data = dfrem_Copenhagen,
                             ln_Price ~ Home_Area + Age + Wealthy)
summary(GLMrem_Copenhagen_red4)
plot(GLMrem_Copenhagen_red4)
anova(GLMrem_Copenhagen_Full,GLMrem_Copenhagen_red4)


#Plot of Residuels
res_Copenhagen <- resid(GLMrem_Copenhagen_red4)
plot(fitted(GLMrem_Copenhagen_red4), res_Copenhagen,
     xlab = "Fitted Values", ylab = "Residuals")
abline(0,0)

# Smooth density plot:
plot(density(dfrem_Copenhagen$ln_Price))



#Prediction
pred_confint = as.data.frame(predict(GLMrem_Copenhagen_red4, df_Copenhagen,
                                     interval = 'confidence'))


df_Copenhagen$Fitted <- pred_confint$fit
df_Copenhagen$Lower_Bound_Confint <- pred_confint$lwr
df_Copenhagen$Upper_Bound_Confint <- pred_confint$upr

predint = as.data.frame(predict(GLMrem_Copenhagen_red4, df_Copenhagen,
                                interval = 'prediction'))

df_Copenhagen$Lower_Bound_Predint <- predint$lwr
df_Copenhagen$Upper_Bound_Predint <- predint$upr

# Plotting the confidence and prediction intervals against fitted values.
df_Copenhagen_arranged = arrange(df_Copenhagen, Fitted)

plot(data = df_Copenhagen_arranged, ln_Price ~ Fitted,
     xlab = "Predictions", ylab = "Logarithm of Price")

lines(df_Copenhagen_arranged$Fitted, df_Copenhagen_arranged$Fitted,
      col = "Red", lwd = 1.5)
lines(df_Copenhagen_arranged$Fitted, df_Copenhagen_arranged$Lower_Bound_Confint,
      col = "Green", lwd = 1.5)
lines(df_Copenhagen_arranged$Fitted, df_Copenhagen_arranged$Upper_Bound_Confint,
      col = "Green", lwd = 1.5)
lines(df_Copenhagen_arranged$Fitted, df_Copenhagen_arranged$Lower_Bound_Predint,
      col = "Blue", lwd = 1.5)
lines(df_Copenhagen_arranged$Fitted, df_Copenhagen_arranged$Upper_Bound_Predint,
      col = "Blue", lwd = 1.5)

# Counting the number of data points inside the prediction interval.
count_inside_prediction = 0

for (i in 1:105)
{if (df_Copenhagen_arranged$Lower_Bound_Predint[i] <= df_Copenhagen_arranged$ln_Price[i]
     && df_Copenhagen_arranged$ln_Price[i] <= df_Copenhagen_arranged$Upper_Bound_Predint[i]) {
  count_inside_prediction = count_inside_prediction + 1
} else {
  0
}}

count_inside_prediction
count_inside_prediction / 105

# We now replicate the above, but predicting the test data frame. First, we
# calculate the predictions, and prediction and confidence intervals.
pred_confint = as.data.frame(predict(GLMrem_Copenhagen_red4, test_df,
                                     interval = 'confidence'))

test_df$Fitted <- pred_confint$fit
test_df$Lower_Bound_Confint <- pred_confint$lwr
test_df$Upper_Bound_Confint <- pred_confint$upr

predint = predict(GLMrem_Copenhagen_red4, test_df,
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































































### Dividing the Data Set in Municipalities ###

df_copenhagen <- subset(training_df, Municipality == 'Copenhagen')
df_aarhus <- subset(training_df, Municipality == 'Aarhus')
df_odense <- subset(training_df, Municipality == 'Odense')
df_aalborg <- subset(training_df, Municipality == 'Aalborg')





### Model for Copenhagen ###

# Full model restricted to Copenhagen. Omit variables Trend, Year, Voting_Area, 
# Parish, Municipality.
GLM_Copenhagen_Full <- lm(data = df_copenhagen,
                          Price ~ Rooms + Ground_Area + Home_Area + 
                            Distance_School + Distance_City_Hall + Age + Wealthy)
summary(GLM_Copenhagen_Full)
plot(GLM_Copenhagen_Full)

#Remove outliers 
dfred_copenhagen <- df_copenhagen[-c(4,8),]

GLMred_Copenhagen_Full <- lm(data = dfred_copenhagen,
                          Price ~ Rooms + Ground_Area + Home_Area + 
                            Distance_School + Distance_City_Hall + Age + Wealthy)
summary(GLMred_Copenhagen_Full)
plot(GLMred_Copenhagen_Full)





# The R-squared values and the F-statistic are great. The t-test gives the 
# following insignificance parameters:
# Ground_Area, Distance_School, and Age.

# We remove the variables mentioned in the above t-test.
GLMred_Copenhagen_Reduced <- lm(data = dfred_copenhagen,
                                Price ~ Rooms + Ground_Area + Home_Area + 
                                 + Distance_City_Hall + Age + Wealthy)
summary(GLMred_Copenhagen_Reduced)
anova(GLMred_Copenhagen_Full,GLMred_Copenhagen_Reduced)



# The R-squared values and the F-statistic are great. The t-test gives the 
# following insignificance parameters:
# Ground_Area, Distance_School, and Age.

# We remove the variables mentioned in the above t-test.
GLMred_Copenhagen_Reduced1 <- lm(data = dfred_copenhagen,
                                Price ~ Rooms + Ground_Area + Home_Area + 
                                  + Age + Wealthy)
summary(GLMred_Copenhagen_Reduced1)
anova(GLMred_Copenhagen_Full,GLMred_Copenhagen_Reduced1)


# The R-squared values and the F-statistic are great. The t-test gives the 
# following insignificance parameters:
# Ground_Area, Distance_School, and Age.

# We remove the variables mentioned in the above t-test.
GLMred_Copenhagen_Reduced2 <- lm(data = dfred_copenhagen,
                                 Price ~ Rooms + Home_Area + 
                                   + Age + Wealthy)
summary(GLMred_Copenhagen_Reduced2)
anova(GLMred_Copenhagen_Full,GLMred_Copenhagen_Reduced2)


# The R-squared values and the F-statistic are great. The t-test gives the 
# following insignificance parameters:
# Ground_Area, Distance_School, and Age.

# We remove the variables mentioned in the above t-test.
GLMred_Copenhagen_Reduced3 <- lm(data = dfred_copenhagen,
                                 Price ~ Rooms + Home_Area 
                                   + Age)
summary(GLMred_Copenhagen_Reduced3)
anova(GLMred_Copenhagen_Full,GLMred_Copenhagen_Reduced3)
plot(GLMred_Copenhagen_Reduced3)

res_Copenhagen <- resid(GLMred_Copenhagen_Reduced3)
plot(fitted(GLMred_Copenhagen_Reduced3), res_Copenhagen,
     xlab = "Fitted Values", ylab = "Residuals")
abline(0,0)

# Smooth density plot:
plot(density(res_Copenhagen))