### Dividing the Data Set in Municipalities ###

df_wealthy_1 <- subset(df, Wealthy == 1)
df_wealthy_0 <- subset(df, Wealthy == 0)





### Full Model for Wealthy Areas ###

# We make a full model for wealthy areas in Denmark. We omit variables Trend, 
# Year, Voting_Area, Parish, Wealthy.
GLM_Full_Wealthy_1 <- lm(data = df_wealthy_1, 
                         Price ~ Fin_Trend + Rooms + Month + Ground_Area + 
                         Home_Area + Big_Ground + Distance_School +
                         Distance_City_Hall + Age + Municipality)
summary(GLM_Full_Wealthy_1)

# We have quite acceptable R-squared values and F-test:
# Multiple R-squared:  0.7513,	Adjusted R-squared:  0.7412
# F-statistic: 74.43 on 22 and 542 DF,  p-value: < 2.2e-16

# The t-test suggest removing the following explanatory variables:
# Month (MonthOctober: 0.235496, MonthSeptember: 0.899657)
# Big_Ground:           0.621462

GLM_Reduced_Wealthy_1 <- lm(data = df_wealthy_1, 
                            Price ~ Fin_Trend + Rooms + Ground_Area + 
                            Home_Area + Distance_School +
                            Distance_City_Hall + Age + Municipality)
summary(GLM_Reduced_Wealthy_1)

# We have almost the same R-squared values and F-statistic as the full model.

# The test the reduction by a F-test, which gives p-value 0.2535, so we accept 
# the reduction.
anova(GLM_Full_Wealthy_1, GLM_Reduced_Wealthy_1)

# No further reduction is implied by the t-test or F-test, so we stop.





### Full Model for Non-Wealthy Areas ###

# We make a full model for non-wealthy areas in Denmark. We omit variables 
# Trend,Year, Voting_Area, Parish, Wealthy.
GLM_Full_Wealthy_0 <- lm(data = df_wealthy_0, 
                         Price ~ Fin_Trend + Rooms + Month + Ground_Area + 
                         Home_Area + Big_Ground + Distance_School +
                         Distance_City_Hall + Age + Municipality)
summary(GLM_Full_Wealthy_0)

# We have quite acceptable R-squared values and F-test:
# Multiple R-squared:  0.7394,	Adjusted R-squared:  0.7156 
# F-statistic: 31.08 on 22 and 241 DF,  p-value: < 2.2e-16

# The t-test suggest removing the following explanatory variables:
# Month (MonthOctober: 0.89769, MonthSeptember: 0.96280)
# Rooms:              0.89496
# Big_Ground:         0.48722
# Distance_City_Hall: 0.49743

# We reduced the above full model by the indications from the above t-test.
GLM_Reduced_Wealthy_0 <- lm(data = df_wealthy_0, 
                            Price ~ Fin_Trend + Ground_Area + Home_Area + 
                            Distance_School + Age + Municipality)
summary(GLM_Reduced_Wealthy_0)

# We have almost the same R-squared values and F-statistic as the full model.

# The test the reduction by a F-test, which gives p-value 0.8569, so we accept 
# the reduction.
anova(GLM_Full_Wealthy_0, GLM_Reduced_Wealthy_0)

# No further reduction is implied by the t-test or F-test, so we stop.

