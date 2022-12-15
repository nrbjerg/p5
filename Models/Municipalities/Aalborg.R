library(dplyr)
par(mfrow = c(1, 2))

### Dividing the Data Set in Municipalities ###
training_df_no_outliers = training_df[-c(2, 4, 8, 17, 19, 29, 77, 116, 255, 471,
                                         486, 545),]
df_Aalborg <- subset(training_df_no_outliers, Municipality == 'Aalborg')

### Model for Aalborg ###

# Full model restricted to Aalborg Omit variables Trend, Year, Voting_Area,
# Parish, Municipality.
GLM_Aalborg_Full <- lm(data = df_Aalborg,
                      ln_Price ~ Rooms + Ground_Area + Home_Area +
                        Distance_School + Distance_City_Hall + Age + Wealthy)
summary(GLM_Aalborg_Full)
plot(GLM_Aalborg_Full)

dfrem_Aalborg <- remove_points_with_too_high_leverage(GLM_Aalborg_Full, df_Aalborg)
GLMrem_Aalborg_Full <- lm(data = dfrem_Aalborg,
                         ln_Price ~ Rooms + Ground_Area + Home_Area +
                           Distance_School + Distance_City_Hall + Age + Wealthy)
summary(GLMrem_Aalborg_Full)
plot(GLMrem_Aalborg_Full)

# Reduce the model due to F-test and Pr(>t).
GLMrem_Aalborg_red1 <- lm(data = dfrem_Aalborg,
                         ln_Price ~ Ground_Area + Home_Area +
                           Distance_School + Distance_City_Hall + Age + Wealthy)
summary(GLMrem_Aalborg_red1)
anova(GLMrem_Aalborg_Full, GLMrem_Aalborg_red1)

GLMrem_Aalborg_red2 <- lm(data = dfrem_Aalborg,
                          ln_Price ~ Home_Area +
                            Distance_School + Distance_City_Hall + Age + Wealthy)
summary(GLMrem_Aalborg_red2)
anova(GLMrem_Aalborg_Full, GLMrem_Aalborg_red2)

GLMrem_Aalborg_red3 <- lm(data = dfrem_Aalborg,
                          ln_Price ~ Home_Area +
                            Distance_City_Hall + Age + Wealthy)
summary(GLMrem_Aalborg_red3)
plot(GLMrem_Aalborg_red3)
anova(GLMrem_Aalborg_Full, GLMrem_Aalborg_red3)

# Plot of Residuals:
res_Aalborg <- resid(GLMrem_Aalborg_red3)
plot(fitted(GLMrem_Aalborg_red3), res_Aalborg,
     xlab = "Fitted Values", ylab = "Residuals")
abline(0,0)

# Plot of Normal QQ-plot:
plot(GLMrem_Aalborg_red3,2,caption='')

# Smooth density plot:
plot(density(dfrem_Aalborg$ln_Price),main='',xlab = 'ln_Price')
