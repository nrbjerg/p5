library(dplyr)
par(mfrow = c(1, 2))

### Dividing the Dataset in Municipalities ###
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

dfrem_Copenhagen <- remove_points_with_too_high_leverage(GLM_Copenhagen_Full, df_Copenhagen)
GLMrem_Copenhagen_Full <- lm(data = dfrem_Copenhagen,
                         ln_Price ~ Rooms + Ground_Area + Home_Area +
                           Distance_School + Distance_City_Hall + Age + Wealthy)
summary(GLMrem_Copenhagen_Full)
plot(GLMrem_Copenhagen_Full)

# Reduce the model due to F-test and Pr(>t).
GLMrem_Copenhagen_red1 <- lm(data = dfrem_Copenhagen,
                         ln_Price ~ Rooms + Ground_Area + Home_Area +
                           Distance_School + Age + Wealthy)
summary(GLMrem_Copenhagen_red1)
anova(GLMrem_Copenhagen_Full, GLMrem_Copenhagen_red1)

GLMrem_Copenhagen_red2 <- lm(data = dfrem_Copenhagen,
                           ln_Price ~ Rooms + Home_Area +
                               Distance_School + Age + Wealthy)
summary(GLMrem_Copenhagen_red2)
anova(GLMrem_Copenhagen_Full, GLMrem_Copenhagen_red2)

GLMrem_Copenhagen_red3 <- lm(data = dfrem_Copenhagen,
                             ln_Price ~ Home_Area +
                               Distance_School + Age + Wealthy)
summary(GLMrem_Copenhagen_red3)
anova(GLMrem_Copenhagen_Full, GLMrem_Copenhagen_red3)

GLMrem_Copenhagen_red4 <- lm(data = dfrem_Copenhagen,
                             ln_Price ~ Home_Area + Age + Wealthy)
summary(GLMrem_Copenhagen_red4)
anova(GLMrem_Copenhagen_Full, GLMrem_Copenhagen_red4)

# Plot of Residuals
res_Copenhagen <- resid(GLMrem_Copenhagen_red4)
plot(fitted(GLMrem_Copenhagen_red4), res_Copenhagen,
     xlab = "Fitted Values", ylab = "Residuals")
abline(0,0)

#Plot of normal Q-Q plot
plot(GLMrem_Copenhagen_red4, 2, caption = '')

# Smooth density plot:
plot(density(dfrem_Copenhagen$ln_Price), main = '', xlab = 'ln_Price')
