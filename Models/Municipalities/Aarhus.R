library(dplyr)
par(mfrow = c(1, 2))

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


dfrem_Aarhus <- remove_points_with_too_high_leverage(GLM_Aarhus_Full,df_Aarhus)
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
anova(GLMrem_Aarhus_Full, GLMrem_Aarhus_red1)

GLMrem_Aarhus_red2 <- lm(data = dfrem_Aarhus,
                            ln_Price ~ Rooms + Home_Area +
                              Distance_School + Distance_City_Hall + Wealthy)
summary(GLMrem_Aarhus_red2)
anova(GLMrem_Aarhus_Full, GLMrem_Aarhus_red2)

GLMrem_Aarhus_red3 <- lm(data = dfrem_Aarhus,
                            ln_Price ~ Rooms + Home_Area +
                              Distance_City_Hall + Wealthy)
summary(GLMrem_Aarhus_red3)
anova(GLMrem_Aarhus_Full, GLMrem_Aarhus_red3)

GLMrem_Aarhus_red4 <- lm(data = dfrem_Aarhus,
                            ln_Price ~ Rooms + Home_Area +
                            Wealthy)
summary(GLMrem_Aarhus_red4)
anova(GLMrem_Aarhus_Full, GLMrem_Aarhus_red4)

GLMrem_Aarhus_red5 <- lm(data = dfrem_Aarhus,
                         ln_Price ~ Home_Area +
                           Wealthy)
summary(GLMrem_Aarhus_red5)
anova(GLMrem_Aarhus_Full, GLMrem_Aarhus_red5)

# Plot of Residuals:
res_Aarhus <- resid(GLMrem_Aarhus_red5)
plot(fitted(GLMrem_Aarhus_red5), res_Aarhus,
     xlab = "Fitted Values", ylab = "Residuals")
abline(0,0)

# Plot of Normal QQ-plot:
plot(GLMrem_Aarhus_red5, 2, caption = '')

# Smooth density plot:
plot(density(dfrem_Aarhus$ln_Price), main = '', xlab = 'ln_Price')
