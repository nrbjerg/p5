### Full Model with Municipality and Wealthy ###

# The full model with all variables included except Trend, Year, Voting_Area, 
# Parish.
GLM_Full_1 <- lm(data = df, Price ~ Fin_Trend + Rooms + Month +
              Ground_Area + Home_Area + Big_Ground + Distance_School +
              Distance_City_Hall + Age + Wealthy + Municipality)
summary(GLM_Full_1)

# The R-squared values and F-test gives reasonable values, which is a good sign:
# Multiple R-squared:  0.7272,	Adjusted R-squared:  0.7194 
# F-statistic:  93.3 on 23 and 805 DF,  p-value: < 2.2e-16

# The following variables gives a high p-value from the t-test, hence we may 
# omit them:
# Month (MonthAugust: 0.4676, MonthDecember: 0.4422)
# Big_Ground:           0.3644    
# Distance_School:      0.6698
# Age                   0.9876


### Reduced Model with Municipality and Wealthy ###

# Reduce the full model based on t-values in a conservative way, that is the 
# above mentioned four variables.
GLM_Reduced_1 <- lm(data = df, Price ~ Fin_Trend + Rooms + Ground_Area + 
                 Home_Area + Distance_City_Hall + Wealthy + Municipality)
summary(GLM_Reduced_1)

# The R-squared values and F-test gives approximatively the same values, which 
# suggest that no accuracy is lost:
# Multiple R-squared:  0.7213,	Adjusted R-squared:  0.7182  
# F-statistic: 235.5 on 9 and 819 DF,  p-value: < 2.2e-16

# F-testing the above null hypothesis. The F-test is confirms our reduction.
anova(GLM_Full_1, GLM_Reduced_1)

# All p-values of the t-test are more than reasonable, thus we conclude that no 
# further reduction is in our interest. We still try and reduce best on testing 
# the limits. The two highest p-values are:
# Ground_Area:          0.00882
# Distance_City_Hall:   0.00819


### Further Reduction with Municipality and Wealthy ###

# A further reduction from the reduced model removing Ground_Area.
GLM_Reduced_11 <- lm(data = df, Price ~ Fin_Trend + Rooms + Home_Area + 
                     Distance_City_Hall + Wealthy + Municipality)
summary(GLM_Reduced_11)

# Again we get good R-squared values, F-test, and t-test.

# F-testing the further reduction 11.
anova(GLM_Full_1, GLM_Reduced_11)
anova(GLM_Reduced_1, GLM_Reduced_11)

# The first test has p-value 0.06219. The second test has p-value 0.008816.
# Thus we may accept the reduction 11 from the full model, but not from the 
# reduced model.

# Another further reduction from the reduced model removing Distance_City_Hall.
GLM_Reduced_12 <- lm(data = df, Price ~ Fin_Trend + Rooms + Home_Area + 
                     Ground_Area + Wealthy + Municipality)
summary(GLM_Reduced_12)

# Again we get good R-squared values and F-test, but the t-test for (Intercept) 
# is bad:
# (Intercept):        0.1795
# The (Intercept) parameter also includes MunicipalityCopenhagen, so the removal 
# of Distance_City_Hall may make the response variable Municipality worse.

# F-testing the further reduction 12.
anova(GLM_Full_1, GLM_Reduced_12)
anova(GLM_Reduced_1, GLM_Reduced_12)

# The first test has p-value 0.06008. The second test has p-value 0.008187.
# The same conclusion as the reduction 11 is made.





### Full Model with Parish ###

# The full model with all variables included except Trend, Year, Voting_Area, 
# Municipality, Wealthy.
GLM_Full_2 <- lm(data = df, Price ~ Fin_Trend + Rooms + Month +
                   Ground_Area + Home_Area + Big_Ground + Distance_School +
                   Distance_City_Hall + Age + Parish)
summary(GLM_Full_2)

# The R-squared values and F-test gives reasonable values, which is a good sign:
# Multiple R-squared:  0.7712,	Adjusted R-squared:  0.7638
# F-statistic:   104 on 26 and 802 DF,  p-value: < 2.2e-16

# Looking at the t-test, parameters associated with Months and Parish both give
# problems, thus we try two different reduction removing one at a time.
# Month (MonthAugust: 0.31692, MonthMay: 0.43521)
# Parish (ParishHasseris: 0.67665, ParishThomas Kingos: 0.85007)


### Reduced Model of Month with Parish ###

# Reduce the full model based by removing the explanatory variable Month.
GLM_Reduced_Month <- lm(data = df, Price ~ Fin_Trend + Rooms + Ground_Area + 
                        Home_Area + Big_Ground + Distance_School +
                        Distance_City_Hall + Age + Parish)
summary(GLM_Reduced_Month)

# The R-squared values and F-test gives approximatively the same values, which 
# suggest that no accuracy is lost.

# F-testing the above null hypothesis. The F-test confirms our reduction with 
# p-value 0.1369.
anova(GLM_Full_2, GLM_Reduced_Month)

# Looking at the t-test, we still have a problem with the variable Parish, 
# where ParishHasseris and ParishThomas Kingos still score high p-values. More
# concerning is the change in Big_Ground, which now scores a value of 0.81722.

# We make a further reduction on Big_Ground.
GLM_Reduced_Month_1 <- lm(data = df, Price ~ Fin_Trend + Rooms + Ground_Area + 
                          Home_Area + Distance_School + Distance_City_Hall + 
                          Age + Parish)
summary(GLM_Reduced_Month_1)

# R-squared values and the F-test has not changed much in values. Making a
# F-test on the reduction gives p-value of 0.8172. Again Parish gives a problem
# in t-testing.
anova(GLM_Reduced_Month, GLM_Reduced_Month_1)


### Reduced Model of Parish with Parish ###

# Reduce the full model based by removing the explanatory variable Parish.
GLM_Reduced_Parish <- lm(data = df, Price ~ Fin_Trend + Rooms + Month + 
                         Ground_Area + Home_Area + Big_Ground + 
                         Distance_School + Distance_City_Hall + Age)
summary(GLM_Reduced_Parish)

# The F-test stays the same, but we have a significant drop in R-squared values:
# Multiple R-squared:  0.5297,	Adjusted R-squared:  0.5187 

# F-testing the above null hypothesis. The F-test rejects our reduction 
# with p-value 2.2e-16.
anova(GLM_Full_2, GLM_Reduced_Parish)

# Thus we move no further.


### Reduced Model of Month and Parish with Parish ###

# Based on the reduced model of Month, which was positive, we try removing both
# problematic variables at the same time.
GLM_Reduced_2 <- lm(data = df, Price ~ Fin_Trend + Rooms + Ground_Area + 
                           Home_Area + Big_Ground + Distance_School +
                           Distance_City_Hall + Age)
summary(GLM_Reduced_2)

# Again we get worse R-squared values, but the same good result of the F-test:
# Multiple R-squared:  0.5225,	Adjusted R-squared:  0.5178

# F-testing the above null hypothesis. The F-test rejects our reduction 
# with p-value 2.2e-16.
anova(GLM_Full_2, GLM_Reduced_2)

# Thus we move no further with reducing the full model number two.