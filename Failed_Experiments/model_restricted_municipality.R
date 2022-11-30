### Dividing the Dataset in Municipalities ###

df_copenhagen <- subset(df, Municipality == 'Copenhagen')
df_aarhus <- subset(df, Municipality == 'Aarhus')
df_odense <- subset(df, Municipality == 'Odense')
df_aalborg <- subset(df, Municipality == 'Aalborg')





### Full Model for Copenhagen ###

# Full model restricted to Copenhagen. Omit variables Trend, Year, Voting_Area, 
# Parish, Municipality.
GLM_Copenhagen_Full <- lm(data = df_copenhagen,
                       Price ~ Fin_Trend + Rooms + Month + Ground_Area + 
                       Home_Area + Big_Ground + Distance_School +
                       Distance_City_Hall + Age + Wealthy)
summary(GLM_Copenhagen_Full)

# The R-squared values and the F-statistic are great:
# Multiple R-squared:  0.8581,	Adjusted R-squared:  0.8282 
# F-statistic: 28.72 on 20 and 95 DF,  p-value: < 2.2e-16

# The t-test gives the following insignificance parameters (based on a 
# significance level of 10%):
# Month (MonthAugust: 0.404547, MonthNovember: 0.243528)
# Ground_Area:          0.603209
# Big_Ground:           0.139994
# Distance_School:      0.149831
# Age:                  0.692154


### Reduced Model for Copenhagen ###

# We remove the variables mentioned in the above t-test.
GLM_Copenhagen_Reduced <- lm(data = df_copenhagen,
                             Price ~ Fin_Trend + Rooms + Home_Area +
                             Distance_City_Hall + Wealthy)
summary(GLM_Copenhagen_Reduced)

# The reduced model has almost the same R-squared values and F-statistic, which
# is a great sign.

# We use a F-test to check the hypothesis of reducing the model. This gives
# p-value 0.4549 and hence confirms our reduction.
anova(GLM_Copenhagen_Full,GLM_Copenhagen_Reduced)

# Using the t-test on individual variables gives good results, that is no
# trivial parameter is included. Thus we stop and accept the reduction.





### Full Model for Aarhus ###

# Full model restricted to Aarhus. Omit variables Trend, Year, Voting_Area, 
# Parish, Municipality.
GLM_Aarhus_Full <- lm(data = df_aarhus,
                      Price ~ Fin_Trend + Rooms + Month + Ground_Area + 
                      Home_Area + Big_Ground + Distance_School +
                      Distance_City_Hall + Age + Wealthy)
summary(GLM_Aarhus_Full)

# The R-squared values are not that great, but the F-test is still good:
# Multiple R-squared:  0.6226,	Adjusted R-squared:  0.5712  
# F-statistic: 12.13 on 20 and 147 DF,  p-value: < 2.2e-16

# The t-test gives the following insignificance parameters (based on a 
# significance level of 10%):
# Month (MonthAugust: 0.7376, MonthDecember: 0.9711)
# Rooms:                0.1973
# Ground_Area:          0.1483
# Big_Ground:           0.9604
# Distance_City_Hall:   0.2483
# Age:                  0.2054


### Reduced Model for Aarhus ###

# We remove the variables mentioned in the above t-test.
GLM_Aarhus_Reduced <- lm(data = df_aarhus,
                         Price ~ Fin_Trend + Home_Area + Distance_School + 
                         Wealthy)
summary(GLM_Aarhus_Reduced)

# The reduced model has almost the same adjusted R-squared value and 
# F-statistic, which is a great sign. The R-squared value is lower by 
# approximately 0.04:
# Multiple R-squared:  0.5876,	Adjusted R-squared:  0.5774

# We use a F-test to check the hypothesis of reducing the model. This gives
# p-value 0.6243 and hence confirms our reduction.
anova(GLM_Aarhus_Full,GLM_Aarhus_Reduced)

# Using the t-test on individual variables gives good results, that is no
# trivial parameter is included. Thus we stop and accept the reduction.





### Full Model for Odense ###

# Full model restricted to Odense. Omit variables Trend, Year, Voting_Area, 
# Parish, Municipality.
GLM_Odense_Full <- lm(data = df_odense,
                      Price ~ Fin_Trend + Rooms + Month + Ground_Area + 
                      Home_Area + Big_Ground + Distance_School +
                      Distance_City_Hall + Age + Wealthy)
summary(GLM_Odense_Full)

# The R-squared values and the F-statistic are great:
# Multiple R-squared:  0.7328,	Adjusted R-squared:  0.7101 
# F-statistic: 32.23 on 20 and 235 DF,  p-value: < 2.2e-16

# The t-test gives the following insignificance parameters (based on a 
# significance level of 10%):
# Month (MonthAugust: 0.46931, MonthFebruary: 0.82601)
# Rooms:                0.83244
# Distance_School:      0.47493
# Distance_City_Hall:   0.80883


### Reduced Model for Odense ###

# We remove the variables mentioned in the above t-test.
GLM_Odense_Reduced <- lm(data = df_odense,
                         Price ~ Fin_Trend + Ground_Area + Home_Area + 
                         Big_Ground + Age + Wealthy)
summary(GLM_Odense_Reduced)

# The reduced model has almost the same R-squared values and F-statistic, which
# is a great sign. Actually we see a small increase in the adjusted R-squared.

# We use a F-test to check the hypothesis of reducing the model. This gives
# p-value 0.8589 and hence confirms our reduction.
anova(GLM_Odense_Full,GLM_Odense_Reduced)

# Using the t-test on individual variables gives good results, that is no
# trivial parameter is included. Thus we stop and accept the reduction.





### Full Model for Aalborg ###

# Full model restricted to Aalborg. Omit variables Trend, Year, Voting_Area, 
# Parish, Municipality.
GLM_Aalborg_Full <- lm(data = df_aalborg,
                       Price ~ Fin_Trend + Rooms + Month + Ground_Area + 
                       Home_Area + Big_Ground + Distance_School +
                       Distance_City_Hall + Age + Wealthy)
summary(GLM_Aalborg_Full)

# The R-squared values are not that great, but not that bad to. The F-test is 
# still good:
# Multiple R-squared:  0.6857,	Adjusted R-squared:  0.6622  
# F-statistic: 29.23 on 20 and 268 DF,  p-value: < 2.2e-16

# The t-test gives the following insignificance parameters (based on a 
# significance level of 10%):
# Month (MonthDecember: 0.60037, MonthFebruary: 0.9711)
# Rooms:                0.6848
# Ground_Area:          0.23466
# Distance_School:      0.38229


### Reduced Model for Aalborg ###

# We remove the variables mentioned in the above t-test.
GLM_Aalborg_Reduced <- lm(data = df_aalborg,
                          Price ~ Fin_Trend + Home_Area + Big_Ground +
                          Distance_City_Hall + Age + Wealthy)
summary(GLM_Aalborg_Reduced)

# The reduced model has almost the same R-squared values and F-statistic, which 
# is a great sign.

# We use a F-test to check the hypothesis of reducing the model. This gives
# p-value 0.4583 and hence confirms our reduction.
anova(GLM_Aalborg_Full,GLM_Aalborg_Reduced)

# Using the t-test on individual variables gives good results, that is no
# trivial parameter is included. Thus we stop and accept the reduction.
