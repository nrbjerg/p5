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

# The R-squared values and the F-statistic are great. The t-test gives the 
# following insignificance parameters:
# Ground_Area, Distance_School, and Age.

# We remove the variables mentioned in the above t-test.
GLM_Copenhagen_Reduced <- lm(data = df_copenhagen,
                             Price ~ Rooms + Home_Area + Distance_City_Hall + 
                             Wealthy)
summary(GLM_Copenhagen_Reduced)
# plot(GLM_Copenhagen_Reduced)

# The reduced model has almost the same R-squared values and F-statistic, which
# is a great sign.

# We use a F-test to check the hypothesis of reducing the model. This confirms
# our reduction.
anova(GLM_Copenhagen_Full, GLM_Copenhagen_Reduced)

# Using the t-test on individual variables gives good results, that is no
# trivial parameter is included. Thus we stop and accept the reduction.

GLM_Copenhagen_Transformed <- lm(data = df_copenhagen,
                                 ln_Price ~ Rooms + ln_Home_Area + 
                                 Distance_City_Hall + 
                                 Wealthy)
summary(GLM_Copenhagen_Transformed)
# plot(GLM_Copenhagen_Transformed)





### Model for Aarhus ###

# Full model restricted to Aarhus. Omit variables Trend, Year, Voting_Area, 
# Parish, Municipality.
GLM_Aarhus_Full <- lm(data = df_aarhus,
                      Price ~ Rooms + Ground_Area + Home_Area + 
                      Distance_School + Distance_City_Hall + Age + Wealthy)
summary(GLM_Aarhus_Full)

# The R-squared values are not great, but okay. The F-test is good, so our
# model should be non-trivial. The t-test gives the following insignificance 
# parameters:
# Rooms, Ground_Area, Distance_School, Distance_City_Hall, Age

# We remove the variables mentioned in the above t-test.
GLM_Aarhus_Reduced <- lm(data = df_aarhus,
                         Price ~ Home_Area + Wealthy)
summary(GLM_Aarhus_Reduced)
# plot(GLM_Aarhus_Reduced)

# The reduced model has almost the same R-squared values and F-statistic, which
# is a great sign.

# We use a F-test to check the hypothesis of reducing the model. This confirms
# our reduction barely.
anova(GLM_Aarhus_Full, GLM_Aarhus_Reduced)

# Using the t-test on individual variables gives good results, that is no
# trivial parameter is included. Thus we stop and accept the reduction.

GLM_Aarhus_Transformed <- lm(data = df_aarhus,
                             ln_Price ~ ln_Home_Area + Wealthy)
summary(GLM_Aarhus_Transformed)
# plot(GLM_Aarhus_Transformed)





### Model for Odense ###

# Full model restricted to Odense. Omit variables Trend, Year, Voting_Area, 
# Parish, Municipality.
GLM_Odense_Full <- lm(data = df_odense,
                      Price ~ Rooms + Ground_Area + Home_Area + 
                      Distance_School + Distance_City_Hall + Age + Wealthy)
summary(GLM_Odense_Full)

# The R-squared values are okay and the F-statistic great. The t-test gives the 
# following insignificance parameters:
# Rooms, Distance_School, and Distance_City_Hall.

# We remove the variables mentioned in the above t-test.
GLM_Odense_Reduced <- lm(data = df_odense,
                         Price ~ Ground_Area + Home_Area + Age + Wealthy)
summary(GLM_Odense_Reduced)
# plot(GLM_Odense_Reduced)

# The reduced model has almost the same R-squared values and F-statistic, which
# is a great sign.

# We use a F-test to check the hypothesis of reducing the model. This confirms
# our reduction.
anova(GLM_Odense_Full, GLM_Odense_Reduced)

# Using the t-test on individual variables gives good results, that is no
# trivial parameter is included. Thus we stop and accept the reduction.

GLM_Odense_Transformed <- lm(data = df_odense,
                             ln_Price ~ ln_Ground_Area + ln_Home_Area + Age + 
                             Wealthy)
summary(GLM_Odense_Transformed)
# plot(GLM_Odense_Transformed)





### Model for Aalborg ###

# Full model restricted to Aalborg. Omit variables Trend, Year, Voting_Area, 
# Parish, Municipality.
GLM_Aalborg_Full <- lm(data = df_aalborg,
                       Price ~ Rooms + Ground_Area + Home_Area + 
                       Distance_School + Distance_City_Hall + Age + Wealthy)
summary(GLM_Aalborg_Full)

# The R-squared values are okay and the F-statistic is great. The t-test gives 
# the following insignificance parameters:
# Rooms, Ground_Area, Distance_School, and Age.

# We remove the variables mentioned in the above t-test.
GLM_Aalborg_Reduced <- lm(data = df_aalborg,
                          Price ~ Home_Area + Distance_City_Hall + 
                          Wealthy)
summary(GLM_Aalborg_Reduced)
# plot(GLM_Aalborg_Reduced)

# The reduced model has almost the same R-squared values and F-statistic, which
# is a great sign.

# We use a F-test to check the hypothesis of reducing the model. This confirms
# our reduction.
anova(GLM_Aalborg_Full, GLM_Aalborg_Reduced)

# Using the t-test on individual variables gives good results, that is no
# trivial parameter is included. Thus we stop and accept the reduction.

GLM_Aalborg_Transformed <- lm(data = df_aalborg,
                              ln_Price ~ ln_Home_Area + Distance_City_Hall + 
                              Wealthy)
summary(GLM_Aalborg_Transformed)
# plot(GLM_Aalborg_Transformed)
