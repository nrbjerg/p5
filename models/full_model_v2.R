### Full Model ###

# The full model with all variables included except Trend, Year, Voting_Area, 
# Parish, Month, Big_Ground.
GLM_Full <- lm(data = training_df, Price ~ Rooms + Ground_Area + Home_Area + 
               Distance_School + Distance_City_Hall + Age + Wealthy + 
               Municipality)
summary(GLM_Full)
# plot(GLM_Full)

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

# The R-squared values are an improvement partly due to the smaller values, and
# the F-test still shows that the model i significant. Also the t-test is good.

GLM_Transform <- lm(data = training_df, ln_Price ~ Rooms + Ground_Area + 
                    Home_Area + Wealthy + Municipality)
summary(GLM_Transform)
plot(GLM_Transform)
