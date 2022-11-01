### Full Model ###

# The full model with all variables included except some geographic parameters like Sogn and Afstemningsomraade.
L_full <- lm(data = cityhomes, Pris_Salg ~ Areal_Bolig+Areal_Grund+StorGrund+Ejd_AntalRum+Salgsmaaned+Trend+Alder+Dist_skole+Dist_raadhus+KommuneNavn+Velhavende)
summary(L_full)

# The R-squared values are 0.7243 and adjusted 0.7168.
# The F-test gives p-value 2.2e-16, thus the model is non-trivial.

# The following variables gives a high p-value from the t-test, hence we may omit them:
# StorGrund with p-value 0.628635.
# Salgsmaaned, where the indicators have p-values (note that April is the reference month): 
# (0.167148, 0.556460, 0.138642, NA, 0.928731, 0.518359, 0.057502, 0.418802, 0.850670, 0.165848, 0.401303)
# Alder with p-value 0.980269.
# Dist_skole with p-value 0.892358.



### Reduced Model ###

# Reduce the full model based on t-values in a conservative way, that is the above mentioned variables.
L_reduced <- lm(data = cityhomes, Pris_Salg ~ Areal_Bolig+Areal_Grund+Ejd_AntalRum+Trend+Dist_raadhus+KommuneNavn+Velhavende)
summary(L_reduced)

# The R-squared values are 0.7199 and adjusted 0.7169.
# The reduced model does not include insignificant variables with a significance level of 0.05.
# The closest are:
# (Intercept) with p-value 0.0346, which is also reference variable for KommuneNavnKøbenhavn.
# Dist_raadhus with p-value 0.0206.
# Most parameter stay about the same.

# F-testing the above null hypothesis.
anova(L_full,L_reduced)

# The F-test give p-value 0.489, hence it confirms the null hypothesis.



### Further Reduction ###

# A further reduction from the reduced model removing Dist_raadhus.
L_reduced2 <- lm(data = cityhomes, Pris_Salg ~ Areal_Bolig+Areal_Grund+Ejd_AntalRum+Trend+KommuneNavn+Velhavende)
summary(L_reduced2)

# The R-squared values are 0.7181 and adjusted 0.7155.
# No insignificance parameter based on p-values except (Intercept).

# F-testing the further reduced model.
anova(L_reduced,L_reduced2)
anova(L_full,L_reduced2)

# The first test has p-value 0.02064.
# The second test has p-value 0.2228.

# Another further reduction from the reduced model removing geographic indicators.
L_reduced3 <- lm(data = cityhomes, Pris_Salg ~ Areal_Bolig+Areal_Grund+Ejd_AntalRum+Trend+Velhavende)
summary(L_reduced3)

# The R-squared values are 0.5289 and adjusted 0.5261.
# Variable Areal_Grund has p-value 0.805, while the rest stay significant.

anova(L_reduced,L_reduced3)
anova(L_full,L_reduced3)

# The first test has p-value 2.2e-16.
# The second test has p-value 2.2e-16.



### Substituting Areal_Grund with StorGrund ###

# Including StorGrund instead of Areal_Grund to test the merit of the two variables.
L_StorGrund <- lm(data = cityhomes, Pris_Salg ~ Areal_Bolig+StorGrund+Ejd_AntalRum+Trend+Dist_raadhus+KommuneNavn+Velhavende)
summary(L_StorGrund)

# The R-squared values are 0.7154 and adjusted 0.7124.
# StorGrund has p-value 0.1128 and (Intercept) also gets worse with p-value 0.0924.

# Testing this reduction with F-test:
anova(L_full,L_StorGrund)

# We get p-value 0.02057.
