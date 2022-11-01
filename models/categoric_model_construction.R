### Dividing the Data Set ###

cityhomes_koebenhavn <- subset(cityhomes, KommuneNavn == 'København')
cityhomes_aarhus <- subset(cityhomes, KommuneNavn == 'Aarhus')
cityhomes_odense <- subset(cityhomes, KommuneNavn == 'Odense')
cityhomes_aalborg <- subset(cityhomes, KommuneNavn == 'Aalborg')

# cityhomes_aalborg_velhavende <- subset(subset(cityhomes, KommuneNavn == 'Aalborg'), Velhavende == 1)



### Full Model for Aalborg ###

# Full model restricted to Aalborg.
L_aalborg_full <- lm(data = cityhomes_aalborg, Pris_Salg ~ Areal_Bolig+Areal_Grund+StorGrund+Ejd_AntalRum+Salgsmaaned+Trend+Alder+Dist_skole+Dist_raadhus+Velhavende)
summary(L_aalborg_full)

# The reduced model of the full model for Aalborg based on the t-test.
L_aalborg_reduced <- lm(data = cityhomes_aalborg, Pris_Salg ~ Areal_Bolig+Areal_Grund+Trend+Alder+Dist_raadhus+Velhavende)
summary(L_aalborg_reduced)

# The following F-test shows that this is an acceptable reduction.
anova(L_aalborg_full, L_aalborg_reduced)


cityhomes_aalborg[2]


pred_int <- predict(L_aalborg_reduced, interval = "prediction")

pred_int
cityhomes_aalborg <- cbind(cars, pred_int)
# 2. Regression line + confidence intervals
library("ggplot2")
p <- ggplot(cityhomes_aalborg, aes(trend, Pris_Salg)) +
  geom_point() +
  stat_smooth(method = lm)
# 3. Add prediction intervals
p + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed")
