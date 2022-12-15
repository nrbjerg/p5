df <- subset(training_df, Municipality == "Odense")

mod <- lm(data = df, ln_Price ~
  Rooms + Ground_Area + Home_Area + Distance_School + 
    Distance_City_Hall + Age + Wealthy)
plot(mod)

df <- remove_points_with_too_high_leverage(mod, df)

par(mfrow = c(1, 2))
plot(density(subset(df, Wealthy==1)$ln_Price), main="", xlab="ln_Price")
plot(density(subset(df, Wealthy==0)$ln_Price), main="", xlab="ln_Price")
par(mfrow = c(1, 1))
plot(density(df$ln_Price), main="", xlab="ln_Price")

mod <- lm(data = df, ln_Price ~ Rooms + Ground_Area + Home_Area + Distance_School + Distance_City_Hall + Age + Wealthy)
summary(mod)
plot(mod) # Looks good.

reduced <- lm(data = df, ln_Price ~ Ground_Area + Home_Area + Age + Wealthy)
anova(reduced, mod) # p = 0.2706.
summary(reduced) # No obvious further reductions.

par(mfrow = c(1, 2))
plot(fitted(reduced), residuals(reduced), xlab = "Fitted Values", ylab = "Residuals")
abline(0, 0)

plot(reduced, 2, caption="", main="")

par(mfrow = c(1, 1))
plot(fitted(reduced), residuals(reduced), xlab = "Fitted Values", ylab = "Residuals", pch = 2 - df$Wealthy)
abline(0, 0)