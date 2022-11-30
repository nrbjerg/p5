# Create data frames.
df_w <- subset(df, Wealthy==1)
df_n <- subset(df, Wealthy==0)
rm(df)

# Create models (NOTE THAT WE DO NOT HAVE WEALTHY).
mod_w <- lm(data =df_w, ln_Price ~
  Rooms + Ground_Area + Home_Area + Distance_School + 
    Distance_City_Hall + Age + Municipality)
mod_n <- lm(data =df_n, ln_Price ~
  Rooms + Ground_Area + Home_Area + Distance_School + 
    Distance_City_Hall + Age + Municipality)

# Remove outliers and points with high leverage.
df_w <- remove_points_with_to_high_leverage(mod_w, df_w) 
df_n <- remove_points_with_to_high_leverage(mod_n, df_n) 

mod_w <- lm(data =df_w, ln_Price ~
  Rooms + Ground_Area + Home_Area + Distance_School + 
    Distance_City_Hall + Age + Municipality)
mod_n <- lm(data =df_n, ln_Price ~
  Rooms + Ground_Area + Home_Area + Distance_School + 
    Distance_City_Hall + Age + Municipality)

plot(mod_w)
plot(mod_n)
# Both look good.

summary(mod_w)
summary(mod_n)

# We start by reducing the wealthy model.
reduced_w <- lm(data = df_w, ln_Price ~ Home_Area + Distance_City_Hall + Age + Municipality)
anova(mod_w, reduced_w) # 0.1077
summary(reduced_w)

reduced_n <- lm(data = df_n, ln_Price ~ Home_Area + Distance_School + Municipality)
anova(mod_n, reduced_n) # 0.9285
summary(reduced_n)

# Conclusion: We get nothing interesting.


