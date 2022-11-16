old_df <- df
require("purrr")
require("faraway")
require("Hmisc")
df <- old_df 
# Modeling with corona
df$Corona <- unlist(map(df$Year, function(year) {
  if (year == 2020 ) { # Corona went into affect in 2020
    return(1)
  } else {
    return(0)
  }
}))

add_corona <- function (df) {
  df$Corona <- unlist(map(df$Year, function(year) {
    if (year == 2020 ) { # Corona went into affect in 2020
      return(1)
    } else {
      return(0)
    }
  }))
  return(df)
}

df <- add_corona(df)
# We start by reducing 
full <- lm(data = df,ln_Price ~ Rooms + Ground_Area + 
                    Home_Area + Distance_School + Distance_City_Hall +
                    Age + Wealthy + Municipality + Corona + Trend)
summary(full) 
plot(full) 

# Removing outliers.
df <- df[-c(4, compute_new_index(8, cityhomes, df))] # 8 becomes 7

full <- lm(data = df,ln_Price ~ Rooms + Ground_Area + 
                    Home_Area + Distance_School + Distance_City_Hall +
                    Age + Wealthy + Municipality + Corona + Trend)
summary(full) 
plot(full)
max(hatvalues(full))

# We see that we may remove Distance_School & Distance_City_Hall

reduced <- lm(data = df,ln_Price ~ Rooms + Ground_Area + 
                    Home_Area + Wealthy + Municipality + Corona + Trend)
summary(reduced) 

anova(reduced, full) 
# We get a p value of 0.00027 => we aren't able to remove both terms

# Checking if we can remove each term one by one.

# Removing Distance_City_Hall
reduced_2 <- lm(data = df,ln_Price ~ Rooms + Ground_Area + 
                    Home_Area + Distance_School +
                    Age + Wealthy + Municipality + Corona + Trend)
summary(reduced_2) # Can not reduce further 
anova(reduced_2, full) # p value of 0.11 => we can remove the term

# Removing Distance_School
reduced_3 <- lm(data = df,ln_Price ~ Rooms + Ground_Area + 
                    Home_Area + Distance_City_Hall +
                    Age + Wealthy + Municipality + Corona + Trend)
summary(reduced_3) 
# p values indicate that we may remove Distance_City_Hall,
# however we tried this earlier (didn't work)
anova(reduced_3, full) # p value of 0.07 => we can remove the term

summary(reduced_2)$adj.r.squared; summary(reduced_3)$adj.r.squared
# We see that the model where we removed Distance_City_Hall performs better.

plot(df$Home_Area, residuals(reduced_2))
# We will now check for colinarity using "variance of inflation" and "condition numbers"

# Variance of Inflation Factors.
# 1 means no colinearity, 1-5 means little colinearity, 5+ reconsider the model.
vif(reduced_2)


1 / (1 - summary(lm(Rooms ~ Ground_Area + Home_Area + Distance_School + Age + Wealthy + Municipality + Corona + Trend, data=df))$r.squared)
# Output: please note that we have removed the prepended municipality for demonstration purposes
# Rooms                  Ground_Area            Home_Area 
# 1.913791               1.172526               1.851480 
# 
# Distance_School        Age                    Wealthy 
# 1.454920               1.279836               1.323772 
#
# Aarhus                 Copenhagen             Odense 
# 1.368772               1.470851               1.806908 
#
# Corona                 Trend 
# 1.133202               1.154540 
# Conclusion: This does not seem to be that bad.


# Condition Numbers (we want them less than 30)
X <- model.matrix(reduced_2) # Code from vytaute
X <- scale(X, center = FALSE, scale = sqrt(colSums(X^2)))
e <- eigen(t(X) %*% X)$values
sqrt(max(e)/e)
# Output:
#  [1]  1.00  2.63  2.69  2.91  4.30  5.01
#  [7]  5.76  6.36  7.70  8.09 14.49 16.75
