require("purrr")
require("faraway")
require("Hmisc")
# Modeling with corona
f <- function(indicated_year) {
  return(unlist(map(df$Year, function(year) {
  if (year == indicated_year) { # Corona went into affect in 2020
    return(1)
  } else {
    return(0)
  }
  })))
}
df$Corona_2020 <- f(2020)
df$Corona_2021 <- f(2021)
df$Corona_2022 <- f(2022)
# We start by removing outliers using the full model
full <- lm(data = df,ln_Price ~ Rooms + Ground_Area + 
                    Home_Area + Distance_School + Distance_City_Hall +
                    Age + Wealthy + Municipality +Corona_2020+ Corona_2021 + Corona_2022 + Trend)
summary(full) 
plot(full) 

# Removing outliers.
df <- remove_points_with_to_high_leverage(full, df)

full <- lm(data = df,ln_Price ~ Rooms + Ground_Area + 
                    Home_Area + Distance_School + Distance_City_Hall +
                    Age + Wealthy + Municipality + Corona_2021 + Corona_2022 + Trend)
# Indicates that we mightwant to remove rooms, ground_area, distance_school & age
summary(full) 


# We see that we may remove rooms, ground_area, distance_school, age
reduced <- lm(data = df,ln_Price ~  
                Home_Area + Distance_City_Hall +
                Wealthy + Municipality + Corona_2021 + Trend)
summary(reduced) 

anova(reduced, full) 
# We get a p value of 0.1661 => we are able to remove all terms

# Variance of Inflation Factors.
# 1 means no colinearity, 1-5 means little colinearity, 5+ reconsider the model.
vif(reduced)

# Output: 
# Home_Area              Distance_City_Hall     Wealthy     MunicipalityCopenhagen 
# 1.144756               2.727092               1.845909    1.409125 
# MunicipalityAarhus     MunicipalityOdense     Corona      Trend 
# 1.636678               1.860744               1.138364    1.158779 
# Conclusion: This does not seem to be that bad.


# Condition Numbers (we want them less than 30) NOTE: this code is from vytaute
X <- model.matrix(reduced) 
X <- scale(X, center = FALSE, scale = sqrt(colSums(X^2)))
e <- eigen(t(X) %*% X)$values
sqrt(max(e)/e)
# Output:
#  [1]  1.000000  2.227809  2.268016  2.454677  3.916728  4.291657  6.391971  9.420670 18.389007
# Conclusion: This does not seem to be that bad.
