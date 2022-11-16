require("purrr")
require("faraway")
require("Hmisc")

# Modeling with corona
df$Corona <- unlist(map(df$Year, function(year) {
  if (year == 2020 ) { # Corona went into affect in 2020
    return(1)
  } else {
    return(0)
  }
}))
mod <- lm(data = df,ln_Price ~ Rooms + Ground_Area + 
                    Home_Area + Wealthy + Municipality + Corona + Trend)
summary(mod)

# Variance inflation factors (1 means no colinearity, 1-5 means little colinearity, 5+ reconsider the model.)
vif(mod)
# Rooms                  ln_Ground_Area            ln_Home_Area           Wealthy 
# 2.223377               1.678412                  2.328144               1.299075 
# 
# MunicipalityAarhus     MunicipalityCopenhagen    MunicipalityOdense     Trend 
# 1.345726               1.679809                  1.395269               1.162285 
#
# Corona 
# 1.130922 

# Checking conditon numbers (High values are considered evidence that we have colinarity (we want them less than 30))
X <- model.matrix(mod) # Code from vytaute
X <- scale(X, center = FALSE, scale = sqrt(colSums(X^2)))
e <- eigen(t(X) %*% X)$values
sqrt(max(e)/e)
# Output for mod without corona and trend:  1.00 2.40 2.43 2.60 3.90 4.47 5.45 6.95 10.84 13.86
# Output for mod with corona and trend:     1.00 2.20 2.24 3.57 4.11 5.75 8.84 12.6

# Other options
library(olsrr)
ols_eigen_cindex(mod)