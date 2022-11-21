# The partial goal of the program is to:
# 1) define indicators for the municipalities
# 2) define the desired variables 'locally', that is separately for each municipality
# 3) construct a general linear model based on multiple linear regression



# Function, which creates indicator vectors based on a data vector and item to indicate.
indicator <- function(data, category){
  ind <- numeric(length(data)) # creates the indicator vector with all zeroes
  
  for (i in 1:length(data)){
    if (data[i] == category){
      ind[i] <- 1 # adds one in an entry if it is the desired category
    }
  }
  
  return (ind)
}



# Defining a model data frame by adding ln_Price and the local variables.
municipality_vector <- df[,17] # municipality data vector
ln_Price <- df[,18] # ln_Price data vector

model_df <- data.frame(ln_Price) # data frame with only ln_Price

# For loops to construct the indicators for municipalities and local variables.
for (municipality in c("Copenhagen", "Aarhus", "Odense", "Aalborg")){
  ind <- paste("Ind", municipality, sep = "_")
  df[[ind]] <- indicator(municipality_vector, municipality) # adding indicators to the main data frame
  
  for (variable in c("Rooms", "Ground_Area", "Home_Area", "Distance_School", "Distance_City_Hall", "Age", "Wealthy")){
    new_variable <- paste(municipality, variable, sep = "_")
    model_df[[new_variable]] <- (df[[variable]]) * (df[[ind]]) # adding local variables to the model data frame
    
  }
}

# Cleaning.
rm(municipality_vector, ln_Price, municipality, ind, variable, new_variable)



# Creating the model based on the choices:
#  - the response variable is ln_Price, the logarithmic Price
#  - the explanatory variables are the local variables constructed above
#  - the relationship will be multiple linear regression
mod <- lm(data = model_df, ln_Price ~ .)
summary(mod)

# Manuel backward elimination.
mod_red <- lm(model_df[,-c(3, 5, 6, 9, 12, 13, 14, 16, 19, 23, 24, 26)])
summary(mod_red)

step(mod)
