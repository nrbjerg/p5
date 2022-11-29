# Uses functions from linear_regression.R.

### General Functions ###

indicator <- function(data, category){
  # Creates the indicator vector with all zeroes.
  ind <- numeric(length(data))
  
  for (i in 1:length(data)){
    if (data[i] == category){
      # Adds one in an entry if it is the desired category.
      ind[i] <- 1
    }
  }
  
  return (ind)
}





### Local Variables ###

# Defining a model data frame by adding ln_Price and the local variables.
municipality_vector <- df[,17]

model_df <- data.frame(Trend = df[,1], ln_Price = df[,18])

# For loops to construct the indicators for municipalities and local variables.
for (municipality in c("Copenhagen", "Aarhus", "Odense", "Aalborg")){
  ind <- paste("Ind", municipality, sep = "_")
  df[[ind]] <- indicator(municipality_vector, municipality)
  
  for (variable in c("Rooms", "Ground_Area", "Home_Area", "Distance_School", "Distance_City_Hall", "Age", "Wealthy")){
    new_variable <- paste(municipality, variable, sep = "_")
    model_df[[new_variable]] <- (df[[variable]]) * (df[[ind]])
    
  }
}

rm(municipality_vector, municipality, ind, variable, new_variable)





### Model with Local Variables ###

# Creating a full model with linear regression and local variables.
model_df_training <- subset(model_df, Trend < 12)
model_df_test <- subset(model_df, Trend == 12)

mod <- lm(data = model_df_training, ln_Price ~ . -Trend)
summary(mod)

# Residual plots.
residual_plots(mod)

# Outliers from leverages and Cooks distance.
outliers <- leverages_cooks_distance(mod)

# Model without outliers.
mod <- lm(data = model_df_training[-outliers,], ln_Price ~ . -Trend)
summary(mod)

# Model from backward elimination with significance level 5%.
mod_backward_elimination <- lm(data = model_df_training[-outliers,], 
                               ln_Price ~ . -Trend-Copenhagen_Ground_Area-Odense_Distance_School-Copenhagen_Distance_School-Copenhagen_Rooms-Odense_Rooms-Aarhus_Distance_City_Hall-Aalborg_Rooms-Aalborg_Ground_Area-Aarhus_Ground_Area-Aarhus_Age-Aalborg_Distance_School-Copenhagen_Distance_City_Hall-Aarhus_Distance_School)
summary(mod_backward_elimination)
  
# Model from AIC criterion and search methods.
mod_red <- step(mod)
summary(mod_red)

# F-test the reduction using step.
anova(mod, mod_red)

# Residual plots.
residual_plots(mod_red)

# Predictions.
par(mfrow = c(1, 2))

# Plot the predicted values for the training set with confidence and prediction intervals.
predictions(mod_red, model_df_training[-outliers,], "ln_Price", "Logarithm of Price")

# Plot the prediction values for the test set with confidence and prediction intervals.
predictions(mod_red, model_df_test, "ln_Price", "Logarithm of Price")

# Cleaning.
rm(mod, mod_red, mod_backward_elimination, model_df, model_df_test, model_df_training, outliers)
