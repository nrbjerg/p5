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
ln_Price <- df[, c(1, 18)] # ln_Price data vector

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
model_df_training <- subset(model_df, Trend < 12)
model_df_test <- subset(model_df, Trend == 12)

mod <- lm(data = model_df_training, ln_Price ~ . -Trend)
summary(mod)

outliers <- c()

# Residual plots.
par(mfrow = c(2, 2))

plot(mod, which = 1, caption = "", sub.caption = "")
plot(mod, which = 2, caption = "", sub.caption = "")
plot(mod, which = 3, caption = "", sub.caption = "")
plot(mod, which = 5, caption = "", sub.caption = "")

# Outliers from the leverages.
leverages <- hatvalues(mod)

for (i in 1:length(leverages)){
  if (leverages[i] > 3 * sum(leverages) / length(leverages)){
    print(i)
    outliers <- append(outliers, i)
  }
}

# Outliers from the Cook's distances.
cooks_distance <- cooks.distance(mod)

for (i in 1:length(cooks_distance)){
  if (cooks_distance[i] >= 0.5){
    print(i)
    outliers <- append(outliers, i)
  }
}

# Using the step function for stepwise elimination of parameters.
mod <- lm(data = model_df_training[-outliers,], ln_Price ~ . -Trend)
summary(mod)

mod_backward_elimination <- lm(data = model_df_training[-outliers,], 
                               ln_Price ~ . -Trend-Copenhagen_Ground_Area-Odense_Distance_School-Copenhagen_Distance_School-Copenhagen_Rooms-Odense_Rooms-Aarhus_Distance_City_Hall-Aalborg_Rooms-Aalborg_Ground_Area-Aarhus_Ground_Area-Aarhus_Age-Aalborg_Distance_School-Copenhagen_Distance_City_Hall-Aarhus_Distance_School)
summary(mod_backward_elimination)
  
mod_red <- step(mod)
summary(mod_red)

anova(mod, mod_red)

# Residual plots.
par(mfrow = c(2, 2))

plot(mod_red, which = 1, caption = "", sub.caption = "")
plot(mod_red, which = 2, caption = "", sub.caption = "")
plot(mod_red, which = 3, caption = "", sub.caption = "")
plot(mod_red, which = 5, caption = "", sub.caption = "")



# Predictions.
par(mfrow = c(1, 2))

# Calculating and adding predictions of the training data based on the reduced 
# log model together with prediction and confidence intervals.
model_df_training_no_outliers <- model_df_training[-outliers,]

pred_confint = predict(mod_red, model_df_training_no_outliers, 
                       interval = 'confidence')
pred_confint = as.data.frame(pred_confint)

model_df_training_no_outliers$Fitted <- pred_confint$fit
model_df_training_no_outliers$Lower_Bound_Confint <- pred_confint$lwr
model_df_training_no_outliers$Upper_Bound_Confint <- pred_confint$upr

predint = predict(mod_red, model_df_training_no_outliers, 
                  interval = 'prediction')
predint = as.data.frame(predint)

model_df_training_no_outliers$Lower_Bound_Predint <- predint$lwr
model_df_training_no_outliers$Upper_Bound_Predint <- predint$upr

# Plotting the confidence and prediction intervals against fitted values.
model_df_training_no_outliers_arranged = arrange(model_df_training_no_outliers, Fitted)

plot(data = model_df_training_no_outliers_arranged, ln_Price ~ Fitted,
     xlab = "Predictions", ylab = "Logarithm of Price")

lines(model_df_training_no_outliers_arranged$Fitted, model_df_training_no_outliers_arranged$Fitted, 
      col = "Red", lwd = 1.5)
lines(model_df_training_no_outliers_arranged$Fitted, model_df_training_no_outliers_arranged$Lower_Bound_Confint, 
      col = "Green", lwd = 1.5)
lines(model_df_training_no_outliers_arranged$Fitted, model_df_training_no_outliers_arranged$Upper_Bound_Confint, 
      col = "Green", lwd = 1.5)
lines(model_df_training_no_outliers_arranged$Fitted, model_df_training_no_outliers_arranged$Lower_Bound_Predint, 
      col = "Blue", lwd = 1.5)
lines(model_df_training_no_outliers_arranged$Fitted, model_df_training_no_outliers_arranged$Upper_Bound_Predint, 
      col = "Blue", lwd = 1.5)

# Counting the number of data points inside the prediction interval.
count_inside_prediction <- 0

for (i in 1:length(model_df_training_no_outliers_arranged$ln_Price))
{if (model_df_training_no_outliers_arranged$Lower_Bound_Predint[i] <= model_df_training_no_outliers_arranged$ln_Price[i] 
     && model_df_training_no_outliers_arranged$ln_Price[i] <= model_df_training_no_outliers_arranged$Upper_Bound_Predint[i]) {
  count_inside_prediction = count_inside_prediction + 1
} else {
  0
}}

count_inside_prediction
count_inside_prediction / length(model_df_training_no_outliers_arranged$ln_Price)

# We now replicate the above, but predicting the test data frame. First, we
# calculate the predictions, and prediction and confidence intervals.
pred_confint = predict(mod_red, model_df_test, 
                       interval = 'confidence')
pred_confint = as.data.frame(pred_confint)

model_df_test$Fitted <- pred_confint$fit
model_df_test$Lower_Bound_Confint <- pred_confint$lwr
model_df_test$Upper_Bound_Confint <- pred_confint$upr

predint = predict(mod_red, model_df_test, 
                  interval = 'prediction')
predint = as.data.frame(predint)

model_df_test$Lower_Bound_Predint <- predint$lwr
model_df_test$Upper_Bound_Predint <- predint$upr

# Plotting the confidence and prediction intervals against fitted values.
model_df_test_arranged = arrange(model_df_test, Fitted)

plot(data = model_df_test_arranged, ln_Price ~ Fitted,
     xlab = "Predictions", ylab = "Logarithm of Price")

lines(model_df_test_arranged$Fitted, model_df_test_arranged$Fitted, 
      col = "Red", lwd = 1.5)
lines(model_df_test_arranged$Fitted, model_df_test_arranged$Lower_Bound_Confint, 
      col = "Green", lwd = 1.5)
lines(model_df_test_arranged$Fitted, model_df_test_arranged$Upper_Bound_Confint, 
      col = "Green", lwd = 1.5)
lines(model_df_test_arranged$Fitted, model_df_test_arranged$Lower_Bound_Predint, 
      col = "Blue", lwd = 1.5)
lines(model_df_test_arranged$Fitted, model_df_test_arranged$Upper_Bound_Predint, 
      col = "Blue", lwd = 1.5)

# Counting the number of data points inside the prediction interval.
count_inside_prediction = 0

for (i in 1:length(model_df_test_arranged$ln_Price))
{if (model_df_test_arranged$Lower_Bound_Predint[i] <= model_df_test_arranged$ln_Price[i] 
     && model_df_test_arranged$ln_Price[i] <= model_df_test_arranged$Upper_Bound_Predint[i]) {
  count_inside_prediction = count_inside_prediction + 1
} else {
  0
}}

count_inside_prediction
count_inside_prediction / length(model_df_test_arranged$ln_Price)

# Counting the number of data points with ln_Price larger than the prediction.
count_bigger_than_prediction = 0

for (i in 1:length(model_df_test_arranged$ln_Price))
{if (model_df_test_arranged$Fitted[i] < model_df_test_arranged$ln_Price[i]) {
  count_bigger_than_prediction = count_bigger_than_prediction + 1
} else {
  0
}}

count_bigger_than_prediction
count_bigger_than_prediction / length(model_df_test_arranged$ln_Price)

# Cleaning.
rm(mod, mod_red, model_df, model_df_test, model_df_training)
rm(mod_backward_elimination, model_df_test_arranged, 
   model_df_training_no_outliers, model_df_training_no_outliers_arranged,
   pred_confint, predint, cooks_distance, count_bigger_than_prediction,
   count_inside_prediction, i, leverages, outliers)
