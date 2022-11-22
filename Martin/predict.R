# Model and data specificiation 
GLM_Reduced_1 <- lm(data = df, Price ~ Fin_Trend + Rooms + Ground_Area + 
                 Home_Area + Distance_City_Hall + Wealthy + Municipality)

# Predict on test dataset.
predicted_prices <- predict(GLM_Reduced_1, newdata=test_df)
plot(test_df$Fin_Trend, predicted_prices)
residuals <- test_df$Price - predicted_prices
residuals
plot(test_df$Fin_Trend, residuals)
plot(residuals)

# Predict for training dataset 
df$Predicted_Price <- predict(GLM_Reduced_1, newdata=df)
df$Residuals <- df$Predicted_Price - df$Price


Fin_Trend_Plot <- ggplot(df, aes(x = Fin_Trend, y = Residuals)) + geom_point()
Rooms_Plot <- ggplot(df, aes(x = Rooms, y = Residuals)) + geom_point()
Ground_Area_Plot <- ggplot(df, aes(x = Ground_Area, y = Residuals)) + geom_point()
Home_Area_Plot <- ggplot(df, aes(x = Home_Area, y = Residuals)) + geom_point()
Distance_City_Hall_Plot <- ggplot(df, aes(x = Distance_City_Hall, y = Residuals)) + geom_point()
Wealthy_Plot <- ggplot(df, aes(x = Wealthy, y = Residuals)) + geom_point()
Municipality_Plot <- ggplot(df, aes(x = Municipality, y = Residuals)) + geom_point()

install.packages("minqa")
install.packages("SparseM")
install.packages("pbkrtest")
install.packages("ggpubr",
                 repos = c("https://cran.rediris.org/", "https://cloud.r-project.org/"),
                 dependencies = TRUE)
plot(GLM_Reduced_1)
install.packages("nloptr")
library("ggpubr")
ggarrange(
 Fin_Trend_Plot,
 Rooms_Plot,
 Ground_Area_Plot,
 Home_Area_Plot,  
 Distance_City_Hall,
 Wealthy_Plot,         
 Municipality_Plot,          
 labels = c("A", "B", "C", "D", "E", "F", "G"),
 ncol = 2, nrow = 3)
