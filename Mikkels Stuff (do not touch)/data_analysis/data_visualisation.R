# General summary of the data set
summary(df)

# Distribution of the Price:
summary(df$Price)
hist(df$Price)
boxplot(df$Price)





### Geographical Information ###

# Distribution of data points in each geographical category:
table(df$Municipality)
table(df$Parish)
table(df$Voting_Area)

# Example of testing the distribution of a geographical category, when 
# restricted to a larger category:
table(df$Parish[df$Municipality == 'Copenhagen'])
table(df$Voting_Area[df$Parish == 'Husumvold'])

# There is total inclusion of the geographical categories.

# Distribution of data points in wealthy/non-wealthy areas:
table(df$Wealthy)

# Testing the relation between the Wealthy variable and geographical markers:
boxplot(data = df, Wealthy ~ Municipality,
        xlab = "Municipality", ylab = "Wealthy")
boxplot(data = df, Wealthy ~ Parish,
        xlab = "Parish", ylab = "Wealthy")

# Wealthy is dependent on Parish.

# Plotting the distribution of the Price restricted to municipalities:
boxplot(data = df, Price ~ Municipality,
        xlab = "Municipality", ylab = "Price")
boxplot(data = df, Price ~ Municipality,
        xlab = "Municipality", ylab = "Price",
        outline = F)

# We see a clear difference in the distribution of the Price in each 
# municipality.

# The means of Price in each municipality illustrates this:
mean(df$Price[df$Municipality == 'Copenhagen'])
mean(df$Price[df$Municipality == 'Aarhus'])
mean(df$Price[df$Municipality == 'Odense'])
mean(df$Price[df$Municipality == 'Aalborg'])

# The larger the city, the larger the mean.





### Size Information ###

# Comparison of Ground_Area and Big_Ground:
plot(data = df, Big_Ground ~ Ground_Area)
cor(df$Big_Ground, df$Ground_Area)

# We see total dependence of Big_Ground on Ground_Area. The maximum of the
# small grounds and minimum of the big grounds illustrate this:
summary(df$Ground_Area[df$Big_Ground == 0])
summary(df$Ground_Area[df$Big_Ground == 1])

# Lets visualise this with a plot:
boxplot(data = df, Ground_Area ~ Big_Ground)
boxplot(data = df, Ground_Area ~ Big_Ground,
        outline = F)

# Comparison of Home_Area and Ground_Area:
plot(data = df, Home_Area ~ Ground_Area,
     xlab = "Ground Area", ylab = "Home Area")
LM = lm(data = df,
        Home_Area ~ Ground_Area)
abline(LM, col = "red"); summary(LM)

# Calculating the correlation:
cor(df$Home_Area, df$Ground_Area)

# Removing the extreme points:
df_subset <- subset(subset(df, Home_Area < 800), Ground_Area <10000)
plot(data = df_subset, Home_Area ~ Ground_Area,
     xlab = "Ground Area", ylab = "Home Area")
LM = lm(data = df_subset,
        Home_Area ~ Ground_Area)
abline(LM, col = "red"); summary(LM)

# Calculating the correlation:
cor(df_subset$Home_Area, df_subset$Ground_Area)

# There is more correlation between the variables when the abnormal values are 
# removed.

# Comparison of Home_Area and Rooms:
plot(data = df, Rooms ~ Home_Area,
     xlab = "Home Area", ylab = "Rooms")
LM = lm(data = df,
        Rooms ~ Home_Area)
abline(LM, col = "red"); summary(LM)

# Calculating the correlation:
cor(df$Home_Area, df$Rooms)

# Removing the extreme point:
df_subset <- subset(df, Home_Area < 800)
plot(data = df_subset, Rooms ~ Home_Area,
     xlab = "Home Area", ylab = "Rooms")
LM = lm(data = df_subset,
        Rooms ~ Home_Area)
abline(LM, col = "red"); summary(LM)

# Calculating the correlation:
cor(df_subset$Rooms, df_subset$Home_Area)

# There is more correlation between the variables when the abnormal values are 
# removed.





### Price and Wealthy Areas ###

# Comparison of Price and Wealthy:
plot(df$Price, df$Wealthy)
boxplot(data = df, Price ~ Wealthy,
        ylab='Price')
boxplot(data = df, Price ~ Wealthy,
        ylab='Price',
        outline = F)

# Wealthy areas cover all of the extreme sales. There is a clear difference in 
# pricepoints.
cor(df$Price, df$Wealthy)





### Time Information ###

# Distribution of sales over the years and months:
table(df$Year)
table(df$Month)

# Comparing the Price evolving over years:
plot(data = df, Price ~ Trend)
LM = lm(data = df,
        Price ~ Trend)
abline(LM, col = "red"); summary(LM)

# The correlation is:
cor(df$Price, df$Trend)

# Plotting the price in different months:
boxplot(data = df, Price ~ Month)
boxplot(data = df, Price ~ Month,
        outline = F)





### Distance Information ###

# Comparison of Distance_School and Distance_City_Hall:
plot(data = df, Distance_School ~ Distance_City_Hall,
     xlab = "Distance to City Hall", ylab = "Distance to School")
LM = lm(data = df,
        Distance_School ~ Distance_City_Hall)
abline(LM, col = "red"); summary(LM)

# No trend is seen. Calculate the correlation:
cor(df$Distance_School, df$Distance_City_Hall)

# Comparison of Distance_School and Price:
cor(df$Distance_School, df$Price)

# Plots:
plot(data = df, Price ~ Distance_School,
     xlab = "Distance School", ylab = "Price")
LM = lm(data = df,
        Price ~ Distance_School)
abline(LM, col = "red"); summary(LM)

# Plotting Distance_School over Wealthy:
boxplot(data = df, Distance_School ~ Wealthy,
        xlab = "Wealthy", ylab = "Distance School")

# Comparison of Distance_City_Hall and Price:
cor(df$Distance_City_Hall, df$Price)

# Plots:
plot(data = df, Price ~ Distance_City_Hall,
     xlab = "Distance City Hall", ylab = "Price")
LM = lm(data = df,
        Price ~ Distance_City_Hall)
abline(LM, col = "red"); summary(LM)

# Plotting Distance_City_Hall over Wealthy:
boxplot(data = df, Distance_City_Hall ~ Wealthy,
        xlab = "Wealthy", ylab = "Distance City Hall")

# The distance variables do not seem all that much dependent on Price and 
# Wealthy with the exception being that wealthy areas tend to be closer to the 
# city hall.

# Testing relation between size variables and distance variables:
df_subset <- subset(subset(df, Home_Area < 800), Ground_Area <10000)
plot(data = df_subset, Home_Area ~ Distance_City_Hall)

# No relation seems likely.

par(mfrow = c(1, 2))
boxplot(data = df, Distance_School ~ Wealthy,
        xlab = "Wealthy", ylab = "Distance School")
boxplot(data = df, Distance_City_Hall ~ Wealthy,
        xlab = "Wealthy", ylab = "Distance City Hall")






# Cleaning
rm(list=ls())
