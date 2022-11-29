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

# Distribution of data points in wealthy/non-wealthy areas:
table(df$Wealthy)

# Testing the relation between the Wealthy variable and geographical markers:
boxplot(data = df, Wealthy ~ Municipality,
        xlab = "Municipality", ylab = "Wealthy")
boxplot(data = df, Wealthy ~ Parish,
        xlab = "Parish", ylab = "Wealthy")

# Plotting the distribution of the Price restricted to municipalities:
boxplot(data = df, Price ~ Municipality,
        xlab = "Municipality", ylab = "Price")
boxplot(data = df, Price ~ Municipality,
        xlab = "Municipality", ylab = "Price",
        outline = F)

# The means of Price in each municipality:
mean(df$Price[df$Municipality == 'Copenhagen'])
mean(df$Price[df$Municipality == 'Aarhus'])
mean(df$Price[df$Municipality == 'Odense'])
mean(df$Price[df$Municipality == 'Aalborg'])





### Size Information ###

# First a density plot of Ground_Area, Home_Area, and Rooms:
plot(density(df$Ground_Area))
plot(density(df$Home_Area))
plot(density(df$Rooms))

# Comparison of Ground_Area and Big_Ground:
plot(data = df, Big_Ground ~ Ground_Area)
cor(df$Big_Ground, df$Ground_Area)

# Descriptions of the Ground_Area partitioned by Big_Ground:
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





### Price and Wealthy Areas ###

# Comparison of Price and Wealthy:
plot(df$Price, df$Wealthy)
boxplot(data = df, Price ~ Wealthy,
        ylab='Price')
boxplot(data = df, Price ~ Wealthy,
        ylab='Price',
        outline = F)

# Correlation between Price and Wealthy:
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

# Plotting the Age variable over Wealthy:
boxplot(data = df, Age ~ Wealthy,
        xlab = "Wealthy", ylab = "Age")

# Plotting the Age variable over Municipality:
boxplot(data = df, Distance_City_Hall ~ Municipality,
        xlab = "Municipality", ylab = "Age")





### Distance Information ###

# Comparison of Distance_School and Distance_City_Hall:
par(mfrow = c(1, 1))
plot(data = df, Distance_School ~ Distance_City_Hall,
     xlab = "Distance to City Hall", ylab = "Distance to School")
LM = lm(data = df,
        Distance_School ~ Distance_City_Hall)
abline(LM, col = "red"); summary(LM)

# Calculate the correlation:
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
par(mfrow = c(1, 2))
boxplot(data = df, Distance_School ~ Wealthy,
        xaxt = "n", xlab = "", 
        ylab = "Distance School")
axis(side = 1, at = c(1, 2), labels = c("Non-wealthy Area", "Wealthy Area"))

# Plotting Distance_School over Municipality:
boxplot(data = df, Distance_School ~ Municipality,
        xlab = "",
        ylab = "Distance School")

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
        xaxt = "n", xlab = "", 
        ylab = "Distance City Hall")
axis(side = 1, at = c(1, 2), labels = c("Non-wealthy Area", "Wealthy Area"))

# Plotting Distance_City_Hall over Municipality:
boxplot(data = df, Distance_City_Hall ~ Municipality,
        xlab = "", 
        ylab = "Distance City Hall")

# Testing relation between size variables and distance variables:
df_subset <- subset(subset(df, Home_Area < 800), Ground_Area <10000)
plot(data = df_subset, Home_Area ~ Distance_City_Hall)

# Boxplots of Distance_School partitioned by Wealthy and Municipality:
par(mfrow = c(1, 1))
NW <- subset(df, Wealthy == 0)$Distance_School
W <- subset(df, Wealthy == 1)$Distance_School
NW_O <- subset(df, Wealthy == 0 & Municipality == "Odense")$Distance_School
W_O <- subset(df, Wealthy == 1 & Municipality == "Odense")$Distance_School
NW_AL <- subset(df, Wealthy == 0 & Municipality == "Aalborg")$Distance_School
W_AL <- subset(df, Wealthy == 1 & Municipality == "Aalborg")$Distance_School
NW_AR <- subset(df, Wealthy == 0 & Municipality == "Aarhus")$Distance_School
W_AR <- subset(df, Wealthy == 1 & Municipality == "Aarhus")$Distance_School
NW_C <- subset(df, Wealthy == 0 & Municipality == "Copenhagen")$Distance_School
W_C <- subset(df, Wealthy == 1 & Municipality == "Copenhagen")$Distance_School

boxplot(NW, W, NW_O, W_O, NW_AL, W_AL, NW_AR, W_AR, NW_C, W_C,
        xaxt = "n",
        ylab = "Distance School", 
        col = c("Red", "Blue"))
axis(1, at = c(1.5, 3.5, 5.5, 7.5, 9.5), labels = c("General", "Odense", "Aalborg", "Aarhus", "Copenhagen"))
legend(x = "topright", legend = c("Non-wealthy", "Wealthy"), col = c("Red", "Blue"), 
       lty = rep(1, 2), cex = 1, box.lty = 1)

# Boxplots of Distance_City_Hall partitioned by Wealthy and Municipality:
NW <- subset(df, Wealthy == 0)$Distance_City_Hall
W <- subset(df, Wealthy == 1)$Distance_City_Hall
NW_O <- subset(df, Wealthy == 0 & Municipality == "Odense")$Distance_City_Hall
W_O <- subset(df, Wealthy == 1 & Municipality == "Odense")$Distance_City_Hall
NW_AL <- subset(df, Wealthy == 0 & Municipality == "Aalborg")$Distance_City_Hall
W_AL <- subset(df, Wealthy == 1 & Municipality == "Aalborg")$Distance_City_Hall
NW_AR <- subset(df, Wealthy == 0 & Municipality == "Aarhus")$Distance_City_Hall
W_AR <- subset(df, Wealthy == 1 & Municipality == "Aarhus")$Distance_City_Hall
NW_C <- subset(df, Wealthy == 0 & Municipality == "Copenhagen")$Distance_City_Hall
W_C <- subset(df, Wealthy == 1 & Municipality == "Copenhagen")$Distance_City_Hall

boxplot(NW, W, NW_O, W_O, NW_AL, W_AL, NW_AR, W_AR, NW_C, W_C,
        xaxt = "n",
        ylab = "Distance City Hall", 
        col = c("Red", "Blue"))
axis(1, at = c(1.5, 3.5, 5.5, 7.5, 9.5), labels = c("General", "Odense", "Aalborg", "Aarhus", "Copenhagen"))
legend(x = "topright", legend = c("Non-wealthy", "Wealthy"), col = c("Red", "Blue"), 
       lty = rep(1, 2), cex = 1, box.lty = 1)





# Cleaning
rm(LM, df_subset, NW, W, NW_O, W_O, NW_AL, W_AL, NW_AR, W_AR, NW_C, W_C)

# rm(list=ls())
