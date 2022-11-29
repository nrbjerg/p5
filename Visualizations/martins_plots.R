create_dataset <- function (subcriteria) { # Subcriteria, is a function allowing us to specify if we want to get wealthy or not, or we might not even care
  prices <- c()
  cities <- c()
  trends <- c()
  for (trend in c(1:12)) {
    for (city in c("Copenhagen", "Aarhus", "Aalborg", "Odense")) {
      prices <- c(prices, mean(df$Price[subcriteria(df) & df$Municipality == city & df$Trend == trend]))
      cities <- c(cities, city)
      trends <- c(trends, trend)
    }
    prices <- c(prices, mean(df$Price[subcriteria(df) & df$Trend == trend]))
    cities <- c(cities, "General")
    trends <- c(trends, trend)
  }
  return(data.frame(Mean_Prices = prices, Municipality=cities, Trend = trends))
}

color_palette <- function (municipality) {
  if (municipality == "Copenhagen") {
    return("red")
  }
  if (municipality == "Aarhus") {
    return("blue")
  }
  if (municipality == "Odense") {
    return("green")
  }
  if (municipality == "Aalborg") {
    return("purple")  
  }
  return("black")
}

vector_color_palette <- Vectorize(color_palette)

plot_data <- function(df, title) {
  municipalities = unique(df$Municipality)
  plot(df$Trend, df$Mean_Prices, col = vector_color_palette(df$Municipality), xlab="Trend", ylab="Mean Prices", main=title, pch=16)
  legend(x="topleft", legend=municipalities, col=vector_color_palette(municipalities), lty=rep(1, 5), cex=1)
  for (municipality in municipalities) {
    indicies = df$Municipality == municipality
    lines(df$Trend[indicies], df$Mean_Prices[indicies], col = color_palette(municipality), type="l")      
  }
}

general_data <- create_dataset(function(df) {return(rep(TRUE, length(df$Trend)))})
wealthy_data <- create_dataset(function(df) {return(df$Walthy == 1)})
non_wealthy_data <- create_dataset(function(df) {return(df$Walthy == 0)})

par(mfrow = c(1, 1))
plot_data(general_data, "")

par(mfrow = c(1, 2))

plot_data(wealthy_data, "Wealthy Data")
plot_data(non_wealthy_data, "Non Wealthy Data")

# Create plots of ground area plottet against home area
par(mfrow = c(1, 2))
plot(df$Ground_Area, df$Home_Area, xlab="Ground Area", ylab="Home Area")
abline(coefficients(lm(Home_Area ~ Ground_Area, df)), col="red")
plot(df$Ground_Area[-c(4, 118, 120)], df$Home_Area[-c(4, 118, 120)], xlab="Ground Area", ylab="Home Area")
abline(coefficients(lm(Home_Area ~ Ground_Area, df[-c(4, 118, 120),])), col="red")

par(mfrow = c(1, 2))
plot(df$Home_Area, df$Rooms, xlab="Home Area", ylab="Number Of Rooms")
abline(coefficients(lm(Rooms ~ Home_Area, df)), col="red")
plot(df$Home_Area[-c(4, 8)], df$Rooms[-c(4, 8)], xlab="Home Area", ylab="Number Of Rooms")
abline(coefficients(lm(Rooms ~ Home_Area, df[-c(4, 8),])), col="red")

# Plotting the data
par(mfrow(1, 2))
plot(density(df$Price), xlab="Price", main="")
plot(density(log(df$Price)), xlab="ln(Price)", main="")

# Wealthy percentage plots
percentages <- c()
municipalities <- c()
trends <- c()
for (trend in sort(unique(df$Trend))) {
  current <- subset(df, Trend == trend)
  for (municipality in c("Copenhagen", "Aarhus", "Aalborg", "Odense")) {
    specific <- subset(current, Municipality == municipality)
    percentages <- append((100 * nrow(subset(specific, Wealthy == 1)) / nrow(specific)), percentages)  
    municipalities <- append(municipality, municipalities)
    trends <- append(trend, trends)
  }
  percentages <- append((100 * nrow(subset(current, Wealthy == 1)) / nrow(current)), percentages)  
  municipalities <- append("General", municipalities)
  trends <- append(trend, trends) 
}

df_p <- data.frame(Municipality <- municipalities, Trend = trends, Percentages = percentages)
municipalities = unique(df_p$Municipality)
plot(df_p$Trend, df_p$Percentages, col = vector_color_palette(df_p$Municipality), xlab="Trend", ylab="Percentages of Wealthy Homes",  pch=16)
legend(x="bottomleft", legend=municipalities, col=vector_color_palette(municipalities), lty=rep(1, 5), cex=1)
for (municipality in municipalities) {
  indicies = df_p$Municipality == municipality
  lines(df_p$Trend[indicies], df_p$Percentages[indicies], col = color_palette(municipality), type="l")      
}

