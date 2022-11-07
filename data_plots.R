library("ggplot2")

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

general_data <- create_dataset(function(df) {return(rep(TRUE, length(df$Trend)))})
wealthy_data <- create_dataset(function(df) {return(df$Walthy == 1)})
non_wealthy_data <- create_dataset(function(df) {return(df$Walthy == 0)})

# TODO: Use a different color pallate.
ggplot(general_data, aes(x=Trend, y=Mean_Prices)) + 
  scale_fill_brewer(palette="Set1")+
  geom_line(aes(colour=Municipality)) + 
  geom_point(aes(colour=Municipality)) 

