compute_percentages_of_wealthy <- function (df) {
  percentages <- c()
  for (trend in sort(unique(cityhomes$Trend))) {
    df <- subset(cityhomes, Trend == trend)
    percentages <- append((100 * nrow(subset(df, Velhavende == 1)) / nrow(df)), percentages)  
  } 
}

create_dataset <- function (subcriteria) { # Subcriteria, is a function allowing us to specify if we want to get wealthy or not, or we might not even care.
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
  return(data.frame(Mean_Prices = prices, Municipality = cities, Trend = trends))
}

# Finally the following function allows us to compute the new indices.
compute_new_index <- function (old_index, df) {
  for (row in 1:nrow(df)) {
    if (df$Index[row] == old_index) {
      return(row)
    }
  }
  return("Couldn't compute new index.")
}

remove_points_with_too_high_leverage <- function (mod, data_frame) {
  k <- length(coefficients(mod))
  cutoff <- 3 * k / nrow(data_frame)
  print("Cutoff is: ")
  print(cutoff)
  indices_to_drop <- c()
  leverages <- hatvalues(mod)
  for (row in 1:nrow(data_frame)) {
    if (leverages[row] > cutoff) {
      indices_to_drop <- append(row, indices_to_drop)
    }
  }
  print("Removing indices with leverages:")
  print(leverages[indices_to_drop])
  return(data_frame[-indices_to_drop, ])
}
