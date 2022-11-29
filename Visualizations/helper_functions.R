compute_percentages_of_wealthy <- function (df) {
  percentages <- c()
  for (trend in sort(unique(cityhomes$Trend))) {
    df <- subset(cityhomes, Trend == trend)
    percentages <- append((100 * nrow(subset(df, Velhavende == 1)) / nrow(df)), percentages)  
  } 
}

