for (municipality in c("Copenhagen", "Aarhus", "Aalborg", "Odense")) {
  print(municipality)
  df <- subset(training_df, Municipality==municipality)
  plot(density(df$ln_Price), main=municipality)
  plot(density(subset(df, Wealthy==1)$ln_Price), main=sprintf("wealthy, %s", municipality))
  plot(density(subset(df, Wealthy==0)$ln_Price), main=sprintf("non wealthy, %s", municipality))
}
plot(density(subset(training_df, Municipality=="Copenhagen")$ln_Price), xlab="ln_Price", main="")
plot(density(subset(training_df, Municipality=="Aarhus")$ln_Price), xlab="ln_Price", main="")
plot(density(subset(training_df, Municipality=="Aalborg")$ln_Price), xlab="ln_Price", main="")
