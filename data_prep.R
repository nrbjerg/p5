cityhomes$index <- 1:872
df <- cityhomes # Shorthand from now on.

# Adds fin trend, which is trend + 1/12 * month_number.
df$Salgsmaaned_encoded <- as.numeric(factor(df$Salgsmaaned, levels = unique(df$Salgsmaaned), exclude = NULL))
df$Fin_Trend <- df$Trend +  1/12 * df$Salgsmaaned_encoded

# Convert column names to english
df$Price <- df$Pris_Salg
df$Rooms <- df$Ejd_AntalRum
df$Year <- df$Salgsaar
df$Month <- df$Salgsmaaned
df$Ground_Area <- df$Areal_Grund
df$Home_Area <- df$Areal_Bolig
df$Big_Ground <- df$StorGrund
df$Voting_Area <- df$Afstemningsomraade
df$Distance_School <- df$Dist_skole
df$Distance_City_Hall <- df$Dist_raadhus
df$Age <- df$Alder
df$Parish <- df$Sogn
df$Wealthy <- df$Velhavende
df$Municipality <- df$KommuneNavn

drop <- c("Pris_Salg","Areal_Bolig", "Areal_Grund", "Ejd_AntalRum", "Salgsmaaned", "StorGrund", "Dist_skole", "Dist_raadhus", "Dist_Town_Hall", "KommuneNavn", "Velhavende", "Alder", "Dist_raadhus", "Dist_skole", "Sogn", "Afstemningsomraade", "Salgsaar", "Salgsmaaned_encoded")
df = df[,!(names(df) %in% drop)]

#install.packages("stringr")
library(stringr)

# Convert the data point strings to english 
df$Municipality <- str_replace(df$Municipality, "København", "Copenhagen")

df$Parish <- str_replace(df$Parish , "å", "aa")
df$Parish <- str_replace(df$Parish , "ø", "oe")

df$Voting_Area <- str_replace(df$Voting_Area , "å", "aa")
df$Voting_Area <- str_replace(df$Voting_Area , "ø", "oe")
df$Voting_Area <- str_replace(df$Voting_Area , "Ø", "Oe")
df$Voting_Area <- str_replace(df$Voting_Area , "æ", "ae")
df$Voting_Area <- str_replace(df$Voting_Area, "Nordvest", "Northwest")
df$Voting_Area <- str_replace(df$Voting_Area, "Vest", "West")
df$Voting_Area <- str_replace(df$Voting_Area, "Nord", "North")


df$Month <- str_replace(df$Month, "Januar", "January")
df$Month <- str_replace(df$Month, "Februar", "February")
df$Month <- str_replace(df$Month, "Marts", "March")
df$Month <- str_replace(df$Month, "Maj", "May")
df$Month <- str_replace(df$Month, "Juni", "June")
df$Month <- str_replace(df$Month, "Juli", "July")
df$Month <- str_replace(df$Month, "Oktober", "October")


# Remove missing data points 

df <- df[-c(28, 113, 115, 560, 727, 806, 66, 271, 493), ]


# Adding columns by taking the natural logarithm

df$ln_Price <- log(df$Price)
df$ln_Home_Area <- log(df$Home_Area)
df$ln_Ground_Area <- log(df$Ground_Area)
df$ln_Rooms <- log(df$Rooms)
df$ln_Distance_School <- log(df$Distance_School)
df$ln_Distance_City_Hall <- log(df$Distance_City_Hall)
df$ln_Age <- log(df$Age) # Beware of inf values


# Saving

test_df <- subset(df, Year == 2022)
training_df <- subset(df, Year != 2022)

save(cityhomes, file = "cityhomes.Rda")
save(df, file = "data_frame.Rda")
save(training_df, file = "training.Rda")
save(test_df, file = "test.Rda")


# Finally the following function allows us to compute the new indicies
compute_new_index <- function (old_index, df) {
  for (row in 1:nrow(df)) {
    if (df$Index[row] == old_index) {
      return(row)
    } 
  }
  return("Couldn't compute new index.")
}

remove_points_with_to_high_leverage <- function (mod, df) {
  k <- length(coefficients(mod))
  border <- 3 * k / nrow(df) 
  indicies_to_drop <- c()
  leverages <- hatvalues(mod)
  for (row in 1:nrow(df)) {
    if (leverages[row] > border) {
      indicies_to_drop <- append(row, indicies_to_drop)
    }
  }
  print("Removing indicies with leverages:")
  print(leverages[indicies_to_drop])
  return(df[-indicies_to_drop])
}