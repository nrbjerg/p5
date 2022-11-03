df <- cityhomes # Shorthand from now on.

# Adds fin trend, which is trend + 1/12 * month_number.
df$Salgsmaaned_encoded <- as.numeric(factor(df$Salgsmaaned, levels = unique(df$Salgsmaaned), exclude = NULL))
df$Fin_trend <- df$Trend +  1/12 * df$Salgsmaaned_encoded

# Convert column names to english
df$Price <- df$Pris_Salg
df$Rooms <- df$Ejd_AntalRum
df$Year <- df$Salgsaar
df$Month <- df$Salgsmaaned
df$Ground_Areal <- df$Areal_Grund
df$Home_Areal <- df$Areal_Bolig
df$Big_Ground <- df$StorGrund
df$Voting_Area <- df$Afstemningsomraade
df$Distance_School <- df$Dist_skole
df$Distance_City_Hall <- df$Dist_raadhus
df$Age <- df$Alder
df$Parish <- df$Sogn
df$Walthy <- df$Velhavende
df$Municipality <- df$KommuneNavn

drop <- c("Pris_Salg","Areal_Bolig", "Areal_Grund", "Ejd_AntalRum", "Salgsmaaned", "StorGrund", "Dist_skole", "Dist_raadhus", "Dist_Town_Hall", "KommuneNavn", "Velhavende", "Alder", "Dist_raadhus", "Dist_skole", "Sogn", "Afstemningsomraade", "Salgsaar", "Salgsmaaned_encoded")
df = df[,!(names(df) %in% drop)]

install.packages("stringr")
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


# Add missing data
df[is.na(df)] <- 0 # Replace not NA with 0, we will later replace these zeros with the means 

# The data for the number of rooms is missing in datapoints: 28, 113, 115, 560, 727, 806
for (idx in c(28, 119, 115, 560, 727, 806)) {
  df$Rooms[idx] <- mean(df$Rooms)
}

# The ground area is missing: 66, 271, 493
mean(df$Ground_Areal)
for (idx in c(66, 271, 493)) {
  df$Ground_Areal[idx] <- mean(df$Ground_Areal)
  if (df$Ground_Areal[idx] > 1448) {
    df$Big_Ground[idx] <- 1
  } else {
    df$Big_Ground[idx] <- 0
  }
}

any(is.na(df)) # Check that there is no NA Values.

save(df, file="cityhomes.Rda")
