load(file='cityhomes.Rda')
df <- cityhomes # Shorthand from now on.

# Adds fin trend, which is trend + 1/12 * month_number.
df$Salgsmaaned_encoded <- as.numeric(factor(df$Salgsmaaned, levels = unique(df$Salgsmaaned), exclude = NULL))
df$Fin_trend <- df$Trend +  1/12 * df$Salgsmaaned_encoded
df$Log_Pris <- log(df$Pris_Salg)
df$Log_Areal_Bolig <- log(df$Areal_Bolig)
df$Log_Areal_Grund <- log(df$Areal_Grund)
df$Log_Alder <- sqrt(df$Alder)

save(one_hot_encoded, file='cityhomes.Rda')

mod <- lm(Log_Pris~Log_Areal_Bolig + Log_Areal_Grund + Ejd_AntalRum + Fin_trend + StorGrund + Sogn + Dist_skole + Dist_raadhus + Log_Alder + Velhavende, data=df)
summary(mod)$r.squared
summary(mod)$adj.r.squared

