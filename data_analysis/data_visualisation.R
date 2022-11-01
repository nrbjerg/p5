ch <- cityhomes


# Geographical information:
kn = ch$KommuneNavn
table(kn)
table(ch$Velhavende[ch$Sogn == 'Vollsmose'])


# Comparison of KommuneNavn and Pris_Salg
boxplot(ch$Pris_Salg[ch$KommuneNavn == 'København'],
        ch$Pris_Salg[ch$KommuneNavn == 'Aarhus'], 
        ch$Pris_Salg[ch$KommuneNavn == 'Aalborg'],
        ch$Pris_Salg[ch$KommuneNavn == 'Odense'],
        names=c("København", "Aarhus", "Aalborg","Odense"),
        ylab='Pris_Salg', outline = FALSE)
mean(ch$Pris_Salg[ch$KommuneNavn == 'København'])
mean(ch$Pris_Salg[ch$KommuneNavn == 'Aarhus'])
mean(ch$Pris_Salg[ch$KommuneNavn == 'Aalborg'])
mean(ch$Pris_Salg[ch$KommuneNavn == 'Odense'])


# Comparison of Areal_Grund and StorGrund
plot(ch$Areal_Grund, ch$StorGrund)
ch_edit = ch[c(-493, -271, -66,-122,-124),]
max(ch_edit$Areal_Grund[ch_edit$StorGrund == 0])
min(ch_edit$Areal_Grund[ch_edit$StorGrund == 1])
boxplot(ch_edit$Areal_Grund)
mean(ch_edit$Areal_Grund)


# Comparison of Areal_Bolig and Areal_Grund
plot(ch$Areal_Bolig, ch$Areal_Grund)
ch$Areal_Grund[ch$Areal_Grund > 4000]
ch_edit = ch[c(-4,-122,-124, -66, -271, -493),]
plot(ch_edit$Areal_Bolig, ch_edit$Areal_Grund)
L = lm(ch_edit$Areal_Grund~ch_edit$Areal_Bolig)
abline(L); summary(L)
plot(L)


# Comparison of Areal_Bolig and Ejd_AntalRum
ch_edit = ch[c(-28,-113,-115,-560,-727,-806, -4, -8),]
plot(ch_edit$Areal_Bolig, ch_edit$Ejd_AntalRum)
L = lm(ch_edit$Ejd_AntalRum~ch_edit$Areal_Bolig)
abline(L); summary(L)
plot(L)


# Comparison of Pris_Salg and Velhavende
plot(ch$Pris_Salg,ch$Velhavende)
boxplot(ch$Pris_Salg[ch$Velhavende == 1],
        ch$Pris_Salg[ch$Velhavende == 0],
        names=c("Velhavende", "Fattig"),
        ylab='Pris_Salg', outline=FALSE)


# Comparison of Salgsaar and Salgsmaaned, and Pris_Salg
plot(ch$Salgsaar,ch$Pris_Salg)
boxplot(ch$Pris_Salg[ch$Salgsmaaned %in% c('Januar','Februar','December')],
        ch$Pris_Salg[ch$Salgsmaaned %in% c('Marts','April','Maj')], 
        ch$Pris_Salg[ch$Salgsmaaned %in% c('Juni','Juli','August')],
        ch$Pris_Salg[ch$Salgsmaaned %in% c('September','November','Oktober')],
        names=c("Vinter", "Forår", "Sommer","Efterår"),
        ylab='Pris_Salg', outline=FALSE)
table(ch$Salgsaar)
table(ch$Salgsmaaned)
boxplot(ch$Pris_Salg, outline = FALSE)


# Comparison of Dist_skole and Dist_raadhus
plot(ch$Dist_raadhus,ch$Dist_skole)
plot(ch$Dist_raadhus,ch$Velhavende)


plot(ch$Salgsaar,ch$Trend)


plot(ch$Salgsaar[ch$Sogn == 'Vollsmose'], ch$Pris_Salg[ch$Sogn == 'Vollsmose'])


# Cleaning
rm(list=ls())

