#load(cityhomes.Rda) hej
CH <- cityhomes
ch <- na.omit(CH)

Vel <- split(x = ch, f = ch$Velhavende )
V0 <- Vel$'0'
V1 <- Vel$'1'

P0 <- split(x= V0, f = V0$KommuneNavn)
P1 <- split(x = V1, f= V1$KommuneNavn)


Pal <- split(x = ch, f = ch$KommuneNavn)
PC <- Pal$København
PA <- Pal$Aarhus
PO <- Pal$Odense
PAA <- Pal$Aalborg

C0 <- P0$København
C1 <- P1$København
A0 <- P0$Aarhus
A1 <- P1$Aarhus
O0 <- P0$Odense
O1 <- P1$Odense
AA0 <- P0$Aalborg
AA1 <- P1$Aalborg



ps <- log(ch$Pris_Salg)
psC <- log(PC$Pris_Salg)
psC0 <- log(C0$Pris_Salg)
psC1 <- log(C1$Pris_Salg)
psA <- log(PA$Pris_Salg)
psA0 <- log(A0$Pris_Salg)
psA1 <- log(A1$Pris_Salg)
psO <- log(PO$Pris_Salg)
psO0 <- log(O0$Pris_Salg)
psO1 <- log(O1$Pris_Salg)
psAA <- log(PAA$Pris_Salg)
psAA0 <- log(AA0$Pris_Salg)
psAA1 <- log(AA1$Pris_Salg)
ps0 <- log(V0$Pris_Salg)
ps1 <- log(V1$Pris_Salg)

ab <- ch$Areal_Bolig
abC <- PC$Areal_Bolig
abC0 <- C0$Areal_Bolig
abC1 <- C1$Areal_Bolig
abA <- PA$Areal_Bolig
abA0 <- A0$Areal_Bolig
abA1 <- A1$Areal_Bolig
abO <- PO$Areal_Bolig
abO0 <- O0$Areal_Bolig
abO1 <- O1$Areal_Bolig
abAA <- PAA$Areal_Bolig
abAA0 <- AA0$Areal_Bolig
abAA1 <- AA1$Areal_Bolig
ab0 <- V0$Areal_Bolig
ab1 <- V1$Areal_Bolig

ag <- ch$Areal_Grund
agC <- PC$Areal_Grund
agC0 <- C0$Areal_Grund
agC1 <- C1$Areal_Grund
agA <- PA$Areal_Grund
agA0 <- A0$Areal_Grund
agA1 <- A1$Areal_Grund
agO <- PO$Areal_Grund
agO0 <- O0$Areal_Grund
agO1 <- O1$Areal_Grund
agAA <- PAA$Areal_Grund
agAA0 <- AA0$Areal_Grund
agAA1 <- AA1$Areal_Grund
ag0 <- V0$Areal_Grund
ag1 <- V1$Areal_Grund

ea <- ch$Ejd_AntalRum
eaC <- PC$Ejd_AntalRum
eaC0 <- C0$Ejd_AntalRum
eaC1 <- C1$Ejd_AntalRum
eaA <- PA$Ejd_AntalRum
eaA0 <- A0$Ejd_AntalRum
eaA1 <- A1$Ejd_AntalRum
eaO <- PO$Ejd_AntalRum
eaO0 <- O0$Ejd_AntalRum
eaO1 <- O1$Ejd_AntalRum
eaAA <- PAA$Ejd_AntalRum
eaAA0 <- AA0$Ejd_AntalRum
eaAA1 <- AA1$Ejd_AntalRum
ea0 <- V0$Ejd_AntalRum
ea1 <- V1$Ejd_AntalRum

ds <- ch$Dist_skole
dsC <- PC$Dist_skole
dsC0 <- C0$Dist_skole
dsC1 <- C1$Dist_skole
dsA <- PA$Dist_skole
dsA0 <- A0$Dist_skole
dsA1 <- A1$Dist_skole
dsO <- PO$Dist_skole
dsO0 <- O0$Dist_skole
dsO1 <- O1$Dist_skole
dsAA <- PAA$Dist_skole
dsAA0 <- AA0$Dist_skole
dsAA1 <- AA1$Dist_skole
ds0 <- V0$Dist_skole
ds1 <- V1$Dist_skole

dr <- ch$Dist_raadhus
drC <- PC$Dist_raadhus
drC0 <- C0$Dist_raadhus
drC1 <- C1$Dist_raadhus
drA <- PA$Dist_raadhus
drA0 <- A0$Dist_raadhus
drA1 <- A1$Dist_raadhus
drO <- PO$Dist_raadhus
drO0 <- O0$Dist_raadhus
drO1 <- O1$Dist_raadhus
drAA <- PAA$Dist_raadhus
drAA0 <- AA0$Dist_raadhus
drAA1 <- AA1$Dist_raadhus
dr0 <- V0$Dist_raadhus
dr1 <- V1$Dist_raadhus

al <- ch$Alder
alC <- PC$Alder
alC0 <- C0$Alder
alC1 <- C1$Alder
alA <- PA$Alder
alA0 <- A0$Alder
alA1 <- A1$Alder
alO <- PO$Alder
alO0 <- O0$Alder
alO1 <- O1$Alder
alAA <- PAA$Alder
alAA0 <- AA0$Alder
alAA1 <- AA1$Alder
al0 <- V0$Alder
al1 <- V1$Alder




plot(ab, ps)
plot(log(ab), ps)#voksende

plot(abC, psC)
plot(log(abC), psC) #voksende
plot(abC0, psC0) # voksende
plot(abC1, psC1) 
plot(log(abC1), psC1) #voksende
#ingen grund til opdeling

plot(abA, psA) #voksende
plot(abA0, psA0)
plot(abA1, psA1)
#ingen grund til opdeling

plot(abO, psO)
plot(log(abO), psO) #voksende
plot(abO0, psO0) #log
plot(abO1, psO1) #voksende
#ingen grund til opdeling

plot(abAA, psAA)#voksende
plot(abAA0, psAA0)
plot(abAA1, psAA1)
#Overvej at dele op

plot(ab0, ps0) #voksende

plot(ab1, ps1)
plot(log(ab1), ps1) #voksende



plot(ag, ps) #ingen tydlig sammenhæng

plot(agC, psC) #voksende måske log
plot(log(agC), psC)
plot(agC0, psC0) #Voksende
plot(agC1, psC1) #måske log
#Overvej opdeling

plot(agA, psA)#ingen tydelig sammenhæng
plot(agA0, psA0)
plot(agA1, psA1)
#ingen grund til opdeling

plot(agO, psO)#voksende
plot(agO0, psO0) #ingen tydelig sammenhæng
plot(agO1, psO1)
plot(log(agO1), psO1) #Voksende
#Overvej opdeling

plot(agAA, psAA) #ingen tydelig sammenhæng
abline(lm(psAA ~agAA), col="red")
plot(agAA0, psAA0) #ingen tydlig sammenhæng
plot(agAA1, psAA1) #svag voksende sammenhæng
#Overvej opdeling

plot(ag0, ps0) #svagt aftagende/ingen tydelig sammenhæng
abline(lm(ps0 ~ag0), col="red")

plot(ag1, ps1) #voksende
abline(lm(ps1 ~ag1), col = "red")



plot(ea, ps) #vosende

plot(eaC, psC) #voksende

plot(eaA, psA)
plot(log(eaA), psA) # voksende

plot(eaO, psO)#voksende

plot(eaAA, psAA) #voksende

plot(ea0, ps0) #voksende
abline(lm(ps0 ~ea0), col = "red")

plot(ea1, ps1)#voksende



plot(ds, ps)#svagt aftagende

plot(dsC, psC) #aftagende
plot(dsC0, psC0) #Svagt aftagende
plot(dsC1, psC1) #Svagt voksende
#Overvej opdeling

plot(dsA, psA) # voksne
abline( lm(psA ~dsA), col="red")
plot(dsA0, psA0) #ingen tydelig sammenhæng
plot(dsA1, psA1) #voksende
#ingen grund til opdeling

plot(dsO, psO) #aftagende
plot(dsO0, psO0) #afstagende
plot(dsO1, psO1) #svagt voksende
#overvej opdeling

plot(dsAA, psAA)
abline(lm(psAA ~ dsAA), col= "red") #svagt aftagende/ingen tydelig sammenhæng
plot(dsAA0, psAA0)
plot(dsAA1, psAA1)
#ingen grund til opdeling

plot(ds0, ps0)#aftagende

plot(ds1, ps1) #svagt aftagende/ingen tydelig sammenhæng
abline(lm(ps1 ~ds1), col = "red")



plot(dr, ps) #svagt aftagende

plot(drC, psC) #ingen tydelig sammenhæng
plot(drC0, psC0) #ingen tydelig sammenhæng
plot(drC1, psC1) #Aftagende
#Overvej opdeling

plot(drA, psA) #ingen tydelig sammenhæng
plot(drA0, psA0)
plot(drA1, psA1)
#Ingen grund til opdeling

plot(drO, psO) # to delt hvor hver især ingen tydelig sammenhæng, ellers aftagende
plot(drO0, psO0)
plot(drO1, psO1)
#Opdel!!

plot(drAA, psAA)# aftagende
plot(drAA0, psAA0)
plot(drAA1, psAA1)
#Ingen grund til opdeling

plot(dr0, ps0)# aftagende

plot(dr1, ps1) # ingen tydelig sammenhæng
abline(lm(ps1 ~dr1), col = "red")



plot(al, ps)#svagt voksende
abline(lm(ps ~ al), col = "red") 

plot(alC, psC)#voksende
abline(lm(psC ~ alC), col = "red")
plot(alC0, psC0)
plot(alC1, psC1)
#Ingen grund til opdeling

plot(alA, psA)# aftagende
abline(lm(psA ~ alA), col = "red")
plot(alA0, psA0)
plot(alA1, psA1)
#Overvej opdeling

plot(alO, psO)#voksende
abline(lm(psO ~ alO), col = "red")
plot(alO0, psO0) #ingen tydlig sammenhæng
plot(alO1, psO1) #Aftagende
#Opdel

plot(alAA, psAA) # aftagende
abline(lm(psAA ~ alAA), col = "red") 
plot(alAA0, psAA0)
plot(alAA1, psAA1)
#ingen grund til opdeling

plot(al0, ps0) #svagt aftagnede/ ingen tydelig sammenhæng
abline(lm(ps0 ~ al0), col = "red")

plot(al1, ps1) #svagt voksende/ ingen tydelig sammenhæng
abline(lm(ps1 ~ al1), col = "red")







