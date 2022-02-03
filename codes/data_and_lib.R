
########################################################
# By Bervelin Lumesa                                   #
#                                                      #
# Mail     : lumesabervelin@gmail.com                  #
# Github   : https://github.com/bervelin-lumesa        #
# Twitter  : https://twitter.com/bervelinL             #
# Linkedin : https://linkedin/in/bervelin-lumesa       #
########################################################

#========================= libraries =================================

library(flexdashboard)
library(haven)
library(foreign)
library(leaflet)
library(tidyverse)
library(kableExtra)
library(knitr)


# sample size
echantillon_menage = 4980
echantillon_femme = 5470
echantillon_homme = 1670
echantillon_individuel = echantillon_femme + echantillon_homme
echantillon_grappe = 249

#========================= loading data =================================

HQ_C <- read.spss("../data/HQ_C.sav", to.data.frame = T)
names(HQ_C)[2] <- "Province"
HQ_C$Province <- as.character(HQ_C$Province)

HQ_L <- read.spss("../data/HQ_L.sav", to.data.frame = T,)
names(HQ_L)[2] <- "Province"
HQ_L$Province <- as.character(HQ_L$Province)

MQ <- read.spss("../data/MQ.sav", to.data.frame = T)
names(MQ)[2] <- "Province"
MQ$Province <- as.character(MQ$Province)

WQ <- read.spss("../data/WQ.sav", to.data.frame = T)
names(WQ)[2] <- "Province"
WQ$Province <- as.character(WQ$Province)

#========================= creating new variables =================================

HQ_C$NEW_STATUT[HQ_C$HQ012_RESULTAT == "POURSUIVRE" & HQ_C$STATUT == "TERMINE"] <- "TERMINE"
HQ_C$NEW_STATUT[HQ_C$HQ012_RESULTAT == "POURSUIVRE" & (is.na(HQ_C$STATUT) | HQ_C$STATUT == "TERMINE PARTIELLEMENT")] <- "TERMINE PARTIELLEMENT"
HQ_C$NEW_STATUT[HQ_C$HQ012_RESULTAT == "PAS A LA MAISON" | HQ_C$HQ012_RESULTAT == "REPORTE" | HQ_C$HQ012_RESULTAT == "EMPECHE / PAS DISPONIBLE"] <- "PAS A LA MAISON / REPORTE / EMPECHE"
HQ_C$NEW_STATUT[HQ_C$HQ012_RESULTAT == "REFUSE"] <- "REFUSE"

HQ_C$COULEUR <- ifelse(HQ_C$NEW_STATUT == "TERMINE", "green",
                       ifelse(HQ_C$NEW_STATUT == "TERMINE PARTIELLEMENT", "orange",
                              ifelse(HQ_C$NEW_STATUT == "PAS A LA MAISON / REPORTE / EMPECHE", "grey", 
                                     ifelse(HQ_C$NEW_STATUT == "REFUSE","red", "yellow"))))

#========================= creating datasets for each province =================================

DATA_10 <- HQ_C %>%
  filter(Province == "KINSHASA")

DATA_20 <- HQ_C %>%
  filter(Province == "KONGO CENTRAL")

DATA_32 <- HQ_C %>%
  filter(Province == "KWILU")

DATA_41 <- HQ_C %>%
  filter(Province == "EQUATEUR")

DATA_42 <- HQ_C %>%
  filter(Province == "SUD-UBANGI")

DATA_44 <- HQ_C %>%
  filter(Province == "MONGALA")

DATA_51 <- HQ_C %>%
  filter(Province == "TSHOPO")

DATA_52 <- HQ_C %>%
  filter(Province == "BAS-UELE")

DATA_62 <- HQ_C %>%
  filter(Province == "SUD-KIVU")

DATA_63 <- HQ_C %>%
  filter(Province == "MANIEMA")

DATA_73 <- HQ_C %>%
  filter(Province == "TANGANYIKA")

DATA_74 <- HQ_C %>%
  filter(Province == "HAUT-KATANGA")

DATA_82 <- HQ_C %>%
  filter(Province == "SANKURU")

DATA_83 <- HQ_C %>%
  filter(Province == "LOMAMI")

DATA_92 <- HQ_C %>%
  filter(Province == "KASAI CENTRAL")

# some known data
grappe_prevue <- tibble(Province = c(
           "KINSHASA",
           "KONGO CENTRAL",
           "KWILU",
           "HAUT-KATANGA",
           "MANIEMA",
           "SUD-KIVU",
           "TANGANYIKA",
           "BAS-UELE",
           "EQUATEUR",
           "MONGALA",
           "SUD-UBANGI",
           "TSHOPO",
           "KASAI CENTRAL",
           "LOMAMI",
           "SANKURU"),
           `Grappes prevues` = c(
             26,
             13,
             12,
             14,
             7,
             17,
             9,
             7,
             9,
             10,
             14,
             14,
             41,
             30,
             26))
           

