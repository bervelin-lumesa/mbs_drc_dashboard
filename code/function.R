
########################################################
# By Bervelin Lumesa                                   #
#                                                      #
# Mail     : lumesabervelin@gmail.com                  #
# Github   : https://github.com/bervelin-lumesa        #
# Twitter  : https://twitter.com/bervelinL             #
# Linkedin : https://linkedin/in/bervelin-lumesa       #
########################################################

# THIS FILE INCLUDES FUNCTIONS

#========================= functions for valueboxes =================================
# number of households visited
nombre_menage_visite <- function(data){
  return(dim(data)[1])
}

# number of women interviewed
nombre_femme_interviewee <- function(data){
  return(dim(data[which(!is.na(data$WQ012_RESULTAT) | !is.na(data$ASSENTIMENT)), ])[1])
}

# number of men interviewed
nombre_homme_interviewe <- function(data){
  return(dim(data[which(!is.na(data$MQ012_RESULTAT) | !is.na(data$CONSENTEMENT_M)), ])[1])
}

# number of areas (village) visited
grappe_touchee <- function(){
  
  tab1 <-  HQ_C %>%
    distinct(Province, HQ006_NOM_GRAPPE) %>%
    group_by(Province) %>%
    summarise("Grappes touchees" = n())
  
  return(as_tibble(tab1))
}

# total number of areas (village) visited
total_grappe_touchee <- function(){
  
  n <- sum(grappe_touchee()$`Grappes touchees`)
  
  return(n)
}


# to combine differents stats into one table
Union <- function(...) {
  # Behaves as MySQL UNION statement. Appends a list just below the other.
  #
  # Args:
  #   ...: Lists to be appended.
  #
  # Returns:
  #   Data frame containing the lists appended.
  aux <- list(...)
  dat <- data.frame()
  for (i in seq(along = aux)) {
    if (length(aux[[i]]) == 0) {
      dat[i, ] <- rep(0, ncol(dat))
    } else {
      for (j in names(aux[[i]]))
        dat[i, j] <- aux[[i]][j] 
    }
  }
  dat <- rapply(dat, f = function(x) ifelse(is.na(x), 0, x), how = "replace")
  return(dat)
}

#========================= functions for building different tables  =================================

# function to build the table for the area infos

info_grappe <- function(){
  tab <- grappe_touchee() %>% 
    inner_join(grappe_prevue, by = "Province") %>%
    mutate(`Reste a faire` = `Grappes prevues` - `Grappes touchees`)
  return(tab)
}

# function to build the table for household infos
info_menage <- function(data){
  
# same as : 
#  data %>%
#    filter(!is.na(data$HQ012_RESULTAT)) %>%
#    select(Province) %>%
#    magrittr::use_series(Province) %>%
#    table()
  
  menage_approche <- table(data[which(!is.na(data$HQ012_RESULTAT)), 'Province']) 
    
  menage_termine  <- table(data[which(data$STATUT == "TERMINE"), 'Province'])
  
  menage_partiel <- table(data[which(data$HQ012_RESULTAT == "POURSUIVRE" & (is.na(data$STATUT))), 'Province'])
  
  menage_autres  <- table(data[which(data$HQ012_RESULTAT == "PAS A LA MAISON" | data$HQ012_RESULTAT == "REPORTE" | data$HQ012_RESULTAT == "EMPECHE / PAS DISPONIBLE"), 'Province'])
  
  menage_refus    <- table(data[which(data$HQ012_RESULTAT == "REFUSE"), 'Province'])
  
profil <- Union(menage_approche,
                menage_termine,
                menage_partiel,
                menage_autres,
                menage_refus
                )
  
  row.names(profil) <- c(
    'Menages visites',
    "Menages avec enquete terminee",
    "Menage avec enquete partielle",
    "Pas a la maison / reporte / empeche / pas disponible",
    "Menages avec refus"
  )
  
  profil$ENSEMBLE <- rowSums(profil)
  
  profil <- t(profil)
  return(profil)
    
}

# function to build the table for women infos
info_femme <- function(data_liste, data_femme){
  
  eligible     <- table(data_liste[which(data_liste$HQ032_SEXE == "FEMME" & (data_liste$HQ035_AGE >= 15 & data_liste$HQ035_AGE <= 49) & data_liste$HQ033_RESIDENCE_VIT == "OUI" & !is.na(data_liste$HQ030_LIGNE)), 'Province']) 
  
  interviewee  <- table(data_femme[which(!is.na(data_femme$WQ012_RESULTAT) | !is.na(data_femme$ASSENTIMENT)), 'Province'])
  
  terminee     <- table(data_femme[which(data_femme$STATUT_W == "TERMINE"), 'Province'])
  
  partiel     <- table(data_femme[which(data_femme$WQ012_RESULTAT == "POURSUIVRE" & (is.na(data_femme$STATUT_W))), 'Province'])
  
  femme_autres    <- table(data_femme[which(data_femme$WQ012_RESULTAT == "PAS A LA MAISON" | data_femme$WQ012_RESULTAT == "REPORTE" | data_femme$WQ012_RESULTAT == "EMPECHEE / PAS DISPONIBLE" | data_femme$ASSENTIMENT == "FEMME NON DISPONIBLE"), 'Province'])
  
  femme_refus <- table(data_femme[which(data_femme$WQ012_RESULTAT == "REFUSE"), 'Province'])
  
  profil <- Union(eligible,
                  interviewee,
                  terminee,
                  partiel,
                  femme_autres,
                  femme_refus)
  
  row.names(profil) <- c(
    'Femmes eligibles',
    "Femmes interviewees",
    "Femmes avec enquete terminee",
    "Femme avec enquete partielle",
    "Pas a la maison / reporte / empeche / pas disponible",
    "Femmes avec refus"
  )
  
  profil$ENSEMBLE <- rowSums(profil)
  
  profil <- t(profil)
  
  return(profil)
  
}

# function to build the table for men infos
info_homme <- function(data_liste, data_homme){
  
  eligible     <- table(data_liste[which((data_liste$HQ035_AGE >= 18 & data_liste$HQ035_AGE <= 59) & data_liste$HQ032_SEXE == "HOMME"), 'Province']) 
  
  interviewe  <- table(data_homme[which(!is.na(data_homme$CONSENTEMENT_M)), 'Province'])
  
  terminee     <- table(data_homme[which(data_homme$STATUT_M == "TERMINE"), 'Province'])
  
  partiel     <- table(data_homme[which(data_homme$MQ012_RESULTAT == "POURSUIVRE" & (is.na(data_homme$STATUT_M))), 'Province'])
  
  homme_autres    <- table(data_homme[which(data_homme$MQ012_RESULTAT == "PAS A LA MAISON" | data_homme$MQ012_RESULTAT == "REPORTE" | data_homme$MQ012_RESULTAT == "EMPECHE / PAS DISPONIBLE"), 'Province'])
  
  homme_refus <- table(data_homme[which(data_homme$MQ012_RESULTAT == "REFUSE"), 'Province'])
  
  profil <- Union(eligible,
                  interviewe,
                  terminee,
                  partiel,
                  homme_autres,
                  homme_refus)
  
  row.names(profil) <- c(
    'Hommes eligibles',
    "Hommes interviewes",
    "Hommes avec enquete terminee",
    "Homme avec enquete partielle",
    "Pas a la maison / reporte / empeche / pas disponible",
    "Hommes avec refus"
  )
  
  profil$ENSEMBLE <- rowSums(profil)
  
  profil <- t(profil)
  
  return(profil)
  
}




