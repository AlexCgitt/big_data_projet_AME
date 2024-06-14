
#   ____            _      _     ____  _         ____        _
#  |  _ \ _ __ ___ (_) ___| |_  | __ )(_) __ _  |  _ \  __ _| |_ __ _
#  | |_) | '__/ _ \| |/ _ \ __| |  _ \| |/ _` | | | | |/ _` | __/ _` |
#  |  __/| | | (_) | |  __/ |_  | |_) | | (_| | | |_| | (_| | || (_| |
#  |_|   |_|  \___// |\___|\__| |____/|_|\__, | |____/ \__,_|\__\__,_|
#                |__/                    |___/



library(sf)
library(leaflet)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(corrplot)



#   _     _____            _                 _   _                   _                     __   # nolint
#  / |   | ____|_  ___ __ | | ___  _ __ __ _| |_(_) ___  _ __     __| | ___  _ __  _ __   /_/  # nolint
#  | |   |  _| \ \/ / '_ \| |/ _ \| '__/ _` | __| |/ _ \| '_ \   / _` |/ _ \| '_ \| '_ \ / _ \ # nolint: line_length_linter.
#  | |_  | |___ >  <| |_) | | (_) | | | (_| | |_| | (_) | | | | | (_| | (_) | | | | | | |  __/ # nolint
#  |_(_) |_____/_/\_\ .__/|_|\___/|_|  \__,_|\__|_|\___/|_| |_|  \__,_|\___/|_| |_|_| |_|\___| # nolint: line_length_linter.
#                   |_|                                                                         # nolint: trailing_whitespace_linter, line_length_linter.


# Chargement des données
data <- read.csv("Patrimoine_Arbore.csv", header = TRUE, sep = ",")

# Correction encodage UTF-8 (expliquer dans le rapport)
utf8 <- function(data) {
    for (col in colnames(data)) {# nolint
        data[[col]] <- iconv(data[[col]], from = "latin1", to = "UTF-8")# nolint
    }# nolint
    return(data)
}

data <- utf8(data)

# Description du jeu de données dans le rapport

# comparaison de fk_nomtech, nomlatin et nomfrancais
print("------------Comparaison de fk_nomtech, nomlatin et nomfrancais-----------")# nolint
for (i in 1:nrow(data)) {
    if (data$fk_nomtech[i] != data$nomlatin[i] || data$fk_nomtech[i] != data$nomfrancais[i] || data$nomlatin[i] != data$nomfrancais[i]) { # nolint
        print(data[i, c("fk_nomtech", "nomlatin", "nomfrancais")]) # nolint
        print("---------------------------------------------------")
    } # nolint
}# on observe que les données sont pratiquement les mêmes donc on décide de supprimer les colonnes nomlatin et nomfrancais #nolint


#suppression de la colonne nomlatin
data$nomlatin <- NULL

# Conversion des types de données (remplacement des espaces (caractère) par NA)
data[data == ""] <- NA
data[data == " "] <- NA

data$X <- as.numeric(data$X) #a converti la colonne X en numérique
data$Y <- as.numeric(data$Y) #a converti la colonne Y en numérique
data$OBJECTID <- as.numeric(data$OBJECTID) #a converti la colonne OBJECTID en numérique # nolint
data$created_date <- as.Date(data$created_date, format = "%Y/%m/%d") #a converti la colonne created_date en date # nolint
data$created_user <- as.character(data$created_user)  # converti la colonne created_user en caractère # nolint
data$src_geo <- as.character(data$src_geo)  # converti la colonne src_geo en caractère # nolint
data$clc_quartier <- as.character(data$clc_quartier)  # converti la colonne clc_quartier en caractère # nolint
data$clc_secteur <- as.character(data$clc_secteur)  # converti la colonne clc_secteur en caractère # nolint
data$id_arbre <- as.numeric(data$id_arbre)  # converti la colonne id_arbre en numérique # nolint
data$haut_tot <- as.numeric(data$haut_tot)  # converti la colonne haut_tot en numérique # nolint
data$haut_tronc <- as.numeric(data$haut_tronc)  # converti la colonne haut_tronc en numérique # nolint
data$tronc_diam <- as.numeric(data$tronc_diam)  # converti la colonne tronc_diam en numérique # nolint
data$fk_arb_etat <- as.character(data$fk_arb_etat)  # converti la colonne fk_arb_etat en caractère # nolint    
data$fk_stadedev <- as.factor(data$fk_stadedev)  # converti la colonne fk_stadedev en facteur # nolint
data$fk_port <- as.character(data$fk_port)  # converti la colonne fk_port en caractère # nolint
data$fk_pied <- as.character(data$fk_pied)  # converti la colonne fk_pied en caractère # nolint
data$fk_situation <- as.character(data$fk_situation)  # converti la colonne fk_situation en caractère # nolint
data$fk_revetement <- as.character(data$fk_revetement)  # converti la colonne fk_revetement en caractère # nolint
data$commentaire_environnement <- as.character(data$commentaire_environnement)  # converti la colonne commentaire_environnement en caractère # nolint
data$dte_plantation <- as.Date(data$dte_plantation, format = "%Y/%m/%d")  # converti la colonne dte_plantation en date # nolint
data$age_estim <- as.numeric(data$age_estim)  # converti la colonne age_estim en numérique # nolint
data$fk_prec_estim <- as.numeric(data$fk_prec_estim)  # converti la colonne fk_prec_estim en numérique # nolint
data$clc_nbr_diag <- as.numeric(data$clc_nbr_diag)  # converti la colonne clc_nbr_diag en numérique # nolint
data$dte_abattage <- as.Date(data$dte_abattage, format = "%Y/%m/%d")  # converti la colonne dte_abattage en date # nolint
data$fk_nomtech <- as.character(data$fk_nomtech)  # converti la colonne fk_nomtech en caractère # nolint
data$last_edited_user <- as.character(data$last_edited_user)  # converti la colonne last_edited_user en caractère # nolint
data$last_edited_date <- as.Date(data$last_edited_date, format = "%Y/%m/%d")  # converti la colonne last_edited_date en date # nolint
data$villeca <- as.character(data$villeca)  # converti la colonne villeca en caractère # nolint
data$nomfrancais <- as.character(data$nomfrancais)  # converti la colonne nomfrancais en caractère # nolint
data$GlobalID <- as.character(data$GlobalID)  # converti la colonne GlobalID en caractère  # nolint
data$CreationDate <- as.Date(data$CreationDate, format = "%Y/%m/%d")  # converti la colonne CreationDate en date # nolint
data$Creator <- as.character(data$Creator)  # converti la colonne Creator en caractère # nolint
data$EditDate <- as.Date(data$EditDate, format = "%Y/%m/%d")  # converti la colonne EditDate en date # nolint
data$Editor <- as.character(data$Editor)  # converti la colonne Editor en caractère # nolint
data$feuillage <- as.character(data$feuillage)  # converti la colonne feuillage en caractère # nolint
data$remarquable <- as.character(data$remarquable)  # converti la colonne remarquable en caractère # nolint

# verification des données via # print(class(...))
# ex print("data$OBJECTID")


#   _   _      _   _                          _                 _                     __            # nolint
#  | \ | | ___| |_| |_ ___  _   _  ___ _ __  | | ___  ___    __| | ___  _ __  _ __   /_/  ___  ___  # nolint
#  |  \| |/ _ \ __| __/ _ \| | | |/ _ \ '__| | |/ _ \/ __|  / _` |/ _ \| '_ \| '_ \ / _ \/ _ \/ __| # nolint
#  | |\  |  __/ |_| || (_) | |_| |  __/ |    | |  __/\__ \ | (_| | (_) | | | | | | |  __/  __/\__ \ # nolint
#  |_| \_|\___|\__|\__\___/ \__, |\___|_|    |_|\___||___/  \__,_|\___/|_| |_|_| |_|\___|\___||___/ # nolint
#                           |___/

#suppression des lignes où il y a + de 12 valeurs NA
data <- data[rowSums(is.na(data)) < 13, ]



# Mise en minuscule de toute les colonnes de type caractère
for (colonne in names(data)) {
  if (is.character(data[[colonne]])) {
    data[[colonne]] <- tolower(data[[colonne]])
  }
}

#Mise en minuscule de la colonne stade de développement
data$fk_stadedev <- tolower(data$fk_stadedev)

# A décommenter pour vérifier si on a des doublons fonction qui prend enormement de temps 5 minutes
# #Vérification des doublons basés sur les coordonnées X et Y
# marquer_doublons <- function(data, verif) { #nolint
#   is_duplicated <- rep(FALSE, nrow(data))
#   for (i in 1:(nrow(data)-1)) { # -1 pour éviter de comparer le dernier élément avec un élément inexistant # nolint
#     for (j in (i + 1):nrow(data)) {
#       if (!is.na(data$X[i]) && !is.na(data$X[j]) &&
#           !is.na(data$Y[i]) && !is.na(data$Y[j]) && # nolint
#           data$X[i] == data$X[j] && data$Y[i] == data$Y[j] &&
#           !is.na(data$fk_arb_etat[i]) && !is.na(data$fk_arb_etat[j]) &&
#           data$fk_arb_etat[i] %in% verif &&
#           data$fk_arb_etat[j] %in% verif) {
#         is_duplicated[j] <- TRUE
#       }
#     }
#   }
#   return(is_duplicated)
# }
# verif <- c("en place", "non essouché", "abattu")
# is_duplicated <- marquer_doublons(data, verif)
# duplicated_rows <- data[is_duplicated, ]
# print("------------Vérification de si on a des doublons-----------")#nolint
# print(nrow(duplicated_rows))
# print("------------------------------bilan: 1 doublons------------------------------------")#nolint
# data <- data[!is_duplicated, ]
# is_duplicated <- marquer_doublons(data, verif)
# duplicated_rows <- data[is_duplicated, ]
# print("------------Vérification de si on a des doublons après suppression-----------")#nolint
# print(nrow(duplicated_rows))
# print("------------------------------bilan: 0 doublons------------------------------------")#nolint



#Nettoyage colonne X et Y dans le cas ou il reste des valeurs NA
data <- data[!is.na(data$X) & !is.na(data$Y), ]



#Verification de si on a des NA dans OBJECTID
print("------------Vérification de si on a des valeurs NA dans OBJECT-----------")#nolint
print(table(is.na(data$OBJECTID)))
print("-----------------------------bilan: aucune valeur NA-------------------------")#nolint



#Nettoyage colonne created_date
print("------------Vérification de si on a des NA dans created_date-----------")
print(table(is.na(data$created_date)))
print(table(data$created_date))
print("------------------------------bilan : 129 valeurs NA------------------------------------")# nolint

for (x in 2:length(data$created_date)) { # reprise de l'exercice fait en cours de Big Data # nolint
    if (is.na(data$created_date[x])) {# nolint
      if (!is.na(data$created_date[x-1])){# nolint
        data$created_date[x] <- data$created_date[x-1] # nolint
      } else if (!is.na(data$created_date[x+1])){# nolint
        data$created_date[x] <- data$created_date[x+1] # nolint
      } else {
        for (y in x:length(data$created_date)) {
          if (!is.na(data$created_date[y])) {
            data$created_date[x] <- data$created_date[y]
            break
          }
        }
      }
    }
}

print("------------Vérification de si on a des NA dans created_date après modification-----------") # nolint
print(table(is.na(data$created_date)))
print(table(data$created_date))
print("------------------------------bilan : 0 valeur NA------------------------------------")# nolint



#pour ce qui  est des autres dates il est expliqué dans le rapport pourquoi on ne les retrouves pas ici #nolint



#Nettoyage de la colonne feuillage

print("------------Vérification de si on a des NA dans feuillage-----------")
print(table(is.na(data$feuillage)))
print(table(data$feuillage))
print("------------------------------bilan : 170 valeurs NA------------------------------------")# nolint

data$feuillage[is.na(data$feuillage)] <- "inconnu" # remplace les valeurs NA par "Inconnu" # nolint

print("------------Vérification de si on a des NA dans feuillage après modification-----------") # nolint
print(table(is.na(data$feuillage)))
print(table(data$feuillage))
print("------------------------------bilan : 0 valeurs NA------------------------------------")# nolint



# Nettoyage de commentaire_environnement

print("------------Vérification de si on a des NA dans commentaire_environnement-----------") # nolint
print(table(is.na(data$commentaire_environnement)))
print("------------------------------bilan : 472 valeurs NA------------------------------------")# nolint

for (i in seq_len(nrow(data))) {
  if (is.na(data$commentaire_environnement[i])) {
    data$commentaire_environnement[i] <- "ras"
  }
}

print("------------Vérification de si on a des NA dans commentaire_environnement après modification-----------") # nolint
print(table(is.na(data$commentaire_environnement)))
print("------------------------------bilan : 0 valeurs NA------------------------------------")# nolint



# Nettoyage de fk_nomtech

print("------------Vérification de si on a des NA dans fk_nomtech-----------")
print(table(is.na(data$fk_nomtech)))
print("------------------------------bilan : 29 valeurs NA------------------------------------")# nolint

for (i in seq_len(nrow(data))) {
  if (is.na(data$fk_nomtech[i])) {
    data$fk_nomtech[i] <- "ras"
  }
}

print("------------Vérification de si on a des NA dans fk_nomtech après modification-----------") # nolint
print(table(is.na(data$fk_nomtech)))
print("------------------------------bilan : 0 valeurs NA------------------------------------")# nolint



# Nettoyage de nomfrancais

print("------------Vérification de si on a des NA dans nomfrancais-----------")
print(table(is.na(data$nomfrancais)))
print("------------------------------bilan : 0 valeurs NA------------------------------------")# nolint

for (i in seq_len(nrow(data))) {
  if (is.na(data$nomfrancais[i])) {
    data$nomfrancais[i] <- "ras"
  }
}

print("------------Vérification de si on a des NA dans nomfrancais après modification-----------") # nolint
print(table(is.na(data$nomfrancais)))
print("------------------------------bilan : 0 valeurs NA------------------------------------")# nolint



#nettoyage colonne created_user

print("------------Vérification de si on a des NA dans created_user-----------")
print(table(is.na(data$created_user)))
print(table(data$created_user))
print("------------------------------bilan : 154 valeurs NA------------------------------------")# nolint

data$created_user <- gsub(" ", ".", data$created_user) # gsub remplace les espaces par des points #nolint

for (i in seq_len(nrow(data))) { # parcours de la colonne created_user # nolint
    if (is.na(data$created_user[i])) { # si la valeur est NA # nolint
        data$created_user[i] <- "unknown" # on remplace par unknown # nolint
    } #nolint
}
print("------------Vérification de si on a des NA dans created_user après modification-----------") # nolint
print(table(is.na(data$created_user)))
print(table(data$created_user))
print("------------------------------bilan : 0 valeurs NA------------------------------------")# nolint



# Nettoyage de Creator

print("------------Vérification de si on a des NA dans Creator-----------")
print(table(is.na(data$Creator)))
print(table(data$Creator))
print("------------------------------bilan : 155 valeurs NA------------------------------------")# nolint

for (i in seq_len(nrow(data))) {
  if (is.na(data$Creator[i])) {
    data$Creator[i] <- "unknow"
  }
}

print("------------Vérification de si on a des NA dans Creator après modification-----------") # nolint
print(table(is.na(data$Creator)))
print(table(data$Creator))
print("------------------------------bilan : 0 valeurs NA------------------------------------")# nolint



# Nettoyage de Editor

print("------------Vérification de si on a des NA dans Editor-----------")
print(table(is.na(data$Editor)))
print(table(data$Editor))
print("------------------------------bilan : 155 valeurs NA------------------------------------")# nolint

for (i in seq_len(nrow(data))) {
  if (is.na(data$Editor[i])) {
    data$Editor[i] <- "unknow"
  }
}

print("------------Vérification de si on a des NA dans Editor après modification-----------") # nolint
print(table(is.na(data$Editor)))
print(table(data$Editor))
print("------------------------------bilan : 0 valeurs NA------------------------------------")# nolint



# Nettoyage de last_edited_user

print("------------Vérification de si on a des NA dans last_edited_user-----------") # nolint
print(table(is.na(data$last_edited_user)))
print(table(data$last_edited_user))
print("------------------------------bilan : 155 valeurs NA------------------------------------")# nolint

for (i in seq_len(nrow(data))) {
  if (is.na(data$last_edited_user[i])) {
    data$last_edited_user[i] <- "unknow"
  }
}

print("------------Vérification de si on a des NA dans last_edited_user après modification-----------") # nolint
print(table(is.na(data$last_edited_user)))
print(table(data$last_edited_user))
print("------------------------------bilan : 0 valeurs NA------------------------------------")# nolint



#Nettoyage colonne src_geo

print("------------Vérification de si on a des NA dans src_geo-----------") # nolint
print(table(is.na(data$src_geo)))
print(table(data$src_geo))
print("------------------------------bilan : 549 valeurs NA------------------------------------")# nolint

data$src_geo <- "orthophoto"# on remplace toutes les valeurs par orthophoto # nolint

print("------------Vérification de si on a des NA dans src_geo après modification-----------") # nolint
print(table(is.na(data$src_geo)))
print(table(data$src_geo))
print("------------------------------bilan : 0 valeurs NA------------------------------------")# nolint



#nettoyage clc_nbr_diag

print("------------Vérification de si on a des NA dans clc_nbr_diag-----------") # nolint
print(table(is.na(data$clc_nbr_diag)))
print("------------------------------bilan : 2526 valeurs NA------------------------------------")# nolint

data$clc_nbr_diag[is.na(data$clc_nbr_diag)] <- 0

print("------------Vérification de si on a des NA dans clc_nbr_diag après modification-----------") # nolint
print(table(is.na(data$clc_nbr_diag)))
print("------------------------------bilan : 0 valeurs NA------------------------------------")# nolint



#nettoyage colonne villeca

print("------------Vérification de si on a des NA dans villeca-----------") # nolint
print(table(is.na(data$villeca)))
print(table(data$villeca))
print("------------------------------bilan : 25 valeurs NA------------------------------------")# nolint

distance_geo <- function(x1, y1, x2, y2) {
    return(sqrt((x1 - x2)^2 + (y1 - y2)^2)) # nolint
}

oui_villeca <- function(data) {
  for (i in 1:nrow(data)) {
      if (is.na(data$villeca[i])) {# nolint
          min_dist <- Inf# nolint
          closest_ville <- NA
          for (j in 1:nrow(data)) {
              if (!is.na(data$villeca[j]) && i != j) {# nolint
                  dist <- distance_geo(data$X[i], data$Y[i], data$X[j], data$Y[j])# nolint
                  if (dist < min_dist) {
                      min_dist <- dist# nolint
                      closest_ville <- data$villeca[j]
                  }# nolint
              }# nolint
          }# nolint
          data$villeca[i] <- closest_ville
      }# nolint
  }
  return(data)
}

data <- oui_villeca(data) # nolint

print("------------Vérification de si on a des NA dans villeca après modification-----------") # nolint
print(table(is.na(data$villeca)))
print(table(data$villeca))
print("------------------------------bilan : 0 valeurs NA------------------------------------")# nolint


#netttoyage colonne clc_quartier

print("------------Vérification de si on a des NA dans clc_quartier-----------") # nolint
print(table(is.na(data$clc_quartier)))
print(table(data$clc_quartier))
print("------------------------------bilan : 97 valeurs NA------------------------------------")# nolint

distance_geo <- function(x1, y1, x2, y2) {
    return(sqrt((x1 - x2)^2 + (y1 - y2)^2)) # nolint
}


oui_clc_quartier <- function(data) { # Imputation des valeurs manquantes dans clc_quartier en utilisant la distance géographique # nolint
    for (i in 1:nrow(data)) { # nolint
        if (is.na(data$clc_quartier[i])) { # nolint
            min_dist <- Inf # nolint
            closest_quartier <- NA
            for (j in 1:nrow(data)) { # nolint
                if (!is.na(data$clc_quartier[j]) && i != j) { # nolint
                    dist <- distance_geo(data$X[i], data$Y[i], data$X[j], data$Y[j]) # nolint
                    if (dist < min_dist) {
                        min_dist <- dist # nolint
                        closest_quartier <- data$clc_quartier[j]
                    } # nolint
                } # nolint
            } # nolint
            # Si la distance minimale est supérieure à 500, on met "quartier inconnu" # nolint
            if (min_dist > 200) {
                closest_quartier <- "quartier inconnu" # nolint
            } # nolint
            data$clc_quartier[i] <- closest_quartier
        } # nolint
    } # nolint
    return(data)
}


data <- oui_clc_quartier(data) # Appliquer la fonction d'imputation

print("------------Vérification de si on a des NA dans clc_quartier après modification-----------") # nolint
print(table(is.na(data$clc_quartier)))
print(table(data$clc_quartier))
print("------------------------------bilan : 0 valeurs NA------------------------------------")# nolint



# netttoyage colonne clc_secteur

print("------------Vérification de si on a des NA dans clc_secteur-----------") # nolint
print(table(is.na(data$clc_secteur)))
print("------------------------------bilan : 0 valeurs NA------------------------------------")# nolint



# netttoyage colonne id_arbre

print("------------Vérification de si on a des NA dans id_arbre-----------") # nolint
print(table(is.na(data$id_arbre)))
print("------------------------------bilan : 0 valeurs NA------------------------------------")# nolint

for (x in 2:length(data$id_arbre)) {
    if (is.na(data$id_arbre[x])) { # nolint
      if (!is.na(data$id_arbre[x-1])){ # nolint
        data$id_arbre[x] <- data$id_arbre[x-1] + 1 # nolint
      } else if (!is.na(data$id_arbre[x+1])){ # nolint
        data$id_arbre[x] <- data$id_arbre[x+1] - 1 # nolint
      } else {
        for (y in x:length(data$id_arbre)) {
          if (!is.na(data$id_arbre[y])) {
            data$id_arbre[x] <- data$id_arbre[y] + 1
            break
          }
        }
      }
    }
}

print("------------Vérification de si on a des NA dans id_arbre après modification-----------") # nolint
print(table(is.na(data$id_arbre)))
print("------------------------------bilan : 0 valeurs NA------------------------------------")# nolint



# Nettoyage de fk_arb_etat

print("------------Vérification de si on a des NA dans fk_arb_etat-----------") # nolint
print(table(is.na(data$fk_arb_etat)))
print(table(data$fk_arb_etat))
print("------------------------------bilan : 0 valeurs NA------------------------------------")# nolint

for (i in seq_len(nrow(data))) {
  if (data$fk_arb_etat[i] != "en place" && !is.na(data$dte_abattage[i])) {# nolint
    data$fk_arb_etat[i] <- "abattu"
  }
  if (is.na(data$fk_arb_etat[i])) {
    data$fk_arb_etat[i] <- "abattu"
  }
}

print("------------Vérification de si on a des NA dans fk_arb_etat après modification-----------") # nolint
print(table(is.na(data$fk_arb_etat)))
print(table(data$fk_arb_etat))
print("------------------------------bilan : 0 valeurs NA------------------------------------")# nolint



# netttoyage colonne haut_tot, haut_tronc, tronc_diam

print("------------Vérification de si on a des NA dans les dimensions -----------") # nolint
print(sum(is.na(data$haut_tronc)))
print(sum(is.na(data$tronc_diam)))
print(sum(is.na(data$haut_tot)))


data_temp = data[!is.na(data$haut_tot) & !is.na(data$haut_tronc) & !is.na(data$fk_stadedev) & !is.na(data$tronc_diam) & data$feuillage != "inconnu" & !is.na(data$age_estim),] # nolint
model2 <- lm(tronc_diam ~ haut_tot + haut_tronc + fk_stadedev + feuillage + age_estim, data = data_temp) # nolint
data$tronc_diam[is.na(data$tronc_diam)] <- predict(model2, newdata = data[is.na(data$tronc_diam),]) # nolint


data_temp = data[!is.na(data$haut_tot) & !is.na(data$haut_tronc) & !is.na(data$fk_stadedev) & !is.na(data$tronc_diam),] # nolint
model3 <- lm(haut_tronc ~ haut_tot + tronc_diam + fk_stadedev , data = data_temp) # nolint
#met à jour les données NA de data$tronc_diam à partir de model3
data$haut_tronc[is.na(data$haut_tronc)] <- predict(model3, newdata = data[is.na(data$haut_tronc),]) # nolint

print("------------Vérification de si on a des NA dans les dimensions après modification-----------") # nolint
print(sum(is.na(data$haut_tronc)))
print(sum(is.na(data$tronc_diam)))
print(sum(is.na(data$haut_tot)))
print("------------------------------bilan : 0 valeurs NA------------------------------------")# nolint



# nettoyage colonne remarquable

data$nomfrancais <- as.factor(data$nomfrancais)


remarquable <- function(data){
    print("------------Vérification de si on a des NA dans les remarquables-----------")
     # nolint
     print(sum(is.na(data$haut_tot)))

    
    data$remarquable[data$remarquable == "oui"] <- 1
    data$remarquable[data$remarquable == "non"] <- 0
    data$remarquable <- as.numeric(data$remarquable)

    print(sum(is.na(data$remarquable)))
    print(table(data$remarquable))
    #data$remarquable[is.na(data$remarquable)] <- FALSE #on va faire une regression à la place

    data_temp=data[!is.na(data$remarquable) & !is.na(data$fk_stadedev) & !is.na(data$haut_tot) & !is.na(data$haut_tronc) & !is.na(data$tronc_diam),]
    #View(data_temp)
    model_remarquable <- glm(remarquable ~  fk_stadedev + haut_tot + haut_tronc + tronc_diam, data = data_temp, family = "binomial")
    data$remarquable[is.na(data$remarquable)] <- predict(model_remarquable, newdata = data[is.na(data$remarquable),], type = "response") > 0.5
    print(sum(is.na(data$remarquable)))
    print(table(data$remarquable))

    #met les 0 et 1 en booleens
    data$remarquable <- as.logical(data$remarquable)
    print("------------Vérification de si on a des NA dans ramarquable apres modification-----------") # nolint

    print(sum(is.na(data$remarquable)))
    print(table(data$remarquable))
    
    return(data)
}
data = remarquable(data)



#      _                _                                  _                 _        _                     # nolint
#     / \   _ __   __ _| |_   _ ___  ___    _____  ___ __ | | ___  _ __ __ _| |_ ___ (_)_ __ ___            # nolint
#    / _ \ | '_ \ / _` | | | | / __|/ _ \  / _ \ \/ / '_ \| |/ _ \| '__/ _` | __/ _ \| | '__/ _ \           # nolint
#   / ___ \| | | | (_| | | |_| \__ \  __/ |  __/>  <| |_) | | (_) | | | (_| | || (_) | | | |  __/           # nolint
#  /_/   \_\_| |_|\__,_|_|\__, |___/\___|  \___/_/\_\ .__/|_|\___/|_|  \__,_|\__\___/|_|_|  \___|           # nolint
#                         |___/                     |_|                                                     # nolint


# Afficher les graphiques 1 à 1

print("-----------------------Statistique descriptive univariée------------------------------") # nolint
print(summary(data))
print("--------------Fin Statistique descriptive univariée------------------------------------")# nolint



#Histogramme de la hauteur totale # nolint
hist(data$haut_tot)



# Boxplot du diamètre du tronc
boxplot(data$tronc_diam)



# Boxplot de la hauteur totale par quartier
boxplot(haut_tot ~ clc_quartier, data = data)



# Distribution des arbres par quartier
barplot(table(data$clc_quartier))



# Répartition des types de feuillage
pie(table(data$feuillage))



# Fréquence des variables catégorielles

print("-----------------------Fréquence des variables catégorielles------------------------------") # nolint
# categorical_columns <- c("created_user", "src_geo", "clc_quartier", "clc_secteur", # nolint
#                          "fk_arb_etat", "fk_stadedev", "fk_port", "fk_pied", #
#                          "fk_situation", "fk_revetement", "commentaire_environnement", # nolint
#                          "fk_prec_estim", "fk_nomtech", "last_edited_user", #nolint
#                          "villeca", "nomfrancais", "nomlatin", "Creator", # nolint
#                          "Editor", "feuillage", "remarquable")
# for (col in categorical_columns) {
#   cat("\nFréquence de la variable:", col)
#   print(table(data[[col]]))
# }
print("--------------Fin Fréquence des variables catégorielles------------------------------------")# nolint





#  __     ___                 _ _           _   _                   _                 _                     __                                  _                                   _     _                                 # nolint
#  \ \   / (_)___ _   _  __ _| (_)___  __ _| |_(_) ___  _ __     __| | ___  ___    __| | ___  _ __  _ __   /_/  ___  ___   ___ _   _ _ __    __| | ___  ___    __ _ _ __ __ _ _ __ | |__ (_) __ _ _   _  ___  ___   _       # nolint
#   \ \ / /| / __| | | |/ _` | | / __|/ _` | __| |/ _ \| '_ \   / _` |/ _ \/ __|  / _` |/ _ \| '_ \| '_ \ / _ \/ _ \/ __| / __| | | | '__|  / _` |/ _ \/ __|  / _` | '__/ _` | '_ \| '_ \| |/ _` | | | |/ _ \/ __| (_)      # nolint
#    \ V / | \__ \ |_| | (_| | | \__ \ (_| | |_| | (_) | | | | | (_| |  __/\__ \ | (_| | (_) | | | | | | |  __/  __/\__ \ \__ \ |_| | |    | (_| |  __/\__ \ | (_| | | | (_| | |_) | | | | | (_| | |_| |  __/\__ \  _       # nolint
#     \_/  |_|___/\__,_|\__,_|_|_|___/\__,_|\__|_|\___/|_| |_|  \__,_|\___||___/  \__,_|\___/|_| |_|_| |_|\___|\___||___/ |___/\__,_|_|     \__,_|\___||___/  \__, |_|  \__,_| .__/|_| |_|_|\__, |\__,_|\___||___/ (_)      # nolint
#                                                                                                                                                             |___/          |_|               |_|                          # nolint



# Graphique en camembert de la répartition des arbres par stade de développement
plot1 <- ggplot(data = data, aes(x = "", fill = fk_stadedev)) +
    geom_bar(width = 1) + coord_polar(theta = "y") + #nolint
    labs(title = "Répartition des arbres suivant leur stade de développement", #nolint
        fill = "Stade de développement") + #nolint
    theme_void() + theme(legend.position = "right") #nolint

# Histogramme de la quantité d'arbres par quartier
plot2 <- ggplot(data = data, aes(x = clc_quartier)) +
    geom_bar(fill = "orange") + #nolint
    labs(title = "Quantité d'arbres par quartier",
        x = "Quartier", #nolint
        y = "Nombre d'arbres") +
    theme_minimal() +   # Fond blanc quadrillé #nolint
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Histogramme de la quantité d'arbres par secteur
plot3 <- ggplot(data = data, aes(x = clc_secteur)) +
    geom_bar(fill = "yellow") + #nolint
    labs(title = "Quantité d'arbres par secteur",
          x = "Secteur", # nolint
          y = "Nombre d'arbres") +
    theme_minimal() +     # Fond blanc quadrillé # nolint
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Histogramme de la quantité d'arbres par situation
plot4 <- ggplot(data = data, aes(x = fk_situation)) +
    geom_bar(fill = "green") + #nolint
    labs(title = "Quantité d'arbres par situation",
          x = "Situation", #nolint
          y = "Nombre d'arbres") +
    theme_minimal() +     # Fond blanc quadrillé #nolint
    theme(axis.text.x = element_text(angle = 90, hjust = 1))





#  __     ___                 _ _           _   _                   _                 _                     __                                                                _         #nolint
#  \ \   / (_)___ _   _  __ _| (_)___  __ _| |_(_) ___  _ __     __| | ___  ___    __| | ___  _ __  _ __   /_/  ___  ___   ___ _   _ _ __   _   _ _ __   ___    ___ __ _ _ __| |_ ___   #nolint
#   \ \ / /| / __| | | |/ _` | | / __|/ _` | __| |/ _ \| '_ \   / _` |/ _ \/ __|  / _` |/ _ \| '_ \| '_ \ / _ \/ _ \/ __| / __| | | | '__| | | | | '_ \ / _ \  / __/ _` | '__| __/ _ \  #nolint
#    \ V / | \__ \ |_| | (_| | | \__ \ (_| | |_| | (_) | | | | | (_| |  __/\__ \ | (_| | (_) | | | | | | |  __/  __/\__ \ \__ \ |_| | |    | |_| | | | |  __/ | (_| (_| | |  | ||  __/  #nolint
#     \_/  |_|___/\__,_|\__,_|_|_|___/\__,_|\__|_|\___/|_| |_|  \__,_|\___||___/  \__,_|\___/|_| |_|_| |_|\___|\___||___/ |___/\__,_|_|     \__,_|_| |_|\___|  \___\__,_|_|   \__\___|  #nolint



# Conversion des données en objet spatial sf
# Transformation des coordonnées de EPSG:3949 à EPSG:4326
tree_sf <- st_as_sf(data, coords = c("X", "Y"), crs = 3949)
tree_sf_transfo <- st_transform(tree_sf, crs = 4326)

# Extraire les coordonnées transformées
tree_transfo <- cbind(data, st_coordinates(tree_sf_transfo))
names(tree_transfo)[(ncol(tree_transfo)-1):ncol(tree_transfo)] <- c("Longitude", "Latitude")

# Création d'une palette de couleurs
n <- length(unique(data$clc_quartier))
palette <- colorFactor(palette = brewer.pal(n, "Set3"), domain = data$clc_quartier)

# Calcul du rayon proportionnel à la variable tronc_diam divisé par 100
radius <- data$tronc_diam/100

# Visualisation sur une carte de Saint Quentin
carte <- leaflet(tree_transfo) %>% addTiles() %>%
    addCircleMarkers(~Longitude, ~Latitude, color = ~palette(clc_quartier), popup = ~paste("ID:", OBJECTID), radius = ~radius) %>%
    addLegend(pal = palette, values = ~clc_quartier, title = "Quartiers", position = "bottomright")
print(carte)

# Graphique de la quantité d'arbres par quartier
graph1 <- ggplot(data = tree_transfo, aes(x = clc_quartier)) +
    geom_bar(fill = "blue") +
    labs(title = "Quantité d'arbres par quartier",
        x = "Quartier",
        y = "Nombre d'arbres") +
    theme_minimal()       # Fond blanc quadrillé
print(graph1)

# Graphique de la quantité d'arbres par secteur
graph2 <- ggplot(data = tree_transfo, aes(x = clc_secteur)) +
    geom_bar(fill = "blue") +
    labs(title = "Quantité d'arbres par secteur",
        x = "Secteur",
        y = "Nombre d'arbres") +
    theme_minimal()       # Fond blanc quadrillé
print(graph2)



#   _____ _             _            _                                  __ _       _   _                              _                              _       _     _                    #nolint
#  | ____| |_ _   _  __| | ___    __| | ___  ___    ___ ___  _ __ _ __ /_/| | __ _| |_(_) ___  _ __  ___    ___ _ __ | |_ _ __ ___  __   ____ _ _ __(_) __ _| |__ | | ___  ___   _      #nolint
#  |  _| | __| | | |/ _` |/ _ \  / _` |/ _ \/ __|  / __/ _ \| '__| '__/ _ \ |/ _` | __| |/ _ \| '_ \/ __|  / _ \ '_ \| __| '__/ _ \ \ \ / / _` | '__| |/ _` | '_ \| |/ _ \/ __| (_)     #nolint
#  | |___| |_| |_| | (_| |  __/ | (_| |  __/\__ \ | (_| (_) | |  | | |  __/ | (_| | |_| | (_) | | | \__ \ |  __/ | | | |_| | |  __/  \ V / (_| | |  | | (_| | |_) | |  __/\__ \  _      #nolint
#  |_____|\__|\__,_|\__,_|\___|  \__,_|\___||___/  \___\___/|_|  |_|  \___|_|\__,_|\__|_|\___/|_| |_|___/  \___|_| |_|\__|_|  \___|   \_/ \__,_|_|  |_|\__,_|_.__/|_|\___||___/ (_)     #nolint



# Matrice de corrélation entre toutes les colonnes numériques
num_var <- data %>% select(where(is.numeric))
cor_matrix <- cor(num_var, use = "complete.obs", method="pearson")
corrplot(cor_matrix, method="circle", type="upper")
print(cor_matrix)

# Régression linéaire de age_estim
model <- lm(age_estim ~ haut_tot + haut_tronc + tronc_diam + fk_prec_estim + clc_nbr_diag, data = data)
#print(summary(model))

# Liste des colonnes à analyser
colonnes <- c("fk_arb_etat", "fk_stadedev")

# Analyse entre clc_quartier et chaques colonnes
for (col in colonnes) {
  # Création d'une table de contingence
  table <- table(data$clc_quartier, data[[col]])
  #print(table)
  
  # Test du chi2 pour chaque table
  chi2 <- chisq.test(table)
  #print(chi2)
  
  # Création d'un graphique en mosaique
  #mosaicplot(table, main = paste("Mosaic plot entre clc_quartier et", col), las = 2)
}



#   ____        __     _ _      _   _                  __  ____   __                         _                  #nolint
#  |  _ \ _ __ /_/  __| (_) ___| |_(_) ___  _ __      / / |  _ \ /_/  __ _ _ __ ___  ___ ___(_) ___  _ __       #nolint
#  | |_) | '__/ _ \/ _` | |/ __| __| |/ _ \| '_ \    / /  | |_) / _ \/ _` | '__/ _ \/ __/ __| |/ _ \| '_ \      #nolint
#  |  __/| | |  __/ (_| | | (__| |_| | (_) | | | |  / /   |  _ <  __/ (_| | | |  __/\__ \__ \ | (_) | | | |     #nolint
#  |_|   |_|  \___|\__,_|_|\___|\__|_|\___/|_| |_| /_/    |_| \_\___|\__, |_|  \___||___/___/_|\___/|_| |_|     #nolint
#                                                                    |___/                                      #nolint

print("--------Etude de regression sur age_etim-----------")

model1 <- lm(age_estim ~ tronc_diam, data = data)

# Résumé du modèle
print("Model 1 : Régression linéaire simple avec tronc_diam")
print(summary(model1))

# Modèle 2 : Régression linéaire simple avec haut_tot
model2 <- lm(age_estim ~ haut_tot, data = data)

# Résumé du modèle
print("Model 2 : Régression linéaire simple avec haut_tot")
print(summary(model2))

# Modèle 3 : Régression linéaire multiple avec tronc_diam et haut_tot
model3 <- lm(age_estim ~ tronc_diam + haut_tot, data = data)

# Résumé du modèle
print("Model 3 : Régression linéaire simple avec tronc_diam et haut_tot")
print(summary(model3))

# Modèle 4 : Régression linéaire multiple avec toutes les variables
print("Model 4 : Régression linéaire simple avec haut_tot, tronc_diam et haut_tot")
model4 <- lm(age_estim ~ tronc_diam + haut_tot + haut_tronc, data = data)

# Résumé du modèle
print(summary(model4))

# Modèle 5 : Régression linéaire multiple avec toutes les variables
print("Model 5 : Régression linéaire simple avec haut_tot, tronc_diam, haut_tronc et clc_nbr_diag")
model5 <- lm(age_estim ~ tronc_diam + haut_tot + haut_tronc + clc_nbr_diag, data = data)

# Résumé du modèle
print(summary(model5))

# Modèle 6 : Régression linéaire multiple avec toutes les variables
print("Model 6 : Régression linéaire simple avec tronc_diam, haut_tronc et clc_nbr_diag (sans haut_tot car on voit avant qu'il n'est pas significatif)")
model6 <- lm(age_estim ~ tronc_diam  + haut_tronc + clc_nbr_diag, data = data)
# Résumé du modèle
print(summary(model6))

print("Meilleur model : Modele 6 car il a le meilleur R² et les variables sont toutes significatives")
# nettoyage colonne remarquable
print("Nombre de NA avant regression :")
print(sum(is.na(data$age_estim)))
data$age_estim[is.na(data$age_estim)] <- predict(model6, newdata = data[is.na(data$age_estim),])
print("Nombre de NA après regression :")
print(sum(is.na(data$age_estim)))

print("--------Etude de regression sur l'harmonisation des quartiers-----------")

data$clc_quartier <- as.factor(data$clc_quartier)

# Calcul du nombre d'arbres par quartier
tree_count <- data %>% group_by(clc_quartier) %>% summarise(num_arbres = n())
print(tree_count)


#Y liste de 1 ou 0 si le quartier est harmonisé ou non (nombre d'arbre supérieur à la moyenne )
harmonisation <- ifelse(tree_count$num_arbres < mean(tree_count$num_arbres), 1, 0)
print(harmonisation)

model_plant <- glm(harmonisation ~ num_arbres, data = tree_count)

summary(model_plant)

courbe_regression = ggplot(data = tree_count, aes(x = num_arbres, y = harmonisation)) +
    geom_point() +
    geom_smooth(method = "glm", method.args = list(family = "binomial")) +
    labs(title = "Régression logistique de l'harmonisation des quartiers",
        x = "Nombre d'arbres",
        y = "Harmonisation") +
    theme_minimal()

print(courbe_regression)



print("--------Etude de regression sur l'abattage des arbres-----------")

# Calcul du nombre d'arbres abattus
abattage <- ifelse(data$fk_arb_etat != "en place", 1, 0)

model_abattage <- glm(abattage ~ haut_tot + tronc_diam + fk_stadedev , data = data, family = "binomial")

#fait la liste des arbres à abattre en focntion du modele d'abattage
data$abattage <- predict(model_abattage, newdata = data, type = "response") > 0.5

summary(model_abattage)

print(sum(is.na(data$abattage)))

model_abattage <- glm(abattage ~ haut_tot + tronc_diam , data = data, family = "binomial")
#refait la regression lineaire pour les NA de data$abattage (car pas de fk_stadedev)
data$abattage[is.na(data$abattage)] <- predict(model_abattage, newdata = data[is.na(data$abattage),], type = "response") > 0.5

#affiche les lignes (juste haut_tot tronc_diam fk_stadedev et remarquable) qui devraient être abattues sans prendre ceux où remarquable = TRUE
print(data[data$abattage == TRUE & data$remarquable == FALSE, c("haut_tot", "tronc_diam", "fk_stadedev", "remarquable")])



#                              _        _   _               #nolint
#    _____  ___ __   ___  _ __| |_ __ _| |_(_) ___  _ __    #nolint
#   / _ \ \/ / '_ \ / _ \| '__| __/ _` | __| |/ _ \| '_ \   #nolint
#  |  __/>  <| |_) | (_) | |  | || (_| | |_| | (_) | | | |  #nolint
#   \___/_/\_\ .__/ \___/|_|   \__\__,_|\__|_|\___/|_| |_|  #nolint
#            |_|                                            #nolint


# Sauvegarde des données nettoyées
write.csv(data, "data_cleaned.csv", row.names = FALSE)